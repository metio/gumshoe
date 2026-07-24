;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.apiversions-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.apiversions :as apiversions]
            [gumshoe.detectives.apiversions :as detectives]
            [gumshoe.detectives.registry :as registry]))

(deftest collector-seam-test
  (testing "a plain resource type resolves to the shared get-all default"
    (is (identical? (registry/collector-for "pods")
                    (registry/collector-for "services"))))
  (testing "api-resources has its own collector, distinct from the default"
    (is (not (identical? (registry/collector-for "api-resources")
                         (registry/collector-for "pods"))))))

(def ^:private api-resources-sample
  ;; --no-headers output: rows with and without SHORTNAMES, core and grouped.
  (str "pods                     po           v1                                       true    Pod\n"
       "componentstatuses        cs           v1                                       false   ComponentStatus\n"
       "alerts                                notification.toolkit.fluxcd.io/v1beta2   true    Alert\n"
       "providers                            notification.toolkit.fluxcd.io/v1beta2   true    Provider\n"
       "receivers                            notification.toolkit.fluxcd.io/v1        true    Receiver\n"))

(deftest parse-api-resources-test
  (testing "keys on [group plural]->version, ignoring the optional SHORTNAMES column"
    (let [parsed (apiversions/parse-api-resources api-resources-sample)]
      (is (= "v1beta2" (get parsed ["notification.toolkit.fluxcd.io" "alerts"])))
      (is (= "v1beta2" (get parsed ["notification.toolkit.fluxcd.io" "providers"])))
      (is (= "v1" (get parsed ["notification.toolkit.fluxcd.io" "receivers"])))
      (testing "core-group rows land under group \"\""
        (is (= "v1" (get parsed ["" "pods"])))
        (is (= "v1" (get parsed ["" "componentstatuses"]))))))
  (testing "empty input is empty, not a crash"
    (is (= {} (apiversions/parse-api-resources "")))
    (is (= {} (apiversions/parse-api-resources nil)))))

(defn- crd [group plural version-specs]
  {:spec {:group group
          :names {:plural plural}
          :versions (mapv (fn [[name served]] {:name name :served served}) version-specs)}})

(deftest served-version-names-test
  (testing "only served versions count; storage-only/unserved never appear in discovery"
    (is (= #{"v1beta3"} (detectives/served-version-names
                         (crd "x.io" "widgets" [["v1beta2" false] ["v1beta3" true]]))))))

(deftest detect-version-drift-test
  (let [crds {:items [(crd "notification.toolkit.fluxcd.io" "alerts" [["v1beta3" true]])
                      (crd "notification.toolkit.fluxcd.io" "providers" [["v1beta3" true]])
                      (crd "notification.toolkit.fluxcd.io" "receivers" [["v1" true]])]}]
    (testing "advertised version the CRD no longer serves is a critical finding"
      (let [evidence {"customresourcedefinitions" crds
                      "api-resources" (apiversions/parse-api-resources api-resources-sample)}
            findings (detectives/detect-version-drift evidence)]
        (is (= 2 (count findings)))
        (is (every? #(= :critical (:severity %)) findings))
        (is (= #{"notification.toolkit.fluxcd.io/alerts"
                 "notification.toolkit.fluxcd.io/providers"}
               (set (map :component findings))))
        (is (re-find #"v1beta2.*v1beta3" (:summary (first findings))))))
    (testing "when discovery agrees with the CRD, nothing is flagged"
      (let [agrees (str "alerts     notification.toolkit.fluxcd.io/v1beta3   true   Alert\n"
                        "receivers  notification.toolkit.fluxcd.io/v1        true   Receiver\n")
            evidence {"customresourcedefinitions" crds "api-resources" (apiversions/parse-api-resources agrees)}]
        (is (empty? (detectives/detect-version-drift evidence)))))
    (testing "a resource discovery does not list at all is not a drift (nothing to compare)"
      (is (empty? (detectives/detect-version-drift {"customresourcedefinitions" crds "api-resources" {}}))))))
