;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.helm-releases-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.helm :as helm]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.kubectl :as kubectl]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- epoch [iso] (.getEpochSecond (java.time.Instant/parse iso)))

(defn- rev [version status & {:keys [at release namespace] :or {at "2026-11-19T11:00:00Z" release "web" namespace "shop"}}]
  {:namespace namespace :release release :status status :version version :modified-at (epoch at)})

(deftest parsing-the-listing
  (let [text (str "shop\tweb\tdeployed\t3\t1763550000\t1763540000\t2026-11-19T10:00:00Z\n"
                  "shop\tweb\tsuperseded\t2\t\t1763530000\t2026-11-18T10:00:00Z\n"
                  "\n"
                  "broken\t\tdeployed\t1\t\t\t\n")]
    (is (= [{:namespace "shop" :release "web" :status "deployed" :version 3
             :modified-at 1763550000 :created-at 1763540000 :creation-timestamp "2026-11-19T10:00:00Z"}
            {:namespace "shop" :release "web" :status "superseded" :version 2
             :modified-at nil :created-at 1763530000 :creation-timestamp "2026-11-18T10:00:00Z"}]
           (kubectl/parse-helm-listing text)))
    (is (= [] (kubectl/parse-helm-listing "")))
    (is (= [] (kubectl/parse-helm-listing nil)))))

(deftest when-a-revision-changed
  (is (= (java.time.Instant/ofEpochSecond 20) (helm/changed-at {:modified-at 20 :created-at 10 :creation-timestamp "2026-01-01T00:00:00Z"})))
  (is (= (java.time.Instant/ofEpochSecond 10) (helm/changed-at {:created-at 10 :creation-timestamp "2026-01-01T00:00:00Z"})))
  (is (= (java.time.Instant/parse "2026-01-01T00:00:00Z") (helm/changed-at {:creation-timestamp "2026-01-01T00:00:00Z"})))
  (is (nil? (helm/changed-at {}))))

(deftest only-the-newest-revision-is-judged
  (let [[latest] (helm/latest-revisions [(rev 1 "superseded") (rev 10 "failed") (rev 9 "deployed") (rev 2 "superseded")])]
    (is (= 10 (:version latest)) "numeric, not lexical, order")
    (is (= [9] (:deployed latest))))
  (is (= 3 (count (helm/latest-revisions [(rev 1 "deployed") (rev 1 "deployed" :release "api")
                                          (rev 1 "deployed" :namespace "other")])))
      "releases are told apart by namespace and name")
  (testing "a healthy history reports nothing"
    (is (empty? (helm/detect-helm-releases {:now now "helm-releases" [(rev 1 "superseded") (rev 2 "failed") (rev 3 "deployed")]})))))

(deftest a-pending-release-holds-its-lock
  (let [finding (helm/release-finding (first (helm/latest-revisions [(rev 4 "deployed") (rev 5 "pending-upgrade" :at "2026-11-19T11:30:00Z")])) now)]
    (is (= :critical (:severity finding)))
    (is (= "shop/web" (:component finding)))
    (is (= "Helm release is stuck in pending-upgrade (revision 5)" (:summary finding)))
    (is (str/includes? (:hint finding) "another operation (install/upgrade/rollback) is in progress"))
    (is (str/includes? (:hint finding) "helm rollback web 4 -n shop"))
    (is (str/includes? (:hint finding) "sh.helm.release.v1.web.v5"))
    (is (= "2026-11-19T11:30:00Z" (:since finding))))
  (testing "a pending install has nothing to roll back to"
    (is (str/includes? (:hint (helm/release-finding (first (helm/latest-revisions [(rev 1 "pending-install")])) now))
                       "helm uninstall web -n shop")))
  (testing "every pending status counts"
    (doseq [status ["pending-install" "pending-upgrade" "pending-rollback"]]
      (is (= :critical (:severity (helm/release-finding (rev 2 status) now))) status)))
  (testing "a running operation is left alone"
    (is (nil? (helm/release-finding (rev 2 "pending-upgrade" :at "2026-11-19T11:55:00Z") now))))
  (testing "ten minutes is stuck"
    (is (some? (helm/release-finding (rev 2 "pending-upgrade" :at "2026-11-19T11:50:00Z") now))))
  (testing "without a timestamp it cannot wait, so it reports"
    (let [finding (helm/release-finding {:namespace "shop" :release "web" :status "pending-upgrade" :version 2} now)]
      (is (= "Helm release is stuck in pending-upgrade (revision 2)" (:summary finding)))
      (is (nil? (:since finding))))))

(deftest a-failed-release
  (let [finding (helm/release-finding (first (helm/latest-revisions [(rev 1 "superseded") (rev 2 "deployed") (rev 3 "failed")])) now)]
    (is (= :warning (:severity finding)) "the previous revision still runs")
    (is (= "Helm release's last operation failed (revision 3)" (:summary finding)))
    (is (str/includes? (:hint finding) "revision 2 is still deployed")))
  (testing "a failed install with nothing deployed is critical"
    (let [finding (helm/release-finding (first (helm/latest-revisions [(rev 1 "failed")])) now)]
      (is (= :critical (:severity finding)))
      (is (str/includes? (:hint finding) "no revision of it is deployed"))))
  (testing "a fresh failure reports at once"
    (is (some? (helm/release-finding (rev 3 "failed" :at "2026-11-19T11:59:00Z") now)))))

(deftest an-uninstall-that-does-not-finish
  (is (= "Helm release is stuck uninstalling (revision 2)"
         (:summary (helm/release-finding (rev 2 "uninstalling") now))))
  (is (= :warning (:severity (helm/release-finding (rev 2 "uninstalling") now))))
  (is (nil? (helm/release-finding (rev 2 "uninstalling" :at "2026-11-19T11:58:00Z") now))))

(deftest settled-statuses-say-nothing
  (doseq [status ["deployed" "superseded" "uninstalled" "unknown"]]
    (is (nil? (helm/release-finding (rev 2 status) now)) status)))

(deftest the-detective-is-registered-with-its-collector
  (is (some #(= "helm-releases" (:name %)) (registry/for-scope :workloads)))
  (is (not= (registry/collector-for "nothing-registered") (registry/collector-for "helm-releases"))
      "release payloads are never read with a plain get"))
