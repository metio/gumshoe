;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.status-subresource-pin-test
  "An object whose CRD declares no status subresource raises its generation
   on every status write, so a pin compares its spec instead: a controller
   reporting progress during the confirmation does not refuse the change,
   and a real spec change still does."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.mutation :as mutation]))

(def ^:private application-type "applications.argoproj.io")

(defn- application
  [generation rv spec status]
  {:metadata {:namespace "argocd" :name "shop" :uid "uid-shop" :generation generation :resourceVersion rv}
   :spec spec :status status})

(deftest comparison-decides-what-a-pin-compares
  (testing "a type without the status subresource is compared by spec, generation or not"
    (is (= :spec (change/comparison (application 7 "1" {} {}) application-type nil)))
    (is (= :spec (change/comparison (application 7 "1" {} {}) application-type :resource-version))))
  (testing "a declared :spec wins over a generation"
    (is (= :spec (change/comparison {:metadata {:generation 2}} "deployments" :spec))))
  (testing "a generation tracks the spec of every other type"
    (is (= :generation (change/comparison {:metadata {:generation 2}} "deployments" nil)))
    (is (= :generation (change/comparison {:metadata {:generation 2}} nil :resource-version))))
  (testing "without a generation the resourceVersion is compared"
    (is (= :resource-version (change/comparison {:metadata {:resourceVersion "3"}} "configmaps" nil))))
  (is (contains? change/types-without-status-subresource application-type)))

(deftest a-status-write-during-the-confirmation-is-not-a-drift
  (let [ref {:type application-type :namespace "argocd" :name "shop"}
        before (mutation/ref-fingerprint ref (application 7 "100" {:project "default"} {:reconciledAt "t1"}))
        after (mutation/ref-fingerprint ref (application 8 "101" {:project "default"} {:reconciledAt "t2"}))]
    (is (= {:uid "uid-shop" :spec {:project "default"}} before))
    (is (nil? (mutation/drift before after))
        "the application controller wrote its status, which raised the generation")))

(deftest a-spec-change-of-an-application-is-still-refused
  (let [ref {:type application-type :namespace "argocd" :name "shop"}]
    (is (= "its spec changed after the confirmation: project"
           (mutation/drift (mutation/ref-fingerprint ref (application 7 "100" {:project "default"} {}))
                           (mutation/ref-fingerprint ref (application 8 "101" {:project "other"} {})))))
    (is (= "it was deleted and created again after the confirmation"
           (mutation/drift (mutation/ref-fingerprint ref (application 7 "100" {} {}))
                           (mutation/ref-fingerprint ref (assoc-in (application 1 "200" {} {}) [:metadata :uid] "uid-new")))))))

(deftest a-deployment-is-still-pinned-by-generation
  (let [ref {:type "deployments" :namespace "shop" :name "web"}
        deployment (fn [generation] {:metadata {:uid "u" :generation generation :resourceVersion (str generation)} :spec {}})]
    (is (= {:uid "u" :generation 3} (mutation/ref-fingerprint ref (deployment 3))))
    (is (= "its spec changed after the confirmation (generation 3, now 4)"
           (mutation/drift (mutation/ref-fingerprint ref (deployment 3)) (mutation/ref-fingerprint ref (deployment 4)))))))

(deftest the-argocd-books-pin-an-application
  (doseq [path ["tools/argocd/runbooks/sync.clj" "tools/argocd/runbooks/refresh.clj"]]
    (testing path
      (let [spec (with-redefs [mutation/book identity] (load-file path))]
        (is (contains? change/types-without-status-subresource (get-in spec [:subject :type])))))))
