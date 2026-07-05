;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.stageset-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.stageset :as stageset]))

(defn- summaries [findings] (set (map :summary findings)))

(deftest stageset-severity-by-reason-test
  (testing "Ready=False maps its reason to a severity; hard failures are critical, gates warn, human-waits inform"
    (let [evidence {stageset/stageset-type
                    {:items [{:metadata {:namespace "apps" :name "web"}
                              :status {:conditions [{:type "Ready" :status "False" :reason "RollbackStoreFailed"
                                                     :message "snapshot missing"}]}}
                             {:metadata {:namespace "apps" :name "api"}
                              :status {:conditions [{:type "Ready" :status "False" :reason "AwaitingPromotion"}]}}
                             {:metadata {:namespace "apps" :name "worker"}
                              :status {:conditions [{:type "Ready" :status "False" :reason "BudgetExhausted"}]}}
                             {:metadata {:namespace "apps" :name "healthy"}
                              :status {:conditions [{:type "Ready" :status "True"}]}}]}}
          findings (stageset/detect-stageset-problems evidence)
          by-summary (into {} (map (juxt :summary :severity) findings))]
      (is (= #{"StageSet is not Ready (RollbackStoreFailed)"
               "StageSet is not Ready (AwaitingPromotion)"
               "StageSet is not Ready (BudgetExhausted)"}
             (summaries findings)) "a healthy StageSet produces nothing")
      (is (= :critical (by-summary "StageSet is not Ready (RollbackStoreFailed)")))
      (is (= :info (by-summary "StageSet is not Ready (AwaitingPromotion)")))
      (is (= :warning (by-summary "StageSet is not Ready (BudgetExhausted)"))))))

(deftest held-update-is-info-test
  (is (= #{"a new revision is held by the update window"}
         (summaries (stageset/detect-held-updates
                     {stageset/stageset-type
                      {:items [{:metadata {:namespace "apps" :name "web"}
                                :status {:pendingUpdate "v2.1.0" :nextWindowOpens "2026-07-06T02:00:00Z"}}]}})))))

(deftest package-registers-delivery-scope-and-capability-test
  (is (seq (registry/for-scope :delivery)) "the package fills the :delivery scan scope")
  (is (contains? (set (capabilities/registered)) :stageset)))
