;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.cnpg]))

(defn- summaries [findings] (set (map :summary findings)))

(deftest cnpg-detective-test
  (let [evidence {cnpg/cluster-type
                  {:items [{:metadata {:namespace "moodle" :name "database"}
                            :spec {:instances 3}
                            :status {:phase "Failing over"
                                     :readyInstances 1
                                     :conditions [{:type "ContinuousArchiving" :status "False"
                                                   :message "WAL archive check failed"}]}}
                           {:metadata {:namespace "fine" :name "database"}
                            :spec {:instances 2}
                            :status {:phase "Cluster in healthy state"
                                     :readyInstances 2
                                     :conditions [{:type "ContinuousArchiving" :status "True"}]}}]}}
        findings (cnpg/detect-cnpg-problems evidence)]
    (is (= #{"cluster phase: Failing over"
             "only 1 of 3 instances are ready"
             "continuous WAL archiving is failing"}
           (summaries findings)))))

(deftest cnpg-backup-detective-test
  (is (= #{"backup failed" "scheduled backup is suspended"}
         (summaries (cnpg/detect-backup-problems
                     {cnpg/backup-type
                      {:items [{:metadata {:namespace "moodle" :name "nightly-1"}
                                :status {:phase "failed" :error "connection refused"}}
                               {:metadata {:namespace "fine" :name "nightly-2"}
                                :status {:phase "completed"}}]}
                      cnpg/scheduled-backup-type
                      {:items [{:metadata {:namespace "moodle" :name "nightly"}
                                :spec {:suspend true}}]}})))))

(deftest package-registers-databases-scope-and-capability-test
  (is (seq (registry/for-scope :databases)) "the package joins the :databases scan scope")
  (is (contains? (set (capabilities/registered)) :cnpg)))
