;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.db-operator-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detectives.db-operator :as db-operator]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.db-operator]))

(defn- summaries [findings] (set (map :summary findings)))

(deftest db-operator-detective-test
  (is (= #{"database is not ready (phase: Creating)"}
         (summaries (db-operator/detect-database-problems
                     {db-operator/database-type
                      {:items [{:metadata {:namespace "moodle" :name "moodle-db"}
                                :status {:status false :phase "Creating"}}
                               {:metadata {:namespace "fine" :name "keycloak-db"}
                                :status {:status true :phase "Ready"}}]}})))))

(deftest package-registers-databases-scope-and-capability-test
  (is (seq (registry/for-scope :databases)) "the package joins the :databases scan scope")
  (is (contains? (set (capabilities/registered)) :db-operator)))
