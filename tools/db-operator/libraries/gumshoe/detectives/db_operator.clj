;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.db-operator
  "Detective for db-operator: Database resources that are not ready."
  (:require [gumshoe.kubectl :as kubectl]))

(def database-type "databases.kinda.rocks")

(defn detect-database-problems
  [evidence]
  (for [database (kubectl/items-of (get evidence database-type))
        :when (not (true? (-> database :status :status)))]
    {:severity :critical
     :component (kubectl/namespace-name-of database)
     :summary (format "database is not ready (phase: %s)"
                      (or (-> database :status :phase) "unknown"))
     :hint "check the db-operator logs and the database instance it connects to"}))

(def detectives
  [{:name "db-operator"
    :description "db-operator Database resources that are not ready"
    :requires [database-type]
    :detect detect-database-problems}])
