;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.db-operator
  "Detective for db-operator: Database resources that are not ready."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def database-type "databases.kinda.rocks")

(defn- status-details
  "What a kinda.rocks/v1beta1 DatabaseStatus says besides its readiness: the
   engine, the database and user it created, and the operator version that
   last wrote it. The type has no phase field."
  [status]
  (let [parts (keep (fn [[k label]] (when-let [v (not-empty (str (get status k)))] (str label " " v)))
                    [[:engine "engine"] [:database "database"] [:user "user"]
                     [:operatorVersion "operator"]])]
    (when (seq parts) (str/join ", " parts))))

(defn detect-database-problems
  "Databases whose status.status is not true. A Database without any status
   has not been reconciled yet, which reads differently from one the
   operator marked not ready."
  [evidence]
  (for [database (kubectl/items-of (get evidence database-type))
        :let [status (:status database)]
        :when (not (true? (:status status)))]
    {:severity :critical
     :component (kubectl/namespace-name-of database)
     :summary (if (contains? status :status)
                (str "database is not ready"
                     (when-let [details (status-details status)] (str " (" details ")")))
                "database has no status - db-operator has not reconciled it")
     :hint "check the db-operator logs and the database instance it connects to"}))

(def detectives
  [{:name "db-operator"
    :description "db-operator Database resources that are not ready"
    :requires [database-type]
    :detect detect-database-problems}])
