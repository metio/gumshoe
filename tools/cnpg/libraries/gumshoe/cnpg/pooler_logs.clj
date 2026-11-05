;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.pooler-logs
  "PgBouncer logs for the pooler pods of a cluster.

   A pooler that cannot log in to its backend is healthy in every field of the
   resource API: the Deployment is available, the pods are ready, the Pooler
   reports an active phase. The fault exists only in the log, so a scan that
   reads objects alone reports such a cluster clean while no client can reach
   its database.

   A pod whose log could not be read is reported as unread. Silence here is
   missing data, and a log that failed to read is not a log without failures."
  (:require [gumshoe.kubectl :as kubectl]))

(def evidence-key "cnpg-pooler-logs")

(def tail-lines
  "Enough to span several server reconnects without pulling a whole shift of
   health-check chatter into memory for every pooler in the cluster."
  400)

(def ^:private pooler-role-selector "cnpg.io/podRole=pooler")

(defn collect
  "One row per pooler pod: its namespace, name, whether its log was read, and
   the log itself."
  [list-pooler-pods read-log]
  (vec (for [pod (list-pooler-pods)
             :let [namespace (kubectl/namespace-of pod)
                   name (kubectl/name-of pod)]]
         (merge {:namespace namespace :pod name}
                (read-log namespace name)))))

(defn collector
  "The evidence collector, for registration through the plugin seam."
  [context _key]
  (collect #(kubectl/items-of (kubectl/get-selected context "pods" pooler-role-selector))
           (fn [namespace pod]
             (kubectl/container-logs context namespace pod "pgbouncer" tail-lines))))
