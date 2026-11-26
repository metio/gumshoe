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
  (:require [gumshoe.cnpg.instances :as inst]
            [gumshoe.kubectl :as kubectl]))

(def evidence-key "cnpg-pooler-logs")

(def tail-lines
  "Enough to span several server reconnects without pulling a whole shift of
   health-check chatter into memory for every pooler in the cluster."
  400)

(def ^:private pooler-role-selector "cnpg.io/podRole=pooler")

(defn collect
  "One row per pooler pod that is not terminated (inst/terminated?): its
   namespace, name, whether its log was read, and the log itself. A
   terminated pod serves no client, and its log may be gone with its
   container, which would read as an unread log. nil when the pod list did
   not answer (list-pooler-pods returned nil), so a failed list leaves the
   evidence unread rather than reading as a cluster without poolers."
  [list-pooler-pods read-log]
  (when-some [pods (list-pooler-pods)]
    (vec (for [pod (remove inst/terminated? pods)
               :let [namespace (kubectl/namespace-of pod)
                     name (kubectl/name-of pod)]]
           (merge {:namespace namespace :pod name}
                  (read-log namespace name))))))

(defn- list-pooler-pods
  "The pooler pods, nil unless the list read answered."
  [context]
  (let [{:keys [ok? doc]} (kubectl/get-json-result context "pods" "--all-namespaces"
                                                   (str "--selector=" pooler-role-selector))]
    (when ok? (vec (kubectl/items-of doc)))))

(defn collector
  "The evidence collector, for registration through the plugin seam."
  [context _key]
  (collect #(list-pooler-pods context)
           (fn [namespace pod]
             (kubectl/container-logs context namespace pod "pgbouncer" tail-lines))))
