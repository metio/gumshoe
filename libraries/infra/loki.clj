;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.loki
  "Evidence collection for a microservice-mode Loki. The components are separate
   workloads - distributor, ingester, querier, query-frontend, query-scheduler,
   compactor, index-gateway - so the detective discovers them from the namespace
   itself rather than probing one endpoint. When a component that serves the
   hash ring is reachable, the ring state is collected too; an unreachable ring
   is simply not judged."
  (:require [infra.http :as http]
            [infra.kubectl :as kubectl]
            [infra.stdout :as stdout]))

(def ring-paths
  "Component ring name -> HTTP path. The ingester ring lives at /ring for
   historical reasons; the rest follow /<component>/ring."
  {"ingester" "/ring"
   "distributor" "/distributor/ring"
   "ruler" "/ruler/ring"
   "compactor" "/compactor/ring"})

(defn workload-record
  "Pure: the fields we judge a Loki component workload on. The component name is
   the app.kubernetes.io/component label the Loki chart sets (distributor,
   ingester, ...); a workload without it is still recorded under its own name."
  [item]
  {:kind (:kind item)
   :name (get-in item [:metadata :name])
   :component (get-in item [:metadata :labels (keyword "app.kubernetes.io/component")])
   :desired (or (get-in item [:spec :replicas]) 1)
   :ready (get-in item [:status :readyReplicas] 0)})

(defn workloads-from
  "Pure: the Loki workload records in one namespace from raw deployment and
   statefulset items."
  [items namespace]
  (->> items
       (filter #(= namespace (get-in % [:metadata :namespace])))
       (mapv workload-record)))

(defn collect-workloads!
  "The Loki component workloads (deployments and statefulsets) in the namespace
   matching the selector. This is the microservice view: every component, its
   desired replicas, and how many are ready."
  [{:keys [context namespace selector]}]
  (stdout/print-section "🔍 Evidence (Loki components)")
  (stdout/err-println (format "  %s namespace %s (selector %s)" (stdout/blue "▸") namespace selector))
  (let [items (kubectl/items-of (kubectl/get-selected context "deployments,statefulsets" selector))
        workloads (workloads-from items namespace)]
    (doseq [{:keys [component name]} workloads]
      (stdout/err-println (str "    " (stdout/blue "▸") " " (or component name))))
    {:now (java.time.Instant/now)
     "namespace" namespace
     "workloads" workloads}))

(defn collect-rings!
  "The hash ring state from a reachable ring-serving component (a port-forward to
   the distributor). Best-effort: an unreachable ring yields no evidence, and
   the workload view already tells the health story on its own."
  [base-url]
  (stdout/print-section "🔍 Evidence (Loki hash rings)")
  (stdout/err-println (format "  %s %s" (stdout/blue "▸") base-url))
  (let [rings (mapv (fn [[ring path]]
                      (future [ring (http/fetch (str base-url path))]))
                    ring-paths)]
    {"url" base-url
     "rings" (into {} (map deref) rings)}))
