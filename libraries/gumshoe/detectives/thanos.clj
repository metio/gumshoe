;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.thanos
  "Detectives for Thanos Query, judging its HTTP API: readiness, the connected
   store endpoints (a query layer with no stores answers nothing), and rule
   evaluation health."
  (:require [clojure.string :as str]))

(defn detect-readiness
  [evidence]
  (let [url (get evidence "url")
        ready (get evidence "ready")]
    (cond
      (not (:reachable ready))
      [{:severity :critical
        :component url
        :summary "Thanos Query is unreachable"
        :hint (:error ready)}]

      (not= 200 (:status ready))
      [{:severity :critical
        :component url
        :summary (format "readiness endpoint returned HTTP %s" (:status ready))
        :hint "the query layer is up but not ready to serve"}]

      :else [])))

(defn store-endpoints
  "Flattens the /api/v1/stores data map into a seq of endpoints tagged with
   their store type (sidecar, store, receive, rule)."
  [evidence]
  (for [[store-type endpoints] (-> evidence (get "stores") :json :data)
        endpoint endpoints]
    (assoc endpoint :store-type (name store-type))))

(defn detect-stores
  [evidence]
  (let [stores (get evidence "stores")
        endpoints (store-endpoints evidence)]
    (cond
      (not (:reachable stores))
      []

      (empty? endpoints)
      [{:severity :critical
        :component (get evidence "url")
        :summary "Thanos Query has no connected stores"
        :hint "no sidecars or store gateways are attached - every query returns empty"}]

      :else
      (for [endpoint endpoints
            :when (not (str/blank? (str (:lastError endpoint))))]
        {:severity :critical
         :component (:name endpoint)
         :summary (format "%s store endpoint reports an error" (:store-type endpoint))
         :hint (:lastError endpoint)}))))

(defn detect-rules
  [evidence]
  (for [group (-> evidence (get "rules") :json :data :groups)
        rule (:rules group)
        :when (= "err" (:health rule))]
    {:severity :critical
     :component (format "%s/%s" (:name group) (:name rule))
     :summary "rule fails to evaluate"
     :hint (:lastError rule)}))

(def detectives
  [{:name "thanos-readiness"
    :description "Thanos Query is up and ready to serve"
    :requires ["ready" "url"]
    :detect detect-readiness}
   {:name "thanos-stores"
    :description "Store endpoints are connected and error-free"
    :requires ["stores" "url"]
    :detect detect-stores}
   {:name "thanos-rules"
    :description "Recording and alerting rules evaluate cleanly"
    :requires ["rules"]
    :detect detect-rules}])
