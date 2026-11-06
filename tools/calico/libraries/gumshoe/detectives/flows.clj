;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.flows
  "Detectives over Calico flow logs. A connectivity probe says whether one path
   works; a flow log says which policy decided, which is the question whenever a
   namespace carries a deny-all. These read the verdict rather than the traffic:
   what was denied and by which policy, and what a staged policy would decide
   differently."
  (:require [clojure.string :as str]))

(defn- denied? [row] (= "Deny" (:action row)))

(defn- where
  "Pure: a namespace/name pair, with a host-network endpoint named as such -
   goldmane reports no namespace for one, and a bare dash reads as missing data."
  [namespace name]
  (if (str/blank? (str namespace))
    (str "host/" (or name "?"))
    (str namespace "/" (or name "?"))))

(defn- flow-label
  [{:keys [source-namespace source-name dest-namespace dest-name protocol port]}]
  (format "%s -> %s %s/%s"
          (where source-namespace source-name)
          (where dest-namespace dest-name)
          (or protocol "?") (str (or port "?"))))

(defn- no-rule-matched? [row] (= "EndOfTier" (:enforced-kind row)))

(defn- silent?
  "A denial with traffic sent and nothing back. The client sees a timeout rather
   than a refusal, which reads as a routing or DNS fault and is neither."
  [row]
  (and (pos? (or (:packets-out row) 0)) (zero? (or (:packets-in row) 0))))

(defn- clip [body] (some-> body str (subs 0 (min 200 (count (str body))))))

(defn detect-api-unreachable
  [evidence]
  (let [{:keys [reachable status error url body]} evidence]
    (cond
      (not reachable) [{:severity :critical
                        :component (str url)
                        :summary "the Whisker flow API did not answer"
                        :hint (or error "no response")}]
      (not (<= 200 (or status 0) 299))
      [{:severity :critical
        :component (str url)
        :summary (format "the Whisker flow API answered %s" status)
        :hint (case status
                401 "basic auth rejected - check the whisker credentials in the password store"
                500 (str "the backend could neither list nor stream the window - it reads "
                         "goldmane over gRPC under a 4 MiB receive limit, so a busy cluster "
                         "exceeds it however short the window; check the whisker-backend log "
                         "for ResourceExhausted. backend body: "
                         (clip body))
                (clip body))}]
      :else [])))

(defn detect-denied-flows
  "Denied flows, grouped by source namespace, destination namespace and port, so
   one broken path reports once however many packets it produced. The hint names
   the deciding policy and the two refinements that change what to look at: a
   denial where no rule matched at all (usually a rule written against the wrong
   port or selector - a pod-to-Service rule naming the Service port is matched
   after DNAT and never fires), and one that reaches the client as a timeout."
  [evidence]
  (for [[[src dst port] rows] (->> (:flows evidence)
                                   (filter denied?)
                                   (group-by (juxt :source-namespace :dest-namespace :port)))
        :let [policies (->> rows (keep :enforced-policy) distinct (str/join ", "))
              tiers (->> rows (filter no-rule-matched?) (keep :enforced-tier) distinct (str/join ", "))
              silent (filter silent? rows)]]
    {:severity :warning
     :component (format "%s -> %s:%s"
                        (if (str/blank? (str src)) "host" src)
                        (or dst "?")
                        (str (or port "?")))
     :summary (if (= 1 (count rows))
                "1 denied flow over the window"
                (format "%d denied flows over the window" (count rows)))
     :hint (str/join "; "
                     (remove nil?
                             [(format "decided by %s"
                                      (if (str/blank? policies) "no policy in the chain" policies))
                              (when-not (str/blank? tiers)
                                (format "no rule matched - denied at the end of tier %s" tiers))
                              (when (seq silent)
                                (format "surfaces as a client timeout (%s packets out, none back)"
                                        (str (apply max (map #(or (:packets-out %) 0) silent)))))
                              (format "e.g. %s" (flow-label (first rows)))]))}))

(defn detect-staged-disagreement
  "Flows whose staged policies would decide differently from the enforced ones.
   Enforcing a staged default-deny breaks exactly these paths, so this is the
   check to run before promoting one."
  [evidence]
  (for [row (:flows evidence)
        :let [{:keys [enforced-action pending-action]} row]
        :when (and enforced-action pending-action (not= enforced-action pending-action))]
    {:severity (if (= "Deny" pending-action) :critical :info)
     :component (flow-label row)
     :summary (format "staged policy would change this verdict: %s -> %s"
                      enforced-action pending-action)
     :hint (format "enforced by %s, staged %s"
                   (or (:enforced-policy row) "no policy")
                   (or (:pending-policy row) "no policy"))}))

(def detectives
  [{:name "calico-flows-api"
    :description "The Whisker flow API answers"
    :requires ["url"]
    :detect detect-api-unreachable}
   {:name "calico-denied-flows"
    :description "Flows denied over the window, grouped by path, with the deciding policy"
    :requires ["flows"]
    :detect detect-denied-flows}
   {:name "calico-staged-disagreement"
    :description "Flows a staged policy would decide differently"
    :requires ["flows"]
    :detect detect-staged-disagreement}])
