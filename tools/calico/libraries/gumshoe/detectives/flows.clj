;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.flows
  "Detectives over Calico flow logs. A connectivity probe says whether one path
   works; a flow log says which policy decided, which is the question whenever a
   namespace carries a deny-all. These read the verdict rather than the traffic:
   what was denied, what a staged policy would decide differently, and which
   denials surface to the client as a timeout instead of a refusal."
  (:require [clojure.string :as str]))

(defn- denied? [row] (= "Deny" (:action row)))

(defn- flow-label
  [{:keys [source-namespace source-name dest-namespace dest-name protocol port]}]
  (format "%s/%s -> %s/%s %s/%s"
          (or source-namespace "?") (or source-name "?")
          (or dest-namespace "?") (or dest-name "?")
          (or protocol "?") (str (or port "?"))))

(defn detect-api-unreachable
  [evidence]
  (let [{:keys [reachable status error url]} evidence]
    (cond
      (not reachable) [{:severity :critical
                        :component (str url)
                        :summary "the Whisker flow API did not answer"
                        :hint (or error "no response")}]
      (not (<= 200 (or status 0) 299)) [{:severity :critical
                                         :component (str url)
                                         :summary (format "the Whisker flow API answered %s" status)
                                         :hint (if (= 401 status)
                                                 "basic auth rejected - check the whisker credentials in the password store"
                                                 (some-> (:body evidence) (subs 0 (min 200 (count (str (:body evidence))))))) }]
      :else [])))

(defn detect-denied-flows
  "Denied flows, grouped by source namespace, destination namespace and port, so
   one broken path reports once however many packets it produced."
  [evidence]
  (for [[[src dst port] rows] (->> (:flows evidence)
                                   (filter denied?)
                                   (group-by (juxt :source-namespace :dest-namespace :port)))
        :let [policies (->> rows (keep :enforced-policy) distinct (str/join ", "))]]
    {:severity :warning
     :component (format "%s -> %s:%s" (or src "?") (or dst "?") (str (or port "?")))
     :summary (format "%d denied flow(s) over the window" (count rows))
     :hint (format "decided by %s; e.g. %s"
                   (if (str/blank? policies) "no policy in the chain" policies)
                   (flow-label (first rows)))}))

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

(defn detect-silent-denies
  "Denials with packets leaving and none returning. The client sees a timeout
   rather than a refusal, which reads as a routing or DNS fault and is neither."
  [evidence]
  (for [row (:flows evidence)
        :when (and (denied? row)
                   (pos? (or (:packets-out row) 0))
                   (zero? (or (:packets-in row) 0)))]
    {:severity :warning
     :component (flow-label row)
     :summary "denied with traffic sent and nothing returned - surfaces as a client timeout"
     :hint (format "%s packets out, 0 in, decided by %s"
                   (str (:packets-out row)) (or (:enforced-policy row) "no policy"))}))

(defn detect-end-of-tier-denies
  "Denials where no rule matched at all, rather than a rule denying. Usually a
   rule written against the wrong port or selector - a pod-to-Service rule
   naming the Service port is matched after DNAT and never fires."
  [evidence]
  (for [row (:flows evidence)
        :when (and (denied? row) (= "EndOfTier" (:enforced-kind row)))]
    {:severity :warning
     :component (flow-label row)
     :summary (format "no rule matched - denied at the end of tier %s"
                      (or (:enforced-tier row) "?"))
     :hint (format "closest policy %s; check the rule's ports and selector against the real destination"
                   (or (:enforced-policy row) "none"))}))

(def detectives
  [{:name "calico-flows-api"
    :description "The Whisker flow API answers"
    :requires ["url"]
    :detect detect-api-unreachable}
   {:name "calico-denied-flows"
    :description "Flows denied over the window, grouped by path"
    :requires ["flows"]
    :detect detect-denied-flows}
   {:name "calico-staged-disagreement"
    :description "Flows a staged policy would decide differently"
    :requires ["flows"]
    :detect detect-staged-disagreement}
   {:name "calico-silent-denies"
    :description "Denials that surface to the client as a timeout"
    :requires ["flows"]
    :detect detect-silent-denies}
   {:name "calico-end-of-tier-denies"
    :description "Denials where no rule matched at all"
    :requires ["flows"]
    :detect detect-end-of-tier-denies}])
