;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.karpenter
  "Detectives for Karpenter (karpenter.sh/v1): NodePools that cannot launch
   nodes - not Ready, nodes that fail to register, a resource limit reached -
   and NodeClaims that never become a Ready node or stay Drifted because
   their replacement is blocked."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]
            [gumshoe.quantity :as quantity]))

(def nodepool-type "nodepools.karpenter.sh")
(def nodeclaim-type "nodeclaims.karpenter.sh")

(def ^:private launch-minutes
  "How long a NodeClaim may take to become a Ready node - a cloud instance
   boots and joins in a few minutes, so this leaves room for a slow image."
  15)

(def ^:private drift-minutes
  "How long a Drifted NodeClaim may wait for its replacement before the
   disruption counts as blocked."
  60)

(def ^:private launch-steps
  "The conditions a NodeClaim passes on its way to Ready, in order."
  ["Launched" "Registered" "Initialized"])

(defn- conditions
  [object]
  (into {} (map (juxt :type identity)) (-> object :status :conditions)))

(defn- minutes-since
  [value now]
  (when-let [at (try (some-> value java.time.Instant/parse) (catch Exception _ nil))]
    (when now (.toMinutes (java.time.Duration/between at ^java.time.Instant now)))))

(defn- reason-text
  [{:keys [reason message]}]
  (str/join " - " (remove str/blank? [reason message])))

(defn- format-amount
  [resource value]
  (let [n (quantity/quantity->number value)]
    (cond
      (nil? n) (str value)
      (= "memory" resource) (format "%.0fGi" (/ n 1073741824.0))
      (== n (Math/floor n)) (str (long n))
      :else (format "%.1f" n))))

(defn reached-limits
  "The resources a NodePool uses at least as much of as its limits allow, as
   [resource used limit] triples."
  [nodepool]
  (let [used (-> nodepool :status :resources)]
    (sort (for [[resource limit] (-> nodepool :spec :limits)
                :let [used-value (get used resource)
                      u (quantity/quantity->number used-value)
                      l (quantity/quantity->number limit)]
                :when (and u l (>= u l))]
            [(name resource) used-value limit]))))

(defn nodepool-findings
  [nodepool]
  (let [by-type (conditions nodepool)
        component (kubectl/name-of nodepool)
        ready (get by-type "Ready")
        registration (get by-type "NodeRegistrationHealthy")
        cause (or (first (filter #(= "False" (:status %)) (map by-type ["ValidationSucceeded" "NodeClassReady"]))) ready)]
    (concat
     (when (= "False" (:status ready))
       [(onset/since
         {:severity :critical
          :component component
          :summary (format "NodePool is not Ready (%s)" (if (= cause ready) (or (:reason ready) "False") (:type cause)))
          :hint (str (some-> (not-empty (reason-text cause)) (str " - "))
                     "Karpenter launches no nodes for it until it is Ready")}
         (:lastTransitionTime ready))])
     (when (= "False" (:status registration))
       [(onset/since
         {:severity :critical
          :component component
          :summary "NodePool's nodes fail to register"
          :hint (str (some-> (not-empty (reason-text registration)) (str " - "))
                     "instances launch but never join the cluster - check the node class's user data, subnets and security groups, and the kubelet's bootstrap credentials")}
         (:lastTransitionTime registration))])
     (for [[resource used limit] (reached-limits nodepool)]
       {:severity :warning
        :component component
        :summary (format "NodePool reached its %s limit (%s of %s)" resource
                         (format-amount resource used) (format-amount resource limit))
        :hint "pending pods that only fit this pool stay Pending - raise spec.limits or let another pool take them"}))))

(defn nodeclaim-findings
  [nodeclaim now]
  (let [by-type (conditions nodeclaim)
        component (kubectl/name-of nodeclaim)
        age (minutes-since (-> nodeclaim :metadata :creationTimestamp) now)
        drifted (get by-type "Drifted")
        drifted-minutes (minutes-since (:lastTransitionTime drifted) now)
        deleting? (some? (-> nodeclaim :metadata :deletionTimestamp))]
    (remove nil?
            [(when (and (not deleting?) (not= "True" (:status (get by-type "Ready")))
                        age (>= age launch-minutes))
               (let [step (or (first (remove #(= "True" (:status (get by-type %))) launch-steps)) "Ready")
                     condition (get by-type step)]
                 (onset/since
                  {:severity :critical
                   :component component
                   :summary (format "NodeClaim is not %s after %d minutes" step age)
                   :hint (str (some-> (not-empty (reason-text condition)) (str " - "))
                              (case step
                                "Launched" "the cloud provider did not create the instance - capacity, quota or the node class's settings"
                                "Registered" "the instance runs but its node never joined the cluster"
                                "Initialized" "the node joined but is not ready - its startup taints, CNI or daemonsets have not settled"
                                "the node is not ready"))}
                  (-> nodeclaim :metadata :creationTimestamp))))
             (when (and (not deleting?) (= "True" (:status drifted))
                        drifted-minutes (>= drifted-minutes drift-minutes))
               (onset/since
                {:severity :warning
                 :component component
                 :summary (format "NodeClaim has been Drifted for %d minutes without being replaced" drifted-minutes)
                 :hint (str (some-> (not-empty (reason-text drifted)) (str " - "))
                            "a disruption budget of its NodePool, a PodDisruptionBudget or a karpenter.sh/do-not-disrupt pod holds it")}
                (:lastTransitionTime drifted)))])))

(defn detect-nodepools
  [evidence]
  (mapcat nodepool-findings (kubectl/items-of (get evidence nodepool-type))))

(defn detect-nodeclaims
  [evidence]
  (mapcat #(nodeclaim-findings % (:now evidence)) (kubectl/items-of (get evidence nodeclaim-type))))

(def detectives
  [{:name "karpenter-nodepools"
    :description "Karpenter NodePools that are not Ready, whose nodes fail to register, or that reached a limit"
    :requires [nodepool-type]
    :detect detect-nodepools}
   {:name "karpenter-nodeclaims"
    :description "Karpenter NodeClaims that never become a Ready node, or stay Drifted"
    :requires [nodeclaim-type]
    :detect detect-nodeclaims}])
