;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.network
  "Detectives for network segmentation: namespaces running pods with no
   NetworkPolicy at all, and Services whose every ready backend NetworkPolicies
   isolate without admitting any traffic. Calico-aware: when GlobalNetworkPolicies exist (via
   the projectcalico.org/v3 aggregated API), traffic is governed globally and
   the per-namespace findings would be noise."
  (:require [clojure.string :as str]
            [gumshoe.detectives.services :as services]
            [gumshoe.evidence :as evidence]
            [gumshoe.kubectl :as kubectl]))

(def global-policy-type "globalnetworkpolicies.projectcalico.org")

(def unprotected-namespace-joins
  "The reads the namespace check joins: without the NetworkPolicy list every
   namespace running pods would read as unprotected. The GlobalNetworkPolicy
   type is served only where Calico runs, so it is required but not joined."
  ["networkpolicies" "pods"])

(defn detect-unprotected-namespaces
  "Namespaces running pods without any NetworkPolicy, unless Calico
   GlobalNetworkPolicies govern the cluster. Nothing is judged when a joined
   read did not answer."
  [evidence]
  (let [global (kubectl/items-of (get evidence global-policy-type))
        covered (set (map kubectl/namespace-of
                          (kubectl/items-of (get evidence "networkpolicies"))))
        workload-namespaces (distinct (map kubectl/namespace-of
                                           (kubectl/items-of (get evidence "pods"))))]
    (cond
      (not (evidence/joined? evidence unprotected-namespace-joins))
      nil

      (seq global)
      [{:severity :info
        :component "cluster"
        :summary (format "%d calico GlobalNetworkPolicies govern cluster traffic" (count global))
        :hint "per-namespace NetworkPolicy findings are suppressed - review the global policies instead"}]

      :else
      (for [namespace (sort workload-namespaces)
            :when (not (str/starts-with? (str namespace) "kube-"))
            :when (not= namespace "fire-drill")
            :when (not (covered namespace))]
        {:severity :info
         :component namespace
         :summary "namespace runs pods without any NetworkPolicy - all traffic is allowed"
         :hint "add a default-deny policy and allow only what the workloads need"}))))

;; --- Services behind policies that admit nothing -----------------------------------

(defn policy-types
  "The directions a NetworkPolicy isolates. Without policyTypes it isolates
   ingress always, and egress when it has egress rules."
  [policy]
  (or (-> policy :spec :policyTypes)
      (if (-> policy :spec :egress) ["Ingress" "Egress"] ["Ingress"])))

(defn ingress-policies
  "The NetworkPolicies that isolate the pod's ingress."
  [policies pod]
  (filter #(and (= (kubectl/namespace-of %) (kubectl/namespace-of pod))
                (some #{"Ingress"} (policy-types %))
                (kubectl/label-selector-matches? (-> % :spec :podSelector) (-> pod :metadata :labels)))
          policies))

(defn admits-nothing?
  "Whether the pod is isolated for ingress and no policy selecting it has an
   ingress rule. Rules add up across policies, so one rule anywhere admits
   something."
  [policies pod]
  (let [selecting (ingress-policies policies pod)]
    (and (seq selecting) (not-any? #(seq (-> % :spec :ingress)) selecting))))

(defn- default-deny?
  [policy]
  (= {} (-> policy :spec :podSelector)))

(def isolated-service-joins
  "A Service's backends are its ready pods, and the NetworkPolicies say which
   of them admit nothing. Calico's GlobalNetworkPolicies only add a note, and
   a cluster without Calico does not serve them."
  ["services" "pods" "networkpolicies"])

(defn detect-isolated-services
  "Services whose every ready backend is isolated for ingress with no rule
   admitting anything. Nothing is judged when a joined read did not answer."
  [evidence]
  (let [policies (kubectl/items-of (get evidence "networkpolicies"))
        pods (kubectl/items-of (get evidence "pods"))
        calico? (seq (kubectl/items-of (get evidence global-policy-type)))]
    (for [service (when (evidence/joined? evidence isolated-service-joins)
                    (kubectl/items-of (get evidence "services")))
          :let [selector (-> service :spec :selector)
                backends (filter #(and (= (kubectl/namespace-of %) (kubectl/namespace-of service))
                                       (services/selects? selector (-> % :metadata :labels))
                                       (services/ready? %)
                                       (nil? (-> % :metadata :deletionTimestamp)))
                                 pods)]
          :when (and (seq backends) (every? #(admits-nothing? policies %) backends))
          :let [involved (sort-by kubectl/name-of (distinct (mapcat #(ingress-policies policies %) backends)))
                names (str/join ", " (map kubectl/name-of involved))
                only-default-deny? (every? default-deny? involved)]]
      {:severity :warning
       :component (kubectl/namespace-name-of service)
       :summary (format "Service's %d ready backend(s) admit no ingress traffic (NetworkPolicy %s)" (count backends) names)
       :hint (str (if only-default-deny?
                    "the namespace's default-deny policy isolates them and no policy allows any traffic in - add a policy that allows the Service's clients"
                    "the policies that select them isolate ingress but none has an ingress rule - add a rule that allows the Service's clients")
                  (when calico? "; a Calico policy may still admit the traffic before these apply"))})))

(def detectives
  [{:name "network-policies"
    :description "Namespaces running pods without any NetworkPolicy"
    :requires (conj unprotected-namespace-joins global-policy-type)
    :joins unprotected-namespace-joins
    :detect detect-unprotected-namespaces}
   {:name "isolated-services"
    :description "Services whose every ready backend NetworkPolicies isolate without any ingress rule"
    :requires (conj isolated-service-joins global-policy-type)
    :joins isolated-service-joins
    :detect detect-isolated-services}])
