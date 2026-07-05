;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.network
  "Detective for network segmentation: namespaces running pods with no
   NetworkPolicy at all. Calico-aware: when GlobalNetworkPolicies exist (via
   the projectcalico.org/v3 aggregated API), traffic is governed globally and
   the per-namespace findings would be noise."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def global-policy-type "globalnetworkpolicies.projectcalico.org")

(defn detect-unprotected-namespaces
  [evidence]
  (let [global (kubectl/items-of (get evidence global-policy-type))
        covered (set (map kubectl/namespace-of
                          (kubectl/items-of (get evidence "networkpolicies"))))
        workload-namespaces (distinct (map kubectl/namespace-of
                                           (kubectl/items-of (get evidence "pods"))))]
    (if (seq global)
      [{:severity :info
        :component "cluster"
        :summary (format "%d calico GlobalNetworkPolicies govern cluster traffic" (count global))
        :hint "per-namespace NetworkPolicy findings are suppressed - review the global policies instead"}]
      (for [namespace (sort workload-namespaces)
            :when (not (str/starts-with? (str namespace) "kube-"))
            :when (not= namespace "fire-drill")
            :when (not (covered namespace))]
        {:severity :info
         :component namespace
         :summary "namespace runs pods without any NetworkPolicy - all traffic is allowed"
         :hint "add a default-deny policy and allow only what the workloads need"}))))

(def detectives
  [{:name "network-policies"
    :description "Namespaces running pods without any NetworkPolicy"
    :requires ["networkpolicies" "pods" global-policy-type]
    :detect detect-unprotected-namespaces}])
