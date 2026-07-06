;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.ipfamily
  "Dual-stack consistency: whether the cluster's IPv4/IPv6 families line up across
   nodes and services. Everything here is inferred from the API - node
   `.status.addresses` and `.spec.podCIDRs`, service `.spec.ipFamilies` /
   `.ipFamilyPolicy` / `.clusterIPs`. The kubelet's own `--node-ip` and family
   flags live on the node host, not the API, so this reports what the objects
   show and says so; reading kubelet config (over SSH) and probing whether a
   family actually routes are a separate, later step."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(defn ip-family
  "The family of an IP or CIDR string: :ipv6 when it carries a colon, :ipv4 when a
   dot, else nil."
  [s]
  (let [s (str s)]
    (cond
      (str/includes? s ":") :ipv6
      (str/includes? s ".") :ipv4
      :else nil)))

(defn node-families
  "The set of IP families a node carries, from its InternalIP addresses and pod
   CIDRs."
  [node]
  (into #{}
        (keep ip-family
              (concat (for [address (-> node :status :addresses)
                            :when (= "InternalIP" (:type address))]
                        (:address address))
                      (-> node :spec :podCIDRs)))))

(defn- families-label
  "IPv4, IPv6 - families as a stable, human label (v4 before v6)."
  [families]
  (->> [:ipv4 :ipv6]
       (filter (set families))
       (map {:ipv4 "IPv4" :ipv6 "IPv6"})
       (str/join ", ")))

(defn cluster-families
  "Every IP family present across the nodes."
  [nodes]
  (apply set/union #{} (map node-families (kubectl/items-of nodes))))

(defn detect-cluster-stack
  "One info line naming the cluster's stack, so an operator has the context the
   node and service findings are judged against."
  [evidence]
  (let [families (cluster-families (get evidence "nodes"))]
    (when (seq families)
      [{:severity :info
        :component "cluster"
        :summary (if (> (count families) 1)
                   (str "cluster is dual-stack (" (families-label families) ")")
                   (str "cluster is single-stack " (families-label families)))
        :hint "inferred from node addresses and pod CIDRs, not kubelet config"}])))

(defn detect-node-families
  "Nodes short an IP family the rest of a dual-stack cluster has - workloads of
   that family can not schedule there."
  [evidence]
  (let [nodes (kubectl/items-of (get evidence "nodes"))
        by-node (into {} (map (fn [node] [(kubectl/name-of node) (node-families node)])) nodes)
        cluster (apply set/union #{} (vals by-node))]
    (when (> (count cluster) 1)
      (for [[node families] (sort-by key by-node)
            :let [missing (set/difference cluster families)]
            :when (and (seq families) (seq missing))]
        {:severity :warning
         :component node
         :summary (str "node is " (families-label families) " in a dual-stack cluster")
         :hint (format "missing %s that other nodes carry, so those workloads can not schedule here - inferred from node addresses, not kubelet flags"
                       (families-label missing))}))))

(defn detect-service-families
  "Services that asked for dual-stack but were assigned a single family."
  [evidence]
  (for [service (kubectl/items-of (get evidence "services"))
        :let [spec (:spec service)
              policy (:ipFamilyPolicy spec)
              families (:ipFamilies spec)
              cluster-ips (:clusterIPs spec)]
        ;; headless (clusterIP None) and ExternalName services carry no real
        ;; clusterIPs, so their family count is not a shortfall
        :when (not (contains? #{nil "None"} (:clusterIP spec)))
        :when (or (and (= "RequireDualStack" policy) (< (count cluster-ips) 2))
                  (and (> (count families) 1) (< (count cluster-ips) (count families))))]
    {:severity :warning
     :component (kubectl/namespace-name-of service)
     :summary "service requested dual-stack but was assigned a single family"
     :hint (format "ipFamilyPolicy %s, ipFamilies %s, but clusterIPs %s - the second family was not assigned"
                   (or policy "unset") (vec families) (vec cluster-ips))}))

(def detectives
  [{:name "ip-family-cluster"
    :description "The cluster's IPv4/IPv6 stack, inferred from nodes"
    :requires ["nodes"]
    :detect detect-cluster-stack}
   {:name "ip-family-nodes"
    :description "Nodes missing an IP family the rest of a dual-stack cluster has"
    :requires ["nodes"]
    :detect detect-node-families}
   {:name "ip-family-services"
    :description "Services that requested dual-stack but got a single family"
    :requires ["services"]
    :detect detect-service-families}])
