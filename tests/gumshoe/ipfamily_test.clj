;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ipfamily-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.ipfamily :as ipfamily]))

(deftest ip-family-test
  (testing "classifies addresses and CIDRs by family"
    (is (= :ipv4 (ipfamily/ip-family "10.0.0.5")))
    (is (= :ipv4 (ipfamily/ip-family "10.244.0.0/24")))
    (is (= :ipv6 (ipfamily/ip-family "fd00::5")))
    (is (= :ipv6 (ipfamily/ip-family "fd00::/64")))
    (is (nil? (ipfamily/ip-family "not-an-ip")))))

(defn- node [name internal-ips pod-cidrs]
  {:metadata {:name name}
   :status {:addresses (concat (for [ip internal-ips] {:type "InternalIP" :address ip})
                               [{:type "Hostname" :address name}])}
   :spec {:podCIDRs pod-cidrs}})

(deftest node-families-test
  (testing "a node's families come from its InternalIPs and pod CIDRs, not its hostname"
    (is (= #{:ipv4 :ipv6} (ipfamily/node-families (node "n1" ["10.0.0.1" "fd00::1"] ["10.244.0.0/24" "fd00:10::/64"]))))
    (is (= #{:ipv4} (ipfamily/node-families (node "n2" ["10.0.0.2"] ["10.244.1.0/24"]))))))

(def dual-stack-nodes
  {:items [(node "worker-1" ["10.0.0.1" "fd00::1"] ["10.244.0.0/24" "fd00:10::/64"])
           (node "worker-2" ["10.0.0.2"] ["10.244.1.0/24"])]})   ; v4-only, the odd one out

(deftest detect-node-families-test
  (testing "a v4-only node in an otherwise dual-stack cluster is a warning"
    (let [findings (ipfamily/detect-node-families {"nodes" dual-stack-nodes})]
      (is (= 1 (count findings)))
      (is (= "worker-2" (:component (first findings))))
      (is (= :warning (:severity (first findings))))))
  (testing "a uniformly single-stack cluster raises nothing - it is intentional"
    (is (empty? (ipfamily/detect-node-families
                 {"nodes" {:items [(node "a" ["10.0.0.1"] ["10.244.0.0/24"])
                                   (node "b" ["10.0.0.2"] ["10.244.1.0/24"])]}})))))

(deftest detect-cluster-stack-test
  (testing "reports dual-stack as an info line"
    (let [[finding] (ipfamily/detect-cluster-stack {"nodes" dual-stack-nodes})]
      (is (= :info (:severity finding)))
      (is (= "cluster is dual-stack (IPv4, IPv6)" (:summary finding))))))

(deftest detect-service-families-test
  (let [services {:items [{:metadata {:namespace "moodle" :name "web"}
                           :spec {:ipFamilyPolicy "RequireDualStack" :ipFamilies ["IPv4" "IPv6"]
                                  :clusterIP "10.1.0.1" :clusterIPs ["10.1.0.1"]}}       ; got one, wanted two
                          {:metadata {:namespace "moodle" :name "api"}
                           :spec {:ipFamilyPolicy "RequireDualStack" :ipFamilies ["IPv4" "IPv6"]
                                  :clusterIP "10.1.0.2" :clusterIPs ["10.1.0.2" "fd00::2"]}}  ; satisfied
                          {:metadata {:namespace "moodle" :name "headless"}
                           :spec {:ipFamilyPolicy "PreferDualStack" :ipFamilies ["IPv4" "IPv6"]
                                  :clusterIP "None" :clusterIPs ["None"]}}]}]             ; headless - skip
    (testing "a dual-stack request that got one family is flagged; a satisfied or headless one is not"
      (let [findings (ipfamily/detect-service-families {"services" services})]
        (is (= ["moodle/web"] (map :component findings)))
        (is (= :warning (:severity (first findings))))))))
