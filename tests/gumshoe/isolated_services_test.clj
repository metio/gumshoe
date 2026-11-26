;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.isolated-services-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.network :as network]))

(defn- pod [name labels & {:keys [ready deleting namespace] :or {ready true namespace "shop"}}]
  {:metadata (cond-> {:namespace namespace :name name :labels labels}
               deleting (assoc :deletionTimestamp "2026-11-19T10:00:00Z"))
   :status {:conditions [{:type "Ready" :status (if ready "True" "False")}]}})

(defn- policy [name selector & {:keys [types ingress egress namespace] :or {namespace "shop"}}]
  {:metadata {:namespace namespace :name name}
   :spec (cond-> {:podSelector selector}
           types (assoc :policyTypes types)
           ingress (assoc :ingress ingress)
           egress (assoc :egress egress))})

(def ^:private web-service
  {:metadata {:namespace "shop" :name "web"} :spec {:selector {:app "web"}}})

(defn- detect [& {:keys [services pods policies global]}]
  (vec (network/detect-isolated-services
        {"services" {:items (or services [web-service])}
         "pods" {:items pods}
         "networkpolicies" {:items policies}
         network/global-policy-type {:items (or global [])}
         network/calico-policy-type {:items []}})))

(def ^:private backends [(pod "web-1" {:app "web"}) (pod "web-2" {:app "web"})])

(deftest policy-types-default-as-the-api-defines
  (is (= ["Ingress"] (network/policy-types (policy "p" {}))))
  (is (= ["Ingress" "Egress"] (network/policy-types (policy "p" {} :egress [{}]))))
  (is (= ["Egress"] (network/policy-types (policy "p" {} :types ["Egress"])))))

(deftest a-default-deny-without-allows-isolates-the-service
  (let [[finding & more] (detect :pods backends :policies [(policy "default-deny" {})])]
    (is (nil? more))
    (is (= :warning (:severity finding)))
    (is (= "shop/web" (:component finding)))
    (is (= "Service's 2 ready backend(s) admit no ingress traffic (NetworkPolicy default-deny)" (:summary finding)))
    (is (re-find #"default-deny policy isolates them" (:hint finding)))
    (is (not (re-find #"Calico" (:hint finding))))))

(deftest any-ingress-rule-admits-something
  (is (empty? (detect :pods backends :policies [(policy "default-deny" {}) (policy "allow-web" {:matchLabels {:app "web"}} :ingress [{:from [{:podSelector {}}]}])])))
  (is (empty? (detect :pods backends :policies [(policy "allow-all" {} :ingress [{}])])) "an empty rule allows everything"))

(deftest a-targeted-policy-without-rules-is-named
  (let [[finding] (detect :pods backends :policies [(policy "lock-web" {:matchExpressions [{:key "app" :operator "In" :values ["web"]}]})])]
    (is (re-find #"NetworkPolicy lock-web" (:summary finding)))
    (is (re-find #"none has an ingress rule" (:hint finding)))))

(deftest only-ingress-isolation-counts
  (is (empty? (detect :pods backends :policies [(policy "egress-only" {} :types ["Egress"])])))
  (is (seq (detect :pods backends :policies [(policy "both" {} :types ["Ingress" "Egress"] :egress [{}])]))
      "egress rules admit no ingress"))

(deftest every-ready-backend-must-be-isolated
  (testing "a backend no policy selects still serves"
    (is (empty? (detect :pods [(pod "web-1" {:app "web" :tier "locked"}) (pod "web-2" {:app "web"})]
                        :policies [(policy "lock" {:matchLabels {:tier "locked"}})]))))
  (testing "unready and terminating backends do not count"
    (let [[finding] (detect :pods [(pod "web-1" {:app "web"}) (pod "web-2" {:app "web"} :ready false) (pod "web-3" {:app "web"} :deleting true)]
                            :policies [(policy "default-deny" {})])]
      (is (re-find #"1 ready backend" (:summary finding)))))
  (testing "no ready backend is the services detective's case"
    (is (empty? (detect :pods [(pod "web-1" {:app "web"} :ready false)] :policies [(policy "default-deny" {})])))))

(deftest policies-apply-within-their-namespace
  (is (empty? (detect :pods backends :policies [(policy "default-deny" {} :namespace "other")])))
  (is (empty? (detect :pods [(pod "web-1" {:app "web"} :namespace "other")] :policies [(policy "default-deny" {} :namespace "other")]))
      "a pod in another namespace is not the Service's backend"))

(deftest services-without-selector-are-skipped
  (is (empty? (detect :services [{:metadata {:namespace "shop" :name "manual"} :spec {}}]
                      :pods backends :policies [(policy "default-deny" {})]))))

(deftest a-nil-pod-selector-selects-nothing
  (is (empty? (detect :pods backends :policies [{:metadata {:namespace "shop" :name "odd"} :spec {}}]))))

(deftest calico-global-policies-hedge-the-hint
  (let [[finding] (detect :pods backends :policies [(policy "default-deny" {})] :global [{:metadata {:name "g"}}])]
    (is (re-find #"a Calico policy may still admit the traffic" (:hint finding)))))

(deftest several-policies-are-listed-sorted
  (let [[finding] (detect :pods backends :policies [(policy "z-lock" {:matchLabels {:app "web"}}) (policy "a-deny" {})])]
    (is (re-find #"NetworkPolicy a-deny, z-lock" (:summary finding)))
    (is (re-find #"none has an ingress rule" (:hint finding)) "not only a default-deny")))
