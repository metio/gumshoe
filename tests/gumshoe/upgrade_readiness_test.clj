;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.upgrade-readiness-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.detectives.upgrade :as upgrade]))

;; --- webhook backends ------------------------------------------------------------

(defn- service [namespace name & {:keys [type]}]
  {:metadata {:namespace namespace :name name} :spec (cond-> {} type (assoc :type type))})

(defn- slice [namespace service & readiness]
  {:metadata {:namespace namespace :name (str service "-abc") :labels {:kubernetes.io/service-name service}}
   :endpoints (vec (for [ready readiness] (if (= :unset ready) {} {:conditions {:ready ready}})))})

(defn- configuration [kind-key & webhooks]
  {kind-key {:items [{:metadata {:name "policy"} :webhooks (vec webhooks)}]}})

(defn- webhook [name service & {:keys [policy timeout]}]
  (cond-> {:name name :clientConfig {:service {:namespace "policy" :name service}}}
    policy (assoc :failurePolicy policy)
    timeout (assoc :timeoutSeconds timeout)))

(defn- detect [webhooks services slices]
  (upgrade/detect-webhook-backends
   (merge {"validatingwebhookconfigurations" {:items [{:metadata {:name "policy"} :webhooks (vec webhooks)}]}
           "mutatingwebhookconfigurations" {:items []}
           "services" {:items services}
           "endpointslices" {:items slices}})))

(deftest ready-backends-reads-endpointslices
  (is (= #{"policy/up" "policy/unset"}
         (upgrade/ready-backends {:items [(slice "policy" "up" false true)
                                          (slice "policy" "unset" :unset)
                                          (slice "policy" "down" false false)
                                          (slice "policy" "empty")
                                          {:metadata {:namespace "policy" :name "orphan"} :endpoints [{}]}]}))))

(deftest a-webhook-without-a-ready-backend
  (let [[finding & more] (detect [(webhook "validate.kyverno.svc" "kyverno")]
                                 [(service "policy" "kyverno")]
                                 [(slice "policy" "kyverno" false)])]
    (is (empty? more))
    (is (= :critical (:severity finding)))
    (is (= "policy" (:component finding)))
    (is (= "ValidatingWebhookConfiguration webhook validate.kyverno.svc calls Service policy/kyverno, which has no ready endpoint"
           (:summary finding)))
    (is (str/starts-with? (:hint finding) "failurePolicy Fail")))
  (testing "a Service with no EndpointSlice at all"
    (is (= 1 (count (detect [(webhook "w" "kyverno")] [(service "policy" "kyverno")] [])))))
  (testing "failurePolicy Ignore costs the timeout"
    (let [[finding] (detect [(webhook "w" "kyverno" :policy "Ignore" :timeout 5)] [(service "policy" "kyverno")] [])]
      (is (= :warning (:severity finding)))
      (is (= "failurePolicy Ignore - every request it matches waits 5s before it passes unchecked" (:hint finding))))
    (is (str/includes? (:hint (first (detect [(webhook "w" "kyverno" :policy "Ignore")] [(service "policy" "kyverno")] []))) "10s"))))

(deftest webhooks-left-to-other-checks
  (testing "a ready backend"
    (is (empty? (detect [(webhook "w" "kyverno")] [(service "policy" "kyverno")] [(slice "policy" "kyverno" true)]))))
  (testing "a missing Service is webhook-references' finding"
    (is (empty? (detect [(webhook "w" "gone")] [(service "policy" "kyverno")] []))))
  (testing "an ExternalName Service has no endpoints"
    (is (empty? (detect [(webhook "w" "ext")] [(service "policy" "ext" :type "ExternalName")] []))))
  (testing "a URL webhook names no Service"
    (is (empty? (detect [{:name "w" :clientConfig {:url "https://policy.example/validate"}}] [(service "policy" "kyverno")] []))))
  (testing "the same Service name in another namespace does not count"
    (is (= 1 (count (detect [(webhook "w" "kyverno")] [(service "policy" "kyverno")] [(slice "other" "kyverno" true)])))))
  (testing "without Services or EndpointSlices nothing is judged"
    (is (empty? (upgrade/detect-webhook-backends {"validatingwebhookconfigurations" {:items [{:metadata {:name "p"} :webhooks [(webhook "w" "x")]}]}})))))

(deftest mutating-webhooks-too
  (is (= ["MutatingWebhookConfiguration webhook m calls Service policy/kyverno, which has no ready endpoint"]
         (map :summary (upgrade/detect-webhook-backends
                        (merge (configuration "mutatingwebhookconfigurations" (webhook "m" "kyverno"))
                               {"validatingwebhookconfigurations" {:items []}
                                "services" {:items [(service "policy" "kyverno")]}
                                "endpointslices" {:items []}})))))
  (testing "an unanswered ValidatingWebhookConfiguration read judges no mutating webhook either"
    (is (empty? (upgrade/detect-webhook-backends
                 (merge (configuration "mutatingwebhookconfigurations" (webhook "m" "kyverno"))
                        {"services" {:items [(service "policy" "kyverno")]} "endpointslices" {:items []}}))))))

;; --- kubelet against the API server -------------------------------------------------

(defn- node [name version]
  {:metadata {:name name} :status {:nodeInfo {:kubeletVersion version}}})

(defn- skew [server & nodes]
  (upgrade/detect-kubelet-apiserver-skew {"server-version" {:major "1" :minor server} "nodes" {:items (vec nodes)}}))

(deftest server-minor-tolerates-provider-suffixes
  (is (= 37 (upgrade/server-minor {:major "1" :minor "37"})))
  (is (= 30 (upgrade/server-minor {:major "1" :minor "30+"})))
  (is (nil? (upgrade/server-minor {:major "1" :minor ""})))
  (is (nil? (upgrade/server-minor nil))))

(deftest the-skew-policy
  (is (= 3 (upgrade/allowed-lag 25)))
  (is (= 3 (upgrade/allowed-lag 34)))
  (is (= 2 (upgrade/allowed-lag 24))))

(deftest kubelets-within-the-policy-say-nothing
  (is (empty? (skew "37" (node "a" "v1.37.1") (node "b" "v1.36.4") (node "c" "v1.35.0"))))
  (is (empty? (skew "25" (node "a" "v1.24.17"))) "one behind, for a pre-1.25 kubelet"))

(deftest kubelets-at-the-edge-of-the-policy
  (let [[finding & more] (skew "37" (node "a" "v1.37.1") (node "w-2" "v1.34.3") (node "w-1" "v1.34.3"))]
    (is (empty? more))
    (is (= :warning (:severity finding)))
    (is (= "kubelet 1.34 trails the API server (1.37) by 3 minors, the most the skew policy allows" (:summary finding)))
    (is (= "w-1, w-2 - upgrade these nodes before the control plane's next minor, which would leave them unsupported"
           (:hint finding))))
  (is (= [:warning] (map :severity (skew "26" (node "old" "v1.24.0")))) "a pre-1.25 kubelet two behind 1.26 is at its edge"))

(deftest kubelets-outside-the-policy
  (let [[finding] (skew "37" (node "a" "v1.33.9"))]
    (is (= :critical (:severity finding)))
    (is (= "kubelet 1.33 trails the API server (1.37) by 4 minors, more than the 3 the skew policy allows" (:summary finding)))
    (is (str/includes? (:hint finding) "unsupported")))
  (is (= ["kubelet 1.24 trails the API server (1.27) by 3 minors, more than the 2 the skew policy allows"]
         (map :summary (skew "27" (node "a" "v1.24.0")))))
  (let [[finding] (skew "36" (node "a" "v1.37.0"))]
    (is (= :critical (:severity finding)))
    (is (= "kubelet 1.37 is newer than the API server (1.36)" (:summary finding)))))

(deftest many-nodes-are-sampled
  (is (= "n-1, n-2, n-3 and 2 more - upgrade these nodes before the control plane's next minor, which would leave them unsupported"
         (:hint (first (apply skew "37" (for [i (range 1 6)] (node (str "n-" i) "v1.34.0"))))))))

(deftest skew-needs-the-server-version
  (is (empty? (upgrade/detect-kubelet-apiserver-skew {"nodes" {:items [(node "a" "v1.20.0")]}})))
  (is (empty? (skew "37" (node "a" "weird")))))

;; --- the scope ----------------------------------------------------------------------

(deftest the-upgrade-scope
  (let [names (set (map :name (registry/for-scope :upgrade)))]
    (is (= upgrade/detectives (filter #(names (:name %)) upgrade/detectives)))
    (is (= registry/upgrade-checks names) "every named check resolves to a detective"))
  (testing "the new detectives join the platform scan too"
    (is (every? (set (map :name (registry/for-scope :platform))) (map :name upgrade/detectives))))
  (is (empty? (detective/run-detectives upgrade/detectives {}))))
