;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.flows-test
  (:require [cheshire.core]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.calico.flows :as flows]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.flows :as detectives]
            [gumshoe.http :as http]))

(defn- summaries [findings] (set (map :summary findings)))

(def ^:private cert-manager-deny
  "The flow that blocked every HTTP-01 self-check on 2026-11-06: cert-manager to
   an envoy proxy on the gateway Service's target port, denied at the end of the
   default tier because the rule named port 80 and DNAT had rewritten it."
  {:source_namespace "cert-manager"
   :source_name "cert-manager-99f7fcb7f-*"
   :dest_namespace "tigera-gateway"
   :dest_name "calico-gateway-*"
   :action "Deny"
   :reporter "Src"
   :protocol "tcp"
   :dest_port 10080
   :packets_in 0
   :packets_out 182
   :policies {:enforced [{:kind "EndOfTier" :tier "kube-admin" :action "Pass"
                          :trigger {:kind "GlobalNetworkPolicy" :name "kube-dns" :tier "kube-admin"}}
                         {:kind "EndOfTier" :tier "default" :action "Deny"
                          :trigger {:kind "CalicoNetworkPolicy" :name "deny-all" :namespace "cert-manager"}}]
              :pending [{:kind "EndOfTier" :tier "default" :action "Deny"
                         :trigger {:kind "CalicoNetworkPolicy" :name "cert-manager" :namespace "cert-manager"}}]}})

(def ^:private staged-would-deny
  {:source_namespace "prometheus-general"
   :source_name "prometheus-general-0"
   :dest_namespace "loki"
   :dest_name "loki-gateway-*"
   :action "Allow"
   :protocol "tcp"
   :dest_port 8080
   :packets_in 12
   :packets_out 14
   :policies {:enforced [{:kind "CalicoNetworkPolicy" :tier "default" :action "Allow"
                          :trigger {:kind "CalicoNetworkPolicy" :name "loki" :namespace "loki"}}]
              :pending [{:kind "EndOfTier" :tier "default" :action "Deny"
                         :trigger {:kind "StagedCalicoNetworkPolicy" :name "default-deny" :namespace "loki"}}]}})

(deftest flows-url-takes-a-negative-window-test
  (is (= "https://whisker.example.org/whisker-backend/flows?startTimeGte=-3600"
         (flows/flows-url "https://whisker.example.org" 3600)))
  (testing "a trailing slash does not double up"
    (is (= "https://whisker.example.org/whisker-backend/flows?startTimeGte=-60"
           (flows/flows-url "https://whisker.example.org/" 60)))))

(deftest basic-auth-header-is-omitted-without-credentials-test
  (is (= {} (flows/basic-auth-header nil nil)))
  (is (= {} (flows/basic-auth-header "user" "")))
  (is (= {"Authorization" "Basic dXNlcjpwYXNz"} (flows/basic-auth-header "user" "pass"))))

(deftest normalise-reads-the-decisive-verdict-test
  (let [row (flows/normalise cert-manager-deny)]
    (is (= "Deny" (:enforced-action row)))
    (is (= "EndOfTier" (:enforced-kind row)))
    (is (= "default" (:enforced-tier row)))
    (is (= "CalicoNetworkPolicy cert-manager/deny-all" (:enforced-policy row)))
    (is (= 10080 (:port row)))))

(deftest denied-flows-group-by-path-test
  (let [evidence {:flows (mapv flows/normalise [cert-manager-deny cert-manager-deny])}
        findings (detectives/detect-denied-flows evidence)
        finding (first findings)]
    (is (= 1 (count findings)) "two flows on one path report once")
    (is (= "cert-manager -> tigera-gateway:10080" (:component finding)))
    (is (= "2 denied flows over the window" (:summary finding)))
    (testing "the hint carries the deciding policy and both refinements"
      (is (re-find #"CalicoNetworkPolicy cert-manager/deny-all" (:hint finding)))
      (is (re-find #"no rule matched - denied at the end of tier default" (:hint finding)))
      (is (re-find #"surfaces as a client timeout \(182 packets out" (:hint finding))))))

(deftest one-denied-flow-reads-singular-test
  (let [findings (detectives/detect-denied-flows {:flows [(flows/normalise cert-manager-deny)]})]
    (is (= #{"1 denied flow over the window"} (summaries findings)))))

(deftest a-host-network-source-is-named-host-test
  (let [row (flows/normalise (assoc cert-manager-deny :source_namespace nil
                                    :source_name "worker-1.example.org"))
        findings (detectives/detect-denied-flows {:flows [row]})]
    (is (= "host -> tigera-gateway:10080" (:component (first findings))))
    (is (re-find #"host/worker-1.example.org" (:hint (first findings))))))

(deftest staged-disagreement-is-critical-when-staging-would-deny-test
  (let [findings (detectives/detect-staged-disagreement {:flows [(flows/normalise staged-would-deny)]})]
    (is (= 1 (count findings)))
    (is (= :critical (:severity (first findings))))
    (is (= #{"staged policy would change this verdict: Allow -> Deny"} (summaries findings)))))

(deftest collect-evidence-parses-every-envelope-test
  (testing "a bare array, an items envelope, newline-delimited objects, and SSE data lines"
    (doseq [[label json-body body] [["array" [cert-manager-deny] nil]
                                    ["items" {:items [cert-manager-deny]} nil]
                                    ["flows" {:flows [cert-manager-deny]} nil]]]
      (is (= 1 (count (flows/parse-body body json-body))) label)))
  (let [line (cheshire.core/generate-string cert-manager-deny)]
    (is (= 1 (count (flows/parse-body line nil))) "newline-delimited")
    (is (= 1 (count (flows/parse-body (str "data: " line) nil))) "server-sent event")))

(deftest unauthorised-api-reports-the-credentials-test
  (let [findings (detectives/detect-api-unreachable {:url "https://whisker.example.org/whisker-backend/flows"
                                                     :reachable true :status 401 :body "nope"})]
    (is (= 1 (count findings)))
    (is (re-find #"whisker credentials" (:hint (first findings))))))

(deftest server-error-points-at-the-receive-limit-test
  (let [findings (detectives/detect-api-unreachable {:url "https://whisker.example.org/whisker-backend/flows"
                                                     :reachable true :status 500
                                                     :body "{\"error\":\"Internal Server Error\"}"})]
    (is (= 1 (count findings)))
    (is (re-find #"4 MiB receive limit" (:hint (first findings))))
    (is (re-find #"Internal Server Error" (:hint (first findings))))))

(deftest watch-url-streams-the-same-window-test
  (is (= "https://whisker.example.org/whisker-backend/flows?startTimeGte=-60&watch=true"
         (flows/watch-url "https://whisker.example.org/" 60))))

(deftest unreachable-api-is-critical-test
  (let [findings (detectives/detect-api-unreachable {:url "https://whisker.example.org"
                                                     :reachable false :error "connection refused"})]
    (is (= #{"the Whisker flow API did not answer"} (summaries findings)))))

(defn- unread-checks
  "The names of the flow checks a report would list as unread for evidence."
  [evidence]
  (set (map :name (filter :unread (detective/mark-unread detectives/detectives evidence)))))

(deftest window-flows-is-nil-unless-the-api-answered-2xx-test
  (let [line (cheshire.core/generate-string cert-manager-deny)]
    (is (nil? (flows/window-flows {:reachable false :error "refused"} nil nil)) "unreachable")
    (is (nil? (flows/window-flows {:reachable true :status 401} "nope" nil)) "401")
    (is (nil? (flows/window-flows {:reachable true :status 500} line nil)) "500 with a flow body")
    (is (= 1 (count (flows/window-flows {:reachable true :status 200} line nil))) "200 with a flow")))

(deftest window-flows-is-nil-for-a-2xx-body-that-is-not-json-test
  (is (nil? (flows/window-flows {:reachable true :status 200}
                                "<!doctype html>\n<html><body>Whisker</body></html>" nil))
      "the web app's index.html answered for the backend path"))

(deftest window-flows-keeps-an-empty-window-empty-test
  (is (= [] (flows/window-flows {:reachable true :status 200} "" nil)) "a blank body")
  (is (= [] (flows/window-flows {:reachable true :status 200} "[]" [])) "an empty array")
  (is (= [] (flows/window-flows {:reachable true :status 200} ": keepalive\nevent: ping\n" nil))
      "a stream of nothing but event framing"))

(deftest flow-checks-are-unread-when-the-api-did-not-answer-test
  (doseq [[label response] [["unreachable" {:reachable false :error "connection refused"}]
                            ["non-2xx" {:reachable true :status 503 :body "busy"}]
                            ["html" {:reachable true :status 200 :body "<html></html>"}]]]
    (with-redefs [http/fetch (fn [& _] response)
                  http/fetch-lines! (fn [& _] (assoc response :lines []))]
      (let [evidence (flows/collect-evidence! {:url "https://whisker.example.org" :window 60})]
        (is (nil? (:flows evidence)) label)
        (is (= #{"calico-denied-flows" "calico-staged-disagreement"} (unread-checks evidence)) label)))))

(deftest flow-checks-read-an-answered-empty-window-test
  (with-redefs [http/fetch (fn [& _] {:reachable true :status 200 :body "[]" :json []})]
    (let [evidence (flows/collect-evidence! {:url "https://whisker.example.org" :window 60})]
      (is (= [] (:flows evidence)))
      (is (= #{} (unread-checks evidence))))))

(deftest a-2xx-without-a-flow-log-is-critical-test
  (let [findings (detectives/detect-api-unreachable {:url "https://whisker.example.org/whisker-backend/flows"
                                                     :reachable true :status 200
                                                     :body "<html></html>" :flows nil})]
    (is (= #{"the Whisker flow API answered 200 without a flow log"} (summaries findings)))
    (is (= :critical (:severity (first findings))))))

(deftest a-2xx-with-a-flow-log-answers-test
  (is (empty? (detectives/detect-api-unreachable {:url "https://whisker.example.org" :reachable true
                                                  :status 200 :flows []}))))
