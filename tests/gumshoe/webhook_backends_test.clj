;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.webhook-backends-test
  "The check that an admission webhook's Service has a ready endpoint. It
   joins four reads - both kinds of webhook configuration, the Services and
   the EndpointSlices - and none of them may stand in for another: a read that
   did not answer makes the check unread and silent. Fixtures are synthetic,
   shaped as the API server and the EndpointSlice controller write them."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.detectives.upgrade :as upgrade]))

;; --- fixtures ---------------------------------------------------------------------

(def ^:private validating "validatingwebhookconfigurations")
(def ^:private mutating "mutatingwebhookconfigurations")

(defn- webhook
  [name namespace service & {:keys [policy timeout]}]
  (cond-> {:name name :clientConfig {:service {:namespace namespace :name service}}}
    policy (assoc :failurePolicy policy)
    timeout (assoc :timeoutSeconds timeout)))

(defn- configuration-of [kind name & webhooks]
  {:apiVersion "admissionregistration.k8s.io/v1"
   :kind kind
   :metadata {:name name}
   :webhooks (vec webhooks)})

(defn- validating-of [name & webhooks] (apply configuration-of "ValidatingWebhookConfiguration" name webhooks))
(defn- mutating-of [name & webhooks] (apply configuration-of "MutatingWebhookConfiguration" name webhooks))

(defn- service-of [namespace name & {:keys [type]}]
  {:apiVersion "v1"
   :kind "Service"
   :metadata {:namespace namespace :name name}
   :spec (cond-> {:type (or type "ClusterIP")}
           (= "ExternalName" type) (assoc :externalName "policy.example.org"))})

(defn- endpoint [address & {:as conditions}]
  {:addresses [address] :conditions (or conditions {})})

(defn- slice-of [namespace service name & endpoints]
  {:apiVersion "discovery.k8s.io/v1"
   :kind "EndpointSlice"
   :metadata {:namespace namespace :name name :labels {:kubernetes.io/service-name service}}
   :addressType "IPv4"
   :endpoints (vec endpoints)})

(defn- evidence
  "The four reads; nil for a read that did not answer."
  [validatings mutatings services slices]
  {validating (some->> validatings (hash-map :items))
   mutating (some->> mutatings (hash-map :items))
   "services" (some->> services (hash-map :items))
   "endpointslices" (some->> slices (hash-map :items))})

(def ^:private check
  (first (filter #(= "webhook-backends" (:name %)) upgrade/detectives)))

(defn- detect [evidence] (vec ((:detect check) evidence)))

;; --- the table --------------------------------------------------------------------

(def ^:private kyverno (validating-of "kyverno" (webhook "validate.kyverno.svc" "policy" "kyverno")))
(def ^:private injector (mutating-of "injector" (webhook "inject.mesh.io" "mesh" "injector")))
(def ^:private kyverno-service (service-of "policy" "kyverno"))
(def ^:private injector-service (service-of "mesh" "injector"))
(def ^:private kyverno-down (slice-of "policy" "kyverno" "kyverno-x1" (endpoint "10.0.0.1" :ready false)))
(def ^:private kyverno-up (slice-of "policy" "kyverno" "kyverno-x1" (endpoint "10.0.0.1" :ready true)))
(def ^:private injector-down (slice-of "mesh" "injector" "injector-x1" (endpoint "10.0.1.1" :ready false)))

(def ^:private kyverno-summary
  "ValidatingWebhookConfiguration webhook validate.kyverno.svc calls Service policy/kyverno, which has no ready endpoint")
(def ^:private injector-summary
  "MutatingWebhookConfiguration webhook inject.mesh.io calls Service mesh/injector, which has no ready endpoint")

(def ^:private scenarios
  "label, evidence, the summaries expected, and whether the check is unread."
  [["answered: a validating webhook whose backend is not ready"
    (evidence [kyverno] [] [kyverno-service] [kyverno-down])
    [kyverno-summary] false]
   ["answered: a mutating webhook whose backend is not ready"
    (evidence [] [injector] [injector-service] [injector-down])
    [injector-summary] false]
   ["answered: both kinds, neither backend ready"
    (evidence [kyverno] [injector] [kyverno-service injector-service] [kyverno-down injector-down])
    [kyverno-summary injector-summary] false]
   ["answered: a ready backend"
    (evidence [kyverno] [] [kyverno-service] [kyverno-up])
    [] false]
   ["answered: the Service has no EndpointSlice at all"
    (evidence [kyverno] [] [kyverno-service] [])
    [kyverno-summary] false]
   ["answered: a missing Service is webhook-references' finding"
    (evidence [kyverno] [] [] [])
    [] false]
   ["answered: an ExternalName Service has no endpoints to judge"
    (evidence [kyverno] [] [(service-of "policy" "kyverno" :type "ExternalName")] [])
    [] false]
   ["answered: a configuration that carries no webhooks"
    (evidence [(validating-of "empty")] [] [kyverno-service] [kyverno-down])
    [] false]
   ["unanswered read alone: the ValidatingWebhookConfigurations"
    (evidence nil [injector] [kyverno-service injector-service] [kyverno-down injector-down])
    [] true]
   ["unanswered read alone: the MutatingWebhookConfigurations"
    (evidence [kyverno] nil [kyverno-service injector-service] [kyverno-down injector-down])
    [] true]
   ["unanswered read alone: the Services"
    (evidence [kyverno] [injector] nil [kyverno-down injector-down])
    [] true]
   ["unanswered read alone: the EndpointSlices"
    (evidence [kyverno] [injector] [kyverno-service injector-service] nil)
    [] true]
   ["unanswered read: every one"
    (evidence nil nil nil nil)
    [] true]
   ["no webhooks at all: nothing to judge"
    (evidence [] [] [kyverno-service] [kyverno-down])
    [] true]])

(deftest every-scenario-reports-as-the-table-says
  (doseq [[label evidence summaries unread?] scenarios]
    (testing label
      (is (= summaries (mapv :summary (detect evidence))))
      (is (= unread? (detective/unread? check evidence))))))

(deftest an-unanswered-read-runs-the-check-and-reports-nothing
  ;; an unread check still runs: run-detectives passes it the same evidence
  (doseq [[label evidence _ unread?] scenarios
          :when unread?]
    (testing label
      (is (empty? (detective/run-detectives (detective/mark-unread [check] evidence) evidence))))))

(deftest the-check-declares-what-it-joins-and-judges
  (is (= [validating mutating "services" "endpointslices"] (:requires check)))
  (is (= [validating mutating "services" "endpointslices"] (:joins check)))
  (is (= [validating mutating] (:judges check)))
  (is (not (str/blank? (:description check)))))

(deftest the-check-stays-in-the-upgrade-and-platform-scopes
  (is (some #(= "webhook-backends" (:name %)) (registry/for-scope :upgrade)))
  (is (some #(= "webhook-backends" (:name %)) (registry/for-scope :platform))))

;; --- the JSON report --------------------------------------------------------------

(def ^:private golden-path "tests/gumshoe/webhook_backends_golden.json")

(def ^:private golden-scenarios
  [["a fail-closed validating webhook and an ignoring mutating one, both without a ready backend, beside a serving one"
    (evidence [kyverno (validating-of "gatekeeper" (webhook "check.gatekeeper.sh" "gatekeeper" "gatekeeper"))]
              [(mutating-of "injector" (webhook "inject.mesh.io" "mesh" "injector" :policy "Ignore" :timeout 5))]
              [kyverno-service injector-service (service-of "gatekeeper" "gatekeeper")]
              [kyverno-down (slice-of "gatekeeper" "gatekeeper" "gatekeeper-x1" (endpoint "10.0.2.1" :ready true))])]
   ["the EndpointSlice read did not answer"
    (evidence [kyverno] [] [kyverno-service] nil)]
   ["the MutatingWebhookConfiguration read did not answer"
    (evidence [kyverno] nil [kyverno-service] [kyverno-down])]])

(defn- json-report
  [evidence]
  (let [checked (detective/mark-unread [check] evidence)
        findings (detective/run-detectives checked evidence)]
    (json/parse-string
     (with-out-str (binding [*err* (java.io.StringWriter.)]
                     (detective/report! checked findings "json"))))))

(defn- reports []
  (into (sorted-map) (for [[label evidence] golden-scenarios] [label (json-report evidence)])))

(deftest the-json-findings-are-pinned
  (let [actual (reports)]
    (if (System/getenv "GUMSHOE_RECORD_GOLDEN")
      (spit golden-path (str (json/generate-string actual {:pretty true}) "\n"))
      (is (= (json/parse-string (slurp golden-path)) actual)))))

;; --- no unanswered read passes ----------------------------------------------------

(def ^:private seed 20261125)

(def ^:private gen-namespace (gen/elements ["policy" "mesh"]))
(def ^:private gen-name (gen/elements ["kyverno" "injector" "gone"]))

(def ^:private gen-webhook
  (gen/let [n gen-name ns gen-namespace s gen-name policy (gen/elements [nil "Fail" "Ignore"])]
    (webhook (str n ".example.org") ns s :policy policy)))

(defn- gen-configurations [build]
  (gen/vector (gen/let [n gen-name hooks (gen/vector gen-webhook 0 3)] (apply build n hooks)) 0 3))

(def ^:private gen-services
  (gen/vector (gen/let [ns gen-namespace n gen-name t (gen/elements ["ClusterIP" "ExternalName"])]
                (service-of ns n :type t))
              0 4))

(def ^:private gen-endpoint
  (gen/let [address (gen/elements ["10.0.0.1" "10.0.0.2"])
            ready (gen/elements [true false nil])]
    {:addresses [address] :conditions (cond-> {} (some? ready) (assoc :ready ready))}))

(def ^:private gen-slices
  (gen/vector (gen/let [ns gen-namespace s gen-name es (gen/vector gen-endpoint 0 3)]
                (apply slice-of ns s (str s "-x") es))
              0 4))

(defn- answered-or-not [g] (gen/one-of [(gen/return nil) g]))

(def ^:private gen-evidence-with-an-unanswered-read
  (gen/such-that (fn [e] (some nil? (vals e)))
                 (gen/let [v (answered-or-not (gen-configurations validating-of))
                           m (answered-or-not (gen-configurations mutating-of))
                           s (answered-or-not gen-services)
                           l (answered-or-not gen-slices)]
                   (evidence v m s l))
                 100))

(defn- no-unanswered-read-passes
  [detective]
  (prop/for-all [e gen-evidence-with-an-unanswered-read]
    (and (detective/unread? detective e)
         (empty? ((:detect detective) e)))))

(deftest no-unanswered-read-yields-a-pass-or-a-finding
  (let [result (tc/quick-check 300 (no-unanswered-read-passes check) :seed seed)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :fail])))))

(deftest the-property-catches-a-check-that-forgets-its-join
  ;; the positive control: without :joins one answered read keeps the check
  ;; from being unread, so the property must find a counterexample
  (is (not (:pass? (tc/quick-check 300 (no-unanswered-read-passes (dissoc check :joins)) :seed seed)))))

(deftest answered-reads-do-produce-findings
  ;; the generators are not vacuous: fully answered evidence reaches findings
  (is (some seq (map detect
                     (gen/sample (gen/let [v (gen-configurations validating-of)
                                           m (gen-configurations mutating-of)
                                           s gen-services
                                           l gen-slices]
                                   (evidence v m s l))
                                 200)))))
