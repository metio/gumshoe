;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pooler-endpoints-test
  "The check that a Pooler's Service resolves to a ready address. Fixtures are
   synthetic, shaped as CloudNativePG and the EndpointSlice controller write
   them: CNPG names a Pooler's Service after the Pooler and selects its pods by
   cnpg.io/poolerName."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.pooler :as pooler]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.cnpg]))

;; --- fixtures ---------------------------------------------------------------------

(defn- pooler-of
  "A Pooler; instances :absent leaves spec.instances out, as an object the API
   server never defaulted would read."
  [namespace name instances]
  {:apiVersion "postgresql.cnpg.io/v1"
   :kind "Pooler"
   :metadata {:namespace namespace :name name}
   :spec (cond-> {:cluster {:name "db"} :type "rw"}
           (not= :absent instances) (assoc :instances instances))})

(defn- service-of
  "The Service CNPG creates for a Pooler: named after it, selecting its pods."
  [namespace name & {:keys [selects type]}]
  {:apiVersion "v1"
   :kind "Service"
   :metadata {:namespace namespace :name name}
   :spec (cond-> {:type (or type "ClusterIP")
                  :selector {:cnpg.io/poolerName (or selects name)}}
           (= "ExternalName" type) (-> (dissoc :selector) (assoc :externalName "db.example.org")))})

(defn- endpoint [address & {:as conditions}]
  {:addresses [address] :conditions (or conditions {})})

(defn- slice-of [namespace service name & endpoints]
  {:apiVersion "discovery.k8s.io/v1"
   :kind "EndpointSlice"
   :metadata {:namespace namespace :name name :labels {:kubernetes.io/service-name service}}
   :addressType "IPv4"
   :endpoints (vec endpoints)})

(defn- evidence
  "The three reads; nil for a read that did not answer."
  [poolers services slices]
  {pooler/pooler-type (some->> poolers (hash-map :items))
   "services" (some->> services (hash-map :items))
   "endpointslices" (some->> slices (hash-map :items))})

(def ^:private check
  (first (filter #(= "cnpg-pooler-endpoints" (:name %)) pooler/detectives)))

(defn- detect [evidence] (vec ((:detect check) evidence)))

;; --- the table --------------------------------------------------------------------

(def ^:private rw (pooler-of "db" "pooler-rw" 1))
(def ^:private rw-service (service-of "db" "pooler-rw"))

(def ^:private scenarios
  "label, evidence, the summaries expected, and whether the check is unread."
  [["ready: one ready endpoint"
    (evidence [rw] [rw-service] [(slice-of "db" "pooler-rw" "s1" (endpoint "10.0.0.1" :ready true))])
    [] false]
   ["ready: ready unset reads as ready"
    (evidence [rw] [rw-service] [(slice-of "db" "pooler-rw" "s1" (endpoint "10.0.0.1"))])
    [] false]
   ["not ready: the single replica's pod is not ready"
    (evidence [rw] [rw-service] [(slice-of "db" "pooler-rw" "s1" (endpoint "10.0.0.1" :ready false :serving false))])
    ["serves nothing: Service db/pooler-rw has no ready address while the Pooler declares 1 instance"] false]
   ["not ready: two declared instances, neither ready"
    (evidence [(pooler-of "db" "pooler-rw" 2)] [rw-service]
              [(slice-of "db" "pooler-rw" "s1" (endpoint "10.0.0.1" :ready false) (endpoint "10.0.0.2" :ready false))])
    ["serves nothing: Service db/pooler-rw has no ready address while the Pooler declares 2 instances"] false]
   ["zero instances: a Pooler scaled to 0 serves nothing on purpose"
    (evidence [(pooler-of "db" "pooler-rw" 0)] [rw-service] [])
    [] false]
   ["absent instances: CNPG defaults spec.instances to 1"
    (evidence [(pooler-of "db" "pooler-rw" :absent)] [rw-service] [])
    ["serves nothing: Service db/pooler-rw has no ready address while the Pooler declares 1 instance"] false]
   ["no slice: the Service has no EndpointSlice at all"
    (evidence [rw] [rw-service] [])
    ["serves nothing: Service db/pooler-rw has no ready address while the Pooler declares 1 instance"] false]
   ["no slice: an empty slice"
    (evidence [rw] [rw-service] [(slice-of "db" "pooler-rw" "s1")])
    ["serves nothing: Service db/pooler-rw has no ready address while the Pooler declares 1 instance"] false]
   ["no Service: the Service CNPG names after the Pooler does not exist"
    (evidence [rw] [] [])
    ["serves nothing: Service db/pooler-rw does not exist while the Pooler declares 1 instance"] false]
   ["unanswered read: the EndpointSlices"
    (evidence [rw] [rw-service] nil)
    [] true]
   ["unanswered read: the Services"
    (evidence [rw] nil [])
    [] true]
   ["unanswered read: the Poolers"
    (evidence nil [rw-service] [])
    [] true]
   ["unanswered read: every one"
    (evidence nil nil nil)
    [] true]
   ["no Pooler: nothing to judge"
    (evidence [] [rw-service] [])
    [] true]
   ["multiple slices: one of them holds a ready endpoint"
    (evidence [rw] [rw-service]
              [(slice-of "db" "pooler-rw" "s1" (endpoint "10.0.0.1" :ready false))
               (slice-of "db" "pooler-rw" "s2" (endpoint "10.0.0.2" :ready true))])
    [] false]
   ["multiple slices: none holds a ready endpoint"
    (evidence [rw] [rw-service]
              [(slice-of "db" "pooler-rw" "s1" (endpoint "10.0.0.1" :ready false))
               (slice-of "db" "pooler-rw" "s2")])
    ["serves nothing: Service db/pooler-rw has no ready address while the Pooler declares 1 instance"] false]
   ["terminating endpoints: the only endpoint is draining"
    (evidence [rw] [rw-service]
              [(slice-of "db" "pooler-rw" "s1" (endpoint "10.0.0.1" :ready false :serving true :terminating true))])
    ["serves nothing: Service db/pooler-rw has no ready address while the Pooler declares 1 instance"] false]
   ["terminating endpoints: a ready replacement beside a draining pod"
    (evidence [rw] [rw-service]
              [(slice-of "db" "pooler-rw" "s1"
                         (endpoint "10.0.0.1" :ready false :serving true :terminating true)
                         (endpoint "10.0.0.2" :ready true))])
    [] false]
   ["another namespace: a ready slice of the same name elsewhere does not count"
    (evidence [rw] [rw-service] [(slice-of "other" "pooler-rw" "s1" (endpoint "10.0.0.1" :ready true))])
    ["serves nothing: Service db/pooler-rw has no ready address while the Pooler declares 1 instance"] false]
   ["a Service of our own selecting the Pooler's pods, empty while CNPG's has an address"
    (evidence [rw] [rw-service (service-of "db" "pooler-extra" :selects "pooler-rw")]
              [(slice-of "db" "pooler-rw" "s1" (endpoint "10.0.0.1" :ready true))])
    ["serves nothing through part of its Services: Service db/pooler-extra has no ready address while the Pooler declares 1 instance"] false]
   ["a Service selecting the Pooler's pods, beside CNPG's, both empty"
    (evidence [rw] [rw-service (service-of "db" "pooler-extra" :selects "pooler-rw")] [])
    ["serves nothing: Service db/pooler-extra has no ready address, Service db/pooler-rw has no ready address while the Pooler declares 1 instance"] false]
   ["an ExternalName Service named after the Pooler has no endpoints to judge"
    (evidence [rw] [(service-of "db" "pooler-rw" :type "ExternalName")] [])
    ["serves nothing: Service db/pooler-rw does not exist while the Pooler declares 1 instance"] false]
   ["two Poolers, one serving"
    (evidence [rw (pooler-of "db" "pooler-ro" 1)] [rw-service (service-of "db" "pooler-ro")]
              [(slice-of "db" "pooler-ro" "s1" (endpoint "10.0.0.5" :ready true))])
    ["serves nothing: Service db/pooler-rw has no ready address while the Pooler declares 1 instance"] false]])

(deftest every-scenario-reports-as-the-table-says
  (doseq [[label evidence summaries unread?] scenarios]
    (testing label
      (let [findings (detect evidence)]
        (is (= summaries (mapv :summary findings)))
        (is (every? #(= :critical (:severity %)) findings) "a pooler serving nothing is a total outage")
        (is (= unread? (detective/unread? check evidence)))))))

(deftest the-finding-names-the-pooler
  (let [[finding] (detect (evidence [rw] [rw-service] [(slice-of "db" "pooler-rw" "s1" (endpoint "10.0.0.1" :ready false))]))]
    (is (= "db/pooler-rw" (:component finding)) "the Pooler, not its pod or Deployment")
    (is (str/includes? (:hint finding) "kubectl -n db get pods -l cnpg.io/poolerName=pooler-rw"))))

(deftest a-missing-service-points-at-the-operator
  (let [[finding] (detect (evidence [rw] [] []))]
    (is (str/includes? (:hint finding) "CNPG creates the Service named after the Pooler"))))

(deftest the-check-declares-what-it-joins-and-judges
  (is (= [pooler/pooler-type "services" "endpointslices"] (:requires check)))
  (is (= [pooler/pooler-type "services" "endpointslices"] (:joins check)))
  (is (= [pooler/pooler-type] (:judges check)))
  (is (not (str/blank? (:description check)))))

(deftest the-check-joins-the-databases-scope
  (is (some #(= "cnpg-pooler-endpoints" (:name %)) (registry/for-scope :databases))))

;; --- the JSON report --------------------------------------------------------------

(def ^:private golden-path "tools/cnpg/tests/gumshoe/pooler_endpoints_golden.json")

(def ^:private golden-scenarios
  [["a single-replica pooler whose pod is not ready, beside a serving one and one scaled to 0"
    (evidence [rw (pooler-of "db" "pooler-ro" 1) (pooler-of "db" "pooler-idle" 0)]
              [rw-service (service-of "db" "pooler-ro") (service-of "db" "pooler-idle")]
              [(slice-of "db" "pooler-rw" "pooler-rw-x1" (endpoint "10.0.0.1" :ready false :serving false))
               (slice-of "db" "pooler-ro" "pooler-ro-x1" (endpoint "10.0.0.5" :ready true))])]
   ["the EndpointSlice read did not answer"
    (evidence [rw] [rw-service] nil)]])

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

(def ^:private gen-endpoint
  (gen/let [address (gen/elements ["10.0.0.1" "10.0.0.2" "10.0.0.3"])
            ready (gen/elements [true false nil])
            terminating (gen/elements [true false nil])]
    {:addresses [address]
     :conditions (cond-> {} (some? ready) (assoc :ready ready) (some? terminating) (assoc :terminating terminating))}))

(def ^:private gen-namespace (gen/elements ["db" "other"]))
(def ^:private gen-name (gen/elements ["pooler-rw" "pooler-ro" "pooler-extra"]))

(def ^:private gen-poolers
  (gen/vector (gen/let [ns gen-namespace n gen-name i (gen/elements [:absent 0 1 2])] (pooler-of ns n i)) 0 4))

(def ^:private gen-services
  (gen/vector (gen/let [ns gen-namespace n gen-name s gen-name] (service-of ns n :selects s)) 0 4))

(def ^:private gen-slices
  (gen/vector (gen/let [ns gen-namespace s gen-name es (gen/vector gen-endpoint 0 3)]
                (apply slice-of ns s (str s "-x") es))
              0 4))

(defn- answered-or-not [g] (gen/one-of [(gen/return nil) g]))

(def ^:private gen-evidence-with-an-unanswered-read
  (gen/such-that (fn [e] (some nil? (vals e)))
                 (gen/let [p (answered-or-not gen-poolers) s (answered-or-not gen-services) l (answered-or-not gen-slices)]
                   (evidence p s l))
                 100))

(defn- no-unanswered-read-passes
  [detective]
  (prop/for-all [e gen-evidence-with-an-unanswered-read]
    (and (detective/unread? detective e)
         (empty? ((:detect detective) e)))))

(deftest no-unanswered-read-yields-a-pass-or-a-zero-endpoint-finding
  (let [result (tc/quick-check 300 (no-unanswered-read-passes check) :seed seed)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :fail])))))

(deftest the-property-catches-a-check-that-forgets-its-join
  ;; the positive control: without :joins one answered read keeps the check
  ;; from being unread, so the property must find a counterexample
  (is (not (:pass? (tc/quick-check 300 (no-unanswered-read-passes (dissoc check :joins)) :seed seed)))))

(deftest answered-reads-do-produce-findings
  ;; the generators are not vacuous: fully answered evidence reaches findings
  (is (some seq (map (fn [e] (detect e))
                     (gen/sample (gen/let [p gen-poolers s gen-services l gen-slices] (evidence p s l)) 200)))))
