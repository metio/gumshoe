;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.reads-test
  "The three-state reads: the pure classification of every kubectl answer,
   the collection against a stubbed shell, the polarity of the predicates,
   extractors and refusals, the unserved-type versus failed-discovery split,
   the pinned kube context and the read cache."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.reads :as reads]
            [gumshoe.shell :as shell]
            [gumshoe.verify :as verify]))

(def ^:private pod-ref {:type "pods" :namespace "web" :name "api-0"})
(def ^:private crd-ref {:type "listenersets.gateway.networking.x-k8s.io" :namespace "web" :name "edge"})

(defn- ok [out] {:exit 0 :out out :err ""})
(def ^:private pod-json "{\"kind\":\"Pod\",\"metadata\":{\"name\":\"api-0\",\"namespace\":\"web\"}}")
(def ^:private timeout {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 192.0.2.1:6443: i/o timeout"})
(def ^:private forbidden {:exit 1 :out "" :err "Error from server (Forbidden): pods \"api-0\" is forbidden: User \"u\" cannot get resource \"pods\""})
(def ^:private throttled {:exit 1 :out "" :err "Error from server (TooManyRequests): the server has received too many requests"})
(def ^:private not-found {:exit 1 :out "" :err "Error from server (NotFound): pods \"api-0\" not found"})
(defn- no-type [type] {:exit 1 :out "" :err (format "error: the server doesn't have a resource type \"%s\"" type)})

(defn- never [] (throw (AssertionError. "discovery must not be asked")))

;; --- classify-object ---------------------------------------------------------------

(deftest an-object-read-is-absent-only-on-a-clean-answer
  (testing "--ignore-not-found prints nothing for a missing object"
    (is (= {:state :absent :ref pod-ref} (reads/classify-object pod-ref (ok "") never)))
    (is (= {:state :absent :ref pod-ref} (reads/classify-object pod-ref (ok "\n") never))))
  (testing "an empty List is an answer too"
    (is (reads/absent? (reads/classify-object pod-ref (ok "{\"kind\":\"List\",\"items\":[]}") never)))))

(deftest an-object-read-is-present-only-for-the-object-it-named
  (let [r (reads/classify-object pod-ref (ok pod-json) never)]
    (is (= :present (:state r)))
    (is (= "api-0" (get-in r [:object :metadata :name]))))
  (testing "a List holding exactly that object"
    (is (reads/present? (reads/classify-object pod-ref (ok (str "{\"kind\":\"List\",\"items\":[" pod-json "]}")) never))))
  (testing "another name, another namespace, or several objects are not an answer"
    (is (reads/unread? (reads/classify-object pod-ref (ok "{\"metadata\":{\"name\":\"api-1\",\"namespace\":\"web\"}}") never)))
    (is (reads/unread? (reads/classify-object pod-ref (ok "{\"metadata\":{\"name\":\"api-0\",\"namespace\":\"db\"}}") never)))
    (is (reads/unread? (reads/classify-object pod-ref (ok (str "{\"items\":[" pod-json "," pod-json "]}")) never)))
    (is (reads/unread? (reads/classify-object pod-ref (ok "{\"items\":{}}") never))))
  (testing "a cluster-scoped ref answered with a namespaced object read the default namespace"
    (is (reads/unread? (reads/classify-object {:type "pods" :name "api-0"} (ok pod-json) never)))))

(deftest every-other-object-answer-is-unread
  (doseq [[why result] {"unparsable output" (ok "{\"kind\":\"Pod\",\"meta")
                        "a JSON scalar" (ok "42")
                        "a timeout" timeout
                        "an RBAC denial" forbidden
                        "throttling" throttled
                        "a NotFound that --ignore-not-found did not swallow" not-found
                        "a crash with no stderr" {:exit 137 :out "" :err ""}}]
    (testing why
      (let [r (reads/classify-object pod-ref result never)]
        (is (= :unread (:state r)))
        (is (string? (:reason r)))))))

;; --- the unserved type versus a failed discovery ------------------------------------

(deftest a-missing-type-is-absent-only-when-discovery-confirms-it
  (let [asked (atom 0)
        confirm (fn [answer] (fn [] (swap! asked inc) answer))]
    (testing "discovery confirms: absent and marked unserved"
      (is (= {:state :absent :ref crd-ref :unserved true}
             (reads/classify-object crd-ref (no-type (:type crd-ref)) (confirm true)))))
    (testing "discovery does not confirm (a group whose discovery failed): unread"
      (let [r (reads/classify-object crd-ref (no-type (:type crd-ref)) (confirm false))]
        (is (reads/unread? r))
        (is (str/includes? (:reason r) "did not confirm"))))
    (is (= 2 @asked))))

(deftest a-type-without-its-group-is-never-confirmed-absent
  (let [r (reads/classify-object {:type "listenerset" :namespace "web" :name "edge"} (no-type "listenerset") never)]
    (is (reads/unread? r))
    (is (str/includes? (:reason r) "<plural>.<group>"))))

(deftest discovery-is-asked-only-for-the-missing-type-message
  (doseq [result [timeout forbidden throttled not-found (ok "") (ok pod-json)]]
    (is (map? (reads/classify-object crd-ref result never)))
    (is (map? (reads/classify-list crd-ref result never)))))

;; --- classify-list -----------------------------------------------------------------

(deftest a-list-is-present-absent-or-unread
  (let [ref {:type "pods" :namespace "web"}]
    (is (= {:state :present :ref ref :items [{:metadata {:name "api-0"}}]}
           (reads/classify-list ref (ok "{\"items\":[{\"metadata\":{\"name\":\"api-0\"}}]}") never)))
    (is (= {:state :absent :ref ref :items []} (reads/classify-list ref (ok "{\"kind\":\"List\",\"items\":[]}") never)))
    (testing "no output is not an empty list"
      (is (reads/unread? (reads/classify-list ref (ok "") never))))
    (testing "an object instead of a list"
      (is (reads/unread? (reads/classify-list ref (ok pod-json) never))))
    (is (reads/unread? (reads/classify-list ref timeout never)))
    (is (= {:state :absent :ref crd-ref :items [] :unserved true}
           (reads/classify-list crd-ref (no-type (:type crd-ref)) (constantly true))))
    (is (reads/unread? (reads/classify-list crd-ref (no-type (:type crd-ref)) (constantly false))))))

;; --- polarity -----------------------------------------------------------------------

(def ^:private present {:state :present :ref pod-ref :object {:metadata {:name "api-0"}}})
(def ^:private absent {:state :absent :ref pod-ref})
(def ^:private unanswered {:state :unread :ref pod-ref :reason "the read did not answer: i/o timeout"})

(deftest the-predicates-partition-every-value
  (doseq [[r expected] [[present :present] [absent :absent] [unanswered :unread]
                        [nil :unread] [{} :unread] [{:state :gone} :unread] ["present" :unread]]]
    (testing (pr-str r)
      (is (= [(= :present expected) (= :absent expected) (= :unread expected)]
             [(reads/present? r) (reads/absent? r) (reads/unread? r)]))
      (is (= (not= :unread expected) (reads/answered? r))))))

(deftest the-polarity-matrix
  ;; caller kind -> what it may proceed on, for each state
  (let [assert-presence #(nil? (reads/refusal-unless-present %))
        confirm-deletion #(nil? (reads/refusal-unless-absent %))
        skip-object #(reads/on-state % {:present (constantly :act) :absent (constantly :skip) :unread (constantly :refuse)})]
    (is (= [true false false] (mapv assert-presence [present absent unanswered])))
    (is (= [false true false] (mapv confirm-deletion [present absent unanswered])))
    (is (= [:act :skip :refuse] (mapv skip-object [present absent unanswered])))
    (testing "the predicates agree with the refusals"
      (is (= (mapv reads/present? [present absent unanswered]) (mapv assert-presence [present absent unanswered])))
      (is (= (mapv reads/absent? [present absent unanswered]) (mapv confirm-deletion [present absent unanswered]))))))

(deftest refusal-lines-name-the-object-and-the-reason
  (is (= "pods web/api-0 does not exist" (reads/refusal-unless-present absent)))
  (is (= "the database still exists" (reads/refusal-unless-absent present {:what "the database"})))
  (let [line (reads/unread-refusal unanswered {:because "a purge would delete live data"})]
    (is (str/includes? line "could not read pods web/api-0"))
    (is (str/includes? line "i/o timeout"))
    (is (str/includes? line "a purge would delete live data")))
  (is (nil? (reads/unread-refusal present)))
  (is (nil? (reads/unread-refusal absent)))
  (is (string? (reads/unread-refusal nil)) "a malformed result refuses"))

(deftest on-state-needs-every-branch
  (is (thrown? IllegalArgumentException (reads/on-state present {:present identity :absent identity})))
  (is (thrown? IllegalArgumentException (reads/on-state present {:present identity :absent identity :unread :refuse})))
  (is (= :refused (reads/on-state nil {:present (constantly :ok) :absent (constantly :ok) :unread (constantly :refused)}))))

;; --- extractors ---------------------------------------------------------------------

(defn- unread-ex? [f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e (true? (::kubectl/unread (ex-data e))))))

(deftest extracting-from-an-unread-result-throws-a-strict-read-failure
  (is (= {:metadata {:name "api-0"}} (reads/object! present)))
  (is (nil? (reads/object! absent)))
  (is (unread-ex? #(reads/object! unanswered)))
  (is (unread-ex? #(reads/object! nil)))
  (is (= [] (reads/items! {:state :absent :items []})))
  (is (= [{:a 1}] (reads/items! {:state :present :items '({:a 1})})))
  (is (unread-ex? #(reads/items! unanswered)))
  (is (unread-ex? #(reads/value! unanswered))))

(deftest a-derived-value-never-sees-an-unread-read
  (let [host #(get-in % [:metadata :name])]
    (is (= "api-0" (reads/value! (reads/derived present host))))
    (is (nil? (reads/value! (reads/derived absent host))))
    (is (identical? unanswered (reads/derived unanswered (fn [_] (throw (AssertionError. "f ran"))))))
    (testing "the retired-host bug: an unread domain cannot reach a manifest"
      (is (unread-ex? #(str "host: " (reads/value! (reads/derived unanswered host)))))))
  (testing "a list derives over its items, also when none answered"
    (is (= 0 (reads/value! (reads/derived {:state :absent :items []} count))))
    (is (= 2 (reads/value! (reads/derived {:state :present :items [1 2]} count))))))

(deftest a-post-check-built-on-the-extractors-fails-closed
  (let [ctx "ctx"
        checks [{:description "the pod exists"
                 :check #(reads/present? (reads/namespaced-object ctx "web" "pods" "api-0"))}
                {:description "the pod is gone"
                 :verifies-absence? true
                 :check #(reads/absent? (reads/namespaced-object ctx "web" "pods" "api-0"))}
                {:description "the pod's node is set"
                 :check #(some? (get-in (reads/object! (reads/namespaced-object ctx "web" "pods" "api-0")) [:spec :nodeName]))}
                {:description "nothing is left in web"
                 :verifies-absence? true
                 :check #(empty? (reads/items! (reads/listing ctx {:type "pods" :namespace "web"})))}]]
    (is (= [] (verify/fail-closed-findings checks))))
  (testing "verify/attempt reports an extractor on an unanswered read as unread"
    (with-redefs [shell/execute (fn [& _] timeout)]
      (is (string? (verify/attempt #(some? (reads/object! (reads/cluster-object "ctx" "nodes" "n1")))))))))

;; --- collection against a stubbed shell ---------------------------------------------

(defn- stub
  "A shell/execute stub: a --raw read answers what its path maps to, every
   other command the first answer whose predicate holds for its argv."
  [seen answers raw]
  (fn [& argv]
    (swap! seen conj (vec argv))
    (if-let [path (second (drop-while #(not= "--raw" %) argv))]
      (get raw path {:exit 1 :out "" :err (str "unexpected path " path)})
      (or (some (fn [[pred answer]] (when (pred argv) answer)) answers)
          {:exit 1 :out "" :err "unexpected command"}))))

(defn- run
  [answers raw f]
  (let [seen (atom [])]
    {:value (with-redefs [shell/execute (stub seen answers raw)] (f))
     :seen @seen}))

(defn- get? [argv] (some #{"get"} argv))

(deftest a-named-read-asks-for-that-object-and-ignores-not-found
  (let [{:keys [value seen]} (run [[get? (ok pod-json)]] {} #(reads/namespaced-object "prod" "web" "pods" "api-0"))]
    (is (reads/present? value))
    (is (= [["kubectl" "--context=prod" "--request-timeout=30s" "get" "--namespace=web"
             "pods" "api-0" "--ignore-not-found" "--output=json"]]
           seen)))
  (let [{:keys [value seen]} (run [[get? (ok "")]] {} #(reads/cluster-object "prod" "nodes" "n1"))]
    (is (reads/absent? value))
    (is (not-any? #(str/starts-with? % "--namespace") (first seen)))))

(def ^:private group-doc
  "{\"kind\":\"APIGroup\",\"name\":\"gateway.networking.x-k8s.io\",\"versions\":[{\"groupVersion\":\"gateway.networking.x-k8s.io/v1alpha1\",\"version\":\"v1alpha1\"}]}")

(deftest an-unserved-type-reads-absent-and-a-failed-discovery-unread
  (let [missing (no-type (:type crd-ref))]
    (testing "the group is unknown to the API server"
      (let [{:keys [value]} (run [[get? missing]]
                                 {"/apis/gateway.networking.x-k8s.io" {:exit 1 :out "" :err "Error from server (NotFound): the server could not find the requested resource"}}
                                 #(reads/object "ctx" crd-ref))]
        (is (= {:state :absent :ref crd-ref :unserved true} value))))
    (testing "the group is listed, and its version answers without the type"
      (let [{:keys [value]} (run [[get? missing]]
                                 {"/apis/gateway.networking.x-k8s.io" (ok group-doc)
                                  "/apis/gateway.networking.x-k8s.io/v1alpha1" (ok "{\"resources\":[{\"name\":\"xbackendtrafficpolicies\"}]}")}
                                 #(reads/listing "ctx" {:type (:type crd-ref) :namespace "web"}))]
        (is (reads/absent? value))
        (is (= [] (reads/items! value)))))
    (testing "the version's discovery did not answer (an aggregated API server that is down)"
      (let [{:keys [value]} (run [[get? missing]]
                                 {"/apis/gateway.networking.x-k8s.io" (ok group-doc)
                                  "/apis/gateway.networking.x-k8s.io/v1alpha1" {:exit 1 :out "" :err "Error from server (ServiceUnavailable): the server is currently unable to handle the request"}}
                                 #(reads/object "ctx" crd-ref))]
        (is (reads/unread? value))))
    (testing "the group's discovery did not answer"
      (let [{:keys [value]} (run [[get? missing]] {"/apis/gateway.networking.x-k8s.io" timeout}
                                 #(reads/object "ctx" crd-ref))]
        (is (reads/unread? value))))
    (testing "the version lists the type under its singular name or Kind after all"
      (let [{:keys [value]} (run [[get? (no-type "listenerset.gateway.networking.x-k8s.io")]]
                                 {"/apis/gateway.networking.x-k8s.io" (ok group-doc)
                                  "/apis/gateway.networking.x-k8s.io/v1alpha1"
                                  (ok "{\"resources\":[{\"name\":\"listenersets\",\"singularName\":\"listenerset\",\"kind\":\"ListenerSet\"}]}")}
                                 #(reads/object "ctx" {:type "listenerset.gateway.networking.x-k8s.io" :namespace "web" :name "edge"}))]
        (is (reads/unread? value))))
    (testing "resource.version.group is never confirmed unserved"
      (let [{:keys [value seen]} (run [[get? (no-type "deployments.v1.apps")]] {}
                                      #(reads/object "ctx" {:type "deployments.v1.apps" :namespace "web" :name "api"}))]
        (is (reads/unread? value))
        (is (not-any? #(some #{"--raw"} %) seen))))))

(deftest a-listing-names-its-scope-and-selectors
  (let [{:keys [seen]} (run [[get? (ok "{\"items\":[]}")]] {}
                            #(reads/listing "ctx" {:type "pods" :namespace :all :selector "app=api" :field-selector "status.phase=Running"}))]
    (is (= ["kubectl" "--context=ctx" "--request-timeout=30s" "get" "pods" "--all-namespaces"
            "--selector=app=api" "--field-selector=status.phase=Running" "--output=json"]
           (first seen))))
  (let [{:keys [seen]} (run [[get? (ok "{\"items\":[]}")]] {} #(reads/listing "ctx" {:type "nodes"}))]
    (is (= ["kubectl" "--context=ctx" "--request-timeout=30s" "get" "nodes" "--output=json"] (first seen)))))

(deftest a-ref-that-would-list-instead-of-get-is-refused
  (doseq [bad [{:type "pods" :namespace "web" :name nil}
               {:type "pods" :namespace "web" :name ""}
               {:type nil :namespace "web" :name "api-0"}
               {:type "pods" :namespace :all :name "api-0"}]]
    (testing (pr-str bad)
      (is (thrown? IllegalArgumentException (reads/object "ctx" bad)))))
  (is (thrown? IllegalArgumentException (reads/namespaced-object "ctx" nil "pods" "api-0")))
  (is (thrown? IllegalArgumentException (reads/listing "ctx" {:type "pods" :namespace ""}))))

(deftest the-reads-ignore-strict-reads-and-return-unread
  (binding [kubectl/*strict-reads* true]
    (let [{:keys [value]} (run [[get? timeout]] {} #(reads/namespaced-object "ctx" "web" "pods" "api-0"))]
      (is (reads/unread? value)))))

;; --- the pinned kube context ---------------------------------------------------------

(deftest the-reads-follow-the-pinned-kube-context
  (binding [kube-context/*pinned* {:context "staging" :cluster "staging" :announce-as "staging"}]
    (testing "nil reads the pinned context"
      (let [{:keys [seen]} (run [[get? (ok "")]] {} #(reads/cluster-object nil "nodes" "n1"))]
        (is (some #{"--context=staging"} (first seen)))))
    (testing "the pinned context itself"
      (let [{:keys [value]} (run [[get? (ok "")]] {} #(reads/cluster-object "staging" "nodes" "n1"))]
        (is (reads/absent? value))))
    (testing "another context is refused before anything is read"
      (let [seen (atom [])]
        (with-redefs [shell/execute (stub seen [] {})]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"pinned to \"staging\""
                                (reads/cluster-object "production" "nodes" "n1"))))
        (is (= [] @seen))))))

;; --- the read cache ----------------------------------------------------------------

(deftest an-absence-is-served-from-the-read-cache
  (let [{:keys [value seen]} (run [[get? (ok "")]] {}
                                  #(binding [kubectl/*read-cache* (kubectl/read-cache)]
                                     [(reads/cluster-object "ctx" "nodes" "n1")
                                      (reads/cluster-object "ctx" "nodes" "n1")]))]
    (is (every? reads/absent? value))
    (is (= 1 (count seen))))
  (testing "a read that did not answer is asked again"
    (let [{:keys [seen]} (run [[get? timeout]] {}
                              #(binding [kubectl/*read-cache* (kubectl/read-cache)]
                                 (reads/cluster-object "ctx" "nodes" "n1")
                                 (reads/cluster-object "ctx" "nodes" "n1")))]
      (is (= 2 (count seen)))))
  (testing "a blank answer without --ignore-not-found is not cached"
    (let [{:keys [seen]} (run [[get? (ok "")]] {}
                              #(binding [kubectl/*read-cache* (kubectl/read-cache)]
                                 (reads/listing "ctx" {:type "nodes"})
                                 (reads/listing "ctx" {:type "nodes"})))]
      (is (= 2 (count seen))))))

;; --- properties --------------------------------------------------------------------

(def ^:private gen-err
  (gen/elements ["" "error: the server doesn't have a resource type \"pods\""
                 "error: the server doesn't have a resource type \"listenersets.gateway.networking.x-k8s.io\""
                 "Error from server (NotFound): pods \"api-0\" not found"
                 "Error from server (Forbidden): forbidden" "Unable to connect to the server"
                 "Warning: v1 ComponentStatus is deprecated"]))

(def ^:private gen-out
  (gen/one-of [(gen/elements ["" " " "{" "null" "[]" "42" "{\"items\":[]}" "{\"items\":null}"
                              pod-json (str "{\"items\":[" pod-json "]}") (str "{\"items\":[" pod-json "," pod-json "]}")
                              "{\"metadata\":{\"name\":\"other\",\"namespace\":\"web\"}}"])
               gen/string-ascii]))

(def ^:private gen-result
  (gen/hash-map :exit (gen/elements [0 0 0 1 2 124 137]) :out gen-out :err gen-err))

(def ^:private gen-ref
  (gen/elements [pod-ref crd-ref {:type "pods" :name "api-0"} {:type "listenerset" :namespace "web" :name "edge"}]))

(deftest every-answer-classifies-into-exactly-one-state-and-absence-needs-proof
  (let [result
        (tc/quick-check
         500
         (prop/for-all [ref gen-ref
                        result gen-result
                        confirmed? gen/boolean]
           (let [asked (atom false)
                 confirm (fn [] (reset! asked true) confirmed?)
                 o (reads/classify-object ref result confirm)
                 l (reads/classify-list (dissoc ref :name) result confirm)
                 missing-type? (str/includes? (:err result) "doesn't have a resource type")]
             (and
              ;; exactly one state, and the predicates agree
              (every? #(contains? reads/states (:state %)) [o l])
              (every? #(= 1 (count (filter true? [(reads/present? %) (reads/absent? %) (reads/unread? %)]))) [o l])
              ;; a failed command is absent only for a confirmed unserved grouped type
              (or (zero? (:exit result))
                  (not (reads/absent? o))
                  (and missing-type? confirmed? (str/includes? (:type ref) ".") (:unserved o)))
              (or (zero? (:exit result))
                  (not (reads/absent? l))
                  (and missing-type? confirmed? (:unserved l)))
              ;; a clean exit is absent only on no output or an empty list
              (or (not (zero? (:exit result)))
                  (not (reads/absent? o))
                  (str/blank? (:out result))
                  (= [] (:items (kubectl/parse-json (:out result)))))
              ;; discovery is asked only for the missing-type message
              (or missing-type? (not @asked))
              ;; extraction throws exactly for unread
              (= (reads/unread? o) (unread-ex? #(reads/object! o)))
              (= (reads/unread? l) (unread-ex? #(reads/items! l)))
              ;; the refusals follow the predicates
              (= (reads/present? o) (nil? (reads/refusal-unless-present o)))
              (= (reads/absent? o) (nil? (reads/refusal-unless-absent o)))
              (= (reads/unread? o) (string? (reads/unread-refusal o)))))))
        ]
    (is (true? (:pass? result)) (pr-str (select-keys result [:seed :shrunk])))))
