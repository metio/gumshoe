;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dsl-checks-test
  "gumshoe.dsl's :state as several post-checks: what compile refuses, what a
   vector of checks compiles to, and whole runs through
   gumshoe.mutation/run-on! against a fake cluster whose objects move on a
   clock the post-checks wait by, so each check is shown judged within its
   own :timeout and :interval without sleeping."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.change :as change]
            [gumshoe.conformance :as conformance]
            [gumshoe.dsl :as dsl]
            [gumshoe.dsl.registry :as registry]
            [gumshoe.interact :as interact]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.testing.refusal :as refusal]
            [gumshoe.verify :as verify]))

(registry/register! :gumshoe.dsl-checks-test/namespaces
                    {:fn (fn [_context] ["shop"]) :kind :candidates :args ["context"]
                     :doc "the namespaces a test offers"})

;; --- the changes ---------------------------------------------------------------------

(def ^:private asks-for-three
  {:holds [:field [:spec :replicas] := 3]
   :says "Deployment {target} asks for 3 replicas" :timeout 30 :interval 5})

(def ^:private serves-three
  {:holds [:field [:status :readyReplicas] := 3]
   :says "Deployment {target} serves 3 ready replicas" :timeout 900 :interval 30})

(def ^:private stamped
  {:holds [:field [:metadata :labels :stamped] := "yes"]
   :says "Deployment {target} carries the stamp" :timeout 120 :interval 10})

(def ^:private scale
  "Scales a Deployment to three replicas and stamps it, proven by three
   post-checks with windows of their own."
  {:gumshoe/dsl :v0-experimental
   :description "Scales a Deployment to three replicas and stamps it"
   :target {:pick :namespaced :label "Deployment" :type "deployments"
            :namespace-flag :namespace :name-flag :name
            :candidates {:unless :state :order :listed}}
   :prerequisites {}
   :state [asks-for-three serves-three stamped]
   :refuse [{:when [:state] :says "it runs three stamped replicas already"}]
   :confirm {:action "scale a Deployment to three" :destructive? false}
   :announce false
   :effect [[:kubectl :patch {:merge {:spec {:replicas 3} :metadata {:labels {"stamped" "yes"}}}}]]})

(def ^:private stamp-each
  "Stamps every ConfigMap of a namespace, each proven by two post-checks."
  {:gumshoe/dsl :v0-experimental
   :description "Stamps every ConfigMap of a namespace"
   :target {:pick :one :label "namespace" :type "namespaces" :flag :namespace
            :candidates [:call :gumshoe.dsl-checks-test/namespaces [:context]]}
   :prerequisites {}
   :reads {:maps [:list "configmaps" {:namespace [:target]}]}
   :confirm {:action "stamp the ConfigMaps" :destructive? false}
   :announce false
   :effect [[:each :maps
             {:state [{:holds [:field [:metadata :labels :stamped] := "yes"]
                       :says "{each.metadata.name} carries the stamp" :timeout 30 :interval 5}
                      {:holds [:field [:metadata :annotations :owner] := "team"]
                       :says "{each.metadata.name} names its owner" :timeout 60 :interval 10}]}
             [:kubectl :patch {:merge {:metadata {:labels {"stamped" "yes"} :annotations {"owner" "team"}}}}]]]})

;; --- what compile refuses ------------------------------------------------------------

(defn- refusal-of
  [dsl]
  (try (dsl/compile dsl) nil
       (catch clojure.lang.ExceptionInfo e
         (when (dsl/invalid? e) {:path (:path (ex-data e)) :message (ex-message e)}))))

(def ^:private gone {:holds :absent :says "Deployment {target} is gone"})

(def ^:private refused
  [[(assoc scale :state []) [:state] "or a vector of them for several post-checks"]
   [(assoc scale :state "ready") [:state] "or a vector of them for several post-checks"]
   [(assoc scale :state [asks-for-three "ready"]) [:state 1] "each post-check of the :state is a map"]
   [(assoc scale :state [asks-for-three (dissoc serves-three :holds)]) [:state 1 :holds] "says what holds"]
   [(assoc scale :state [asks-for-three (dissoc serves-three :says)]) [:state 1 :says] "non-blank"]
   [(assoc scale :state [asks-for-three (assoc serves-three :wait 3)]) [:state 1 :wait] "not a key of the :state"]
   [(assoc scale :state [asks-for-three (assoc serves-three :says (:says asks-for-three))]) [:state 1 :says]
    "is said by an earlier post-check"]
   [(assoc scale :state [asks-for-three gone]) [:state 1 :holds] ":holds :absent is the :state's only post-check"]
   [(assoc scale :state [gone asks-for-three]) [:state 0 :holds] ":holds :absent is the :state's only post-check"]
   [(assoc scale :state [asks-for-three (assoc serves-three :timeout 0)]) [:state 1 :timeout] "above zero"]
   [(assoc scale :state [asks-for-three (assoc serves-three :timeout 3601)]) [:state 1 :timeout] "at most 3600 seconds"]
   [(assoc scale :state [asks-for-three (assoc serves-three :interval 301)]) [:state 1 :interval] "at most 300 seconds"]
   [(assoc scale :state [asks-for-three (assoc serves-three :timeout 20 :interval 30)]) [:state 1 :interval]
    ":interval 30 is longer than the :timeout 20"]
   [(assoc scale :state [asks-for-three (-> serves-three (dissoc :timeout) (assoc :interval 90))]) [:state 1 :interval]
    "longer than the :timeout 60 it waits within (the default)"]
   [(assoc scale :state (assoc asks-for-three :timeout 7200)) [:state :timeout] "at most 3600 seconds"]
   [(assoc scale :state [asks-for-three (assoc serves-three :holds [:field [:status :readyReplicas] :≈ 3])]) [:state 1 :holds 2]
    "is no comparison"]
   [(assoc scale :state [asks-for-three (assoc serves-three :says "{nothing.here}")]) [:state 1 :says] "nothing.here"]
   [(assoc-in stamp-each [:effect 0 2 :state] []) [:effect 0 2 :state] "or a vector of them"]
   [(assoc-in stamp-each [:effect 0 2 :state 1 :says] "{each.metadata.name} carries the stamp") [:effect 0 2 :state 1 :says]
    "is said by an earlier post-check"]
   [(assoc-in stamp-each [:effect 0 2 :state 1 :interval] 600) [:effect 0 2 :state 1 :interval] "at most 300 seconds"]
   [(assoc stamp-each :effect [[:each :maps {:state [{:holds :absent :says "{each.metadata.name} is gone"}
                                                      {:holds [:some? [:each :metadata]] :says "x"}]}
                                [:kubectl :delete]]]
           :recovery "a deleted ConfigMap is gone")
    [:effect 0 2 :state 0 :holds] ":holds :absent is the :state's only post-check"]
   [(assoc stamp-each :effect [[:each :maps {:state [{:holds [:some? [:each :metadata]] :says "x"}]} [:kubectl :delete]]]
           :recovery "a deleted ConfigMap is gone")
    [:effect 0 2 :state 0 :holds] "brings about an absence"]])

(deftest every-malformed-list-of-post-checks-is-refused-at-its-path
  (doseq [[dsl path words] refused]
    (let [r (refusal-of dsl)]
      (testing (pr-str path words)
        (is (some? r) (str "accepted: " (pr-str (:state dsl))))
        (is (= path (:path r)) (:message r))
        (when words (is (str/includes? (str (:message r)) words) (:message r)))))))

(deftest the-bounds-accept-their-limits
  (doseq [state [[asks-for-three (assoc serves-three :timeout 3600 :interval 300)]
                 [asks-for-three (assoc serves-three :timeout 5 :interval 5)]
                 [(assoc asks-for-three :interval 30)]
                 [asks-for-three]
                 asks-for-three]]
    (is (nil? (refusal-of (assoc scale :state state))) (pr-str state))))

;; --- what it compiles to -------------------------------------------------------------

(def ^:private web
  {:kind "Deployment"
   :metadata {:name "web" :namespace "shop" :uid "uid-web" :generation 1 :resourceVersion "1"}
   :spec {:replicas 1}
   :status {:readyReplicas 1}})

(defn- ctx-of
  [object]
  {:context "ctx" :target "shop/web" :object object
   :ref {:type "deployments" :namespace "shop" :name "web"}})

(deftest several-checks-compile-to-one-post-check-each-with-its-own-window
  (let [c (dsl/compile scale)
        checks ((:verify c) (ctx-of web))]
    (is (= c (change/normalize c)) "normalize finds nothing to change")
    (is (= [["Deployment shop/web asks for 3 replicas" 30 5]
            ["Deployment shop/web serves 3 ready replicas" 900 30]
            ["Deployment shop/web carries the stamp" 120 10]]
           (mapv (juxt :description :timeout :interval) checks)))
    (is (= [] (verify/fail-closed-findings checks)) "every check fails closed")
    (is (true? (:done c)) "the engine joins :done with every post-check")
    (is (= :on (:post-check (dsl/safeguards-of c))))
    (is (= [] (conformance/contract-problems c)))))

(deftest a-single-state-and-a-list-of-one-compile-alike
  (let [single (dsl/compile (assoc scale :state asks-for-three))
        listed (dsl/compile (assoc scale :state [asks-for-three]))
        shape (fn [c] (mapv #(dissoc % :check) ((:verify c) (ctx-of web))))]
    (is (= (shape single) (shape listed)))
    (is (= (dsl/safeguards-of single) (dsl/safeguards-of listed)))
    (is (= (:done single) (:done listed)))))

(deftest the-state-in-a-refusal-and-the-candidates-is-every-check-at-once
  (let [c (dsl/compile scale)
        all (-> web (assoc-in [:spec :replicas] 3) (assoc-in [:status :readyReplicas] 3)
                (assoc-in [:metadata :labels] {:stamped "yes"}))
        some-of (-> all (assoc-in [:metadata :labels] {}))]
    (testing "[:state] refuses only when every check holds"
      (is (= "it runs three stamped replicas already" ((:refusal c) (ctx-of all))))
      (is (nil? ((:refusal c) (ctx-of some-of))))
      (is (nil? ((:refusal c) (ctx-of web)))))
    (testing "{:unless :state} offers an object until every check holds"
      (with-redefs [kubectl/get-all
                    (fn [& _] {:items [(assoc-in all [:metadata :name] "done")
                                       (assoc-in some-of [:metadata :name] "half")
                                       (assoc-in web [:metadata :name] "fresh")]})]
        (is (= ["shop/half" "shop/fresh"] ((get-in c [:select :candidates]) "ctx")))))))

;; --- the fake cluster and its clock ---------------------------------------------------

(defn- parse-argv
  [argv]
  (reduce (fn [acc arg]
            (if-let [[_ k v] (re-matches #"--([^=]+)(?:=(.*))?" arg)]
              (assoc-in acc [:flags k] (or v true))
              (update acc :words conj arg)))
          {:flags {} :words []}
          (rest argv)))

(defn- deep-merge
  [a b]
  (if (and (map? a) (map? b)) (merge-with deep-merge a b) b))

(defn- plural
  [type]
  (if (str/ends-with? type "s") type (str type "s")))

(defn- respond!
  "Answers a kubectl call from state's objects, keyed [type namespace name],
   after applying (:evolve state) to them at the clock's time, so an object
   reaches a state some time after the change, as a controller would bring it."
  [state log clock argv]
  (let [{:keys [flags words]} (parse-argv argv)
        [verb type name] words
        type (some-> type plural)
        ns (get flags "namespace")
        k [type ns name]
        _ (swap! state (fn [{:keys [evolve] :as s}]
                         (if evolve (update s :objects evolve @clock) s)))
        {:keys [objects]} @state]
    (case verb
      "get" (if name
              (if-let [o (get objects k)]
                {:exit 0 :out (json/generate-string o) :err ""}
                (refusal/not-found type name))
              {:exit 0
               :out (json/generate-string {:items (vec (for [[[t n] o] (map (fn [[[t n _] o]] [[t n] o]) objects)
                                                              :when (and (= t type) (= n ns))]
                                                          o))})
               :err ""})
      "patch" (do (swap! log conj [:patch type ns name @clock])
                  (if (contains? objects k)
                    (do (swap! state update-in [:objects k]
                               #(-> (deep-merge % (json/parse-string (get flags "patch") true))
                                    (update-in [:metadata :resourceVersion] (fn [v] (str (inc (parse-long v)))))))
                        {:exit 0 :out "patched" :err ""})
                    (refusal/not-found type name)))
      {:exit 1 :out "" :err (str "the fake cluster does not answer " (pr-str argv))})))

(defn- run-change
  "Runs dsl compiled against target. scenario: :objects the cluster, :evolve
   (fn [objects millis] -> objects) applied at the clock's time before every
   call. The post-checks wait by a clock that starts at 0 and advances by
   every sleep, so nothing sleeps. Returns the result, what was printed, the
   banner, the mutations, each post-check as [description timeout interval
   held? seconds-when-done], and the recorded outcome."
  [dsl target {:keys [objects evolve]}]
  (let [spec (dsl/compile dsl)
        clock (atom 0)
        state (atom {:objects objects :evolve evolve})
        log (atom [])
        checks (atom [])
        eventually verify/eventually
        record (outcome/fresh)
        out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        shell! (fn [& argv] (respond! state log clock (vec argv)))
        streamed (fn [& argv] (:exit (apply shell! argv)))]
    (with-redefs-fn
      {#'shell/execute shell!
       #'shell/run-attributed streamed
       #'shell/run-with-output streamed
       #'interact/confirm! (constantly true)
       #'announce/announce! (constantly nil)
       #'verify/eventually (fn [check]
                             (let [held (eventually check)]
                               (swap! checks conj [(:description check) (:timeout check) (:interval check)
                                                   (true? held) (quot @clock 1000)])
                               held))}
      (fn []
        (let [result (binding [*out* out *err* err
                               outcome/*record* record
                               verify/*clock* {:now (fn [] @clock) :sleep (fn [ms] (swap! clock + (long ms)))}
                               kube-context/*pinned* {:context "ctx" :cluster "cluster"}]
                       (mutation/run-on! spec target {} nil))]
          {:result result
           :printed (stdout/strip-colors (str out err))
           :banner (:text (outcome/banner {:changes? true :outcome (if result :ok :failed)} @record))
           :mutations @log
           :checks @checks
           :record @record})))))

(defn- web-cluster
  [object]
  {["deployments" "shop" "web"] object})

(defn- ready-after
  "Evolves the Deployment to serve its asked replicas once seconds passed."
  [seconds]
  (fn [objects millis]
    (if (<= (* 1000 seconds) millis)
      (update objects ["deployments" "shop" "web"]
              (fn [o] (when o (assoc-in o [:status :readyReplicas] (get-in o [:spec :replicas])))))
      objects)))

;; --- several checks, end to end ------------------------------------------------------

(deftest every-check-holds-within-its-own-window
  (let [{:keys [result checks banner mutations]} (run-change scale "shop/web" {:objects (web-cluster web)
                                                                                :evolve (ready-after 300)})]
    (is (true? result))
    (is (= 1 (count mutations)))
    (is (= [["Deployment shop/web asks for 3 replicas" 30 5 true 0]
            ["Deployment shop/web serves 3 ready replicas" 900 30 true 300]
            ["Deployment shop/web carries the stamp" 120 10 true 300]]
           checks)
        "the rollout takes 300 s, within the 900 s of its own check, beyond the 30 s of the first")
    (is (str/starts-with? banner "DONE - applied and verified") banner)))

(deftest the-one-check-that-fails-is-named
  (let [{:keys [result checks banner printed record]}
        (run-change (assoc-in scale [:effect 0 2 :merge :metadata :labels "stamped"] "no")
                    "shop/web" {:objects (web-cluster web) :evolve (ready-after 300)})]
    (is (false? result))
    (is (= [["Deployment shop/web asks for 3 replicas" 30 5 true 0]
            ["Deployment shop/web serves 3 ready replicas" 900 30 true 300]
            ["Deployment shop/web carries the stamp" 120 10 false 420]]
           checks)
        "the stamp check runs out its own 120 s, every other check holds")
    (is (= ["Deployment shop/web carries the stamp"] (:unverified record)))
    (is (= "NOT VERIFIED - the change ran, but a post-check did not hold: Deployment shop/web carries the stamp - read the messages above"
           banner))
    (is (str/includes? printed "NOT verified within 120s: Deployment shop/web carries the stamp") printed)
    (is (not (str/includes? printed "NOT verified within 900s")) printed)
    (is (str/includes? (get-in record [:fallback :text]) "a post-check did not hold: Deployment shop/web carries the stamp")
        "the guidance under the failed banner names it too")))

(deftest a-rollout-beyond-its-window-fails-and-later-checks-still-run
  (let [{:keys [result checks banner]} (run-change scale "shop/web" {:objects (web-cluster web)
                                                                      :evolve (ready-after 1200)})]
    (is (false? result))
    (is (= [["Deployment shop/web asks for 3 replicas" 30 5 true 0]
            ["Deployment shop/web serves 3 ready replicas" 900 30 false 900]
            ["Deployment shop/web carries the stamp" 120 10 true 900]]
           checks))
    (is (str/includes? banner "a post-check did not hold: Deployment shop/web serves 3 ready replicas") banner)))

(deftest several-failing-checks-are-all-named
  (let [{:keys [banner record]} (run-change (assoc scale :effect [[:kubectl :patch {:merge {:spec {:paused true}}}]])
                                            "shop/web" {:objects (web-cluster web)})]
    (is (= ["Deployment shop/web asks for 3 replicas" "Deployment shop/web serves 3 ready replicas"
            "Deployment shop/web carries the stamp"]
           (:unverified record)))
    (is (str/includes? banner (str "3 post-checks did not hold: Deployment shop/web asks for 3 replicas; "
                                   "Deployment shop/web serves 3 ready replicas; Deployment shop/web carries the stamp"))
        banner)))

(deftest a-change-is-done-only-when-every-check-holds
  (let [all (-> web (assoc-in [:spec :replicas] 3) (assoc-in [:status :readyReplicas] 3)
                (assoc-in [:metadata :labels] {:stamped "yes"}))]
    (testing "every check holds: skipped, nothing patched"
      ;; the subject is picked by name, so the refusal on [:state] does not
      ;; stand in the way of the skip: :done is judged first
      (let [{:keys [result mutations printed]} (run-change (dissoc scale :refuse) "shop/web" {:objects (web-cluster all)})]
        (is (true? result))
        (is (= [] mutations))
        (is (str/includes? printed "skipped, already done") printed)))
    (testing "two of three hold: not done, so the change runs"
      (let [{:keys [result mutations]} (run-change (dissoc scale :refuse) "shop/web"
                                                   {:objects (web-cluster (assoc-in all [:metadata :labels] {}))})]
        (is (true? result))
        (is (= 1 (count mutations)))))))

;; --- several checks inside an :each --------------------------------------------------

(defn- configmap
  [name & {:as metadata}]
  {:kind "ConfigMap"
   :metadata (merge {:name name :namespace "shop" :uid (str "uid-" name) :resourceVersion "1"} metadata)})

(defn- shop
  [& maps]
  (into {["namespaces" nil "shop"] {:kind "Namespace" :metadata {:name "shop" :uid "uid-shop" :resourceVersion "1"}}}
        (map (fn [m] [["configmaps" "shop" (get-in m [:metadata :name])] m]))
        maps))

(deftest every-object-of-an-each-is-proven-by-every-check
  (let [{:keys [result checks mutations]}
        (run-change stamp-each "shop" {:objects (shop (configmap "a")
                                                      (configmap "b" :labels {:stamped "yes"})
                                                      (configmap "c" :labels {:stamped "yes"} :annotations {:owner "team"}))})]
    (is (true? result))
    (is (= [["configmaps" "shop" "a"] ["configmaps" "shop" "b"]] (mapv #(subvec % 1 4) mutations))
        "b holds one check of two, so it is acted on; c holds both and is skipped")
    (is (= [["a carries the stamp" 30 5 true 0] ["a names its owner" 60 10 true 0]
            ["b carries the stamp" 30 5 true 0] ["b names its owner" 60 10 true 0]]
           checks))))

(deftest the-failing-check-of-one-object-is-named
  (let [{:keys [result checks banner]}
        (run-change (assoc-in stamp-each [:effect 0 3 2 :merge :metadata :annotations "owner"] "nobody")
                    "shop" {:objects (shop (configmap "a"))})]
    (is (false? result))
    (is (= [["a carries the stamp" 30 5 true 0] ["a names its owner" 60 10 false 60]] checks))
    (is (str/includes? banner "a post-check did not hold: a names its owner") banner)))

(deftest an-each-whose-objects-hold-every-check-is-done
  (let [{:keys [result mutations printed]}
        (run-change stamp-each "shop" {:objects (shop (configmap "c" :labels {:stamped "yes"} :annotations {:owner "team"}))})]
    (is (true? result))
    (is (= [] mutations))
    (is (str/includes? printed "skipped, already done") printed)))

;; --- negative controls ---------------------------------------------------------------

(deftest control-a-post-check-derivation-keeping-only-the-first-check-misses-the-failure
  (let [derive @#'dsl/post-checks-of]
    (with-redefs-fn {#'dsl/post-checks-of (fn [checks] (derive (take 1 checks)))}
      (fn []
        (let [{:keys [result checks]}
              (run-change (assoc-in scale [:effect 0 2 :merge :metadata :labels "stamped"] "no")
                          "shop/web" {:objects (web-cluster web) :evolve (ready-after 300)})]
          (is (true? result) "with one check derived, the unstamped Deployment would pass")
          (is (= 1 (count checks))))))))

(deftest control-an-each-keeping-only-the-first-check-misses-the-failure
  (let [checks-of @#'dsl/each-checks]
    (with-redefs-fn {#'dsl/each-checks (fn [each ctx] (checks-of (update each :checks #(vec (take 1 %))) ctx))}
      (fn []
        (let [{:keys [result]}
              (run-change (assoc-in stamp-each [:effect 0 3 2 :merge :metadata :annotations "owner"] "nobody")
                          "shop" {:objects (shop (configmap "a"))})]
          (is (true? result) "with one check per object, the wrong owner would pass"))))))

(deftest control-one-shared-window-fails-the-slow-rollout
  (let [eventually verify/eventually]
    (with-redefs [verify/eventually (fn [check] (eventually (assoc check :timeout 60 :interval 5)))]
      (let [{:keys [result record]} (run-change scale "shop/web" {:objects (web-cluster web) :evolve (ready-after 300)})]
        (is (false? result) "a rollout of 300 s fails a 60 s window, so the passing run used its own 900 s")
        (is (= ["Deployment shop/web serves 3 ready replicas"] (:unverified record)))))))
