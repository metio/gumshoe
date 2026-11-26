;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]))

(deftest run-on-uses-the-given-target-test
  (testing "run-on! runs the spec against the provided target, skipping selection"
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (constantly true)]
      (let [seen (atom nil)
            spec {:select {:mode :one :label "Namespace"}
                  :confirm {:action "do a thing"}
                  :effect (fn [{:keys [target]}] (reset! seen target) [])}]
        (is (true? (mutation/run-on! spec "ns-1" {} nil)))
        (is (= "ns-1" @seen) "the effect must see the target passed to run-on!")))))

(deftest run-selects-then-runs-on-the-pick-test
  (testing "run! resolves the subject from the flags, then runs on it"
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (constantly true)]
      (let [seen (atom nil)
            spec {:select {:mode :one :label "Namespace" :flag :namespace
                           :candidates (fn [_context] ["ns-1" "ns-2"])}
                  :confirm {:action "do a thing"}
                  :effect (fn [{:keys [target]}] (reset! seen target) [])}]
        (is (true? (mutation/run! spec {:namespace "ns-2"} nil)))
        (is (= "ns-2" @seen) "the provided flag picks ns-2 without a prompt")))))

(deftest run-nothing-to-select-succeeds-quietly-test
  (testing "run! returns true (nothing to do) when there are no candidates"
    (with-redefs [kubectl/current-context (constantly "ctx")]
      (let [ran (atom false)
            spec {:select {:mode :one :label "Namespace" :flag :namespace
                           :candidates (fn [_context] [])}
                  :empty-message "every node is already cordoned"
                  :confirm {:action "x"}
                  :effect (fn [_] (reset! ran true) [])}]
        (is (true? (mutation/run! spec {} nil)))
        (is (false? @ran) "no candidates means the effect is never built or run")))))

(deftest help-describes-the-mutation-from-its-spec
  (is (= ["What it does:"
          "  picks one Namespace (--namespace, or interactively)"
          "  asks to confirm: delete it (destructive - type yes)"
          "  checks again right before acting that what was confirmed still holds"
          "  has no post-check"
          ""
          "Needs:"
          "  tools: kubectl"]
         (mutation/details {:select {:mode :one :label "Namespace" :flag :namespace}
                            :confirm {:action "delete it" :destructive? true}
                            :precondition (constantly true)
                            :prerequisites {:installed-tools ["kubectl"]}}))))

(deftest an-unreadable-cluster-is-not-nothing-to-do
  (let [spec {:select {:mode :one :label "Node" :candidates #(map kubectl/name-of (kubectl/items-of (kubectl/get-all % "nodes")))}
              :empty-message "every node is already cordoned"}
        out (java.io.StringWriter.)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  gumshoe.shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server: dial tcp: i/o timeout"})]
      (binding [*out* out *err* out]
        (is (false? (mutation/run! spec {} {})))))
    (is (not (clojure.string/includes? (str out) "every node is already cordoned")))
    (testing "an answered read with nothing in it is still nothing to do"
      (with-redefs [kubectl/current-context (constantly "ctx")
                    gumshoe.shell/execute (fn [& _] {:exit 0 :out "{\"items\":[]}" :err ""})]
        (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
          (is (true? (mutation/run! spec {} {}))))))
    (testing "a type the cluster does not serve has no candidates"
      (with-redefs [kubectl/current-context (constantly "ctx")
                    gumshoe.shell/execute (fn [& _] {:exit 1 :out "" :err "error: the server doesn't have a resource type \"widgets\""})]
        (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
          (is (true? (mutation/run! spec {} {}))))))))

(defn- derived-release-spec
  "A book that derives the HelmRelease its precondition gates on, recording
   what each later stage was reached with."
  [reached]
  {:select {:mode :namespaced :label "HelmRelease"}
   :confirm {:action "reset the release" :destructive? true}
   :derive (fn [{:keys [context target]}]
             (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
               {:release (kubectl/get-namespaced-resource context namespace "helmreleases" name)
                :pods (vec (kubectl/items-of (kubectl/get-namespaced context namespace "pods")))}))
   :precondition (fn [{:keys [release]}]
                   (swap! reached conj [:precondition release])
                   (not (get-in release [:status :inFlight])))
   :effect (fn [_] (swap! reached conj :effect) [])})

(deftest a-derive-read-that-did-not-answer-never-reaches-the-precondition
  (let [reached (atom [])
        out (java.io.StringWriter.)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (fn [& _] (swap! reached conj :confirm) true)
                  gumshoe.shell/execute (fn [& _] {:exit 1 :out "" :err "Error from server (TooManyRequests): the server has received too many requests"})]
      (binding [*out* out *err* out]
        (is (false? (mutation/run-on! (derived-release-spec reached) "web/moodle" {} nil)))))
    (is (= [] @reached) "neither the confirmation, the precondition nor the effect ran")
    (is (re-find #"TooManyRequests" (str out)) "the reason the read failed is reported")))

(deftest a-derive-read-that-answered-reaches-the-precondition
  (testing "an object that answered NotFound is derived as nil, as a lax read would"
    (let [reached (atom [])]
      (with-redefs [kubectl/current-context (constantly "ctx")
                    kubectl/current-cluster (constantly "cluster")
                    interact/confirm! (constantly true)
                    gumshoe.shell/execute (fn [& _] {:exit 1 :out "" :err "Error from server (NotFound): helmreleases.helm.toolkit.fluxcd.io \"moodle\" not found"})]
        (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
          (is (true? (mutation/run-on! (derived-release-spec reached) "web/moodle" {} nil)))))
      (is (some #{[:precondition nil]} @reached) "the precondition saw the absent release")))
  (testing "a derive that reads lazily is realized while the reads are strict"
    (let [reached (atom [])
          spec (assoc (derived-release-spec reached)
                      :derive (fn [{:keys [context]}]
                                {:names (map kubectl/name-of (kubectl/items-of (kubectl/get-all context "pods")))}))]
      (with-redefs [kubectl/current-context (constantly "ctx")
                    kubectl/current-cluster (constantly "cluster")
                    interact/confirm! (constantly true)
                    gumshoe.shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"})]
        (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
          (is (false? (mutation/run-on! spec "web/moodle" {} nil)))))
      (is (= [] @reached)))))

(defn- precondition-release-spec
  "A book whose precondition reads the HelmRelease it gates on itself."
  [reached]
  {:select {:mode :namespaced :label "HelmRelease"}
   :confirm {:action "reset the release" :destructive? true}
   :precondition (fn [{:keys [context target]}]
                   (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
                         release (kubectl/get-namespaced-resource context namespace "helmreleases" name)]
                     (swap! reached conj [:precondition release])
                     (not (get-in release [:status :inFlight]))))
   :effect (fn [_] (effect/plan (effect/note "reset")))})

(deftest a-precondition-read-that-did-not-answer-refuses-the-change
  (let [reached (atom [])
        out (java.io.StringWriter.)
        ran (atom false)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (constantly true)
                  effect/run! (fn [& _] (reset! ran true) true)
                  gumshoe.shell/execute (fn [& _] {:exit 1 :out "" :err "Error from server (TooManyRequests): the server has received too many requests"})]
      (binding [*out* out *err* out]
        (is (false? (mutation/run-on! (precondition-release-spec reached) "web/moodle" {} nil)))))
    (is (false? @ran) "the effect did not run")
    (is (re-find #"refusing: could not read" (str out)))
    (is (re-find #"TooManyRequests" (str out)) "the reason the read failed is reported")))

(deftest a-precondition-read-that-answered-not-found-reaches-the-precondition
  (let [reached (atom [])]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (constantly true)
                  gumshoe.shell/execute (fn [& _] {:exit 1 :out "" :err "Error from server (NotFound): helmreleases.helm.toolkit.fluxcd.io \"moodle\" not found"})]
      (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
        (is (true? (mutation/run-on! (precondition-release-spec reached) "web/moodle" {} nil)))))
    (is (some #{[:precondition nil]} @reached) "the precondition judged the absent release")))

(deftest a-standalone-change-records-its-recovery-when-it-fails
  (with-redefs [kubectl/current-context (constantly "ctx")
                kubectl/current-cluster (constantly "cluster")
                interact/confirm! (constantly true)]
    (let [record (outcome/fresh)
          spec {:select {:mode :one :label "Namespace"}
                :confirm {:action "do a thing"}
                :derive (fn [_] {:observed :parked})
                :effect (fn [_] [[:note "first"] [:cmd "true"] [:cmd "this-binary-truly-does-not-exist-xyz"]])
                :recovery (fn [{:keys [target observed applied]}] (format "%s is %s after %d" target (name observed) applied))}]
      (binding [*err* (java.io.StringWriter.) *out* (java.io.StringWriter.) outcome/*record* record]
        (is (false? (mutation/run-on! spec "ns-1" {} nil))))
      (is (= {:text "ns-1 is parked after 1"} (:recovery @record))
          "rendered with the derived values and the effects applied"))))

(defn- shape-spec
  [effect-fn]
  {:select {:mode :one :label "Namespace"}
   :confirm {:action "reset the course"}
   :effect effect-fn})

(defmacro ^:private on-a-stubbed-cluster
  "Runs body with kubectl answering 0 and recording what it was asked, the
   confirmation answered yes and recording that it was asked, and the output
   discarded. Binds `ran` and `asked`."
  [& body]
  `(let [~'ran (atom []) ~'asked (atom 0)]
     (with-redefs [kubectl/current-context (constantly "ctx")
                   kubectl/current-cluster (constantly "cluster")
                   interact/confirm! (fn [_#] (swap! ~'asked inc) true)
                   shell/run-with-output (fn [& args#] (swap! ~'ran conj (vec args#)) 0)
                   shell/run-attributed (fn [& args#] (swap! ~'ran conj (vec args#)) 0)]
       (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
         ~@body))))

(deftest a-lone-effect-runs-as-a-plan-of-one
  (testing "an :effect returning one effect applies it"
    (on-a-stubbed-cluster
     (is (true? (mutation/run-on! (shape-spec (fn [_] (effect/kubectl "ctx" "cordon" "w-1"))) "w-1" {} nil)))
     (is (= [["kubectl" "--context=ctx" "cordon" "w-1"]] @ran))))
  (testing "a lone note applies too"
    (on-a-stubbed-cluster
     (is (true? (mutation/run-on! (shape-spec (fn [_] (effect/note "nothing to reset"))) "w-1" {} nil)))))
  (testing "a dry run shows a lone effect and runs nothing"
    (on-a-stubbed-cluster
     (let [shown (java.io.StringWriter.)]
       (binding [flow/*dry-run* true *err* shown]
         (is (true? (mutation/run-on! (shape-spec (fn [_] (effect/kubectl "ctx" "cordon" "w-1"))) "w-1" {} nil))))
       (is (str/includes? (str shown) "would run: kubectl cordon w-1"))
       (is (= [] @ran))))))

(defn- refusal
  "The message run-on! throws for a spec whose :effect returns `returned`, or
   nil when it does not throw."
  [returned & {:keys [dry-run?]}]
  (on-a-stubbed-cluster
   (try (binding [flow/*dry-run* (boolean dry-run?)]
          (mutation/run-on! (shape-spec (constantly returned)) "w-1" {} nil)
          nil)
        (catch clojure.lang.ExceptionInfo e
          {:message (ex-message e) :ran @ran :asked @asked}))))

(deftest an-effect-that-is-not-a-plan-is-refused-naming-the-step
  (doseq [[what returned] [["a keyword" :note]
                           ["a map" {:kubectl "cordon"}]
                           ["a string" "kubectl cordon w-1"]
                           ["a number" 42]
                           ["a seq of non-effects" ["cordon" "w-1"]]
                           ["a plan holding one non-effect" [(effect/kubectl "ctx" "cordon" "w-1") :oops]]]
          dry-run? [false true]]
    (testing (str what (when dry-run? " under --dry-run"))
      (let [{:keys [message ran asked] :as refused} (refusal returned :dry-run? dry-run?)]
        (is (some? refused) "the step is refused")
        (is (str/includes? (str message) "reset the course") "the message names the step")
        (is (str/includes? (str message) (pr-str returned)) "the message says what the :effect returned")
        (is (str/includes? (str message) "an :effect returns a plan"))
        (is (str/includes? (str message) "effect/plan") "the message names the fix")
        (is (not (str/includes? (str message) "ISeq")))
        (is (= [] ran) "no part of the plan runs")
        (is (zero? asked) "the refusal comes before the confirmation")))))

(deftest a-nil-or-empty-plan-behaves-as-before
  (testing "an empty plan runs nothing and succeeds"
    (on-a-stubbed-cluster
     (is (true? (mutation/run-on! (shape-spec (constantly [])) "w-1" {} nil)))
     (is (= [] @ran))))
  (testing "a nil plan is a malformed change, refused without a prompt"
    (on-a-stubbed-cluster
     (is (false? (mutation/run-on! (shape-spec (constantly nil)) "w-1" {} nil)))
     (is (zero? @asked)))))

(defn- failing-after
  "Runs spec against w-1 with every effect but the `n`th succeeding (nil:
   all succeed), and returns the record and the lines printed under the
   banner it leaves."
  [spec n & {:keys [dry-run?]}]
  (let [record (outcome/fresh)
        calls (atom 0)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (constantly true)
                  shell/run-with-output (fn [& _] (if (= n (swap! calls inc)) 1 0))
                  shell/run-attributed (fn [& _] (if (= n (swap! calls inc)) 1 0))]
      (binding [*err* (java.io.StringWriter.) *out* (java.io.StringWriter.)
                outcome/*record* record flow/*dry-run* (boolean dry-run?)]
        (mutation/run-on! spec "w-1" {} nil)))
    {:record @record
     :lines (outcome/recovery-lines :error @record)}))

(defn- three-effects
  [& {:as extra}]
  (merge {:select {:mode :one :label "Node"}
          :confirm {:action "maintain the node"}
          :effect (fn [_] [[:note "cordoning first"]
                           [:kubectl "ctx" "cordon" "w-1"]
                           [:kubectl "ctx" "drain" "w-1"]])}
         extra))

(deftest a-change-without-recovery-that-failed-after-applying-prints-a-fallback
  (let [{:keys [lines]} (failing-after (three-effects) 2)
        text (str/join "\n" lines)]
    (is (= "Recovery:" (first lines)) text)
    (is (str/includes? text "declares no recovery guidance") text)
    (is (str/includes? text "stayed applied") text)
    (is (str/includes? text "kubectl cordon w-1") "names the effect that stayed applied")
    (is (not (str/includes? text "cordoning first")) "a note changed nothing and is not listed")
    (is (str/includes? text "stopped at: kubectl drain w-1") text)
    (is (str/includes? text "w-1 on cluster") "names the object and where it lives")))

(deftest a-change-whose-post-check-failed-after-applying-points-at-the-post-check
  (let [spec (three-effects :verify (fn [_] [{:description "w-1 is drained" :check (constantly false) :timeout 0}])
                            :announce (fn [_] "draining w-1"))
        {:keys [lines]} (failing-after spec nil)
        text (str/join "\n" lines)]
    (is (str/includes? text "kubectl cordon w-1") text)
    (is (str/includes? text "kubectl drain w-1") text)
    (is (str/includes? text "post-check") text)
    (is (str/includes? text "changelog") "an announced change points at its changelog entry")))

(deftest a-change-that-applied-nothing-prints-no-fallback
  (is (nil? (:lines (failing-after (three-effects) 1)))
      "the first change failed - the note before it changed nothing"))

(deftest a-rehearsal-prints-no-fallback
  (is (nil? (:lines (failing-after (three-effects :verify (fn [_] [{:description "never" :check (constantly false) :timeout 0}]))
                                   nil :dry-run? true)))))

(defn- rehearsal-that-throws
  "A --dry-run of spec against w-1, whose throw is caught: {:thrown <message>
   :record <the outcome record>}. A rehearsal runs no post-check and its
   effects always succeed, so a throw is the path that reaches failed!."
  [spec]
  (let [record (outcome/fresh)
        thrown (with-redefs [kubectl/current-context (constantly "ctx")
                             kubectl/current-cluster (constantly "cluster")
                             interact/confirm! (constantly true)]
                 (binding [*err* (java.io.StringWriter.) *out* (java.io.StringWriter.)
                           outcome/*record* record flow/*dry-run* true]
                   (try (mutation/run-on! spec "w-1" {} nil) nil
                        (catch clojure.lang.ExceptionInfo e (ex-message e)))))]
    {:thrown thrown :record @record}))

(deftest a-rehearsal-that-throws-records-no-recovery
  (testing "an :effect builder that throws"
    (let [{:keys [thrown record]} (rehearsal-that-throws
                                   (three-effects :recovery "uncordon w-1 by hand"
                                                  :effect (fn [_] (throw (ex-info "boom" {})))))]
      (is (= "boom" thrown) "the fixture reaches failed! through the catch")
      (is (not (contains? record :recovery)))
      (is (nil? (outcome/recovery-lines :error record)))))
  (testing "a :verify builder that throws once the plan is built"
    (let [{:keys [thrown record]} (rehearsal-that-throws
                                   (three-effects :recovery "uncordon w-1 by hand"
                                                  :verify (fn [_] (throw (ex-info "no checks" {})))))]
      (is (= "no checks" thrown))
      (is (not (contains? record :recovery)))))
  (testing "a spec without :recovery records no fallback"
    (let [{:keys [thrown record]} (rehearsal-that-throws
                                   (three-effects :effect (fn [_] (throw (ex-info "boom" {})))))]
      (is (= "boom" thrown))
      (is (not (contains? record :fallback)))
      (is (not (contains? record :recovery))))))

(deftest a-change-with-its-own-recovery-prints-only-that
  (let [{:keys [lines]} (failing-after (three-effects :recovery "uncordon w-1 by hand") 2)]
    (is (= ["Recovery:" "  uncordon w-1 by hand"] lines))))

(deftest the-fallback-is-a-function-of-what-the-change-left
  (is (= ["This change declares no recovery guidance. What it left behind:"
          "  2 effects of this change stayed applied - nothing was rolled back:"
          "    kubectl cordon w-1"
          "    kubectl drain w-1"
          "  its post-check did not hold"
          "Look at:"
          "  the post-check messages above"
          "  the changelog entry this change posted"
          "  the current state of w-1 on cluster"]
         (str/split-lines (mutation/fallback-guidance {:applied ["kubectl cordon w-1" "kubectl drain w-1"]
                                                       :post-check-failed? true
                                                       :announced? true
                                                       :subject "w-1"
                                                       :cluster "cluster"}))))
  (is (= ["This change declares no recovery guidance. What it left behind:"
          "  1 effect of this change stayed applied - nothing was rolled back:"
          "    kubectl cordon w-1"
          "  it stopped at: kubectl drain w-1"
          "Look at:"
          "  the messages of the effect that failed, above"
          "  the current state of w-1 on cluster"]
         (str/split-lines (mutation/fallback-guidance {:applied ["kubectl cordon w-1"]
                                                       :stopped "kubectl drain w-1"
                                                       :subject "w-1"
                                                       :cluster "cluster"})))))
