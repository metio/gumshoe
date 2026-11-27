;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-problems-test
  "A change's :problems: every message it returns is printed as its own
   refusing line and the gate decides on the same sequence, before the
   confirmation and after it; an empty sequence proceeds; nil, a value that
   is not a sequential collection, a bad message, a throw and a read refuse
   the change; --dry-run prints the messages; normalize refuses :problems
   beside :precondition; and the banner names the step a refusal stopped.
   Runs go through the real flow/change! against a recorded cluster."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.change :as change]
            [gumshoe.conformance :as conformance]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.playbook :as playbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.testing.refusal :as refusal]))

;; --- the recorded cluster -------------------------------------------------------------

(defn- arg [argv prefix]
  (some #(when (and (string? %) (str/starts-with? % prefix)) (subs % (count prefix))) argv))

(defn- key-of
  [argv]
  (let [[_ type name] (drop-while #(not= "get" %) argv)]
    [type (arg argv "--namespace=") name]))

(defn- answer
  [objects k]
  (if-let [o (get objects k)]
    {:exit 0 :out (json/generate-string o) :err ""}
    {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" (first k) (last k))}))

(def ^:private settings-key ["configmaps" "batch" "settings"])

(defn- settings
  [blockers]
  {:metadata {:namespace "batch" :name "settings" :uid "settings-uid"}
   :data {:blockers blockers}})

(defn- read-settings
  [{:keys [context]}]
  (kubectl/get-namespaced-resource context "batch" "configmaps" "settings"))

(defn- blockers-of
  "The :problems the specs below declare: the blockers the settings read
   holds, each a message."
  [{:keys [settings]}]
  (vec (get-in settings [:data :blockers])))

(defn- spec-with
  [problems & {:as extra}]
  (merge {:select {:mode :namespaced :label "CronJob"}
          :title "suspend nightly"
          :confirm {:action "suspend a CronJob"}
          :reads {:settings read-settings}
          :problems problems
          :effect (fn [_] (effect/plan (effect/note "go")))}
         extra))

(defn- run
  "Runs spec on batch/nightly through mutation/run-on! and the real
   flow/change! against a cluster whose settings hold blockers, with a
   fresh outcome record. `confirming`, when given, edits the blockers while
   the confirmation is shown. Returns the result, what was printed, the
   refusing lines, how many confirmations were asked, the effects that ran
   and the banner."
  [spec blockers & {:keys [confirming dry-run?]}]
  (let [cluster (atom {settings-key (settings blockers)})
        asked (atom 0)
        ran (atom [])
        record (outcome/fresh)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  shell/execute (fn [& argv] (answer @cluster (key-of argv)))
                  interact/confirm! (fn [_]
                                      (swap! asked inc)
                                      (when confirming
                                        (swap! cluster assoc settings-key (settings confirming)))
                                      true)
                  effect/run! (fn [plan] (swap! ran conj plan) true)]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out
                             flow/*dry-run* (boolean dry-run?)
                             outcome/*record* record]
                     (mutation/run-on! spec "batch/nightly" {} nil))
            printed (stdout/strip-colors (str out))]
        {:result result
         :out printed
         :refusing (mapv second (re-seq #"(?m)^refusing: (.*)$" printed))
         :asked @asked
         :ran @ran
         :record @record
         :banner (:text (outcome/banner {:dry-run? (boolean dry-run?) :changes? true
                                         :outcome (if result :ok :failed)}
                                        @record))}))))

;; --- what a :problems function may return ---------------------------------------------

(deftest judge-problems-reads-an-empty-sequence-as-no-problems
  (doseq [empty-seq [[] () (lazy-seq nil) (map identity [])]]
    (is (nil? (change/judge-problems (constantly empty-seq) {})) (pr-str empty-seq))))

(deftest judge-problems-returns-every-message-in-order
  (is (= ["a" "b"] (change/judge-problems (constantly ["a" "b"]) {})))
  (is (= ["a" "b"] (change/judge-problems (constantly (list "a" "b")) {})))
  (is (= ["x-1" "x-2"] (change/judge-problems (fn [_] (map #(str "x-" %) [1 2])) {})))
  (is (= ["from ctx"] (change/judge-problems (fn [{:keys [m]}] [m]) {:m "from ctx"}))))

(deftest judge-problems-refuses-what-is-not-a-sequence-of-messages
  (doseq [[label returned why]
          [["nil" nil #"returned nil, not a sequence of messages - return \[\]"]
           ["a string" "a problem" #"returned \"a problem\", not a sequence of messages"]
           ["a set" #{"a"} #"returned #\{\"a\"\}, not a sequence of messages"]
           ["a map" {:a 1} #"not a sequence of messages"]
           ["false" false #"returned false, not a sequence"]
           ["true" true #"returned true, not a sequence"]
           ["a keyword" :ok #"returned :ok, not a sequence"]
           ["a message that is not a string" ["fine" :bad] #"returned :bad among its messages"]
           ["a blank message" ["fine" "  "] #"returned \"  \" among its messages"]
           ["a nil message" [nil] #"returned nil among its messages"]]]
    (testing label
      (let [reason (change/judge-problems (constantly returned) {})]
        (is (string? reason) "one reason naming the defect, so the change is refused")
        (is (re-find why (str reason)) reason)))))

(deftest judge-problems-refuses-a-function-that-throws
  (is (= "its :problems threw: boom"
         (change/judge-problems (fn [_] (throw (ex-info "boom" {}))) {})))
  (is (= "its :problems threw: java.lang.NullPointerException"
         (change/judge-problems (fn [_] (throw (NullPointerException.))) {})))
  (testing "a throw while the result is realized"
    (is (= "its :problems threw: late"
           (change/judge-problems (fn [_] (map (fn [_] (throw (ex-info "late" {}))) [1])) {})))))

(deftest judge-problems-rethrows-a-strict-read-that-did-not-answer
  (let [e (ex-info "the read did not answer" {::kubectl/unread true})]
    (is (identical? e (try (change/judge-problems (fn [_] (throw e)) {})
                           (catch clojure.lang.ExceptionInfo thrown thrown))))))

(deftest judge-problems-refuses-a-function-that-reaches-outside-the-process
  (let [started (atom [])]
    (with-redefs [babashka.process/process (fn [& args] (swap! started conj args) (throw (ex-info "no" {})))]
      (let [why (change/judge-problems (fn [ctx] [(str (kubectl/get-namespaced-resource ctx "batch" "configmaps" "settings"))])
                                       {:context "ctx"})]
        (is (re-find #"^its :problems reached outside the process: not running 'kubectl .*configmaps" why) why)
        (is (str/includes? why change/problems-forbidden))
        (is (= [] @started) "the guard refused before any process started")))))

;; --- the gate and the printed lines are one list ----------------------------------------

(def ^:private message-gen
  (gen/fmap #(str "problem " %) (gen/not-empty gen/string-alphanumeric)))

(deftest the-printed-reasons-are-the-reasons-the-gate-decided-on
  (let [result (tc/quick-check
                60
                (prop/for-all [messages (gen/vector message-gen 0 5)]
                  (let [{:keys [result refusing asked ran]} (run (spec-with blockers-of) messages)]
                    (and (= messages refusing)
                         (= (empty? messages) (true? result))
                         (= (if (seq messages) 0 1) asked)
                         (= (if (seq messages) 0 1) (count ran)))))
                :seed 0x8a3d86b3)]
    (is (:pass? result) (pr-str (:shrunk result)))))

(deftest the-property-fails-when-the-printer-drops-a-message
  (testing "a positive control: a refuse! that prints only the first message breaks the property"
    (let [original @#'gumshoe.mutation/refuse!
          result (with-redefs-fn {#'gumshoe.mutation/refuse!
                                  (fn [why] (original (if (sequential? why) (first why) why)))}
                   #(tc/quick-check
                     30
                     (prop/for-all [messages (gen/vector message-gen 0 5)]
                       (= messages (:refusing (run (spec-with blockers-of) messages))))
                     :seed 0x8a3d86b3))]
      (is (false? (:pass? result))))))

(deftest a-malformed-answer-never-lets-the-change-run
  (let [result (tc/quick-check
                60
                (prop/for-all [returned (gen/one-of [gen/any-printable-equatable
                                                     (gen/vector (gen/one-of [gen/keyword gen/small-integer (gen/return "") (gen/return nil)]) 1 3)])]
                  (let [valid? (and (sequential? returned)
                                    (every? #(and (string? %) (not (str/blank? %))) returned))
                        {:keys [result ran refusing]} (run (spec-with (constantly returned)) [])]
                    (or valid?
                        (and (false? result) (empty? ran) (= 1 (count refusing))))))
                :seed 1706)]
    (is (:pass? result) (pr-str (:shrunk result)))))

(deftest problems-before-the-confirmation-refuse-before-asking
  (let [{:keys [result out asked ran banner]} (run (spec-with blockers-of) ["the rollout is not done" "a backup runs"])]
    (is (false? result))
    (is (= 0 asked) "the operator is never asked to approve a change with problems")
    (is (= [] ran))
    (is (str/includes? out "refusing: the rollout is not done\nrefusing: a backup runs\n") out)
    (is (= "FAILED - refused at \"suspend nightly\" (2 problems) - read the messages above; nothing was changed" banner))))

(deftest no-problems-lets-the-change-run
  (let [{:keys [result asked ran out]} (run (spec-with blockers-of) [])]
    (is (true? result))
    (is (= 1 asked))
    (is (= 1 (count ran)))
    (is (not (str/includes? out "refusing")) out)))

(deftest problems-after-the-confirmation-refuse-before-the-effect
  (let [{:keys [result out asked ran banner]} (run (spec-with blockers-of) [] :confirming ["a backup started meanwhile"])]
    (is (false? result))
    (is (= 1 asked))
    (is (= [] ran) "the effect never ran")
    (is (str/includes? out "refusing: a backup started meanwhile") out)
    (is (str/includes? banner "refused at \"suspend nightly\"") banner)
    (is (str/starts-with? banner "ABORTED - ") banner)))

(deftest a-malformed-answer-after-the-confirmation-refuses-too
  (let [calls (atom 0)
        problems (fn [_] (if (= 1 (swap! calls inc)) [] nil))
        {:keys [result out ran]} (run (spec-with problems) [])]
    (is (false? result))
    (is (= [] ran))
    (is (= 2 @calls) "judged before and after the confirmation")
    (is (str/includes? out "refusing: its :problems returned nil") out)))

(deftest a-refusal-is-judged-before-problems
  (let [seen (atom 0)
        spec (spec-with (fn [_] (swap! seen inc) ["never shown"])
                        :refusal (constantly "it is suspended already"))
        {:keys [result refusing]} (run spec [])]
    (is (false? result))
    (is (= ["it is suspended already"] refusing))
    (is (= 0 @seen) "only a change no :refusal stopped reaches :problems")))

(deftest dry-run-prints-the-problems-as-the-reasons-the-step-would-not-run
  (let [{:keys [result out ran refusing banner record]} (run (spec-with blockers-of) ["the rollout is not done" "a backup runs"] :dry-run? true)]
    (is (= ["the rollout is not done" "a backup runs"] refusing) out)
    (is (= [] ran))
    (is (str/includes? out "a real run would abort here") out)
    (is (= ["suspend a CronJob"] (:unmet record)))
    (is (true? result) "a rehearsal shows the plan and goes on; the banner fails it")
    (is (str/starts-with? banner "DRY RUN - nothing was changed") banner)
    (is (= 1 (outcome/exit-code :ok record)))))

(deftest dry-run-with-no-problems-prints-no-reasons
  (let [{:keys [out refusing record]} (run (spec-with blockers-of) [] :dry-run? true)]
    (is (= [] refusing) out)
    (is (empty? (:unmet record)))))

;; --- normalize --------------------------------------------------------------------------

(defn- refusal-of [spec]
  (try (change/normalize spec) nil
       (catch clojure.lang.ExceptionInfo e (when (change/invalid? e) (ex-message e)))))

(deftest normalize-refuses-problems-beside-a-precondition
  (is (re-find #"has :problems and :precondition"
               (str (refusal-of (spec-with blockers-of :precondition (constantly true)))))))

(deftest normalize-refuses-a-problems-that-is-not-a-function
  (doseq [x [["a"] "a" {:a 1} :problems]]
    (is (re-find #"^:problems is \(fn \[ctx\]" (str (refusal-of (spec-with x)))) (pr-str x))))

(deftest normalize-folds-problems-into-the-refusal-and-is-idempotent
  (let [n (change/normalize (spec-with blockers-of))]
    (is (not (contains? n :problems)))
    (is (fn? (:refusal n)))
    (is (= ["x"] ((:refusal n) {:settings (settings ["x"])})))
    (is (nil? ((:refusal n) {:settings (settings [])})))
    (is (= n (change/normalize n)))))

(deftest a-nil-problems-is-absent
  (is (nil? (:refusal (change/normalize (spec-with nil))))))

(deftest a-recheck-may-be-judged-by-problems
  (is (map? (change/normalize (-> (spec-with blockers-of) (dissoc :reads) (assoc :recheck (constantly {})))))))

(deftest compose-joins-problems-with-other-parts-guards
  (testing "a fragment's :precondition and a caller's :problems are two parts, judged in part order"
    (let [composed (change/compose {:precondition (fn [{:keys [ok?]}] ok?)}
                                   {:problems blockers-of})]
      (is (= change/unmet ((:refusal composed) {:ok? false :settings (settings ["x"])})))
      (is (= ["x"] ((:refusal composed) {:ok? true :settings (settings ["x"])})))
      (is (nil? ((:refusal composed) {:ok? true :settings (settings [])})))))
  (testing "the same :problems in two parts is judged once"
    (let [calls (atom 0)
          problems (fn [_] (swap! calls inc) [])
          composed (change/compose {:problems problems} {:problems problems})]
      ((:refusal composed) {})
      (is (= 1 @calls)))))

(deftest refusal-messages-reads-every-shape-of-reason
  (is (nil? (change/refusal-messages nil)))
  (is (nil? (change/refusal-messages change/unmet)))
  (is (= ["a"] (change/refusal-messages "a")))
  (is (= ["a" "b"] (change/refusal-messages ["a" "b"]))))

(deftest a-refusal-returning-an-empty-list-refuses-saying-so
  (let [{:keys [result refusing]} (run (-> (spec-with nil) (assoc :refusal (constantly []))) [])]
    (is (false? result))
    (is (= ["its :refusal returned an empty list of reasons - return nil to let the change run"] refusing))))

;; --- conformance -----------------------------------------------------------------------

(def ^:private contract
  {:select {:mode :one :label "Node"}
   :confirm {:action "drain a node" :destructive? true}
   :prerequisites {}
   :effect (fn [_] (effect/plan (effect/note "go")))
   :no-verify "the test has nothing to check"})

(deftest conformance-counts-problems-as-a-guard
  (is (= [] (conformance/spec-guard-problems {:spec (assoc contract :problems (constantly []))})))
  (testing "a negative control: the same spec without it is a destructive spec with no guard"
    (is (= 1 (count (conformance/spec-guard-problems {:spec contract}))))))

(deftest conformance-reports-a-spec-normalize-refuses-for-its-problems
  (is (re-find #"^is refused: the spec has :problems and :precondition"
               (first (conformance/spec-problems {:spec (assoc contract :problems (constantly []) :precondition (constantly true))}))))
  (is (re-find #"^is refused: :problems is \(fn"
               (first (conformance/spec-problems {:spec (assoc contract :problems ["a"])}))))
  (is (= [] (vec (conformance/spec-problems {:spec (assoc contract :problems (constantly []))})))))

;; --- a playbook names the step that blocked it --------------------------------------------

(deftest a-playbook-banner-names-the-step-whose-problems-blocked-it
  (let [record (outcome/fresh)
        out (java.io.StringWriter.)
        step (fn [title problems]
               {:title title
                :select {:mode :one :label "Node"}
                :confirm {:action (str "act: " title)}
                :problems problems
                :effect (fn [_] (effect/plan (effect/note title)))})
        result (with-redefs-fn
                 {#'gumshoe.playbook/attended? (constantly true)
                  #'gumshoe.playbook/wait-for-operator! (fn [_] true)}
                 #(with-redefs [kubectl/current-context (constantly "ctx")
                                kubectl/current-cluster (constantly "cluster")
                                interact/confirm! (constantly true)
                                effect/run! (constantly true)]
                    (binding [*out* out *err* out outcome/*record* record]
                      (playbook/run-on! {:select {:mode :one :label "Node"}
                                         :confirm {:action "maintain the node"}
                                         :steps [(step "cordon" (constantly ["the node runs etcd" "a drain is running"]))
                                                 (step "drain" (constantly []))]}
                                        "w-1" {} nil))))
        text (stdout/strip-colors (str out))]
    (is (false? result))
    (is (re-find #"cordon[^\n]*\n\s*refusing: the node runs etcd\nrefusing: a drain is running" text) text)
    (is (= {:step "cordon" :messages ["the node runs etcd" "a drain is running"]} (:refused @record)))
    (is (= "FAILED - refused at \"cordon\" (2 problems) - read the messages above; nothing was changed"
           (:text (outcome/banner {:changes? true :outcome :failed} @record))))))

(deftest a-banner-without-a-refusal-is-unchanged
  (is (= "FAILED - read the messages above; nothing was changed"
         (:text (outcome/banner {:changes? true :outcome :failed} {:applied 0}))))
  (is (= "FAILED - refused at \"x\" - read the messages above; nothing was changed"
         (:text (outcome/banner {:changes? true :outcome :failed} {:applied 0 :refused {:step "x" :messages ["one"]}}))))
  (testing "a refusal no step was named for adds nothing"
    (is (= "FAILED - read the messages above; nothing was changed"
           (:text (outcome/banner {:changes? true :outcome :failed} {:applied 0 :refused {:step nil :messages ["one"]}}))))))

(deftest only-the-first-refusal-of-a-run-is-kept
  (let [record (outcome/fresh)]
    (binding [outcome/*record* record]
      (outcome/refused! "first" ["a"])
      (outcome/refused! "second" ["b" "c"]))
    (is (= {:step "first" :messages ["a"]} (:refused @record)))))

;; --- the polarity helper proves a :problems gate --------------------------------------------

(def ^:private settings-read
  (refusal/kubectl-get {:type "configmaps" :namespace "batch" :name "settings"}))

(deftest the-polarity-helper-proves-a-problems-gate-over-two-answers
  (let [result (refusal/polarity (spec-with blockers-of)
                                 {:target "batch/nightly"
                                  :read settings-read
                                  :refusing-answer (settings ["the rollout is not done" "a backup runs"])
                                  :answer (settings [])
                                  :refusal ["the rollout is not done" "a backup runs"]})]
    (is (:holds? result) (str/join "\n" (:failures result)))))

(deftest the-polarity-helper-proves-a-problems-gate-over-an-unanswered-read
  (let [result (refusal/polarity (spec-with blockers-of)
                                 {:target "batch/nightly"
                                  :read settings-read
                                  :answer (settings [])
                                  :refusal #"could not read"})]
    (is (:holds? result) (str/join "\n" (:failures result)))))

(deftest the-polarity-helper-fails-a-problems-gate-that-always-fires
  (let [result (refusal/polarity (spec-with (fn [ctx] (conj (blockers-of ctx) "always")))
                                 {:target "batch/nightly"
                                  :read settings-read
                                  :refusing-answer (settings ["x"])
                                  :answer (settings [])
                                  :refusal ["x" "always"]})]
    (is (not (:holds? result)))
    (is (some #(re-find #"once kubectl get configmaps settings --namespace=batch answers, the gate must proceed, but before the confirmation it refused: \"always\"" %)
              (:failures result))
        (str/join "\n" (:failures result)))))

(deftest the-polarity-helper-fails-a-message-list-that-differs
  (let [result (refusal/polarity (spec-with blockers-of)
                                 {:target "batch/nightly"
                                  :read settings-read
                                  :refusing-answer (settings ["a" "b"])
                                  :answer (settings [])
                                  :refusal ["a"]})]
    (is (not (:holds? result)))
    (is (= 2 (count (filter #(str/includes? % "refused with another reason: [\"a\" \"b\"]") (:failures result))))
        (str/join "\n" (:failures result)))))

(deftest the-polarity-helper-catches-a-problems-that-reads
  (testing "a read the :reads already made, which the phase's cache would serve, is refused as well"
    (let [spec (spec-with (fn [ctx] (when (read-settings ctx) [])))
          result (refusal/polarity spec {:target "batch/nightly"
                                         :read settings-read
                                         :answer (settings [])
                                         :refusal #"could not read"})]
      (is (not (:holds? result)))
      (is (some #(str/includes? % "its :problems reached outside the process") (:failures result))
          (str/join "\n" (:failures result))))))
