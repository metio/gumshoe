;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-done-test
  "A change that declares :done is skipped when it is already done: judged
   over the evidence its :derive phase read, before anything is asked,
   never on a read that did not answer, and never on evidence its
   post-check would not accept. A run goes through the real flow/change!
   against a recorded cluster whose kubectl answers from an atom."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.change :as change]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.recording :as recording]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.toggle :as toggle]
            [gumshoe.verify :as verify]))

;; --- the recorded cluster -------------------------------------------------------------

(defn- arg [argv prefix]
  (some #(when (and (string? %) (str/starts-with? % prefix)) (subs % (count prefix))) argv))

(defn- key-of
  [argv]
  (let [[_ type name] (drop-while #(not= "get" %) argv)]
    [type (arg argv "--namespace=") name]))

(defn- answer
  [objects k]
  (let [o (get objects k)]
    (cond
      (= :unanswered o) {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"}
      o {:exit 0 :out (json/generate-string o) :err ""}
      :else {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" (first k) (last k))})))

(def ^:private cronjob-key ["cronjobs" "batch" "nightly"])

(defn- cronjob
  [suspend]
  [cronjob-key {:metadata {:namespace "batch" :name "nightly" :uid "nightly-uid" :generation 1}
                :spec {:suspend suspend}}])

(defn- suspended?
  [object]
  (true? (get-in object [:spec :suspend])))

(defn- suspended-check
  [ctx]
  {:description "CronJob batch/nightly is suspended"
   :check (fn [] (verify/object-holds? (mutation/read-subject ctx) suspended?))})

(def ^:private suspend-spec
  "A change that suspends the CronJob, whose post-check is the state it
   brings about, and that is done when that post-check already holds."
  {:select {:mode :namespaced :label "CronJob"}
   :confirm {:action "suspend a CronJob" :destructive? false}
   :subject {:type "cronjobs"}
   :refusal (fn [{:keys [object]}] (when (suspended? object) "it is suspended already"))
   :effect (fn [_] (effect/plan (effect/note "suspend")))
   :verify (fn [ctx] [(suspended-check ctx)])
   :done true})

(defn- run
  "Runs spec on batch/nightly through mutation/run-on! and the real
   flow/change! against a cluster holding objects, with a run record bound.
   The operator confirms; `confirming`, when given, edits the cluster while
   the confirmation is shown. Returns what the run returned and printed,
   the confirmations asked, the effects that ran and the record."
  [spec objects & {:keys [confirming dry-run? run-opts]}]
  (let [cluster (atom (into {} objects))
        asked (atom 0)
        ran (atom [])
        record (outcome/fresh)
        eventually verify/eventually]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  shell/execute (fn [& argv] (answer @cluster (key-of argv)))
                  announce/announce! (fn [_ _ message] message)
                  interact/confirm! (fn [_]
                                      (swap! asked inc)
                                      (when confirming (swap! cluster confirming))
                                      true)
                  effect/run! (fn [plan]
                                (swap! ran conj plan)
                                (swap! cluster assoc-in [cronjob-key :spec :suspend] true)
                                true)
                  verify/eventually (fn [check] (eventually (assoc check :timeout 0)))]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out
                             outcome/*record* record
                             flow/*dry-run* (boolean dry-run?)]
                     (mutation/run-on! spec "batch/nightly" {} nil run-opts))]
        {:result result :out (stdout/strip-colors (str out)) :asked @asked :ran @ran :record @record}))))

;; --- judge-done, pure -------------------------------------------------------------------

(defn- check [description result]
  {:description description :check (constantly result)})

(defn- attempt-result
  "An attempt that returns what the check function returns: true, false, or
   the message of a read that did not answer."
  [f]
  (f))

(deftest judge-done-decides-from-the-function-and-the-post-checks
  (doseq [[label done checks =>]
          [["no :done is never done" nil [(check "a" true)] {:done? false}]
           [":done true with every post-check holding is done"
            true [(check "a" true) (check "b" true)] {:done? true :held ["a" "b"]}]
           [":done true with one post-check not holding is not done"
            true [(check "a" true) (check "b" false)] {:done? false}]
           [":done true with no post-check has nothing to judge"
            true [] {:done? false}]
           [":done true with a post-check whose read did not answer is unread"
            true [(check "a" true) (check "b" "the server did not answer")] {:unread "the server did not answer"}]
           ["a function alone that holds is done" (constantly true) nil {:done? true :held []}]
           ["a function alone that does not hold is not done" (constantly false) nil {:done? false}]
           ["a function that holds beside a post-check that does not is not done"
            (constantly true) [(check "a" false)] {:done? false}]
           ["a function that does not hold beside post-checks that do is not done"
            (constantly false) [(check "a" true)] {:done? false}]
           ["a function and post-checks that all hold is done, naming the checks"
            (constantly true) [(check "a" true)] {:done? true :held ["a"]}]
           ["a function whose read did not answer is unread"
            #(throw (ex-info "the server did not answer" {::kubectl/unread true})) [(check "a" true)]
            {:unread "the server did not answer"}]]]
    (testing label
      (is (= => (mutation/judge-done done checks attempt-result))))))

(deftest judge-done-stops-at-the-first-post-check-that-does-not-hold
  (let [ran (atom [])
        tracked (fn [d r] {:description d :check #(do (swap! ran conj d) r)})]
    (is (= {:done? false} (mutation/judge-done true [(tracked "a" false) (tracked "b" true)] attempt-result)))
    (is (= ["a"] @ran))))

(deftest judge-done-propagates-an-error-that-is-not-an-unanswered-read
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"a bug"
                        (mutation/judge-done #(throw (ex-info "a bug" {})) [] attempt-result))))

(deftest a-function-that-is-not-done-runs-no-post-check
  (let [ran (atom 0)]
    (mutation/judge-done (constantly false) [{:description "a" :check #(swap! ran inc)}] attempt-result)
    (is (zero? @ran))))

;; --- normalize ---------------------------------------------------------------------------

(def ^:private base
  {:select {:mode :namespaced :label "CronJob"}
   :confirm {:action "suspend a CronJob" :destructive? false}
   :effect (fn [_] (effect/plan (effect/note "go")))
   :verify (fn [_] [])})

(defn- refusal-of
  [spec]
  (try (change/normalize spec) nil
       (catch clojure.lang.ExceptionInfo e
         (when (change/invalid? e) (ex-message e)))))

(deftest normalize-maps-satisfied-onto-done-and-refuses-what-it-cannot-run
  (let [f (fn [_] true)]
    (doseq [[label spec => refused] [["no :done" base nil nil]
                                     [":done true" (assoc base :done true) true nil]
                                     [":done false is absent" (assoc base :done false) nil nil]
                                     [":done nil is absent" (assoc base :done nil) nil nil]
                                     [":satisfied? true is :done true" (assoc base :satisfied? true) true nil]
                                     [":satisfied? a function is that :done" (assoc base :satisfied? f) f nil]
                                     [":done a function" (assoc base :done f) f nil]
                                     [":done and :satisfied? are refused" (assoc base :done true :satisfied? true) nil
                                      "the spec has :done and :satisfied? - write one of them"]
                                     [":done false and :satisfied? true are refused" (assoc base :done false :satisfied? true) nil
                                      "the spec has :done and :satisfied? - write one of them"]
                                     [":done that is a map is refused" (assoc base :done {:checks 1}) nil
                                      ":done is true, to skip the change when its post-checks already hold, or (fn [ctx] -> bool), not {:checks 1}"]
                                     [":done that is a keyword is refused" (assoc base :done :yes) nil
                                      ":done is true, to skip the change when its post-checks already hold, or (fn [ctx] -> bool), not :yes"]
                                     [":done true without a :verify is refused" (assoc (dissoc base :verify) :done true :no-verify "why") nil
                                      ":done true skips the change when its post-checks already hold, and the spec has no :verify"]
                                     ["a function without a :verify is accepted on a change that is not destructive"
                                      (assoc (dissoc base :verify) :done f) f nil]
                                     ["a destructive function without a :verify is refused"
                                      (-> base (dissoc :verify) (assoc :done f) (assoc-in [:confirm :destructive?] true)) nil
                                      "a destructive change is skipped as done only on evidence its post-check accepts, so its :done needs a :verify"]
                                     ["a destructive function beside a :verify is accepted"
                                      (-> base (assoc :done f) (assoc-in [:confirm :destructive?] true)) f nil]]]
      (testing label
        (if refused
          (is (= refused (refusal-of spec)))
          (let [canonical (change/normalize spec)]
            (is (= => (change/done canonical)))
            (is (not (contains? canonical :satisfied?)))
            (is (= canonical (change/normalize canonical)) "idempotent")))))))

;; --- a standalone change ---------------------------------------------------------------

(def ^:private skip-lines
  (str "✅ already holds: CronJob batch/nightly is suspended\n"
       "✅ skipped, already done: suspend a CronJob\n"))

(deftest a-change-that-is-already-done-is-skipped-before-anything-is-asked
  (let [{:keys [result out asked ran record]} (run suspend-spec [(cronjob true)])]
    (is (true? result) "a skipped change succeeded")
    (is (zero? asked) "nothing is asked")
    (is (empty? ran) "no effect runs")
    (is (= skip-lines out) "what held and that it was skipped, and nothing else - no refusal, no post-check")
    (is (= ["suspend a CronJob"] (:skipped record)))
    (is (= {:tone :ok :text "DONE - already done, nothing was changed - skipped: suspend a CronJob"}
           (outcome/banner {:changes? true :outcome :ok} record)))))

(deftest a-change-that-is-not-done-runs-as-it-always-did
  (let [{:keys [result out asked ran record]} (run suspend-spec [(cronjob false)])
        {plain-out :out plain-asked :asked} (run (dissoc suspend-spec :done) [(cronjob false)])]
    (is (true? result))
    (is (= 1 asked plain-asked))
    (is (= 1 (count ran)))
    (is (= plain-out out) "judging it not done prints nothing")
    (is (nil? (:skipped record)))))

(deftest done-is-judged-before-the-refusal-which-a-done-change-never-reaches
  (let [refused (atom 0)
        spec (assoc suspend-spec :refusal (fn [_] (swap! refused inc) "it is suspended already"))
        {:keys [result]} (run spec [(cronjob true)])]
    (is (true? result))
    (is (zero? @refused))))

(deftest a-change-without-done-is-refused-where-it-was
  (let [{:keys [result out asked]} (run (dissoc suspend-spec :done) [(cronjob true)])]
    (is (false? result))
    (is (zero? asked))
    (is (= "refusing: it is suspended already\n" out))))

(deftest done-fails-closed
  (doseq [[label objects printed]
          [["a subject whose read did not answer stops the change before done is judged"
            [[cronjob-key :unanswered]] #"could not read what the change rests on"]
           ["a subject that does not exist is not done, and is refused as missing"
            [] #"refusing: it does not exist"]]]
    (testing label
      (let [{:keys [result out asked ran]} (run suspend-spec objects)]
        (is (false? result))
        (is (zero? asked))
        (is (empty? ran))
        (is (not (str/includes? out "skipped")) out)
        (is (re-find printed out) out)))))

(defn- settings-check
  [{:keys [context]}]
  {:description "settings say suspended"
   :check (fn [] (= "suspended" (get-in (kubectl/get-namespaced-resource context "batch" "configmaps" "settings")
                                        [:data :mode])))})

(def ^:private settings-key ["configmaps" "batch" "settings"])

(deftest a-post-check-whose-read-did-not-answer-is-never-done
  (let [spec (assoc suspend-spec
                    :verify (fn [ctx] [(suspended-check ctx) (settings-check ctx)])
                    :refusal (constantly nil))
        {:keys [result out asked ran]} (run spec [(cronjob true) [settings-key :unanswered]]
                                            :confirming #(assoc % settings-key {:metadata {:name "settings"}
                                                                                :data {:mode "suspended"}}))]
    (is (true? result) "it runs as a change that is not done")
    (is (= 1 asked))
    (is (= 1 (count ran)))
    (is (str/includes? out "could not tell whether this change is already done - ") out)
    (is (not (str/includes? out "skipped")) out)))

(deftest a-function-whose-read-did-not-answer-is-never-done
  (let [spec (assoc suspend-spec
                    :done (fn [{:keys [context]}] (some? (kubectl/get-namespaced-resource context "batch" "configmaps" "settings")))
                    :refusal (constantly nil))
        {:keys [result out asked]} (run spec [(cronjob true) [settings-key :unanswered]])]
    (is (true? result))
    (is (= 1 asked))
    (is (str/includes? out "could not tell whether this change is already done") out)))

(deftest a-destructive-change-is-skipped-only-when-its-post-checks-hold
  (doseq [[label done objects skipped?]
          [["a function that says done is not enough when the post-check does not hold"
            (constantly true) [(cronjob false)] false]
           ["a function that says done and a post-check that holds skip it"
            (constantly true) [(cronjob true)] true]
           ["a post-check that holds is not enough when the function says not done"
            (constantly false) [(cronjob true)] false]]]
    (testing label
      (let [spec (-> suspend-spec
                     (assoc :done done :refusal (constantly nil))
                     (assoc-in [:confirm :destructive?] true))
            {:keys [out asked]} (run spec objects)]
        (is (= skipped? (str/includes? out "skipped, already done")) out)
        (is (= (if skipped? 0 1) asked))))))

(deftest a-function-receives-the-derived-context
  (let [seen (atom nil)
        spec (assoc suspend-spec :done (fn [ctx] (reset! seen ctx) true)
                    :values {:stamp (constantly "s-1")})]
    (run spec [(cronjob true)])
    (is (= "s-1" (:stamp @seen)))
    (is (= "nightly" (get-in @seen [:object :metadata :name])))
    (is (= {:type "cronjobs" :namespace "batch" :name "nightly"} (:ref @seen)))))

(deftest done-is-judged-in-the-derive-phase-and-reads-the-subject-from-its-cache
  (let [phases (atom []) gets (atom 0)
        spec (assoc suspend-spec :done (fn [_] (swap! phases conj mutation/*phase*) true))]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  shell/execute (fn [& argv] (swap! gets inc) (answer (into {} [(cronjob true)]) (key-of argv)))]
      (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
        (is (true? (mutation/run-on! spec "batch/nightly" {} nil)))))
    (is (= [:derive] @phases))
    (is (= 1 @gets) "the post-check's read of the subject is served from the :derive phase's cache")))

(deftest a-rehearsal-reports-a-done-change-skipped
  (let [{:keys [result out ran]} (run suspend-spec [(cronjob true)] :dry-run? true)]
    (is (true? result))
    (is (empty? ran))
    (is (= skip-lines out))))

(deftest on-skip-is-called-only-for-a-skipped-change
  (doseq [[objects calls] [[[(cronjob true)] 1] [[(cronjob false)] 0]]]
    (let [called (atom 0)]
      (run suspend-spec objects :run-opts {:on-skip #(swap! called inc)})
      (is (= calls @called)))))

(deftest a-skip-names-the-step-by-its-title
  (let [{:keys [out record]} (run (assoc suspend-spec :title "Suspend") [(cronjob true)])]
    (is (str/includes? out "skipped, already done: Suspend"))
    (is (= ["Suspend"] (:skipped record)))))

;; --- prejudge ----------------------------------------------------------------------------

(defn- prejudged
  [spec objects]
  (with-redefs [kubectl/current-context (constantly "ctx")
                kubectl/current-cluster (constantly "cluster")
                shell/execute (fn [& argv] (answer (into {} objects) (key-of argv)))]
    (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
      (mutation/prejudge spec "batch/nightly" {} nil))))

(deftest prejudge-judges-done-first
  (testing "a done change returns its verdict and no cache, so it is judged again when reached"
    (let [verdict (prejudged suspend-spec [(cronjob true)])]
      (is (= {:done {:done? true :held ["CronJob batch/nightly is suspended"]}} verdict))))
  (testing "a change that is not done is judged for its refusal and hands over its cache"
    (let [{:keys [cache derived done undecided]} (prejudged suspend-spec [(cronjob false)])]
      (is (some? cache))
      (is (some? derived))
      (is (nil? done))
      (is (nil? undecided))))
  (testing "a change whose done read did not answer says so, so it is derived again when reached"
    (let [spec (assoc suspend-spec :verify (fn [ctx] [(suspended-check ctx) (settings-check ctx)]) :refusal (constantly nil))
          {:keys [cache undecided]} (prejudged spec [(cronjob true) [settings-key :unanswered]])]
      (is (some? cache))
      (is (str/includes? undecided "Unable to connect")))))

;; --- help -------------------------------------------------------------------------------

(deftest help-says-when-a-change-is-skipped
  (doseq [[label spec line] [[":done true" suspend-spec "  skips it, asking nothing, when its post-checks already hold"]
                             ["a function beside a :verify" (assoc suspend-spec :done (constantly true))
                              "  skips it, asking nothing, when it is already done and its post-checks already hold"]
                             ["a function alone" (-> suspend-spec (dissoc :verify) (assoc :done (constantly true) :no-verify "why"))
                              "  skips it, asking nothing, when it is already done"]]]
    (testing label
      (is (= line (second (rest (mutation/details spec))))))))

(deftest help-says-nothing-of-a-skip-without-done
  (is (not-any? #(str/includes? % "skips") (mutation/details (dissoc suspend-spec :done)))))

;; --- outcome and recording -----------------------------------------------------------------

(deftest the-banner-names-the-skipped-changes
  (doseq [[label record text]
          [["only skips" {:applied 0 :skipped ["a" "b"]} "DONE - already done, nothing was changed - skipped: a, b"]
           ["applied, verified and skipped" {:applied 1 :verified true :skipped ["a"]}
            "DONE - applied and verified; skipped as already done: a"]
           ["applied without post-checks and skipped" {:applied 1 :skipped ["a"]}
            "DONE - applied; this book has no post-checks; skipped as already done: a"]
           ["nothing skipped" {:applied 1 :verified true} "DONE - applied and verified"]
           ["nothing at all" {:applied 0} "DONE - nothing was changed"]]]
    (testing label
      (is (= {:tone :ok :text text} (outcome/banner {:changes? true :outcome :ok} record))))))

(deftest a-failed-run-is-not-turned-into-a-success-by-a-skip
  (is (= :error (:tone (outcome/banner {:changes? true :outcome :failed} {:applied 1 :stopped "x" :skipped ["a"]}))))
  (is (zero? (outcome/exit-code :ok {:skipped ["a"]}))))

(deftest skipped-records-nothing-outside-a-run
  (is (nil? (outcome/skipped! "a"))))

(deftest the-recording-names-the-skipped-changes
  (let [dir (str (fs/create-temp-dir))]
    (try
      (with-redefs [recording/base-directory dir]
        (let [with-skips (recording/save! {:book-file "runbooks/a/b.clj" :outcome :ok :opts {} :skipped ["a"]})]
          (is (= ["a"] (:skipped (edn/read-string (slurp (str with-skips "/meta.edn"))))))))
      (with-redefs [recording/base-directory (str dir "/plain")]
        (let [plain (recording/save! {:book-file "runbooks/a/b.clj" :outcome :ok :opts {}})]
          (is (not (contains? (edn/read-string (slurp (str plain "/meta.edn"))) :skipped)))))
      (finally (fs/delete-tree dir)))))

;; --- toggle pairs ---------------------------------------------------------------------------

(def ^:private declaration
  {:spec {:select {:mode :namespaced :label "CronJob"} :subject {:type "cronjobs"}}
   :refusal (fn [_ _] nil)
   :items (fn [{:keys [target]} wanted] [(str wanted " " target)])
   :patch (fn [wanted] (str "{\"spec\":{\"suspend\":" wanted "}}"))
   :applied? (fn [object wanted] (= wanted (suspended? object)))
   :verify {:timeout 1}
   :on {:verb "suspend" :confirm {:action "suspend" :destructive? false} :announce "%s" :verified "%s is suspended"}
   :off {:verb "resume" :confirm {:action "resume" :destructive? false} :announce "%s" :verified "%s is resumed"}})

(deftest a-toggle-side-is-done-when-its-post-check-holds-unless-it-opts-out
  (let [{:keys [on off]} (toggle/pair declaration)
        opted-out (toggle/pair (assoc declaration :done false))]
    (is (true? (:done on)))
    (is (true? (:done off)))
    (is (nil? (change/done (change/normalize (:on opted-out)))))
    (testing "each side skips an object already in its wanted state and runs on one that is not"
      (doseq [[side objects skipped?] [[on [(cronjob true)] true] [on [(cronjob false)] false]
                                       [off [(cronjob false)] true] [off [(cronjob true)] false]]]
        (let [{:keys [out asked]} (run side objects)]
          (is (= skipped? (str/includes? out "skipped, already done")) out)
          (is (= (if skipped? 0 1) asked) out))))))
