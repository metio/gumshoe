;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-prejudge-test
  "The judgement before the confirmation: a change whose :refusal, folded
   :precondition or pinned objects already rule it out is refused before
   the operator is asked, over the evidence its :derive phase read, and the
   judgement after the confirmation still guards the window. A run goes
   through the real flow/change! against a recorded cluster whose kubectl
   counts every get with the phase it ran in."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

;; --- the recorded cluster -------------------------------------------------------------

(defn- object
  [type name & {:keys [generation data uid] :or {generation 1}}]
  [[type "batch" name]
   {:metadata {:namespace "batch" :name name :uid (or uid (str name "-uid")) :generation generation}
    :data (or data {})}])

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

(defn- run
  "Runs spec on target through mutation/run-on! and the real flow/change!
   against a cluster holding objects. The operator confirms; `confirming`,
   when given, edits the cluster while the confirmation is shown. Returns
   what the run returned and printed, whether a confirmation was asked
   (:asked), the effects that ran and every get with its phase."
  [spec target objects & {:keys [confirming dry-run?]}]
  (let [cluster (atom (into {} objects))
        reads (atom [])
        asked (atom 0)
        ran (atom [])]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  shell/execute (fn [& argv]
                                  (let [k (key-of argv)]
                                    (swap! reads conj {:object k :phase mutation/*phase*})
                                    (answer @cluster k)))
                  interact/confirm! (fn [_]
                                      (swap! asked inc)
                                      (when confirming (swap! cluster confirming))
                                      true)
                  effect/run! (fn [plan] (swap! ran conj plan) true)]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out flow/*dry-run* (boolean dry-run?)]
                     (mutation/run-on! (merge {:select {:mode :namespaced :label "CronJob"}
                                               :confirm {:action "suspend a CronJob"}
                                               :effect (fn [_] (effect/plan (effect/note "go")))}
                                              spec)
                                       target {} nil))]
        {:result result :out (stdout/strip-colors (str out)) :asked @asked :ran @ran :reads @reads}))))

(defn- fetches
  [reads phase]
  (frequencies (map :object (filter #(= phase (:phase %)) reads))))

(def ^:private cronjob-key ["cronjobs" "batch" "nightly"])
(def ^:private settings-key ["configmaps" "batch" "settings"])

(defn- cronjob
  [suspend & more]
  (apply object "cronjobs" "nightly" :data {:suspend suspend} more))

(def ^:private settings (object "configmaps" "settings" :data {:mode "on"}))

(defn- suspended-refusal
  [{:keys [object]}]
  (when (= "true" (get-in object [:data :suspend])) "it is suspended already"))

(def ^:private subject-spec
  {:subject {:type "cronjobs"} :refusal suspended-refusal})

(defn- edit
  [k path value]
  (fn [objects] (assoc-in objects (into [k] path) value)))

;; --- the verdict, as a table -----------------------------------------------------------

(def ^:private verdicts
  "[label spec cluster confirming => result asked? printed]: what a change
   decides, and whether it decided it before asking."
  [["a refusal over the state before the confirmation is printed before asking"
    subject-spec [(cronjob "true")] nil
    false 0 "refusing: it is suspended already\n"]
   ["a refusal over the state after the confirmation still refuses after asking"
    subject-spec [(cronjob "false")] (edit cronjob-key [:data :suspend] "true")
    false 1 "refusing: it is suspended already"]
   ["a refusal that holds before and after lets the change run"
    subject-spec [(cronjob "false")] nil
    true 1 nil]
   ["a precondition that does not hold refuses before asking, in the engine's words"
    {:precondition (constantly false)} [(cronjob "false")] nil
    false 0 (str "refusing: " mutation/unmet-before-confirmation "\n")]
   ["a precondition that prints its reason prints it before the engine's words"
    {:precondition (fn [_] (stdout/error "the owner is unknown") false)} [(cronjob "false")] nil
    false 0 (str "❌ the owner is unknown\nrefusing: " mutation/unmet-before-confirmation "\n")]
   ["a refusal is judged before a precondition, which then is not consulted"
    (assoc subject-spec :precondition (fn [_] (throw (ex-info "consulted" {}))))
    [(cronjob "true")] nil
    false 0 "refusing: it is suspended already\n"]
   ["a subject that does not exist is refused before asking"
    {:subject {:type "cronjobs"}} [] nil
    false 0 "refusing: it does not exist\n"]
   ["a pinned object that is not there is refused before asking"
    {:pin (constantly {:type "configmaps" :namespace "batch" :name "settings"})} [(cronjob "false")] nil
    false 0 "refusing: it does not exist or could not be read\n"]
   ["a required subject that does not exist is refused by name before asking"
    {:subjects (constantly [{:type "configmaps" :namespace "batch" :name "settings"}])} [] nil
    false 0 "refusing: configmaps batch/settings: it does not exist\n"]
   ["an optional subject that does not exist lets the change run"
    {:subjects (constantly [{:type "configmaps" :namespace "batch" :name "settings" :optional? true}])} [] nil
    true 1 nil]
   ["a subject removed during the confirmation is refused after asking"
    {:subject {:type "cronjobs"}} [(cronjob "false")] #(dissoc % cronjob-key)
    false 1 "refusing: it was deleted after the confirmation"]
   ["a refusal returning unmet refuses before asking"
    {:refusal (constantly change/unmet)} [(cronjob "false")] nil
    false 0 (str "refusing: " mutation/unmet-before-confirmation "\n")]])

(deftest a-change-is-judged-before-and-after-its-confirmation
  (doseq [[label spec cluster confirming result asked printed] verdicts]
    (testing label
      (let [r (run spec "batch/nightly" cluster :confirming confirming)]
        (is (= result (:result r)))
        (is (= asked (:asked r)) (:out r))
        (when (false? result) (is (empty? (:ran r)) "no effect ran"))
        (cond
          (nil? printed) (is (not (str/includes? (:out r) "refusing")) (:out r))
          (zero? asked) (is (= printed (:out r)))
          :else (is (str/includes? (:out r) printed) (:out r)))))))

;; --- one signature, two phases ---------------------------------------------------------

(deftest the-refusal-runs-twice-with-one-signature
  (let [calls (atom [])
        spec {:subject {:type "cronjobs"}
              :refusal (fn [ctx] (swap! calls conj [mutation/*phase* (set (keys ctx))]) nil)}
        {:keys [result]} (run spec "batch/nightly" [(cronjob "false")])]
    (is (true? result))
    (is (= [:derive :judge] (map first @calls)) "before the confirmation, then after it")
    (is (apply = (map second @calls)) "both judge a context with the same keys")))

(deftest a-refusal-reads-the-derived-object-before-and-the-fresh-one-after
  (let [seen (atom [])
        spec {:subject {:type "cronjobs"}
              :refusal (fn [{:keys [object]}] (swap! seen conj (get-in object [:data :suspend])) nil)}]
    (run spec "batch/nightly" [(cronjob "false")] :confirming (edit cronjob-key [:data :suspend] "moved"))
    (is (= ["false" "moved"] @seen))))

;; --- what :recheck provides ------------------------------------------------------------

(defn- mode-of
  [context]
  (get-in (kubectl/get-namespaced-resource context "batch" "configmaps" "settings") [:data :mode]))

(def ^:private recheck-only
  "A refusal judging a value only :recheck provides: the settings mode,
   which :derive never reads."
  {:subject {:type "cronjobs"}
   :recheck (fn [{:keys [context]}] {:mode (mode-of context)})
   :refusal (fn [{:keys [mode]}] (when-not (= "on" mode) (str "settings are " (pr-str mode))))})

(deftest a-refusal-before-the-confirmation-judges-what-recheck-returns
  (testing "a value only :recheck reads is real before the confirmation, so nothing is refused falsely"
    (let [{:keys [result asked]} (run recheck-only "batch/nightly" [(cronjob "false") settings])]
      (is (true? result))
      (is (= 1 asked))))
  (testing "the value :recheck reads before the confirmation refuses before asking"
    (let [{:keys [result asked out]} (run recheck-only "batch/nightly"
                                          [(cronjob "false") (object "configmaps" "settings" :data {:mode "off"})])]
      (is (false? result))
      (is (zero? asked))
      (is (= "refusing: settings are \"off\"\n" out))))
  (testing "a value that moves during the confirmation is caught by :recheck after it"
    (let [{:keys [result asked out]} (run recheck-only "batch/nightly" [(cronjob "false") settings]
                                          :confirming (edit settings-key [:data :mode] "off"))]
      (is (false? result))
      (is (= 1 asked))
      (is (str/includes? out "refusing: settings are \"off\""))))
  (testing "a :recheck read that does not answer refuses before asking"
    (let [{:keys [result asked out]} (run recheck-only "batch/nightly" [(cronjob "false") [settings-key :unanswered]])]
      (is (false? result))
      (is (zero? asked))
      (is (str/starts-with? out "refusing: could not read what the precondition rests on - ")))))

(deftest recheck-reads-before-the-confirmation-are-served-from-the-derive-cache
  (let [spec {:subject {:type "cronjobs"}
              :derive (fn [{:keys [context]}] {:mode (mode-of context)})
              :recheck (fn [{:keys [context]}] {:mode (mode-of context)})
              :refusal (fn [{:keys [mode]}] (when-not (= "on" mode) "off"))}
        {:keys [result reads]} (run spec "batch/nightly" [(cronjob "false") settings])]
    (is (true? result))
    (is (= {cronjob-key 1 settings-key 1} (fetches reads :derive))
        "the judgement before the confirmation read nothing :derive had not")
    (is (= {cronjob-key 1 settings-key 1} (fetches reads :judge)))))

;; --- what a judgement before the confirmation costs ---------------------------------------

(def ^:private cost-specs
  "Specs whose judgement before the confirmation reads nothing more: every
   object it judges was read in the :derive phase."
  [["a subject and a refusal" subject-spec [(cronjob "false")]]
   ["a subject, a refusal that reads the subject again and a precondition"
    {:subject {:type "cronjobs"}
     :refusal (fn [ctx] (suspended-refusal (assoc ctx :object (mutation/read-subject ctx))))
     :precondition (fn [ctx] (some? (mutation/read-subject ctx)))}
    [(cronjob "false")]]
   ["a pin" {:pin (constantly {:type "configmaps" :namespace "batch" :name "settings"})} [(cronjob "false") settings]]
   ["subjects" {:subjects (constantly [{:type "configmaps" :namespace "batch" :name "settings"}])
                :refusal (fn [{:keys [objects]}] (when-not (first objects) "gone"))}
    [settings]]])

(deftest a-change-that-runs-reads-no-more-than-without-the-judgement-before-it
  (doseq [[label spec cluster] cost-specs]
    (testing label
      (let [{:keys [result reads]} (run spec "batch/nightly" cluster)
            {without :reads} (run (assoc spec :confirmed? true) "batch/nightly" cluster)]
        (is (true? result))
        (is (= (fetches without :derive) (fetches reads :derive))
            "the :derive phase reads what a covered change, judged after the confirmation alone, reads")
        (is (= (fetches without :judge) (fetches reads :judge)))
        (is (every? #(= 1 %) (vals (fetches reads :derive))) "each object once")))))

;; --- what is printed --------------------------------------------------------------------

(deftest a-judgement-that-lets-the-change-run-prints-only-after-the-confirmation
  (let [spec {:subject {:type "cronjobs"}
              :precondition (fn [_] (println "table of what is about to change") (stdout/warn "careful") true)}
        {:keys [result out]} (run spec "batch/nightly" [(cronjob "false")])]
    (is (true? result))
    (is (= 1 (count (re-seq #"table of what is about to change" out))) out)
    (is (= 1 (count (re-seq #"careful" out))) out)
    (is (< (str/index-of out "table of what") (str/index-of out "⚡ Action")) "where the judgement after the confirmation prints it")))

(deftest a-refusing-judgement-prints-what-it-printed-to-the-stream-it-chose
  (let [out (java.io.StringWriter.) err (java.io.StringWriter.)
        [result chunks] (mutation/held #(do (print "to out") (binding [*out* *err*] (print "to err")) :done))]
    (is (= :done result))
    (is (= [[:out "to out"] [:err "to err"]] (vec (remove (comp str/blank? second) chunks))))
    (binding [*out* out *err* err] (mutation/replay-held! chunks))
    (is (= "to out" (str out)))
    (is (= "to err" (str err)))))

(deftest a-judgement-that-throws-replays-what-it-printed-and-rethrows
  (let [out (java.io.StringWriter.)]
    (binding [*out* out *err* out]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"broken"
                            (mutation/held #(do (println "before it broke") (throw (ex-info "broken" {}))))))
      (is (= "before it broke\n" (str out))))))

(deftest a-refusal-that-throws-before-the-confirmation-stops-the-change
  (let [{:keys [asked]} (try (run {:refusal (fn [_] (throw (ex-info "the book is broken" {})))}
                                  "batch/nightly" [(cronjob "false")])
                             (catch clojure.lang.ExceptionInfo e {:asked :threw :message (ex-message e)}))]
    (is (= :threw asked) "the exception propagates and nothing is asked")))

;; --- runs that ask nothing judge once ---------------------------------------------------

(deftest a-rehearsal-judges-once-as-before
  (let [calls (atom [])
        spec {:subject {:type "cronjobs"}
              :refusal (fn [ctx] (swap! calls conj mutation/*phase*) (suspended-refusal ctx))}
        {:keys [result asked out]} (run spec "batch/nightly" [(cronjob "true")] :dry-run? true)]
    (is (true? result) "a rehearsal shows the plan and reports the refusal")
    (is (zero? asked))
    (is (= [:judge] @calls) "judged once, as flow rehearses a precondition")
    (is (str/includes? out "this step's precondition does not hold against the current state"))))

(deftest a-change-a-playbook-confirmation-covered-judges-after-it-alone
  (let [calls (atom [])
        spec {:subject {:type "cronjobs"} :confirmed? true
              :refusal (fn [ctx] (swap! calls conj mutation/*phase*) (suspended-refusal ctx))}
        {:keys [result asked out]} (run spec "batch/nightly" [(cronjob "true")])]
    (is (false? result))
    (is (zero? asked) "a covered change asks nothing")
    (is (= [:judge] @calls))
    (is (str/includes? out "suspend a CronJob") "its confirmation is shown before the refusal")
    (is (str/includes? out "refusing: it is suspended already"))))

;; --- pure parts -------------------------------------------------------------------------

(deftest gone-before-confirmation-names-what-is-not-there
  (let [ref {:type "configmaps" :namespace "batch" :name "settings"}
        fp {:uid "u" :generation 1}]
    (doseq [[label pinned refs objects =>]
            [["nothing pinned, no subjects" nil [] [] nil]
             ["a pick subject that was read" {:ref ref :from :pick :before fp} [] [] nil]
             ["a pick subject that is not there" {:ref ref :from :pick :before nil} [] [] "it does not exist"]
             ["a pin that was not read" {:ref ref :from :reads :before nil} [] [] "it does not exist or could not be read"]
             ["a required subject that is not there" nil [ref] [nil] "configmaps batch/settings: it does not exist"]
             ["an optional subject that is not there" nil [(assoc ref :optional? true)] [nil] nil]
             ["the pin is judged before the subjects"
              {:ref ref :from :pick :before nil} [ref] [nil] "it does not exist"]
             ["the first missing subject is named"
              nil [ref (assoc ref :name "other")] [{} nil] "configmaps batch/other: it does not exist"]]]
      (testing label
        (is (= => (mutation/gone-before-confirmation pinned refs objects)))))))

(deftest prejudge-answers-what-a-playbook-needs
  (with-redefs [kubectl/current-context (constantly "ctx")
                kubectl/current-cluster (constantly "cluster")
                shell/execute (fn [& argv] (answer (into {} [(cronjob "true") settings]) (key-of argv)))]
    (let [base {:select {:mode :namespaced :label "CronJob"} :confirm {:action "x"}
                :effect (fn [_] (effect/plan (effect/note "go")))}]
      (testing "a refusal"
        (is (= {:refusal "it is suspended already" :printed []}
               (mutation/prejudge (merge base subject-spec) "batch/nightly" {} nil))))
      (testing "a pick that does not parse"
        (is (= {:refusal "the pick \"nightly\" does not name a CronJob"}
               (mutation/prejudge (merge base subject-spec) "nightly" {} nil))))
      (testing "an unanswered read of :derive"
        (is (contains? (mutation/prejudge (merge base {:derive (fn [_] (throw (ex-info "kubectl get did not answer" {::kubectl/unread true})))})
                                          "batch/nightly" {} nil)
                       :unread)))
      (testing "a change that may be asked carries the cache of its :derive phase"
        (is (some? (:cache (mutation/prejudge (merge base {:subject {:type "configmaps"}}) "batch/settings" {} nil))))))))
