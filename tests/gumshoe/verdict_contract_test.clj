;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.verdict-contract-test
  "The final banner and the exit code of a run are one contract: a green DONE
   only with exit 0, a FAILED, NOT VERIFIED or BLOCKED only with exit 1, and
   no banner claiming a change, a verification or an absence of change the
   record does not support."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.playbook :as playbook]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.verify :as verify]))

;; --- the contract over every combination ------------------------------------------

(def ^:private books
  "Configs covering a book that announces or not, crossed with :change?
   absent, true and false."
  (for [announce? [true false]
        change [::absent true false]]
    (cond-> {:announce? announce?}
      (not= ::absent change) (assoc :change? change))))

(defn- records
  "Every record the dimensions of a run can leave: effects applied 0/1/n, a
   step that stopped, a step not rehearsed, a soft post-check that timed
   out, an unmet precondition, crossed with how verification, a legacy
   thunk, an abort and a pause ended."
  []
  (for [applied [0 1 3]
        stopped? [false true]
        unrehearsed? [false true]
        unconfirmed? [false true]
        unmet? [false true]
        verified [nil true false]
        thunk [nil :ok :failed]
        aborted? [false true]
        paused? [false true]]
    (cond-> {:applied applied}
      stopped? (assoc :stopped "kubectl delete pvc data-0")
      unrehearsed? (assoc :unrehearsed ["start the upgrade"])
      unconfirmed? (assoc :unconfirmed ["the claim reports 20Gi"])
      unmet? (assoc :unmet ["purge the old deployment"])
      (some? verified) (assoc :verified verified)
      thunk (assoc :thunk thunk)
      aborted? (assoc :aborted "declined at the confirmation")
      paused? (assoc :paused "continue with --segment 2"))))

(defn- violations
  "What is wrong with the verdict of one run, as strings; empty when the
   banner and the exit code agree."
  [config opts outcome record findings?]
  (let [{{:keys [tone text]} :banner exit :exit} (runbook/verdict config opts outcome record findings?)
        changed? (or (pos? (:applied record)) (some? (:thunk record)))]
    (cond-> []
      (and (= :ok tone) (not= 0 exit)) (conj "a green banner with exit 1")
      (and (= :error tone) (not= 1 exit)) (conj "a red banner with exit 0")
      (and (str/starts-with? text "DONE") (not= 0 exit)) (conj "DONE with exit 1")
      (and (re-find #"^(FAILED|NOT VERIFIED|BLOCKED)|^DRY RUN failed" text) (not= 1 exit))
      (conj "a failure banner with exit 0")
      (and (seq (:unconfirmed record)) (re-find #"verified" text) (not (re-find #"NOT VERIFIED" text)))
      (conj "claims verification over an unconfirmed post-check")
      (and changed? (not (:dry-run opts)) (str/includes? text "nothing was changed"))
      (conj "says nothing was changed after a change ran")
      (str/includes? text "0 effects") (conj "counts 0 effects"))))

(deftest the-banner-and-the-exit-code-agree-for-every-run
  (let [checked (atom 0)
        broken (atom [])]
    (doseq [config books
            dry-run? [false true]
            outcome [:ok :failed :error]
            findings? [false true]
            record (records)
            :let [opts {:dry-run dry-run?}
                  problems (violations config opts outcome record findings?)]]
      (swap! checked inc)
      (when (and (seq problems) (< (count @broken) 5))
        (swap! broken conj {:config config :opts opts :outcome outcome :record record
                            :verdict (runbook/verdict config opts outcome record findings?)
                            :problems problems})))
    (is (< 100000 @checked))
    (is (empty? @broken) (pr-str @broken))))

(deftest the-contract-test-catches-a-banner-that-ignores-the-record
  ;; A positive control: an exit code judged from the outcome alone lets a
  ;; recorded failure exit 0 under a red banner, which the contract names.
  (with-redefs [outcome/exit-code (fn [o _] (if (= :ok o) 0 1))]
    (is (seq (violations {:announce? true} {} :ok {:applied 1 :stopped "x"} false)))))

;; --- a change book that posts nothing --------------------------------------------

(def ^:private silent-change {:announce? false :change? true})

(deftest a-change-book-that-posts-nothing-is-bannered-as-a-change
  (testing "a rehearsal is a rehearsal"
    (is (= {:tone :neutral :text "DRY RUN - nothing was changed, this was a rehearsal"}
           (:banner (runbook/verdict silent-change {:dry-run true} :ok {:applied 0} false)))))
  (testing "a real run says what it applied"
    (is (= "DONE - applied; this book has no post-checks"
           (get-in (runbook/verdict silent-change {} :ok {:applied 1} false) [:banner :text]))))
  (testing "a failed run names the effect it stopped at"
    (is (str/starts-with? (get-in (runbook/verdict silent-change {} :failed {:applied 1 :stopped "kubectl exec"} false)
                                  [:banner :text])
                          "FAILED after 1 effect - stopped at: kubectl exec")))
  (testing "an unmet precondition is red and exits 1"
    (let [{:keys [banner exit]} (runbook/verdict silent-change {:dry-run true} :ok {:applied 0 :unmet ["exec"]} false)]
      (is (= :error (:tone banner)))
      (is (= 1 exit))))
  (testing "a book changing only under a flag is judged on the parsed options"
    (is (= "DONE - applied; this book has no post-checks"
           (get-in (runbook/verdict {:announce? false :change? (fn [opts] (boolean (:promote opts)))} {:promote true} :ok {:applied 1} false)
                   [:banner :text])))))

(deftest etcd-check-performance-is-bannered-as-a-change
  (let [config (binding [runbook/*loading* true] (load-file "runbooks/etcd/check_performance.clj"))]
    (is (false? (:announce? config)))
    (is (= "DRY RUN - nothing was changed, this was a rehearsal"
           (get-in (runbook/verdict config {:dry-run true} :ok {:applied 0} false) [:banner :text])))))

;; --- a failed rehearsal ----------------------------------------------------------------

(deftest a-rehearsal-that-failed-before-its-plan-says-so
  (let [{:keys [banner exit]} (runbook/verdict {:announce? true} {:dry-run true} :failed {:applied 0} false)]
    (is (= {:tone :error
            :text "DRY RUN failed - nothing was changed, and the plan was not shown in full - read the messages above"}
           banner))
    (is (= 1 exit))))

;; --- notes change nothing --------------------------------------------------------------

(defn- recorded
  [f]
  (let [record (outcome/fresh)
        result (binding [outcome/*record* record
                         *err* (java.io.StringWriter.)
                         *out* (java.io.StringWriter.)]
                 (f))]
    {:result result :record @record}))

(defn- run-plan
  "The record effect/run! leaves for plan, with every command exiting with exit."
  [plan exit]
  (with-redefs [shell/run-with-output (fn [& _] exit)]
    (recorded #(effect/run! plan))))

(deftest a-note-is-never-an-applied-effect
  (testing "a note then a failing patch changed nothing"
    (let [{:keys [record]} (run-plan [(effect/note "consumers keep serving")
                                      (effect/kubectl "ctx" "patch" "jsonnetsnippets" "s")] 1)]
      (is (= 0 (:applied record)))
      (is (= "FAILED at the first effect: kubectl patch jsonnetsnippets s - nothing was changed"
             (:text (outcome/banner {:changes? true :outcome :failed} record))))))
  (testing "a plan of notes changed nothing"
    (let [{:keys [record]} (run-plan [(effect/note "nothing to restart")] 0)]
      (is (= 0 (:applied record)))
      (is (= "DONE - nothing was changed" (:text (outcome/banner {:changes? true :outcome :ok} record))))))
  (testing "notes between effects are not counted"
    (let [{:keys [record]} (run-plan [(effect/note "a") (effect/kubectl "ctx" "cordon" "w-1")
                                      (effect/note "b") (effect/kubectl "ctx" "drain" "w-1")] 0)]
      (is (= 2 (:applied record))))))

(deftest what-a-step-left-behind-skips-its-notes
  (is (= ["kubectl cordon w-1"]
         (playbook/applied-effects [[:note "first"] [:kubectl "ctx" "cordon" "w-1"] [:kubectl "ctx" "drain" "w-1"]] 1)))
  (let [plan [[:note "first"] [:kubectl "ctx" "cordon" "w-1"] [:kubectl "ctx" "drain" "w-1"]]
        left (binding [outcome/*record* (atom {:applied 1 :stopped "kubectl drain w-1"})]
               ((deref #'mutation/left-behind) {:select {:mode :one}} {:cluster "c" :target "w-1"} plan 0))]
    (is (str/includes? (str left) "kubectl cordon w-1"))
    (is (not (str/includes? (str left) "# first")))))

;; --- a soft post-check that ran out its time ---------------------------------------

(deftest a-soft-post-check-that-timed-out-is-not-claimed-as-verified
  (let [{:keys [result record]} (recorded
                                 #(do (outcome/applied!)
                                      (verify/all [{:description "the claim reports 20Gi" :soft? true
                                                    :timeout 0 :check (constantly false)}])))
        {:keys [banner exit]} (runbook/verdict {:announce? true} {} :ok record false)]
    (is (true? result) "a soft check does not fail the run")
    (is (= ["the claim reports 20Gi"] (:unconfirmed record)))
    (is (= {:tone :neutral :text "DONE - applied, NOT confirmed (best effort): the claim reports 20Gi"} banner))
    (is (= 0 exit))))

(deftest a-soft-post-check-that-held-is-verified
  (let [{:keys [record]} (recorded #(do (outcome/applied!)
                                        (verify/all [{:description "x" :soft? true :timeout 0 :check (constantly true)}])))]
    (is (nil? (:unconfirmed record)))
    (is (= "DONE - applied and verified" (:text (outcome/banner {:changes? true :outcome :ok} record))))))

;; --- a legacy thunk ------------------------------------------------------------------

(deftest a-run-whose-thunk-ran-never-says-nothing-was-applied
  (doseq [[outcome expected]
          [[:failed "FAILED - read the messages above; a step whose effects are unknown had run"]
           [:error "FAILED - an unexpected error stopped the run; a step whose effects are unknown had run before it - read the messages above"]]]
    (testing outcome
      (is (= expected (:text (outcome/banner {:changes? true :outcome outcome} {:applied 0 :thunk :ok})))))))

(deftest a-thunk-run-through-flow-is-named-when-a-later-step-fails
  (let [{:keys [record]} (recorded #(flow/change! {:confirmation {:action "restart" :target "t" :items ["t"]}
                                                    :confirmed? true
                                                    :execute! (constantly true)}))]
    (is (= :ok (:thunk record)))
    (is (= "FAILED - read the messages above; a step whose effects are unknown had run"
           (:text (outcome/banner {:changes? true :outcome :failed} record))))))
