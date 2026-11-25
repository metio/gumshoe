;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.playbook-done-test
  "A playbook whose changes declare :done resumes an interrupted run by
   skipping the steps that took effect: each change is judged done before
   the one confirmation as far as its evidence exists, judged again when
   reached, and a run with nothing left to do asks nothing. The world here
   is an atom the steps' effects change, so a step judges what an earlier
   step did."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.playbook :as playbook]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

;; --- steps over a world ------------------------------------------------------------------

(defn- change
  "A change step named action whose effect applies (apply-to world) and that
   is done when (holds? world), which is also its post-check. Each judgement
   of :done is recorded under :judged with the phase it ran in."
  [action world holds? apply-to & {:as extra}]
  (merge {:select {:mode :one :label "Node"}
          :confirm {:action action :destructive? false}
          :effect (fn [_] (effect/plan (effect/note action)))
          :verify (fn [_] [{:description (str action " holds") :check #(boolean (holds? @world))}])
          :done (fn [_]
                  (swap! world update :judged (fnil conj []) [action mutation/*phase*])
                  true)
          ::apply apply-to}
         extra))

(defn- setting
  "A change that sets k in the world to v, done once k is v."
  [world k v]
  (change (str "set " (name k)) world #(= v (get % k)) #(assoc % k v)))

(defn- look [world]
  {:title "look" :check (fn [_] (swap! world update :looked (fnil inc 0)) true)})

(defn- run
  "Runs a playbook of steps against w-1 with the world starting as world,
   the operator confirming and waiting at every pause, at a terminal.
   `failing` names an action whose effect fails; `confirming` edits the
   world while the confirmation is shown. Returns the result, what was
   printed, the confirmations asked with their items, the effects applied
   in order, the world after and the run record."
  [world steps & {:keys [failing confirming dry-run?]}]
  (let [asked (atom [])
        applied (atom [])
        record (outcome/fresh)
        by-action (into {} (keep (fn [s] (when-let [f (::apply s)] [(get-in s [:confirm :action]) f])) steps))
        out (java.io.StringWriter.)
        eventually verify/eventually
        result (with-redefs-fn
                 {#'gumshoe.playbook/attended? (constantly true)
                  #'gumshoe.playbook/wait-for-operator! (fn [_] true)}
                 #(with-redefs [kubectl/current-context (constantly "ctx")
                                kubectl/current-cluster (constantly "cluster")
                                interact/confirm! (fn [{:keys [action items]}]
                                                    (swap! asked conj {:action action :items (vec items)})
                                                    (when confirming (swap! world confirming))
                                                    true)
                                effect/run! (fn [plan]
                                              (every? (fn [[_ action]]
                                                        (if (= failing action)
                                                          (do (outcome/stopped! action) false)
                                                          (do (swap! applied conj action)
                                                              (swap! world (by-action action))
                                                              true)))
                                                      plan))
                                verify/eventually (fn [check] (eventually (assoc check :timeout 0)))]
                    (binding [*out* out *err* out
                              outcome/*record* record
                              flow/*dry-run* (boolean dry-run?)]
                      (playbook/run-on! {:select {:mode :one :label "Node"}
                                         :confirm {:action "maintain the node"}
                                         :steps (mapv (fn [step] (dissoc step ::apply)) steps)}
                                        "w-1" {} nil))))]
    {:result result :out (stdout/strip-colors (str out)) :asked @asked :applied @applied
     :world @world :record @record}))

;; --- the rules, pure ------------------------------------------------------------------------

(deftest prejudgeable-walks-past-every-change-judged-done
  (let [w (atom {})
        c (setting w :a 1) k (look w) p {:pause "act"}]
    (doseq [[label steps waits-first? done =>]
            [["nothing done: the first change" [c c c] true #{} [0]]
             ["the first done: it and the second" [c c c] true #{0} [0 1]]
             ["two done: all three" [c c c] true #{0 1} [0 1 2]]
             ["every change done" [c c] true #{0 1} [0 1]]
             ["checks between do not end the walk" [c k c] true #{0} [0 2]]
             ["a waiting pause ends it" [c p c] true #{0} [0]]
             ["a done change before a waiting pause is still judged" [k c p c] true #{1} [1]]
             ["a non-waiting opening pause does not end it" [p c c] false #{1} [1 2]]
             ["a change after a waiting opening pause is not judged" [p c] true #{} nil]
             ["a done index that is not reached changes nothing" [c c] true #{1} [0]]]]
      (testing label
        (is (= => (playbook/prejudgeable steps waits-first? done)))))))

(deftest nothing-to-do-needs-every-change-done-and-no-waiting-pause
  (let [w (atom {})
        done (assoc (setting w :a 1) ::playbook/done {:done? true :held []})
        open (setting w :a 1) k (look w) p {:pause "act"}]
    (doseq [[label steps waits-first? =>]
            [["every change done" [done done] true true]
             ["one change not done" [done open] true false]
             ["checks only" [k k] true false]
             ["done changes and checks" [k done k] true true]
             ["a waiting pause among them" [done p k] true false]
             ["a non-waiting opening pause" [p done] false true]
             ["a waiting opening pause" [p done] true false]]]
      (testing label
        (is (= => (playbook/nothing-to-do? steps waits-first?)))))))

;; --- running twice -------------------------------------------------------------------------

(deftest a-playbook-run-again-skips-what-the-first-run-did-and-asks-nothing
  (let [world (atom {})
        steps [(setting world :suspended true) (look world) (setting world :replicas 3)]
        first-run (run world steps)
        second-run (run world steps)]
    (testing "the first run does everything"
      (is (true? (:result first-run)))
      (is (= ["set suspended" "set replicas"] (:applied first-run)))
      (is (= 1 (count (:asked first-run)))))
    (testing "the second run skips every change, runs the check and asks nothing"
      (is (true? (:result second-run)))
      (is (= [] (:asked second-run)))
      (is (= [] (:applied second-run)))
      (is (str/includes? (:out second-run) "every change of this playbook is already done - nothing to confirm"))
      (is (= ["set suspended" "set replicas"] (get-in second-run [:record :skipped])))
      (is (= 2 (get-in second-run [:world :looked])) "the check ran on both runs")
      (is (= "DONE - already done, nothing was changed - skipped: set suspended, set replicas"
             (:text (outcome/banner {:changes? true :outcome :ok} (:record second-run))))))))

(deftest a-run-interrupted-at-any-step-resumes-by-skipping-exactly-the-steps-that-took-effect
  (let [keys [:a :b :c :d]]
    (doseq [stop-at (range (count keys))]
      (testing (str "stopped at step " (inc stop-at))
        (let [world (atom {})
              steps (mapv #(setting world % true) keys)
              actions (mapv #(str "set " (name %)) keys)
              interrupted (run world steps :failing (nth actions stop-at))
              resumed (run world steps)
              done (take stop-at actions)
              rest-of (drop stop-at actions)]
          (is (false? (:result interrupted)))
          (is (= (vec done) (:applied interrupted)) "the first run applied the steps before the failure")
          (is (true? (:result resumed)))
          (is (= (vec rest-of) (:applied resumed)) "the resumed run performs the rest")
          (is (= (vec done) (vec (get-in resumed [:record :skipped]))) "and skips exactly what took effect")
          (is (= 1 (count (:asked resumed))) "something is left, so the operator confirms")
          (is (= (concat ["w-1"]
                         (map-indexed (fn [i a] (str (inc i) "/4 " a (when (< i stop-at) " (already done)"))) actions))
                 (:items (first (:asked resumed))))
              "the confirmation marks the steps already done"))))))

(deftest a-second-run-is-told-apart-from-steps-never-reached
  (let [world (atom {})
        steps [(setting world :a true) (setting world :b true) (setting world :c true)]
        _ (run world steps :failing "set b")
        stopped (run world steps :failing "set c")]
    (is (= ["set a"] (get-in stopped [:record :skipped])) "skipped: judged done")
    (is (= ["set b"] (:applied stopped)) "applied")
    (is (= "set c" (get-in stopped [:record :stopped])) "stopped at: the step never past")))

;; --- judging before and after the confirmation ------------------------------------------------

(deftest a-step-done-before-the-confirmation-is-judged-again-when-reached
  (testing "still done: skipped"
    (let [world (atom {:a true})
          {:keys [applied world]} (run world [(setting world :a true) (setting world :b true)])]
      (is (= ["set b"] applied))
      (is (= [["set a" :derive] ["set a" :derive]] (filter #(= "set a" (first %)) (:judged world)))
          "once before the confirmation, once when reached")))
  (testing "no longer done when reached: it runs, as the confirmation covered it"
    (let [world (atom {:a true})
          {:keys [applied asked record]} (run world [(setting world :a true) (setting world :b true)]
                                              :confirming #(dissoc % :a))]
      (is (= 1 (count asked)))
      (is (= ["set a" "set b"] applied))
      (is (nil? (:skipped record))))))

(deftest a-later-change-is-judged-done-before-the-confirmation-only-past-done-changes
  (let [world (atom {:b true})
        {:keys [asked applied world]} (run world [(setting world :a true) (setting world :b true)])]
    (is (= ["w-1" "1/2 set a" "2/2 set b"] (:items (first asked)))
        "set b is not marked: set a runs first and may change what it judges")
    (is (= ["set a"] applied))
    (is (= [["set a" :derive] ["set b" :derive]] (:judged world))
        "set a is judged before the confirmation, set b only when reached")))

(deftest a-change-that-took-effect-after-the-step-before-it-is-skipped-when-reached
  (let [world (atom {})
        a (change "set a" world :a #(assoc % :a true :b true))
        b (setting world :b true)
        {:keys [applied record]} (run world [a b])]
    (is (= ["set a"] applied))
    (is (= ["set b"] (:skipped record)))))

(deftest a-waiting-pause-keeps-the-confirmation
  (let [world (atom {:a true})
        {:keys [asked applied]} (run world [(setting world :a true) {:pause "act"} (look world)])]
    (is (= 1 (count asked)) "the pause has its manual step ahead of it")
    (is (= [] applied))))

(deftest a-step-that-asks-separately-is-skipped-like-any-other
  (let [world (atom {:a true})
        step (assoc-in (setting world :a true) [:confirm :separately?] true)
        {:keys [asked applied record]} (run world [step])]
    (is (= [] asked))
    (is (= [] applied))
    (is (= ["set a"] (:skipped record)))))

(deftest a-done-judgement-that-did-not-answer-is-read-afresh-when-reached
  (let [world (atom {:a true})
        calls (atom 0)
        step (assoc (setting world :a true)
                    :done (fn [_]
                            (when (= 1 (swap! calls inc))
                              (throw (ex-info "the server did not answer" {::kubectl/unread true})))
                            true))
        {:keys [asked applied record out]} (run world [step (setting world :b true)])]
    (is (= ["w-1" "1/2 set a" "2/2 set b"] (:items (first asked))) "not marked done: it could not be told")
    (is (= ["set b"] applied))
    (is (= ["set a"] (:skipped record)) "judged again when reached, where it answered")
    (is (not (str/includes? out "could not tell")) "the judgement before the confirmation prints nothing")))

(deftest a-change-whose-done-never-answers-runs-as-one-that-is-not-done
  (let [world (atom {:a true})
        step (assoc (setting world :a true)
                    :done (fn [_] (throw (ex-info "the server did not answer" {::kubectl/unread true}))))
        {:keys [applied out record]} (run world [step])]
    (is (= ["set a"] applied))
    (is (nil? (:skipped record)))
    (is (str/includes? out "could not tell whether this change is already done - the server did not answer") out)))

(deftest a-change-refused-before-the-confirmation-is-still-refused-past-done-steps
  (let [world (atom {:a true})
        b (assoc (setting world :b true) :refusal (constantly "b is locked"))
        {:keys [result asked applied out]} (run world [(setting world :a true) b])]
    (is (false? result))
    (is (= [] asked))
    (is (= [] applied))
    (is (str/includes? out "refusing: b is locked"))))

;; --- rehearsals ---------------------------------------------------------------------------------

(deftest a-rehearsal-judges-no-change-done-after-one-it-would-run
  (testing "a later change done now is rehearsed, since the change before it would run first"
    (let [world (atom {:b true})
          {:keys [out record applied]} (run world [(setting world :a true) (setting world :b true)] :dry-run? true)]
      (is (= [] applied))
      (is (nil? (:skipped record)))
      (is (= 2 (count (re-seq #"DRY RUN - nothing will be changed" out))) out)))
  (testing "changes done now before any that would run are reported skipped"
    (let [world (atom {:a true})
          {:keys [out record]} (run world [(setting world :a true) (setting world :b true)] :dry-run? true)]
      (is (= ["set a"] (:skipped record)))
      (is (= 1 (count (re-seq #"DRY RUN - nothing will be changed" out))) out)))
  (testing "a change after a pause that would wait is rehearsed"
    (let [world (atom {:a true})
          {:keys [record]} (run world [{:pause "act"} (setting world :a true)] :dry-run? true)]
      (is (nil? (:skipped record))))))
