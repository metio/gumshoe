;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.playbook-prejudge-test
  "A playbook judges its steps before its one confirmation as far as their
   evidence exists: every change's pick, and the refusal of the first
   change unless a waiting pause comes before it. A later step is judged
   when the run reaches it. The world here is an atom the steps' effects
   change, so a refusal reads what an earlier step did."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.playbook :as playbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

;; --- a node the steps cordon and uncordon ------------------------------------------------

(defn- change
  "A change step named action whose effect applies (apply-to world) and
   whose refusal is (refuse world), recording each refusal call with the
   phase it ran in under :judged."
  [action world refuse apply-to & {:as extra}]
  (merge {:select {:mode :one :label "Node"}
          :confirm {:action action}
          :refusal (fn [_]
                     (swap! world update :judged (fnil conj []) [action mutation/*phase*])
                     (refuse @world))
          :effect (fn [_] (effect/plan (effect/note action)))
          ::apply apply-to}
         extra))

(defn- cordon [world]
  (change "cordon" world #(when (:cordoned? %) "the node is cordoned already") #(assoc % :cordoned? true)))

(defn- drain [world]
  (change "drain" world #(when-not (:cordoned? %) "the node is not cordoned") #(assoc % :drained? true)))

(defn- uncordon [world]
  (change "uncordon" world #(when-not (:cordoned? %) "the node is not cordoned") #(assoc % :cordoned? false)))

(defn- look [world]
  {:title "look" :check (fn [_] (swap! world update :looked (fnil inc 0)) true)})

(defn- run
  "Runs a playbook of steps against w-1 with the world starting as world,
   the operator confirming and waiting at every pause, at a terminal.
   Returns the result, what was printed, the confirmations asked, the
   effects applied in order and the world after."
  [world steps & {:keys [opts dry-run?]}]
  (let [asked (atom [])
        applied (atom [])
        by-action (into {} (keep (fn [s] (when-let [f (::apply s)] [(get-in s [:confirm :action]) f])) steps))
        out (java.io.StringWriter.)
        result (with-redefs-fn
                 {#'gumshoe.playbook/attended? (constantly true)
                  #'gumshoe.playbook/wait-for-operator! (fn [_] true)}
                 #(with-redefs [kubectl/current-context (constantly "ctx")
                                kubectl/current-cluster (constantly "cluster")
                                interact/confirm! (fn [{:keys [action]}] (swap! asked conj action) true)
                                effect/run! (fn [plan]
                                              (doseq [[_ action] plan]
                                                (swap! applied conj action)
                                                (swap! world (by-action action)))
                                              true)]
                    (binding [*out* out *err* out flow/*dry-run* (boolean dry-run?)]
                      (playbook/run-on! {:select {:mode :one :label "Node"}
                                         :confirm {:action "maintain the node"}
                                         :steps (mapv (fn [step] (dissoc step ::apply)) steps)}
                                        "w-1" (or opts {}) nil))))]
    {:result result :out (stdout/strip-colors (str out)) :asked @asked :applied @applied :world @world}))

;; --- which steps are judged before the confirmation ----------------------------------

(deftest prejudgeable-names-the-first-change-before-any-waiting-pause
  (let [w (atom {})
        c (cordon w) k (look w) p {:pause "act"}]
    (doseq [[label steps waits-first? =>]
            [["no steps" [] true nil]
             ["checks alone" [k k] true nil]
             ["the first of two changes" [c c] true [0]]
             ["a change after checks" [k k c c] true [2]]
             ["a change after a pause that waits" [p c] true nil]
             ["a change after the pause opening a resumed segment" [p c] false [1]]
             ["a change after a pause past the first step" [k p c] false nil]
             ["a change after a check and a non-waiting opening pause" [p k c] false [2]]]]
      (testing label
        (is (= => (playbook/prejudgeable steps waits-first?)))))))

;; --- the first change ----------------------------------------------------------------

(deftest a-first-change-that-would-be-refused-is-refused-before-the-confirmation
  (let [world (atom {:cordoned? true})
        {:keys [result out asked applied]} (run world [(look world) (cordon world) (drain world)])]
    (is (false? result))
    (is (= [] asked) "the operator is never asked to approve the playbook")
    (is (= [] applied))
    (is (= 1 (:looked @world)) "the leading checks ran and reported first")
    (is (re-find #"2/3 cordon[^\n]*\n\s*refusing: the node is cordoned already" out) out)
    (is (= [["cordon" :derive]] (:judged @world)))))

(deftest a-first-change-judged-before-is-judged-again-after-the-confirmation
  (let [world (atom {})
        {:keys [result asked applied]} (run world [(cordon world) (drain world)])]
    (is (true? result))
    (is (= ["maintain the node"] asked))
    (is (= ["cordon" "drain"] applied))
    (is (= [["cordon" :derive] ["cordon" :judge] ["drain" :judge]] (:judged @world))
        "the first change before and after the confirmation, the second only when reached")))

(deftest a-change-after-a-change-is-judged-only-when-reached
  (testing "its evidence depends on the earlier change, so judging it before would refuse falsely"
    (let [world (atom {})
          {:keys [result applied]} (run world [(cordon world) (drain world)])]
      (is (true? result))
      (is (= ["cordon" "drain"] applied)))))

(deftest a-change-after-a-waiting-pause-is-judged-only-when-reached
  (let [world (atom {:cordoned? false})
        steps [(cordon world) {:pause "maintain it"} (uncordon world)]
        {:keys [result applied]} (run world steps)]
    (is (true? result) "uncordon, refused before the cordon ran, is not judged before the confirmation")
    (is (= ["cordon" "uncordon"] applied))))

(deftest a-resumed-segment-judges-the-change-after-its-opening-pause
  (let [world (atom {:cordoned? false})
        steps [(cordon world) {:pause "maintain it"} (uncordon world)]
        {:keys [result asked applied out]} (run world steps :opts {:from-segment 2})]
    (is (false? result))
    (is (= [] asked) "the manual step is done, so the uncordon's evidence exists now")
    (is (= [] applied))
    (is (str/includes? out "refusing: the node is not cordoned"))))

;; --- picks ---------------------------------------------------------------------------

(deftest a-later-step-whose-pick-does-not-parse-is-refused-before-the-confirmation
  (let [world (atom {})
        namespaced (assoc (drain world) :select {:mode :namespaced :label "Node"})
        {:keys [result asked applied out]} (run world [(cordon world) namespaced])]
    (is (false? result))
    (is (= [] asked))
    (is (= [] applied) "the first change never ran")
    (is (str/includes? out "refusing: the pick \"w-1\" does not name a Node") out)))

;; --- a point of no return ------------------------------------------------------------

(deftest a-first-change-that-asks-again-is-judged-before-both-confirmations
  (let [world (atom {:cordoned? true})
        separate (assoc-in (cordon world) [:confirm :separately?] true)
        {:keys [result asked]} (run world [separate])]
    (is (false? result))
    (is (= [] asked)))
  (testing "and once more, over fresh reads, before its own confirmation"
    (let [world (atom {})
          separate (assoc-in (cordon world) [:confirm :separately?] true)
          {:keys [result asked]} (run world [separate])]
      (is (true? result))
      (is (= ["maintain the node" "cordon"] asked))
      (is (= [["cordon" :derive] ["cordon" :derive] ["cordon" :judge]] (:judged @world))))))

;; --- a rehearsal ---------------------------------------------------------------------

(deftest a-rehearsal-judges-each-step-as-it-reaches-it
  (let [world (atom {:cordoned? true})
        {:keys [result asked applied out]} (run world [(cordon world) (drain world)] :dry-run? true)]
    (is (true? result))
    (is (= [] asked))
    (is (= [] applied))
    (is (= [["cordon" :judge] ["drain" :judge]] (:judged @world)) "once per step, as before")
    (is (str/includes? out "this step's precondition does not hold against the current state"))))

;; --- what the judgement costs and prints ---------------------------------------------

(defn- answer
  [objects k]
  (if-let [o (get objects k)]
    {:exit 0 :out (json/generate-string o) :err ""}
    {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" (first k) (last k))}))

(deftest the-first-change-reads-its-subject-once-before-the-confirmation-and-once-after
  (let [gets (atom [])
        node {:metadata {:name "w-1" :uid "u" :generation 1}}
        step {:select {:mode :one :label "Node"}
              :confirm {:action "cordon"}
              :subject {:type "nodes"}
              :refusal (fn [{:keys [object]}] (when-not object "gone"))
              :effect (fn [_] (effect/plan (effect/note "cordon")))}]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  shell/execute (fn [& argv]
                                  (let [[_ type name] (drop-while #(not= "get" %) argv)]
                                    (swap! gets conj [type name mutation/*phase*])
                                    (answer {["nodes" "w-1"] node} [type name])))
                  interact/confirm! (constantly true)
                  effect/run! (constantly true)]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out]
                     (playbook/run-on! {:select {:mode :one :label "Node"} :confirm {:action "maintain"} :steps [step]}
                                       "w-1" {} nil))]
        (is (true? result) (str out))
        (is (= {["nodes" "w-1" :derive] 1 ["nodes" "w-1" :judge] 1} (frequencies @gets))
            "the step's :derive phase is the one judged before the playbook's confirmation")))))

(deftest a-judgement-that-passes-prints-nothing-before-the-confirmation
  (let [world (atom {})
        noisy (assoc (cordon world) :precondition (fn [_] (println "what the cordon is about to change") true))
        {:keys [result out]} (run world [noisy])]
    (is (true? result))
    (is (= 1 (count (re-seq #"what the cordon is about to change" out))) out)))

(deftest a-driver-judges-a-segment-before-confirming-it
  (let [world (atom {:cordoned? true})
        steps (first (playbook/segments [(cordon world)]))
        out (java.io.StringWriter.)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")]
      (binding [*out* out *err* out]
        (is (nil? (playbook/prejudge! steps {:target "w-1" :opts {}} true)))))
    (is (str/includes? (str out) "refusing: the node is cordoned already"))
    (testing "steps that may be confirmed come back, the judged change carrying its cache"
      (reset! world {})
      (with-redefs [kubectl/current-context (constantly "ctx")
                    kubectl/current-cluster (constantly "cluster")]
        (let [judged (playbook/prejudge! steps {:target "w-1" :opts {}} true)]
          (is (= 1 (count judged)))
          (is (some? (::playbook/derive-cache (first judged)))))))))
