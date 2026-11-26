;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.playbook-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.playbook :as playbook]
            [gumshoe.runbook]
            [gumshoe.stdout]
            [gumshoe.theme]
            [gumshoe.steps.nodes :as nodes]))

(defn- change
  "A change whose plan names its action in a note and applies one effect, a
   command that succeeds; a note alone changes nothing and is not counted."
  [action ran & {:as extra}]
  (merge {:select {:mode :one :label "Node"}
          :confirm {:action action}
          :effect (fn [{:keys [target]}] (swap! ran conj [action target]) [[:note action] [:cmd "true"]])}
         extra))

(defn- check
  [title ran ok?]
  {:title title :check (fn [_] (swap! ran conj [title]) ok?)})

(defmacro ^:private quietly
  [& body]
  `(binding [*err* (java.io.StringWriter.) *out* (java.io.StringWriter.)]
     (with-redefs-fn {#'gumshoe.playbook/attended? (constantly false)}
       #(with-redefs [kubectl/current-context (constantly "ctx")
                      kubectl/current-cluster (constantly "cluster")]
          ~@body))))

(deftest a-playbook-needs-what-every-step-needs
  (is (= {:installed-tools ["kubectl" "fzf" "git"]
          :cluster-capabilities []
          :kubectl-can-get ["nodes" "pods" "daemonsets"]
          :kubectl-can-patch ["nodes"]
          :kubectl-can-create ["pods/eviction"]}
         (playbook/all-prerequisites {:prerequisites {:installed-tools ["git"]}
                                      :steps [nodes/cordon nodes/drain {:pause "x"} nodes/uncordon]}))))

(deftest a-playbook-reads-every-option-its-steps-read
  (is (= #{:node :grace-period :drain-timeout}
         (set (keys (playbook/all-options {:steps [nodes/cordon nodes/drain nodes/uncordon]})))))
  (testing "the playbook's own declaration wins"
    (is (= "mine" (get-in (playbook/all-options {:options {:node {:desc "mine" :alias :n :coerce :string}}
                                                 :steps [nodes/cordon]})
                          [:node :desc]))))
  (testing "steps that read one option differently can not be composed"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"--node"
                          (playbook/all-options {:steps [nodes/cordon
                                                         {:check identity :options {:node {:coerce :long}}}]})))))

(deftest a-step-without-a-kind-is-refused
  (is (thrown? clojure.lang.ExceptionInfo (playbook/kind {:title "nothing"}))))

(deftest pauses-split-the-steps-into-numbered-segments
  (let [ran (atom [])
        segs (playbook/segments [(check "look" ran true) (change "a" ran) {:pause "act"} (change "b" ran)])]
    (is (= [["1/3 look" "2/3 a"] ["⏸  act" "3/3 b"]]
           (mapv #(mapv ::playbook/title %) segs)))
    (is (= [[1 1] [2 2]] (mapv #(mapv ::playbook/segment %) segs)))))

(deftest one-confirmation-lists-every-step-and-is-destructive-when-any-change-is
  (let [ran (atom [])
        steps (apply concat (playbook/segments [(change "cordon" ran)
                                                {:pause "act"}
                                                (change "drain" ran :confirm {:action "drain" :destructive? true})]))]
    (is (= {:action "maintain"
            :target "cluster"
            :items ["w-1" "1/2 cordon" "⏸  act" "2/2 drain"]
            :destructive? true}
           (update (playbook/confirmation {:confirm {:action "maintain"} :select {:mode :one}}
                                          steps {:cluster "cluster" :target "w-1"})
                   :items vec)))))

(deftest leading-checks-run-before-the-one-confirmation-and-the-changes-after-it
  (let [ran (atom [])
        confirmations (atom 0)
        pb {:select {:mode :one} :confirm {:action "maintain"}
            :steps [(check "look" ran true) (change "cordon" ran) (change "drain" ran)]}]
    (quietly
     (with-redefs [interact/confirm! (fn [_] (swap! ran conj [:confirm]) (swap! confirmations inc) true)]
       (is (true? (playbook/run-on! pb "w-1" {} nil)))))
    (is (= [["look"] [:confirm] ["cordon" "w-1"] ["drain" "w-1"]] @ran))
    (is (= 1 @confirmations))))

(deftest a-declined-confirmation-runs-no-change
  (let [ran (atom [])]
    (quietly
     (with-redefs [interact/confirm! (constantly false)]
       (is (false? (playbook/run-on! {:select {:mode :one} :confirm {:action "x"}
                                      :steps [(change "cordon" ran)]}
                                     "w-1" {} nil)))))
    (is (empty? @ran))))

(deftest a-failing-step-stops-the-playbook
  (let [ran (atom [])]
    (quietly
     (with-redefs [interact/confirm! (constantly true)]
       (is (false? (playbook/run-on! {:select {:mode :one} :confirm {:action "x"}
                                      :steps [(change "cordon" ran) (check "fails" ran false) (change "drain" ran)]}
                                     "w-1" {} nil)))))
    (is (= [["cordon" "w-1"] ["fails"]] @ran))))

(deftest a-pause-without-a-terminal-stops-and-names-the-segment-to-continue-with
  (let [ran (atom [])
        record (outcome/fresh)
        pb {:select {:mode :one} :confirm {:action "x"}
            :steps [(change "cordon" ran) {:pause "do the maintenance"} (change "uncordon" ran)]}]
    (quietly
     (with-redefs [interact/confirm! (constantly true)]
       (binding [outcome/*record* record]
         (is (false? (playbook/run-on! pb "w-1" {} nil))))))
    (is (= [["cordon" "w-1"]] @ran))
    (is (= "continue with --from-segment 2 once this is done: do the maintenance" (:paused @record)))
    (testing "the named segment checks the pause's result without waiting, then continues"
      (let [verified (atom false)
            pb (assoc-in pb [:steps 1 :verify] (fn [_] [{:description "done" :check #(reset! verified true)}]))]
        (reset! ran [])
        (quietly
         (with-redefs [interact/confirm! (constantly true)]
           (is (true? (playbook/run-on! pb "w-1" {:from-segment 2} nil)))))
        (is @verified)
        (is (= [["uncordon" "w-1"]] @ran))))))

(deftest an-unattended-pause-continues-to-its-verification
  (let [ran (atom [])]
    (quietly
     (with-redefs [interact/confirm! (constantly true)]
       (is (true? (playbook/run-on! {:select {:mode :one} :confirm {:action "x"}
                                     :steps [{:pause "push" :unattended :continue
                                              :verify (fn [_] [{:description "pushed" :check (constantly true)}])}
                                             (change "drain" ran)]}
                                    "w-1" {} nil)))))
    (is (= [["drain" "w-1"]] @ran))))

(deftest a-dry-run-passes-every-pause-and-changes-nothing
  (let [ran (atom [])]
    (quietly
     (binding [flow/*dry-run* true]
       (is (true? (playbook/run-on! {:select {:mode :one} :confirm {:action "x"}
                                     :steps [(change "cordon" ran) {:pause "act"} (change "uncordon" ran)]}
                                    "w-1" {} nil)))))
    (is (= [["cordon" "w-1"] ["uncordon" "w-1"]] @ran) "the plans are built and shown, not run")))

(deftest one-segment-runs-alone-for-a-driver
  (let [ran (atom [])
        pb {:select {:mode :one} :confirm {:action "x"}
            :steps [(change "orphan" ran) (change "stamp" ran) {:pause "commit"} (change "drain" ran)]}]
    (quietly
     (is (true? (playbook/run-segment! pb 1 "ns-1" {} nil)))
     (is (false? (playbook/run-segment! pb 3 "ns-1" {} nil))))
    (is (= [["orphan" "ns-1"] ["stamp" "ns-1"]] @ran))))

(deftest a-point-of-no-return-asks-again-with-its-own-items
  (let [ran (atom [])
        asked (atom [])
        pb {:select {:mode :one} :confirm {:action "adopt"}
            :steps [(change "stamp" ran)
                    (change "precleanup" ran :confirm {:action "precleanup" :separately? true})]}]
    (is (= ["1/2 stamp" "2/2 precleanup (asks again)"]
           (drop 1 (:items (playbook/confirmation pb (apply concat (playbook/segments (:steps pb)))
                                                  {:target "ns-1"})))))
    (quietly
     (with-redefs [interact/confirm! (fn [request] (swap! asked conj (:action request)) true)]
       (is (true? (playbook/run-on! pb "ns-1" {} nil)))))
    (is (= ["adopt" "precleanup"] @asked))
    (testing "declining it stops the playbook before its change runs"
      (let [record (outcome/fresh)]
        (quietly
         (with-redefs [interact/confirm! (fn [request] (not= "precleanup" (:action request)))]
           (binding [outcome/*record* record]
             (is (false? (playbook/run-on! pb "ns-1" {} nil))))))
        (is (= 1 (:applied @record)) "only the stamp's effect ran")))))

(deftest help-describes-the-playbook-from-its-steps
  (let [lines (vec (playbook/details {:select {:mode :one :label "Node" :flag :node}
                                      :confirm {:action "maintain"}
                                      :steps [{:title "look" :check identity}
                                              (assoc nodes/cordon :title "Cordon")
                                              {:pause "do the work"}
                                              (assoc nodes/drain :title "Drain")]}))]
    (is (= ["What it does:"
            "  picks one Node (--node, or interactively), then:"
            "  1/3 look - reads and reports"
            "  - asks once to confirm every step below (destructive - type yes)"
            "  2/3 Cordon - cordon (mark unschedulable) a node, verified, skipped when already done"
            "  ⏸  do the work - waits for ENTER; --from-segment 2 continues here"
            "  3/3 Drain - drain a node - every pod on it is evicted and emptyDir data is deleted (destructive), verified, skipped when already done"
            ""
            "Needs:"]
           (subvec lines 0 9)))
    (is (some #{"  kubectl create: pods/eviction"} lines))))

(deftest a-pause-right-after-the-leading-checks-waits-for-the-operator
  (doseq [[what steps] {"after the leading checks" [(check "look" (atom []) true) {:pause "act"}]
                        "opening the playbook" [{:pause "act"}]}]
    (testing what
      (let [ran (atom [])
            waited (atom 0)
            pb {:select {:mode :one} :confirm {:action "maintain"}
                :steps (conj steps (change "after" ran))}]
        (binding [*err* (java.io.StringWriter.) *out* (java.io.StringWriter.)]
          (with-redefs-fn {#'gumshoe.playbook/attended? (constantly true)
                           #'gumshoe.playbook/wait-for-operator! (fn [_] (swap! waited inc) false)}
            #(with-redefs [kubectl/current-context (constantly "ctx")
                           kubectl/current-cluster (constantly "cluster")
                           interact/confirm! (constantly true)]
               (playbook/run-on! pb "w-1" {} {}))))
        (is (= 1 @waited) "the operator is asked to do the manual step")
        (is (empty? @ran) "nothing after the pause runs before they have"))))
  (testing "the pause opening a resumed segment does not wait again"
    (let [ran (atom [])
          waited (atom 0)
          pb {:select {:mode :one} :confirm {:action "maintain"}
              :steps [(change "before" ran) {:pause "act"} (change "after" ran)]}]
      (binding [*err* (java.io.StringWriter.) *out* (java.io.StringWriter.)]
        (with-redefs-fn {#'gumshoe.playbook/attended? (constantly true)
                         #'gumshoe.playbook/wait-for-operator! (fn [_] (swap! waited inc) false)}
          #(with-redefs [kubectl/current-context (constantly "ctx")
                         kubectl/current-cluster (constantly "cluster")
                         interact/confirm! (constantly true)]
             (playbook/run-on! pb "w-1" {:from-segment 2} {}))))
      (is (zero? @waited))
      (is (= [["after" "w-1"]] @ran)))))

(deftest a-playbook-opening-with-a-pause-can-be-resumed-where-it-says
  (let [ran (atom [])
        steps [{:pause "act"} (change "after" ran)]
        segs (playbook/segments steps)]
    (is (= 2 (::playbook/segment (first (playbook/numbered steps)))) "the pause names segment 2")
    (is (= 2 (count segs)) "so there is a segment 2")
    (is (= [] (first segs)) "and segment 1 holds nothing")
    (testing "--from-segment 2 runs the steps after the pause without waiting again"
      (let [waited (atom 0)]
        (binding [*err* (java.io.StringWriter.) *out* (java.io.StringWriter.)]
          (with-redefs-fn {#'gumshoe.playbook/attended? (constantly true)
                           #'gumshoe.playbook/wait-for-operator! (fn [_] (swap! waited inc) false)}
            #(with-redefs [kubectl/current-context (constantly "ctx")
                           kubectl/current-cluster (constantly "cluster")
                           interact/confirm! (constantly true)]
               (is (playbook/run-on! {:select {:mode :one} :confirm {:action "m"} :steps steps}
                                     "w-1" {:from-segment 2} {})))))
        (is (zero? @waited))
        (is (= [["after" "w-1"]] @ran))))))

(deftest a-drain-is-not-verified-by-a-pod-read-that-failed
  (let [check (:check (second (nodes/drain-checks "ctx" "n1")))]
    (with-redefs [gumshoe.shell/execute (fn [& _] {:exit 1 :out "" :err "the server is currently unable to handle the request"})]
      (is (false? (check)) "a failed read is no empty node"))
    (with-redefs [gumshoe.shell/execute (fn [& _] {:exit 0 :out "{\"items\":[]}" :err ""})]
      (is (true? (check)) "an answered read without pods is"))))

(def ^:private missing-binary "this-binary-truly-does-not-exist-xyz")

(defn- failing-change
  [action & {:as extra}]
  (merge {:select {:mode :one :label "Node"}
          :confirm {:action action}
          :effect (fn [_] [[:cmd missing-binary]])}
         extra))

(defn- printed-outcome
  "What the harness prints at the end of a real run of `pb` against w-1:
   {:ok? <run-on!'s result> :err <the banner and what follows it>}."
  [pb]
  (let [record (outcome/fresh)
        ok? (quietly
             (with-redefs [interact/confirm! (constantly true)]
               (binding [outcome/*record* record]
                 (playbook/run-on! pb "w-1" {} nil))))
        err (java.io.StringWriter.)]
    (binding [*err* err]
      (#'gumshoe.runbook/print-outcome!
       (outcome/banner {:changes? true :outcome (if ok? :ok :failed)} @record)
       @record))
    {:ok? ok? :err (str err)}))

(deftest the-failed-banner-prints-the-recovery-of-the-step-the-run-stopped-at
  (let [ran (atom [])
        {:keys [ok? err]} (printed-outcome
                           {:select {:mode :one} :confirm {:action "maintain"}
                            :steps [(change "cordon" ran)
                                    (failing-change "drain"
                                                    :recovery (fn [{:keys [target applied]}]
                                                                (format "%s stays cordoned after %d effect - uncordon it" target applied)))
                                    (change "uncordon" ran :recovery "never shown")]})
        banner-at (str/index-of err "FAILED after 1 effect - stopped at: " )
        recovery-at (str/index-of err "w-1 stays cordoned after 1 effect - uncordon it")]
    (is (false? ok?))
    (is (some? banner-at) err)
    (is (some? recovery-at) err)
    (is (< banner-at recovery-at) "the guidance prints under the banner")
    (is (not (str/includes? err "never shown")))))

(defn- noted-change
  "A change whose plan is only a note, so it changes nothing."
  [action]
  {:select {:mode :one :label "Node"}
   :confirm {:action action}
   :effect (fn [_] [[:note action]])})

(deftest a-book-without-recovery-prints-only-the-banner
  (let [{:keys [err]} (printed-outcome {:select {:mode :one} :confirm {:action "maintain"}
                                        :steps [(noted-change "cordon") (failing-change "drain")]})
        expected (java.io.StringWriter.)]
    (binding [*err* expected]
      (gumshoe.stdout/print-banner gumshoe.stdout/red
                                   (str (gumshoe.theme/token :error) " FAILED at the first effect: "
                                        missing-binary " - nothing was changed")))
    (is (= (str expected) err))))

(deftest a-book-without-recovery-names-what-earlier-steps-left-behind
  (let [ran (atom [])
        {:keys [err]} (printed-outcome {:select {:mode :one} :confirm {:action "maintain"}
                                        :steps [(change "cordon" ran) (failing-change "drain")]})]
    (is (str/includes? err (str "FAILED after 1 effect - stopped at: " missing-binary " - the effects before it stay applied")))
    (is (str/includes? err "What the steps before this one left behind"))
    (is (str/includes? err "1/2 cordon:"))))

(deftest recovery-that-cannot-render-keeps-the-banner-and-says-so
  (let [{:keys [err]} (printed-outcome {:select {:mode :one} :confirm {:action "maintain"}
                                        :steps [(noted-change "cordon")
                                                (failing-change "drain" :recovery (fn [_] (throw (ex-info "no cluster" {}))))]})]
    (is (str/includes? err "FAILED at the first effect: "))
    (is (str/includes? err "Recovery: the guidance for the step the run stopped at could not be rendered - no cluster"))))

(deftest a-failing-check-prints-its-own-recovery
  (let [ran (atom [])
        {:keys [err]} (printed-outcome {:select {:mode :one} :confirm {:action "maintain"}
                                        :steps [(change "cordon" ran)
                                                (assoc (check "drained?" ran false) :recovery "uncordon w-1 by hand")]})]
    (is (str/includes? err "FAILED - read the messages above; 1 effect had been applied"))
    (is (str/includes? err "uncordon w-1 by hand"))))

(deftest the-playbook-recovery-covers-steps-without-their-own-once-something-changed
  (let [ran (atom [])
        pb {:select {:mode :one} :confirm {:action "maintain"} :recovery "the node may still be cordoned"}]
    (testing "after a change, a step without guidance falls back to the playbook's"
      (is (str/includes? (:err (printed-outcome (assoc pb :steps [(change "cordon" ran) (failing-change "drain")])))
                         "the node may still be cordoned")))
    (testing "a step's own guidance comes first and the playbook's standing guidance follows it"
      (let [{:keys [err]} (printed-outcome (assoc pb :steps [(change "cordon" ran) (failing-change "drain" :recovery "mine")]))]
        (is (str/includes? err "mine"))
        (is (str/includes? err "the node may still be cordoned"))
        (is (< (str/index-of err "mine") (str/index-of err "the node may still be cordoned")))))
    (testing "a leading check that fails before any change prints no fallback"
      (is (not (str/includes? (:err (printed-outcome (assoc pb :steps [(check "look" ran false) (change "cordon" ran)])))
                              "Recovery"))))))

(deftest a-step-that-throws-records-its-recovery-and-rethrows
  (let [record (outcome/fresh)]
    (quietly
     (with-redefs [interact/confirm! (constantly true)]
       (binding [outcome/*record* record]
         (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exploded"
                               (playbook/run-on! {:select {:mode :one} :confirm {:action "x"}
                                                  :steps [{:check (fn [_] (throw (ex-info "exploded" {})))
                                                           :recovery "read the node by hand"}]}
                                                 "w-1" {} nil))))))
    (is (= {:text "read the node by hand"} (:recovery @record)))))

(defn- rehearsed-record
  "The outcome record a --dry-run of `pb` against w-1 leaves behind, with any
   exception the run throws swallowed."
  [pb]
  (let [record (outcome/fresh)]
    (quietly
     (with-redefs [interact/confirm! (constantly true)]
       (binding [outcome/*record* record flow/*dry-run* true]
         (try (playbook/run-on! pb "w-1" {} nil)
              (catch clojure.lang.ExceptionInfo _ nil)))))
    @record))

(deftest a-dry-run-records-no-recovery
  (let [pb {:select {:mode :one} :confirm {:action "x"} :recovery "the node may still be cordoned"}
        reports-applied {:title "counted" :check (fn [_] (outcome/applied!) true)}]
    (testing "a failing check that declares its own recovery"
      (let [record (rehearsed-record
                    (assoc pb :steps [{:title "look" :check (fn [_] false) :recovery "read the node by hand"}]))]
        (is (not (contains? record :recovery)))))
    (testing "a failing check after a step that reported an applied effect"
      (let [record (rehearsed-record
                    (assoc pb :steps [reports-applied
                                      {:title "look" :check (fn [_] false) :recovery "read the node by hand"}]))]
        (is (pos? (:applied record)) "the fixture reaches the standing playbook recovery")
        (is (not (contains? record :recovery)))))
    (testing "a step that throws and declares its own recovery"
      (let [record (rehearsed-record
                    (assoc pb :steps [{:title "look" :check (fn [_] (throw (ex-info "exploded" {})))
                                       :recovery "read the node by hand"}]))]
        (is (not (contains? record :recovery)))))))

(deftest a-step-whose-effect-is-not-a-plan-is-refused-before-it-runs
  (let [ran (atom [])
        broken {:select {:mode :one :label "Node"}
                :title "write the reset note"
                :confirm {:action "note the reset"}
                :effect (fn [_] (swap! ran conj [:built]) :note)}
        refusal (fn [dry-run?]
                  (quietly
                   (with-redefs [interact/confirm! (constantly true)]
                     (binding [flow/*dry-run* dry-run?]
                       (try (playbook/run-on! {:select {:mode :one} :confirm {:action "x"}
                                               :steps [(change "cordon" ran) broken]}
                                              "w-1" {} nil)
                            nil
                            (catch clojure.lang.ExceptionInfo e (ex-message e)))))))]
    (doseq [dry-run? [true false]]
      (testing (if dry-run? "under --dry-run" "in a real run")
        (reset! ran [])
        (let [message (refusal dry-run?)]
          (is (str/includes? (str message) "write the reset note") "the message names the step")
          (is (str/includes? (str message) "effect/plan"))
          (is (not (str/includes? (str message) "ISeq"))))))))

(deftest a-step-returning-one-effect-runs-it
  (let [ran (atom [])
        lone {:select {:mode :one :label "Node"}
              :confirm {:action "note the reset"}
              :effect (fn [_] (swap! ran conj [:lone]) (effect/note "reset done"))}]
    (doseq [dry-run? [true false]]
      (reset! ran [])
      (quietly
       (with-redefs [interact/confirm! (constantly true)]
         (binding [flow/*dry-run* dry-run?]
           (is (true? (playbook/run-on! {:select {:mode :one} :confirm {:action "x"}
                                         :steps [(change "cordon" ran) lone]}
                                        "w-1" {} nil))))))
      (is (= [["cordon" "w-1"] [:lone]] @ran)))))

;; --- positions ---------------------------------------------------------------------

(def ^:private anchored
  [[nil {:title "read" :check (constantly true)}]
   [:after-delete {:title "delete" :confirm {:destructive? true} :effect (fn [_] [])}]
   [:after-resume {:title "resume" :effect (fn [_] [])}]])

(deftest positions-are-the-anchors-in-run-order
  (is (= [:after-delete :after-resume] (playbook/positions anchored))))

(deftest extra-steps-go-right-after-the-step-their-position-names
  (let [mine {:title "mine" :check (constantly true)}]
    (is (= ["read" "delete" "mine" "resume"]
           (map :title (playbook/with-extra anchored {:after-delete [mine]}))))
    (is (= ["read" "delete" "resume" "mine"]
           (map :title (playbook/with-extra anchored {:after-resume [mine]}))))
    (is (= ["read" "delete" "resume"] (map :title (playbook/with-extra anchored nil))))))

(defn- refusal [extra options]
  (try (playbook/with-extra anchored extra options) nil
       (catch clojure.lang.ExceptionInfo e [(ex-message e) (ex-data e)])))

(deftest a-misplaced-extra-step-is-refused-as-the-steps-are-built
  (is (= [":before-read is not a position of the playbook's steps; the positions are [:after-delete :after-resume]"
          {:position :before-read :positions [:after-delete :after-resume]}]
         (refusal {:before-read []} {})))
  (is (re-find #"of the reset steps" (first (refusal {:before-read []} {:of "the reset steps"}))))
  (is (= "the extra steps must be a map of position -> steps" (first (refusal [:x] {}))))
  (is (= "the steps for :after-delete must be a vector of step maps"
         (first (refusal {:after-delete {:title "not a vector"}} {}))))
  (testing "a refusal the playbook names"
    (let [refuse (fn [position step]
                   (when (and (= :after-resume position) (playbook/destructive? step))
                     (str "no destructive step after the resume: " (:title step))))]
      (is (= ["no destructive step after the resume: wipe" {:position :after-resume :step "wipe"}]
             (refusal {:after-resume [{:title "keep"} {:title "wipe" :confirm {:destructive? true}}]}
                      {:refuse refuse})))
      (is (nil? (refusal {:after-delete [{:title "wipe" :confirm {:destructive? true}}]} {:refuse refuse}))))))

(defn- applying-then-failing
  "A change running `true` before the effect that fails."
  [action & {:as extra}]
  (merge (failing-change action) {:effect (fn [_] [[:cmd "true"] [:cmd missing-binary]])} extra))

(deftest the-engine-fallback-of-a-step-comes-before-the-playbook-recovery
  (let [pb {:select {:mode :one} :confirm {:action "maintain"} :recovery "the node may still be cordoned"}]
    (testing "a step without guidance that applied an effect names it, then the playbook's guidance follows"
      (let [{:keys [err]} (printed-outcome (assoc pb :steps [(applying-then-failing "drain")]))]
        (is (str/includes? err "the node may still be cordoned") err)
        (is (str/includes? err "This change declares no recovery guidance") err)
        (is (< (str/index-of err "This change declares no recovery guidance")
               (str/index-of err "the node may still be cordoned")))))
    (testing "without playbook guidance the engine says what the step left applied"
      (let [{:keys [err]} (printed-outcome (assoc pb :recovery nil :steps [(applying-then-failing "drain")]))]
        (is (str/includes? err "This change declares no recovery guidance") err)
        (is (str/includes? err "    true") "names the effect that stayed applied")))))

;; --- recovery composition -----------------------------------------------------------

(def ^:private cordoned {:text "w-1 stays cordoned - uncordon it"})

(deftest recovery-composes-from-the-state-of-the-run
  (doseq [[why state expected]
          [["nothing at all" {} nil]
           ["a run that changed nothing prints the step's guidance alone"
            {:step {:text "mine"} :changed? false :standing cordoned} {:text "mine"}]
           ["a run that changed nothing prints no standing guidance"
            {:changed? false :standing cordoned} nil]
           ["a run that changed something and whose step has none prints the standing guidance"
            {:changed? true :standing cordoned} cordoned]
           ["the step's guidance comes first, the standing guidance follows"
            {:step {:text "mine"} :changed? true :standing cordoned}
            {:text "mine\nw-1 stays cordoned - uncordon it"}]
           ["a standing text equal to the step's prints once"
            {:step cordoned :changed? true :standing cordoned} cordoned]
           ["a single part that could not be rendered keeps its form"
            {:step {:error "no cluster"} :changed? false} {:error "no cluster"}]
           ["a part that could not be rendered says so in its place"
            {:step {:error "no cluster"} :changed? true :standing cordoned}
            {:text "The guidance for the step the run stopped at could not be rendered - no cluster\nw-1 stays cordoned - uncordon it"}]
           ["standing guidance that could not be rendered says so after the step's"
            {:step {:text "mine"} :changed? true :standing {:error "boom"}}
            {:text "mine\nThe guidance for the playbook could not be rendered - boom"}]
           ["without standing guidance, the earlier steps' effects are named"
            {:step {:text "mine"} :changed? true
             :left-behind [{:title "1/3 cordon" :effects ["kubectl cordon w-1"]}]}
            {:text (str "mine\n"
                        "This playbook declares no recovery guidance. What the steps before this one left behind - nothing was rolled back:\n"
                        "  1/3 cordon:\n"
                        "    kubectl cordon w-1")}]
           ["without any guidance, the earlier steps' effects alone are named"
            {:changed? true
             :left-behind [{:title "1/3 cordon" :effects ["kubectl cordon w-1"]}
                           {:title "2/3 label" :effects ["kubectl label node w-1 a=b" "kubectl label node w-1 c=d"]}]}
            {:text (str "This playbook declares no recovery guidance. What the steps before this one left behind - nothing was rolled back:\n"
                        "  1/3 cordon:\n"
                        "    kubectl cordon w-1\n"
                        "  2/3 label:\n"
                        "    kubectl label node w-1 a=b\n"
                        "    kubectl label node w-1 c=d")}]
           ["a standing text equal to the step's prints once and lists no effects"
            {:step cordoned :changed? true :standing cordoned
             :left-behind [{:title "1/3 cordon" :effects ["kubectl cordon w-1"]}]}
            cordoned]
           ["standing guidance replaces the list of effects"
            {:changed? true :standing cordoned
             :left-behind [{:title "1/3 cordon" :effects ["kubectl cordon w-1"]}]}
            cordoned]
           ["a list of effects needs a run that changed something"
            {:changed? false :left-behind [{:title "1/3 cordon" :effects ["kubectl cordon w-1"]}]}
            nil]]]
    (testing why
      (is (= expected (playbook/compose-recovery state))))))

(deftest a-step-applied-the-first-effects-of-its-plan-without-its-notes
  (doseq [[why plan applied expected]
          [["none applied" [[:kubectl "ctx" "cordon" "w-1"]] 0 []]
           ["all applied" [[:kubectl "ctx" "cordon" "w-1"] [:note "done"]] 2 ["kubectl cordon w-1"]]
           ["a prefix" [[:kubectl "ctx" "cordon" "w-1"] [:kubectl "ctx" "drain" "w-1"]] 1 ["kubectl cordon w-1"]]
           ["one effect is a plan of one" [:kubectl "ctx" "cordon" "w-1"] 1 ["kubectl cordon w-1"]]
           ["no plan" nil 3 []]]]
    (testing why
      (is (= expected (playbook/applied-effects plan applied))))))

(defn- cmd-change
  "A change whose effect is a command, so it applies something a recovery
   must name."
  [action & {:as extra}]
  (merge {:select {:mode :one :label "Node"}
          :title action
          :confirm {:action action}
          :effect (fn [{:keys [target]}] [[:cmd "kubectl" action target]])}
         extra))

(deftest a-playbook-without-recovery-names-what-the-earlier-steps-applied
  (with-redefs [gumshoe.shell/run-with-output (fn [& argv] (if (some #{"drain"} argv) 1 0))
                gumshoe.shell/run-attributed (fn [& argv] (if (some #{"drain"} argv) 1 0))]
    (let [{:keys [ok? err]} (printed-outcome {:select {:mode :one} :confirm {:action "maintain"}
                                              :steps [(cmd-change "cordon") (cmd-change "drain")]})]
      (is (false? ok?))
      (is (str/includes? err "What the steps before this one left behind - nothing was rolled back:") err)
      (is (str/includes? err "1/2 cordon:") err)
      (is (str/includes? err "kubectl cordon w-1") err))))

(deftest a-step-added-at-a-position-keeps-the-playbooks-standing-guidance
  (let [ran (atom [])
        base [[nil (change "suspend" ran)] [:after-suspend (change "delete" ran)] [nil (change "resume" ran)]]
        extra {:after-suspend [(failing-change "delete the database" :recovery "the database may be half gone")]}
        {:keys [err]} (printed-outcome {:select {:mode :one} :confirm {:action "reset"}
                                        :recovery (fn [{:keys [target]}] (str target " stays suspended - resume it"))
                                        :steps (playbook/with-extra base extra)})]
    (is (str/includes? err "the database may be half gone") err)
    (is (str/includes? err "w-1 stays suspended - resume it") err)
    (is (< (str/index-of err "the database may be half gone") (str/index-of err "w-1 stays suspended - resume it")))))

(deftest a-pause-that-cannot-wait-records-no-recovery
  (let [record (outcome/fresh)]
    (quietly
     (with-redefs [interact/confirm! (constantly true)]
       (binding [outcome/*record* record]
         (playbook/run-on! {:select {:mode :one} :confirm {:action "x"}
                            :recovery "never"
                            :steps [(change "cordon" (atom [])) {:pause "act"}]}
                           "w-1" {} nil))))
    (is (some? (:paused @record)))
    (is (not (contains? @record :recovery)))))
