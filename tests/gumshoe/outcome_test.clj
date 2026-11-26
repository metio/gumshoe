;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.outcome-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.outcome :as outcome]
            [gumshoe.verify :as verify]))

(defn- banner
  ([run] (banner run {}))
  ([run record] (outcome/banner run (merge {:applied 0} record))))

(defn- recorded
  "The record a body leaves behind when it runs inside a run."
  [f]
  (let [record (outcome/fresh)]
    (binding [outcome/*record* record] (f))
    @record))

(deftest a-dry-run-is-a-rehearsal-whatever-the-book-returned
  (is (= {:tone :neutral :text "DRY RUN - nothing was changed, this was a rehearsal"}
         (banner {:dry-run? true :changes? true :outcome :ok})))
  (testing "a read-only book still reports what it read"
    (is (= "DONE - no findings" (:text (banner {:dry-run? true :changes? false :outcome :ok}))))))

(deftest a-rehearsal-names-the-steps-whose-precondition-does-not-hold
  (is (= {:tone :error
          :text "DRY RUN - nothing was changed; the precondition of purge the old deployment does not hold against the current state"}
         (banner {:dry-run? true :changes? true :outcome :ok} {:unmet ["purge the old deployment"]})))
  (is (= 1 (outcome/exit-code :ok {:unmet ["purge"]})))
  (is (= 0 (outcome/exit-code :ok {:applied 0})))
  (is (= 1 (outcome/exit-code :failed {}))))

(deftest a-rehearsal-that-passed-a-step-it-could-not-show-is-never-done
  (doseq [outcome [:ok :failed]
          changes? [true false]]
    (testing (str outcome " " (if changes? "change book" "book that does not announce"))
      (is (= {:tone :error
              :text "DRY RUN - nothing was changed; not rehearsed: start the upgrade - a real run would do there what this rehearsal could not show"}
             (banner {:dry-run? true :changes? changes? :outcome outcome} {:unrehearsed ["start the upgrade"]})))))
  (is (= "DRY RUN - nothing was changed; the precondition of purge does not hold against the current state; not rehearsed: ratchet, start - a real run would do there what this rehearsal could not show"
         (:text (banner {:dry-run? true :changes? true :outcome :ok} {:unmet ["purge"] :unrehearsed ["ratchet" "start"]}))))
  (is (= 1 (outcome/exit-code :ok {:unrehearsed ["start the upgrade"]})))
  (is (= ["a" "b"] (:unrehearsed (recorded #(do (outcome/unrehearsed! "a") (outcome/unrehearsed! "b")))))))

(deftest a-read-only-book-reports-findings-without-claiming-a-failure
  (is (= {:tone :ok :text "DONE - no findings"}
         (banner {:changes? false :outcome :ok})))
  (is (= {:tone :neutral :text "FINDINGS - read the report above"}
         (banner {:changes? false :outcome :failed :findings? true})))
  (testing "a read-only book that failed without findings still fails"
    (is (= :error (:tone (banner {:changes? false :outcome :failed}))))))

(deftest only-a-change-whose-post-checks-held-claims-verification
  (is (= {:tone :ok :text "DONE - applied and verified"}
         (banner {:changes? true :outcome :ok} {:applied 2 :verified true})))
  (is (= {:tone :ok :text "DONE - applied; this book has no post-checks"}
         (banner {:changes? true :outcome :ok} {:applied 2})))
  (is (not-any? #(str/includes? (:text %) "verified")
                [(banner {:changes? true :outcome :ok} {:applied 1})
                 (banner {:changes? true :outcome :ok} {:thunk :ok})
                 (banner {:changes? true :outcome :ok} {})])))

(deftest a-change-that-stops-says-how-much-stayed-applied
  (is (= {:tone :error
          :text "FAILED after 4 effects - stopped at: kubectl delete pvc data-4 - the effects before it stay applied"}
         (banner {:changes? true :outcome :failed} {:applied 4 :stopped "kubectl delete pvc data-4"})))
  (is (= {:tone :error :text "FAILED at the first effect: kubectl cordon w-1 - nothing was changed"}
         (banner {:changes? true :outcome :failed} {:stopped "kubectl cordon w-1"})))
  (is (str/starts-with? (:text (banner {:changes? true :outcome :failed} {:applied 1 :verified false}))
                        "NOT VERIFIED")))

(deftest an-abort-at-the-confirmation-changed-nothing
  (is (= {:tone :neutral :text "ABORTED - declined at the confirmation - nothing was changed"}
         (banner {:changes? true :outcome :failed} {:aborted "declined at the confirmation"}))))

(deftest an-unexpected-error-names-what-was-applied-before-it
  (is (= {:tone :error
          :text "FAILED - an unexpected error stopped the run; 3 effects had been applied before it - read the messages above"}
         (banner {:changes? true :outcome :error} {:applied 3}))))

(deftest the-effect-interpreter-records-what-it-applied-and-where-it-stopped
  (with-redefs [gumshoe.shell/run-with-output (fn [& args] (if (some #{"fail"} args) 1 0))]
    (is (= {:applied 2 :stopped "echo fail"}
           (recorded #(effect/run! [[:cmd "echo" "a"] [:cmd "echo" "b"] [:cmd "echo" "fail"] [:cmd "echo" "c"]]))))))

(deftest every-post-check-must-hold-for-the-run-to-count-as-verified
  (with-redefs [verify/eventually (fn [check] (:held check))]
    (is (true? (:verified (recorded #(verify/all [{:held true}])))))
    (is (false? (:verified (recorded #(do (verify/all [{:held false}])
                                          (verify/all [{:held true}]))))))))

(deftest recording-outside-a-run-does-nothing
  (is (nil? outcome/*record*))
  (is (nil? (outcome/applied!))))

(deftest a-playbook-that-stops-at-a-pause-says-how-to-continue
  (is (= {:tone :neutral
          :text "PAUSED after 2 effects - continue with --segment 2 once the maintenance is done"}
         (banner {:changes? true :outcome :failed}
                 {:applied 2 :paused "continue with --segment 2 once the maintenance is done"})))
  (is (= 1 (outcome/exit-code :failed {:paused "x"}))))

(deftest an-abort-after-applied-effects-says-they-stay-applied
  (let [{:keys [tone text]} (outcome/banner {:changes? true :outcome :failed}
                                            {:applied 2 :aborted "declined at the confirmation"})]
    (is (= :error tone))
    (is (= "ABORTED after 2 effects - declined at the confirmation - the effects before it stay applied" text)))
  (is (= "ABORTED - declined - nothing was changed"
         (:text (outcome/banner {:changes? true :outcome :failed} {:applied 0 :aborted "declined"})))))

(deftest a-rehearsal-stopped-by-an-error-says-so
  (is (= {:tone :error
          :text "DRY RUN stopped by an unexpected error - nothing was changed - read the messages above"}
         (outcome/banner {:dry-run? true :changes? true :outcome :error} {:applied 0}))))

(deftest a-scan-with-warnings-does-not-claim-no-findings
  (is (= {:tone :ok :text "DONE - nothing critical; read the findings above"}
         (outcome/banner {:changes? false :outcome :ok :findings? true} {:applied 0})))
  (is (= "DONE - no findings" (:text (outcome/banner {:changes? false :outcome :ok} {:applied 0})))))

(deftest recovery-guidance-is-recorded-once-per-run
  (testing "a string is recorded as written"
    (is (= {:text "resume the owner"}
           (:recovery (recorded #(outcome/recovery! "resume the owner" {:target "w-1"}))))))
  (testing "a function renders against the step's context and the effects applied so far"
    (is (= {:text "w-1 after 2"}
           (:recovery (recorded #(do (outcome/applied!) (outcome/applied!)
                                     (outcome/recovery! (fn [{:keys [target applied]}] (str target " after " applied))
                                                        {:target "w-1"})))))))
  (testing "the guidance of the step the run stopped at wins over a later one"
    (is (= {:text "first"}
           (:recovery (recorded #(do (outcome/recovery! "first" {}) (outcome/recovery! "second" {})))))))
  (testing "a function that throws records why, so the failure it explains stays readable"
    (is (= {:error "boom"}
           (:recovery (recorded #(outcome/recovery! (fn [_] (throw (ex-info "boom" {}))) {}))))))
  (testing "no guidance, blank guidance and nil from a function record nothing"
    (is (not (contains? (recorded #(outcome/recovery! nil {})) :recovery)))
    (is (not (contains? (recorded #(outcome/recovery! "  " {})) :recovery)))
    (is (not (contains? (recorded #(outcome/recovery! (constantly nil) {})) :recovery))))
  (testing "outside a run nothing is rendered"
    (let [called (atom false)]
      (outcome/recovery! (fn [_] (reset! called true) "x") {})
      (is (false? @called)))))

(deftest recovery-lines-follow-only-a-failed-banner
  (is (= ["Recovery:" "  resume the owner" "  then the HelmRelease"]
         (outcome/recovery-lines :error {:recovery {:text "resume the owner\nthen the HelmRelease"}})))
  (is (= ["Recovery: the guidance for the step the run stopped at could not be rendered - boom"]
         (outcome/recovery-lines :error {:recovery {:error "boom"}})))
  (is (nil? (outcome/recovery-lines :error {:applied 1 :stopped "x"})) "no guidance, no lines")
  (is (nil? (outcome/recovery-lines :neutral {:recovery {:text "x"}})) "a pause or a clean abort prints none")
  (is (nil? (outcome/recovery-lines :ok {:recovery {:text "x"}}))))

(deftest a-fallback-prints-only-when-no-guidance-was-recorded
  (let [recorded (fn [f] (let [record (outcome/fresh)]
                           (binding [outcome/*record* record] (f))
                           @record))]
    (is (= ["Recovery:" "  generic"]
           (outcome/recovery-lines :error (recorded #(outcome/fallback! "generic" {})))))
    (is (= ["Recovery:" "  specific"]
           (outcome/recovery-lines :error (recorded #(do (outcome/fallback! "generic" {})
                                                         (outcome/recovery! "specific" {})))))
        "guidance recorded after the fallback still wins")
    (is (nil? (outcome/recovery-lines :neutral (recorded #(outcome/fallback! "generic" {})))))
    (is (nil? (outcome/fallback! "generic" {})) "outside a run nothing is recorded")))
