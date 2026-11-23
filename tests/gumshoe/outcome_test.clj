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
