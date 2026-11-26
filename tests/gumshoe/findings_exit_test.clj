;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.findings-exit-test
  "Whole books, run in a fresh bb, end under a banner and an exit code that
   tell a run that did its job and reported critical findings from one that
   could not do its job, under the default and the detailed exit-code
   contract."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.runbook :as runbook]))

(defn- absolute-classpath
  []
  (->> (str/split (System/getProperty "java.class.path") #":")
       (map #(str (fs/absolutize %)))
       (str/join ":")))

(defn- run-script
  "Runs `book` (the source of a form calling execute! or detective/book) in a
   fresh bb from a scratch directory with no env.edn, with the arguments and
   the extra environment given."
  [book args env]
  (fs/with-temp-dir [dir {}]
    (let [script (str "(require '[gumshoe.runbook :as runbook] '[gumshoe.detective :as detective]"
                      "         '[gumshoe.outcome :as outcome] '[gumshoe.stdout :as stdout])"
                      "(binding [*command-line-args* " (pr-str (vec args)) "] " book ")")]
      @(process/process {:dir (str dir) :out :string :err :string :continue true :in ""
                         :extra-env (merge {"GUMSHOE_ENV" (str (fs/path dir "env.edn"))
                                            "GUMSHOE_EXIT_CODES" ""
                                            "NO_COLOR" "1"
                                            "KUBECONFIG" "/dev/null"}
                                           env)}
                        "bb" "-cp" (absolute-classpath) "-e" script))))

(defn- scan
  "A detective book over collected evidence: one check that reads `pods` and
   reports a critical finding for each, and, with crash?, one that throws."
  [evidence crash?]
  (str "(detective/book {:description \"scans pods\" :prerequisites {}"
       " :collect (fn [_] " (pr-str evidence) ")"
       " :detectives [{:name \"pods-ready\" :description \"every pod is ready\" :requires [\"pods\"]"
       "               :detect (fn [ev] (for [p (get ev \"pods\")] {:severity :critical :component p :summary \"not ready\"}))}"
       (when crash?
         "              {:name \"pods-broken\" :description \"throws\" :requires [\"pods\"] :detect (fn [_] (throw (ex-info \"boom\" {})))}")
       "]})"))

(def ^:private audit
  "A plain read-only book in the shape of an adoption audit: it prints its own
   report, records its critical findings and returns false."
  (str "(runbook/execute! {:description \"audits\" :announce? false"
       " :action (fn [_ _] (stdout/warn \"3 object(s) belong to nobody\")"
       "                   (outcome/findings! {:critical 3}) false)})"))

(def ^:private unread-audit
  "The same book when its read did not answer: it says so and returns false,
   recording no findings."
  (str "(runbook/execute! {:description \"audits\" :announce? false"
       " :action (fn [_ _] (stdout/error \"could not read the namespace\") false)})"))

(def ^:private refused
  "A book whose prerequisite refuses: the directory --checkout names is absent."
  (str "(runbook/execute! {:description \"needs a checkout\" :announce? false"
       " :options {:checkout {:desc \"the checkout\"}}"
       " :prerequisites {:existing-paths [:checkout]}"
       " :action (fn [_ _] (println \"ACTION RAN\") true)})"))

(def ^:private change-with-findings
  "A change book whose action records critical findings and fails: a change
   book never ends as findings, since its check refused the change."
  (str "(runbook/execute! {:description \"changes\" :announce? false :change? true"
       " :action (fn [_ _] (outcome/findings! {:critical 1}) false)})"))

(def ^:private modes
  "Each contract with the way it is chosen and the code a findings-only run
   exits with under it."
  [{:label "default" :args [] :env {} :findings-exit 1}
   {:label "--exit-codes detailed" :args ["--exit-codes" "detailed"] :env {} :findings-exit 2}
   {:label "GUMSHOE_EXIT_CODES=detailed" :args [] :env {"GUMSHOE_EXIT_CODES" "detailed"} :findings-exit 2}])

(defn- banner-line
  [err]
  (some #(when (re-find #"(DONE|FAILED|FINDINGS|PREREQUISITES NOT MET) " %) %) (str/split-lines err)))

(deftest a-scan-that-reported-critical-findings-ends-as-findings
  (doseq [{:keys [label args env findings-exit]} modes]
    (testing label
      (let [{:keys [exit out err]} (run-script (scan {"pods" ["web-0" "web-1"]} false) args env)]
        (is (= findings-exit exit) (str out err))
        (is (str/includes? (str (banner-line err)) "FINDINGS - the book ran and reported 2 critical findings, see above")
            err)))))

(deftest a-scan-that-read-nothing-fails-in-every-contract
  (doseq [{:keys [label args env]} modes]
    (testing label
      (let [{:keys [exit out err]} (run-script (scan {"pods" nil} false) args env)]
        (is (= 1 exit) (str out err))
        (is (str/includes? (str (banner-line err)) "FAILED - nothing was checked") err)))))

(deftest a-crashed-check-beside-critical-findings-fails-in-every-contract
  (doseq [{:keys [label args env]} (rest modes)]
    (testing label
      (let [{:keys [exit out err]} (run-script (scan {"pods" ["web-0"]} true) args env)]
        (is (= 1 exit) (str out err))
        (is (str/includes? (str (banner-line err)) "FAILED - 1 check could not run") err)
        (is (not (str/includes? err "FINDINGS")) err)))))

(deftest a-plain-book-that-recorded-findings-ends-as-findings
  (doseq [{:keys [label args env findings-exit]} modes]
    (testing label
      (let [{:keys [exit out err]} (run-script audit args env)]
        (is (= findings-exit exit) (str out err))
        (is (str/includes? (str (banner-line err)) "FINDINGS - the book ran and reported 3 critical findings") err))
      (let [{:keys [exit out err]} (run-script unread-audit args env)]
        (is (= 1 exit) (str out err))
        (is (str/includes? (str (banner-line err)) "FAILED - read the messages above") err)))))

(deftest a-refused-prerequisite-fails-in-every-contract
  (doseq [{:keys [label args env]} (rest modes)]
    (testing label
      (let [{:keys [exit out err]} (run-script refused (into ["--checkout" "/nonexistent/checkout"] args) env)]
        (is (= 1 exit) (str out err))
        (is (str/includes? (str out err) "PREREQUISITES NOT MET") err)
        (is (not (str/includes? out "ACTION RAN")))))))

(deftest a-change-book-never-ends-as-findings
  (let [{:keys [exit out err]} (run-script change-with-findings ["--exit-codes" "detailed"] {})]
    (is (= 1 exit) (str out err))
    (is (str/includes? (str (banner-line err)) "FAILED") err)))

(deftest the-flag-wins-over-the-environment-and-a-bad-value-falls-back-to-default
  (testing "--exit-codes default over GUMSHOE_EXIT_CODES=detailed"
    (let [{:keys [exit]} (run-script audit ["--exit-codes" "default"] {"GUMSHOE_EXIT_CODES" "detailed"})]
      (is (= 1 exit))))
  (testing "an env value naming no contract warns and keeps default"
    (let [{:keys [exit err]} (run-script audit [] {"GUMSHOE_EXIT_CODES" "verbose"})]
      (is (= 1 exit))
      (is (str/includes? err "GUMSHOE_EXIT_CODES=\"verbose\" names no exit-code contract") err)))
  (testing "a flag value naming no contract is refused before the action"
    (let [{:keys [exit err]} (run-script audit ["--exit-codes" "loud"] {})]
      (is (= 1 exit))
      (is (str/includes? err "invalid exit-code contract \"loud\"") err)
      (is (not (str/includes? err "FINDINGS")) err))))

(deftest exit-mode-test
  (is (= {:mode :default} (runbook/exit-mode nil nil)))
  (is (= {:mode :default} (runbook/exit-mode nil "  ")))
  (is (= {:mode :detailed} (runbook/exit-mode nil " Detailed ")))
  (is (= {:mode :default} (runbook/exit-mode "default" "detailed")))
  (is (= {:mode :detailed} (runbook/exit-mode "detailed" nil)))
  (is (= :default (:mode (runbook/exit-mode nil "2"))))
  (is (string? (:note (runbook/exit-mode nil "2")))))
