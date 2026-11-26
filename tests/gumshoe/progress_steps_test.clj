;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.progress-steps-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.outcome :as outcome]
            [gumshoe.progress :as progress]
            [gumshoe.render :as render]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

(def ^:private escape #"\u001b\[")

(defn- plain-err
  "What f prints on stderr with the list forced plain, colour on for every
   other printer, so an escape in the result could only come from the list
   letting one through."
  [f]
  (let [err (java.io.StringWriter.)]
    (with-redefs-fn {#'stdout/colors-enabled? (constantly true)
                     #'progress/live? (constantly false)}
      #(binding [*err* err] (f)))
    (str err)))

(defn- live-err
  "What f prints on stderr with the list forced live, stdout off the screen."
  [f]
  (let [err (java.io.StringWriter.)]
    (with-redefs-fn {#'progress/live? (constantly true)
                     #'progress/terminal? (fn [fd] (= 2 fd))}
      #(binding [*err* err] (f)))
    (str err)))

(defn- screen
  "The lines a terminal shows after it was sent s, reading the only cursor
   movement a list writes: up n lines, carriage return, clear to the end of
   the screen, and newline."
  [s]
  (let [tokens (re-seq #"\u001b\[(\d+)A|\u001b\[J|\r|\n|[^\u001b\r\n]+" s)]
    (loop [[[token up] & more] tokens lines [] row 0]
      (cond
        (nil? token) (subvec lines 0 (min row (count lines)))
        up (recur more lines (max 0 (- row (parse-long up))))
        (= "\u001b[J" token) (recur more (subvec lines 0 (min row (count lines))) row)
        (= "\r" token) (recur more lines row)
        (= "\n" token) (recur more (if (< row (count lines)) lines (conj lines "")) (inc row))
        :else (recur more
                     (if (< row (count lines)) (update lines row str token) (conj lines token))
                     row)))))

(defn- after-last-clear
  "What a live list left on the screen at the end, as one string."
  [s]
  (str (str/join "\n" (screen s)) "\n"))

(deftest step-result-names-how-a-step-ended
  (testing "true and other truthy values are done, carrying the value"
    (is (= {:status :done :ok? true :value true} (progress/step-result true nil)))
    (is (= {:status :done :ok? true :value [1 2]} (progress/step-result [1 2] nil))))
  (testing "false and nil fail"
    (is (= :failed (:status (progress/step-result false nil))))
    (is (= :failed (:status (progress/step-result nil nil))))
    (is (false? (:ok? (progress/step-result nil nil)))))
  (testing "a map says how it ended, and one without :ok? fails with the reason"
    (is (= {:status :done :ok? true :value 3 :label "kubectl (v1.36)"}
           (progress/step-result {:ok? true :value 3 :label "kubectl (v1.36)"} nil)))
    (is (= {:status :failed :ok? false :value nil :reason "the step returned a map without :ok?"}
           (progress/step-result {:pods 3} nil)))
    (is (= :skipped (:status (progress/step-result {:skipped? true} nil))))
    (is (true? (:ok? (progress/step-result {:skipped? true} nil)))))
  (testing "a step that could not read its answer is unread, never ok, whatever :ok? says"
    (let [r (progress/step-result {:ok? true :unread? true} nil)]
      (is (= :unread (:status r)))
      (is (false? (:ok? r)))
      (is (= "the step could not read its answer" (:reason r)))))
  (testing "a throw fails with its message"
    (is (= {:status :failed :ok? false :reason "threw: boom"}
           (progress/step-result nil (ex-info "boom" {}))))
    (is (= "threw: java.lang.AssertionError"
           (:reason (progress/step-result nil (AssertionError.))))))
  (testing "a read that did not answer is unread, with what it said"
    (is (= {:status :unread :ok? false :reason "could not read: the API server did not answer"}
           (progress/step-result nil (ex-info "the API server did not answer" {:gumshoe.kubectl/unread true}))))))

(def ^:private four-outcomes
  [{:label "ok" :run (constantly true)}
   {:label "fails" :run (constantly false)}
   {:label "throws" :run #(throw (ex-info "boom" {}))}
   {:label "unread" :run #(throw (ex-info "no answer" {:gumshoe.kubectl/unread true}))}])

(deftest run-steps-returns-one-result-per-step
  (let [results (atom nil)]
    (plain-err #(reset! results (progress/run-steps four-outcomes {:stop-on-failure? false})))
    (is (= [:done :failed :failed :unread] (mapv :status @results)))
    (is (= ["ok" "fails" "throws" "unread"] (mapv :label @results)))
    (is (= [true false false false] (mapv :ok? @results)))))

(deftest plain-rows-draw-each-outcome-once-with-no-escapes
  (let [err (plain-err #(progress/run-steps four-outcomes {:stop-on-failure? false}))
        theme (render/context :default false)]
    (is (= [(render/task theme :done "ok")
            (render/task theme :failed "fails")
            (render/task theme :failed "throws")
            "    threw: boom"
            (render/task theme :unread "unread")
            "    could not read: no answer"]
           (str/split-lines err)))
    (is (not (re-find escape err)) "no colour and no cursor movement")
    (is (not (str/includes? err (render/task theme :done "throws"))) "a throw is never drawn done")
    (is (not (str/includes? err (render/task theme :done "unread"))) "an unread step is never drawn done")))

(deftest the-escape-check-sees-escapes-when-there-are-some
  ;; positive control for the plain-mode check above: the same detector finds
  ;; the cursor movement and colour a live list writes
  (let [err (with-redefs-fn {#'stdout/colors-enabled? (constantly true)}
              #(live-err (fn [] (progress/run-steps four-outcomes {:stop-on-failure? false}))))]
    (is (re-find escape err))
    (is (re-find #"\u001b\[\d+A" err) "a repaint moves the cursor up")))

(deftest a-step-that-prints-keeps-its-text-under-its-row
  (let [steps [{:label "reads pods" :run #(do (stdout/ok "3 pods") (stdout/note "all ready") true)}
               {:label "reads nodes" :run #(do (binding [*out* *err*] (println "2 nodes")) true)}]]
    (testing "plain: the row, then what the step printed, uncoloured"
      (let [err (plain-err #(progress/run-steps steps))]
        (is (= ["  ✅ reads pods" "    ✅ 3 pods" "    all ready" "  ✅ reads nodes" "    2 nodes"]
               (str/split-lines err)))))
    (testing "live: the text is never repainted away, it stays under its row"
      (let [err (live-err #(progress/run-steps steps))
            final (after-last-clear err)]
        (is (str/includes? final "reads pods\n    ✅ 3 pods\n    all ready\n"))
        (is (str/includes? final "reads nodes\n    2 nodes\n"))
        (is (= ["  ✅ reads pods" "    ✅ 3 pods" "    all ready" "  ✅ reads nodes" "    2 nodes"]
               (mapv render/strip-colors (screen err)))
            "the screen ends with every row and its text once")))))

(deftest a-step-printing-to-a-captured-stdout-keeps-it-on-stdout
  (let [out (java.io.StringWriter.)
        err (live-err #(binding [*out* out]
                         (progress/run-steps [{:label "emits" :run (fn [] (println "{\"pods\":3}") true)}])))]
    (is (= "{\"pods\":3}\n" (str out)) "a result piped away is not pulled into the list")
    (is (not (str/includes? err "pods\":3")))))

(deftest stopping-leaves-the-rest-not-yet
  (let [ran (atom [])
        results (atom nil)
        err (plain-err #(reset! results (progress/run-steps [{:label "a" :run (fn [] (swap! ran conj :a) false)}
                                                             {:label "b" :run (fn [] (swap! ran conj :b) true)}])))]
    (is (= [:a] @ran))
    (is (= [:failed :pending] (mapv :status @results)))
    (is (false? (:ok? (second @results))))
    (is (str/includes? err (render/task (render/context :default false) :pending "b"))
        "the step never run is printed as not yet, never left out or drawn done")))

(deftest a-live-status-line-shows-while-running-and-goes-when-settled
  (let [err (live-err #(progress/run-steps [{:label "phase" :run (fn [] (progress/status! "12 of 48 reads") (Thread/sleep 250) true)}]))]
    (is (str/includes? err "phase\n    12 of 48 reads\n") "drawn under the running row")
    (is (not (str/includes? (after-last-clear err) "12 of 48")) "gone once the row settled")))

(deftest a-plain-status-line-prints-at-once
  (let [err (plain-err #(progress/run-steps [{:label "phase" :run (fn [] (progress/status! "12 of 48 reads") true)}]))]
    (is (= ["  ⏳ 12 of 48 reads" "  ✅ phase"] (str/split-lines err))
        "the wait line comes before the row, as a log reads it")
    (is (not (re-find escape err)))))

(deftest status-outside-a-list-prints-an-item
  (let [err (java.io.StringWriter.)]
    (binding [*err* err] (progress/status! "waiting"))
    (is (= (str (render/item (stdout/context) :in-progress "waiting") "\n") (str err)))))

(deftest a-board-adds-rows-as-it-goes
  (testing "rows settled out of order print in order, a row never started as not yet"
    (let [err (plain-err #(progress/with-board [b]
                            (let [one (progress/add! b "first")
                                  two (progress/add! b "second")
                                  _ (progress/add! b "never started")]
                              (progress/start! b two)
                              (progress/settle! b two {:ok? true :label "second (3 of 3)"})
                              (progress/run! b one (constantly true))
                              (progress/run! b "added late" (constantly false)))))]
      (is (= ["  ✅ first" "  ✅ second (3 of 3)" "  ⬜ never started" "  ❌ added late"]
             (str/split-lines err)))))
  (testing "with-board returns what its body returned, and closes on a throw"
    (is (= :value (progress/with-board [_ {:live? false}] :value)))
    (let [err (java.io.StringWriter.)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (binding [*err* err]
                     (progress/with-board [b {:live? false}]
                       (progress/add! b "pending")
                       (throw (ex-info "out" {}))))))
      (is (str/includes? (str err) "pending") "the open row is printed on the way out"))))

(deftest a-list-inside-a-step-prints-plain-into-it
  (let [err (live-err #(progress/run-steps [{:label "outer" :run (fn [] (progress/checklist [["inner" (constantly true)]]))}]))
        final (after-last-clear err)]
    (is (str/includes? final "outer\n      ✅ inner\n") "the inner list is the outer step's text")))

(deftest parallel-tasks-inside-a-step-print-plain-into-it
  (let [err (live-err #(progress/run-steps [{:label "outer" :run (fn [] (progress/watch-futures! [["pods" (future 1)]]) true)}]))]
    (is (str/includes? (after-last-clear err) "outer\n      ▸ pods\n"))))

(deftest a-spinner-inside-a-step-prints-its-message-once
  (let [err (live-err #(progress/run-steps [{:label "outer" :run (fn [] (stdout/with-spinner "reading" (fn [] (Thread/sleep 200) true)))}]))]
    (is (not (re-find #"\rreading|\r\S+ reading" err)) "no carriage-return frames inside the list")
    (is (str/includes? (after-last-clear err) "outer\n    reading\n"))))

(deftest live-lines-are-cut-to-the-width
  (let [err (binding [stdout/*width* 20]
              (live-err #(progress/run-steps [{:label "a very long label that would wrap the terminal"
                                               :run (fn [] (Thread/sleep 150) true)}])))
        regions (->> (str/split err #"\u001b\[J") (filter #(str/includes? % "…")))]
    (is (seq regions) "the running row was drawn cut")
    (is (every? #(<= (render/visible-width %) 20) (mapcat str/split-lines regions)))))

;; --- post-checks ---------------------------------------------------------------------

(defn- checks
  []
  [{:description "the pods run" :check (constantly true) :timeout 0}
   {:description "the claim is bound" :check (constantly false) :timeout 0}])

(deftest plain-post-checks-render-one-row-per-check
  (let [held (atom nil)
        err (plain-err #(binding [outcome/*record* (outcome/fresh)] (reset! held (verify/all (checks)))))]
    (is (false? @held))
    (is (= ["  ✅ verified: the pods run"
            "  ❌ the claim is bound"
            "    NOT verified within 0s: the claim is bound"]
           (str/split-lines err)))
    (is (not (re-find escape err)))
    (is (= 1 (count (re-seq #"verified: the pods run" err))) "the success is drawn once, by the row")))

(deftest live-post-checks-fill-in-with-their-wait-line
  (let [tries (atom 0)
        held (atom nil)
        err (live-err #(reset! held (verify/all [{:description "the pods run" :interval 1 :timeout 5
                                                 :check (fn [] (> (swap! tries inc) 1))}])))]
    (is (true? @held))
    (is (re-find #"the pods run\n    waiting for: the pods run \(0/5s\)\n" err)
        "the polling wait line is the running row's status")
    (is (str/includes? (after-last-clear err) "✅ verified: the pods run\n"))
    (is (not (str/includes? (after-last-clear err) "waiting for")) "the wait line is gone from the screen")))

(deftest a-post-check-that-throws-is-a-failed-row
  (let [held (atom nil)
        err (plain-err #(with-redefs [verify/eventually (fn [_] (throw (AssertionError. "broken check")))]
                          (reset! held (verify/all [{:description "the pods run" :check (constantly true)}]))))]
    (is (false? @held))
    (is (= ["  ❌ the pods run" "    threw: broken check"] (str/split-lines err)))))

(deftest a-soft-post-check-that-times-out-is-a-caveat
  (let [held (atom nil)
        err (plain-err #(reset! held (verify/all [(verify/unobservable "the cache warmed" "nothing records it")])))]
    (is (true? @held) "best effort still holds")
    (is (str/starts-with? err "  🔶 the cache warmed\n") "drawn as a finding, never as done or verified")
    (is (not (str/includes? err "verified: the cache warmed")))))

(deftest a-malformed-post-check-is-a-failed-row
  (let [err (plain-err #(verify/all [{:check (constantly true)}]))]
    (is (str/starts-with? err "  ❌ a malformed post-check\n"))))
