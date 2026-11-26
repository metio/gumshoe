;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.progress-live-line-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.progress :as progress]
            [gumshoe.progress-transcript-support :as transcript]
            [gumshoe.render :as render]
            [gumshoe.stdout :as stdout]))

(def ^:private plain (render/context :default false))

(def ^:private colour (render/context :default true))

(defn- row
  ([label status] {:label label :status status})
  ([label status extra] (merge {:label label :status status} extra)))

(defn- state
  ([rows] (state rows 0))
  ([rows committed] {:rows (vec rows) :committed committed :loose [] :frame 0}))

;; --- board-frame: what is committed ----------------------------------------------

(deftest settled-rows-from-the-top-are-committed-in-order
  (let [f (progress/board-frame plain (state [(row "a" :done) (row "b" :failed) (row "c" :running)])
                                {:live? true :width 80})]
    (is (= [(render/task plain :done "a") (render/task plain :failed "b")] (:commit f)))
    (is (= 2 (:upto f)))))

(deftest a-row-settled-behind-a-running-one-waits-for-it
  (testing "c settled first, but b still runs: nothing past b is committed"
    (let [s (state [(row "a" :done) (row "b" :running) (row "c" :done)] 1)
          f (progress/board-frame plain s {:live? true :width 80})]
      (is (= [] (:commit f)))
      (is (= 1 (:upto f)))))
  (testing "once b settles, b and c are committed together, in the order they were added"
    (let [s (state [(row "a" :done) (row "b" :warned) (row "c" :done)] 1)
          f (progress/board-frame plain s {:live? true :width 80})]
      (is (= [(render/task plain :warned "b") (render/task plain :done "c")] (:commit f)))
      (is (= 3 (:upto f))))))

(deftest every-terminal-state-commits-and-the-others-do-not
  (doseq [status [:done :failed :unread :warned :skipped]]
    (is (= 1 (:upto (progress/board-frame plain (state [(row "x" status)]) {:live? true}))) (str status)))
  (doseq [status [:pending :running]]
    (is (= 0 (:upto (progress/board-frame plain (state [(row "x" status)]) {:live? true}))) (str status))))

(deftest a-steps-text-is-committed-under-its-row
  (let [s (state [(row "reads pods" :done {:output "3 pods\nall ready\n"})
                  (row "reads nodes" :failed {:reason "threw: boom" :output "2 nodes"})])
        f (progress/board-frame plain s {:live? true :width 80})]
    (is (= [(render/task plain :done "reads pods") "    3 pods" "    all ready"
            (render/task plain :failed "reads nodes") "    threw: boom" "    2 nodes"]
           (:commit f)))))

(deftest committed-text-is-never-cut-to-the-width
  (let [long-text (apply str (repeat 50 "word "))
        f (progress/board-frame plain (state [(row "x" :done {:output long-text})]) {:live? true :width 20})]
    (is (some #(str/includes? % (str/trim long-text)) (:commit f))
        "scrollback is never repainted, so it keeps every column")))

(deftest a-nested-list-lands-in-the-outer-rows-text
  (let [inner (str (render/item plain :done "inner one") "\n" (render/item plain :failure "inner two") "\n")
        f (progress/board-frame plain (state [(row "outer" :failed {:output inner})]) {:live? true :width 80})]
    (is (= [(render/task plain :failed "outer") "      ✅ inner one" "      ❌ inner two"] (:commit f)))))

(deftest closing-commits-every-row-and-draws-no-live-line
  (let [f (progress/board-frame plain (state [(row "a" :done) (row "b" :running) (row "c" :pending)] 1)
                                {:live? true :closing? true :width 80})]
    (is (= [(render/task plain :running "b") (render/task plain :pending "c")] (:commit f)))
    (is (= 3 (:upto f)))
    (is (nil? (:live f)))))

(deftest plain-frames-draw-no-live-line-and-no-colour
  (let [f (progress/board-frame colour (state [(row "a" :done {:output "\u001b[31mred\u001b[0m"}) (row "b" :running)])
                                {:live? false})]
    (is (nil? (:live f)))
    (is (= ["  ✅ a" "    red"] (:commit f)))))

(deftest live-frames-keep-colour-in-committed-rows
  (let [f (progress/board-frame colour (state [(row "a" :done)]) {:live? true :width 80})]
    (is (= [(render/task colour :done "a")] (:commit f)))))

(deftest text-outside-a-step-is-printed-in-whole-lines
  (testing "an unfinished line waits for its newline, so the live line never clears it"
    (let [f (progress/board-frame plain (assoc (state [(row "a" :running)]) :loose ["one\ntw"]) {:live? true :width 80})]
      (is (= "one\n" (:loose f)))
      (is (= "tw" (:loose-rest f)))))
  (testing "with no newline at all, nothing is printed yet"
    (let [f (progress/board-frame plain (assoc (state [(row "a" :running)]) :loose ["half"]) {:live? true :width 80})]
      (is (= "" (:loose f)))
      (is (= "half" (:loose-rest f)))))
  (testing "a closing board prints the rest, ended"
    (let [f (progress/board-frame plain (assoc (state [(row "a" :running)]) :loose ["half"]) {:live? true :closing? true})]
      (is (= "half\n" (:loose f)))
      (is (= "" (:loose-rest f))))))

;; --- the live line ---------------------------------------------------------------

(defn- live
  ([rows] (live rows 80))
  ([rows width] (:live (progress/board-frame plain (state rows) {:live? true :width width :frame 0}))))

(def ^:private spin (render/spinner-frame plain 0))

(deftest one-running-row-shows-its-label
  (is (= (str "  " spin " reads pods") (live [(row "reads pods" :running) (row "later" :pending)]))))

(deftest several-running-rows-show-their-count-and-labels
  (is (= (str "  " spin " 3 running: a, b, c")
         (live [(row "a" :running) (row "b" :running) (row "c" :running) (row "d" :pending)]))))

(deftest a-row-held-behind-a-running-one-is-not-in-flight
  (is (= (str "  " spin " b") (live [(row "b" :running) (row "c" :done)]))))

(deftest nothing-running-shows-what-has-not-started
  (is (= (str "  " spin " 2 not started") (live [(row "a" :pending) (row "b" :pending)]))))

(deftest nothing-in-flight-draws-no-live-line
  (is (nil? (live [])))
  (is (nil? (live [(row "a" :done)])) "a settled row is committed, not drawn live"))

(deftest a-status-note-replaces-the-live-text
  (testing "the running row's note, after its label"
    (is (= (str "  " spin " phase: 12 of 48 reads") (live [(row "phase" :running {:note "12 of 48 reads"})]))))
  (testing "with others running, how many more"
    (is (= (str "  " spin " b: waiting (2 more running)")
           (live [(row "a" :running) (row "b" :running {:note "waiting"}) (row "c" :running)]))))
  (testing "a note on a settled row is not shown"
    (is (= (str "  " spin " a") (live [(row "a" :running) (row "b" :done {:note "old"})]))))
  (testing "in colour the note is dimmed"
    (is (str/includes? (render/live-line colour [(row "p" :running {:note "n"})] {:frame 0 :width 80})
                       (render/paint colour :dim "p: n")))))

(deftest the-live-line-is-cut-to-the-width
  (let [labels (mapv #(str "namespace-" % "/a-rather-long-workload-name") (range 200))
        rows (mapv #(row % :running) labels)]
    (doseq [width [20 39 60 79 119]]
      (let [l (live rows width)]
        (is (<= (render/columns l) width) (str "width " width ": " l))
        (is (str/ends-with? l "…") "a cut line says it was cut")
        (is (str/starts-with? l (str "  " spin " 200 running: ")) "the count survives the cut")))))

(deftest the-live-line-is-cut-for-wide-characters
  (doseq [label ["名前空間の作業負荷名前空間の作業負荷名前空間の作業負荷名前空間" "📦📦📦📦📦📦📦📦📦📦📦📦📦📦📦📦📦📦📦📦📦📦📦📦📦"]]
    (let [l (live [(row label :running)] 24)]
      (is (<= (render/columns l) 24) l)
      (is (str/ends-with? l "…")))))

(deftest the-live-line-carries-no-line-breaks-or-cursor-movement
  (let [l (live [(row "a\nb\r\u001b[2Jc\td" :running {:note "x\ny"})] 80)]
    (is (not (re-find #"[\n\r\t]" l)))
    (is (not (re-find #"\u001b" l)) "no escape at all with colour off")
    (is (= (str "  " spin " a b  [2Jc d: x y") l))))

(deftest a-themed-spinner-frame-is-drawn
  (is (= "  / a" (render/live-line (render/context {:spinner ["|" "/"]} false) [(row "a" :running)] {:frame 1 :width 80}))))

;; --- render helpers ----------------------------------------------------------------

(deftest columns-err-wide
  (is (= 5 (render/columns "hello")))
  (is (= 5 (render/columns "\u001b[31mhello\u001b[0m")) "colour takes no columns")
  (is (= 1 (render/columns "⠋")) "a braille spinner frame is one column")
  (is (= 4 (render/columns "名前")))
  (is (= 2 (render/columns "✅"))))

(deftest cut-keeps-to-the-budget
  (is (= "hello" (render/cut 5 "hello")))
  (is (= "abc…" (render/cut 4 "abcde")))
  (is (= "…" (render/cut 1 "hello")))
  (is (= "" (render/cut 0 "hello")))
  (is (= "名…" (render/cut 4 "名前空間")) "a wide character that does not fit is left out whole")
  (is (= "📦…" (render/cut 3 "📦📦📦")) "never splits a surrogate pair"))

(deftest terminal-columns-are-known-unclamped
  (is (= 30 (stdout/choose-terminal-columns {:columns "30"})) "a narrow terminal counts as it is")
  (is (= 200 (stdout/choose-terminal-columns {:stty "200"})))
  (is (= 50 (stdout/choose-terminal-columns {:override "50" :columns "30"})))
  (is (nil? (stdout/choose-terminal-columns {:columns "0" :stty "x"})) "an unreadable size is unknown")
  (is (= 33 (binding [stdout/*width* 33] (stdout/terminal-columns)))))

;; --- the writer ------------------------------------------------------------------

(defn- live-err
  "What f prints on stderr with the list forced live, stdout off the screen."
  [f]
  (let [err (java.io.StringWriter.)]
    (with-redefs-fn {#'progress/live? (constantly true)
                     #'progress/terminal? (fn [fd] (= 2 fd))}
      #(binding [*err* err stdout/*width* 80] (f)))
    (str err)))

(defn- screen
  "The lines a terminal shows after it was sent s: cursor up n, carriage
   return, clear to the end of the screen and newline are the only movements
   read."
  [s]
  (let [tokens (re-seq #"\u001b\[(\d+)A|\u001b\[J|\r|\n|[^\u001b\r\n]+|\u001b\[[0-9;]*m" s)]
    (loop [[[token up] & more] tokens lines [] r 0]
      (cond
        (nil? token) (subvec lines 0 (min r (count lines)))
        up (recur more lines (max 0 (- r (parse-long up))))
        (= "\u001b[J" token) (recur more (subvec lines 0 (min r (count lines))) r)
        (= "\r" token) (recur more lines r)
        (= "\n" token) (recur more (if (< r (count lines)) lines (conj lines "")) (inc r))
        :else (recur more (if (< r (count lines)) (update lines r str token) (conj lines token)) r)))))

(defn- ups
  "Every cursor-up count the output holds."
  [s]
  (mapv (comp parse-long second) (re-seq #"\u001b\[(\d+)A" s)))

(defn- many-steps
  [n]
  (mapv (fn [i] {:label (str "step " i) :run #(do (Thread/sleep (if (zero? (mod i 5)) 100 0)) true)}) (range n)))

(deftest a-repaint-moves-up-one-line-at-most
  (let [err (live-err #(progress/run-steps (many-steps 20)))]
    (is (seq (ups err)) "positive control: the live line was repainted")
    (is (every? #(= 1 %) (ups err)))))

(deftest parallel-tasks-repaint-one-line-at-most
  (let [err (live-err #(progress/watch-futures! (mapv (fn [i] [(str "task " i) (future (Thread/sleep (* 40 i)) i)]) (range 8))))]
    (is (seq (ups err)))
    (is (every? #(= 1 %) (ups err)))
    (is (= (mapv #(render/strip-colors (render/task plain :done (str "task " %))) (range 8))
           (mapv render/strip-colors (screen err)))
        "the screen ends with every task done, in order")))

(deftest the-screen-is-cleared-only-over-the-live-line
  (let [err (live-err #(progress/run-steps (many-steps 10)))]
    (is (re-find #"\u001b\[J" err) "positive control: there was a clear")
    (is (= (count (re-seq #"\u001b\[J" err))
           (count (re-seq #"\u001b\[1A\r\u001b\[J" err)))
        "every clear comes right after going up over the live line")))

(deftest each-settled-row-is-written-exactly-once
  (testing "steps one after another"
    (let [ctx (stdout/context)
          err (live-err #(progress/run-steps (many-steps 12)))]
      (doseq [i (range 12)]
        (is (= 1 (count (re-seq (re-pattern (java.util.regex.Pattern/quote (str (render/task ctx :done (str "step " i)) "\n"))) err)))
            (str "step " i)))))
  (testing "parallel tasks"
    (let [ctx (stdout/context)
          err (live-err #(progress/watch-futures! (mapv (fn [i] [(str "task " i) (future (Thread/sleep (* 60 (- 4 i))) i)]) (range 4))))]
      (doseq [i (range 4)]
        (is (= 1 (count (re-seq (re-pattern (java.util.regex.Pattern/quote (str (render/task ctx :done (str "task " i)) "\n"))) err)))
            (str "task " i))))))

(deftest stderr-text-of-a-step-lands-under-its-row-on-screen
  (let [err (live-err #(progress/run-steps [{:label "one" :run (fn [] (binding [*out* *err*] (println "first text")) (Thread/sleep 120) true)}
                                            {:label "two" :run (fn [] (binding [*out* *err*] (println "second text")) true)}]))]
    (is (= ["  ✅ one" "    first text" "  ✅ two" "    second text"]
           (mapv render/strip-colors (screen err))))))

(deftest text-outside-any-step-is-kept-above-the-live-line
  (let [err (live-err #(progress/with-board [b]
                         (progress/start! b (progress/add! b "slow"))
                         (binding [*out* *err*] (print "half ") (flush))
                         (Thread/sleep 200)
                         (binding [*out* *err*] (println "a line"))
                         (Thread/sleep 200)
                         (progress/settle! b 0 true)))]
    (is (= ["half a line" "  ✅ slow"] (mapv render/strip-colors (screen err)))
        "a line printed in two writes reaches the screen whole")))

(deftest status-replaces-the-live-line-on-screen
  (let [err (live-err #(progress/run-steps [{:label "phase"
                                             :run (fn []
                                                    (progress/status! "1 of 2")
                                                    (Thread/sleep 200)
                                                    (progress/status! "2 of 2")
                                                    (Thread/sleep 200)
                                                    true)}]))]
    (is (str/includes? (render/strip-colors err) " phase: 1 of 2\n"))
    (is (str/includes? (render/strip-colors err) " phase: 2 of 2\n"))
    (is (= ["  ✅ phase"] (mapv render/strip-colors (screen err))) "no status line is left behind")))

(deftest a-nested-list-prints-plain-into-the-outer-row
  (let [err (live-err #(progress/run-steps [{:label "outer" :run (fn [] (progress/checklist [["inner" (constantly true)]]))}]))]
    (is (= ["  ✅ outer" "      ✅ inner"] (mapv render/strip-colors (screen err))))
    (is (every? #(= 1 %) (ups err)))))

(defn- repaints
  "The repaints in output s: each piece that starts by going up over the live
   line."
  [s]
  (rest (str/split s #"\u001b\[1A")))

(defn- running-board-output
  "A live board of n rows, all running, left to repaint for a while."
  [n]
  (live-err #(progress/with-board [b]
               (doseq [i (range n)] (progress/start! b (progress/add! b (str "row " i))))
               (Thread/sleep 450)
               (doseq [i (range n)] (progress/settle! b i true)))))

(deftest a-repaint-costs-the-same-however-many-rows-the-board-holds
  (let [small (running-board-output 2)
        large (running-board-output 200)
        ticks (fn [s] (filter #(not (str/includes? % "✅")) (repaints s)))]
    (is (seq (ticks large)) "positive control: the large board repainted")
    (is (every? #(= 1 (count (re-seq #"\n" %))) (ticks large)) "each repaint writes one line")
    (is (every? #(<= (render/columns (str/replace % #"\r\u001b\[J|\n" "")) 80) (ticks large)) "and keeps to the width")
    (is (<= (apply max (map count (ticks large)))
            (+ 100 (apply max (map count (ticks small)))))
        "a 200-row repaint is as large as a 2-row one, within the width")))

;; --- plain output is unchanged -----------------------------------------------------

(deftest plain-and-no-color-output-match-the-golden-transcript
  (let [now (transcript/transcripts)
        was (transcript/golden)]
    (is (= (get-in was [:redirected :err]) (get-in now [:redirected :err])) "stderr redirected")
    (is (= (get-in was [:no-color-flag :err]) (get-in now [:no-color-flag :err])) "--no-color on a terminal")
    (is (= was now))))

(deftest plain-output-carries-no-cursor-movement
  (let [{:keys [redirected no-color-flag]} (transcript/transcripts)]
    (is (not (re-find #"\u001b\[[0-9]*[AJK]" (:err redirected))))
    (is (not (re-find #"\u001b" (:err no-color-flag))) "no escape at all under --no-color")))
