;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.render
  "The pure half of gumshoe's output: every function here takes a render
   context and returns the string a book prints, so a test renders under any
   theme and colour setting without touching a stream. gumshoe.stdout builds
   the context from the active theme and the terminal and does the printing.

   A context is a map

     {:theme  a resolved theme (gumshoe.theme/resolve-theme)
      :color? whether ANSI colour may be written
      :width  the column budget of a rule, when one is drawn}

   With :color? false an outcome is drawn with its :plain glyph where the
   theme gives one, so a theme whose glyphs differ only by colour still tells
   success from failure in a log file."
  (:require [clojure.string :as str]
            [gumshoe.theme :as theme]))

(def ^:private escapes
  {:red "\u001b[31m" :green "\u001b[32m" :yellow "\u001b[33m"
   :blue "\u001b[34m" :bold "\u001b[1m" :dim "\u001b[2m"})

(def ^:private escape-reset "\u001b[0m")

(defn context
  "A render context for a theme and a colour setting. The theme is a map,
   resolved or partial, or the name of a registered theme; an unknown name
   gives the default."
  ([theme color?] (context theme color? nil))
  ([theme color? width]
   (let [resolved (if (keyword? theme)
                    (or (theme/named theme) (theme/resolve-theme nil))
                    (theme/resolve-theme theme))]
     {:theme resolved
      :color? (boolean (and color? (:color? resolved true)))
      :width width})))

(defn paint
  "text wrapped in the escape of `color` (one of gumshoe.theme/colors) when
   the context allows colour, else text unchanged. An unknown or nil colour
   leaves the text as it is."
  [{:keys [color?]} color text]
  (let [escape (get escapes color)]
    (if (and color? escape) (str escape text escape-reset) (str text))))

(defn strip-colors
  "Removes ANSI colour escapes from a string."
  [s]
  (str/replace (str s) #"\u001b\[[0-9;]*m" ""))

(defn printable
  "s with every terminal escape sequence removed and every other control,
   format (bidirectional overrides, zero-width characters) and line or paragraph
   separator character turned into a space, runs of whitespace collapsed and the
   ends trimmed. Text from a book or a cluster can then neither move the
   cursor, recolour nor reorder what the operator reads next to it."
  [s]
  (-> (str s)
      (str/replace #"\u001b\[[0-?]*[ -/]*[@-~]" "")
      (str/replace #"\u001b\][^\u0007\u001b]*(?:\u0007|\u001b\\)?" "")
      (str/replace #"[\p{Cc}\p{Cf}\p{Zl}\p{Zp}]" " ")
      (str/replace #"\s+" " ")
      str/trim))

(defn visible-width
  "The printed column count of s, colour escapes excluded, so a colourised value
   measures the same as its plain form."
  [s]
  (let [bare (strip-colors (str s))]
    (.codePointCount ^String bare 0 (count bare))))

(defn elide
  "Shortens s to at most `at` visible columns with a middle ellipsis, so a long
   value still shows where it begins and ends. A shorter string passes through."
  [at s]
  (let [s (str s)]
    (if (<= (visible-width s) at)
      s
      ;; head/tail are code-point counts; slice on code points (not UTF-16 units)
      ;; so an astral character at a cut boundary is never split into a lone
      ;; surrogate.
      (let [budget (max 1 (dec at))          ; one column for the ellipsis
            head (quot budget 2)
            tail (- budget head)
            n (count s)]
        (str (subs s 0 (.offsetByCodePoints ^String s 0 head))
             "…"
             (subs s (.offsetByCodePoints ^String s n (- tail))))))))

(defn mark
  "The glyph of an outcome, coloured as the theme says; with colour off, its
   :plain fallback when the theme gives one."
  [{:keys [theme color?] :as ctx} outcome]
  (let [{:keys [glyph color plain]} (theme/outcome-in theme outcome)]
    (if color?
      (paint ctx color glyph)
      (or plain glyph))))

(defn line
  "A report line: the outcome's mark, then the text."
  [ctx outcome text]
  (str (mark ctx outcome) " " text))

(defn item
  "A report line one level in, under a heading or a top-level line - the shape
   of each check in a list of checks."
  [ctx outcome text]
  (str "  " (line ctx outcome text)))

(defn detail
  "A plain line one level in, with no mark: the explanation under an item, a
   command to run by hand, a row of a hand-laid list."
  [_ctx text]
  (str "  " text))

(defn warning-lines
  "The lines showing the warnings a step's child commands wrote, two levels
   in under the step's own line, each marked as a finding."
  [ctx warnings]
  (mapv #(detail ctx (item ctx :finding %)) warnings))

(defn rule
  "A full-width horizontal rule."
  [{:keys [theme width]}]
  (apply str (repeat width (:rule theme))))

(defn section
  "A titled horizontal rule: ── Title ─────────. A title too long for the frame is
   elided in the middle, so the rule always closes within the width."
  [{:keys [theme width] :as ctx} title]
  (let [title (elide (- width 6) title)
        rule (:rule theme)
        padding (apply str (repeat (max 0 (- width 4 (visible-width title))) rule))]
    (str rule rule " " (paint ctx :bold title) " " padding)))

(defn banner
  "A full-width banner that is hard to miss: a rule, the outcome's mark and the
   text, and a rule, each line in the outcome's colour. Returns the three lines."
  [{:keys [theme] :as ctx} outcome text]
  (let [{:keys [color]} (theme/outcome-in theme outcome)
        edge (rule ctx)]
    [(paint ctx color edge)
     (paint ctx color (str "  " (mark (assoc ctx :color? false) outcome) " " text))
     (paint ctx color edge)]))

(defn spinner-frame
  "Frame n of the theme's spinner, in the in-progress colour."
  [{:keys [theme] :as ctx} n]
  (let [frames (:spinner theme)]
    (paint ctx (:color (theme/outcome-in theme :in-progress)) (nth frames (mod n (count frames))))))

(def task-outcomes
  "The outcome a live task list draws for each state of a task. A running task
   draws a spinner frame instead while the list repaints in place. An :unread
   task could not learn its answer and draws :not-yet, so it never reads as
   done; a :warned one finished with a caveat and draws :finding."
  {:pending :not-yet
   :running :in-progress
   :done :done
   :failed :failure
   :unread :not-yet
   :warned :finding
   :skipped :skipped})

(defn task
  "One row of a live task list: the mark of the task's state, then its label.
   With a frame, a running task draws that spinner frame; without one, the
   in-progress mark."
  ([ctx status label] (task ctx status label nil))
  ([ctx status label frame]
   (str "  "
        (if (and (= :running status) frame)
          (spinner-frame ctx frame)
          (mark ctx (get task-outcomes status :not-yet)))
        " " label)))

(defn- fit
  "line cut to width visible columns, colour removed first when it must be cut,
   so a cut never splits an escape. No width leaves the line whole."
  [width line]
  (if (and width (> (visible-width line) width))
    (elide width (strip-colors line))
    line))

(defn step-lines
  "The lines of one row of a task list, as the list draws it: the task line,
   then, one level further in, the reason it did not finish, the text its
   work printed and, while it runs, its latest status note. A row is

     {:label \"…\" :status :pending|:running|:done|:failed|:unread|:warned|:skipped
      :reason \"…\" :output \"text the step printed\" :note \"…\"}

   Options: :frame draws a running task's spinner frame, :tail keeps only the
   last n lines of the output, and :width cuts every line to that many
   columns (a repainted list must not wrap, or the repaint loses count)."
  ([ctx row] (step-lines ctx row {}))
  ([ctx {:keys [label status reason output note]} {:keys [frame tail width]}]
   (let [printed (when-not (str/blank? output) (str/split-lines (str/trimr output)))
         printed (if (and tail (> (count printed) tail)) (take-last tail printed) printed)]
     (mapv (partial fit width)
           (concat [(task ctx status label frame)]
                   (when-not (str/blank? reason) [(str "    " reason)])
                   (map #(str "    " %) printed)
                   (when (and (= :running status) (not (str/blank? note)))
                     [(str "    " (paint ctx :dim note))]))))))

(defn- code-point-columns
  "The columns code point cp may take on a terminal, erring wide: below
   U+1100, and in the punctuation, arrow, box-drawing and braille blocks a
   spinner draws from, one; anything else (CJK, emoji, symbols that a
   terminal may draw two wide) two. A line measured too wide is only cut
   shorter than it had to be."
  [cp]
  (cond
    (< cp 0x1100) 1
    (<= 0x2000 cp 0x22FF) 1
    (<= 0x2500 cp 0x25FF) 1
    (<= 0x2800 cp 0x28FF) 1
    :else 2))

(defn columns
  "The terminal columns s takes at most, colour escapes excluded, counting
   every character that may be drawn two wide as two."
  [s]
  (let [bare (strip-colors s)]
    (reduce + 0 (map code-point-columns (.toArray (.codePoints ^String bare))))))

(defn cut
  "s cut at the end to at most `budget` columns (see columns), with an
   ellipsis where it was cut. s must carry no escapes. A budget below one
   gives the empty string."
  [budget s]
  (let [s (str s)]
    (cond
      (< budget 1) ""
      (<= (columns s) budget) s
      :else (loop [cps (seq (.toArray (.codePoints ^String s))) used 1 kept (StringBuilder.)]
              (let [cp (first cps)
                    w (when cp (code-point-columns cp))]
                (if (and cp (<= (+ used w) budget))
                  (recur (next cps) (+ used w) (.appendCodePoint kept (int cp)))
                  (str kept "…")))))))

(defn printable-cut
  "s made printable (see printable), then cut to at most `budget` columns
   (see cut). Applying it twice gives the same string as applying it once."
  [budget s]
  (cut budget (printable s)))

(defn printable-data
  "data with every string in it made printable and cut (see printable-cut),
   map keys included, so a structure built from text a cluster supplied can be
   printed in any format without carrying escapes, bidirectional or zero-width
   characters, or unbounded text. `budget-of` maps the key a string sits
   under - the nearest map key above it, nil at the top - to its column
   budget. Keywords, numbers, instants and other values pass through.

   A collection nested more than `max-depth` levels deep is replaced by its
   printed form, printable and cut, with print length and level bounded, so
   neither deep nor endless data stops the walk."
  [budget-of max-depth data]
  (letfn [(walk [k depth v]
            (cond
              (string? v) (printable-cut (budget-of k) v)
              (not (coll? v)) v
              (> depth max-depth) (printable-cut (budget-of k)
                                                 (binding [*print-length* 100 *print-level* 4]
                                                   (pr-str v)))
              (map? v) (let [entry (fn [[mk mv]]
                                     [(if (string? mk) (printable-cut (budget-of nil) mk) mk)
                                      (walk mk (inc depth) mv)])]
                         (if (record? v)
                           (reduce (fn [m [ok :as e]]
                                     (let [[mk mv] (entry e)]
                                       (assoc (if (= ok mk) m (dissoc m ok)) mk mv)))
                                   v v)
                           (into (empty v) (map entry) v)))
              (vector? v) (mapv #(walk k (inc depth) %) v)
              (set? v) (into (empty v) (map #(walk k (inc depth) %)) v)
              :else (doall (map #(walk k (inc depth) %) v))))]
    (walk nil 0 data)))

(defn- one-line
  "text as one line of plain text: colour removed and every control
   character, a newline or an escape included, turned into a space, so it can
   never move the cursor."
  [text]
  (str/replace (strip-colors text) #"[\x00-\x1f\x7f]" " "))

(defn live-line
  "The one line a live task list repaints, from the rows it has not printed
   for good yet: a spinner frame, then what is in flight. The first running
   row with a status note (gumshoe.progress/status!) shows as its label and
   that note, with how many more run; else one running row shows its label
   and several their count and labels; with nothing running, how many rows
   have not started. Nil when there is nothing in flight. The text is cut at
   the end to `width` columns, so the line never wraps; nil leaves it whole."
  [ctx rows {:keys [frame width]}]
  (let [running (filter #(= :running (:status %)) rows)
        noted (first (remove #(str/blank? (:note %)) running))
        waiting (count (filter #(= :pending (:status %)) rows))
        text (cond
               noted (str (one-line (:label noted)) ": " (one-line (:note noted))
                          (when (next running) (str " (" (dec (count running)) " more running)")))
               (= 1 (count running)) (one-line (:label (first running)))
               (seq running) (str (count running) " running: "
                                  (str/join ", " (map (comp one-line :label) running)))
               (pos? waiting) (str waiting " not started"))]
    (when text
      (let [spinner (spinner-frame ctx (or frame 0))
            lead (str "  " (strip-colors spinner) " ")
            body (if width (cut (- width (columns lead)) text) text)]
        (str "  " spinner " " (if noted (paint ctx :dim body) body))))))

(defn command
  "The echo of a command about to run: $ and the words."
  [ctx words]
  (str (paint ctx :blue "$") " " (str/join " " words)))
