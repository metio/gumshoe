;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.stdout
  "The output primitives every book composes its terminal output from.

   Diagnostics (sections, report lines, task lists, banners, commands) go to
   stderr; results go to stdout. That keeps `bb <book> --output json | jq`
   and `> result.txt` clean while the interactive chatter stays visible on
   the terminal.

   The surface, each taking its marks from the active theme (gumshoe.theme):

     (print-section \"Title\")            a titled rule
     (line :success \"text\")             a report line for any outcome:
                                         :success :failure :finding :info
                                         :done :not-yet :in-progress :skipped
     (item :not-yet \"text\")             the same, one level in
     (note \"text\") (detail \"text\")     a plain line, at the margin or one in
     (banner :failure \"text\")           a full-width banner
     (print-table cols rows)             an aligned table
     (print-command \"kubectl\" ...)       the echo of a command about to run
     gumshoe.progress/run-steps          a live task list of a book's own steps

   ok, error, warn, check-ok and check-error are the names lines had before
   outcomes were named; each prints the same as its `line`/`item` outcome.
   The strings these print are rendered by gumshoe.render, pure functions
   of a context, which `context` builds from the active theme and terminal."
  (:require [briko.paths :as paths]
            [clojure.string :as str]
            [gumshoe.render :as render]
            [gumshoe.theme :as theme]))

(def fixed-width
  "The width of output that is captured: piped, redirected or recorded output
   is compared and shared, so it must not depend on the terminal it came from."
  80)

(def ^:private width-range [60 120])

(defn- fd-target
  "What file descriptor fd is pointing at, via /proc, or nil where there is no
   /proc (macOS) or it can not be read."
  [fd]
  (try
    (str (java.nio.file.Files/readSymbolicLink
          (.toPath (java.io.File. (str "/proc/self/fd/" fd)))))
    (catch Exception _ nil)))

(defn- terminal?
  "Whether fd is a terminal. A pipe or a redirect to a file points somewhere
   that is not under /dev, so ANSI would just be noise there. Where /proc is
   unavailable the target is unknown, so we assume a terminal and let NO_COLOR
   be the escape hatch."
  [fd]
  (let [target (fd-target fd)]
    (or (nil? target)
        (str/starts-with? target "/dev/"))))

(defn- parse-width
  [value]
  (some-> value str str/trim not-empty (#(re-matches #"\d+" %)) parse-long))

(defn choose-width
  "The output width from what is known about the destination: an explicit
   override wins wherever output goes; a live terminal uses COLUMNS, else the
   size stty reports; anything else - and a terminal whose size cannot be read
   - gets the fixed width. A live width is clamped, so a very wide terminal
   does not stretch a rule across the screen and a narrow one keeps titles
   readable. Without /proc the destination of a stream is unknown, so there
   output keeps the fixed width unless the override is set."
  [{:keys [override live? columns stty]}]
  (let [[low high] width-range
        clamp #(max low (min high %))]
    (if-let [explicit (parse-width override)]
      (clamp explicit)
      (if-let [live (when live? (or (parse-width columns) (parse-width stty)))]
        (clamp live)
        fixed-width))))

(defn- stty-columns
  "The terminal's columns as stty reports them, read from /dev/tty, or nil."
  []
  (try
    (let [process (-> (ProcessBuilder. ["stty" "size"])
                      (.redirectInput (java.io.File. "/dev/tty"))
                      (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD)
                      (.start))
          out (slurp (.getInputStream process))]
      (when (.waitFor process 2 java.util.concurrent.TimeUnit/SECONDS)
        (second (str/split (str/trim out) #"\s+"))))
    (catch Exception _ nil)))

(defn- detect-width
  [fd]
  (let [live? (and (fd-target fd) (terminal? fd))]
    (choose-width {:override (System/getenv "GUMSHOE_WIDTH")
                   :live? live?
                   :columns (when live? (System/getenv "COLUMNS"))
                   :stty (when live? (stty-columns))})))

(def ^:private diagnostics-width
  ;; stderr carries the rules and banners; detected once, on first use
  (delay (detect-width 2)))

(def ^:private results-width
  ;; stdout carries the findings; nil unless it is a live terminal (or an
  ;; override is set), so captured results keep one line per hint
  (delay (when (or (parse-width (System/getenv "GUMSHOE_WIDTH"))
                   (and (fd-target 1) (terminal? 1)))
           (detect-width 1))))

(def ^:dynamic *width*
  "The diagnostics width, when bound; tests and callers that render for a
   known destination bind it."
  nil)

(defn choose-terminal-columns
  "The columns of a live terminal as they are known, unclamped: the override,
   else COLUMNS, else what stty reports, else nil. A repainted line must fit
   the real terminal, so a terminal narrower than the clamped width counts
   as it is."
  [{:keys [override columns stty]}]
  (some #(when-let [w (parse-width %)] (when (pos? w) w)) [override columns stty]))

(def ^:private stderr-columns
  (delay (when (and (fd-target 2) (terminal? 2))
           (choose-terminal-columns {:override (System/getenv "GUMSHOE_WIDTH")
                                     :columns (System/getenv "COLUMNS")
                                     :stty (stty-columns)}))))

(def unknown-terminal-columns
  "The columns a repainted line keeps to when the terminal's are unknown:
   narrow enough for any terminal still in use."
  40)

(defn terminal-columns
  "The columns a line repainted in place on stderr must fit: *width* when
   bound, else the terminal's own, else unknown-terminal-columns."
  []
  (or *width* @stderr-columns unknown-terminal-columns))

(defn width
  "The width rules, banners and wrapped prose on stderr fill."
  []
  (or *width* @diagnostics-width))

(defn results-wrap-width
  "The width to wrap results on stdout at, or nil to leave them unwrapped."
  []
  (or *width* @results-width))

(def ^:private colors?
  ;; Decided once at load: FORCE_COLOR forces on (for `| less -R`), NO_COLOR
  ;; forces off (https://no-color.org), otherwise on only when stdout is a
  ;; terminal - so `./detect > file` and `./detect | cat` come out clean.
  (atom (cond
          (not (str/blank? (System/getenv "FORCE_COLOR"))) true
          (not (str/blank? (System/getenv "NO_COLOR"))) false
          :else (terminal? 1))))

(def ^:private no-color-flag?
  ;; Whether the operator passed --no-color, kept apart from colors?, which
  ;; is also off when stdout is merely captured: a captured stdout says
  ;; nothing about the terminal on stderr, the flag says no decoration at all.
  (atom false))

(defn disable-colors!
  "Turns off ANSI coloring for the rest of the run (the --no-color flag)."
  []
  (reset! no-color-flag? true)
  (reset! colors? false))

(defn colors-enabled?
  "Whether this run writes ANSI colour (see colors? above)."
  []
  @colors?)

(defn no-color-requested?
  "Whether the operator asked for no colour with --no-color this run."
  []
  @no-color-flag?)

(defn strip-colors
  "Removes ANSI color escapes from a string. fzf --ansi renders colors but
   returns the selected line without them, so a colored menu label must be
   matched back to its item by its stripped form."
  [s]
  (render/strip-colors s))

(defn- visible-width
  [s]
  (render/visible-width s))

(defn context
  "The render context of this run: the active theme, and whether colour is on
   - both a colour-capable destination (tty, FORCE_COLOR, no NO_COLOR or
   --no-color) and a theme that permits it. `width` adds the column budget a
   rule or banner fills."
  ([] {:theme (theme/active-theme)
       :color? (boolean (and @colors? (theme/token :color? true)))})
  ([width] (assoc (context) :width width)))

(defn elide
  "Shortens s to at most `at` visible columns with a middle ellipsis, so a long
   value still shows where it begins and ends. A shorter string passes through."
  [at s]
  (render/elide at s))

(defn wrap
  "Word-wraps text to at most `at` columns (default the shared width), breaking on
   whitespace so long prose reflows cleanly instead of at the terminal's ragged
   edge. A word longer than the line is left whole rather than split; existing
   newlines are kept as paragraph breaks."
  ([text] (wrap text (width)))
  ([text at]
   (str/join
    "\n"
    (for [para (str/split-lines (str text))]
      (->> (str/split (str/trim para) #"\s+")
           (reduce (fn [lines word]
                     (let [cur (peek lines)]
                       (if (and cur (<= (+ (visible-width cur) 1 (visible-width word)) at))
                         (conj (pop lines) (str cur " " word))
                         (conj lines word))))
                   [])
           (str/join "\n"))))))

(defn hanging
  "text after a label, wrapped to at columns with every further line indented
   under the text, or the plain line when at is nil."
  [indent label text at]
  (let [lead (str indent label)]
    (if-not at
      (str lead text)
      (let [pad (apply str (repeat (visible-width lead) \space))
            [first-line & more] (str/split-lines (wrap text (max 20 (- at (visible-width lead)))))]
        (str/join "\n" (cons (str lead first-line) (map #(str pad %) more)))))))

(defn shorten-path
  "Replaces the home-directory prefix of a path with ~, so a long absolute path
   reads shorter while staying copy-pasteable. A path outside home is unchanged.
   The home is the one a shell expands ~ to, $HOME, then the user.home
   property (briko.paths/home)."
  [path]
  (let [p (str path)
        home (try (paths/home)
                  (catch clojure.lang.ExceptionInfo e
                    (if (paths/refusal? e) "" (throw e))))]
    ;; match the home directory itself or a child under it, on a path boundary -
    ;; a bare starts-with? would rewrite a sibling like /home/sebastian when home
    ;; is /home/seb.
    (if (and (not (str/blank? home))
             (or (= p home) (str/starts-with? p (str home "/"))))
      (str "~" (subs p (count home)))
      p)))

;; Colour needs both a colour-capable destination (tty / FORCE_COLOR / not
;; NO_COLOR) and a theme that permits it, so a :plain theme is always colourless.
(defn red "text in red when colour is on." [text] (render/paint (context) :red text))
(defn green "text in green when colour is on." [text] (render/paint (context) :green text))
(defn yellow "text in yellow when colour is on." [text] (render/paint (context) :yellow text))
(defn blue "text in blue when colour is on." [text] (render/paint (context) :blue text))
(defn bold "text in bold when colour is on." [text] (render/paint (context) :bold text))

(defn err-println
  "Prints to stderr - the channel for everything that is not a result."
  [& parts]
  (binding [*out* *err*]
    (apply println parts)))

(defn animate?
  "Pure: whether a line on stderr may be repainted in place. The repaint
   writes carriage returns to stderr, so stderr must be a terminal: `bb book
   2> run.log` gets the line once instead of a frame every 90 ms, and
   FORCE_COLOR asks for colour, not for cursor movement into a log. NO_COLOR
   and --no-color turn it off as well. Whether stdout is captured plays no
   part, so `-o json | jq` keeps the live line on the terminal."
  [{:keys [stderr-terminal? no-color? no-color-flag?]}]
  (boolean (and stderr-terminal?
                (not no-color?)
                (not no-color-flag?))))

(def ^:dynamic *in-task-list*
  "True while a gumshoe.progress task list collects what its steps print. A
   spinner writing carriage returns there would land in a step's text, so
   with-spinner prints its message once instead."
  false)

(defn- stderr-live?
  []
  (and (not *in-task-list*)
       (animate? {:stderr-terminal? (terminal? 2)
             :no-color? (not (str/blank? (System/getenv "NO_COLOR")))
             :no-color-flag? @no-color-flag?})))

(defn with-spinner
  "Runs thunk while animating a spinner with the message on stderr, so a slow
   fetch never looks like a hang. Returns thunk's value. Only animates when
   stderr is a live terminal (animate?); elsewhere it prints the message once
   and runs. The spinner writes to stderr only and erases its own line when
   done, so a captured result and a scrolling log both stay clean."
  [message thunk]
  (if (stderr-live?)
    (let [ctx (context)
          running (atom true)
          animation (future
                      (loop [i 0]
                        (when @running
                          (binding [*out* *err*]
                            (print (str "\r" (render/spinner-frame ctx i) " " message))
                            (flush))
                          (Thread/sleep 90)
                          (recur (inc i)))))]
      (try
        (thunk)
        (finally
          (reset! running false)
          @animation
          (binding [*out* *err*]
            (print (str "\r" (apply str (repeat (+ 4 (count message)) \space)) "\r"))
            (flush)))))
    (do (err-println message)
        (thunk))))

(defn print-section-marker
  "A full-width rule with no title, closing a block of output."
  []
  (err-println (render/rule (context (width)))))

(defn print-section
  "A titled horizontal rule: ── Title ─────────. A title too long for the frame is
   elided in the middle, so the rule always closes within the width."
  [title]
  (err-println (render/section (context (width)) title)))

(defn print-banner
  "A full-width banner that is hard to miss, every line passed through color-fn
   (red, green, yellow ...). `banner` takes the colour and the leading glyph
   from an outcome instead."
  [color-fn text]
  (let [line (render/rule (context (width)))]
    (err-println (color-fn line))
    (err-println (color-fn (str "  " text)))
    (err-println (color-fn line))))

(defn banner
  "A full-width banner for an outcome: the theme's mark and colour for it, and
   the text."
  [outcome text]
  (doseq [l (render/banner (context (width)) outcome text)]
    (err-println l)))

(defn line
  "A report line on stderr: the theme's mark for outcome, then the parts joined
   by spaces. The outcomes are those gumshoe.theme/outcomes names - :success,
   :failure, :finding and :info for what a check concluded, :done, :not-yet,
   :in-progress and :skipped for how far a piece of work got."
  [outcome & parts]
  (err-println (render/line (context) outcome (str/join " " parts))))

(defn item
  "A report line one level in (see line): one check of a list under a section
   or a top-level line."
  [outcome & parts]
  (err-println (render/item (context) outcome (str/join " " parts))))

(defn note
  "A plain line on stderr with no mark, the parts joined by spaces: prose a
   book explains itself with, a blank line, a hint. err-println, under the
   name the surface documents."
  [& parts]
  (err-println (str/join " " parts)))

(defn detail
  "A plain line one level in, with no mark: the explanation under an item, a
   command to run by hand, a row of a hand-laid list."
  [& parts]
  (err-println (render/detail (context) (str/join " " parts))))

(defn child-warnings
  "Prints the warnings a step's child commands wrote under the step's line
   (render/warning-lines); nothing when there are none."
  [warnings]
  (run! err-println (render/warning-lines (context) warnings)))

(defn ok
  "A success line: (line :success ...)."
  [& parts]
  (apply line :success parts))

(defn error
  "A failure line: (line :failure ...)."
  [& parts]
  (apply line :failure parts))

(defn warn
  "A finding line, something worth the operator's attention: (line :finding ...)."
  [& parts]
  (apply line :finding parts))

(defn check-ok
  "A passed check one level in: (item :success ...)."
  [& parts]
  (apply item :success parts))

(defn check-error
  "A failed check one level in: (item :failure ...)."
  [& parts]
  (apply item :failure parts))

(defn selected
  "Echoes a resolved selection - the label and what was chosen - to the terminal.
   An interactive fuzzy pick otherwise leaves no trace in the run's visible output,
   only in the recording's selections.edn, so a copy-pasted transcript cannot show
   which subject the run acted on. Diagnostic, so it joins the other chatter on
   stderr; a multi-pick collection is rendered comma-separated."
  [label value]
  (err-println (render/mark (context) :info)
               (str (bold label) ":")
               (green (if (coll? value) (str/join ", " value) (str value)))))

(defn data-table
  "Formats key/value data as aligned lines, preserving insertion order."
  [data]
  (if (empty? data)
    ""
    (let [labels (map name (keys data))
          label-width (apply max (map count labels))]
      (->> (map (fn [label value] (format (str "%-" label-width "s : %s") label value))
                labels
                (vals data))
           (str/join "\n")))))

(defn print-data-table
  "Prints key/value data (see data-table) under a rule."
  [data]
  (print-section-marker)
  (err-println (data-table data)))

(defn- pad
  [s target]
  (str s (apply str (repeat (max 0 (- target (visible-width s))) \space))))

(defn table
  "Renders rows as an aligned text table. `columns` is a seq of [header accessor]
   pairs; accessor is a keyword (looked up in the row map) or a fn of the row.
   Each column is padded to its widest visible cell - colour codes excluded, so a
   colourised value still lines up - and trailing padding is trimmed. Returns the
   table as a string (header line first); empty rows yield an empty string."
  [columns rows]
  (if (empty? rows)
    ""
    (let [headers (mapv (comp str first) columns)
          accessors (mapv second columns)
          cell (fn [row acc] (str (if (fn? acc) (acc row) (get row acc))))
          grid (into [headers] (mapv (fn [row] (mapv #(cell row %) accessors)) rows))
          widths (mapv (fn [i] (apply max (map #(visible-width (nth % i)) grid)))
                       (range (count headers)))
          render (fn [cells] (str/trimr (str/join "  " (map pad cells widths))))]
      (str/join "\n" (map render grid)))))

(defn print-table
  "Prints a titled table: the header row in bold, then each data row. A table with
   no rows prints nothing, so a caller need not guard the empty case."
  [columns rows]
  (when (seq rows)
    (let [[header & body] (str/split-lines (table columns rows))]
      (err-println (bold header))
      (doseq [line body] (err-println line)))))

(defn print-command
  "Shows the exact command about to run, so the underlying tooling stays learnable."
  [& args]
  (err-println (render/command (context) args)))
