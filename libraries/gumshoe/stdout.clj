;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.stdout
  "Terminal output helpers shared by all books.

   Diagnostics (sections, checks, banners, commands) go to stderr; results go
   to stdout. That keeps `bb <book> --output json | jq` and `> result.txt`
   clean while the interactive chatter stays visible on the terminal."
  (:require [clojure.string :as str]
            [gumshoe.theme :as theme]))

(def ^:private escape-red "\u001b[31m")
(def ^:private escape-green "\u001b[32m")
(def ^:private escape-yellow "\u001b[33m")
(def ^:private escape-blue "\u001b[34m")
(def ^:private escape-bold "\u001b[1m")
(def ^:private escape-reset "\u001b[0m")

(def ^:private width 80)

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

(def ^:private colors?
  ;; Decided once at load: FORCE_COLOR forces on (for `| less -R`), NO_COLOR
  ;; forces off (https://no-color.org), otherwise on only when stdout is a
  ;; terminal - so `./detect > file` and `./detect | cat` come out clean.
  (atom (cond
          (not (str/blank? (System/getenv "FORCE_COLOR"))) true
          (not (str/blank? (System/getenv "NO_COLOR"))) false
          :else (terminal? 1))))

(defn disable-colors!
  "Turns off ANSI coloring for the rest of the run (the --no-color flag)."
  []
  (reset! colors? false))

(defn colors-enabled? [] @colors?)

(defn- colorize
  [escape text]
  ;; Colour needs both a colour-capable destination (tty / FORCE_COLOR / not
  ;; NO_COLOR) and a theme that permits it, so a :plain theme is always colourless.
  (if (and @colors? (theme/token :color? true)) (str escape text escape-reset) text))

(defn strip-colors
  "Removes ANSI color escapes from a string. fzf --ansi renders colors but
   returns the selected line without them, so a colored menu label must be
   matched back to its item by its stripped form."
  [s]
  (str/replace (str s) #"\[[0-9;]*m" ""))

(defn- visible-width
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

(defn wrap
  "Word-wraps text to at most `at` columns (default the shared width), breaking on
   whitespace so long prose reflows cleanly instead of at the terminal's ragged
   edge. A word longer than the line is left whole rather than split; existing
   newlines are kept as paragraph breaks."
  ([text] (wrap text width))
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

(defn shorten-path
  "Replaces the home-directory prefix of a path with ~, so a long absolute path
   reads shorter while staying copy-pasteable. A path outside home is unchanged."
  [path]
  (let [p (str path)
        home (str (System/getProperty "user.home"))]
    ;; match the home directory itself or a child under it, on a path boundary -
    ;; a bare starts-with? would rewrite a sibling like /home/sebastian when home
    ;; is /home/seb.
    (if (and (not (str/blank? home))
             (or (= p home) (str/starts-with? p (str home "/"))))
      (str "~" (subs p (count home)))
      p)))

(defn red [text] (colorize escape-red text))
(defn green [text] (colorize escape-green text))
(defn yellow [text] (colorize escape-yellow text))
(defn blue [text] (colorize escape-blue text))
(defn bold [text] (colorize escape-bold text))

(defn err-println
  "Prints to stderr - the channel for everything that is not a result."
  [& parts]
  (binding [*out* *err*]
    (apply println parts)))

(defn with-spinner
  "Runs thunk while animating a spinner with the message on stderr, so a slow
   fetch never looks like a hang. Returns thunk's value. Only animates on a
   terminal (reusing the color decision as the tty signal); elsewhere it prints
   the message once and runs. The spinner writes to stderr only and erases its
   own line when done, so a captured result and a scrolling log both stay clean."
  [message thunk]
  (if @colors?
    (let [frames (theme/token :spinner)
          running (atom true)
          animation (future
                      (loop [i 0]
                        (when @running
                          (binding [*out* *err*]
                            (print (str "\r" (blue (nth frames (mod i (count frames)))) " " message))
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
  []
  (err-println (apply str (repeat width (theme/token :rule)))))

(defn print-section
  "A titled horizontal rule: ── Title ─────────. A title too long for the frame is
   elided in the middle, so the rule always closes within the width."
  [title]
  (let [title (elide (- width 6) title)
        rule (theme/token :rule)
        visible (visible-width title)
        padding (apply str (repeat (max 0 (- width 4 visible)) rule))]
    (err-println (str rule rule " " (bold title) " " padding))))

(defn print-banner
  "A full-width banner that is hard to miss."
  [color-fn text]
  (let [line (apply str (repeat width (theme/token :rule)))]
    (err-println (color-fn line))
    (err-println (color-fn (str "  " text)))
    (err-println (color-fn line))))

;; The status marks are ANSI-colored like the check-ok/check-error ticks, so a
;; terminal that renders the ✅/❌/🔶 emoji monochrome still shows green/red/yellow.
;; colorize honors --no-color / NO_COLOR and the ASCII theme, so this is a no-op
;; when color is off.
(defn ok
  [& parts]
  (err-println (green (theme/token :ok)) (str/join " " parts)))

(defn error
  [& parts]
  (err-println (red (theme/token :error)) (str/join " " parts)))

(defn warn
  [& parts]
  (err-println (yellow (theme/token :warn)) (str/join " " parts)))

(defn check-ok
  [& parts]
  (err-println (str "  " (green (theme/token :check-ok))) (str/join " " parts)))

(defn check-error
  [& parts]
  (err-println (str "  " (red (theme/token :check-error))) (str/join " " parts)))

(defn selected
  "Echoes a resolved selection - the label and what was chosen - to the terminal.
   An interactive fuzzy pick otherwise leaves no trace in the run's visible output,
   only in the recording's selections.edn, so a copy-pasted transcript cannot show
   which subject the run acted on. Diagnostic, so it joins the other chatter on
   stderr; a multi-pick collection is rendered comma-separated."
  [label value]
  (err-println (blue (theme/token :bullet))
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
  (err-println (blue "$") (str/join " " args)))
