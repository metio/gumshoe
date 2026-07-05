;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.stdout
  "Terminal output helpers shared by all books.

   Diagnostics (sections, checks, banners, commands) go to stderr; results go
   to stdout. That keeps `bb <book> --output json | jq` and `> result.txt`
   clean while the interactive chatter stays visible on the terminal."
  (:require [clojure.string :as str]))

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
  (if @colors? (str escape text escape-reset) text))

(defn strip-colors
  "Removes ANSI color escapes from a string. fzf --ansi renders colors but
   returns the selected line without them, so a colored menu label must be
   matched back to its item by its stripped form."
  [s]
  (str/replace (str s) #"\[[0-9;]*m" ""))

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

(def ^:private spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(defn with-spinner
  "Runs thunk while animating a spinner with the message on stderr, so a slow
   fetch never looks like a hang. Returns thunk's value. Only animates on a
   terminal (reusing the color decision as the tty signal); elsewhere it prints
   the message once and runs. The spinner writes to stderr only and erases its
   own line when done, so a captured result and a scrolling log both stay clean."
  [message thunk]
  (if @colors?
    (let [running (atom true)
          animation (future
                      (loop [i 0]
                        (when @running
                          (binding [*out* *err*]
                            (print (str "\r" (blue (nth spinner-frames (mod i (count spinner-frames)))) " " message))
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
  (err-println (apply str (repeat width "─"))))

(defn print-section
  "A titled horizontal rule: ── Title ─────────"
  [title]
  (let [visible (.codePointCount ^String title 0 (count title))
        padding (apply str (repeat (max 0 (- width 4 visible)) "─"))]
    (err-println (str "── " (bold title) " " padding))))

(defn print-banner
  "A full-width banner that is hard to miss."
  [color-fn text]
  (let [line (apply str (repeat width "─"))]
    (err-println (color-fn line))
    (err-println (color-fn (str "  " text)))
    (err-println (color-fn line))))

(defn ok
  [& parts]
  (err-println "✅" (str/join " " parts)))

(defn error
  [& parts]
  (err-println "❌" (str/join " " parts)))

(defn warn
  [& parts]
  (err-println "🔶" (str/join " " parts)))

(defn check-ok
  [& parts]
  (err-println (str "  " (green "✓")) (str/join " " parts)))

(defn check-error
  [& parts]
  (err-println (str "  " (red "✗")) (str/join " " parts)))

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

(defn print-command
  "Shows the exact command about to run, so the underlying tooling stays learnable."
  [& args]
  (err-println (blue "$") (str/join " " args)))
