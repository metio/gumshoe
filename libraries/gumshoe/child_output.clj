;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.child-output
  "Pure judgements over what a child command writes to stderr: which lines
   are warnings, so the step that ran the child can show them under its own
   line, and which are anything else.

   Two shapes count as a warning:
   - `Warning: <message>`, the line client-go's warning writer prints for
     every API warning header (code 299): deprecations, Warn-mode admission
     policies, kubectl drain's ignored DaemonSet pods. kubectl colours the
     label (`ESC[33;1mWarning:ESC[0m`) only when its stderr is a terminal.
   - a klog line at warning level, `Wmmdd hh:mm:ss.uuuuuu <pid> file:line]
     <message>`; the pid is padded to seven characters with spaces.

   Error-level klog lines (`E…`) and every other line are not warnings. A
   failed step shows them in full, and a step that succeeded passes them
   through as the child wrote them."
  (:require [clojure.string :as str]))

(def ^:private ansi
  #"\u001b\[[0-9;]*[A-Za-z]")

(defn- bare
  "s without ANSI escapes and without the line ending a child wrote."
  [s]
  (-> (str s) (str/replace ansi "") (str/replace #"[\r\n]+$" "")))

(def ^:private klog-warning
  #"^W\d{4} \d{2}:\d{2}:\d{2}\.\d{6}\s+\d+ [^\]\s]+\] ")

(def ^:private api-warning-label "Warning:")

(defn warning-line?
  "Whether one stderr line is a warning: an API warning kubectl printed or a
   warning-level klog line."
  [line]
  (let [s (bare line)]
    (boolean (or (str/starts-with? s api-warning-label)
                 (re-find klog-warning s)))))

(defn warning-text
  "The warning as it is shown under a step: the line without colour and
   without its line ending, otherwise as the child wrote it, so it still
   reads as kubectl's own words."
  [line]
  (bare line))

(defn split-stderr
  "{:warnings [...] :other [...]} of a child's stderr text, each a vector of
   lines in the order written. Warnings are cleaned with warning-text and
   given once each, as kubectl itself prints a message once per process;
   blank lines are dropped from :other."
  [text]
  (let [lines (str/split-lines (str text))]
    {:warnings (into [] (comp (filter warning-line?) (map warning-text) (distinct)) lines)
     :other (into [] (comp (remove warning-line?) (map #(str/replace % #"\r$" "")) (remove str/blank?)) lines)}))

(defn line-start
  "How far the beginning of a stderr line, `prefix`, decides whether the
   line is a warning: :warning once it is one, :other once it can no longer
   become one, and :undecided while it still could. A streaming reader
   holds back only an :undecided or :warning beginning, so any other line -
   a progress message, a prompt with no line ending - reaches the terminal
   as soon as its first characters do."
  [prefix]
  (let [p (str prefix)]
    (if (re-find #"\u001b(\[[0-9;]*)?$" p)
      ;; an escape sequence not yet complete, which may colour a label
      :undecided
      (let [s (str/replace p ansi "")]
        (cond
          (str/starts-with? s api-warning-label) :warning
          (str/starts-with? api-warning-label s) :undecided
          (re-find #"^W\d{4} " s) :warning
          (re-find #"^W\d{0,4}$" s) :undecided
          :else :other)))))
