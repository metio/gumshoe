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
   draws a spinner frame instead while the list repaints in place."
  {:pending :not-yet
   :running :in-progress
   :done :done
   :failed :failure
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

(defn command
  "The echo of a command about to run: $ and the words."
  [ctx words]
  (str (paint ctx :blue "$") " " (str/join " " words)))
