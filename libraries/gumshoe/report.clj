;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.report
  "Report formats as a plugin seam. A scan renders its findings in the format the
   operator asked for with --output; text (the rich terminal story), json, and edn
   are built in. A plugin registers more - sarif so findings flow into code
   scanning, junit so a CI job turns a scan into pass/fail - with register-format!,
   and nothing about those formats lives in the engine. Each format is
   (fn [report] -> prints to stdout), where report is {:findings :checked :summary}."
  (:require [gumshoe.config :as config]))

(defonce ^:private formats (atom {}))

(defn register-format!
  "Registers a report format under `name` (the --output value that selects it):
   render is (fn [{:keys [findings checked summary]}] -> prints to stdout)."
  [name render]
  {:pre [(string? name) (fn? render)]}
  (swap! formats assoc name render))

(defn registered
  "Every registered format name plus the three built-ins, sorted - so a book can
   list the formats it accepts."
  []
  (sort (into #{"text" "json" "edn"} (keys @formats))))

(defn format-fn
  "The renderer registered for `name`, or nil when none is (the built-in text/json/
   edn are handled by the caller)."
  [name]
  (get @formats name))

(defn configured-format
  "The report format from env.edn :output, or the given default - so an operator
   can make json the house default without passing --output every time."
  [fallback]
  (config/value [:output] fallback))
