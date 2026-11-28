;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.json
  "Reading JSON that a judgement rests on, through briko.json. A read takes
   exactly one RFC 8259 value and reads it whole before it returns, so a
   truncated top-level array fails inside the caller's try instead of
   wherever a lazy element is first reached. It refuses trailing text, an
   object naming one key twice, raw control characters inside strings, and
   input larger or deeper than the limits of its source.

   The limits name where the text came from. command-output covers anything
   a captured command printed, kubectl-output the deeper documents kubectl
   prints."
  (:require [briko.json :as briko-json]
            [clojure.string :as str]
            [gumshoe.shell :as shell]))

(def command-output
  "Limits for JSON a captured command printed. gumshoe.shell keeps at most
   shell/max-out-bytes of stdout and answers exit 125 past it, so the text
   can never be larger, and the byte limit only states that bound again.
   Nesting keeps briko.json's default of 64 levels."
  {:max-bytes shell/max-out-bytes})

(def kubectl-output
  "Limits for JSON kubectl printed: command-output, nested up to 128 levels.
   A CustomResourceDefinition's openAPIV3Schema nests far deeper than other
   objects: of 1761 CRDs applied to a kind cluster (Kubernetes v1.37.0,
   2026-11-28), the deepest single-CRD listing needed a limit of 60, so
   briko.json's default of 64 leaves almost no room and 128 leaves twice the
   deepest one measured."
  (assoc command-output :max-depth 128))

(defn parse-strict
  "The single JSON value text holds, read whole within limits (command-output
   unless given), with keyword keys unless limits names :keys. nil for nil,
   blank text or null. Throws briko.json's refusal when text is not one
   complete JSON value within the limits."
  ([text] (parse-strict text command-output))
  ([text limits]
   (when-not (str/blank? text)
     (briko-json/parse-string text (merge {:keys :keyword} limits)))))

(defn parse-or-nil
  "parse-strict, or nil when text is not one complete JSON value within
   limits."
  ([text] (parse-or-nil text command-output))
  ([text limits]
   (try (parse-strict text limits) (catch Exception _ nil))))
