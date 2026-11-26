;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.json
  "Reading JSON that a judgement rests on. cheshire's parse-string reads a
   top-level array lazily, so a truncated array parses without an error and
   throws only later, wherever the first missing element is reached - outside
   the try that meant to catch it. parse-strict reads the whole document
   before it returns, so a caller's try sees every failure."
  (:require [cheshire.core :as cheshire]
            [clojure.string :as str]))

(defn parse-strict
  "The single JSON value text holds, read whole, with keyword keys. nil for
   nil or blank text. Throws when text is not one complete JSON value:
   malformed or truncated JSON, or anything but whitespace after the value."
  [text]
  (when-not (str/blank? text)
    (with-open [reader (java.io.StringReader. text)]
      ;; parsed-seq reads every value whole, top-level arrays included, and
      ;; asking for a second value makes it read past the first, so trailing
      ;; text fails here instead of being silently dropped.
      (let [values (vec (take 2 (cheshire/parsed-seq reader true)))]
        (when (< 1 (count values))
          (throw (ex-info "more than one JSON value" {:text-length (count text)})))
        (first values)))))

(defn parse-or-nil
  "parse-strict, or nil when text is not one complete JSON value."
  [text]
  (try (parse-strict text) (catch Exception _ nil)))
