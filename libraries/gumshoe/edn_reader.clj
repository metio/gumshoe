;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.edn-reader
  "Reads a book written as EDN from a file as data, and nothing else. A
   book file may come from anywhere on disk, so the reader evaluates
   nothing and trusts nothing about the file:

     - clojure.edn reads it, never the Clojure reader, so #= read-eval,
       reader conditionals, syntax quote, regexes and anonymous functions
       are refused as syntax;
     - every tagged literal is refused, #inst and #uuid included, because a
       tag reader is code that runs while the file is read;
     - the file is at most max-bytes long and nests at most max-depth
       levels deep, judged before the reader runs, so an oversized or
       deeply nested file is refused before anything else looks at it;
     - it holds exactly one form, a map, and is valid UTF-8.

   Every refusal is an ex-info carrying ::refused, the :file and the
   :reason, with a message naming both. briko.edn does the reading and
   enforces the bounds; this namespace sets the bounds a book gets, adds
   the one-map rule and words each refusal for an operator. What the map
   means is not judged here: gumshoe.edn-book compiles it with
   gumshoe.dsl/compile, which refuses any key, word or symbol outside its
   vocabulary. This namespace requires nothing of the engine, so a
   launcher listing books reads their descriptions without loading it."
  (:require [briko.edn :as edn]))

(def max-bytes
  "The largest book file read, in bytes. gumshoe's own EDN books are
   about 2 KiB; a change that needs more than this is better split."
  65536)

(def max-depth
  "The deepest nesting of collections a book may hold. gumshoe's own EDN
   books nest 6 deep."
  32)

(def ^:private read-options
  {:max-bytes max-bytes :max-depth max-depth})

(defn- refuse!
  [file reason]
  (throw (ex-info (str file ": " reason) {::refused true :file (str file) :reason reason})))

(defn refused?
  "Whether e is the reader, or a loader building on it, refusing a file."
  [e]
  (boolean (::refused (ex-data e))))

(defn- underlying-message
  "The message of the first cause of a briko.edn refusal that is not itself
   a refusal: the reader's or the file system's own words."
  [e]
  (let [cause (some #(when-not (edn/refusal? %) %) (rest (take-while some? (iterate ex-cause e))))]
    (when cause (or (ex-message cause) (str cause)))))

(defn- reason
  "How a briko.edn refusal reads in a book refusal."
  [e]
  (let [{:keys [type tag limit]} (ex-data e)]
    (case type
      :briko.edn/not-a-file "is not a file"
      :briko.edn/unreadable (str "cannot be read: " (underlying-message e))
      :briko.edn/too-large (format "is larger than %d bytes - a book file is at most %d bytes" max-bytes max-bytes)
      :briko.edn/invalid-utf8 "is not valid UTF-8"
      :briko.edn/too-deep (format "nests deeper than %d collections" max-depth)
      :briko.edn/tag-refused (format "holds the tagged literal #%s - a book is plain EDN, and no tag is read" tag)
      :briko.edn/number-too-long (format "is not plain EDN: it holds a number literal longer than %d characters" limit)
      :briko.edn/no-form "holds no form - a book is one map"
      :briko.edn/trailing-form "holds more than one form - a book is one map"
      (str "is not plain EDN: " (or (underlying-message e) (ex-message e))))))

(defn read-book
  "The map a book file holds, read as described in the namespace doc, or
   an ex-info carrying ::refused naming the file and why."
  [file]
  (let [form (try
               (edn/read-file file read-options)
               (catch clojure.lang.ExceptionInfo e
                 (if (edn/refusal? e)
                   (refuse! file (reason e))
                   (throw e))))]
    (if (map? form)
      form
      (refuse! file (format "holds %s, not a map - a book is one map"
                            (cond (vector? form) "a vector"
                                  (seq? form) "a list"
                                  (set? form) "a set"
                                  :else (pr-str form)))))))

(defn description-of
  "The :description a book file declares, when it reads and declares one
   as a string, else nil. Never throws, so a launcher lists a book it
   cannot read by its path."
  [file]
  (try
    (let [description (:description (read-book file))]
      (when (string? description) description))
    (catch Exception _ nil)))
