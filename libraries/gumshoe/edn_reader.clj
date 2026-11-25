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
       collections deep, so an oversized or deeply nested file is refused
       before anything else looks at it;
     - it holds exactly one form, a map, and is valid UTF-8.

   Every refusal is an ex-info carrying ::refused, the :file and the
   :reason, with a message naming both. What the map means is not judged
   here: gumshoe.edn-book compiles it with gumshoe.dsl/compile, which
   refuses any key, word or symbol outside its vocabulary. This namespace
   requires nothing beyond clojure.edn, so a launcher listing books reads
   their descriptions without loading the engine."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io PushbackReader StringReader)
           (java.nio ByteBuffer)
           (java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets)))

(def max-bytes
  "The largest book file read, in bytes. gumshoe's own EDN books are
   about 2 KiB; a change that needs more than this is better split."
  65536)

(def max-depth
  "The deepest nesting of collections a book may hold. gumshoe's own EDN
   books nest 6 deep."
  32)

(defn- refuse!
  [file reason]
  (throw (ex-info (str file ": " reason) {::refused true :file (str file) :reason reason})))

(defn refused?
  "Whether e is the reader, or a loader building on it, refusing a file."
  [e]
  (boolean (::refused (ex-data e))))

(defn- bounded-bytes
  "The file's bytes, read up to one past max-bytes, so a file that grows
   between a size check and the read is still refused."
  [file]
  (with-open [in (io/input-stream (fs/file file))]
    (let [bytes (.readNBytes in (int (inc max-bytes)))]
      (when (> (alength bytes) max-bytes)
        (refuse! file (format "is larger than %d bytes - a book file is at most %d bytes" max-bytes max-bytes)))
      bytes)))

(defn- utf-8
  [file bytes]
  (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))]
    (try
      (str (.decode decoder (ByteBuffer/wrap bytes)))
      (catch CharacterCodingException _
        (refuse! file "is not valid UTF-8")))))

(defn- tag-refusal
  [tag]
  (fn [_]
    (throw (ex-info (format "holds the tagged literal #%s - a book is plain EDN, and no tag is read" tag)
                    {::tag tag}))))

(def ^:private read-options
  ;; :readers shadows the two tags clojure.edn reads by default; :default
  ;; refuses every other tag.
  {:readers {'inst (tag-refusal 'inst) 'uuid (tag-refusal 'uuid)}
   :default (fn [tag _] ((tag-refusal tag) nil))})

(defn- nested-deeper?
  "Whether x nests collections deeper than limit, a map's keys and values
   one level below the map. Walks with an explicit stack, so a deep value
   cannot overflow the call stack here."
  [x limit]
  (loop [stack [[x 1]]]
    (if-let [[v depth] (peek stack)]
      (cond
        (> depth limit) true
        (coll? v) (recur (into (pop stack)
                               (map (fn [child] [child (inc depth)]))
                               (if (map? v) (mapcat identity v) v)))
        :else (recur (pop stack)))
      false)))

(defn- read-forms
  "The first form of text and whether another follows it."
  [file text]
  (let [eof (Object.)
        in (PushbackReader. (StringReader. text))
        read-one #(edn/read (assoc read-options :eof eof) in)]
    (try
      (let [form (read-one)]
        [form eof (when-not (identical? form eof) (read-one))])
      (catch StackOverflowError _
        (refuse! file (format "nests too deep to read - a book nests at most %d collections" max-depth)))
      (catch Exception e
        (refuse! file (if-let [tagged (some #(when (::tag (ex-data %)) %) (take-while some? (iterate ex-cause e)))]
                        (ex-message tagged)
                        (str "is not plain EDN: " (or (ex-message e) (str e)))))))))

(defn read-book
  "The map a book file holds, read as described in the namespace doc, or
   an ex-info carrying ::refused naming the file and why."
  [file]
  (when-not (fs/regular-file? file)
    (refuse! file "is not a file"))
  (let [text (utf-8 file (bounded-bytes file))
        [form eof more] (read-forms file text)]
    (cond
      (identical? form eof) (refuse! file "holds no form - a book is one map")
      (not (identical? more eof)) (refuse! file "holds more than one form - a book is one map")
      (not (map? form)) (refuse! file (format "holds %s, not a map - a book is one map"
                                              (cond (vector? form) "a vector"
                                                    (seq? form) "a list"
                                                    (set? form) "a set"
                                                    :else (pr-str form))))
      (nested-deeper? form max-depth) (refuse! file (format "nests deeper than %d collections" max-depth))
      :else form)))

(defn description-of
  "The :description a book file declares, when it reads and declares one
   as a string, else nil. Never throws, so a launcher lists a book it
   cannot read by its path."
  [file]
  (try
    (let [description (:description (read-book file))]
      (when (string? description) description))
    (catch Exception _ nil)))
