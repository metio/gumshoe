;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.edn-book
  "Runs a book written as EDN: a change in the gumshoe.dsl vocabulary, in a
   file of its own. The run door launches one as

     bb -m gumshoe.edn-book <file> [flags]

   which reads the file with gumshoe.edn-reader (plain EDN only, bounded,
   no tags), compiles it with gumshoe.dsl/compile (a closed vocabulary,
   functions only through gumshoe.dsl.registry) and hands the change to
   mutation/book, so it gets the --help, --dry-run, prerequisites,
   confirmation, pin and post-checks every mutation book gets.

   The format is experimental. A book declares :gumshoe/dsl
   :v0-experimental, anything else is refused, and every run prints a
   one-line note on stderr that the format may change in any release."
  (:require [gumshoe.dsl :as dsl]
            [gumshoe.edn-reader :as reader]
            [gumshoe.mutation :as mutation]
            [gumshoe.plugins :as plugins]
            [gumshoe.stdout :as stdout]))

(defn experimental-note
  "The line every run of an EDN book prints on stderr before anything else."
  [file]
  (format "note: %s is written in the experimental EDN book format (:gumshoe/dsl %s), which may change in any release"
          file dsl/version))

(defn load-book
  "The change a book file holds, compiled by gumshoe.dsl/compile, or an
   ex-info carrying gumshoe.edn-reader/refused? naming the file and why it
   is refused: the reader's refusals, compile's (with the path of the
   offending form), and a change too deep for compile to walk."
  [file]
  (let [source (reader/read-book file)]
    (try
      (dsl/compile source)
      (catch clojure.lang.ExceptionInfo e
        (if (dsl/invalid? e)
          (throw (ex-info (str file ": " (ex-message e))
                          (assoc (ex-data e) ::reader/refused true :file (str file) :reason (ex-message e))))
          (throw e)))
      (catch StackOverflowError _
        (throw (ex-info (str file ": nests too deep to compile")
                        {::reader/refused true :file (str file) :reason "nests too deep to compile"}))))))

(defn book
  "Hands the change a book file holds to mutation/book: under
   gumshoe.runbook/*loading* that returns the book's config and runs
   nothing, as loading a Clojure book does; otherwise it runs the book
   with *command-line-args* as its flags."
  [file]
  (mutation/book (load-book file)))

(defn run
  "Runs the book file with args as its flags. Returns 1 for a refused
   file and 0 after --help; a run that gets as far as its action exits
   from the harness with the book's own exit code. The configured plugins load
   first, so a function a plugin registers with gumshoe.dsl.registry is
   known when the book compiles."
  [file args]
  (stdout/err-println (experimental-note file))
  (plugins/load!)
  (if-let [change (try (load-book file)
                       (catch clojure.lang.ExceptionInfo e
                         (if (reader/refused? e)
                           (stdout/error (str "refused " (ex-message e)))
                           (throw e))))]
    (do
      ;; the harness names the book in --help, the run header and the
      ;; recording from this property, as `bb <file>` sets it for a
      ;; Clojure book
      (System/setProperty "babashka.file" (str file))
      (binding [*command-line-args* (seq args)]
        (mutation/book change))
      0)
    1))

(defn -main
  [& [file & args]]
  (if (nil? file)
    (do (stdout/error "name the book file: bb -m gumshoe.edn-book <file.edn> [flags]")
        (System/exit 1))
    (let [code (run file args)]
      (when-not (zero? code) (System/exit code)))))
