;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.conformance-test
  "Machine-enforced repository rules. Every book and library follows the
   same contract, and these tests keep it that way - discipline is nice,
   automation does not forget. The rules a casebook shares live in
   gumshoe.conformance; the ones about this repository - capabilities a tool
   package provides, the test run list - stay here."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.conformance :as conformance]))

(def ^:private all-clj-files
  "Every book and library, core and tool-package - scanned statically for both
   what a tool provides and what a book requires."
  (conformance/clj-files ["runbooks" "playbooks" "firebooks" "libraries" "tools"]))

(def ^:private book-files
  (conformance/clj-files (:books conformance/default-paths)))

(def ^:private library-files
  (conformance/clj-files (:libraries conformance/default-paths)))

(def ^:private forms-of conformance/forms-of)

(defn- assert-none
  "A failure reads as the list of files to fix."
  [findings]
  (is (empty? findings) (str/join "\n" (map conformance/format-finding findings))))

(defn- parsed [files] (mapv conformance/parse files))

(deftest every-file-parses
  (assert-none (conformance/unparseable (parsed all-clj-files))))

(deftest every-file-carries-the-license
  (assert-none (conformance/missing-license (concat book-files library-files) "0BSD")))

(deftest every-namespace-documents-itself
  (assert-none (conformance/undocumented-namespaces (parsed (concat book-files library-files)))))

(deftest every-book-runs-through-the-harness
  (assert-none (conformance/harness-entries (parsed book-files))))

(deftest every-book-describes-itself
  (assert-none (conformance/undescribed-books (parsed book-files))))

(def ^:private tool-book-files
  (conformance/clj-files (for [tool (fs/list-dir "tools")
                               root ["runbooks" "playbooks" "firebooks"]]
                           (str (fs/path tool root)))))

(deftest every-book-loads
  ;; Every book is loaded in this one process with the harness told not to
  ;; run, so a book referring to a var that does not exist fails here.
  (assert-none (conformance/unloadable (concat book-files tool-book-files))))

(deftest every-edn-book-passes-the-gate
  ;; A book written as EDN is held to the whole gate a casebook's verify!
  ;; runs, guards required: wherever it sits in a book root, and gumshoe's
  ;; own EDN forms under tests/edn-books.
  (let [edn-books (filter conformance/edn-book?
                          (conformance/book-files (concat (:books conformance/default-paths)
                                                          (for [tool (fs/list-dir "tools")
                                                                root ["runbooks" "playbooks" "firebooks"]]
                                                            (str (fs/path tool root)))
                                                          ["tests/edn-books/runbooks"])))]
    (is (<= 4 (count edn-books)))
    (assert-none (:findings (conformance/report {:books edn-books :libraries [] :license "0BSD" :guards :require})))))

(deftest every-casebook-example-book-loads
  (assert-none (conformance/unloadable
                (conformance/clj-files ["casebook-example/runbooks" "casebook-example/playbooks"
                                        "casebook-example/firebooks"])
                {:classpath ["casebook-example/libraries"]})))

(def ^:private tool-library-files
  ;; ui-charm builds on a terminal UI library that only its own deps.edn
  ;; brings, so it cannot be required on this classpath; it defines no spec.
  (conformance/clj-files (for [tool (fs/list-dir "tools")
                               :when (not= "ui-charm" (str (fs/file-name tool)))]
                           (str (fs/path tool "libraries")))))

(deftest every-mutation-spec-keeps-its-contract
  ;; Every core, tool and example book is loaded and every library required,
  ;; and each mutation spec they define is judged as it evaluates.
  (let [books (concat book-files tool-book-files
                      (conformance/clj-files ["casebook-example/runbooks" "casebook-example/playbooks"]))
        libraries (concat library-files tool-library-files
                          (conformance/clj-files ["casebook-example/libraries"]))
        parsed-books (parsed books)]
    (assert-none (conformance/contract-findings
                  (conformance/load-books books {:classpath ["casebook-example/libraries"]})
                  parsed-books
                  (parsed libraries)))))

(deftest every-option-is-documented-and-aliases-never-collide
  (assert-none (conformance/option-problems (parsed book-files))))

;; malformed post-checks are refused at runtime by verify/eventually, which
;; has its own tests - a textual rule here would misjudge checks composed via
;; helpers like firebook/burning-check

(defn- nodes-of
  "Every node in a file's parsed forms, for structural scans."
  [file]
  (tree-seq coll? seq (vec (forms-of file))))

(defn- capabilities-provided
  "Every capability keyword a tool package registers a detector for, read from the
   :capabilities map of its plugin/provide! manifest."
  []
  (into #{}
        (for [file all-clj-files
              node (nodes-of file)
              :when (and (seq? node)
                         (symbol? (first node))
                         (= "provide!" (name (first node)))
                         (map? (second node))
                         (map? (:capabilities (second node))))
              capability (keys (:capabilities (second node)))]
          capability)))

(defn- capabilities-required
  "Every capability keyword a file declares under :cluster-capabilities, whether
   the prerequisites map is inline in the book or a separate def."
  [file]
  (into #{}
        (for [node (nodes-of file)
              :when (and (map? node) (contains? node :cluster-capabilities))
              :let [declared (:cluster-capabilities node)]
              :when (sequential? declared)
              capability declared
              :when (keyword? capability)]
          capability)))

(deftest every-required-capability-is-provided
  ;; A book's :cluster-capabilities is checked at runtime against the cluster's
  ;; env.edn labels; a capability no tool package provides a detector for is a
  ;; typo or a stale name the prerequisite check could never satisfy anywhere.
  (let [provided (capabilities-provided)]
    (doseq [file all-clj-files
            :let [unknown (remove provided (capabilities-required file))]]
      (is (empty? unknown)
          (format "%s requires capabilities no tool package provides: %s" file (vec unknown))))))

;; A *_test.clj file is loaded via the bb.edn `test` task's :requires (so it
;; compiles) but only actually RUNS if its namespace is also in the run-tests
;; call. A ns in :requires but not in that list is silently compiled-but-never-
;; run - the test passes because it never executes. This check keeps the two in
;; step: every test namespace on disk must appear in the run-tests list.

(def ^:private test-files
  (vec (concat (map str (fs/glob "tests" "**_test.clj"))
               (map str (fs/glob "examples" "**_test.clj"))
               (map str (fs/glob "tools" "*/tests/**_test.clj")))))

(def ^:private run-list
  "The namespaces bb.edn's `test` task passes to run-tests (the quoted `-test`
   symbols), which is what actually decides whether a test runs. A tool
   package whose tests need a library the monorepo classpath lacks
   (ui-charm) runs them from its own bb.edn, which CI runs too."
  (into #{} (comp (mapcat #(re-seq #"'([a-zA-Z0-9._-]+-test)" (slurp (str %))))
                  (map (comp symbol second)))
        (cons "bb.edn" (fs/glob "tools" "*/bb.edn"))))

(defn- ns-name-of
  "The namespace a file declares, read straight from its (ns …) form. A regex,
   not a full parse, so a test file using reader syntax edamame rejects (a #'var
   literal, say) does not break this check."
  [file]
  (some-> (re-find #"\(ns\s+([a-zA-Z0-9._-]+)" (slurp file)) second symbol))

(deftest every-test-namespace-is-in-the-run-list
  (doseq [file test-files
          :let [ns-sym (ns-name-of file)]]
    (is (contains? run-list ns-sym)
        (format "%s (%s) is not in bb.edn's test run-tests list -- it compiles but never runs; add '%s to the run-tests call"
                file ns-sym ns-sym))))

(deftest no-file-destructures-namespace-name-map-as-a-vector
  (assert-none (conformance/map-destructured-as-vector
                (parsed (concat all-clj-files (conformance/clj-files ["casebook-example"]))))))

(deftest no-book-or-library-prints-an-outcome-glyph-by-hand
  ;; gumshoe holds itself to what a casebook opts into with :glyphs :require
  (assert-none (conformance/hand-written-glyphs
                (parsed (concat all-clj-files (conformance/clj-files ["casebook-example" "examples"]))))))

(deftest no-file-calls-the-deprecated-split-namespace-name
  (doseq [file (concat all-clj-files (conformance/clj-files ["casebook-example" "tests"]))
          ;; kubectl defines the alias, conformance names it to find misuse,
          ;; and their tests pin both
          :when (not-any? #(str/ends-with? file %) ["libraries/gumshoe/kubectl.clj"
                                                    "libraries/gumshoe/conformance.clj"
                                                    "tests/gumshoe/kubectl_test.clj"
                                                    "tests/gumshoe/conformance_test.clj"
                                                    "tests/gumshoe/conformance_library_test.clj"])]
    (is (not (str/includes? (slurp file) "split-namespace-name"))
        (str file " calls the deprecated split-namespace-name; use namespace-name-map"))))
