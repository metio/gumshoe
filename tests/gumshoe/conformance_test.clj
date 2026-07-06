;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.conformance-test
  "Machine-enforced repository rules. Every book and library follows the
   same contract, and these tests keep it that way - discipline is nice,
   automation does not forget."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [edamame.core :as edamame]))

(defn- clj-files
  [root]
  (mapv str (fs/glob root "**.clj")))

(def ^:private book-files
  (vec (mapcat clj-files ["runbooks" "playbooks" "firebooks"])))

(def ^:private library-files
  (clj-files "libraries"))

(defn- forms-of
  [file]
  ;; :auto-resolve lets the parser accept ::keyword and ::alias/keyword (e.g. a
  ;; file using clojure.spec) - we only inspect structure, so any non-throwing
  ;; resolution is fine.
  (edamame/parse-string-all (slurp file)
                            {:fn true :regex true :deref true :quote true
                             :auto-resolve (fn [alias] (if (= :current alias) 'current alias))}))

(defn- ns-form
  [forms]
  (first (filter #(and (seq? %) (= 'ns (first %))) forms)))

(defn- find-def
  [forms name]
  (some #(when (and (seq? %) (= 'def (first %)) (= name (second %)))
           (nth % 2 nil))
        forms))

(def ^:private harness-calls
  "The ways a book enters the harness: the raw runbook config, the declarative
   mutation spec, or the detective spec (both build a runbook config internally)."
  #{'runbook/execute! 'mutation/book 'detective/book})

(defn- execute-config
  [forms]
  (some #(when (and (seq? %) (harness-calls (first %)))
           (second %))
        forms))

(def ^:private shared-options
  "Options merged in from libraries, by the symbol books use for them."
  {'detective/output-option {:output {:desc "Report format" :alias :o}}})

(defn- option-maps
  [options-form]
  (cond
    (map? options-form) [options-form]
    (and (seq? options-form) (= 'merge (first options-form)))
    (mapcat option-maps (rest options-form))
    (symbol? options-form) [(get shared-options options-form {})]
    :else []))

(def ^:private known-option-keys
  #{:desc :alias :default :require :coerce :validate})

(deftest every-file-carries-the-license
  (doseq [file (concat book-files library-files)]
    ;; REUSE-IgnoreStart -- asserting every book carries the tag, not declaring one here.
    (is (str/includes? (slurp file) "SPDX-License-Identifier: 0BSD")
        (str file " is missing its SPDX header"))))
;; REUSE-IgnoreEnd

(deftest every-namespace-documents-itself
  (doseq [file (concat book-files library-files)]
    (let [form (ns-form (forms-of file))]
      (is (some? form) (str file " has no ns form"))
      (is (string? (nth form 2 nil))
          (str file " has no namespace docstring")))))

(deftest every-book-runs-through-the-harness
  (doseq [file book-files]
    (let [entries (count (re-seq #"runbook/execute!|mutation/book|detective/book" (slurp file)))]
      (is (= 1 entries)
          (str file " must enter the harness exactly once (runbook/execute!, mutation/book, or detective/book)")))))

(deftest every-book-describes-itself
  (doseq [file book-files]
    (let [config (execute-config (forms-of file))]
      (is (map? config) (str file " must pass a literal config map to the harness"))
      (is (string? (:description config))
          (str file " needs a :description"))
      ;; a runbook declares :action, a mutation :select, a detective :detectives
      ;; or a :scope (a registry scope resolved when the book runs)
      (is (some #(contains? config %) [:action :select :detectives :scope])
          (str file " needs an :action (runbook), :select (mutation), or :detectives/:scope (detective)")))))

(deftest every-option-is-documented-and-aliases-never-collide
  (doseq [file book-files]
    (let [forms (forms-of file)
          maps (option-maps (or (find-def forms 'options)
                                (:options (execute-config forms))))]
      (doseq [option-map maps
              [option spec] option-map
              :when (map? spec)]
        (testing (str file " option " option)
          (is (string? (:desc spec))
              (str file " option " option " has no :desc"))
          (is (empty? (remove known-option-keys (keys spec)))
              (str file " option " option " uses unknown option keys"))))
      ;; :h belongs to the harness's --help everywhere
      (let [aliases (concat [:h]
                            (for [option-map maps
                                  [_ spec] option-map
                                  :when (and (map? spec) (:alias spec))]
                              (:alias spec)))]
        (is (= (count aliases) (count (distinct aliases)))
            (str file " has colliding option aliases: " (vec aliases)))))))

;; malformed post-checks are refused at runtime by verify/eventually, which
;; has its own tests - a textual rule here would misjudge checks composed via
;; helpers like firebook/burning-check
