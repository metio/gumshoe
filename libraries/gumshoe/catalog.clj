;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.catalog
  "Discovers the runnable books on disk and reads each one's declared
   description, so a launcher can present every book without a hand-maintained
   list. ./detect fronts the read-only detectives with a guided \"what hurts\"
   flow; ./run uses this to offer any book for a direct, one-shot launch."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [edamame.core :as edamame]
            [gumshoe.config :as config]
            [gumshoe.extensions :as extensions]))

(def ^:private harness-calls
  "The calls a book uses to enter the harness. The config map is their first
   argument and carries the :description."
  #{'runbook/execute! 'mutation/book 'detective/book})

(defn- forms-of
  [file]
  ;; :auto-resolve lets the parser accept ::keyword / ::alias/keyword (a book
  ;; using clojure.spec) - only structure is inspected, so any non-throwing
  ;; resolution is fine.
  (edamame/parse-string-all (slurp file)
                            {:fn true :regex true :deref true :quote true
                             :auto-resolve (fn [alias] (if (= :current alias) 'current alias))}))

(defn- description-of
  "A book's declared :description when it is a plain string literal, else nil. A
   description assembled at runtime (a (format ...) call) can not be read
   statically, so such a book simply falls back to its path."
  [forms]
  (some #(when (and (seq? %) (harness-calls (first %)))
           (let [config (second %)
                 desc (when (map? config) (:description config))]
             (when (string? desc) desc)))
        forms))

(defn short-name
  "The path without its leading root segment or .clj suffix, e.g.
   kubernetes/nodes/cordon - short enough to fuzzy-match by name."
  [path]
  (-> path
      (str/replace #"^[^/]+/" "")
      (str/replace #"\.clj$" "")))

(defn book-at
  "Reads one book file into {:path :name :description}. Never throws - an
   unreadable or unparseable book still appears, labelled by its path, rather
   than vanishing from the launcher."
  [path]
  {:path path
   :name (short-name path)
   :description (try (description-of (forms-of path))
                     (catch Exception _ nil))})

(defn book-roots
  "Where books live: the built-in directories, the book dirs of every cloned
   extension (its gumshoe.edn :book-paths), and any bare :book-paths in env.edn -
   so an extension repo's books are discovered exactly like the built-ins."
  []
  (concat ["runbooks" "playbooks" "firebooks"]
          (extensions/book-paths)
          (config/value [:book-paths] [])))

(defn books
  "Every runnable book under the given roots (default: the built-in directories
   plus configured :book-paths), ordered by path. Each is {:path :name
   :description}. A root that does not exist is skipped, not an error."
  ([] (books (book-roots)))
  ([roots]
   (->> roots
        (filter fs/exists?)
        (mapcat #(map str (fs/glob % "**.clj")))
        (sort)
        (mapv book-at))))
