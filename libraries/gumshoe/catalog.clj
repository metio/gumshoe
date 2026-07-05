;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.catalog
  "Discovers the runnable books on disk and reads each one's declared
   description, so a launcher can present every book without a hand-maintained
   list. ./detect fronts the read-only detectives with a guided \"what hurts\"
   flow; ./run uses this to offer any book for a direct, one-shot launch."
  (:require [babashka.classpath :as classpath]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [edamame.core :as edamame]))

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
  "The path below its book-root segment, without the .clj suffix, e.g.
   kubernetes/nodes/cordon - short enough to fuzzy-match by name. Works whether
   the path is relative (runbooks/…) or an absolute classpath entry
   (/…/gitlibs/…/runbooks/…), so a book from a dependency reads the same as a
   local one."
  [path]
  (-> path
      (str/replace #"^.*/(runbooks|playbooks|firebooks)/" "")
      (str/replace #"^(runbooks|playbooks|firebooks)/" "")
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

(def ^:private book-dir-names
  "A classpath entry holds books when its own name is one of these."
  #{"runbooks" "playbooks" "firebooks"})

(defn book-roots
  "Every book directory on the classpath - gumshoe's own plus each dependency's.
   Because git-dep sources land on the classpath (gumshoe's deps.edn puts the
   book dirs on :paths, and so does a casebook's bb.edn), a casebook's own books,
   gumshoe's built-ins, and any tool package's books are all discovered here with
   no extra configuration - tools.deps resolved the graph, this just reads it."
  []
  (->> (classpath/split-classpath (classpath/get-classpath))
       (filter #(and (contains? book-dir-names (fs/file-name %)) (fs/directory? %)))
       (distinct)))

(defn books
  "Every runnable book on the classpath, ordered by path. Each is {:path :name
   :description}."
  ([] (books (book-roots)))
  ([roots]
   (->> roots
        (mapcat #(map str (fs/glob % "**.clj")))
        (distinct)
        (sort)
        (mapv book-at))))
