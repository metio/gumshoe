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
            [edamame.core :as edamame]
            [gumshoe.edn-reader :as edn-reader]))

(def ^:private harness-calls
  "The calls a book uses to enter the harness. The config map is their first
   argument and carries the :description."
  #{'runbook/execute! 'mutation/book 'detective/book 'playbook/book 'dispatch/book 'firebook/book})

(defn- forms-of
  [file]
  ;; :auto-resolve lets the parser accept ::keyword / ::alias/keyword (a book
  ;; using clojure.spec) - only structure is inspected, so any non-throwing
  ;; resolution is fine.
  (edamame/parse-string-all (slurp file)
                            {:fn true :regex true :deref true :quote true
                             :auto-resolve (fn [alias] (if (= :current alias) 'current alias))}))

(defn- literal-description
  "The :description of a config written as a map literal, or as (merge spec
   {:description ...}) - a book that composes a shared spec and declares its own
   description, where the last literal that names one wins as merge would."
  [config]
  (cond
    (map? config) (:description config)
    (and (seq? config) (= 'merge (first config)))
    (last (keep #(when (map? %) (:description %)) (rest config)))
    :else nil))

(defn description-of
  "A book's declared :description when it is a plain string literal, else nil. A
   description assembled at runtime (a (format ...) call) can not be read
   statically, so such a book simply falls back to its path."
  [forms]
  (some #(when (and (seq? %) (harness-calls (first %)))
           (let [desc (literal-description (second %))]
             (when (string? desc) desc)))
        forms))

(defn edn-book?
  "Whether a path names a book written as EDN (see gumshoe.edn-book) rather
   than Clojure."
  [path]
  (str/ends-with? (str path) ".edn"))

(defn short-name
  "The path below its book-root segment, without the .clj or .edn suffix, e.g.
   kubernetes/nodes/cordon - short enough to fuzzy-match by name. Works whether
   the path is relative (runbooks/…) or an absolute classpath entry
   (/…/gitlibs/…/runbooks/…), so a book from a dependency reads the same as a
   local one."
  [path]
  (-> path
      (str/replace #"^.*/(runbooks|playbooks|firebooks)/" "")
      (str/replace #"^(runbooks|playbooks|firebooks)/" "")
      (str/replace #"\.(clj|edn)$" "")))

(defn book-at
  "Reads one book file into {:path :name :description}. Never throws - an
   unreadable or unparseable book still appears, labelled by its path, rather
   than vanishing from the launcher. An EDN book is read as data only, with
   gumshoe.edn-reader's bounds, and nothing in it is compiled here."
  [path]
  {:path path
   :name (short-name path)
   :description (if (edn-book? path)
                  (edn-reader/description-of path)
                  (try (description-of (forms-of path))
                       (catch Exception _ nil)))})

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

(defn casebook-root
  "The directory of the bb.edn babashka was started with, else the working
   directory. A casebook's own books and its relative book paths are read
   against it."
  []
  (str (fs/canonicalize (if-let [config (System/getProperty "babashka.config")]
                          (fs/parent (fs/absolutize config))
                          (fs/cwd)))))

(def ^:private gitlibs-checkout
  "tools.gitlibs checks a git dependency out to <gitlibs>/libs/<group>/<artifact>/<sha>,
   with the full 40-digit sha, so the path itself names the checkout root."
  #"^(.*/libs/[^/]+/[^/]+/[0-9a-f]{40})(?:/|$)")

(defn- under?
  [dir path]
  (or (= dir path) (str/starts-with? path (str dir "/"))))

(defn- git-root
  [dir]
  (some #(when (fs/exists? (fs/path % ".git")) (str %))
        (take-while some? (iterate fs/parent (fs/path dir)))))

(defn- origin-of
  "The directory a book root's books are named relative to: the casebook when
   the root lies inside it, else the gitlibs checkout or git working tree that
   holds it, else the root's parent. A book therefore keeps one name, such as
   runbooks/detectives/fabric.clj or tools/flux/runbooks/gitops.clj, whichever
   checkout or pinned sha it is read from."
  [root casebook]
  (cond
    (under? casebook root) casebook
    :else (or (second (re-find gitlibs-checkout root))
              (git-root root)
              (str (fs/parent root)))))

(defn- first-per-id
  [books]
  (second (reduce (fn [[seen kept] {:keys [id] :as book}]
                    (if (seen id) [seen kept] [(conj seen id) (conj kept book)]))
                  [#{} []]
                  books)))

(defn books
  "Every runnable book on the classpath, ordered by :id: each .clj file below a
   book root, and each .edn file, a book in the experimental DSL format (see
   gumshoe.edn-book). Each is {:path :id :name
   :description :local?}: :path is the file to run, :id its stable name relative
   to its checkout (see origin-of), and :local? whether it is the casebook's own.
   Books sharing an :id appear once, the casebook's own first and otherwise the
   first in classpath order, the same book resolve-book picks for that path."
  ([] (books (book-roots) (casebook-root)))
  ([roots] (books roots (casebook-root)))
  ([roots casebook]
   (let [casebook (str (fs/canonicalize casebook))]
     (->> roots
          (map #(str (fs/canonicalize %)))
          (distinct)
          (map (fn [root] [root (origin-of root casebook)]))
          (sort-by (fn [[_ origin]] (if (= origin casebook) 0 1)))
          (mapcat (fn [[root origin]]
                    (for [file (sort (map str (fs/glob root "**.{clj,edn}")))]
                      {:path file
                       :id (str (fs/relativize origin file))
                       :local? (= origin casebook)})))
          (first-per-id)
          (sort-by :id)
          (mapv (fn [{:keys [path] :as book}] (merge (book-at path) book)))))))

(defn- edit-distance
  [a b]
  (let [b (vec b)]
    (peek (reduce (fn [previous [i ca]]
                    (reduce (fn [row [j cb]]
                              (conj row (min (inc (peek row))
                                             (inc (nth previous (inc j)))
                                             (+ (nth previous j) (if (= ca cb) 0 1)))))
                            [(inc i)]
                            (map-indexed vector b)))
                  (vec (range (inc (count b))))
                  (map-indexed vector a)))))

(defn near-matches
  "Up to five catalog ids close to a path that named no book: those sharing its
   file name, then those within a third of its length in edit distance, closest
   first."
  [path catalog]
  (let [file-name (fs/file-name path)
        limit (max 2 (quot (count path) 3))]
    (->> catalog
         (map :id)
         (map (fn [id] [id (edit-distance path id)]))
         (filter (fn [[id distance]] (or (= file-name (fs/file-name id)) (<= distance limit))))
         (sort-by (fn [[id distance]] [distance id]))
         (map first)
         (take 5)
         (vec))))

(defn resolve-book
  "The book a path names, as {:path <file to run>}, or {:error <message> :near
   [<ids>]} when it names none. An absolute path names itself. A relative path
   names the casebook's own file of that path when one exists, else the catalog
   book listed under that :id, so a line of the listing runs as printed and the
   casebook shadows an upstream book by placing a file at the same path."
  ([path] (resolve-book path (books) (casebook-root)))
  ([path catalog casebook]
   (let [local (fs/path casebook path)]
     (cond
       (fs/absolute? path)
       (if (fs/regular-file? path)
         {:path (str path)}
         {:error (str "no book at " path) :near []})

       (fs/regular-file? local)
       {:path (str (fs/canonicalize local))}

       :else
       (if-let [book (first (filter #(= path (:id %)) catalog))]
         {:path (:path book)}
         {:error (str "no book at " path " in this casebook or on the classpath")
          :near (near-matches path catalog)})))))

(defn run-command
  "The command a reader types to run the book a listed path names. It goes
   through the run door, which resolves the path with resolve-book, so it works
   in this checkout and in a casebook alike; a bare `bb <path>` loads a file
   from the working directory and finds no upstream book from a casebook."
  [path]
  (str "bb run " path))

(defn resolve-path
  "The absolute path of a book named by a book-dir-relative suffix
   (\"tools/flux/runbooks/gitops.clj\", \"runbooks/investigate.clj\") found on the
   classpath - so a front door names a book the same way whether gumshoe runs from
   its own repo (the book is under the working directory) or a casebook that has it
   as a dependency (the book is under ~/.gitlibs). Falls back to the suffix itself
   when nothing matches, so a bare monorepo path still runs."
  ([suffix] (resolve-path suffix (books)))
  ([suffix catalog]
   (or (some #(when (str/ends-with? (:path %) suffix) (:path %)) catalog)
       suffix)))
