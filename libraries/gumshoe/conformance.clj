;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.conformance
  "The rules every book and library follows, checked statically: each file
   parses, carries a license, and documents its namespace; each book enters
   the harness exactly once, describes itself, and documents its options
   without colliding aliases; no file destructures namespace-name-map as a
   vector. gumshoe's own conformance test runs these, and a
   casebook runs the same checks against its own books and libraries from a
   bb task:

     verify {:doc \"Checks the casebook's books and libraries\"
             :requires ([gumshoe.conformance :as conformance])
             :task (conformance/verify! {})}

   Each book is also loaded without running, so a symbol it names that
   resolves to nothing fails the gate instead of the run.

   An advisory names something worth a look that the gate does not fail on:
   a mutation book whose spec declares no :recovery. gumshoe.mutation prints
   generic guidance when such a change fails after applying an effect, so
   the book works without one; verify! prints the advisories as notes and
   still passes.

   Every check returns findings, {:file :check :message}, rather than
   asserting, so a caller decides how to report them. A file that does not
   parse is reported once as :unparseable and skipped by the structural
   checks, which could only misjudge it."
  (:require [babashka.classpath :as classpath]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [edamame.core :as edamame]
            [gumshoe.runbook :as runbook]))

;; --- files ------------------------------------------------------------------

(def default-paths
  "Where a casebook keeps its books and its libraries, as gumshoe does."
  {:books ["runbooks" "playbooks" "firebooks"]
   :libraries ["libraries"]})

(defn clj-files
  "Every .clj file below the roots, sorted. A root that does not exist holds
   none, so a casebook without playbooks needs no special case."
  [roots]
  (vec (sort (for [root roots
                   :when (fs/directory? root)
                   file (fs/glob root "**.clj")]
               (str file)))))

(def ^:private parse-options
  ;; Every reader macro the Clojure reader accepts is enabled, so only a file
  ;; Clojure cannot read reports as unparseable. :auto-resolve accepts
  ;; ::keyword and ::alias/keyword (a file using clojure.spec, say): the checks
  ;; inspect structure only, so any resolution that does not throw will do.
  {:fn true :regex true :deref true :quote true :var true :syntax-quote true
   :read-cond :allow :features #{:clj :bb}
   :auto-resolve (fn [alias] (if (= :current alias) 'current alias))})

(defn parse
  "{:file :forms} for a file that parses, {:file :error <reason>} for one that
   does not."
  [file]
  (try
    {:file file :forms (edamame/parse-string-all (slurp file) parse-options)}
    (catch Exception e
      {:file file :error (ex-message e)})))

(defn forms-of
  "The parsed forms of a file; throws when it does not parse."
  [file]
  (let [{:keys [forms error]} (parse file)]
    (if error
      (throw (ex-info (format "%s does not parse: %s" file error) {:file file}))
      forms)))

;; --- reading a book's structure ---------------------------------------------

(defn ns-form
  [forms]
  (first (filter #(and (seq? %) (= 'ns (first %))) forms)))

(defn- find-def
  [forms name]
  (some #(when (and (seq? %) (= 'def (first %)) (= name (second %)))
           (nth % 2 nil))
        forms))

(def harness-calls
  "The ways a book enters the harness: the raw runbook config, or one of the
   declarative specs that build a runbook config internally."
  #{'runbook/execute! 'mutation/book 'detective/book 'playbook/book 'dispatch/book 'firebook/book})

(defn- harness-call?
  "Whether a form calls the harness, through the usual alias or the full
   namespace name: (runbook/execute! ...), (gumshoe.mutation/book ...)."
  [form]
  (and (seq? form)
       (symbol? (first form))
       (some? (namespace (first form)))
       (contains? harness-calls
                  (symbol (last (str/split (namespace (first form)) #"\."))
                          (name (first form))))))



(defn execute-config
  "The config form a book passes to the harness at its top level."
  [forms]
  (some #(when (harness-call? %) (second %)) forms))

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

(defn- shared-spec?
  "A config written as (merge <shared spec> {...}): a book running a step its
   library defines, naming its own :description beside it."
  [config]
  (and (seq? config) (= 'merge (first config)) (symbol? (second config))))

(defn- literal-config
  [config]
  (cond
    (map? config) config
    (and (seq? config) (= 'merge (first config))) (apply merge (filter map? (rest config)))
    :else nil))

;; --- checks -----------------------------------------------------------------

(defn- finding
  [file check message]
  {:file file :check check :message message})

(defn unparseable
  "A file that does not parse cannot be required or run."
  [parsed]
  (for [{:keys [file error]} parsed
        :when error]
    (finding file :unparseable (str "does not parse: " error))))

(defn- load-error
  "Why a book does not load, with the line it stopped at when the reader or
   analyser says."
  [e]
  (let [{:keys [line column]} (ex-data e)
        message (or (ex-message e) (str e))]
    (if line
      (format "does not load: %s (line %d%s)" message line (if column (str ", column " column) ""))
      (str "does not load: " message))))

(defn unloadable
  "Every book that does not load. Each is loaded in this process with
   runbook/*loading* bound, so the harness hands back the book's config
   instead of running it, and an unresolvable symbol, a missing namespace or
   a failing top-level form is reported with the book named. `:classpath`
   lists directories added to the classpath first, such as a casebook's
   libraries when the caller's classpath lacks them."
  ([books] (unloadable books {}))
  ([books {:keys [classpath]}]
   (when (seq classpath)
     (classpath/add-classpath (str/join java.io.File/pathSeparator (map str classpath))))
   (vec (for [file books
              :let [error (try
                            (binding [runbook/*loading* true]
                              (load-file file))
                            nil
                            (catch Throwable e (load-error e)))]
              :when error]
          (finding file :load error)))))

;; REUSE-IgnoreStart -- the tag every file must carry, not a declaration of this one.
(defn missing-license
  "A file without an SPDX license identifier, or without `license` when given."
  [files license]
  (let [tag (str "SPDX-License-Identifier: " license)]
    (for [file files
          :when (not (str/includes? (slurp file) tag))]
      (finding file :license (if license
                               (str "is missing its SPDX header for " license)
                               "is missing its SPDX header")))))
;; REUSE-IgnoreEnd

(defn undocumented-namespaces
  [parsed]
  (for [{:keys [file forms]} parsed
        :when forms
        :let [form (ns-form forms)
              problem (cond
                        (nil? form) "has no ns form"
                        (not (string? (nth form 2 nil))) "has no namespace docstring")]
        :when problem]
    (finding file :namespace-docstring problem)))

(defn harness-entries
  "A book enters the harness exactly once. Every call in the file counts,
   nested ones too, so a second entry hidden in a helper is found; a mention
   in a docstring or a comment is not a call."
  [parsed-books]
  (for [{:keys [file forms]} parsed-books
        :when forms
        :let [entries (count (filter harness-call? (tree-seq coll? seq (vec forms))))]
        :when (not= 1 entries)]
    (finding file :harness
             (format "enters the harness %d times; a book enters it exactly once (%s)"
                     entries (str/join ", " (sort (map str harness-calls)))))))

(defn undescribed-books
  [parsed-books]
  (for [{:keys [file forms]} parsed-books
        :when forms
        :let [form (execute-config forms)
              config (literal-config form)]
        problem (cond
                  (not (map? config))
                  ["must pass a literal config map, or (merge <spec> {...}), to the harness"]

                  :else
                  (concat
                   (when-not (string? (:description config))
                     ["needs a :description"])
                   ;; a runbook declares :action, a mutation or playbook
                   ;; :select, a detective :detectives or a :scope, a
                   ;; dispatcher :routes, a firebook :manifests or :ignite;
                   ;; a shared spec carries its own
                   (when-not (or (shared-spec? form)
                                 (some #(contains? config %) [:action :select :detectives :scope :routes :manifests :ignite]))
                     ["needs an :action (runbook), :select (mutation, playbook), :detectives/:scope (detective), :routes (dispatcher) or :manifests/:ignite (firebook)"])))]
    (finding file :description problem)))

(defn option-problems
  "Every option carries a :desc and only known keys, and no two options share
   an alias; :h belongs to the harness's --help everywhere."
  [parsed-books]
  (for [{:keys [file forms]} parsed-books
        :when forms
        :let [maps (option-maps (or (find-def forms 'options)
                                    (:options (execute-config forms))))
              aliases (concat [:h]
                              (for [option-map maps
                                    [_ spec] option-map
                                    :when (and (map? spec) (:alias spec))]
                                (:alias spec)))]
        problem (concat
                 (for [option-map maps
                       [option spec] option-map
                       :when (map? spec)
                       problem [(when-not (string? (:desc spec))
                                  (str "option " option " has no :desc"))
                                (when-let [unknown (seq (remove known-option-keys (keys spec)))]
                                  (str "option " option " uses unknown option keys " (vec unknown)))]
                       :when problem]
                   problem)
                 (when (not= (count aliases) (count (distinct aliases)))
                   [(str "has colliding option aliases: " (vec aliases))]))]
    (finding file :options problem)))

;; namespace-name-map and its deprecated alias split-namespace-name return
;; the map {:namespace .. :name ..}. A vector binding of either throws only
;; when the call runs - inside an :effect that is mid-procedure, after earlier
;; effects applied - so the gate finds the binding in the source instead.

(def ^:private map-returning-fns
  #{"namespace-name-map" "split-namespace-name"})

(defn- map-returning-call?
  [form]
  (and (seq? form)
       (symbol? (first form))
       (contains? map-returning-fns (name (first form)))))

(defn map-destructured-as-vector
  "A binding that destructures the map namespace-name-map returns as a
   vector, (let [[ns name] (kubectl/namespace-name-map target)] ..), in a
   let, a for or doseq :let, a loop or any other binding vector."
  [parsed]
  (for [{:keys [file forms]} parsed
        :when forms
        node (tree-seq coll? seq (vec forms))
        :when (vector? node)
        [binding init] (partition 2 node)
        :when (and (vector? binding) (map-returning-call? init))]
    (finding file :destructuring
             (format "binds %s to the vector %s, but it returns the map {:namespace .. :name ..}; destructure it by key"
                     (first init) binding))))

(defn- mutation-book-call?
  [form]
  (and (harness-call? form)
       (= "book" (name (first form)))
       (= "mutation" (last (str/split (namespace (first form)) #"\.")))))

(defn without-recovery
  "Advisory: mutation books whose spec is a literal map without :recovery. A
   spec built by merge or by a library may carry one from elsewhere, which a
   static read cannot see, so only a literal map is judged."
  [parsed-books]
  (for [{:keys [file forms]} parsed-books
        :when forms
        :let [call (some #(when (mutation-book-call? %) %) forms)
              spec (second call)]
        :when (and (map? spec) (not (contains? spec :recovery)))]
    (finding file :recovery
             "declares no :recovery, so a failure after an applied effect prints only the engine's generic guidance; say what the change leaves behind and how to undo it")))

(defn advisories
  "Every advisory for the book files (see without-recovery). None fails the
   gate."
  [{:keys [books]}]
  (vec (without-recovery (keep #(let [parsed (parse %)] (when (:forms parsed) parsed)) books))))

(defn findings
  "Every finding for the book and library files, unparseable ones first.
   `license` names the SPDX identifier every file must carry; without it any
   identifier will do, and false omits the header check for a repository
   that carries no headers, leaving every other check in place. Every book that parses is also loaded (see
   unloadable, which takes :classpath) unless `load?` is false."
  [{:keys [books libraries license load?] :as opts}]
  (let [parsed-books (mapv parse books)
        parsed-libraries (mapv parse libraries)
        parsed (concat parsed-books parsed-libraries)]
    (vec (concat (unparseable parsed)
                 (when-not (false? license)
                   (missing-license (concat books libraries) license))
                 (undocumented-namespaces parsed)
                 (map-destructured-as-vector parsed)
                 (harness-entries parsed-books)
                 (undescribed-books parsed-books)
                 (option-problems parsed-books)
                 (when-not (false? load?)
                   (unloadable (keep #(when (:forms %) (:file %)) parsed-books)
                               (select-keys opts [:classpath])))))))

(defn files-in
  "{:books [...] :libraries [...]}: the .clj files below the roots of :books
   and :libraries, relative to the working directory, default-paths for a
   root not given."
  [{:keys [books libraries]}]
  {:books (clj-files (or books (:books default-paths)))
   :libraries (clj-files (or libraries (:libraries default-paths)))})

(defn- with-library-classpath
  "opts with the library roots on :classpath, so a book loads the casebook's
   libraries even where the caller's classpath lacks them."
  [{:keys [libraries] :as opts}]
  (update opts :classpath #(vec (concat % (or libraries (:libraries default-paths))))))

(defn findings-in
  "findings for the files below the roots (see files-in)."
  [opts]
  (findings (merge (with-library-classpath opts) (files-in opts))))

(defn format-finding
  [{:keys [file check message]}]
  (format "%s: %s (%s)" file message (name check)))

(defn verify!
  "Prints every finding below the roots (see files-in) and throws an ex-info
   carrying :babashka/exit 1 when there is any, so a bb task running it fails
   with the offending files named. Returns the number of files checked
   otherwise. Advisories are printed first, as notes, and never fail it. opts are those of findings-in:

     {:books [\"runbooks\"] :libraries [\"libraries\"] :license \"0BSD\"}

   A casebook that carries no SPDX headers passes :license false."
  [opts]
  (let [{:keys [books libraries] :as files} (files-in opts)
        found (findings (merge (with-library-classpath opts) files))]
    (binding [*out* *err*]
      (doseq [f (advisories files)] (println (str "note: " (format-finding f))))
      (doseq [f found] (println (format-finding f))))
    (when (seq found)
      (throw (ex-info (format "%d conformance finding(s) in %d file(s)"
                              (count found) (count (distinct (map :file found))))
                      {:babashka/exit 1 :findings found})))
    (println (format "conformance: %d book file(s) and %d library file(s) checked, no findings"
                     (count books) (count libraries)))
    (+ (count books) (count libraries))))
