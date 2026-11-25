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

   Every mutation spec a book or library defines is judged as it evaluates -
   what a book hands mutation/book, playbook/book or dispatch/book, and what
   a library var holds - against the part of its contract that needs no
   cluster: a confirmation action an operator can read, whether it is
   destructive, its :prerequisites, an :effect, and a :verify or a stated
   reason for having none,

     :no-verify \"the session is the result; nothing is left to check\"

   A destructive spec also needs a guard the engine judges again after the
   confirmation - a :refusal, a :precondition, or a subject it reads again
   and pins - or a stated reason for having none,

     :no-guard \"the effect itself refuses a target that moved\"

   Those findings are notes unless the caller passes :guards :require, as
   gumshoe's own gate does.

   A declared :pin is called with a synthetic pick in a namespace of its own
   and must return a ref with a :type and a :name, in the picked namespace
   when the ref has one. A library that cannot be required is reported too,
   since its specs cannot be judged. A casebook that takes a release before
   it has fixed or annotated its specs passes :spec-contract :advise, and
   these findings are printed as notes until it drops the option.

   An advisory names something worth a look that the gate does not fail on:
   a mutation book whose spec declares no :recovery, a destructive spec
   without a guard unless :guards :require, and, under :spec-contract
   :advise, the judgements of the mutation specs. gumshoe.mutation prints
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
            [gumshoe.change :as change]
            [gumshoe.dispatch :as dispatch]
            [gumshoe.mutation :as mutation]
            [gumshoe.playbook :as playbook]
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
    ;; (select/options select {..}): the select's own options are derived
    ;; complete, so only the book's explicit map is read here
    (and (seq? options-form) (= 'select/options (first options-form)))
    (mapcat option-maps (drop 2 options-form))
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

(defn- load-book
  "Loads a book with runbook/*loading* bound, so the harness hands back the
   book's config instead of running it. Returns {:file :error} for a book
   that does not load, else {:file :ns :specs}, where :specs are the
   specs the book handed to mutation/book, playbook/book and dispatch/book,
   each as [builder spec], captured as the builders received them, so a
   spec built with merge, toggle/pair or a library function is seen as it
   runs."
  [file]
  (let [captured (atom [])
        capture (fn [builder original]
                  (fn [spec] (swap! captured conj [builder spec]) (original spec)))
        error (try
                (with-redefs [mutation/book (capture :mutation mutation/book)
                              playbook/book (capture :playbook playbook/book)
                              dispatch/book (capture :dispatch dispatch/book)]
                  (binding [runbook/*loading* true]
                    (load-file file)))
                nil
                (catch Throwable e (load-error e)))]
    (if error
      {:file file :error error}
      {:file file :specs @captured})))

(defn- add-classpath!
  [classpath]
  (when (seq classpath)
    (classpath/add-classpath (str/join java.io.File/pathSeparator (map str classpath)))))

(defn load-books
  "load-book for each of books, after adding the directories `:classpath`
   lists to the classpath."
  [books {:keys [classpath]}]
  (add-classpath! classpath)
  (mapv load-book books))

(defn- load-findings
  [loaded]
  (for [{:keys [file error]} loaded
        :when error]
    (finding file :load error)))

(defn unloadable
  "Every book that does not load. Each is loaded in this process with
   runbook/*loading* bound, so the harness hands back the book's config
   instead of running it, and an unresolvable symbol, a missing namespace or
   a failing top-level form is reported with the book named. `:classpath`
   lists directories added to the classpath first, such as a casebook's
   libraries when the caller's classpath lacks them."
  ([books] (unloadable books {}))
  ([books opts]
   (vec (load-findings (load-books books opts)))))

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

;; --- the contract of a mutation spec ---------------------------------------
;;
;; A spec is data whose callbacks run only when its book runs against a
;; cluster, so an omission in it surfaces mid-change. These checks judge the
;; evaluated spec - what mutation/book, playbook/book or dispatch/book
;; receives, and what a library var holds - so a spec built with merge,
;; toggle/pair or a library function is judged as it runs.

(defn- callable?
  [x]
  (or (fn? x) (var? x)))

(defn- non-blank?
  [x]
  (and (string? x) (not (str/blank? x))))

(defn contract-problems
  "What a mutation spec omits of the part of its contract a gate can judge
   without a cluster, as messages: a confirmation action an operator can read,
   whether it is destructive, its prerequisites, an effect, and a post-check
   or the reason it has none, given as :no-verify \"why\"."
  [{:keys [confirm prerequisites effect verify no-verify] :as spec}]
  (remove nil?
          [(when-not (non-blank? (:action confirm))
             "has no confirmation action an operator can read (:confirm {:action \"...\"})")
           (when-not (boolean? (:destructive? confirm))
             "does not state whether it is destructive (:confirm {:destructive? true or false}), so a change that deletes would be confirmed with a bare keypress")
           (when-not (map? prerequisites)
             "declares no :prerequisites, so a missing permission is found partway through, after earlier effects applied")
           (when-not (callable? effect)
             "declares no :effect")
           (cond
             (and (some? verify) (contains? spec :no-verify))
             "declares a :verify and a :no-verify reason for having none; drop the reason"
             (callable? verify) nil
             (non-blank? no-verify) nil
             (contains? spec :no-verify)
             ":no-verify must be a string saying why the spec has no post-check"
             :else
             "has no post-check (:verify) and states no reason for having none (:no-verify \"why\")")]))

(defn guard-problems
  "What a spec, as gumshoe.change/normalize maps it, lacks to act only on
   the target the operator confirmed, as messages. A destructive spec needs
   a guard the engine judges again after the confirmation: a :refusal (or a
   :precondition, its alias), which judges the state read again then, or a
   subject (:subject, :subjects, :pin), which is read again and refused when
   it was deleted, replaced or changed. A spec with neither states why, as
   :no-guard \"why\". A spec that is not destructive needs no guard."
  [{:keys [confirm refusal subjects no-guard] :as spec}]
  (let [guarded? (or (some? refusal) (boolean (seq subjects)))
        reason? (contains? spec :no-guard)]
    (cond
      (and guarded? reason?)
      ["declares a guard (:refusal, :precondition, :subject, :subjects or :pin) and a :no-guard reason for having none; drop the reason"]

      (and reason? (not (non-blank? no-guard)))
      [":no-guard must be a string saying why the spec judges nothing again after the confirmation"]

      (and (true? (:destructive? confirm)) (not guarded?) (not reason?))
      [(str "is destructive and judges nothing again after the confirmation, so it acts on a target whose state"
            " moved while the operator read the prompt; declare a :refusal over that state, a :subject or"
            " :subjects to pin, or :no-guard \"why\"")]

      :else [])))

(def probe-namespace
  "The namespace of the synthetic pick a declared :pin is called with."
  "gumshoe-probe-namespace")

(def ^:private probe-name "gumshoe-probe-name")

(defn pin-probes
  "The contexts a declared :pin is called with, one per shape a pick in
   probe-namespace may take under select: \"<namespace>/<name>\" for a
   :namespaced select; that or the bare namespace for a :one or :many
   select, whose candidates may be either. Each carries the flags that name
   the pick and, with a :subject, the ref its parser makes of the pick and no
   object, as the engine hands a pin before anything is read."
  [spec {:keys [mode flag namespace-flag name-flag] :as select}]
  (let [subject (change/pick-subject (change/normalize spec))
        qualified (str probe-namespace "/" probe-name)
        picks (case mode
                :namespaced [qualified]
                :many [[qualified] [probe-namespace]]
                [qualified probe-namespace])]
    (for [target picks]
      (merge {:context "gumshoe-probe-context" :cluster "gumshoe-probe-cluster"
              :target target
              :opts (cond-> {:namespace probe-namespace}
                      flag (assoc flag target)
                      namespace-flag (assoc namespace-flag probe-namespace)
                      name-flag (assoc name-flag probe-name))}
             (when (and subject (not= :many mode))
               {:ref (try (mutation/subject-ref subject select target) (catch Exception _ nil))
                :object nil})))))

(defn pin-problems
  "What is wrong with a spec's declared :pin, judged by calling it with
   pin-probes, as messages. It must return a ref carrying a :type and a
   :name; a ref with a namespace must carry the picked one, since a pin that
   ignores it fingerprints another namespace's object, and nothing says so.
   A pin that returns nil for every probe pins nothing until derived values
   it reads exist, which the engine permits, and is not judged."
  [spec select]
  (when-some [pin (change/pin (change/normalize spec))]
    (if-not (callable? pin)
      ["declares a :pin that is not a function"]
      (let [results (for [ctx (pin-probes spec select)]
                      (try {:ref (pin ctx)} (catch Throwable e {:error (or (ex-message e) (str e))})))
            refs (keep :ref results)
            complete (filter #(and (map? %) (non-blank? (:type %)) (non-blank? (:name %))) refs)]
        (cond
          (every? :error results)
          [(format "has a :pin that throws for a pick in namespace %s: %s" probe-namespace (:error (first results)))]

          (and (seq refs) (empty? complete))
          [(format "has a :pin that returns %s for a pick in namespace %s, which is no ref with a :type and a :name"
                   (pr-str (first refs)) probe-namespace)]

          (and (some :namespace complete) (not-any? #(= probe-namespace (:namespace %)) complete))
          [(format "has a :pin that returns namespace %s for a pick in namespace %s, so it would fingerprint another namespace's object"
                   (pr-str (some :namespace complete)) probe-namespace)]

          :else nil)))))

(defn- change-step?
  [x]
  (and (map? x) (contains? x :effect) (not (contains? x :pause))))

(defn specs-in
  "The mutation specs a value holds, as {:name :spec :select}, named after
   `name`: a playbook's change steps, a dispatcher's routes, a spec itself
   (a map with an :effect, as a playbook tells a change step), or the specs
   among a map's values, such as the :on and :off of a toggle/pair. :select
   is what picks the spec's subject: its own, else the enclosing one's."
  [name value select]
  (cond
    (not (map? value)) nil

    (contains? value :steps)
    (let [select (or (:select value) select)]
      (for [[i step] (map-indexed vector (:steps value))
            :when (change-step? step)]
        {:name (str name " step " (inc i) (when (:title step) (str " " (pr-str (:title step)))))
         :spec step
         :select (or (:select step) select)}))

    (contains? value :routes)
    (mapcat (fn [[k route]] (specs-in (str name " route " k) (dispatch/playbook-of route) (or (:select value) select)))
            (sort-by (comp str key) (:routes value)))

    (change-step? value) [{:name name :spec value :select (or (:select value) select)}]

    :else
    (for [[k v] (sort-by (comp str key) value)
          :when (change-step? v)]
      {:name (str name " " k) :spec v :select (or (:select v) select)})))

(defn captured-specs
  "The specs a book handed the harness builders (see load-book), named after
   the book's namespace. A spec handed to mutation/book is one whether or not
   it carries an :effect."
  [book-name captured]
  (mapcat (fn [[builder spec]]
            (if (= :mutation builder)
              (when (map? spec) [{:name (str book-name) :spec spec :select (:select spec)}])
              (specs-in (str book-name) spec nil)))
          captured))

(defn var-specs
  "The specs the vars of a loaded namespace hold, named ns/var, in name
   order. Private vars count: a library builds a playbook's steps from them."
  [ns-sym]
  (when-let [ns (find-ns ns-sym)]
    (mapcat (fn [[var-name v]]
              (let [value (try @v (catch Throwable _ nil))]
                (specs-in (str ns-sym "/" var-name) value nil)))
            (sort-by (comp str key) (ns-interns ns)))))

(defn spec-problems
  "contract-problems and pin-problems of one spec, {:spec :select}, judged
   on the canonical change map, so an alias and its canonical key get the
   same findings. A spec gumshoe.change/normalize refuses has that refusal
   as its one problem."
  [{:keys [spec select]}]
  (let [{:keys [canonical refused]} (try {:canonical (change/normalize spec)}
                                         (catch clojure.lang.ExceptionInfo e
                                           (if (change/invalid? e) {:refused (ex-message e)} (throw e))))]
    (if refused
      [(str "is refused: " refused)]
      (concat (contract-problems canonical) (pin-problems canonical select)))))

(defn spec-guard-problems
  "guard-problems of one spec, {:spec}, judged on the canonical change map.
   A spec gumshoe.change/normalize refuses has none here: spec-problems
   reports the refusal."
  [{:keys [spec]}]
  (let [canonical (try (change/normalize spec)
                       (catch clojure.lang.ExceptionInfo e
                         (if (change/invalid? e) nil (throw e))))]
    (when canonical (guard-problems canonical))))

(defn- contains-spec?
  "Whether spec holds every entry of library-spec as the same value, as
   (merge library-spec {...}) does."
  [spec library-spec]
  (and (not (identical? spec library-spec))
       (every? (fn [[k v]] (identical? v (get spec k ::absent))) library-spec)))

(defn- judged-findings
  "A `check` finding per problem (problems-of item) names, against the file
   that defines the spec. A spec that a book builds by merging a library
   spec inherits that spec's problems, which are reported once, against the
   library."
  [check problems-of specs library-specs]
  (let [library-problems (mapv (fn [l] [(:spec l) (set (problems-of l))]) library-specs)]
    (for [{:keys [file name spec] :as item} specs
          :let [inherited (into #{} (comp (filter #(contains-spec? spec (first %))) (mapcat second))
                                library-problems)]
          problem (problems-of item)
          :when (not (inherited problem))]
      (finding file check (str "mutation spec " name " " problem)))))

(defn spec-findings
  "A finding per problem of each spec (see spec-problems), against the file
   that defines it, from {:file :name :spec :select}. A spec that a book
   builds by merging a library spec inherits that spec's problems, which are
   reported once, against the library."
  ([specs] (spec-findings specs []))
  ([specs library-specs] (judged-findings :spec-contract spec-problems specs library-specs)))

(defn guard-findings
  "A :guard finding per problem of spec-guard-problems for each spec, from
   {:file :name :spec}, inheriting as spec-findings does."
  ([specs] (guard-findings specs []))
  ([specs library-specs] (judged-findings :guard spec-guard-problems specs library-specs)))

(defn- distinct-specs
  "specs without a spec value seen before, so a spec reached through a book
   and through the var it came from is judged once."
  [specs]
  (let [seen (java.util.IdentityHashMap.)]
    (filter (fn [{:keys [spec]}]
              (when-not (.containsKey seen spec)
                (.put seen spec true)
                true))
            specs)))

(defn library-specs
  "The specs the library files define, from the vars of each one's
   namespace, required here, as {:file :name :spec :select}, and a :load
   finding, {:file :finding}, for a library that cannot be required."
  [parsed-libraries]
  (for [{:keys [file forms]} parsed-libraries
        :when forms
        :let [ns-sym (second (ns-form forms))]
        :when (symbol? ns-sym)
        :let [error (try (require ns-sym) nil (catch Throwable e (load-error e)))]
        item (if error
               [{:file file :finding (finding file :load error)}]
               (map #(assoc % :file file) (var-specs ns-sym)))]
    item))

(defn book-specs
  "The specs of the loaded books (see load-book), as {:file :name :spec
   :select}: what each handed the harness builders, else, for a book that
   hands a runbook its config, the specs its namespace's vars hold."
  [loaded parsed-books]
  (let [ns-of (into {} (for [{:keys [file forms]} parsed-books :when forms]
                         [file (second (ns-form forms))]))]
    (for [{:keys [file specs error]} loaded
          :when (not error)
          :let [book-ns (ns-of file)]
          spec (if (seq specs)
                 (captured-specs book-ns specs)
                 (when (symbol? book-ns) (var-specs book-ns)))]
      (assoc spec :file file))))

(defn contract-findings
  "spec-findings and guard-findings for the specs the libraries and loaded
   books define, each judged once, and a :load finding for each library
   that cannot be required."
  [loaded parsed-books parsed-libraries]
  (let [libraries (library-specs parsed-libraries)
        library-specs (remove :finding libraries)
        specs (distinct-specs (concat library-specs (book-specs loaded parsed-books)))]
    (concat (keep :finding libraries)
            (spec-findings specs library-specs)
            (guard-findings specs library-specs))))

(defn advisories
  "Every advisory for the book files (see without-recovery). None fails the
   gate."
  [{:keys [books]}]
  (vec (without-recovery (keep #(let [parsed (parse %)] (when (:forms parsed) parsed)) books))))

(defn report
  "{:findings :advisories} for the book and library files. :findings holds
   every finding, unparseable ones first. `license` names the SPDX identifier
   every file must carry; without it any identifier will do, and false omits
   the header check for a repository that carries no headers, leaving every
   other check in place. Every book that parses is also loaded (see
   unloadable, which takes :classpath) unless `load?` is false, and the
   mutation specs the books and libraries define are judged against their
   contract (see contract-findings). `spec-contract` :advise reports those
   judgements as advisories instead, for a casebook that takes a gumshoe
   release before it has fixed or annotated its specs. The :guard findings
   (see guard-problems) are advisories unless `guards` is :require, so a
   casebook sees a destructive spec without a guard as a note until it opts
   into failing on it. :advisories holds them and every advisory of
   `advisories`."
  [{:keys [books libraries license load? spec-contract guards] :as opts}]
  (let [parsed-books (mapv parse books)
        parsed-libraries (mapv parse libraries)
        parsed (concat parsed-books parsed-libraries)
        loaded (when-not (false? load?)
                 (load-books (keep #(when (:forms %) (:file %)) parsed-books)
                             (select-keys opts [:classpath])))
        contract (when loaded (contract-findings loaded parsed-books parsed-libraries))
        advise? (= :advise spec-contract)
        noted? (fn [{:keys [check]}]
                 (or (and advise? (= :spec-contract check))
                     (and (not= :require guards) (= :guard check))))
        advised (filter noted? contract)]
    {:findings (vec (concat (unparseable parsed)
                            (when-not (false? license)
                              (missing-license (concat books libraries) license))
                            (undocumented-namespaces parsed)
                            (map-destructured-as-vector parsed)
                            (harness-entries parsed-books)
                            (undescribed-books parsed-books)
                            (option-problems parsed-books)
                            (load-findings loaded)
                            (remove noted? contract)))
     :advisories (vec (concat (without-recovery (filter :forms parsed-books)) advised))}))

(defn findings
  "Every finding for the book and library files (see report, which takes the
   same options)."
  [opts]
  (:findings (report opts)))

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

(defn report-in
  "report for the files below the roots (see files-in)."
  [opts]
  (report (merge (with-library-classpath opts) (files-in opts))))

(defn verify!
  "Prints every finding below the roots (see files-in) and throws an ex-info
   carrying :babashka/exit 1 when there is any, so a bb task running it fails
   with the offending files named. Returns the number of files checked
   otherwise. Advisories are printed first, as notes, and never fail it. opts
   are those of report:

     {:books [\"runbooks\"] :libraries [\"libraries\"] :license \"0BSD\"}

   A casebook that carries no SPDX headers passes :license false, one
   whose mutation specs should be noted rather than fail it :spec-contract
   :advise, and one that fails on a destructive spec without a guard
   :guards :require."
  [opts]
  (let [{:keys [books libraries] :as files} (files-in opts)
        {found :findings advised :advisories} (report (merge (with-library-classpath opts) files))]
    (binding [*out* *err*]
      (doseq [f advised] (println (str "note: " (format-finding f))))
      (doseq [f found] (println (format-finding f))))
    (when (seq found)
      (throw (ex-info (format "%d conformance finding(s) in %d file(s)"
                              (count found) (count (distinct (map :file found))))
                      {:babashka/exit 1 :findings found})))
    (println (format "conformance: %d book file(s) and %d library file(s) checked, no findings"
                     (count books) (count libraries)))
    (+ (count books) (count libraries))))
