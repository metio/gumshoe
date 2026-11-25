;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dsl.registry
  "The functions a gumshoe.dsl change may call, each registered once under a
   qualified keyword with what it is and what it takes. A DSL change holds
   no code: it names a function as [:call :ns/name arg..], and compile
   resolves the name here, so only a function somebody registered - and
   reviewed when they did - can run, and a name that is not registered is
   refused when the change compiles.

   An entry:
     {:fn   #'ns/f                  ; called with the evaluated arguments; a var
                                    ; keeps the entry when its namespace reloads
      :kind :predicate              ; what it returns and where it may stand
      :args [\"workload\" \"replicas\"]   ; one name per argument, for errors and docs
      :doc  \"...\"}

   Kinds, and where a change may call them:
     :parse       (pick) -> ref or nil              :target :parse
     :candidates  (context) -> picks, reads         :target :candidates
     :read        (args..) -> value, reads          :reads and :values
     :value       (args..) -> value                 any expression
     :predicate   (args..) -> boolean               any predicate
     :reason      (args..) -> reason or nil         a :refuse entry's :reason
     :items       (args..) -> confirmation lines    :items
   A function that reads the cluster is a :read or :candidates function, so
   compile can keep reads out of refusals, post-checks and plans, which
   judge only what a declared read returned.

   gumshoe.dsl.library registers gumshoe's own functions; a casebook
   registers its own with register! before it compiles a change that names
   them.")

(def kinds
  "Every kind of registered function."
  #{:parse :candidates :read :value :predicate :reason :items})

(defonce ^:private registry (atom {}))

(defn- refuse!
  [message data]
  (throw (ex-info message (assoc data ::invalid true))))

(defn register!
  "Registers f under the qualified keyword name, as a function of kind
   taking the arguments args names (see the namespace doc). Registering a
   name again with another function, kind or arity is refused, so two
   libraries cannot silently replace each other's function; registering
   the same entry again is a no-op, so a namespace may be reloaded."
  [name {f :fn :keys [kind args doc] :as entry}]
  (when-not (qualified-keyword? name)
    (refuse! (format "a registered function is named by a qualified keyword, not %s" (pr-str name)) {:name name}))
  (when-not (ifn? f)
    (refuse! (format "%s registers no :fn" name) {:name name}))
  (when-not (kinds kind)
    (refuse! (format "%s has the kind %s - write one of %s" name (pr-str kind) (pr-str (sort kinds))) {:name name}))
  (when-not (and (vector? args) (every? string? args))
    (refuse! (format "%s names its arguments as a vector of strings, not %s" name (pr-str args)) {:name name}))
  (when-not (and (string? doc) (seq doc))
    (refuse! (format "%s has no :doc" name) {:name name}))
  (let [entry (select-keys entry [:fn :kind :args :doc])]
    (swap! registry (fn [r]
                      (let [seen (get r name)]
                        (cond
                          (nil? seen) (assoc r name entry)
                          (= (dissoc seen :doc) (dissoc entry :doc)) r
                          :else (refuse! (format "%s is registered already as another function" name)
                                         {:name name})))))
    name))

(defn entry
  "The entry registered under name, or nil."
  [name]
  (get @registry name))

(defn entries
  "Every registered entry, by name."
  []
  @registry)
