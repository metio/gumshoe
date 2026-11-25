;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dsl
  "A change written as plain EDN, which compile turns into the canonical
   change map gumshoe.mutation runs (see gumshoe.change). The data holds no
   code: reads, judgements and effects are words of a closed vocabulary,
   and anything beyond it is a registered library function named by a
   keyword (see gumshoe.dsl.registry). Every safeguard is on unless the
   change opts out of it with a reason.

     {:gumshoe/dsl :v0-experimental
      :description \"Suspends a CronJob so no new Job starts\"
      :target {:pick :namespaced :label \"CronJob\" :type \"cronjobs\"
               :namespace-flag :namespace :name-flag :job
               :candidates {:unless :state :order :listed}
               :empty \"every CronJob is suspended already\"}
      :prerequisites {:installed-tools [\"kubectl\" \"fzf\"] :cluster-capabilities []
                      :kubectl-can-get [\"cronjobs\"] :kubectl-can-patch [\"cronjobs\"]}
      :state {:holds [:field [:spec :suspend] := true]
              :says \"CronJob {target} is suspended\" :timeout 30 :interval 3}
      :refuse [{:when [:state] :says \"it is suspended already\"}]
      :confirm {:action \"suspend a CronJob\"}
      :items [:call :gumshoe.workloads/suspend-items [:object] true]
      :announce \"Suspend CronJob {target}\"
      :effect [[:kubectl :patch {:resource \"cronjob\" :merge {:spec {:suspend true}}}]]}

   Keys:
     :gumshoe/dsl    :v0-experimental, the version of this vocabulary;
                     required. The vocabulary is experimental: a later
                     release may change any word of it, and a change
                     declares the marker to say its author knows
     :description    what the book does; required
     :title          optional
     :options        extra or reworded flags, merged over those the target
                     derives (gumshoe.select/options)
     :prerequisites  as a mutation spec declares them; required
     :target         what the change acts on, picked once, see below; required
     :reads          {name read}: live state, read before the confirmation
                     and again after it, where :refuse judges it
     :values         {name expression}: computed once per run, after the reads
     :state          the state the change brings about, see below
     :refuse         [{:when predicate :says \"template\"} or {:reason call}]
     :confirm        {:action \"...\" :destructive? bool :separately? bool}
     :items          [:call <:items function> ..] or [\"template\" ..]
     :announce       \"template\", or false for a change nobody needs to hear of
     :effect         [[:kubectl <verb> {..}] ..], see below; required
     :recovery       \"template\" printed when the change fails partway
     :opt-out        {safeguard \"why\"}, see below
   Any other key of the change, its target, state, confirmation, refusals,
   reads, effects or opt-outs is refused, and so is a symbol, a function or
   any other code anywhere in the data.

   :target
     {:pick :one | :namespaced   :label \"CronJob\"
      :type \"cronjobs\"            ; the resource type of the picked object, or
      :parse :ns/parser           ; a registered :parse function making the ref
      :invalid \"...\"              ; why a pick that does not parse is refused
      :flag :node                 ; :one
      :namespace-flag :namespace :name-flag :job   ; :namespaced
      :preview \"kubectl describe node {}\"          ; :one, optional
      :candidates {:unless :state :order :listed | :sorted}
                  ; or [:call <:candidates function> [:context]]
      :empty \"...\"                ; when nothing can be picked
      :gone \"...\"}                ; the refusal of an object that no longer
                                  ; exists, \"the <label> no longer exists\"
   {:unless :state} lists every object of :type and offers those not in
   the target state yet, in the order the API lists them or sorted.

   Expressions, in :values, :reads, effect arguments and :call arguments:
     [:target]  the pick       [:ref & path]  the parsed pick
     [:object & path]          the picked object, read strictly
     [:opt :name & path]       a flag     [:value :name & path]  a value
     [:read :name & path]      a read     [:context]  the kube context, in
                                          :reads, :values and :candidates
     [:call :ns/name arg ..]   a registered function
     strings, numbers, booleans, nil, keywords, and maps of expressions
   Reads, in :reads and :values:
     [:list \"type\" {:namespace expr}]  {:ok? :items} of a namespaced list
     [:get \"type\" name {:namespace expr}]  one object, nil when NotFound
     [:call <:read function> arg ..]
   Predicates, in :state, :refuse and inside each other:
     [:field path op value]    the picked object's field; op is := :not= :<
                               :<= :> or :>=, and a comparison with a
                               missing number is false
     [:= a b] [:not= a b] [:< a b] ..   over two expressions
     [:some? expr] [:nil? expr] [:not p] [:and p ..] [:or p ..]
     [:call <:predicate function> arg ..]
     [:state]                  the target state holds, in :refuse
     [:absent]                 the picked object does not exist, in :refuse
   Templates are strings whose {target}, {ref.namespace}, {opt.replicas},
   {value.dry-run.message}, {read.hpas.ok?} and {object.spec.schedule}
   placeholders are filled from the run; a placeholder naming anything else
   is refused.

   :state {:holds predicate | :absent :says \"template\" :timeout 60 :interval 5}
   declares once what the change brings about, and compile derives from it:
     the post-check   judges it on the object read again, until :timeout
                      seconds pass; a predicate never holds for an object
                      no read found, and :absent holds only when a read
                      answered NotFound or the object is terminating, so
                      the post-check fails closed
     :done            the run skips a change whose post-check already holds
     the candidates   with :candidates {:unless :state}

   Effects act on the picked object; their arguments are expressions:
     [:kubectl :patch {:merge {..} :resource \"cronjob\"}]   a merge patch;
                                  :resource defaults to the subject's type
     [:kubectl :scale {:replicas expr}]
     [:kubectl :annotate {:annotations {\"key\" expr} :overwrite true}]
                                  a nil value removes the annotation
     [:kubectl :delete]           destructive; does not wait, the post-check does

   Safeguards, each on by default:
     subject pin      the picked object is read strictly before the
                      confirmation, read again after it, and the change is
                      refused when it was replaced, its spec changed or it
                      is gone; a change without a :type or :parse is refused
     strict reads     every read answers or the change stops; there is no
                      opt-out
     post-check       from :state, fail-closed; opt out with :verify
     skip when done   from :state; opt out with :done
     destructive confirmation  a :delete effect makes the confirmation ask
                      for \"yes\"; :destructive? false beside one needs
                      :destructive
     recovery         a destructive change says what it leaves behind and
                      how to undo it; opt out with :recovery
   An opt-out that opts out of nothing is refused, so a stale one cannot
   hide a later change.

   compile throws ex-info carrying ::invalid and the :path of the offending
   form, with a message naming that path, for every key, word, name or
   argument it does not know. The compiled map carries the source as
   (::source (meta m)) and the safeguards it got as (::safeguards (meta m)),
   so a reader of the change can judge the data it was written as."
  (:refer-clojure :exclude [compile])
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.change :as change]
            [gumshoe.dsl.library]
            [gumshoe.dsl.registry :as registry]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]
            [gumshoe.verify :as verify]))

(def version
  "The version of the vocabulary this namespace compiles. The vocabulary is
   experimental and may change in any release, and the marker says so
   wherever a change is written."
  :v0-experimental)

;; --- refusing ----------------------------------------------------------------------

(defn- invalid!
  [path message & {:as data}]
  (throw (ex-info (str (if (seq path) (pr-str path) "the change") ": " message)
                  (merge data {::invalid true :path path :reason message}))))

(defn invalid?
  "Whether e is compile refusing a change."
  [e]
  (boolean (::invalid (ex-data e))))

(defn- known-words
  [ks]
  (str/join " " (map pr-str (sort ks))))

(defn- check-keys!
  "Refuses a map at path that is not a map or holds a key outside allowed."
  [path m allowed what]
  (when-not (map? m)
    (invalid! path (format "%s is a map, not %s" what (pr-str m))))
  (doseq [k (keys m)
          :when (not (contains? allowed k))]
    (invalid! (conj path k) (format "%s is not a key of %s - the keys are %s" (pr-str k) what (known-words allowed)))))

(defn- check-no-code!
  "Refuses a symbol, function, var or any other value EDN cannot carry
   anywhere in x."
  [path x]
  (cond
    (map? x) (doseq [[k v] x] (check-no-code! (conj path k) k) (check-no-code! (conj path k) v))
    (or (vector? x) (seq? x) (set? x))
    (if (or (seq? x) (set? x))
      (invalid! path (format "a change holds vectors and maps, not %s" (pr-str x)))
      (doseq [[i v] (map-indexed vector x)] (check-no-code! (conj path i) v)))
    (or (nil? x) (string? x) (keyword? x) (boolean? x) (integer? x) (double? x)) nil
    (symbol? x) (invalid! path (format "%s is a symbol - a change holds no code; name a registered function as [:call :ns/name ..]" x))
    :else (invalid! path (format "%s is code or a value EDN cannot carry - a change holds no code; name a registered function as [:call :ns/name ..]"
                                 (pr-str x)))))

(defn- non-blank!
  [path x what]
  (when-not (and (string? x) (not (str/blank? x)))
    (invalid! path (format "%s is a non-blank string, not %s" what (pr-str x))))
  x)

;; --- places --------------------------------------------------------------------------
;;
;; A place is where an expression stands: what it may read and which kinds of
;; registered function it may call.

(def ^:private pure-roots #{:target :ref :object :opt :value :read :call})
(def ^:private read-forms #{:list :get})

(defn- place
  [scope what roots calls]
  (assoc scope :what what :roots roots :calls calls))

;; --- expressions ---------------------------------------------------------------------

(declare expr)

(defn- path-step!
  [path step]
  (when-not (or (keyword? step) (string? step) (nat-int? step))
    (invalid! path (format "%s is no step of a path - write a keyword, a string or an index" (pr-str step))))
  step)

(defn- registered!
  "The entry name stands for, of one of kinds, called with n arguments."
  [path name kinds n]
  (let [{:keys [kind args] :as entry} (registry/entry name)]
    (cond
      (not (qualified-keyword? name))
      (invalid! path (format "a registered function is named by a qualified keyword, not %s" (pr-str name)))
      (nil? entry)
      (invalid! path (format "%s is not a registered function - register it with gumshoe.dsl.registry/register! or name one of %s"
                             name (known-words (keys (registry/entries)))))
      (not (kinds kind))
      (invalid! path (format "%s is a %s function, and this place takes %s" name kind
                             (str/join " or " (map pr-str (sort kinds)))))
      (not= n (count args))
      (invalid! path (format "%s takes %d argument(s) (%s), not %d" name (count args) (str/join ", " args) n))
      :else entry)))

(defn- call-of
  "A [:call name arg ..] form as (fn [env] -> value), name of one of kinds."
  [path [_ name & args] place kinds]
  (let [{f :fn} (registered! (conj path 1) name kinds (count args))
        args (vec (map-indexed (fn [i a] (expr (conj path (+ 2 i)) a place)) args))]
    (fn [env] (apply f (map #(% env) args)))))

(defn- named!
  [path root k names]
  (when-not (contains? names k)
    (invalid! path (format "[%s %s] names no declared %s - the declared names are %s"
                           root (pr-str k) (if (= :read root) ":reads entry" (if (= :opt root) "option" ":values entry"))
                           (if (seq names) (known-words names) "none")))))

(defn- getter
  "A path form [root & steps] as (fn [env] -> value)."
  [path [root & steps :as form] {:keys [reads values options] :as place}]
  (let [steps (vec (map-indexed (fn [i s] (path-step! (conj path (inc i)) s)) steps))
        need-name (fn [names]
                    (when (empty? steps)
                      (invalid! path (format "%s names what it reads, as [%s :name]" (pr-str form) root)))
                    (named! (conj path 1) root (first steps) names))]
    (case root
      :target (do (when (seq steps) (invalid! path "[:target] is the pick and takes no path"))
                  (fn [env] (get-in env [:ctx :target])))
      :context (do (when (seq steps) (invalid! path "[:context] takes no path"))
                   (fn [env] (get-in env [:ctx :context])))
      :ref (fn [env] (get-in (get-in env [:ctx :ref]) steps))
      :object (fn [env] (get-in (:object env) steps))
      :opt (do (need-name options) (fn [env] (get-in (get-in env [:ctx :opts]) steps)))
      :value (do (need-name values) (fn [env] (get-in (:ctx env) steps)))
      :read (do (need-name reads) (fn [env] (get-in (:ctx env) steps))))))

(defn- namespace-arg
  [path opts place]
  (when (some? opts)
    (check-keys! path opts #{:namespace} "a read's options")
    (expr (conj path :namespace) (:namespace opts) place)))

(defn- read-of
  [path [op & args :as form] place]
  (case op
    :list (let [[type opts] args]
            (when-not (<= 1 (count args) 2) (invalid! path "[:list \"type\" {:namespace expr}] takes a type and its options"))
            (non-blank! (conj path 1) type "a read's type")
            (let [ns (or (namespace-arg (conj path 2) opts place)
                         (invalid! (conj path 2) "[:list ..] reads one namespace - name it as {:namespace expr}"))]
              (fn [env] (kubectl/get-namespaced-result (get-in env [:ctx :context]) (ns env) type))))
    :get (let [[type name opts] args]
           (when-not (<= 2 (count args) 3) (invalid! path "[:get \"type\" name {:namespace expr}] takes a type, a name and its options"))
           (non-blank! (conj path 1) type "a read's type")
           (let [name (expr (conj path 2) name place)
                 ns (namespace-arg (conj path 3) opts place)]
             (fn [env]
               (let [context (get-in env [:ctx :context])]
                 (if ns
                   (kubectl/get-namespaced-resource context (ns env) type (name env))
                   (kubectl/get-cluster-resource context type (name env)))))))
    (invalid! path (format "%s is no read" (pr-str form)))))

(defn- expr
  "x at path, an expression standing in place, as (fn [env] -> value)."
  [path x {:keys [roots calls what] :as place}]
  (cond
    (map? x) (let [entries (mapv (fn [[k v]]
                                   (when-not (or (keyword? k) (string? k))
                                     (invalid! (conj path k) (format "a map in a change is keyed by keywords or strings, not %s" (pr-str k))))
                                   [k (expr (conj path k) v place)])
                                 x)]
               (fn [env] (into {} (map (fn [[k f]] [k (f env)])) entries)))
    (vector? x)
    (let [op (first x)]
      (cond
        (not (keyword? op))
        (invalid! path (format "%s is a form, whose first element names what it is - a list of values is written as data only inside a map"
                               (pr-str x)))
        (not (contains? (into pure-roots (conj read-forms :context)) op))
        (invalid! (conj path 0) (format "%s is no expression - the expressions are %s" op
                                        (known-words (into pure-roots (conj read-forms :context)))))
        (not (contains? roots op))
        (invalid! (conj path 0) (format "%s is not available in %s" (pr-str x) what))
        (= :call op) (call-of path x place calls)
        (read-forms op) (read-of path x place)
        :else (getter path x place)))
    (or (nil? x) (string? x) (keyword? x) (boolean? x) (number? x)) (constantly x)
    :else (invalid! path (format "%s is no expression" (pr-str x)))))

;; --- templates -----------------------------------------------------------------------

(def ^:private placeholder #"\{([^{}]*)\}")

(defn- template
  "A string at path whose placeholders read place, as (fn [env] -> string).
   A placeholder renders as format's %s renders its value."
  [path s place]
  (non-blank! path s "a template")
  (let [parts (loop [s s acc []]
                (if-let [m (re-find (re-matcher placeholder s))]
                  (let [i (str/index-of s (first m))
                        [root & steps] (str/split (second m) #"\.")
                        root (keyword root)]
                    (when (str/blank? (second m))
                      (invalid! path (format "%s holds an empty placeholder {}" (pr-str s))))
                    (when-not (#{:target :ref :object :opt :value :read} root)
                      (invalid! path (format "{%s} names nothing a template reads - write {target}, {ref.x}, {object.x}, {opt.x}, {value.x} or {read.x}"
                                             (second m))))
                    (recur (subs s (+ i (count (first m))))
                           (conj acc (subs s 0 i)
                                 (try (expr path (into [root] (map keyword steps)) place)
                                      (catch clojure.lang.ExceptionInfo e
                                        (if (invalid? e)
                                          (invalid! path (format "{%s}: %s" (second m) (:reason (ex-data e))))
                                          (throw e)))))))
                  (conj acc s)))]
    (fn [env] (apply str (map #(if (string? %) % (format "%s" (% env))) parts)))))

;; --- predicates ----------------------------------------------------------------------

(def ^:private comparisons
  {:= = :not= not=
   :< #(and (number? %1) (number? %2) (< %1 %2))
   :<= #(and (number? %1) (number? %2) (<= %1 %2))
   :> #(and (number? %1) (number? %2) (> %1 %2))
   :>= #(and (number? %1) (number? %2) (>= %1 %2))})

(def ^:private predicate-words
  (into #{:field :some? :nil? :not :and :or :call :state :absent} (keys comparisons)))

(defn- predicate
  "x at path, a predicate standing in place, as (fn [env] -> boolean).
   (:state place) is the target state's predicate where [:state] may stand."
  [path x {:keys [what] :as place}]
  (when-not (and (vector? x) (keyword? (first x)))
    (invalid! path (format "%s is no predicate - a predicate is a vector such as [:field [:spec :suspend] := true]" (pr-str x))))
  (let [[op & args] x
        arity! (fn [n shape]
                 (when-not (= n (count args))
                   (invalid! path (format "%s is written %s" op shape))))]
    (cond
      (not (predicate-words op))
      (invalid! (conj path 0) (format "%s is no predicate - the predicates are %s" op (known-words predicate-words)))

      (= :field op)
      (let [_ (arity! 3 "[:field path op value]")
            [steps cmp v] args
            _ (when-not (vector? steps)
                (invalid! (conj path 1) (format "a field is a path vector such as [:spec :replicas], not %s" (pr-str steps))))
            _ (when-not (comparisons cmp)
                (invalid! (conj path 2) (format "%s is no comparison - write one of %s" (pr-str cmp) (known-words (keys comparisons)))))
            get (expr (conj path 1) (into [:object] steps) (update place :roots conj :object))
            v (expr (conj path 3) v place)
            cmp (comparisons cmp)]
        (fn [env] (boolean (cmp (get env) (v env)))))

      (comparisons op)
      (let [_ (arity! 2 (format "[%s a b]" op))
            a (expr (conj path 1) (first args) place)
            b (expr (conj path 2) (second args) place)
            cmp (comparisons op)]
        (fn [env] (boolean (cmp (a env) (b env)))))

      (#{:some? :nil?} op)
      (let [_ (arity! 1 (format "[%s expr]" op))
            a (expr (conj path 1) (first args) place)]
        (if (= :some? op) (fn [env] (some? (a env))) (fn [env] (nil? (a env)))))

      (= :not op)
      (let [_ (arity! 1 "[:not predicate]")
            p (predicate (conj path 1) (first args) place)]
        (fn [env] (not (p env))))

      (#{:and :or} op)
      (let [_ (when (empty? args) (invalid! path (format "%s joins one or more predicates" op)))
            ps (vec (map-indexed (fn [i p] (predicate (conj path (inc i)) p place)) args))]
        (if (= :and op)
          (fn [env] (every? #(% env) ps))
          (fn [env] (boolean (some #(% env) ps)))))

      (= :call op)
      (let [f (call-of path x place #{:predicate})]
        (fn [env] (boolean (f env))))

      (= :absent op)
      (do (arity! 0 "[:absent]")
          (when-not (:absent? place)
            (invalid! path (format "[:absent] is not available in %s" what)))
          (fn [env] (nil? (:object env))))

      (= :state op)
      (do (arity! 0 "[:state]")
          (or (when-let [state (:state place)] (fn [env] (state env)))
              (invalid! path (format "[:state] is not available in %s" what)))))))

;; --- the target ----------------------------------------------------------------------

(def ^:private target-keys
  {:one #{:pick :label :type :parse :invalid :flag :preview :candidates :empty :gone}
   :namespaced #{:pick :label :type :parse :invalid :namespace-flag :name-flag :candidates :empty :gone}})

(defn- check-target!
  [{:keys [pick label type parse flag namespace-flag name-flag] :as target}]
  (let [path [:target]]
    (when-not (map? target) (invalid! path (format "the target is a map, not %s" (pr-str target))))
    (when-not (contains? target-keys pick)
      (invalid! (conj path :pick) (format "%s is no pick - write :one or :namespaced (a change acts on one picked object)" (pr-str pick))))
    (check-keys! path target (target-keys pick) (str "a " pick " target"))
    (non-blank! (conj path :label) label "the target's :label")
    (when (nil? (or type parse))
      (invalid! path (str "the target names neither the :type of the picked object nor a :parse making a ref of the pick,"
                          " so the change could not pin it, read it again after the confirmation and refuse it when it moved")))
    (when (some? type) (non-blank! (conj path :type) type "the target's :type"))
    (doseq [k [:invalid :preview :empty :gone] :when (contains? target k)]
      (non-blank! (conj path k) (get target k) (str "the target's " k)))
    (case pick
      :one (when-not (keyword? flag)
             (invalid! (conj path :flag) "a :one target names the flag that picks it, as :flag :node"))
      :namespaced (doseq [[k v] [[:namespace-flag namespace-flag] [:name-flag name-flag]]]
                    (when-not (keyword? v)
                      (invalid! (conj path k) (format "a :namespaced target names its %s as a keyword" k)))))
    (when-not (contains? target :candidates)
      (invalid! (conj path :candidates) "the target names its :candidates: {:unless :state} or [:call <:candidates function> [:context]]"))))

(defn- parse-of
  [{:keys [parse]}]
  (when (some? parse)
    (:fn (registered! [:target :parse] parse #{:parse} 1))))

(defn- listed-name
  [pick object]
  (if (= :namespaced pick) (kubectl/namespace-name-of object) (kubectl/name-of object)))

(defn- candidates-of
  "The target's candidates as (fn [context] -> picks). object-state judges a
   listed object alone, or is nil when the state cannot."
  [{:keys [pick type parse candidates]} object-state scope]
  (let [path [:target :candidates]]
    (cond
      (map? candidates)
      (let [{:keys [unless order] :or {order :sorted}} candidates]
        (check-keys! path candidates #{:unless :order} "the target's :candidates")
        (when-not (= :state unless)
          (invalid! (conj path :unless) (format "the candidates are filtered :unless :state, not %s" (pr-str unless))))
        (when-not (#{:listed :sorted} order)
          (invalid! (conj path :order) (format "%s is no order - write :listed (as the API lists them) or :sorted" (pr-str order))))
        (when (nil? type)
          (invalid! path "{:unless :state} lists the objects of the target's :type, and it names none"))
        (when (some? parse)
          (invalid! path "{:unless :state} offers \"<namespace>/<name>\" or \"<name>\", which a :parse may not read - name a :candidates function"))
        (when (nil? object-state)
          (invalid! path "{:unless :state} needs a :state to judge each listed object by"))
        (let [state (object-state)]
          (fn [context]
            (let [kept (remove #(state {:ctx {:context context} :object %})
                               (kubectl/items-of (kubectl/get-all context type)))]
              (if (= :listed order)
                (vec (distinct (map #(listed-name pick %) kept)))
                (vec (map #(listed-name pick %) (sort-by kubectl/namespace-name-of kept))))))))

      (vector? candidates)
      (let [_ (when-not (= :call (first candidates))
                (invalid! path (format "%s is no candidates form - write {:unless :state} or [:call <:candidates function> [:context]]"
                                       (pr-str candidates))))
            f (call-of path candidates (place scope "the target's :candidates" #{:call :context} #{}) #{:candidates})]
        (fn [context] (vec (f {:ctx {:context context}}))))

      :else (invalid! path (format "%s is no candidates form - write {:unless :state} or [:call <:candidates function> [:context]]"
                                   (pr-str candidates))))))

(defn- select-of
  [{:keys [pick label flag namespace-flag name-flag preview empty]}]
  (cond-> {:mode pick :label label}
    flag (assoc :flag flag)
    namespace-flag (assoc :namespace-flag namespace-flag)
    name-flag (assoc :name-flag name-flag)
    preview (assoc :preview preview)
    empty (assoc :empty-message empty)))

;; --- named entries -------------------------------------------------------------------

(defn- check-names!
  [path entries other]
  (when (some? entries)
    (when-not (map? entries)
      (invalid! path (format "%s is a map of name to its form, not %s" (last path) (pr-str entries))))
    (doseq [k (keys entries)]
      (when-not (simple-keyword? k)
        (invalid! (conj path k) (format "an entry is named by an unqualified keyword, not %s" (pr-str k))))
      (when (change/context-keys k)
        (invalid! (conj path k) (format "%s is a name the engine binds itself - name the entry otherwise" k)))
      (when (contains? other k)
        (invalid! (conj path k) (format "%s is both a read and a value - name one of them otherwise" k))))))

;; --- effects -------------------------------------------------------------------------

(def ^:private verbs
  {:patch #{:merge :resource}
   :scale #{:replicas}
   :annotate #{:annotations :overwrite}
   :delete #{}})

(def ^:private destructive-verbs #{:delete})

(defn- namespace-flag
  [{:keys [namespace]}]
  (when namespace [(str "--namespace=" namespace)]))

(defn- annotation-key
  [k]
  (if (keyword? k) (subs (str k) 1) k))

(defn- effect-of
  "An effect form at path as (fn [env] -> effect)."
  [path form place]
  (when-not (and (vector? form) (= :kubectl (first form)))
    (invalid! path (format "%s is no effect - an effect is [:kubectl <verb> {..}] with a verb of %s"
                           (pr-str form) (known-words (keys verbs)))))
  (let [[_ verb opts & more] form
        opts (or opts {})]
    (when (seq more) (invalid! path (format "%s takes a verb and one map of its options" (pr-str form))))
    (when-not (contains? verbs verb)
      (invalid! (conj path 1) (format "%s is no kubectl verb - the verbs are %s" (pr-str verb) (known-words (keys verbs)))))
    (check-keys! (conj path 2) opts (verbs verb) (str "the options of " verb))
    (let [context #(get-in % [:ctx :context])
          ref #(get-in % [:ctx :ref])]
      (case verb
        :patch (let [_ (when-not (map? (:merge opts))
                         (invalid! (conj path 2 :merge) "a :patch names its merge patch as :merge {..}"))
                     body (expr (conj path 2 :merge) (:merge opts) place)
                     resource (when (contains? opts :resource) (non-blank! (conj path 2 :resource) (:resource opts) "a :resource"))]
                 (fn [env]
                   (let [{:keys [type name] :as r} (ref env)]
                     (apply effect/kubectl (context env)
                            (concat (namespace-flag r)
                                    ["patch" (or resource type) name "--type=merge"
                                     (str "--patch=" (json/generate-string (body env)))])))))
        :scale (let [_ (when-not (contains? opts :replicas)
                         (invalid! (conj path 2) "a :scale names the count as :replicas expr"))
                     replicas (expr (conj path 2 :replicas) (:replicas opts) place)]
                 (fn [env]
                   (let [{:keys [type name] :as r} (ref env)]
                     (apply effect/kubectl (context env)
                            (concat (namespace-flag r)
                                    ["scale" (str (str/replace type #"s$" "") "/" name)
                                     (str "--replicas=" (replicas env))])))))
        :annotate (let [annotations (:annotations opts)
                        _ (when-not (and (map? annotations) (seq annotations))
                            (invalid! (conj path 2 :annotations) "an :annotate names one or more :annotations {\"key\" expr}"))
                        _ (when (contains? opts :overwrite)
                            (when-not (boolean? (:overwrite opts))
                              (invalid! (conj path 2 :overwrite) ":overwrite is true or false")))
                        values (into (sorted-map)
                                     (for [[k v] annotations]
                                       (do (when-not (or (string? k) (keyword? k))
                                             (invalid! (conj path 2 :annotations k) "an annotation is keyed by a string or a keyword"))
                                           [(annotation-key k) (expr (conj path 2 :annotations k) v place)])))]
                    (fn [env]
                      (let [{:keys [type name] :as r} (ref env)]
                        (apply effect/kubectl (context env)
                               (concat (namespace-flag r)
                                       ["annotate" type name]
                                       (for [[k f] values :let [v (f env)]]
                                         (if (nil? v) (str k "-") (str k "=" v)))
                                       (when (:overwrite opts) ["--overwrite"]))))))
        :delete (fn [env]
                  (let [{:keys [type name] :as r} (ref env)]
                    (apply effect/kubectl (context env)
                           (concat (namespace-flag r) ["delete" type name "--wait=false"]))))))))

;; --- the state -----------------------------------------------------------------------

(def ^:private state-keys #{:holds :says :timeout :interval})

(defn- check-state!
  [state]
  (let [path [:state]]
    (check-keys! path state state-keys "the :state")
    (when-not (contains? state :holds)
      (invalid! (conj path :holds) "the :state says what holds once the change took effect, as :holds predicate or :holds :absent"))
    (non-blank! (conj path :says) (:says state) "the :state's :says")
    (doseq [k [:timeout :interval] :when (contains? state k)]
      (when-not (pos-int? (get state k))
        (invalid! (conj path k) (format "%s is a whole number of seconds above zero, not %s" k (pr-str (get state k))))))))

(defn- absent-state?
  [state]
  (= :absent (:holds state)))

(defn- terminating-or-gone?
  [{:keys [context ref] :as ctx}]
  (or (some-> (mutation/read-subject ctx) :metadata :deletionTimestamp some?)
      (kubectl/confirmed-gone? context (:namespace ref) (:type ref) (:name ref))))

(defn- post-checks-of
  "The :verify of a :state: one post-check judging it on the object read
   again."
  [state holds says]
  (let [{:keys [timeout interval] :or {timeout 60 interval 5}} state]
    (if (absent-state? state)
      (fn [ctx]
        [{:description (says {:ctx ctx :object (:object ctx)}) :timeout timeout :interval interval
          :verifies-absence? true
          :check (fn [] (terminating-or-gone? ctx))}])
      (fn [ctx]
        [{:description (says {:ctx ctx :object (:object ctx)}) :timeout timeout :interval interval
          :check (fn [] (verify/object-holds? (mutation/read-subject ctx) #(holds {:ctx ctx :object %})))}]))))

(defn- state-judged
  "The target state as [:state] judges it in a refusal, (fn [env] -> bool)
   over the object read: a predicate holds only for an object that exists,
   and :absent holds for an object that is gone or terminating."
  [state state-pred]
  (cond
    (nil? state) nil
    (absent-state? state) (fn [env] (let [object (:object env)]
                                      (or (nil? object) (some? (-> object :metadata :deletionTimestamp)))))
    :else (fn [env] (verify/object-holds? (:object env) #(state-pred (assoc env :object %))))))

;; --- opt-outs ------------------------------------------------------------------------

(def safeguards
  "The safeguards a change may opt out of, each with a reason."
  #{:verify :done :recovery :destructive})

(defn- check-opt-out!
  [opt-out]
  (let [path [:opt-out]]
    (check-keys! path opt-out safeguards "the :opt-out")
    (doseq [[k reason] opt-out]
      (non-blank! (conj path k) reason (str "the reason for opting out of " k)))))

;; --- compile -------------------------------------------------------------------------

(def ^:private top-keys
  #{:gumshoe/dsl :description :title :options :prerequisites :target :reads :values :state :refuse
    :confirm :items :announce :effect :recovery :opt-out})

(defn- refusal-of
  "The :refuse entries and the target's gone refusal as one :refusal."
  [refuse target scope state-pred]
  (let [path [:refuse]
        refusal-place (assoc (place scope "a :refuse entry" pure-roots #{:value}) :state state-pred :absent? true)
        _ (when-not (or (nil? refuse) (vector? refuse))
            (invalid! path (format "the refusals are a vector of {:when predicate :says \"template\"} or {:reason call}, not %s" (pr-str refuse))))
        entries (vec (map-indexed
                      (fn [i entry]
                        (let [path (conj path i)]
                          (check-keys! path entry #{:when :says :reason} "a :refuse entry")
                          (cond
                            (contains? entry :reason)
                            (do (when (or (contains? entry :when) (contains? entry :says))
                                  (invalid! path "a :refuse entry is {:when predicate :says \"template\"} or {:reason call}, not both"))
                                (when-not (and (vector? (:reason entry)) (= :call (first (:reason entry))))
                                  (invalid! (conj path :reason) "a :reason is [:call <:reason function> arg ..]"))
                                (call-of (conj path :reason) (:reason entry) refusal-place #{:reason}))

                            (and (contains? entry :when) (contains? entry :says))
                            (let [p (predicate (conj path :when) (:when entry) refusal-place)
                                  says (template (conj path :says) (:says entry) refusal-place)]
                              (fn [env] (when (p env) (says env))))

                            :else (invalid! path "a :refuse entry is {:when predicate :says \"template\"} or {:reason call}"))))
                      refuse))
        gone (or (:gone target) (str "the " (:label target) " no longer exists"))
        refusals (conj entries (fn [env] (when (nil? (:object env)) gone)))]
    (fn [ctx]
      (let [env {:ctx ctx :object (:object ctx)}]
        (some #(% env) refusals)))))

(defn- items-of
  [items scope]
  (let [path [:items]
        place (place scope ":items" pure-roots #{:value :items})]
    (cond
      (and (vector? items) (= :call (first items)))
      (let [f (call-of path items place #{:items})]
        (fn [ctx] (vec (f {:ctx ctx :object (:object ctx)}))))

      (and (vector? items) (seq items) (every? string? items))
      (let [lines (vec (map-indexed (fn [i s] (template (conj path i) s place)) items))]
        (fn [ctx] (let [env {:ctx ctx :object (:object ctx)}] (mapv #(% env) lines))))

      :else (invalid! path (format ":items is [:call <:items function> arg ..] or a vector of templates, not %s" (pr-str items))))))

(defn- ctx-template
  [path s place]
  (let [t (template path s place)]
    (fn [ctx] (t {:ctx ctx :object (:object ctx)}))))

(defn- compiled
  [{:keys [description title options prerequisites target reads values state refuse confirm items announce effect recovery opt-out]
    :as dsl}]
  (when-not (map? dsl)
    (invalid! [] (format "a change is a map, not %s" (pr-str dsl))))
  ;; the marker is judged first, so a change written for another version
  ;; is refused for that alone, before any word it uses
  (cond
    (not (contains? dsl :gumshoe/dsl))
    (invalid! [:gumshoe/dsl] (format "the change declares no version - declare :gumshoe/dsl %s, the experimental vocabulary, whose format may change in any release"
                                     version))

    (not= version (:gumshoe/dsl dsl))
    (invalid! [:gumshoe/dsl] (format "the change declares :gumshoe/dsl %s, and this compiles %s - declare :gumshoe/dsl %s, the experimental vocabulary, whose format may change in any release"
                                     (pr-str (:gumshoe/dsl dsl)) version version)))
  (check-no-code! [] dsl)
  (check-keys! [] dsl top-keys "a change")
  (non-blank! [:description] description "the :description")
  (when (contains? dsl :title) (non-blank! [:title] title "the :title"))
  (when-not (map? prerequisites)
    (invalid! [:prerequisites] (format "the change declares its :prerequisites as a map, not %s" (pr-str prerequisites))))
  (check-target! target)
  (when (some? opt-out) (check-opt-out! opt-out))
  (check-keys! [:confirm] confirm #{:action :destructive? :separately?} "the :confirm")
  (non-blank! [:confirm :action] (:action confirm) "the confirmation's :action")
  (doseq [k [:destructive? :separately?] :when (contains? confirm k)]
    (when-not (boolean? (get confirm k))
      (invalid! [:confirm k] (format "%s is true or false" k))))
  (when (some? options)
    (when-not (and (map? options) (every? keyword? (keys options)) (every? map? (vals options)))
      (invalid! [:options] "the :options are a map of flag keyword to its option map")))
  (check-names! [:reads] reads (set (keys values)))
  (check-names! [:values] values #{})
  (when-not (and (vector? effect) (seq effect))
    (invalid! [:effect] "the change names one or more effects, as [[:kubectl <verb> {..}] ..]"))
  (let [select (select-of target)
        options (try (select/options select (or options {}))
                     (catch clojure.lang.ExceptionInfo e (invalid! [:options] (ex-message e))))
        scope {:reads (set (keys reads)) :values (set (keys values)) :options (set (keys options))}
        pure (place scope "this place" pure-roots #{:value})
        object-only (place {:reads #{} :values #{} :options #{}}
                           "the target's :candidates {:unless :state}, which judges each listed object alone"
                           #{:object :call} #{:value})
        state-pred (when (and state (not (absent-state? state)))
                     (check-state! state)
                     (predicate [:state :holds] (:holds state) pure))
        _ (when (absent-state? state) (check-state! state))
        object-state (when state
                       (fn []
                         (if (absent-state? state)
                           (fn [env] (some-> (:object env) :metadata :deletionTimestamp some?))
                           (predicate [:state :holds] (:holds state) object-only))))
        candidates (candidates-of target object-state scope)
        parse (parse-of target)
        read-place (place scope "a :reads entry" #{:target :ref :object :opt :context :call :list :get} #{:value :read})
        value-place (place scope "a :values entry" #{:target :ref :object :opt :read :context :call :list :get} #{:value :read})
        compiled-reads (into {} (for [[k form] reads]
                                  (let [f (if (and (vector? form) (read-forms (first form)))
                                            (read-of [:reads k] form read-place)
                                            (if (and (vector? form) (= :call (first form)))
                                              (call-of [:reads k] form read-place #{:read})
                                              (invalid! [:reads k] (format "%s is no read - write [:list ..], [:get ..] or [:call <:read function> ..]"
                                                                          (pr-str form)))))]
                                    [k (fn [ctx] (f {:ctx ctx :object (:object ctx)}))])))
        compiled-values (into {} (for [[k form] values]
                                   (let [f (expr [:values k] form value-place)]
                                     [k (fn [ctx] (f {:ctx ctx :object (:object ctx)}))])))
        destructive-effect? (boolean (some #(destructive-verbs (second %)) (filter vector? effect)))
        effects (vec (map-indexed (fn [i form] (effect-of [:effect i] form pure)) effect))
        destructive? (if (contains? confirm :destructive?) (:destructive? confirm) destructive-effect?)
        used (cond-> #{}
               (nil? state) (conj :verify)
               (some? state) (conj :done)
               (and destructive? (nil? recovery)) (conj :recovery)
               (and destructive-effect? (false? destructive?)) (conj :destructive))]
    (when (and (nil? state) (not (contains? opt-out :verify)))
      (invalid! [:state] (str "the change declares no target :state, so it can neither verify itself nor skip a run that is already done"
                              " - declare :state, or :opt-out {:verify \"why\"}")))
    (when (and destructive-effect? (false? destructive?) (not (contains? opt-out :destructive)))
      (invalid! [:confirm :destructive?] (str "the change deletes, and says it is not destructive, so it would be confirmed with a bare keypress"
                                              " - drop :destructive? false, or :opt-out {:destructive \"why\"}")))
    (when (and destructive? (nil? recovery) (not (contains? opt-out :recovery)))
      (invalid! [:recovery] (str "the change is destructive and says nothing of what a failure leaves behind"
                                 " - write :recovery \"what is left and how to undo it\", or :opt-out {:recovery \"why\"}")))
    (doseq [k (keys opt-out) :when (not (used k))]
      (invalid! [:opt-out k] (format "the change opts out of %s, which it does not get%s - drop it"
                                     k (case k
                                         :verify " (it declares a :state, which is its post-check)"
                                         :done " (it declares no :state to skip by)"
                                         :recovery (if destructive? " (it declares a :recovery)" " (it is not destructive)")
                                         :destructive " (it is destructive, or deletes nothing)"))))
    (let [subject (cond-> {:from :pick}
                    (:type target) (assoc :type (:type target))
                    parse (assoc :parse parse)
                    (:invalid target) (assoc :invalid (:invalid target)))
          says (when state (template [:state :says] (:says state) pure))
          spec (cond-> {:description description
                        :options options
                        :prerequisites prerequisites
                        :select (assoc select :candidates candidates)
                        :subjects [subject]
                        :refusal (refusal-of refuse target scope (state-judged state state-pred))
                        :confirm (cond-> {:action (:action confirm) :destructive? destructive?}
                                   (contains? confirm :separately?) (assoc :separately? (:separately? confirm)))
                        :effect (fn [ctx] (let [env {:ctx ctx :object (:object ctx)}]
                                            (apply effect/plan (map #(% env) effects))))}
                 title (assoc :title title)
                 (seq compiled-reads) (assoc :reads compiled-reads)
                 (seq compiled-values) (assoc :values compiled-values)
                 (some? items) (assoc :items (items-of items scope))
                 (string? announce) (assoc :announce (ctx-template [:announce] announce pure))
                 (false? announce) (assoc :announce false)
                 state (assoc :verify (post-checks-of state (or state-pred (constantly true)) says))
                 (and state (not (contains? opt-out :done))) (assoc :done true)
                 (nil? state) (assoc :no-verify (:verify opt-out))
                 (some? recovery) (assoc :recovery (ctx-template [:recovery] recovery pure)))]
      (when-not (or (nil? announce) (false? announce) (string? announce))
        (invalid! [:announce] (format ":announce is a template or false, not %s" (pr-str announce))))
      {:spec spec
       :safeguards {:subject-pin :on
                    :strict-reads :on
                    :post-check (if state :on {:off (:verify opt-out)})
                    :done (cond (nil? state) {:off (:verify opt-out)}
                                (contains? opt-out :done) {:off (:done opt-out)}
                                :else :on)
                    :destructive-confirmation (cond destructive? :on
                                                    destructive-effect? {:off (:destructive opt-out)}
                                                    :else :not-destructive)
                    :recovery (cond (not destructive?) :not-destructive
                                    recovery :on
                                    :else {:off (:recovery opt-out)})}})))

(defn compile
  "The canonical change map of a DSL change (see the namespace doc), as
   gumshoe.change/normalize accepts it, with the source and its safeguards
   in its metadata. Pure: it reads no cluster, and the same data compiles
   to a change that behaves the same. Throws ex-info carrying ::invalid
   and :path for a change it refuses."
  [dsl]
  (let [{:keys [spec safeguards]} (compiled dsl)]
    (vary-meta (change/normalize spec) assoc ::source dsl ::safeguards safeguards)))

(defn safeguards-of
  "The safeguards a compiled change got, each :on, :not-destructive, or
   {:off \"why\"}."
  [compiled]
  (::safeguards (meta compiled)))

(defn source-of
  "The DSL data a compiled change was compiled from."
  [compiled]
  (::source (meta compiled)))
