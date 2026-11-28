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
     :state          the state the change brings about, one post-check or
                     a vector of them, see below
     :refuse         [{:when predicate :says \"template\"} or {:reason call}]
     :problems       [{:when predicate :says \"template\"} or {:reasons call}],
                     judged as :refuse is, and every message any entry
                     gives is printed as its own refusing line
     :pin            [{:type \"type\" :namespace expr :name expr :compare :spec}
                      ..]: objects other than the subject the change rests
                     on, see below
     :confirm        {:action \"...\" :destructive? bool :separately? bool}
     :items          [:call <:items function> ..] or [\"template\" ..]
     :announce       \"template\", or false for a change nobody needs to hear of
     :effect         [[:kubectl <verb> {..}] ..] on the picked object,
                     [:effect <registered type> arg ..] for an effect type
                     registered with gumshoe.effect/register-effect-type!,
                     and [:each <read> {..} <effect> ..] on every object a
                     read returned, see below; required
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
      :compare :spec              ; what the pin compares on an object without
                                  ; metadata.generation, see below
      :flag :node                 ; :one
      :namespace-flag :namespace :name-flag :job   ; :namespaced
      :preview \"kubectl describe node {}\"          ; :one, optional
      :candidates {:unless :state :order :listed | :sorted}
                  ; or [:call <:candidates function> [:context]]
      :empty \"...\"                ; when nothing can be picked
      :gone \"...\"}                ; the refusal of an object that no longer
                                  ; exists, \"the <label> no longer exists\"
   :compare names what the subject pin compares on an object the API server
   gives no generation (gumshoe.change/comparisons): resourceVersion when
   absent, so every write refuses the change, or :spec for a type that
   splits spec from status, such as \"nodes\", whose status the kubelet
   writes every few seconds.
   {:unless :state} lists every object of :type and offers those not in
   the target state yet, in the order the API lists them or sorted.

   Expressions, in :values, :reads, effect arguments and :call arguments:
     [:target]  the pick       [:ref & path]  the parsed pick
     [:object & path]          the picked object, read strictly
     [:opt :name & path]       a flag     [:value :name & path]  a value
     [:read :name & path]      a read     [:context]  the kube context, in
                                          :reads, :values and :candidates
     [:each & path]            inside an :each, the object it acts on
     [:call :ns/name arg ..]   a registered function
     strings, numbers, booleans, nil, keywords, and maps of expressions
   Reads, in :reads and :values:
     [:list \"type\" {:namespace expr}]  {:ok? :items} of a namespaced list
     [:get \"type\" name {:namespace expr}]  one object, nil when NotFound
     [:present [{:type \"type\" :name expr :namespace expr} ..] {:namespace expr}]
                               the named objects that exist, each as {:ref
                               :object}, read through gumshoe.reads: an
                               object the cluster answered is absent (or whose
                               type it confirmably does not serve) is left
                               out, and a read that did not answer stops the
                               change; an object's :namespace falls back to
                               the shared one
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
   {value.dry-run.message}, {read.hpas.ok?}, {object.spec.schedule} and,
   inside an :each, {each.metadata.name} placeholders are filled from the
   run; a placeholder naming anything else is refused.

   :state {:holds predicate | :absent :says \"template\" :timeout 60 :interval 5}
   declares once what the change brings about, and compile derives from it:
     the post-check   judges it on the object read again, until :timeout
                      seconds pass; a predicate never holds for an object
                      no read found, and :absent holds only when a read
                      answered NotFound or the object is terminating, so
                      the post-check fails closed
     :done            the run skips a change whose post-check already holds
     the candidates   with :candidates {:unless :state}
   :timeout is at most max-timeout seconds and :interval at most
   max-interval, and never longer than the :timeout it waits within.

   :state [{:holds .. :says .. :timeout .. :interval ..} ..] declares several
   post-checks, each judged on its own within its own window, in order, and
   each run even when an earlier one failed. The target state is all of them
   at once: :done, [:state] in a refusal and {:unless :state} hold only when
   every check holds. A check that does not hold fails the run, and the
   banner and the guidance under it name it by its :says, so two checks may
   not say the same. :holds :absent is a state's only check, since an object
   that is gone holds no predicate. A single map is the same as a vector of
   one.

   Effects act on the picked object; their arguments are expressions:
     [:kubectl :patch {:merge {..} :resource \"cronjob\"}]   a merge patch;
                                  :resource defaults to the subject's type
     [:kubectl :scale {:replicas expr}]
     [:kubectl :annotate {:annotations {\"key\" expr} :overwrite true}]
                                  a nil value removes the annotation
     [:kubectl :delete]           destructive; does not wait, the post-check does

   [:effect <op> arg ..] names an effect type a library registered with
   gumshoe.effect/register-effect-type!, such as one writing a file or a
   commit, and hands its :perform the arguments as [op value ..]. The type
   must be registered, and name its :args, by the time the change
   compiles; compile refuses a built-in op, an unregistered type (listing
   the registered ones, gumshoe.dsl.registry/effects) and a wrong count of
   arguments. The arguments are expressions of this place - inside an
   :each also [:each & path], the object acted on - and never reads:
   every value comes from :reads, :values or the pick, computed before the
   confirmation, so what the confirmation shows is what :perform gets.
   The confirmation lists each registered effect as its :describe renders
   it, after the objects, and --dry-run shows the same lines. A type is
   destructive unless its registration says :destructive? false, so it
   asks for the typed yes and a :recovery as a delete does. Its post-check
   is the :state of the change or of its :each, as for a kubectl effect.

   [:each <read> {:state {..} :max n} <effect> ..] applies the effects -
   kubectl or registered - to
   every object a :reads entry returned, in the order read; <read> is
   :name, or [:name ..] for several reads taken together, each object once.
   The read is a [:list ..], [:get ..] (no object, nothing to act on),
   [:present ..], or [:call <:read function> ..] returning gumshoe.reads
   results (an object, a list, or a sequence of them). Inside it the
   effects act on the object, [:each & path] reads it, and [:field ..]
   judges it. :state {:holds predicate | :absent :says \"template\" :timeout
   :interval}, or a vector of them, says what holds for each object once the
   change took effect, as the change's :state does; it is required unless
   every effect is a [:kubectl :delete], where it is :absent. :max
   bounds how many objects the read may return (default-each-max). Per
   object, compile derives:
     the confirmation  lists every object the change acts on
     the skip          an object whose every check holds already is not acted
                       on; a change acting only through :each is done
                       when no object is left, so an empty read is done
     the pin           every object the confirmation showed is fingerprinted
                       (uid and generation, or resourceVersion) and compared
                       with the read made again after the confirmation; the
                       change is refused when one changed or was replaced,
                       or an object appeared that the confirmation did not
                       show; an
                       object that vanished is refused too unless every
                       effect deletes it, since its delete already happened
     the bound         more than :max objects refuses the change before the
                       confirmation
     the post-check    each object read again, judged by every check of
                       its state, fail-closed as :state's is
   A list that did not answer refuses the change: a strict read throws, an
   :unread result throws, and a [:list ..] that answered its type does not
   exist proves no absence, so it refuses as well - read objects of an
   optional type with [:present ..].

   :pin names objects other than the subject the change rests on, such as
   the custom resource whose secret must not be renamed while the operator
   confirms. Each is read strictly before the confirmation and again after
   it, and the change is refused when one does not exist, was replaced, or
   changed (gumshoe.mutation's :subjects). Its expressions see the pick:
   [:target], [:ref ..], [:object ..], [:opt ..] and :value functions.

   Safeguards, each on by default:
     subject pin      the picked object is read strictly before the
                      confirmation, read again after it, and the change is
                      refused when it was replaced, its spec changed or it
                      is gone; a change without a :type or :parse is refused
     strict reads     every read answers or the change stops; there is no
                      opt-out
     post-check       from :state, fail-closed; opt out with :verify. A
                      change whose every effect is an :each needs no :state:
                      each object's state is its post-check, and the
                      safeguards report the subject's as :subject-not-acted-on
     skip when done   from :state and each :each's states; opt out with :done
     each object      an :each's pin, bound and post-check, with no opt-out
     destructive confirmation  a :delete effect or a registered effect
                      type that does not say :destructive? false, also
                      inside an :each, makes the confirmation ask for
                      \"yes\" and marks
                      the compiled :effect destructive
                      (gumshoe.change/destructive);
                      :destructive? false beside one is refused in the
                      words normalize uses
                      (gumshoe.change/destructive-contradiction) unless
                      the change opts out with :destructive, and an
                      opted-out delete carries no mark
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
            [gumshoe.concurrent :as concurrent]
            [gumshoe.dsl.library]
            [gumshoe.dsl.registry :as registry]
            [gumshoe.deletes :as deletes]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.reads :as reads]
            [gumshoe.reads.result :as result]
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
(def ^:private read-forms #{:list :get :present})
(def ^:private expression-words (into pure-roots (conj read-forms :context :each)))

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
      :read (do (need-name reads) (fn [env] (get-in (:ctx env) steps)))
      :each (fn [env] (get-in (:each env) steps)))))

(defn- namespace-arg
  [path opts place]
  (when (some? opts)
    (check-keys! path opts #{:namespace} "a read's options")
    (expr (conj path :namespace) (:namespace opts) place)))

(defn- present-members
  "The objects refs name that exist, each as {:ref <its ref> :object <it>}, in
   the order of refs, read through gumshoe.reads at once. An object the
   cluster answered does not exist (or whose type it confirmably does not
   serve) is left out; a read that did not answer throws the unread
   exception, so an unanswered read is never an absent object."
  [context refs]
  (let [results (concurrent/all (map (fn [ref] #(reads/object context ref)) refs))]
    (vec (for [[ref r] (map vector refs results)
               :let [object (result/object! r)]
               :when object]
           {:ref ref :object object}))))

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
    :present (let [[refs opts] args]
               (when-not (<= 1 (count args) 2)
                 (invalid! path "[:present [{:type \"type\" :name expr} ..] {:namespace expr}] takes the objects and its options"))
               (when-not (and (vector? refs) (seq refs))
                 (invalid! (conj path 1) "[:present ..] names one or more objects, as [{:type \"type\" :name expr :namespace expr} ..]"))
               (let [ns (namespace-arg (conj path 2) opts place)
                     wanted (vec (map-indexed
                                  (fn [i r]
                                    (let [p (conj path 1 i)]
                                      (check-keys! p r #{:type :name :namespace} "an object of [:present ..]")
                                      (non-blank! (conj p :type) (:type r) "an object's :type")
                                      (when-not (contains? r :name)
                                        (invalid! (conj p :name) "an object of [:present ..] names itself as :name expr"))
                                      {:type (:type r)
                                       :name (expr (conj p :name) (:name r) place)
                                       :namespace (when (contains? r :namespace) (expr (conj p :namespace) (:namespace r) place))}))
                                  refs))]
                 (fn [env]
                   (present-members (get-in env [:ctx :context])
                                    (let [shared (when ns (ns env))]
                                      (mapv (fn [{:keys [type] object-name :name object-namespace :namespace}]
                                              (cond-> {:type type :name (object-name env)}
                                                (or object-namespace shared)
                                                (assoc :namespace (if object-namespace (object-namespace env) shared))))
                                            wanted))))))
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
        (not (contains? expression-words op))
        (invalid! (conj path 0) (format "%s is no expression - the expressions are %s" op
                                        (known-words expression-words)))
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
                    (when-not (#{:target :ref :object :opt :value :read :each} root)
                      (invalid! path (format "{%s} names nothing a template reads - write {target}, {ref.x}, {object.x}, {opt.x}, {value.x}, {read.x}, or {each.x} inside an :each"
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
            ;; inside an :each the field is the field of the object acted on
            root (:field-root place :object)
            get (expr (conj path 1) (into [root] steps) (update place :roots conj root))
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
  {:one #{:pick :label :type :parse :invalid :compare :flag :preview :candidates :empty :gone}
   :namespaced #{:pick :label :type :parse :invalid :compare :namespace-flag :name-flag :candidates :empty :gone}})

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
    (when (contains? target :compare)
      (when-let [why (or (when (nil? (:compare target)) "a pin compares nil - drop :compare, or write :compare :spec")
                         (change/compare-problem (:compare target)))]
        (invalid! (conj path :compare) why)))
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

(declare kubectl-effect-of)

(defn- registered-effect-type!
  "The registration of the effect type op names at path, taking n
   arguments: one registered with gumshoe.effect/register-effect-type!
   that names its :args."
  [path op n]
  (let [{:keys [args] :as entry} (effect/effect-type op)
        known (keys (registry/effects))]
    (cond
      (not (keyword? op))
      (invalid! path (format "an effect type is named by a keyword, not %s" (pr-str op)))
      (effect/built-in-op? op)
      (invalid! path (format "%s is built into gumshoe.effect, and a change names no built-in op - write [:kubectl <verb> {..}], or register an effect type" op))
      (nil? entry)
      (invalid! path (format "%s is no registered effect type - register it with gumshoe.effect/register-effect-type! before the change compiles, or name one of %s"
                             op (if (seq known) (known-words known) "none, since nothing registered one with :args")))
      (not (vector? args))
      (invalid! path (format "%s names no :args, so its arguments cannot be counted - register it with :args [\"name\" ..]" op))
      (not= n (count args))
      (invalid! path (format "%s takes %d argument(s) (%s), not %d" op (count args) (str/join ", " args) n))
      :else entry)))

(defn- registered-effect-of
  "A registered effect form [:effect op arg ..] at path as (fn [env] ->
   [op value ..]). Its arguments are expressions over what was read and
   computed before the plan, so the effect gets values: it reads nothing
   through the change, and the confirmation shows the very arguments its
   :perform runs with."
  [path [_ op & args] place]
  (registered-effect-type! (conj path 1) op (count args))
  (let [args (vec (map-indexed (fn [i a] (expr (conj path (+ 2 i)) a place)) args))]
    (fn [env] (into [op] (map #(% env)) args))))

(defn- effect-form?
  [form]
  (and (vector? form) (#{:kubectl :effect} (first form))))

(defn- registered-form?
  [form]
  (and (vector? form) (= :effect (first form))))

(defn- form-verb
  "What an effect form does, as the confirmation names it: the kubectl verb,
   or the registered effect type."
  [form]
  (when (vector? form) (second form)))

(defn- destructive-form?
  "Whether an effect form destroys what it acts on: a kubectl verb of
   destructive-verbs, or a registered effect type that does not say
   :destructive? false - including one nobody registered, which compile
   refuses anyway."
  [form]
  (cond
    (registered-form? form) (:destructive? (effect/effect-type (second form)) true)
    (and (vector? form) (= :kubectl (first form))) (contains? destructive-verbs (second form))
    :else false))

(defn- effect-of
  "An effect form at path as (fn [env] -> effect). A kubectl effect acts
   on the object (ref env) names: the subject's ref, or inside an :each
   the ref of the object acted on. A registered effect acts on what its
   arguments name."
  [path form place ref]
  (when-not (effect-form? form)
    (invalid! path (format "%s is no effect - an effect is [:kubectl <verb> {..}] with a verb of %s, [:effect <registered type> arg ..], or [:each <read> {..} <effect> ..]"
                           (pr-str form) (known-words (keys verbs)))))
  (if (registered-form? form)
    (registered-effect-of path form place)
    (kubectl-effect-of path form place ref)))

(defn- kubectl-effect-of
  [path form place ref]
  (let [[_ verb opts & more] form
        opts (or opts {})]
    (when (seq more) (invalid! path (format "%s takes a verb and one map of its options" (pr-str form))))
    (when-not (contains? verbs verb)
      (invalid! (conj path 1) (format "%s is no kubectl verb - the verbs are %s" (pr-str verb) (known-words (keys verbs)))))
    (check-keys! (conj path 2) opts (verbs verb) (str "the options of " verb))
    (let [context #(get-in % [:ctx :context])]
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
                  (let [{:keys [type namespace name]} (ref env)]
                    (deletes/effect (context env)
                                    (cond-> {:type type :name name} namespace (assoc :namespace namespace))
                                    {:wait false})))))))

;; --- the state -----------------------------------------------------------------------

(def ^:private state-keys #{:holds :says :timeout :interval})

(def max-timeout
  "The longest :timeout a post-check may wait, in seconds. A wait beyond an
   hour is a run nobody watches to its end."
  3600)

(def max-interval
  "The longest :interval between two attempts of a post-check, in seconds."
  300)

(defn- check-state!
  [path state]
  (check-keys! path state state-keys "the :state")
  (when-not (contains? state :holds)
    (invalid! (conj path :holds) "the :state says what holds once the change took effect, as :holds predicate or :holds :absent"))
  (non-blank! (conj path :says) (:says state) "the :state's :says")
  (doseq [k [:timeout :interval] :when (contains? state k)]
    (when-not (pos-int? (get state k))
      (invalid! (conj path k) (format "%s is a whole number of seconds above zero, not %s" k (pr-str (get state k))))))
  (let [{:keys [timeout interval] :or {timeout 60 interval 5}} state]
    (when (< max-timeout timeout)
      (invalid! (conj path :timeout) (format ":timeout is at most %d seconds, not %d" max-timeout timeout)))
    (when (< max-interval interval)
      (invalid! (conj path :interval) (format ":interval is at most %d seconds, not %d" max-interval interval)))
    (when (< timeout interval)
      (invalid! (conj path :interval) (format ":interval %d is longer than the :timeout %d it waits within%s"
                                              interval timeout (if (contains? state :timeout) "" " (the default)"))))))

(defn- absent-state?
  [state]
  (= :absent (:holds state)))

(defn- state-entries
  "The post-checks a :state at path declares, each {:path :state}: a map is
   one, at path itself, so a single state keeps the paths it always had; a
   vector is several, each at its index. Refuses an empty vector, an :absent
   beside anything else (no object that is gone holds a predicate, so the
   post-check could never pass) and two entries saying the same, since the
   :says of a check is what names it when it fails."
  [path state]
  (cond
    (map? state) (do (check-state! path state) [{:path path :state state}])

    (and (vector? state) (seq state))
    (let [entries (vec (map-indexed (fn [i s]
                                      (let [p (conj path i)]
                                        (when-not (map? s)
                                          (invalid! p (format "each post-check of the :state is a map {:holds .. :says ..}, not %s" (pr-str s))))
                                        (check-state! p s)
                                        {:path p :state s}))
                                    state))]
      (when (and (< 1 (count entries)) (some (comp absent-state? :state) entries))
        (let [{p :path} (first (filter (comp absent-state? :state) entries))]
          (invalid! (conj p :holds) "an object that is gone holds no predicate, so :holds :absent is the :state's only post-check")))
      (reduce (fn [seen {p :path s :state}]
                (when (seen (:says s))
                  (invalid! (conj p :says) (format "%s is said by an earlier post-check of the :state - each says what it checks, which names it when it fails"
                                                   (pr-str (:says s)))))
                (conj seen (:says s)))
              #{} entries)
      entries)

    :else (invalid! path (format "the :state is {:holds .. :says ..}, or a vector of them for several post-checks, not %s" (pr-str state)))))

(defn- entries-absent?
  [entries]
  (boolean (some (comp absent-state? :state) entries)))

(defn- conjunction
  "(fn [env] -> bool) holding when every one of preds holds; nil without any."
  [preds]
  (when (seq preds)
    (fn [env] (every? #(% env) preds))))

(defn- terminating-or-gone?
  [{:keys [context ref] :as ctx}]
  (or (some-> (mutation/read-subject ctx) :metadata :deletionTimestamp some?)
      (kubectl/confirmed-gone? context (:namespace ref) (:type ref) (:name ref))))

(defn- post-checks-of
  "The :verify of a :state: one post-check per entry, each judging its own
   predicate on the subject read again, within its own :timeout and
   :interval. checks are {:state :holds :says}, :holds nil for :absent."
  [checks]
  (fn [ctx]
    (mapv (fn [{:keys [state holds says]}]
            (let [{:keys [timeout interval] :or {timeout 60 interval 5}} state
                  description (says {:ctx ctx :object (:object ctx)})]
              (if (absent-state? state)
                {:description description :timeout timeout :interval interval
                 :verifies-absence? true
                 :check (fn [] (terminating-or-gone? ctx))}
                {:description description :timeout timeout :interval interval
                 :check (fn [] (verify/object-holds? (mutation/read-subject ctx) #(holds {:ctx ctx :object %})))})))
          checks)))

(defn- state-judged
  "The target state as [:state] judges it in a refusal, (fn [env] -> bool)
   over the object read: a predicate holds only for an object that exists,
   and :absent holds for an object that is gone or terminating."
  [absent? state-pred]
  (cond
    absent? (fn [env] (let [object (:object env)]
                        (or (nil? object) (some? (-> object :metadata :deletionTimestamp)))))
    (nil? state-pred) nil
    :else (fn [env] (verify/object-holds? (:object env) #(state-pred (assoc env :object %))))))

;; --- effects on each object of a read ------------------------------------------------
;;
;; [:each <read> {:state .. :max n} <kubectl effect> ..] acts on every object a
;; declared :reads entry returned. The objects are the members of the read,
;; each {:ref {:type :namespace :name} :object o}. The confirmed members are
;; kept as a :values entry, computed once before the confirmation, so the
;; refusal after the confirmation compares them with the read made again.

(def default-each-max
  "How many objects an :each may read before the change is refused, unless
   the :each names its own :max. A selector or a list that matches far more
   than its author meant must not act on all of them behind one
   confirmation."
  25)

(defn- kubectl-delete?
  "Whether an effect form is [:kubectl :delete], whose object is gone once
   it took effect."
  [form]
  (and (vector? form) (= :kubectl (first form)) (= :delete (second form))))

(defn each-form?
  "Whether an effect form is an :each."
  [form]
  (and (vector? form) (= :each (first form))))

(defn- each-effect-forms
  "The kubectl effect forms inside an :each form."
  [form]
  (let [more (drop 2 form)]
    (if (map? (first more)) (rest more) more)))

(defn- listed-members
  [type read-name {:keys [ok? items]}]
  (when-not ok?
    ;; under strict reads a list that is not ok? answered that the type does
    ;; not exist, which kubectl also prints when discovery of its group
    ;; failed, so it proves no absence
    (throw (result/unread-exception
            (format "the list of %s in :reads %s answered that the type does not exist, which does not prove the objects are absent - read objects of an optional type with [:present ..]"
                    type read-name))))
  (mapv (fn [o] {:ref (cond-> {:type type :name (kubectl/name-of o)}
                        (kubectl/namespace-of o) (assoc :namespace (kubectl/namespace-of o)))
                 :object o})
        items))

(defn- result-members
  "The members of what a registered :read returned: a gumshoe.reads result
   (an object or a list), or a sequence of them. An :unread result throws the
   unread exception; anything that is not a result throws."
  [read-name value]
  (let [results (if (sequential? value) value [value])]
    (vec (mapcat (fn [r]
                   (when-not (result/result? r)
                     (throw (ex-info (format ":reads %s feeds an :each and returned %s - a :read function an :each acts on returns gumshoe.reads results"
                                             read-name (pr-str r))
                                     {:read read-name})))
                   (if (contains? r :items)
                     (let [type (get-in r [:ref :type])]
                       (map (fn [o] {:ref (cond-> {:type type :name (kubectl/name-of o)}
                                            (kubectl/namespace-of o) (assoc :namespace (kubectl/namespace-of o)))
                                     :object o})
                            (result/items! r)))
                     (when-let [o (result/object! r)]
                       [{:ref (select-keys (:ref r) [:type :namespace :name]) :object o}])))
                 results))))

(defn- members-reader
  "(fn [value] -> members) for the :reads entry read-name, written as form."
  [read-name form]
  (case (first form)
    :list (let [type (second form)] (fn [value] (listed-members type read-name value)))
    :get (let [type (second form)]
           (fn [value]
             (if value
               [{:ref (cond-> {:type type :name (kubectl/name-of value)}
                        (kubectl/namespace-of value) (assoc :namespace (kubectl/namespace-of value)))
                 :object value}]
               [])))
    :present (fn [value] (vec value))
    :call (fn [value] (result-members read-name value))))

(defn- each-env
  [ctx {:keys [ref object]}]
  {:ctx ctx :object (:object ctx) :each object :each-ref ref})

(defn- member-key
  [member]
  (mutation/object-key (:ref member)))

(defn- distinct-members
  [members]
  (second (reduce (fn [[seen out] m]
                    (let [k (member-key m)]
                      (if (seen k) [seen out] [(conj seen k) (conj out m)])))
                  [#{} []]
                  members)))

(defn- each-of
  "An :each form at path, compiled: {:names :members :snapshot :max :verbs
   :deletes-only? :absent? :holds :checks :effects}. :holds is every check
   at once; :checks are its post-checks, each {:holds :says :timeout
   :interval}, :holds absent for an absence."
  [path form i read-forms value-names scope]
  (let [[_ source & more] form
        names (cond (keyword? source) [source]
                    (and (vector? source) (seq source) (every? keyword? source)) source
                    :else (invalid! (conj path 1) (format "an :each names the :reads entry whose objects it acts on, as :name or [:name ..], not %s"
                                                          (pr-str source))))
        _ (when-not (apply distinct? names)
            (invalid! (conj path 1) "an :each names each :reads entry once"))
        _ (doseq [n names]
            (cond
              (contains? read-forms n) nil
              (contains? value-names n)
              (invalid! (conj path 1) (format "%s is a :values entry, computed once and never read again - an :each acts on a :reads entry, which is read again after the confirmation"
                                              n))
              :else
              (invalid! (conj path 1) (format "%s names no declared :reads entry - the declared names are %s"
                                              n (if (seq read-forms) (known-words (keys read-forms)) "none")))))
        opts? (map? (first more))
        opts (if opts? (first more) {})
        forms (each-effect-forms form)
        offset (if opts? 3 2)
        _ (check-keys! (conj path 2) opts #{:state :max} "the options of an :each")
        _ (when (empty? forms)
            (invalid! path "an :each names one or more effects after its read, as [:each :name {..} <effect> ..]"))
        _ (doseq [[j f] (map-indexed vector forms) :when (each-form? f)]
            (invalid! (conj path (+ offset j)) "an :each acts on objects with kubectl or registered effects, not with another :each"))
        max (if (contains? opts :max)
              (let [m (:max opts)]
                (when-not (pos-int? m)
                  (invalid! (conj path 2 :max) (format ":max is a whole number of objects above zero, not %s" (pr-str m))))
                m)
              default-each-max)
        verbs (mapv form-verb forms)
        deletes? (some kubectl-delete? forms)
        state (:state opts)
        each-place (assoc (place scope "an :each" (conj pure-roots :each) #{:value}) :field-root :each)
        effects (vec (map-indexed (fn [j f] (effect-of (conj path (+ offset j)) f each-place :each-ref)) forms))
        entries (cond
                  (some? state) (let [entries (state-entries (conj path 2 :state) state)]
                                  (when (and deletes? (not (entries-absent? entries)))
                                    (invalid! (conj (:path (first entries)) :holds)
                                              "an :each that deletes brings about an absence - write :holds :absent, or leave :state out"))
                                  entries)
                  deletes? nil
                  :else (invalid! (conj path 2 :state)
                                  (str "an :each that does not delete says what holds for each object once it took effect, as {:state {:holds predicate :says \"template\"}}"
                                       " - it is each object's post-check and what skips an object already done")))
        absent? (or (nil? entries) (entries-absent? entries))
        checks (if absent?
                 [{:timeout (get-in (first entries) [:state :timeout] 60)
                   :interval (get-in (first entries) [:state :interval] 5)
                   :says (if-let [{p :path s :state} (first entries)]
                           (template (conj p :says) (:says s) each-place)
                           (fn [env] (str (mutation/describe-ref (:each-ref env)) " is gone")))}]
                 (mapv (fn [{p :path s :state}]
                         {:timeout (get s :timeout 60)
                          :interval (get s :interval 5)
                          :holds (predicate (conj p :holds) (:holds s) each-place)
                          :says (template (conj p :says) (:says s) each-place)})
                       entries))
        holds (if absent?
                (fn [env] (some? (get-in env [:each :metadata :deletionTimestamp])))
                (let [p (conjunction (mapv :holds checks))]
                  (fn [env] (verify/object-holds? (:each env) #(p (assoc env :each %))))))
        member-readers (into {} (map (fn [n] [n (members-reader n (get read-forms n))])) names)]
    {:names names
     :members (fn [ctx] (distinct-members (mapcat (fn [n] ((member-readers n) (get ctx n))) names)))
     :snapshot (keyword "gumshoe.dsl" (str "each-" i))
     :max max
     :verbs verbs
     :deletes-only? (every? kubectl-delete? forms)
     :absent? absent?
     :holds holds
     :checks checks
     :effects effects}))

(defn- confirmed-members
  "The members of each the confirmation showed."
  [each ctx]
  (get ctx (:snapshot each)))

(defn- acted-on
  "The confirmed members of each whose state does not hold yet: the objects
   the change acts on."
  [each ctx]
  (vec (remove #((:holds each) (each-env ctx %)) (confirmed-members each ctx))))

(defn- each-refusal
  "Why the objects each reads now rule the change out, or nil. Before the
   confirmation the read and the confirmed members are one reading, so only
   the bound can refuse; after it the read is the one made again."
  [each ctx]
  (let [confirmed (confirmed-members each ctx)
        fresh ((:members each) ctx)
        before (into {} (map (juxt member-key identity)) confirmed)
        after (into {} (map (juxt member-key identity)) fresh)
        names (str/join ", " (map pr-str (:names each)))]
    (or (when (< (:max each) (count fresh))
          (format "the :each over %s read %d objects, more than the %d one confirmation may act on - narrow the read, or raise the :each's :max"
                  names (count fresh) (:max each)))
        (some (fn [m]
                (when-not (contains? before (member-key m))
                  (str (mutation/describe-ref (:ref m)) ": it appeared after the confirmation, which did not show it - run the change again to see it")))
              fresh)
        (some (fn [{:keys [ref object] :as m}]
                (if-let [now (get after (member-key m))]
                  (when-let [why (mutation/drift (mutation/ref-fingerprint ref object) (mutation/ref-fingerprint ref (:object now)))]
                    (str (mutation/describe-ref ref) ": " why))
                  ;; a delete of an object that is gone already did what it
                  ;; asked for (gumshoe.deletes passes --ignore-not-found);
                  ;; any other verb planned for it would fail halfway
                  (when-not (:deletes-only? each)
                    (format "%s: it was deleted after the confirmation, and the %s planned for it would fail - run the change again"
                            (mutation/describe-ref ref) (str/join ", " (map name (:verbs each)))))))
              confirmed))))

(defn- each-items
  [each ctx]
  (for [{:keys [ref]} (acted-on each ctx)]
    (str (str/join ", " (map name (:verbs each))) " " (mutation/describe-ref ref))))

(defn- each-effects
  [each ctx]
  (for [m (acted-on each ctx)
        :let [env (each-env ctx m)]
        f (:effects each)]
    (f env)))

(defn- each-checks
  "The post-checks of each object each acts on, one per check of its :state,
   each judging the object read again within its own window; a predicate
   never holds for an object no read found, and an absence holds only when a
   read answered NotFound or the object is terminating."
  [each {:keys [context] :as ctx}]
  (for [{:keys [ref] :as m} (acted-on each ctx)
        :let [env (each-env ctx m)]
        {:keys [holds says timeout interval]} (:checks each)]
    (cond-> {:description (says env) :timeout timeout :interval interval}
      (:absent? each)
      (assoc :verifies-absence? true
             :check (fn [] (or (some-> (mutation/fetch-pinned context ref) :metadata :deletionTimestamp some?)
                               (kubectl/confirmed-gone? context (:namespace ref) (:type ref) (:name ref)))))
      (not (:absent? each))
      (assoc :check (fn [] (verify/object-holds? (mutation/fetch-pinned context ref)
                                                 #(holds (assoc env :each %))))))))

;; --- pins on objects other than the subject ---------------------------------------

(defn- pins-of
  "The :pin entries as a :target subjects entry: each object is read strictly
   before the confirmation and again after it, and the change is refused
   when one does not exist, was replaced or changed."
  [pins scope]
  (let [path [:pin]
        _ (when-not (and (vector? pins) (seq pins))
            (invalid! path "the :pin names one or more objects other than the subject, as [{:type \"type\" :namespace expr :name expr} ..]"))
        pin-place (place scope "a :pin entry" #{:target :ref :object :opt :call} #{:value})
        refs (vec (map-indexed
                   (fn [i r]
                     (let [p (conj path i)]
                       (check-keys! p r #{:type :namespace :name :compare} "a :pin entry")
                       (non-blank! (conj p :type) (:type r) "a pinned object's :type")
                       (when-not (contains? r :name)
                         (invalid! (conj p :name) "a pinned object names itself as :name expr"))
                       (when (contains? r :compare)
                         (when-let [why (or (when (nil? (:compare r)) "a pin compares nil - drop :compare, or write :compare :spec")
                                            (change/compare-problem (:compare r)))]
                           (invalid! (conj p :compare) why)))
                       {:type (:type r)
                        :name (expr (conj p :name) (:name r) pin-place)
                        :namespace (when (contains? r :namespace) (expr (conj p :namespace) (:namespace r) pin-place))
                        :compare (:compare r)}))
                   pins))]
    {:from :target
     :refs (fn [ctx]
             (let [env {:ctx ctx :object (:object ctx)}]
               (mapv (fn [{:keys [type compare] pin-name :name pin-namespace :namespace}]
                       (let [n (pin-name env)
                             ns (when pin-namespace (pin-namespace env))]
                         (when-not (and (string? n) (not (str/blank? n)))
                           (throw (ex-info (format "a pinned %s names no object (its :name is %s)" type (pr-str n)) {:type type})))
                         (cond-> {:type type :name n}
                           ns (assoc :namespace ns)
                           compare (assoc :compare compare))))
                     refs)))}))

;; --- problems ------------------------------------------------------------------------

(defn- problems-of
  "The :problems entries as a :problems function: every message each entry
   gives, in order."
  [problems scope state-pred]
  (let [path [:problems]
        _ (when-not (and (vector? problems) (seq problems))
            (invalid! path "the :problems are a vector of one or more {:when predicate :says \"template\"} or {:reasons call}"))
        problem-place (assoc (place scope "a :problems entry" pure-roots #{:value}) :state state-pred)
        entries (vec (map-indexed
                      (fn [i entry]
                        (let [p (conj path i)]
                          (check-keys! p entry #{:when :says :reasons} "a :problems entry")
                          (cond
                            (contains? entry :reasons)
                            (let [_ (when (or (contains? entry :when) (contains? entry :says))
                                      (invalid! p "a :problems entry is {:when predicate :says \"template\"} or {:reasons call}, not both"))
                                  _ (when-not (and (vector? (:reasons entry)) (= :call (first (:reasons entry))))
                                      (invalid! (conj p :reasons) "a :reasons is [:call <:problems function> arg ..]"))
                                  fname (second (:reasons entry))
                                  f (call-of (conj p :reasons) (:reasons entry) problem-place #{:problems})]
                              (fn [env]
                                (let [v (f env)]
                                  ;; a function that returns no list is a broken
                                  ;; gate; throwing makes change/judge-problems
                                  ;; refuse the change naming it
                                  (when-not (sequential? v)
                                    (throw (ex-info (format "%s returned %s, not a list of messages" fname (pr-str v)) {:name fname})))
                                  v)))

                            (and (contains? entry :when) (contains? entry :says))
                            (let [pred (predicate (conj p :when) (:when entry) problem-place)
                                  says (template (conj p :says) (:says entry) problem-place)]
                              (fn [env] (when (pred env) [(says env)])))

                            :else (invalid! p "a :problems entry is {:when predicate :says \"template\"} or {:reasons call}"))))
                      problems))]
    (fn [ctx]
      (let [env {:ctx ctx :object (:object ctx)}]
        (vec (mapcat #(% env) entries))))))

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
    :confirm :items :announce :effect :recovery :opt-out :pin :problems})

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
      (nil? items) nil

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
  [{:keys [description title options prerequisites target reads values state refuse confirm items announce effect recovery opt-out
           pin problems]
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
  ;; :values first: the :reads check takes the names of the values, so a
  ;; :values that is no map must be refused before anything reads its keys
  (check-names! [:values] values #{})
  (check-names! [:reads] reads (set (keys values)))
  (when-not (and (vector? effect) (seq effect))
    (invalid! [:effect] "the change names one or more effects, as [[:kubectl <verb> {..}] ..], [[:effect <registered type> arg ..] ..] or [[:each <read> {..} <effect> ..] ..]"))
  (let [select (select-of target)
        options (try (select/options select (or options {}))
                     (catch clojure.lang.ExceptionInfo e (invalid! [:options] (ex-message e))))
        scope {:reads (set (keys reads)) :values (set (keys values)) :options (set (keys options))}
        pure (place scope "this place" pure-roots #{:value})
        object-only (place {:reads #{} :values #{} :options #{}}
                           "the target's :candidates {:unless :state}, which judges each listed object alone"
                           #{:object :call} #{:value})
        entries (when (some? state) (state-entries [:state] state))
        absent? (entries-absent? entries)
        state-checks (mapv (fn [{p :path s :state}]
                             {:state s
                              :holds (when-not (absent-state? s) (predicate (conj p :holds) (:holds s) pure))
                              :says (template (conj p :says) (:says s) pure)})
                           entries)
        ;; the target state holds when every one of its post-checks does
        state-pred (when-not absent? (conjunction (keep :holds state-checks)))
        object-state (when entries
                       (fn []
                         (if absent?
                           (fn [env] (some-> (:object env) :metadata :deletionTimestamp some?))
                           (conjunction (mapv (fn [{p :path s :state}] (predicate (conj p :holds) (:holds s) object-only))
                                              entries)))))
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
        destroying (first (keep-indexed (fn [i form]
                                          (cond
                                            (each-form? form)
                                            (when (some destructive-form? (each-effect-forms form))
                                              (format "%s at %s" (pr-str form) (pr-str [:effect i])))
                                            (destructive-form? form)
                                            (format "%s at %s" (pr-str form) (pr-str [:effect i]))))
                                        effect))
        destructive-effect? (some? destroying)
        parts (vec (map-indexed (fn [i form]
                                  (if (each-form? form)
                                    {:each (each-of [:effect i] form i (or reads {}) (set (keys values)) scope)}
                                    {:subject (effect-of [:effect i] form pure #(get-in % [:ctx :ref]))}))
                                effect))
        eaches (vec (keep :each parts))
        subject-effects? (boolean (some :subject parts))
        ;; a change acting only through :each judges itself done by its
        ;; objects; one with an effect on the subject needs the :state of it
        done-able? (or (some? state) (and (seq eaches) (not subject-effects?)))
        destructive? (if (contains? confirm :destructive?) (:destructive? confirm) destructive-effect?)
        used (cond-> #{}
               (and (nil? state) subject-effects?) (conj :verify)
               done-able? (conj :done)
               (and destructive? (nil? recovery)) (conj :recovery)
               (and destructive-effect? (false? destructive?)) (conj :destructive))]
    (when (and (nil? state) subject-effects? (not (contains? opt-out :verify)))
      (invalid! [:state] (str "the change declares no target :state, so it can neither verify itself nor skip a run that is already done"
                              " - declare :state, or :opt-out {:verify \"why\"}")))
    (when-not (contains? opt-out :destructive)
      (when-let [why (change/destructive-contradiction destroying (:destructive? confirm))]
        (invalid! [:confirm :destructive?] (str why ", or :opt-out {:destructive \"why\"}"))))
    (when (and destructive? (nil? recovery) (not (contains? opt-out :recovery)))
      (invalid! [:recovery] (str "the change is destructive and says nothing of what a failure leaves behind"
                                 " - write :recovery \"what is left and how to undo it\", or :opt-out {:recovery \"why\"}")))
    (doseq [k (keys opt-out) :when (not (used k))]
      (invalid! [:opt-out k] (format "the change opts out of %s, which it does not get%s - drop it"
                                     k (case k
                                         :verify (if state
                                                   " (it declares a :state, which is its post-check)"
                                                   " (it acts only through :each, whose :state is each object's post-check)")
                                         :done " (it declares no :state to skip by)"
                                         :recovery (if destructive? " (it declares a :recovery)" " (it is not destructive)")
                                         :destructive " (it is destructive, or deletes nothing)"))))
    (let [subject (cond-> {:from :pick}
                    (:type target) (assoc :type (:type target))
                    parse (assoc :parse parse)
                    (:invalid target) (assoc :invalid (:invalid target))
                    (:compare target) (assoc :compare (:compare target)))
          subject-refusal (refusal-of refuse target scope (state-judged absent? state-pred))
          subject-verify (when entries (post-checks-of state-checks))
          own-items (items-of items scope)
          registered-ops (vec (distinct (for [form effect
                                              f (if (each-form? form) (each-effect-forms form) [form])
                                              :when (registered-form? f)]
                                          (second f))))
          registered? (boolean (seq registered-ops))
          plan-of (fn [ctx] (let [env {:ctx ctx :object (:object ctx)}]
                              (apply effect/plan (mapcat (fn [{:keys [subject each]}]
                                                           (if each (each-effects each ctx) [(subject env)]))
                                                         parts))))
          ;; the confirmation lists the subject or the change's own items,
          ;; every object an :each acts on, and what each registered effect
          ;; will do as its :describe says it
          items-fn (when (or own-items (seq eaches) registered?)
                     (fn [ctx]
                       (vec (concat (if own-items
                                      (own-items ctx)
                                      (select/items (:mode select) (:target ctx)))
                                    (mapcat #(each-items % ctx) eaches)
                                    (when registered?
                                      (->> (plan-of ctx)
                                           (remove #(effect/built-in-op? (first %)))
                                           (map effect/describe)))))))
          all-values (into compiled-values
                           (map (fn [each] [(:snapshot each) (fn [ctx] ((:members each) ctx))]))
                           eaches)
          spec (cond-> {:description description
                        :options options
                        :prerequisites prerequisites
                        :select (assoc select :candidates candidates)
                        :subjects (cond-> [subject] pin (conj (pins-of pin scope)))
                        :refusal (if (seq eaches)
                                   (fn [ctx] (or (subject-refusal ctx) (some #(each-refusal % ctx) eaches)))
                                   subject-refusal)
                        :confirm (cond-> {:action (:action confirm) :destructive? destructive?}
                                   (contains? confirm :separately?) (assoc :separately? (:separately? confirm)))
                        :effect (cond->> plan-of
                                  ;; the mark keeps the change destructive however it is
                                  ;; composed later; an opted-out delete carries none,
                                  ;; so normalize accepts its :destructive? false
                                  (and destructive-effect? destructive?) (change/destructive destroying))}
                 title (assoc :title title)
                 (seq compiled-reads) (assoc :reads compiled-reads)
                 (seq all-values) (assoc :values all-values)
                 items-fn (assoc :items items-fn)
                 (string? announce) (assoc :announce (ctx-template [:announce] announce pure))
                 (false? announce) (assoc :announce false)
                 (and subject-verify (empty? eaches)) (assoc :verify subject-verify)
                 (seq eaches) (assoc :verify (fn [ctx]
                                               (vec (concat (when subject-verify (subject-verify ctx))
                                                            (mapcat #(each-checks % ctx) eaches)))))
                 (and done-able? (not (contains? opt-out :done)) (empty? eaches)) (assoc :done true)
                 ;; done when no object of any :each is left to act on; the
                 ;; engine joins this with every post-check, the subject's too
                 (and done-able? (not (contains? opt-out :done)) (seq eaches))
                 (assoc :done (fn [ctx] (every? #(empty? (acted-on % ctx)) eaches)))
                 (and (nil? state) (empty? eaches)) (assoc :no-verify (:verify opt-out))
                 (some? problems) (assoc :problems (problems-of problems scope (state-judged absent? state-pred)))
                 (some? recovery) (assoc :recovery (ctx-template [:recovery] recovery pure)))]
      (when-not (or (nil? announce) (false? announce) (string? announce))
        (invalid! [:announce] (format ":announce is a template or false, not %s" (pr-str announce))))
      {:spec spec
       :safeguards (cond-> {:subject-pin :on
                    :strict-reads :on
                    :post-check (cond state :on
                                      subject-effects? {:off (:verify opt-out)}
                                      :else :subject-not-acted-on)
                    :done (cond (not done-able?) {:off (:verify opt-out)}
                                (contains? opt-out :done) {:off (:done opt-out)}
                                :else :on)
                    :destructive-confirmation (cond destructive? :on
                                                    destructive-effect? {:off (:destructive opt-out)}
                                                    :else :not-destructive)
                    :recovery (cond (not destructive?) :not-destructive
                                    recovery :on
                                    :else {:off (:recovery opt-out)})}
                   pin (assoc :pins :on)
                   registered? (assoc :effects (mapv (fn [op] {:type op :destructive? (:destructive? (effect/effect-type op))})
                                                     registered-ops))
                   (seq eaches) (assoc :each (mapv (fn [{:keys [names max]}]
                                                     {:over names :max max :pin :on :post-check :on :grown :refused})
                                                   eaches)))})))

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
   {:off \"why\"}, and under :effects every registered effect type it
   names, as {:type op :destructive? bool}."
  [compiled]
  (::safeguards (meta compiled)))

(defn source-of
  "The DSL data a compiled change was compiled from."
  [compiled]
  (::source (meta compiled)))
