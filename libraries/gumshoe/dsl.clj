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
   so a reader of the change can judge the data it was written as.

   A detective - reads, pure checks and findings, and nothing that writes -
   is data declaring :kind :detective, which compile-detective compiles
   into the spec gumshoe.detective/book takes (see its doc). A procedure -
   changes, pauses and checks in order under one confirmation - is data
   declaring :kind :procedure, which compile-procedure compiles into the
   spec gumshoe.playbook/book takes (see its doc)."
  (:refer-clojure :exclude [compile])
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.change :as change]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.dsl.library]
            [gumshoe.dsl.registry :as registry]
            [gumshoe.deletes :as deletes]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as detectives]
            [gumshoe.effect :as effect]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.offline :as offline]
            [gumshoe.onset :as onset]
            [gumshoe.playbook :as playbook]
            [gumshoe.reads :as reads]
            [gumshoe.reads.result :as result]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]
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
  (into #{:field :some? :nil? :not :and :or :call :state :absent :present} (keys comparisons)))

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
      (let [_ (when-let [why (:no-field place)] (invalid! path why))
            _ (arity! 3 "[:field path op value]")
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

      ;; a detective's check judges the state of a read: [:present :name]
      ;; holds when the read found something, [:absent :name] when it
      ;; answered that nothing is there; a read that did not answer leaves
      ;; the check unread, so neither is judged then
      (and (#{:present :absent} op) (:read-states? place))
      (do (arity! 1 (format "[%s :read]" op))
          (let [k (first args)]
            (when-not (keyword? k)
              (invalid! (conj path 1) (format "[%s ..] names a :reads entry as a keyword, not %s" op (pr-str k))))
            (named! (conj path 1) :read k (:reads place))
            (fn [env] (= op (get-in env [:states k])))))

      (= :present op)
      (invalid! path (format "[:present :read] is a detective's check, and is not available in %s" what))

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
  [path form snapshot read-forms value-names scope]
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
     :snapshot snapshot
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

(defn- check-version!
  "Refuses data that declares no :gumshoe/dsl marker or another one than
   version. The marker is judged first, so data written for another
   version is refused for that alone, before any word it uses."
  [dsl what]
  (cond
    (not (contains? dsl :gumshoe/dsl))
    (invalid! [:gumshoe/dsl] (format "the %s declares no version - declare :gumshoe/dsl %s, the experimental vocabulary, whose format may change in any release"
                                     what version))

    (not= version (:gumshoe/dsl dsl))
    (invalid! [:gumshoe/dsl] (format "the %s declares :gumshoe/dsl %s, and this compiles %s - declare :gumshoe/dsl %s, the experimental vocabulary, whose format may change in any release"
                                     what (pr-str (:gumshoe/dsl dsl)) version version))))

(defn- snapshot-key
  "The :values name holding the members the confirmation showed of the
   :each at effect index i. A change compiled alone names it each-<i>; a
   step of a procedure prefixes its own index, so the snapshots of two
   steps never share a name when their specs are composed."
  [prefix i]
  (keyword "gumshoe.dsl" (str prefix "each-" i)))

(defn- compiled
  [{:keys [description title options prerequisites target reads values state refuse confirm items announce effect recovery opt-out
           pin problems]
    :as dsl}
   & [{:keys [snapshot-prefix] :or {snapshot-prefix ""}}]]
  (when-not (map? dsl)
    (invalid! [] (format "a change is a map, not %s" (pr-str dsl))))
  (check-version! dsl "change")
  (check-no-code! [] dsl)
  (when (contains? dsl :kind)
    (invalid! [:kind] (format "a change declares no :kind - a detective, :kind :detective, compiles with compile-detective, and a procedure, :kind :procedure, with compile-procedure, not %s"
                              (pr-str (:kind dsl)))))
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
                                    {:each (each-of [:effect i] form (snapshot-key snapshot-prefix i) (or reads {}) (set (keys values)) scope)}
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
       ;; what a procedure's one confirmation shows of this change once
       ;; the judgement before it derived the change's context: the objects
       ;; each :each acts on and what every registered effect will do
       :outline-now (fn [ctx]
                      (vec (concat (mapcat #(each-items % ctx) eaches)
                                   (when registered?
                                     (->> (plan-of ctx)
                                          (remove #(effect/built-in-op? (first %)))
                                          (map effect/describe))))))
       :destructive? destructive?
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

;; --- detectives ----------------------------------------------------------------------
;;
;; A detective is read-only data: {:kind :detective} with :reads and :checks.
;; compile-detective turns it into the spec gumshoe.detective/book takes, whose
;; :collect makes the reads and whose detectives judge them, so it runs, lists
;; and reports as every Clojure detective does. Nothing in its vocabulary can
;; confirm, pin or change anything: the keys of a change are refused, an
;; effect is no expression, and the checks run inside gumshoe.offline, so a
;; registered function a check calls cannot reach outside the process either.

(def ^:private detective-keys
  #{:gumshoe/dsl :kind :description :when-to-run :options :prerequisites :reads :checks})

(def ^:private change-only-keys
  "The keys of a change. Each confirms, pins, acts or verifies an action, so
   a detective, which is read-only, declares none of them."
  #{:target :values :state :refuse :problems :pin :confirm :items :announce :effect :recovery :opt-out :title})

(def ^:private check-keys
  #{:name :description :severity :each :expect :says :hint :component :since :empty-is-clean})

(def severities
  "The severities a detective's check reports, as gumshoe.detective ranks them."
  #{:critical :warning :info})

(def ^:private reserved-read-names
  "Names no detective read may take: the keys a check's environment binds
   beside the reads, and the :now every scan's evidence carries."
  (into change/context-keys #{:now :states}))

(defn- read-scope
  "The :namespace option of a detective read at path as (fn [env] ->
   namespace): nil for {:namespace :cluster}, a cluster-scoped type, :all for
   {:namespace :all}, which only a [:list ..] takes, else the expression's
   value. An expression that does not evaluate to a non-blank string
   throws, so the read is reported unread: a missing flag never falls back
   to the kube context's default namespace."
  [path opts place all?]
  (when-not (contains? opts :namespace)
    (invalid! (conj path :namespace)
              (str "a detective's read names where it reads: {:namespace expr} for one namespace, "
                   "{:namespace :cluster} for a cluster-scoped type"
                   (when all? ", or {:namespace :all} for every namespace"))))
  (let [v (:namespace opts)]
    (cond
      (= :cluster v) (constantly nil)
      (= :all v) (if all?
                   (constantly :all)
                   (invalid! (conj path :namespace) ":all reads every namespace, which only a [:list ..] does"))
      :else (let [f (expr (conj path :namespace) v place)]
              (fn [env]
                (let [n (f env)]
                  (if (and (string? n) (not (str/blank? n)))
                    n
                    (throw (ex-info (format "its namespace evaluated to %s, so it was not read" (pr-str n)) {})))))))))

(defn- text-arg
  "The optional option k of a read at path as (fn [env] -> non-blank
   string), throwing when it evaluates to anything else, so a selector
   whose flag is missing never widens the read to every object."
  [path opts k place]
  (when (contains? opts k)
    (let [f (expr (conj path k) (get opts k) place)]
      (fn [env]
        (let [v (f env)]
          (if (and (string? v) (not (str/blank? v)))
            v
            (throw (ex-info (format "its %s evaluated to %s, so it was not read" (name k) (pr-str v)) {}))))))))

(defn- detective-read-of
  "A detective's read at path as (fn [env] -> value): a gumshoe.reads
   result, or a sequence of them."
  [path [op & args :as form] place]
  (when-not (vector? form)
    (invalid! path (format "%s is no read - write [:list ..], [:get ..], [:present ..] or [:call <:read function> ..]" (pr-str form))))
  (case op
    :list (let [[type opts] args]
            (when-not (= 2 (count args))
              (invalid! path "[:list \"type\" {:namespace ..}] takes a type and its options, which name where it reads"))
            (non-blank! (conj path 1) type "a read's type")
            (check-keys! (conj path 2) opts #{:namespace :selector :field-selector} "the options of a detective's [:list ..]")
            (let [scope (read-scope (conj path 2) opts place true)
                  selector (text-arg (conj path 2) opts :selector place)
                  field-selector (text-arg (conj path 2) opts :field-selector place)]
              (fn [env]
                (reads/listing (get-in env [:ctx :context])
                               (cond-> {:type type :namespace (scope env)}
                                 selector (assoc :selector (selector env))
                                 field-selector (assoc :field-selector (field-selector env)))))))
    :get (let [[type object-name opts] args]
           (when-not (= 3 (count args))
             (invalid! path "[:get \"type\" name {:namespace ..}] takes a type, a name and its options, which name where it reads"))
           (non-blank! (conj path 1) type "a read's type")
           (check-keys! (conj path 3) opts #{:namespace} "the options of a detective's [:get ..]")
           (let [object-name (expr (conj path 2) object-name place)
                 scope (read-scope (conj path 3) opts place false)]
             (fn [env]
               (let [namespace (scope env)]
                 (reads/object (get-in env [:ctx :context])
                               (cond-> {:type type :name (object-name env)} namespace (assoc :namespace namespace)))))))
    :present (let [[refs opts] args]
               (when-not (= 2 (count args))
                 (invalid! path "[:present [{:type \"type\" :name expr} ..] {:namespace ..}] takes the objects and its options"))
               (when-not (and (vector? refs) (seq refs))
                 (invalid! (conj path 1) "[:present ..] names one or more objects, as [{:type \"type\" :name expr :namespace expr} ..]"))
               (check-keys! (conj path 2) opts #{:namespace} "the options of a detective's [:present ..]")
               (let [scope (read-scope (conj path 2) opts place false)
                     wanted (vec (map-indexed
                                  (fn [i r]
                                    (let [p (conj path 1 i)]
                                      (check-keys! p r #{:type :name :namespace} "an object of [:present ..]")
                                      (non-blank! (conj p :type) (:type r) "an object's :type")
                                      (when-not (contains? r :name)
                                        (invalid! (conj p :name) "an object of [:present ..] names itself as :name expr"))
                                      {:type (:type r)
                                       :name (expr (conj p :name) (:name r) place)
                                       :scope (when (contains? r :namespace) (read-scope p r place false))}))
                                  refs))]
                 (fn [env]
                   (let [context (get-in env [:ctx :context])
                         refs (mapv (fn [{own :scope type :type object-name :name}]
                                      (let [namespace ((or own scope) env)]
                                        (cond-> {:type type :name (object-name env)} namespace (assoc :namespace namespace))))
                                    wanted)]
                     (concurrent/all (mapv (fn [ref] #(reads/object context ref)) refs))))))
    :call (call-of path form place #{:read})
    (invalid! path (format "%s is no read - write [:list ..], [:get ..], [:present ..] or [:call <:read function> ..]" (pr-str form)))))

(def ^:private reason-limit
  "The most characters of a value a reason quotes."
  200)

(defn- quoted
  [x]
  (let [s (pr-str x)]
    (if (< reason-limit (count s)) (str (subs s 0 reason-limit) "…") s)))

(defn read-outcome
  "What one detective read established, from the value its read returned:
   a gumshoe.reads result or a sequence of them.

     {:state :present :items [o ..] :data d}   it found something
     {:state :absent  :items []     :data d}   it answered: nothing there
     {:state :unread  :reason \"why\"}         nothing is known

   :items are the objects found; :data is what [:read :name] gives a check:
   the object of a single object read (nil when absent), else the items. A
   read is :unread when any result it returned is :unread, when it returned
   anything that is no result, or an empty sequence, which says nothing
   about whether it looked."
  [value]
  (let [results (if (sequential? value) (vec value) [value])
        foreign (remove result/result? results)]
    (cond
      (empty? results)
      {:state :unread :reason "it returned no gumshoe.reads result at all"}

      (seq foreign)
      {:state :unread :reason (format "it returned %s, which is no gumshoe.reads result" (quoted (first foreign)))}

      (some result/unread? results)
      {:state :unread
       :reason (str/join "; " (for [r results :when (result/unread? r)]
                                (str (result/describe (:ref r)) ": " (:reason r))))}

      :else
      (let [items (vec (mapcat (fn [r] (if (contains? r :items) (:items r) (when-let [o (:object r)] [o]))) results))]
        {:state (if (seq items) :present :absent)
         :items items
         :data (if (and (map? value) (not (contains? value :items))) (:object value) items)}))))

(defn- outcome-of
  "read-outcome of running thunk, which makes one read under strict reads:
   a read that throws - a strict reader that did not answer, a ref it
   cannot read, an expression that evaluated to nothing - is :unread with
   the exception's message."
  [thunk]
  (try
    (read-outcome (binding [kubectl/*strict-reads* true] (thunk)))
    (catch Throwable e
      {:state :unread :reason (or (ex-message e) (str e))})))

(defn- read-names-in
  "The :reads names x refers to: [:read :name ..], [:present :name] and
   [:absent :name] forms, and {read.name..} placeholders of its templates."
  [x]
  (cond
    (map? x) (mapcat read-names-in (concat (keys x) (vals x)))
    (vector? x) (concat (when (and (#{:read :present :absent} (first x)) (keyword? (second x))) [(second x)])
                        (mapcat read-names-in x))
    (string? x) (keep (fn [[_ inner]]
                        (let [[root n] (str/split inner #"\.")]
                          (when (and (= "read" root) (not (str/blank? n))) (keyword n))))
                      (re-seq placeholder x))
    :else nil))

(defn- uses-options?
  "Whether x reads a flag: an [:opt ..] form or an {opt..} placeholder."
  [x]
  (cond
    (map? x) (boolean (some uses-options? (concat (keys x) (vals x))))
    (vector? x) (or (= :opt (first x)) (boolean (some uses-options? x)))
    (string? x) (boolean (re-find #"\{opt[.}]" x))
    :else false))

(defn- object-key
  [o]
  (if-let [n (kubectl/name-of o)]
    [(:kind o) (kubectl/namespace-of o) n]
    o))

(defn- distinct-objects
  [objects]
  (second (reduce (fn [[seen out] o]
                    (let [k (object-key o)]
                      (if (seen k) [seen out] [(conj seen k) (conj out o)])))
                  [#{} []]
                  objects)))

(defn- object-component
  [o]
  (if-let [namespace (kubectl/namespace-of o)]
    (str namespace "/" (kubectl/name-of o))
    (str (kubectl/name-of o))))

(defn- check-of
  "A detective's check at path as {:check <the gumshoe.detective
   detective> :reads [name ..]}. key-of maps a read's name to its evidence
   key."
  [path check scope key-of]
  (check-keys! path check check-keys "a check")
  (non-blank! (conj path :name) (:name check) "a check's :name")
  (non-blank! (conj path :description) (:description check) "a check's :description")
  (when-not (severities (:severity check))
    (invalid! (conj path :severity) (format "%s is no severity - write one of %s" (pr-str (:severity check)) (known-words severities))))
  (when-not (contains? check :expect)
    (invalid! (conj path :expect) "a check says what holds when all is well, as :expect predicate, and reports a finding where it does not"))
  (let [each (:each check)
        names (when (contains? check :each)
                (cond (keyword? each) [each]
                      (and (vector? each) (seq each) (every? keyword? each)) each
                      :else (invalid! (conj path :each) (format "an :each names the :reads entry whose objects the check judges, as :name or [:name ..], not %s"
                                                                (pr-str each)))))
        _ (when (and names (not (apply distinct? names)))
            (invalid! (conj path :each) "an :each names each :reads entry once"))
        _ (doseq [n names] (named! (conj path :each) :read n (:reads scope)))
        empty-is-clean (:empty-is-clean check)
        _ (when (contains? check :empty-is-clean)
            (when-not names
              (invalid! (conj path :empty-is-clean) "only an :each check judges objects, so only it may say why none is clean"))
            (non-blank! (conj path :empty-is-clean) empty-is-clean "the reason an empty read is clean"))
        what (format "the check %s" (pr-str (:name check)))
        place (cond-> (assoc (place scope what (cond-> #{:read :opt :call} names (conj :each) (:target? scope) (conj :target)) #{:value})
                             :read-states? true)
                names (assoc :field-root :each)
                (not names) (assoc :no-field (str "[:field ..] judges the object an :each check judges, and this check has no :each"
                                                  " - read a field as [:read :name & path]")))
        expect (predicate (conj path :expect) (:expect check) place)
        says (template (conj path :says) (:says check) place)
        hint (when (contains? check :hint) (template (conj path :hint) (:hint check) place))
        component (when (contains? check :component) (template (conj path :component) (:component check) place))
        since (when (contains? check :since) (expr (conj path :since) (:since check) place))
        used (vec (distinct (concat names (read-names-in (select-keys check [:expect :says :hint :component :since])))))
        _ (when (empty? used)
            (invalid! path "the check reads nothing, so it could judge nothing - name a :reads entry in :each, :expect or a template"))
        keys-used (mapv key-of used)
        {check-name :name :keys [severity]} check
        finding (fn [env default-component]
                  (cond-> {:severity severity
                           :component (if component (component env) default-component)
                           :summary (says env)}
                    hint (assoc :hint (hint env))
                    since (onset/since (since env))))
        judge (fn [evidence]
                (let [outcomes (zipmap used (map #(get evidence %) keys-used))]
                  ;; a check rests on every read it names, and run-detectives
                  ;; keeps nothing it concludes once one did not answer; it
                  ;; concludes nothing then, rather than judging a nil
                  (when (every? some? (vals outcomes))
                    (let [env {:ctx (into {:opts (get evidence "options") :target (get evidence ::target)}
                                          (map (fn [[k o]] [k (:data o)])) outcomes)
                               :states (into {} (map (fn [[k o]] [k (:state o)])) outcomes)}]
                      (if names
                        (vec (for [o (distinct-objects (mapcat #(:items (get outcomes %)) names))
                                   :let [env (assoc env :each o)]
                                   :when (not (expect env))]
                               (finding env (object-component o))))
                        (if (expect env) [] [(finding env check-name)]))))))]
    {:reads used
     :check (cond-> {:name check-name
                     :description (:description check)
                     :requires keys-used
                     ;; any read that did not answer leaves the check unread
                     :joins keys-used
                     :detect (fn [evidence]
                               (offline/forbidding
                                (format "the check %s judges only what its reads returned" check-name)
                                #(judge evidence)))}
              ;; an :each over reads that found nothing judged nothing,
              ;; unless the detective says why nothing is clean
              (and names (nil? empty-is-clean)) (assoc :judges (mapv key-of names)))}))

(defn- evidence-key
  "The evidence key of the read name declares as form: unique to the form,
   so the reads of two detectives sharing a name never share a collector
   when both join one scope."
  [k form]
  (format "gumshoe.dsl/read %s %08x" (name k) (hash form)))

(defn- compiled-detective
  [{:keys [description when-to-run options prerequisites reads checks] :as dsl}
   & [{:keys [target?]}]]
  (when-not (map? dsl)
    (invalid! [] (format "a detective is a map, not %s" (pr-str dsl))))
  (check-version! dsl "detective")
  (check-no-code! [] dsl)
  (when-not (= :detective (:kind dsl))
    (invalid! [:kind] (format "the :kind is :detective, not %s - a change declares no :kind and compiles with compile, and a procedure compiles with compile-procedure"
                              (pr-str (:kind dsl)))))
  (doseq [k (sort (filter change-only-keys (keys dsl)))]
    (invalid! [k] (format "%s belongs to a change - a detective is read-only: it declares :reads and :checks, and nothing it declares confirms, pins or changes anything"
                          k)))
  (check-keys! [] dsl detective-keys "a detective")
  (non-blank! [:description] description "the :description")
  (when (contains? dsl :when-to-run) (non-blank! [:when-to-run] when-to-run "the :when-to-run"))
  (when-not (map? prerequisites)
    (invalid! [:prerequisites] (format "the detective declares its :prerequisites as a map, not %s" (pr-str prerequisites))))
  (when (some? options)
    (when-not (map? options)
      (invalid! [:options] "the :options are a map of flag keyword to its option map"))
    (doseq [[k o] options]
      (when-not (simple-keyword? k)
        (invalid! [:options k] (format "a flag is named by an unqualified keyword, not %s" (pr-str k))))
      (when (= :output k)
        (invalid! [:options k] "--output is the report format every detective takes - name the flag otherwise"))
      (when-not (map? o)
        (invalid! [:options k] (format "an option is a map such as {:desc \"..\" :coerce :string}, not %s" (pr-str o))))
      (non-blank! [:options k :desc] (:desc o) "an option's :desc")))
  (when-not (and (map? reads) (seq reads))
    (invalid! [:reads] "a detective declares what it reads, as :reads {name read}"))
  (check-names! [:reads] reads #{})
  (doseq [k (keys reads) :when (reserved-read-names k)]
    (invalid! [:reads k] (format "%s is a name a check binds itself - name the read otherwise" k)))
  (when-not (and (vector? checks) (seq checks))
    (invalid! [:checks] "a detective declares one or more checks, as :checks [{:name .. :expect .. :says ..} ..]"))
  (let [options (or options {})
        scope {:reads (set (keys reads)) :values #{} :options (conj (set (keys options)) :output) :target? target?}
        read-place (place scope "a detective's read" (cond-> #{:opt :context :call} target? (conj :target)) #{:value})
        compiled-reads (into (sorted-map)
                             (for [[k form] reads]
                               [k {:key (evidence-key k form)
                                   :read (detective-read-of [:reads k] form read-place)}]))
        key-of #(:key (compiled-reads %))
        compiled-checks (vec (map-indexed (fn [i c] (check-of [:checks i] c scope key-of)) checks))
        _ (reduce (fn [seen [i c]]
                    (when (seen (:name c))
                      (invalid! [:checks i :name] (format "%s names an earlier check - each check has a name of its own" (pr-str (:name c)))))
                    (conj seen (:name c)))
                  #{} (map-indexed vector checks))
        used (set (mapcat :reads compiled-checks))
        _ (doseq [k (keys reads) :when (not (used k))]
            (invalid! [:reads k] (format "no check judges %s, so reading it proves nothing - drop it or judge it" k)))
        read-all (fn [context opts target]
                   (let [env {:ctx {:context context :opts opts :target target}}
                         ks (vec (keys compiled-reads))
                         outcomes (concurrent/all (mapv (fn [k] #(outcome-of (fn [] ((:read (compiled-reads k)) env)))) ks))]
                     (zipmap ks outcomes)))
        collect-in (fn [context opts target]
                     (let [outcomes (read-all context (dissoc opts :output) target)]
                       (doseq [[k {:keys [state reason]}] outcomes :when (= :unread state)]
                         (stdout/err-println (format "could not read %s - %s; the checks resting on it are reported as reading nothing"
                                                     k reason)))
                       (into {:now (java.time.Instant/now) ::outcomes outcomes ::target target "options" (dissoc opts :output)}
                             (for [[k o] outcomes]
                               [(key-of k) (when-not (= :unread (:state o)) o)]))))
        collect (fn [opts]
                  (collect-in (or (:context kube-context/*pinned*)
                                  (try (kubectl/current-context) (catch Exception _ nil)))
                              opts nil))
        extra (fn [evidence]
                {:reads (vec (for [[k {:keys [state reason items]}] (::outcomes evidence)]
                               (cond-> {:name (name k) :state state}
                                 reason (assoc :reason reason)
                                 (not= :unread state) (assoc :count (count items)))))})
        collectors (into {} (for [[k {:keys [key read]}] compiled-reads]
                              [key (fn [context _key]
                                     (let [o (outcome-of #(read {:ctx {:context context :opts nil}}))]
                                       (when-not (= :unread (:state o)) o)))]))]
    {:spec (cond-> {:description description
                    :prerequisites prerequisites
                    :options options
                    :detectives (mapv :check compiled-checks)
                    :collect collect
                    :extra extra}
             when-to-run (assoc :when-to-run when-to-run))
     :collectors collectors
     :collect-in collect-in
     :options? (uses-options? (select-keys dsl [:reads :checks]))
     :safeguards {:read-only :on
                  :strict-reads :on
                  :unread-is-no-pass :on
                  :pure-checks :on
                  :empty-each (into {} (for [c checks :when (contains? c :each)]
                                         [(:name c) (if (contains? c :empty-is-clean)
                                                      {:off (:empty-is-clean c)}
                                                      :on)]))}}))

(defn compile-detective
  "The detective book spec of a DSL detective, as gumshoe.detective/book
   takes it: {:description :prerequisites :options :when-to-run
   :detectives :collect :extra}, with the source, its safeguards and the
   kind :detective in its metadata. Pure: it reads no cluster. Throws
   ex-info carrying ::invalid and :path for a detective it refuses.

     {:gumshoe/dsl :v0-experimental
      :kind :detective
      :description \"Nodes that are cordoned\"
      :prerequisites {:installed-tools [\"kubectl\"] :kubectl-can-get [\"nodes\"]}
      :reads {:nodes [:list \"nodes\" {:namespace :cluster}]}
      :checks [{:name \"cordoned-nodes\" :description \"nodes that take no new pods\"
                :severity :info :each :nodes
                :expect [:not [:field [:spec :unschedulable] := true]]
                :says \"node is cordoned (unschedulable)\"}]}

   Keys:
     :gumshoe/dsl    :v0-experimental, required
     :kind           :detective, required; data without it is a change
     :description    required
     :when-to-run    a one-line orientation printed before the run
     :options        {flag {:desc .. ..}}: the book's flags beside --output
     :prerequisites  required, as any book declares them
     :reads          {name read}, required; every read is judged by a check
     :checks         [check ..], required
   Every key of a change (:target :values :state :refuse :problems :pin
   :confirm :items :announce :effect :recovery :opt-out :title) is refused
   by name, and so is any other key, a symbol, or code anywhere.

   Reads, each made once, all at once, before any check runs:
     [:list \"type\" {:namespace expr | :cluster | :all
                    :selector expr :field-selector expr}]
     [:get \"type\" name {:namespace expr | :cluster}]
     [:present [{:type \"type\" :name expr :namespace expr} ..] {:namespace ..}]
     [:call <:read function> arg ..]   returning gumshoe.reads results
   Their arguments are expressions over [:opt ..], [:context] and :value
   functions. Each read ends in one of three states (read-outcome):
   present, absent, or unread - a read that did not answer, one whose
   namespace or selector flag evaluated to nothing, or a function that
   returned anything but gumshoe.reads results. :namespace is required, so
   a namespaced read never falls back to the kube context's default.

   A check:
     {:name \"..\" :description \"..\" :severity :critical|:warning|:info
      :each :read | [:read ..]   judge every object those reads found
      :expect predicate          what holds when all is well
      :says \"template\"           the finding's summary
      :hint \"template\" :component \"template\" :since expr   optional
      :empty-is-clean \"why\"}     an :each over nothing judged is clean
   A finding is reported where :expect does not hold: for each object of
   an :each, else once. The polarity fails closed - a field that is
   missing satisfies no comparison, so it reports rather than passes.
   Predicates are those of a change, over [:read :name & path] (the object
   of a [:get ..], the items of any other read), [:opt ..], :value and
   :predicate functions, [:field path op v] and [:each & path] inside an
   :each, and [:present :read] / [:absent :read], which judge whether a
   read found something or answered that nothing is there. The component
   of a finding is the object's namespace/name inside an :each, else the
   check's name.

   Three states. A check rests on every read it names. When one of them
   did not answer, the check is reported as having read nothing (:unread
   in the report's checked list), concludes nothing, and a run whose every
   check read nothing fails as nothing checked; the reason is printed on
   stderr and listed under :reads in the json and edn report. An :each
   whose reads found no object judged nothing, and is unread the same way,
   unless the check says why an empty read is clean with :empty-is-clean.

   Read-only. The spec holds no effect, confirmation, pin or post-check;
   gumshoe.detective/book runs it with :announce? false, so nothing it
   does is announced, and the checks run inside gumshoe.offline/forbidding,
   so a registered function that tried to run a command or send a request
   would crash its check, which fails the run."
  [dsl]
  (let [{:keys [spec safeguards collectors options?]} (compiled-detective dsl)]
    (vary-meta spec assoc ::source dsl ::safeguards safeguards ::kind :detective
               ::collectors collectors ::reads-options? options?)))

(defn detective?
  "Whether x is what compile-detective returns."
  [x]
  (= :detective (::kind (meta x))))

(defn detective-source?
  "Whether data declares a :kind, which only a detective does, so a book
   holding it compiles with compile-detective."
  [data]
  (and (map? data) (contains? data :kind)))

(defn register-detective!
  "Registers the checks of a compiled detective into scope of
   gumshoe.detectives.registry, and a collector for each of its reads, so a
   scan of that scope - or of :all, unless the scope is opt-in - reads and
   judges them beside the Clojure detectives, as a plugin's provide! does.
   A scope's book declares its own flags, so a detective that reads a flag
   is refused here: an [:opt ..] would read whatever flag of that name the
   scanning book has. Returns the names of the checks it registered."
  [scope compiled]
  (when-not (detective? compiled)
    (throw (ex-info "register-detective! takes what compile-detective returns" {::invalid true})))
  (when (::reads-options? (meta compiled))
    (throw (ex-info "the detective reads a flag, and a scope's book declares its own - run it as its own book, or read no [:opt ..]"
                    {::invalid true})))
  (doseq [[k collector] (::collectors (meta compiled))]
    (detectives/register-collector! k collector))
  (detectives/register! scope (:detectives compiled))
  (mapv :name (:detectives compiled)))

;; --- procedures ----------------------------------------------------------------------
;;
;; A procedure is an ordered vector of changes, pauses and checks against one
;; picked target, under one confirmation: data declaring :kind :procedure,
;; which compile-procedure turns into the spec gumshoe.playbook/book runs.
;; Each change step is a change of this vocabulary compiled on its own, so it
;; keeps every safeguard a change gets; the procedure only orders them and
;; asks once.

(def ^:private procedure-keys
  #{:gumshoe/dsl :kind :description :title :options :prerequisites :target :confirm :recovery :steps :phases})

(def ^:private pause-keys #{:says :title :unattended :until})

(def ^:private until-keys #{:read :holds :says :timeout :interval})

(def ^:private check-step-keys #{:title :blocking? :prerequisites :reads :checks})

(defn- at
  "The result of thunk, with the path of every refusal it throws prefixed
   by prefix, so a refusal inside a step names where the step stands."
  [prefix thunk]
  (try (thunk)
       (catch clojure.lang.ExceptionInfo e
         (if (invalid? e)
           (let [{:keys [path reason] :as data} (ex-data e)]
             (apply invalid! (into prefix path) reason (mapcat identity (dissoc data :path :reason ::invalid))))
           (throw e)))))

(defn- outline-of
  "What a change step does, from its data alone, as the one confirmation
   and --help show it before anything is read."
  [{:keys [effect]} label]
  (vec (for [form effect]
         (if (each-form? form)
           (let [[_ source & more] form
                 opts (when (map? (first more)) (first more))
                 forms (each-effect-forms form)
                 names (if (keyword? source) [source] source)]
             (format "%s each object of %s (at most %d)%s"
                     (str/join ", " (map #(if (keyword? %) (name %) (pr-str %)) (map form-verb forms)))
                     (str/join ", " (map pr-str names))
                     (get opts :max default-each-max)
                     (if (some destructive-form? forms) ", destructive" "")))
           (if (registered-form? form)
             (format "%s%s" (pr-str (second form)) (if (destructive-form? form) ", destructive" ""))
             (format "%s the %s%s" (name (second form)) label (if (destructive-form? form) ", destructive" "")))))))

(defn- change-step-of
  "A change step at path: the change data compiled with the procedure's
   target, its snapshot keys unique to step n."
  [path step target n phase-title description options]
  (doseq [[k why] [[:gumshoe/dsl "the procedure declares the version, and its steps share it"]
                   [:target "a step acts on the procedure's target, picked once for every step"]
                   [:kind "a step is a change map, [:pause {..}] or [:check {..}], and declares no :kind"]]
          :when (contains? step k)]
    (invalid! (conj path k) why))
  (let [{:keys [spec outline-now destructive? safeguards]}
        (at path #(compiled (assoc step :gumshoe/dsl version :target target
                                   :prerequisites (get step :prerequisites {})
                                   :options (merge options (:options step))
                                   :description (get step :description
                                                     (or (:title step) (get-in step [:confirm :action]) description)))
                            {:snapshot-prefix (str "step-" n "-")}))
        own-title (or (:title step) (get-in step [:confirm :action]))]
    {:step (-> (change/normalize spec)
               (assoc :title (if phase-title (str phase-title ": " own-title) own-title)
                      :outline (outline-of step (:label target))
                      :outline-now outline-now))
     :destructive? destructive?
     :safeguards safeguards}))

(defn- until-check-of
  "A check of a pause's :until at path, as (fn [ctx] -> post-check)."
  [path check scope]
  (check-keys! path check until-keys "a check of a pause's :until")
  (check-state! path (select-keys check [:holds :says :timeout :interval]))
  (let [form (:read check)
        _ (when-not (and (vector? form) (= :get (first form)))
            (invalid! (conj path :read) "a pause waits on one object, read as [:get \"type\" name {:namespace expr}]"))
        [_ type object-name opts & more] form
        _ (when (or (nil? object-name) (seq more))
            (invalid! (conj path :read) "[:get \"type\" name {:namespace expr}] takes a type, a name and its options"))
        _ (non-blank! (conj path :read 1) type "a read's type")
        name-place (place scope "a pause's :until" #{:target :opt :call} #{:value})
        object-name (expr (conj path :read 2) object-name name-place)
        namespace (namespace-arg (conj path :read 3) opts name-place)
        absent? (absent-state? check)
        holds (when-not absent?
                (predicate (conj path :holds) (:holds check)
                           (assoc (place scope "a pause's :until" #{:target :opt :call :each} #{:value}) :field-root :each)))
        says (template (conj path :says) (:says check) name-place)
        {:keys [timeout interval] :or {timeout 60 interval 5}} check]
    (fn [{:keys [context] :as ctx}]
      (let [env {:ctx ctx}
            n (object-name env)
            ns (when namespace (namespace env))
            fetch #(if ns
                     (kubectl/get-namespaced-resource context ns type n)
                     (kubectl/get-cluster-resource context type n))]
        (cond-> {:description (says env) :timeout timeout :interval interval}
          absent? (assoc :verifies-absence? true
                         :check (fn [] (or (some-> (fetch) :metadata :deletionTimestamp some?)
                                           (kubectl/confirmed-gone? context ns type n))))
          (not absent?) (assoc :check (fn [] (verify/object-holds? (fetch) #(holds (assoc env :each %))))))))))

(defn- pause-step-of
  "A [:pause {..}] at path as a gumshoe.playbook pause."
  [path [_ opts & more :as form] scope phase-title]
  (when (or (not (map? opts)) (seq more))
    (invalid! path (format "a pause is [:pause {:says \"what the operator does now\" ..}], not %s" (pr-str form))))
  (let [path (conj path 1)
        {:keys [says title unattended until]} opts]
    (check-keys! path opts pause-keys "a pause")
    (non-blank! (conj path :says) says "a pause's :says")
    (when (re-find placeholder says)
      (invalid! (conj path :says) "a pause's :says is shown before anything is read, so it holds no placeholder"))
    (when (contains? opts :title) (non-blank! (conj path :title) title "a pause's :title"))
    (when (contains? opts :unattended)
      (when-not (= :continue unattended)
        (invalid! (conj path :unattended) (format ":unattended is :continue, not %s" (pr-str unattended))))
      (when-not (contains? opts :until)
        (invalid! (conj path :unattended) "a pause that goes on without a terminal must prove the operator's part happened - declare :until")))
    (let [checks (when (contains? opts :until)
                   (when-not (and (vector? until) (seq until))
                     (invalid! (conj path :until) "the :until is a vector of one or more {:read [:get ..] :holds predicate :says \"template\"}"))
                   (vec (map-indexed (fn [i c] (until-check-of (conj path :until i) c scope)) until)))]
      (when checks
        (reduce (fn [seen [i c]]
                  (when (seen (:says c))
                    (invalid! (conj path :until i :says) (format "%s is said by an earlier check of the :until - each says what it checks" (pr-str (:says c)))))
                  (conj seen (:says c)))
                #{} (map-indexed vector until)))
      (cond-> {:pause says}
        (or title phase-title) (assoc :title (cond (and title phase-title) (str phase-title ": " title)
                                                   title title
                                                   :else (str phase-title ": " says)))
        checks (assoc :verify (fn [ctx] (mapv #(% ctx) checks)))
        unattended (assoc :unattended :continue)))))

(defn- check-step-of
  "A [:check {..}] at path: a read-only detective, judged when the run
   reaches it; a critical finding or nothing checked stops the procedure
   unless it says :blocking? false."
  [path [_ opts & more :as form] options phase-title]
  (when (or (not (map? opts)) (seq more))
    (invalid! path (format "a check is [:check {:title \"..\" :reads {..} :checks [..]}], not %s" (pr-str form))))
  (let [path (conj path 1)
        {:keys [title blocking? prerequisites reads checks]} opts]
    (check-keys! path opts check-step-keys "a check step")
    (non-blank! (conj path :title) title "a check step's :title")
    (when (contains? opts :blocking?)
      (when-not (boolean? blocking?) (invalid! (conj path :blocking?) ":blocking? is true or false")))
    (let [{:keys [spec collect-in]}
          (at path #(compiled-detective {:gumshoe/dsl version :kind :detective :description title
                                         :options options :prerequisites (or prerequisites {})
                                         :reads reads :checks checks}
                                        {:target? true}))
          detectives (:detectives spec)]
      {:title (if phase-title (str phase-title ": " title) title)
       :prerequisites (or prerequisites {})
       :check (fn [{:keys [context opts target]}]
                (let [evidence (collect-in context opts target)
                      checked (detective/mark-unread detectives evidence)
                      healthy (detective/report! checked (detective/run-detectives checked evidence) "text")]
                  (or (false? blocking?) healthy)))})))

(defn- step-forms
  "The steps of a procedure, each {:path :form :phase-title :phase}, in run
   order, from :steps or from the :steps of each of :phases."
  [{:keys [steps phases] :as dsl}]
  (cond
    (and (contains? dsl :steps) (contains? dsl :phases))
    (invalid! [:phases] "a procedure declares :steps, or :phases each holding steps, not both")

    (contains? dsl :steps)
    (do (when-not (and (vector? steps) (seq steps))
          (invalid! [:steps] "the :steps are a vector of one or more steps"))
        (vec (map-indexed (fn [i f] {:path [:steps i] :form f}) steps)))

    (contains? dsl :phases)
    (do (when-not (and (vector? phases) (seq phases))
          (invalid! [:phases] "the :phases are a vector of one or more {:phase :name :title \"..\" :steps [..]}"))
        (reduce (fn [acc [p {:keys [phase title] :as ph}]]
                  (let [path [:phases p]]
                    (check-keys! path ph #{:phase :title :steps} "a phase")
                    (when-not (simple-keyword? phase)
                      (invalid! (conj path :phase) (format "a phase is named by an unqualified keyword, not %s" (pr-str phase))))
                    (when (some #(= phase (:phase %)) acc)
                      (invalid! (conj path :phase) (format "%s names an earlier phase - each phase has a name of its own" phase)))
                    (non-blank! (conj path :title) title "a phase's :title")
                    (when-not (and (vector? (:steps ph)) (seq (:steps ph)))
                      (invalid! (conj path :steps) "a phase holds one or more steps"))
                    (into acc (map-indexed (fn [i f] {:path (conj path :steps i) :form f :phase phase :phase-title title})
                                           (:steps ph)))))
                []
                (map-indexed vector phases)))

    :else (invalid! [:steps] "a procedure declares its :steps, or :phases each holding steps")))

(defn- compiled-procedure
  [{:keys [description title options prerequisites target confirm recovery] :as dsl}]
  (when-not (map? dsl)
    (invalid! [] (format "a procedure is a map, not %s" (pr-str dsl))))
  (check-version! dsl "procedure")
  (check-no-code! [] dsl)
  (when-not (= :procedure (:kind dsl))
    (invalid! [:kind] (format "the :kind is :procedure, not %s" (pr-str (:kind dsl)))))
  (check-keys! [] dsl procedure-keys "a procedure")
  (non-blank! [:description] description "the :description")
  (when (contains? dsl :title) (non-blank! [:title] title "the :title"))
  (when-not (map? prerequisites)
    (invalid! [:prerequisites] (format "the procedure declares its :prerequisites as a map, not %s" (pr-str prerequisites))))
  (check-target! target)
  (when (map? (:candidates target))
    (invalid! [:target :candidates] "a procedure has no :state of its own to offer candidates :unless - name a :candidates function, [:call <:candidates function> [:context]]"))
  (check-keys! [:confirm] confirm #{:action :destructive?} "the procedure's :confirm")
  (non-blank! [:confirm :action] (:action confirm) "the confirmation's :action")
  (when (contains? confirm :destructive?)
    (when-not (boolean? (:destructive? confirm))
      (invalid! [:confirm :destructive?] ":destructive? is true or false")))
  (when (some? options)
    (when-not (and (map? options) (every? keyword? (keys options)) (every? map? (vals options)))
      (invalid! [:options] "the :options are a map of flag keyword to its option map")))
  (let [forms (step-forms dsl)
        select (select-of target)
        own-options (try (select/options select (or options {}))
                         (catch clojure.lang.ExceptionInfo e (invalid! [:options] (ex-message e))))
        scope {:reads #{} :values #{} :options (set (keys own-options))}
        compiled-steps (vec (map-indexed
                             (fn [n {:keys [path form phase phase-title]}]
                               (let [compiled
                                     (cond
                                       (map? form) (assoc (change-step-of path form target n phase-title description options) :kind :change)
                                       (and (vector? form) (= :pause (first form)))
                                       {:kind :pause :step (pause-step-of path form scope phase-title)}
                                       (and (vector? form) (= :check (first form)))
                                       {:kind :check :step (check-step-of path form own-options phase-title)}
                                       :else (invalid! path (format "a step is a change map, [:pause {..}] or [:check {..}], not %s" (pr-str form))))]
                                 (assoc compiled :path path :phase phase)))
                             forms))
        _ (when-not (some #(= :change (:kind %)) compiled-steps)
            (invalid! (if (contains? dsl :phases) [:phases] [:steps])
                      "a procedure changes something in one or more steps - a procedure that only reads is a detective, :kind :detective"))
        destroying (first (filter :destructive? compiled-steps))
        _ (when (and destroying (false? (:destructive? confirm)))
            (invalid! [:confirm :destructive?]
                      (format "the step at %s is destructive, and the procedure's :confirm says :destructive? false - drop the false; the one confirmation asks for the typed yes when any step is destructive"
                              (pr-str (:path destroying)))))
        steps (mapv :step compiled-steps)
        all-options (try (playbook/all-options {:steps steps :options own-options})
                         (catch clojure.lang.ExceptionInfo e (invalid! [:options] (ex-message e))))
        candidates (candidates-of target nil scope)
        parse (parse-of target)
        subject (cond-> {}
                  (:type target) (assoc :type (:type target))
                  parse (assoc :parse parse)
                  (:invalid target) (assoc :invalid (:invalid target))
                  (:compare target) (assoc :compare (:compare target)))
        pure (place scope "the procedure's :recovery" #{:target :opt} #{})
        recovery-of (when (some? recovery)
                      (let [t (template [:recovery] recovery pure)]
                        (fn [ctx] (t {:ctx ctx}))))
        spec (cond-> {:description description
                      :options all-options
                      :prerequisites prerequisites
                      :select (assoc select :candidates candidates)
                      :subject subject
                      :confirm {:action (:action confirm)
                                :destructive? (boolean (or (:destructive? confirm) destroying))}
                      :report-progress true
                      :steps steps}
               title (assoc :title title)
               recovery-of (assoc :recovery recovery-of))]
    (at [] #(change/normalize (playbook/pick-spec spec)))
    {:spec spec
     :phases (when (contains? dsl :phases)
               (vec (for [{:keys [phase title]} (:phases dsl)]
                      {:phase phase :title title
                       :steps (vec (keep-indexed (fn [i c] (when (= phase (:phase c)) i)) compiled-steps))})))
     :safeguards {:one-confirmation :on
                  :destructive-confirmation (if (get-in spec [:confirm :destructive?]) :on :not-destructive)
                  :each-step-judged-when-reached :on
                  :progress-report :on
                  :shared-data :none
                  :steps (mapv (fn [{:keys [kind path safeguards]}]
                                 (cond-> {:kind kind :path path}
                                   safeguards (assoc :safeguards safeguards)))
                               compiled-steps)}}))

(defn compile-procedure
  "The playbook spec of a DSL procedure, as gumshoe.playbook/book takes it,
   with the source, its safeguards, its phases and the kind :procedure in
   its metadata. Pure: it reads no cluster. Throws ex-info carrying
   ::invalid and :path for a procedure it refuses.

     {:gumshoe/dsl :v0-experimental
      :kind :procedure
      :description \"Adopts a namespace's objects into a Helm release\"
      :target {:pick :one :label \"namespace\" :type \"namespaces\" :flag :namespace
               :candidates [:call :my/namespaces [:context]]}
      :prerequisites {:installed-tools [\"kubectl\"]}
      :confirm {:action \"adopt the namespace\"}
      :recovery \"the namespace {target} may hold stamped objects and no release\"
      :phases [{:phase :prepare :title \"prepare\"
                :steps [{:confirm {:action \"stamp the adoptees\" :destructive? false}
                         :reads {..} :effect [[:each :adoptees {..} [:kubectl :patch {..}]]]}]}
               {:phase :install :title \"install\"
                :steps [[:pause {:says \"merge the release\" :unattended :continue
                                 :until [{:read [:get \"helmreleases.helm.toolkit.fluxcd.io\" \"moodle\" {:namespace [:target]}]
                                          :holds [:field [:status :observedGeneration] :>= 1]
                                          :says \"the release is reconciled\" :timeout 900 :interval 15}]}]
                        [:check {:title \"the release is healthy\" :reads {..} :checks [..]}]]}]}

   Keys:
     :gumshoe/dsl    :v0-experimental, required
     :kind           :procedure, required
     :description    required
     :title          optional
     :options        extra or reworded flags beside those the target derives
     :prerequisites  required; joined with every step's
     :target         picked once, as a change's :target; its :candidates is
                     a [:call ..], since a procedure has no :state of its
                     own to offer the objects :unless
     :confirm        {:action \"..\" :destructive? bool}: the one
                     confirmation, destructive when any step is
     :recovery       \"template\" over {target} and {opt.x}: what the
                     procedure leaves behind when it stops after changing
                     something, printed after the stopped step's own
     :steps          [step ..], or
     :phases         [{:phase :name :title \"..\" :steps [step ..]} ..]:
                     named groups of steps in run order, each step titled
                     with its phase, and (phases-of compiled) names which
                     steps each holds

   A step is one of:
     {change ..}     a change of this vocabulary without :gumshoe/dsl and
                     :target, which it takes from the procedure; its
                     :prerequisites default to {}. It is compiled as compile
                     compiles a change, with every safeguard: its own pin,
                     strict reads, refusals, post-checks, skip when done,
                     typed yes and :recovery when destructive. The snapshot
                     of an :each is named for the step, so the specs of two
                     steps compose.
     [:pause {:says \"what the operator does now\" :title \"..\"
              :unattended :continue
              :until [{:read [:get \"type\" name {:namespace expr}]
                       :holds predicate | :absent :says \"template\"
                       :timeout s :interval s} ..]}]
                     waits for the operator's ENTER, then proves the result
                     with each :until check, fail-closed as a post-check;
                     :unattended :continue goes on without a terminal and
                     waits for the checks instead, so it needs :until. :says
                     is shown before anything is read and holds no
                     placeholder.
     [:check {:title \"..\" :blocking? true :prerequisites {..}
              :reads {..} :checks [..]}]
                     a read-only detective, judged when the run reaches
                     it, as compile-detective compiles one; its reads and
                     templates may name [:target]. A critical finding or a
                     run that checked nothing stops the procedure, unless
                     :blocking? false.

   One confirmation. The checks before the first change or pause run first;
   then the procedure asks once, listing the target and every step - each
   change with what it acts on and how, from its data, and, for the change
   judged before the confirmation, the objects it will act on now - and
   asks for the typed yes when any step is destructive. A :confirm
   :destructive? false beside a destructive step is refused. A change step
   whose :confirm says :separately? true asks again when reached.

   Judgement and failure. Each change step is judged right before it runs,
   over its own fresh reads: its pin, refusals and :each pins after the
   steps before it ran. A step that is refused, fails or whose post-check
   does not hold stops the procedure: no later step runs, and the guidance
   under the FAILED banner names the stopped step's :recovery, the
   procedure's, and the steps finished, the one stopped at and those not
   run. Started again from the top, the steps that took effect are judged
   done and skipped; --from-segment N starts after the pause that opens
   segment N, as gumshoe.playbook resumes.

   No data passes between steps. A step names only its own :reads and
   :values, and compile refuses a name another step declares, so a later
   step that needs what an earlier one produced reads it again itself,
   after that step ran, and its pin and refusals judge that fresh read."
  [dsl]
  (let [{:keys [spec safeguards phases]} (compiled-procedure dsl)]
    (vary-meta spec assoc ::source dsl ::safeguards safeguards ::kind :procedure ::phases phases)))

(defn procedure?
  "Whether x is what compile-procedure returns."
  [x]
  (= :procedure (::kind (meta x))))

(defn procedure-source?
  "Whether data declares :kind :procedure, so a book holding it compiles
   with compile-procedure."
  [data]
  (and (map? data) (= :procedure (:kind data))))

(defn phases-of
  "The phases of a compiled procedure, each {:phase :title :steps [index
   ..]} naming the indices of its steps in (:steps compiled), or nil for a
   procedure declared with :steps."
  [compiled]
  (::phases (meta compiled)))
