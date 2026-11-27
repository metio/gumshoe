;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.change
  "The canonical change map, and `normalize`, which maps every spec shape
   gumshoe.mutation accepts onto it. The engine (mutation/book, run!,
   run-on!, every playbook step and dispatch route) reads only the canonical
   map, so a key has one meaning in one place and a new spelling is added
   here, never in the engine.

   Canonical change map:
     {:title .. :description .. :options {..} :prerequisites {..}
      :connect  (fn [opts] -> {:context :cluster :announce-as} or nil)
      :kube-context (fn [opts] -> a kube context name, or nil for the current one)
      :select   {:mode .. :label .. :candidates .. :empty-message \"...\" ..}
      :subjects [{:from :pick   :type .. :parse .. :invalid .. :facts .. :compare ..}
                 {:from :target :refs (fn [ctx] -> refs)}
                 {:from :reads  :ref  (fn [ctx] -> ref or nil)}]
      :reads    {name (fn [ctx] -> value)}
      :values   {name (fn [ctx] -> value)}
      :refusal  (fn [ctx] -> nil, a reason, a vector of reasons, or
                 gumshoe.change/unmet)
      :confirm  {:action .. :destructive? .. :separately? ..}
      :items    (fn [ctx] -> seq)
      :announce (fn [ctx] -> message), or false for a change that posts nothing
      :effect   (fn [ctx] -> plan)
      :verify   (fn [ctx] -> post-checks)   or  :no-verify \"why\"
      :done     true or (fn [ctx] -> bool)
      :recovery \"...\" or (fn [ctx] -> string)
      :confirmed? bool}

   :subjects declares every object the change acts on, each entry naming
   where its ref comes from, which decides when it is read and how a drift
   refusal names it:
     :pick    the pick itself, parsed by :parse into the :ref of every
              callback and read as :object before :derive; read again before
              :refusal; pinned unless a :reads entry exists; a drift refusal
              speaks of \"it\". At most one, and only on a select that picks
              one object.
     :target  refs computed from the context the pick read made, read as
              :refs and :objects before :derive and again before :refusal;
              every one pinned; a drift refusal names the object.
     :reads   one ref computed from the derived context, fingerprinted at the
              confirmation and again before the effect; a drift refusal
              speaks of \"it\".
   Each :from appears at most once. :connect takes no :subjects, because
   every entry is read with kubectl.

   :kube-context names the kube context the change reads and acts on, from
   the parsed flags, so a book can offer --context; nil means the current
   one. It is resolved once per run from the kubeconfig and reaches the
   candidate query, the subjects, :reads, :values, :derive, the cluster the
   confirmation names and every callback's :context, which the effect and
   the post-checks build their kubectl calls from (see gumshoe.kube-context).
   :connect names something that is not a kube cluster, so a spec takes at
   most one of the two: with both, a :connect could hand a kubectl read an
   SSH host's context, or the kube context could hide what :connect names.

   :reads are the live state a change rests on, each named and read
   strictly. They are read before the confirmation, after the subjects,
   concurrently with each other, and bound in the context under their names,
   so :items, :announce, :effect and :verify see what the confirmation
   showed. When the spec has a :refusal they are read again after the
   confirmation, after the subjects are read again, and :refusal judges
   the fresh values under the same names. A post-check reads one again
   with gumshoe.mutation/read-again.

   :values are computed once per run, before the confirmation and after
   the reads, and never again: a timestamp, a generated name, an operator's
   choice, a snapshot the confirmation shows. A value sees the subjects and
   the reads, never another value, and a read inside it is made once.

   A name is a keyword, and is neither one of the keys the engine hands
   every callback (:context :cluster :target :opts :announcement-data :ref
   :object :refs :objects) nor bound by both maps.

   :refusal is the one guard. It runs before the confirmation, against the
   derived context with what :recheck returns merged over it, and again
   after the confirmation, against the context the subjects and :recheck
   read again (see gumshoe.mutation). It returns nil to let the change run,
   a reason to refuse it with \"refusing: <reason>\", a non-empty vector of
   reasons to refuse it with one such line per reason, or `unmet` to refuse
   it in the engine's generic words (\"its precondition does not hold\"
   before the confirmation, gumshoe.flow's \"its precondition no longer
   holds\" after it), which add nothing of their own. refusal-messages
   reads any of them as the list of lines the operator is shown.

   :done says the change is already satisfied, so a run skips it instead
   of confirming and applying it again. `true` means \"when every post-check
   already holds\": the checks :verify returns are each attempted once, as
   gumshoe.verify/attempt runs them, before the confirmation. A function
   receives the derived context and returns whether what the change brings
   about already holds; beside a :verify it narrows the post-checks and
   never replaces them, so the change is done only when the function says
   so and every post-check holds as well. A destructive change is never
   skipped on evidence its post-check would not accept, so a destructive
   :done function needs a :verify, and :done true always needs one.
   Every read is strict, and a read that did not answer is never done (see
   gumshoe.mutation).

   Aliases, accepted indefinitely:
     :subject {..}         the :pick entry of :subjects
     :subjects (fn)        the :target entry of :subjects
     :pin (fn)             the :reads entry of :subjects
     :precondition (fn)    judged after :refusal, before and after the
                           confirmation, as a refusal returning `unmet`
                           when it returns false
     :problems (fn)        (fn [ctx] -> a sequence of messages, empty to
                           proceed): judged after :refusal, before and after
                           the confirmation, as a refusal returning the
                           vector of its messages, so the lines the operator
                           reads are the list the gate decided on (see
                           problems-refusal for what it may return and
                           what it may not do)
     :empty-message \"..\"  [:select :empty-message]
     :announce? false      :announce false; :announce? true is the default
                           and is dropped
     :satisfied? x         :done x
     :derive (fn)          the :values entry named `derive-entry`: a value
                           whose result, a map, is merged into the context
                           instead of bound under one name. It runs after
                           every named value, once, as :derive always ran.
     :recheck (fn)         the :reads entry named `recheck-entry`: a read
                           whose result, a map, is merged over the context
                           :refusal judges, before the confirmation and
                           after it, and never into the context :items,
                           :effect and :verify see. It runs after every
                           named read, and receives the context with the
                           values :derive returned, so it can compare them
                           with what it reads.
   Neither alias sits beside a named :reads or :values entry: the keys a
   :derive or :recheck map binds are known only when it runs, so either
   could hide a named entry, and the spec is refused instead of one of the
   two winning.
   A nil alias is absent, except :announce?, which the runbook harness reads
   as given. A :done or :satisfied? of nil or false is absent too.

   normalize refuses, with an ex-info carrying ::invalid, a spec that names
   one thing two ways or that the engine could not run as written: a
   canonical :subjects beside :subject or :pin, an unknown or repeated
   :from, a :compare no pin understands, two different empty messages, a changelog message beside
   :announce? false, a :recheck with nothing to judge it, a :subject on a
   :many select, :connect with any subject, :connect beside :kube-context,
   a :kube-context that is not a function, :problems beside :precondition
   (one gate saying why it refuses beside one that cannot, the split
   :problems exists to end) or a :problems that is not a function, :derive beside :values or
   :recheck beside :reads, either alias beside a named entry, a :reads or
   :values that is not a map of keyword to function, a name the engine
   binds itself or both maps bind, a reserved name in the wrong map, and an
   :effect marked destructive beside :confirm :destructive? false (see
   destructive). mutation/book normalizes
   when the book loads, so such a book fails the conformance gate.

   compose combines several specs - a builder's fragment and the caller's
   own keys - into one canonical change map, key by key. Each part is
   first mapped onto the canonical keys as normalize maps it, so a part may
   use the aliases, and the result is a canonical map normalize accepts as
   it is. A part may be nil. The rules never weaken a guard or a
   post-check: every part's refusal still refuses, every part's post-check
   still runs, and a change is destructive when any part says so.
     :reads :values     merged by name; a name two parts bind to different
                        functions is refused, the same function is one entry
     :refusal           every part's, judged in part order; the first reason
                        any part gives refuses, and the parts after it are
                        not judged
     :verify :items     every part's, concatenated in part order
     :done              done when every part's :done holds: true is
                        absorbed by a function (the engine joins a :done
                        function with every post-check, so true adds
                        nothing beside one), and functions must all hold; a
                        part without :done is neutral, its post-checks still
                        join the composed :verify every :done is judged with
     :subjects          concatenated: the :target entries' refs are joined,
                        and a ref two of them return is refused when they are
                        computed; a :pick or :reads entry is single-valued
     :confirm           merged key by key: :destructive? and :separately?
                        are OR-ed, every other key is single-valued; a
                        part saying :destructive? false is refused beside
                        a part whose :effect is marked destructive, in
                        every grouping of the parts
     :prerequisites     joined as gumshoe.prerequisites/union joins them
     :options           joined by name; an option two parts declare with a
                        different :coerce, :alias or :default is refused
     :no-verify :no-guard  every part's reason, joined with \"; \"; refused
                        when any part has a :verify, or a guard (:refusal,
                        :subjects), since the composed change then has one
     every other key    single-valued: two parts giving different values
                        (:effect, :select, :connect, :title, :recovery,
                        :announce, ...) are refused
   A combined function remembers the functions it combines, each once, so
   every grouping of the same parts behaves alike (compose is associative),
   and composing a result with a part it already holds returns that result
   unchanged. Two separate compose calls build two combined functions that
   behave alike and are not =, since functions compare by identity.
   compose skips the checks that need the whole change (a :done true
   without :verify, a :recheck with nothing to judge it, :connect beside a
   subject), because a fragment may be completed by a later compose;
   normalize makes them when the composed spec runs or its book loads."
  (:require [clojure.string :as str]
            [gumshoe.flags :as flags]
            [gumshoe.offline :as offline]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.reads.result :as result]))

(def unmet
  "What a :refusal returns to refuse in the generic words of gumshoe.flow:
   the reason of a :precondition that returned false."
  ::unmet)

(def ^:private sources #{:pick :target :reads})

(def comparisons
  "What a pin may compare on an object that carries no metadata.generation,
   named by :compare on a :pick subject or on a ref a :target or :reads
   entry returns:
     :resource-version  every write to the object, status included; the
                        default, so a type nothing is declared for fails
                        closed
     :spec              the object's spec and uid, for a type whose API
                        server never sets a generation although the type
                        splits what a user asks for (spec) from what a
                        controller reports (status). A Node is one: the
                        kubelet writes its status every few seconds, and
                        its spec holds what a change to its scheduling
                        rests on (unschedulable, taints, podCIDR,
                        providerID). A drift refusal names the spec keys
                        that moved.
   An object that carries a generation is compared by it unless :compare
   says :spec or its type is one of types-without-status-subresource (see
   comparison). Metadata (labels, annotations) is outside :spec; a change
   resting on it names it with :facts, or judges it in its :refusal over the
   object read again."
  #{:resource-version :spec})

(def types-without-status-subresource
  "Custom resource types whose CRD declares no status subresource. For such
   a type the API server raises metadata.generation on every write outside
   metadata, a controller's status update included, so the generation says
   nothing about whether the spec changed. An Argo CD Application is one:
   its CRD declares `subresources: {}`, and the application controller
   writes its status on every reconciliation."
  #{"applications.argoproj.io"})

(defn comparison
  "What a pin compares on object, of resource type `type`, under compare:
     :spec              its uid and spec, when compare says :spec or type is
                        one of types-without-status-subresource
     :generation        its uid and metadata.generation, when it carries one:
                        the API server raises it on every change of the spec
                        and leaves it alone on status writes, for a built-in
                        type and a CRD with the status subresource
     :resource-version  its uid and resourceVersion otherwise
   A type whose CRD lacks the status subresource raises its generation on
   status writes too, so pinning it by generation would refuse a change for
   every status update a controller made during the confirmation."
  [object type compare]
  (cond
    (or (= :spec compare) (contains? types-without-status-subresource type)) :spec
    (-> object :metadata :generation) :generation
    :else :resource-version))

(defn compare-problem
  "Why compare is not a :compare a pin understands, or nil for one of
   comparisons and for nil."
  [compare]
  (when-not (or (nil? compare) (contains? comparisons compare))
    (format "a pin compares %s - write :compare %s"
            (pr-str compare) (str/join " or " (map pr-str (sort comparisons))))))

(def derive-entry
  "The name of the :values entry a :derive alias becomes: a value whose map
   is merged into the context."
  ::derive)

(def recheck-entry
  "The name of the :reads entry a :recheck alias becomes: a read whose map
   is merged over the context :refusal judges."
  ::recheck)

(def context-keys
  "The keys the engine binds in every callback's context itself, which no
   :reads or :values entry may hide."
  #{:context :cluster :target :opts :announcement-data :ref :object :refs :objects})

(def ^:private unnamed-effect "marked by gumshoe.change/destructive")

(defn destructive
  "f, an :effect function, marked as one that destroys what it acts on, and
   named `effect` (a string, such as \"gumshoe.steps.objects/delete\") for
   the refusal below. A spec whose :effect is marked is destructive:
   normalize and compose set an absent :confirm :destructive? to true, and
   destructive? reads the mark on a spec neither has seen. A builder marks
   the effect rather than returning a :confirm of its own, so a caller that
   merges its own :confirm with the builder's fragment, in either order,
   keeps its action and cannot turn the confirmation into a bare keypress.
   A caller that says :destructive? false beside the mark is refused
   (destructive-contradiction), since a run would otherwise ask for a typed
   yes the author said it would not. A caller that replaces the :effect
   replaces the mark with it."
  ([f] (destructive nil f))
  ([effect f]
   (let [m (meta f)]
     (if (and (true? (::destructive m)) (or (nil? effect) (= effect (::effect m))))
       f
       (vary-meta f assoc ::destructive true ::effect (or effect (::effect m)))))))

(defn destructive-effect?
  "Whether f is an :effect function marked by destructive."
  [f]
  (and (fn? f) (true? (::destructive (meta f)))))

(defn destructive-effect
  "The name of f as a refusal names a destructive effect, or nil when f is
   not marked by destructive."
  [f]
  (when (destructive-effect? f)
    (or (::effect (meta f)) unnamed-effect)))

(defn destructive?
  "Whether a spec, canonical or not, is destructive: its :confirm says so,
   or its :effect is marked by destructive."
  [spec]
  (or (true? (get-in spec [:confirm :destructive?]))
      (destructive-effect? (:effect spec))))

(defn destructive-contradiction
  "Why a change is refused whose effect `effect` (its name, nil when no
   effect of the change destroys anything) destroys what it acts on while
   its :confirm says :destructive? `stated`, or nil when the two agree. An
   absent or true :destructive? agrees with every effect, and only an
   explicit false beside a destructive effect contradicts it. normalize,
   compose and gumshoe.dsl/compile all refuse through this check, so a
   change is refused in the same words whichever way it was written."
  [effect stated]
  (when (and (some? effect) (false? stated))
    (format "the :effect %s destroys what it acts on, and the :confirm says :destructive? false, which would confirm it with a bare keypress - drop :destructive? false"
            effect)))

(defn- invalid!
  [message data]
  (throw (ex-info message (assoc data ::invalid true))))

(defn invalid?
  "Whether e is normalize refusing a spec."
  [e]
  (boolean (::invalid (ex-data e))))

(defn- stated-destructive
  "What a :confirm says of :destructive?: false when it, or a part compose
   OR-ed into it, said false, else its value. compose keeps a part's false
   on the :confirm it OR-s to true, so a destructive effect composed in
   later is refused in every grouping of the parts."
  [confirm]
  (if (::stated-false (meta confirm)) false (:destructive? confirm)))

(defn- check-destructive!
  "Refuses spec when its :effect is marked destructive and its :confirm
   says :destructive? false."
  [{:keys [effect confirm]}]
  (let [named (destructive-effect effect)]
    (when-let [why (destructive-contradiction named (stated-destructive confirm))]
      (invalid! why {:effect named :confirm confirm}))))

(defn- canonical-subjects?
  [x]
  (sequential? x))

(defn- check-entries!
  [entries]
  (doseq [entry entries]
    (when-not (and (map? entry) (sources (:from entry)))
      (invalid! (format "a :subjects entry names where its ref comes from as :from %s - write :pick, :target or :reads"
                        (pr-str (when (map? entry) (:from entry))))
                {:entry entry})))
  (doseq [[from n] (frequencies (map :from entries))
          :when (< 1 n)]
    (invalid! (format "the :subjects name :from %s %d times - one entry per source" from n)
              {:from from})))

(defn- subjects-of
  "The canonical :subjects of spec: its own when canonical, else built from
   the aliases :subject, :subjects (a function) and :pin, in that order."
  [{:keys [subjects] :as spec}]
  (if (canonical-subjects? subjects)
    (do (when-let [alias (first (filter #(some? (get spec %)) [:subject :pin]))]
          (invalid! (format "the spec names its subjects in :subjects and again as %s - write one or the other" alias)
                    {:alias alias}))
        (check-entries! subjects)
        (doseq [entry subjects
                :let [why (compare-problem (:compare entry))]
                :when why]
          (invalid! why {:entry entry}))
        (vec subjects))
    (let [{:keys [subject pin]} spec]
      (when-let [why (and (map? subject) (compare-problem (:compare subject)))]
        (invalid! why {:subject subject}))
      (cond-> []
        (some? subject) (conj (assoc subject :from :pick))
        (some? subjects) (conj {:from :target :refs subjects})
        (some? pin) (conj {:from :reads :ref pin})))))

(defn- precondition-refusal
  "A :precondition as a refusal returning unmet. It carries the
   precondition, so compose judges one precondition once however many parts
   name it."
  [precondition]
  (with-meta (fn [ctx] (when-not (precondition ctx) unmet))
    {::guard [:precondition precondition]}))

(def problems-forbidden
  "Why a :problems function may not reach outside the process: it judges
   the context the reads already made, so what it decides on is what the
   reads read and nothing a second, unpinned read saw."
  "a :problems function judges the context the change's reads made - declare what it needs in :reads")

(def ^:private shown-at-most 200)

(defn- shown
  [x]
  (let [s (pr-str x)]
    (if (< shown-at-most (count s)) (str (subs s 0 shown-at-most) "...") s)))

(defn- problems-verdict
  "What a :problems function's result x means for the gate: nil to proceed,
   the vector of its messages to refuse with them, or {::malformed <why>}."
  [x]
  (cond
    (nil? x)
    {::malformed "its :problems returned nil, not a sequence of messages - return [] when nothing stands in the way, so an answer nobody gave never reads as no problems"}

    (not (sequential? x))
    {::malformed (format "its :problems returned %s, not a sequence of messages (a vector or a list, empty to proceed)" (shown x))}

    :else
    (let [messages (vec x)]
      (if-let [[bad] (some #(when-not (and (string? %) (not (str/blank? %))) [%]) messages)]
        {::malformed (format "its :problems returned %s among its messages, and every message is a non-blank string" (shown bad))}
        (not-empty messages)))))

(defn judge-problems
  "What problems, a :problems function, says about ctx as the reason of a
   refusal: nil when it returned an empty sequence, the vector of its
   messages, in order, when it returned any, and one reason naming the
   defect - so the change is refused, never run - when it returned nil,
   something that is not a sequential collection, a message that is not a
   non-blank string, threw, or reached outside the process. It runs inside
   gumshoe.offline/forbidding, its result realized there. A strict read
   that did not answer (an ex-info result/unread-exception? recognises) is
   rethrown, so the engine refuses it in the words it uses for every guard."
  [problems ctx]
  (let [verdict (try
                  (problems-verdict (offline/forbidding problems-forbidden #(problems ctx)))
                  (catch clojure.lang.ExceptionInfo e
                    (cond
                      (result/unread-exception? e) (throw e)
                      (offline/forbidden? e) {::malformed (str "its :problems reached outside the process: " (ex-message e))}
                      :else {::malformed (str "its :problems threw: " (ex-message e))}))
                  (catch Exception e
                    {::malformed (str "its :problems threw: " (or (ex-message e) (.getName (class e))))}))]
    (if (map? verdict) (::malformed verdict) verdict)))

(defn problems-refusal
  "A :problems function as a refusal: judge-problems over the context the
   refusal judges. It carries the function, so compose judges one :problems
   once however many parts name it."
  [problems]
  (with-meta (fn [ctx] (judge-problems problems ctx))
    {::guard [:problems problems]}))

(defn refusal-reason
  "What a :refusal's result why means: nil to let the change run, else the
   reason to refuse with - why itself, except an empty sequential
   collection, which would refuse with nothing to show and is refused with
   a reason naming that instead."
  [why]
  (if (and (sequential? why) (empty? why))
    "its :refusal returned an empty list of reasons - return nil to let the change run"
    why))

(defn refusal-messages
  "The lines a refusal's reason is shown as, each printed after
   \"refusing: \": a vector of one for a string, the messages of a vector,
   and nil for nil and for `unmet`, whose words belong to the engine."
  [why]
  (cond
    (or (nil? why) (= unmet why)) nil
    (sequential? why) (vec why)
    :else [(str why)]))

(declare combined first-reason)

(defn- refusal-of
  "The one guard: :refusal, then :problems as a refusal returning its
   messages, then :precondition as a refusal returning unmet. normalize
   refuses :problems beside :precondition, so at most one of the two is
   there."
  [{:keys [refusal precondition problems] :as spec}]
  (when (and (some? problems) (not (fn? problems)))
    (invalid! (format ":problems is (fn [ctx] -> a sequence of messages, empty to proceed), not %s" (shown problems))
              {:problems problems}))
  (when (and (some? problems) (some? precondition))
    (invalid! (str "the spec has :problems and :precondition - a precondition refuses without saying why, so a"
                   " spec with both prints one gate's reasons and enforces another's; fold the precondition into"
                   " :problems as a message")
              {:problems problems :precondition precondition}))
  (let [guards (remove nil? [refusal
                             (some-> problems problems-refusal)
                             (some-> precondition precondition-refusal)])]
    (if (next guards)
      (combined :refusal first-reason guards)
      (first guards))))

(defn- select-of
  [{:keys [select empty-message]}]
  (let [inner (:empty-message select)]
    (cond
      (nil? empty-message) select
      (and (some? inner) (not= inner empty-message))
      (invalid! (format "the spec has two empty messages, %s at the top and %s in :select - keep the one in :select"
                        (pr-str empty-message) (pr-str inner))
                {:empty-message empty-message :select-empty-message inner})
      :else (assoc select :empty-message empty-message))))

(defn- announce-of
  [{:keys [announce] :as spec}]
  (if-not (contains? spec :announce?)
    announce
    (let [announce? (:announce? spec)]
      (cond
        (and announce? (false? announce))
        (invalid! "the spec says :announce false and :announce? true - drop :announce?" {})
        announce? announce
        (and (some? announce) (not (false? announce)))
        (invalid! "the spec has an :announce message and :announce? false, so it is unclear whether it posts to the changelog - drop one"
                  {})
        :else false))))

(defn- check-named!
  "Refuses an entry map of key that is not {keyword -> function}, or that
   binds a name the engine binds itself or a reserved name other than
   reserved."
  [key entries reserved]
  (when-not (map? entries)
    (invalid! (format "%s is a map of name to (fn [ctx] -> value), not %s" key (pr-str entries))
              {key entries}))
  (doseq [[name f] entries]
    (when-not (keyword? name)
      (invalid! (format "%s names its entries with keywords, not %s" key (pr-str name)) {key name}))
    (when-not (and (ifn? f) (not (coll? f)))
      (invalid! (format "the %s entry %s is a function of the context, not %s" key name (pr-str f))
                {key name}))
    (when (and (= (namespace ::here) (namespace name)) (not= reserved name))
      (invalid! (format "%s is reserved and is not a %s entry" name key) {key name}))
    (when (context-keys name)
      (invalid! (format "the %s entry %s would hide the %s the engine hands every callback - name it otherwise"
                        key name name)
                {key name}))))

(defn- named-of
  "The canonical entry map of key: its own, checked, or {reserved alias}
   for the alias spelling; nil when the spec has neither. Both spellings
   are refused."
  [spec key alias reserved]
  (let [entries (get spec key)
        f (get spec alias)]
    (when (some? entries) (check-named! key entries reserved))
    (cond
      (nil? f) (not-empty entries)
      (some? entries)
      (invalid! (format "the spec has %s and %s - write what %s returns as %s entries" alias key alias key)
                {:alias alias})
      :else {reserved f})))

(defn- check-entries-apart!
  "Refuses an alias entry beside a named entry, and a name both maps bind."
  [{:keys [reads values]}]
  (let [named (concat (keys (dissoc reads recheck-entry)) (keys (dissoc values derive-entry)))]
    (doseq [[reserved alias] [[derive-entry :derive] [recheck-entry :recheck]]
            :when (and (or (contains? values reserved) (contains? reads reserved)) (seq named))]
      (invalid! (format "the spec has %s beside the named entries %s, which a %s map could hide - write what %s returns as named :values and :reads"
                        alias (pr-str (vec (sort named))) alias alias)
                {:alias alias :named (vec named)}))
    (when-let [both (seq (filter (set (keys reads)) (keys values)))]
      (invalid! (format "%s is both a read and a value - a read is read again after the confirmation, a value never, so name one of them otherwise"
                        (first (sort both)))
                {:both (vec both)}))))

(defn- done-of
  "The canonical :done of spec: :done or its alias :satisfied?, true or a
   function, nil when absent (nil or false)."
  [spec]
  (let [given (filter #(some? (get spec %)) [:done :satisfied?])]
    (when (= 2 (count given))
      (invalid! "the spec has :done and :satisfied? - write one of them" {}))
    (let [done (some #(get spec %) [:done :satisfied?])]
      (cond
        (or (nil? done) (false? done)) nil
        (or (true? done) (fn? done)) done
        :else (invalid! (format ":done is true, to skip the change when its post-checks already hold, or (fn [ctx] -> bool), not %s"
                                (pr-str done))
                        {:done done})))))

(defn- check-done!
  [{:keys [done verify confirm]}]
  (cond
    (and (true? done) (nil? verify))
    (invalid! ":done true skips the change when its post-checks already hold, and the spec has no :verify"
              {:done done})
    (and done (nil? verify) (true? (:destructive? confirm)))
    (invalid! "a destructive change is skipped as done only on evidence its post-check accepts, so its :done needs a :verify"
              {:done done})))

(defn- kube-context-of
  "The canonical :kube-context of spec: a function, or nil when absent."
  [spec]
  (let [f (:kube-context spec)]
    (when-not (or (nil? f) (fn? f))
      (invalid! (format ":kube-context is (fn [opts] -> a kube context name or nil), not %s" (pr-str f))
                {:kube-context f}))
    f))

(defn- check-runnable!
  [{:keys [select subjects refusal reads connect kube-context]}]
  (when (and (contains? reads recheck-entry) (nil? refusal))
    (invalid! "a :recheck is judged by :refusal, :problems or :precondition, and the spec has none of them"
              {:recheck (get reads recheck-entry)}))
  (when (and connect kube-context)
    (invalid! "a spec takes :connect, for what is not a kube cluster, or :kube-context, for the kube context its kubectl calls use - not both"
              {:connect connect :kube-context kube-context}))
  (when (and connect (seq subjects))
    (invalid! "a :subject, :subjects or :pin reads its objects with kubectl, so a spec with :connect takes neither"
              {:subjects subjects}))
  (when (and (= :many (:mode select)) (some #(= :pick (:from %)) subjects))
    (invalid! "a :subject needs a select that picks one object, not a :many select"
              {:select select})))

(defn- canonical
  "spec with every alias replaced by its canonical form and each key checked
   on its own, without the checks that need the whole change (check-done!,
   check-runnable!), so a fragment compose combines passes through it."
  [spec]
  (when-not (map? spec)
    (invalid! (str "a change spec is a map, not " (pr-str spec)) {:spec spec}))
  (let [subjects (subjects-of spec)
        refusal (refusal-of spec)
        select (select-of spec)
        announce (announce-of spec)
        reads (named-of spec :reads :recheck recheck-entry)
        values (named-of spec :values :derive derive-entry)
        done (done-of spec)
        kube-context (kube-context-of spec)
        _ (check-destructive! spec)
        canonical (cond-> (dissoc spec :subject :subjects :pin :precondition :problems :refusal
                                  :empty-message :announce? :announce :select
                                  :derive :recheck :reads :values :done :satisfied?
                                  :kube-context)
                    (some? kube-context) (assoc :kube-context kube-context)
                    (seq subjects) (assoc :subjects subjects)
                    (seq reads) (assoc :reads reads)
                    (seq values) (assoc :values values)
                    (some? refusal) (assoc :refusal refusal)
                    (some? select) (assoc :select select)
                    (some? announce) (assoc :announce announce)
                    (some? done) (assoc :done done)
                    (destructive-effect? (:effect spec)) (assoc-in [:confirm :destructive?] true))]
    (check-entries-apart! canonical)
    canonical))

(defn normalize
  "spec as the canonical change map (see the namespace doc): every alias
   replaced by its canonical form, every other key carried as it is.
   Idempotent, pure, and throws ex-info with ::invalid for a spec it
   refuses."
  [spec]
  (doto (canonical spec)
    (check-done!)
    (check-runnable!)))

(defn subject
  "The entry of a canonical spec's :subjects that comes :from source, or nil."
  [spec source]
  (some #(when (= source (:from %)) %) (:subjects spec)))

(defn pick-subject
  "The :pick entry of a canonical spec: what its :subject alias declared."
  [spec]
  (subject spec :pick))

(defn pin
  "The function of a canonical spec's :reads entry: what its :pin alias
   declared."
  [spec]
  (:ref (subject spec :reads)))

(defn reads
  "The named live reads of a canonical spec, {name -> (fn [ctx] -> value)},
   without the entry a :recheck alias became."
  [spec]
  (not-empty (dissoc (:reads spec) recheck-entry)))

(defn values
  "The named per-run values of a canonical spec, {name -> (fn [ctx] ->
   value)}, without the entry a :derive alias became."
  [spec]
  (not-empty (dissoc (:values spec) derive-entry)))

(defn derive-alias
  "The function a canonical spec's :derive alias declared, or nil."
  [spec]
  (get-in spec [:values derive-entry]))

(defn done
  "The :done of a canonical spec: true, a function, or nil."
  [spec]
  (:done spec))

(defn recheck-alias
  "The function a canonical spec's :recheck alias declared, or nil."
  [spec]
  (get-in spec [:reads recheck-entry]))

;; --- composition ---------------------------------------------------------------------

(defn- parts-of
  "The functions f combines for key, or [f] when it combines none."
  [key f]
  (or (get-in (meta f) [::parts key]) [f]))

(defn- combined
  "One function for key standing for the distinct functions of given (by
   identity, a :precondition's or :problems' refusal by that function, in order, each
   combined one flattened into what it combines), built by (join fs). A given function that already stands for exactly
   those is returned as it is, so combining a function with one it already
   combines yields the same function; nil when there are none."
  [key join given]
  (let [given (remove nil? given)
        identity-of #(or (::guard (meta %)) %)
        fs (->> (mapcat #(parts-of key %) given)
                (reduce (fn [[seen acc :as kept] f]
                          (if (contains? seen (identity-of f))
                            kept
                            [(conj seen (identity-of f)) (conj acc f)]))
                        [#{} []])
                second)]
    (when (seq fs)
      (or (some #(when (= fs (parts-of key %)) %) given)
          (with-meta (join fs) {::parts {key fs}})))))

(defn- first-reason
  [fs]
  (fn [ctx] (some #(% ctx) fs)))

(defn- concatenated
  [fs]
  (fn [ctx] (into [] (mapcat #(% ctx)) fs)))

(defn- all-hold
  [fs]
  (fn [ctx] (every? #(% ctx) fs)))

(defn- ref-key [{:keys [type namespace name]}] [type namespace name])

(defn- distinct-refs!
  [refs]
  (doseq [[[type namespace name] n] (frequencies (map ref-key refs))
          :when (< 1 n)]
    (invalid! (format "two parts of the change name the subject %s %s - a subject is read, pinned and refused once, so one part names it"
                      type (if namespace (str namespace "/" name) name))
              {:ref {:type type :namespace namespace :name name}}))
  refs)

(defn- joined-refs
  [fs]
  (fn [ctx] (distinct-refs! (into [] (mapcat #(% ctx)) fs))))

(defn- single
  "The one value two parts give key: either, when only one gives it or both
   give the same."
  [key a b]
  (cond
    (nil? a) b
    (nil? b) a
    (= a b) a
    :else (invalid! (format "two parts of the change give %s differently - a change has one %s, so one part names it"
                            key key)
                    {:key key :values [a b]})))

(defn- by-name
  "The entries of two {name -> function} maps as one, in part order; a name
   both bind is one entry when they bind the same function and refused
   otherwise."
  [key a b]
  (let [entries (reduce (fn [acc [name f]]
                          (if-let [seen (some #(when (= name (first %)) %) acc)]
                            (if (= (second seen) f)
                              acc
                              (invalid! (format "two parts of the change bind the %s entry %s to different functions - name one of them otherwise"
                                                key name)
                                        {key name}))
                            (conj acc [name f])))
                        []
                        (concat a b))]
    (when (seq entries) (apply array-map (mapcat identity entries)))))

(defn- either-true
  [a b]
  (cond
    (or (true? a) (true? b)) true
    (or (false? a) (false? b)) false
    :else nil))

(defn- combine-confirm
  [a b]
  (let [combined (not-empty
                  (into {}
                        (keep (fn [k]
                                (let [v (if (#{:destructive? :separately?} k)
                                          (either-true (get a k) (get b k))
                                          (single [:confirm k] (get a k) (get b k)))]
                                  (when (some? v) [k v]))))
                        (distinct (concat (keys a) (keys b)))))]
    (cond-> combined
      (and (true? (:destructive? combined))
           (some #(false? (stated-destructive %)) [a b]))
      (vary-meta assoc ::stated-false true))))

(def ^:private option-identity [:coerce :alias :default])

(defn- combine-options
  [a b]
  (reduce-kv (fn [acc k v]
               (if-let [seen (get acc k)]
                 (if (= (select-keys seen option-identity) (select-keys v option-identity))
                   acc
                   (invalid! (format "two parts of the change declare --%s differently: %s and %s"
                                     (name k) (pr-str seen) (pr-str v))
                             {:option k :declarations [seen v]}))
                 (flags/merge-ordered acc {k v})))
             (or a {})
             (or b {})))

(defn- combine-prerequisites
  [a b]
  (try
    (prerequisites/union a b)
    (catch clojure.lang.ExceptionInfo e
      (invalid! (str "two parts of the change cannot share their prerequisites: " (ex-message e))
                (ex-data e)))))

(defn- combine-subjects
  "Two canonical :subjects vectors as one, in order of first appearance per
   :from: the :target entries' refs joined, a :pick or :reads entry
   single-valued."
  [a b]
  (let [entries (concat a b)]
    (not-empty
     (vec (for [from (distinct (map :from entries))
                :let [same (filter #(= from (:from %)) entries)]]
            (if (= :target from)
              (assoc (reduce (fn [x y] (merge-with #(single [:subjects :target] %1 %2) x y))
                             (map #(dissoc % :refs) same))
                     :refs (combined :refs joined-refs (map :refs same)))
              (reduce #(single [:subjects from] %1 %2) same)))))))

(defn- combine-done
  [a b]
  (let [given (remove nil? [a b])
        fns (remove true? given)]
    (cond
      (empty? given) nil
      (empty? fns) true
      :else (combined :done all-hold fns))))

(def ^:private reason-separator "; ")

(defn- reasons
  "Two parts' reasons for key as one, each distinct reason once, in part
   order. A reason that is not a string is single-valued, and conformance
   says what is wrong with it."
  [key a b]
  (if (and (string? a) (string? b))
    (str/join reason-separator (distinct (mapcat #(str/split % #"; ") [a b])))
    (single key a b)))

(defn- guarded?
  [{:keys [refusal subjects]}]
  (or (some? refusal) (seq subjects)))

(defn- check-reasons!
  "Refuses a composed change that says it has no post-check or no guard
   while one of its parts gives it one. Judged on the result, so it holds
   for every grouping of the parts."
  [{:keys [no-verify no-guard verify] :as composed}]
  (when (and (some? no-verify) (some? verify))
    (invalid! (format "a part of the change says it has no post-check (%s) and a part declares a :verify - drop the reason"
                      (pr-str no-verify))
              {:no-verify no-verify}))
  (when (and (some? no-guard) (guarded? composed))
    (invalid! (format "a part of the change says it has no guard (%s) and a part declares a :refusal or :subjects - drop the reason"
                      (pr-str no-guard))
              {:no-guard no-guard})))

(def ^:private rules
  {:reads #(by-name :reads %1 %2)
   :values #(by-name :values %1 %2)
   :refusal #(combined :refusal first-reason [%1 %2])
   :verify #(combined :verify concatenated [%1 %2])
   :items #(combined :items concatenated [%1 %2])
   :done combine-done
   :subjects combine-subjects
   :confirm combine-confirm
   :prerequisites combine-prerequisites
   :options combine-options
   :no-verify #(reasons :no-verify %1 %2)
   :no-guard #(reasons :no-guard %1 %2)})

(defn- combine
  ([] {})
  ([a b]
   (into {}
         (keep (fn [k]
                 (let [x (get a k) y (get b k)
                       v (cond
                           (nil? x) y
                           (nil? y) x
                           :else ((get rules k #(single k %1 %2)) x y))]
                   (when (some? v) [k v]))))
         (distinct (concat (keys a) (keys b))))))

(defn compose
  "One canonical change map from several specs, each a fragment or a whole
   spec in any spelling normalize accepts, combined key by key as the
   namespace doc lists; nil parts are skipped. Pure and associative, and
   composing a result with a part it holds again returns it unchanged.
   Throws ex-info with ::invalid for parts that clash."
  [& parts]
  (doto (reduce combine (map canonical (remove nil? parts)))
    (check-entries-apart!)
    (check-reasons!)
    (check-destructive!)))
