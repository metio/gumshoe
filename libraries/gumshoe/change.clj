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
      :select   {:mode .. :label .. :candidates .. :empty-message \"...\" ..}
      :subjects [{:from :pick   :type .. :parse .. :invalid .. :facts ..}
                 {:from :target :refs (fn [ctx] -> refs)}
                 {:from :reads  :ref  (fn [ctx] -> ref or nil)}]
      :reads    {name (fn [ctx] -> value)}
      :values   {name (fn [ctx] -> value)}
      :refusal  (fn [ctx] -> nil, a reason, or gumshoe.change/unmet)
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
   a reason to refuse it with \"refusing: <reason>\", or `unmet` to refuse
   it in the engine's generic words (\"its precondition does not hold\"
   before the confirmation, gumshoe.flow's \"its precondition no longer
   holds\" after it), which add nothing of their own.

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
   :from, two different empty messages, a changelog message beside
   :announce? false, a :recheck with nothing to judge it, a :subject on a
   :many select, :connect with any subject, :derive beside :values or
   :recheck beside :reads, either alias beside a named entry, a :reads or
   :values that is not a map of keyword to function, a name the engine
   binds itself or both maps bind, and a reserved name in the wrong map. mutation/book normalizes
   when the book loads, so such a book fails the conformance gate.")

(def unmet
  "What a :refusal returns to refuse in the generic words of gumshoe.flow:
   the reason of a :precondition that returned false."
  ::unmet)

(def ^:private sources #{:pick :target :reads})

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

(defn- invalid!
  [message data]
  (throw (ex-info message (assoc data ::invalid true))))

(defn invalid?
  "Whether e is normalize refusing a spec."
  [e]
  (boolean (::invalid (ex-data e))))

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
        (vec subjects))
    (let [{:keys [subject pin]} spec]
      (cond-> []
        (some? subject) (conj (assoc subject :from :pick))
        (some? subjects) (conj {:from :target :refs subjects})
        (some? pin) (conj {:from :reads :ref pin})))))

(defn- refusal-of
  "The one guard: :refusal, then :precondition as a refusal returning unmet."
  [{:keys [refusal precondition]}]
  (cond
    (nil? precondition) refusal
    (nil? refusal) (fn [ctx] (when-not (precondition ctx) unmet))
    :else (fn [ctx] (or (refusal ctx) (when-not (precondition ctx) unmet)))))

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

(defn- check-runnable!
  [{:keys [select subjects refusal reads connect]}]
  (when (and (contains? reads recheck-entry) (nil? refusal))
    (invalid! "a :recheck is judged by :refusal or :precondition, and the spec has neither"
              {:recheck (get reads recheck-entry)}))
  (when (and connect (seq subjects))
    (invalid! "a :subject, :subjects or :pin reads its objects with kubectl, so a spec with :connect takes neither"
              {:subjects subjects}))
  (when (and (= :many (:mode select)) (some #(= :pick (:from %)) subjects))
    (invalid! "a :subject needs a select that picks one object, not a :many select"
              {:select select})))

(defn normalize
  "spec as the canonical change map (see the namespace doc): every alias
   replaced by its canonical form, every other key carried as it is.
   Idempotent, pure, and throws ex-info with ::invalid for a spec it
   refuses."
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
        canonical (cond-> (dissoc spec :subject :subjects :pin :precondition :refusal
                                  :empty-message :announce? :announce :select
                                  :derive :recheck :reads :values :done :satisfied?)
                    (seq subjects) (assoc :subjects subjects)
                    (seq reads) (assoc :reads reads)
                    (seq values) (assoc :values values)
                    (some? refusal) (assoc :refusal refusal)
                    (some? select) (assoc :select select)
                    (some? announce) (assoc :announce announce)
                    (some? done) (assoc :done done))]
    (check-entries-apart! canonical)
    (check-done! canonical)
    (check-runnable! canonical)
    canonical))

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
