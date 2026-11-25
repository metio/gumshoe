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
      :derive   (fn [ctx] -> map)
      :recheck  (fn [ctx] -> map)
      :refusal  (fn [ctx] -> nil, a reason, or gumshoe.change/unmet)
      :confirm  {:action .. :destructive? .. :separately? ..}
      :items    (fn [ctx] -> seq)
      :announce (fn [ctx] -> message), or false for a change that posts nothing
      :effect   (fn [ctx] -> plan)
      :verify   (fn [ctx] -> post-checks)   or  :no-verify \"why\"
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

   :refusal is the one guard. It runs before the confirmation, against the
   derived context with what :recheck returns merged over it, and again
   after the confirmation, against the context the subjects and :recheck
   read again (see gumshoe.mutation). It returns nil to let the change run,
   a reason to refuse it with \"refusing: <reason>\", or `unmet` to refuse
   it in the engine's generic words (\"its precondition does not hold\"
   before the confirmation, gumshoe.flow's \"its precondition no longer
   holds\" after it), which add nothing of their own.

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
   A nil alias is absent, except :announce?, which the runbook harness reads
   as given.

   normalize refuses, with an ex-info carrying ::invalid, a spec that names
   one thing two ways or that the engine could not run as written: a
   canonical :subjects beside :subject or :pin, an unknown or repeated
   :from, two different empty messages, a changelog message beside
   :announce? false, a :recheck with nothing to judge it, a :subject on a
   :many select, and :connect with any subject. mutation/book normalizes
   when the book loads, so such a book fails the conformance gate.")

(def unmet
  "What a :refusal returns to refuse in the generic words of gumshoe.flow:
   the reason of a :precondition that returned false."
  ::unmet)

(def ^:private sources #{:pick :target :reads})

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

(defn- check-runnable!
  [{:keys [select subjects refusal recheck connect]}]
  (when (and recheck (nil? refusal))
    (invalid! "a :recheck is judged by :refusal or :precondition, and the spec has neither"
              {:recheck recheck}))
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
        canonical (cond-> (dissoc spec :subject :subjects :pin :precondition :refusal
                                  :empty-message :announce? :announce :select)
                    (seq subjects) (assoc :subjects subjects)
                    (some? refusal) (assoc :refusal refusal)
                    (some? select) (assoc :select select)
                    (some? announce) (assoc :announce announce))]
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
