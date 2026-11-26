;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation
  "A declarative shape for the many books that are the same underneath: pick a
   subject, then run a confirmed, announced, verified change against it. Such a
   book is a map - what to select, what to confirm, the effect plan, the
   post-checks - and this engine composes it out of gumshoe.select, gumshoe.flow,
   and gumshoe.effect. The bespoke pieces (the candidate query, the effect, the
   checks) stay small, named, and testable; the flow around them is written and
   tested once, here.

   book, run!, run-on!, parse-pick and details first map a spec onto the
   canonical change map with gumshoe.change/normalize, and the engine reads
   only that map. The keys below are what books write; :subject, a
   :subjects function, :pin, :precondition, :derive, :recheck, a top-level
   :empty-message and :announce? are aliases normalize keeps indefinitely, and a spec naming
   one thing two ways is refused when the book loads (see gumshoe.change).
   A spec built from parts - a step library's fragment and the book's own
   keys - is combined with gumshoe.change/compose, which merges the parts
   key by key and never drops a part's refusal or post-check as merge
   would.

   Spec:
     {:description   \"...\"
      :options       {..cli options for the target flags..}
      :prerequisites {..}
      :select {:mode :one | :many | :namespaced
               :label \"Node\"
               :candidates (fn [context] -> seq of names)
               :flag :node                      ; :one / :many
               :namespace-flag :namespace       ; :namespaced
               :name-flag :name                 ; :namespaced
               :preview \"kubectl describe node {}\"}  ; :one, optional fzf side pane
      :empty-message \"every node is already cordoned\"   ; when nothing to pick
      :confirm  {:action \"...\" :destructive? false
                 :separately? false}   ; true: asks again inside a playbook
      :subject  {:type \"cronjobs\"                ; the object the change acts on, see below
                 :parse pick/namespaced          ; optional, the pick as a ref
                 :invalid \"...\"                 ; optional, why a pick that does not parse is refused
                 :facts (fn [object] -> value)   ; optional, joins the pin
                 :compare :spec}                 ; optional, see below
      :subjects (fn [ctx] -> seq of {:type :name :namespace? :facts? :compare? :optional?})
                                                  ; optional, the several objects the
                                                  ; change acts on, see below
      :refusal  (fn [ctx] -> reason or nil)       ; optional, judged before the confirmation
                                                  ; and again after it; a reason is
                                                  ; printed as \"refusing: <reason>\"
      :precondition (fn [ctx] -> bool)            ; optional, judged after :refusal, before
                                                  ; and after the confirmation;
                                                  ; reads only, a dry run calls it too
      :announce (fn [ctx] -> changelog message)   ; optional
      :effect   (fn [ctx] -> effect plan)         ; one effect runs as a plan of one;
                                                  ; anything else is refused naming
                                                  ; the step (:title, else the action)
      :verify   (fn [ctx] -> seq of post-check maps)   ; optional
      :done     true or (fn [ctx] -> bool)  ; optional, already satisfied:
                                    ; the run skips the change, see below
      :satisfied? ..                ; optional, alias of :done
      :reads    {name (fn [ctx] -> value)}   ; optional, live reads, read before the
                                    ; confirmation and again after it for :refusal;
                                    ; strict, see below
      :values   {name (fn [ctx] -> value)}   ; optional, computed once per run and
                                    ; shared by :items, :effect and :verify, see below
      :derive   (fn [ctx] -> map)   ; optional, alias: values merged into ctx once;
                                    ; its reads are strict, see below
      :recheck  (fn [ctx] -> map)   ; optional, alias: read again after the confirmation
                                    ; and merged over ctx for :refusal and :precondition
                                    ; only, see below
      :items    (fn [ctx] -> seq)   ; optional, overrides the confirmation items
      :pin      (fn [ctx] -> {:type :name :namespace? :facts? :compare?})  ; optional, see below
      :recovery \"...\" or (fn [ctx] -> string)   ; optional, printed under the
                                    ; FAILED banner when this change fails,
                                    ; see gumshoe.playbook and below
      :connect  (fn [opts] -> {:context .. :cluster .. :announce-as ..} or nil)
                                    ; optional, what the change acts on when that
                                    ; is not the kube context, see below
      :announce? false  ; optional, handed to the runbook harness; a book whose
                        ; change nobody else needs to hear of (a debug session)
                        ; posts nothing to the changelog. It is still a change:
                        ; pre-hooks see :change? true and no reproducer of its
                        ; calls is offered
      :confirmed? true}  ; set by a playbook whose confirmation covered this step

   The :refusal, :precondition, :announce, :effect, :verify, :derive,
   :recheck and :items callbacks, and every :reads and :values entry, each
   receive one context map, so a book destructures exactly what it needs:
     {:context .. :cluster .. :target <the pick(s)> :opts <parsed flags>
      :ref .. :object ..     ; with a :subject
      :refs .. :objects ..   ; with :subjects
      <name> ..}             ; each of :reads and :values

   :done declares when the change is already satisfied, so a run that finds
   it so skips it instead of confirming and applying it again: an
   interrupted playbook run again resumes past the steps that took effect.
   It is judged once its :derive phase read the evidence, before the
   judgement before the confirmation and before anything is asked, in that
   phase with its read cache and strict reads (done-verdict). `true` judges
   the post-checks :verify returns, each attempted once as
   gumshoe.verify/attempt runs them; a function judges the derived context
   and, beside a :verify, is joined by those post-checks, so a change is
   never skipped on evidence its post-check would not accept. A read that
   did not answer is never done: the change then goes on as one that is not
   done, saying it could not tell. A skipped change prints what held and
   \"skipped, already done: <step>\", records itself with
   gumshoe.outcome/skipped!, asks nothing, runs no effect and no
   post-check, and returns true. A rehearsal judges it too. :refusal is
   judged only for a change that is not done, so a change whose refusal
   says \"it is suspended already\" is skipped as done before it would be
   refused. A playbook judges its steps' :done before its confirmation as
   far as their evidence exists (see gumshoe.playbook).

   :reads and :values split what a change derives by when it is true. A
   read is live state the change rests on (which locations are available,
   whether a listener drifted): it is read before the confirmation, after
   the subjects, all reads at once, and again after the confirmation, after
   the subjects are read again, when the spec has a :refusal, which judges
   the fresh value under the same name. :items, :announce, :effect and
   :verify keep the value read before the confirmation, which the operator
   approved and the plan was computed from; a post-check that needs the
   live value calls read-again. A value is fixed by the run (a timestamp, a
   generated name, an operator's choice, a snapshot the confirmation
   shows): it is computed once, after the reads, and never again, so the
   effect stamps and the post-check waits for the same token. A spec writes
   either these or the :derive and :recheck aliases, never both (see
   gumshoe.change).

   :subject declares the one object a change acts on. :parse turns the pick
   into a ref {:type :namespace :name}, plus whatever else the pick carries
   (a kind, a revision); it defaults to gumshoe.pick/namespaced for a
   :namespaced select and gumshoe.pick/cluster-scoped for a :one select, and
   :type fills in a ref whose parser names no type. A :many select picks
   several objects and takes no :subject. The engine then hands every
   callback the ref as :ref and the object as :object, read strictly before
   :derive runs; pins the ref, with :facts, unless the spec names its own
   :pin; and reads the object again right before :refusal and :precondition,
   so both judge the object as it is when the change is about to run.
   read-subject reads the object again, for a post-check.

   Every pick is judged by parse-pick before anything else runs - before
   :connect, the reads, :derive and the confirmation - by run!, run-on! and
   every playbook step alike. With a :subject a pick that does not parse is
   refused with :invalid. Without one, a :namespaced pick must be
   \"<namespace>/<name>\", a :many pick a non-empty collection of non-blank
   strings and any other pick a non-blank string; anything else is refused
   as \"the pick <pick> does not name a <label>\". A refused pick prints
   \"refusing: <why>\" and the change returns false.

   :subjects declares the several objects one change acts on, for a change
   whose confirmation rests on a set (the volume, the database and the routes
   an adoption stamps). It receives the context the subject read made - the
   pick as :target, plus :ref and :object with a :subject - and returns refs
   {:type :name :namespace?}, each optionally with :facts (fn [object] ->
   value), which joins that object's pin, and :optional? true for an object
   that may not exist. A :many select may declare :subjects; its :target is
   the picked names. The engine does for each of them what it does for a
   :subject: reads every one strictly before :derive and hands the callbacks
   :refs, the refs in the order declared, and :objects, the objects in the
   same order (nil for an optional object that does not exist); reads every
   one again right before :refusal and :precondition, which see those as
   :objects; and pins every one, so the change is refused when one was
   replaced, its spec changed, its :facts moved, an optional one appeared, or
   one that is not optional did not exist when the change was confirmed. The
   refusal names the object, as \"refusing: <type> <namespace>/<name>:
   <why>\". :subjects composes with :subject and :pin; each is pinned.
   read-subjects reads them again, for a post-check.

   :recheck reads again, after the confirmation, the live state :refusal and
   :precondition judge, for a change whose judgement rests on reads no
   :subject or :subjects declares (which adoptees exist, whether a listener
   drifted). It runs right before :refusal, after the subject and subjects
   are read again, and receives the context they judge: the derived values,
   the fresh :object and :objects. The map it returns is merged over that
   context for :refusal and :precondition alone; :items, :announce, :effect
   and :verify keep what :derive returned, which is what the confirmation
   showed and what the effect plan was computed from. A spec whose reads
   are all reads of the live state can name one function as both :derive
   and :recheck; one that needs the value it confirmed next to the value now
   returns the new one under its own key. Its reads are strict, as those of
   :precondition are: a read that did not answer refuses the change with
   the reason and no judgement runs. :recheck composes with :subject,
   :subjects and :pin, whose drift checks still follow the judgement, and a
   spec with :recheck needs a :refusal or a :precondition to judge it.

   *phase* names the phase whose reads are running: :derive for the reads
   before the confirmation (the subject and subjects reads, :subjects,
   :reads, :values, :derive and the fingerprint of a :pin), :judge for the
   reads after it (those reads and :reads again, :recheck, :refusal,
   :precondition and the drift checks of every pin). Each phase
   runs inside one binding of it, so no read after the confirmation runs
   inside the binding of the reads before it.

   Each phase also binds a fresh kubectl/*read-cache*, so within one phase
   an object read twice - by the subject read and by :derive, by :refusal
   and by the drift check of its pin - is fetched once, and both judge the
   same answer. The cache of a phase is bound inside that phase only: no
   read after the confirmation is served from a read before it, and a
   post-check, which runs outside both phases, reads the cluster on every
   poll. A rehearsal under --dry-run has the same two phases, each with its
   own cache. A playbook hands the first change it judged before its own
   confirmation the cache of that judgement: that :derive phase is the one
   the playbook's confirmation rests on, as a standalone change's rests on
   its own.

   Independent reads run concurrently: read-subjects reads its refs at
   once, and the :judge phase starts by fetching every object it is about
   to judge (the subject, the pinned object and the subjects) at once into
   its cache. The named :reads of a phase are read at once, too. A :derive
   or :recheck that makes several independent reads issues them together
   with gumshoe.concurrent/values, whose futures
   carry *phase*, the cache and kubectl/*strict-reads* along.

   :refusal returns why the change must not run, or nil. The engine prints
   the reason as \"refusing: <reason>\" and stops, and only a change no
   refusal stopped reaches :precondition.

   A change is judged twice: before the confirmation is shown, so the
   operator is never asked to approve a change that will be refused, and
   again after it, over fresh reads, which guards the window between the
   two. The judgement before (prejudgement) calls :refusal, with
   :precondition folded into it, with the one signature it has after the
   confirmation, over the derived context with what :recheck returns for it
   merged over it, both in the :derive *phase* with that phase's read
   cache; a value only :recheck reads is therefore real before the
   confirmation, never missing, so nothing is refused for a key :derive
   does not provide. A refusal that must tell the two apart reads *phase*.
   It then refuses a pinned object or a subject that is not there
   (gone-before-confirmation); drift needs a confirmed fingerprint, so it is
   judged after the confirmation alone. A precondition that returned false
   prints \"refusing: its precondition does not hold\" before the
   confirmation, and gumshoe.flow's own words after it. What the judgement
   before prints is held back unless it refuses, so a change that runs
   prints its judgement once, after the confirmation, where it always did.
   It reads nothing :derive already read; a :recheck or :refusal reading
   something :derive did not, or reading outside kubectl (SSH), reads it
   once more before the confirmation. A rehearsal asks for no confirmation
   and judges once, as gumshoe.flow rehearses a precondition, and so does
   a change a playbook's confirmation covered (:confirmed?), which the
   playbook judged before its own confirmation where its evidence existed
   (see gumshoe.playbook).

   The subject read, :reads, :values, :derive, :refusal and :precondition run under
   kubectl/*strict-reads* (the :subjects reads and the :subjects function
   too), as the
   candidate query of :select and every post-check do. A read in :derive that
   did not answer stops the change before the confirmation, so neither the
   precondition nor the effect is computed from a read that stood in for an
   object nobody could see. A read in :precondition that did not answer
   refuses the change with the reason. A read that answered that its object
   or resource type does not exist still returns nil, which the precondition
   judges. Only the gumshoe.kubectl readers that honour *strict-reads* are
   strict here (see its docstring).

   :connect names what the change acts on when that is not the cluster of
   the kube context: an SSH host, a storage cluster's API. It receives the
   parsed flags and returns :context, which the candidate query, every
   callback and the effect receive in place of the kube context; :cluster,
   the name the confirmation shows the change acting on; and :announce-as,
   the system the changelog names, defaulting to :cluster. It returns nil
   when what it names cannot be reached, having said why, and the change
   stops before anything is listed. Without :connect the context is the
   current kube context and :cluster its cluster. :subject, :subjects and
   :pin read their objects with kubectl, so a spec with :connect takes none
   of them.

   A change without :recovery that fails after applying an effect - a later
   effect failed, its post-check did not hold, or an error stopped it - is
   still followed by guidance: `fallback-guidance` names the effects that
   stayed applied, where it stopped, and where to look (the messages of the
   failure, the changelog entry it posted, the object on its cluster). It is
   recorded with gumshoe.outcome/fallback!, so the guidance of a playbook
   running the change as a step wins over it, and a rehearsal records none.

   Three entry points, same spec: `book` is a standalone runbook; `run!` selects
   the subject and runs it, returning a boolean; `run-on!` runs against a subject
   already chosen, so a playbook picks once and drives many steps without
   re-prompting.

   :pin names the object the change acts on. Its fingerprint is taken when the
   confirmation is shown and taken again right before the effect; when the
   object was replaced (another uid) or its spec changed (another generation),
   the change is refused, because the operator confirmed an object that no
   longer exists in that form. Status and metadata updates leave the
   generation alone, so a controller reporting progress does not trip it. An
   object without a generation is compared by resourceVersion, unless its
   subject or ref names :compare :spec, for a type the API server never
   gives a generation although it splits spec from status (a Node): then
   its uid and spec are compared, and a refusal names the spec keys that
   moved, as \"its spec changed after the confirmation: taints\". See
   gumshoe.change/comparisons. A change that
   rests on something outside the spec - an annotation, a status field - names
   it with :facts (fn [object] -> value), which must be equal too. An object
   that cannot be read before the confirmation is refused before it is
   shown, and in a change whose confirmation a playbook covered, when it is
   shown."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [gumshoe.change :as change]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.select :as select]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
            [gumshoe.outcome :as outcome]
            [gumshoe.pick :as pick]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

;; --- the confirmed object --------------------------------------------------------

(defn check-compare!
  "compare when it names one of gumshoe.change/comparisons or is nil;
   throws otherwise, so
   a misspelt :compare never silently falls back to another comparison."
  [compare]
  (when-let [why (change/compare-problem compare)]
    (throw (ex-info why {:compare compare})))
  compare)

(defn fingerprint
  "What identifies the object the operator confirmed: its uid, its generation
   or, for an object without one, what compare names (see
   gumshoe.change/comparisons; resourceVersion when nil), and the value of facts when given."
  ([object] (fingerprint object nil nil))
  ([object facts] (fingerprint object facts nil))
  ([object facts compare]
   (check-compare! compare)
   (when object
     (let [{:keys [uid generation resourceVersion]} (:metadata object)]
       (cond-> (cond
                 generation {:uid uid :generation generation}
                 (= :spec compare) {:uid uid :spec (or (:spec object) {})}
                 :else {:uid uid :resource-version resourceVersion})
         facts (assoc :facts (facts object)))))))

(defn ref-fingerprint
  "The fingerprint of object as the ref that names it declares it: with its
   :facts and its :compare."
  [ref object]
  (fingerprint object (:facts ref) (:compare ref)))

(defn- moved-keys
  "The keys of two maps whose values differ, sorted, as text."
  [before after]
  (->> (concat (keys before) (keys after))
       distinct
       (remove #(= (get before %) (get after %)))
       (map #(if (keyword? %) (name %) (str %)))
       sort
       (str/join ", ")))

(defn drift
  "Why the object no longer is the one confirmed, or nil."
  [before after]
  (cond
    (nil? before) nil
    (nil? after) "it was deleted after the confirmation"
    (not= (:uid before) (:uid after)) "it was deleted and created again after the confirmation"
    (and (:generation before) (not= (:generation before) (:generation after)))
    (format "its spec changed after the confirmation (generation %s, now %s)" (:generation before) (:generation after))
    (and (contains? before :spec) (not= (:spec before) (:spec after)))
    ;; an after without :spec was fingerprinted by another comparison (the
    ;; object gained a generation), so no key-by-key difference is named
    (let [moved (when (contains? after :spec) (moved-keys (:spec before) (:spec after)))]
      (if (str/blank? moved)
        "its spec changed after the confirmation"
        (str "its spec changed after the confirmation: " moved)))
    (and (:resource-version before) (not= (:resource-version before) (:resource-version after)))
    "it changed after the confirmation"
    (not= (:facts before) (:facts after))
    (format "what the change rests on moved after the confirmation (%s, now %s)" (pr-str (:facts before)) (pr-str (:facts after)))
    :else nil))

(defn- refuse!
  "Prints why the change is refused on stderr, beside the banner that ends
   the run, and returns false, the verdict of a precondition that refused."
  [why]
  (stdout/err-println (str "refusing: " why))
  false)

(defn fetch-pinned
  [context {:keys [type namespace name]}]
  (if namespace
    (kubectl/get-namespaced-resource context namespace type name)
    (kubectl/get-cluster-resource context type name)))

(defn pinned-precondition
  "The book's precondition followed by the drift check. fetch is
   (fn [ref] -> object); before is the fingerprint taken at confirmation.
   With a ref but no before, the object was not read when the change was
   confirmed - a read that did not answer looks the same as a missing
   object - so nothing ties what the effect would act on to what the
   operator confirmed, and the change is refused."
  [precondition ref before fetch]
  (if-not ref
    precondition
    (fn []
      (and (if precondition (precondition) true)
           (let [why (if before
                       (drift before (ref-fingerprint ref (fetch ref)))
                       "it could not be read when the change was confirmed")]
             (if why (refuse! why) true))))))

(def ^:dynamic *phase*
  "The phase of a change whose reads are running: :derive before the
   confirmation, :judge after it, nil outside both (see the namespace doc)."
  nil)

(defn- in-phase
  "thunk running with *phase* bound to phase and kubectl/*read-cache* bound
   to cache, a fresh one for each call when none is given; nil without a
   thunk. A thunk called twice - a precondition a rehearsal and a playbook
   both run - reads afresh each time."
  ([phase thunk] (in-phase phase nil thunk))
  ([phase cache thunk]
   (when thunk
     (fn [] (binding [*phase* phase
                      kubectl/*read-cache* (or cache (kubectl/read-cache))]
              (thunk))))))

(defn derived
  "{:values <what derive returned>} or {:unread <reason>}. derive runs under
   kubectl/*strict-reads*, in the :derive *phase* with cache as its
   kubectl/*read-cache* (a fresh one when none is given), and its result is
   realized inside the binding, so a lazy seq cannot run its reads after it,
   and a read that did not answer surfaces as its message instead of as nil
   - the same contract as the candidate listing of a select. A read that
   answered that its object does not exist still derives nil."
  ([derive base] (derived derive base nil))
  ([derive base cache]
   (try
     {:values (when derive
                ((in-phase :derive cache
                           #(binding [kubectl/*strict-reads* true]
                              (walk/postwalk identity (derive base))))))}
     (catch clojure.lang.ExceptionInfo e
       (if (::kubectl/unread (ex-data e)) {:unread (ex-message e)} (throw e))))))

(defn strict-precondition
  "precondition as a thunk whose reads are strict: a read in it that did not
   answer refuses with the reason instead of being judged as an absent
   object."
  [precondition]
  (when precondition
    (fn []
      (try
        (binding [kubectl/*strict-reads* true]
          (boolean (precondition)))
        (catch clojure.lang.ExceptionInfo e
          (if (::kubectl/unread (ex-data e))
            (refuse! (str "could not read what the precondition rests on - " (ex-message e)))
            (throw e)))))))

(defn subject-ref
  "The ref {:type :namespace :name ..} a :subject makes of target, or nil
   when target does not parse into an object with a type and a name."
  [{:keys [type parse]} select target]
  (when (= :many (:mode select))
    (throw (ex-info "a :subject needs a select that picks one object, not a :many select"
                    {:select select})))
  (let [parse (or parse (if (= :namespaced (:mode select)) pick/namespaced pick/cluster-scoped))
        ref (some->> (parse target) (merge (when type {:type type})))]
    (when (and (:type ref) (:name ref)) ref)))

(defn read-subject
  "The object the ref of ctx names, read now, or nil without a ref. A
   post-check calls it to see the subject after the change."
  [{:keys [context ref]}]
  (when ref (fetch-pinned context ref)))

(defn describe-ref
  "A ref as a refusal names it: \"<type> <namespace>/<name>\", or
   \"<type> <name>\" for a cluster-scoped object."
  [{:keys [type namespace name]}]
  (str type " " (when namespace (str namespace "/")) name))

(defn subjects-of
  "The refs the :target subjects of a spec (its :subjects function) return
   for ctx, as a vector, or nil without them. A ref without a :type and a
   :name is a mistake in the spec and throws."
  [spec ctx]
  (when-let [subjects (:refs (change/subject (change/normalize spec) :target))]
    (let [refs (vec (subjects ctx))]
      (doseq [ref refs]
        (when-not (and (:type ref) (:name ref))
          (throw (ex-info "a :subjects ref needs a :type and a :name" {:ref ref}))))
      refs)))

(defn read-subjects
  "The objects the refs name, read now, in the same order - nil for an
   object that does not exist. Every read of the :subjects of a change goes
   through here, one call per phase with the whole set, so the phase is the
   unit a cache works on; the reads run concurrently. Under
   kubectl/*strict-reads* a read that did not answer throws. A post-check
   calls it with the :refs of ctx to see the objects after the change."
  [context refs]
  (concurrent/all (map (fn [ref] #(fetch-pinned context ref)) refs)))

(defn subjects-fingerprints
  "The fingerprint of each object, as its ref declares it (ref-fingerprint)."
  [refs objects]
  (mapv ref-fingerprint refs objects))

(defn subjects-drift
  "Why the first of the refs no longer is the object confirmed, named as
   \"<ref>: <why>\", or nil. before and after are the fingerprints taken at
   the confirmation and right before the effect, in the order of refs. A ref
   without :optional? must have existed at the confirmation; an optional one
   that did not must not exist now either."
  [refs before after]
  (some (fn [[ref b a]]
          (when-let [why (cond
                           (and (nil? b) (not (:optional? ref))) "it did not exist when the change was confirmed"
                           (nil? b) (when a "it was created after the confirmation")
                           :else (drift b a))]
            (str (describe-ref ref) ": " why)))
        (map vector refs before after)))

(defn- subjects-precondition
  "precondition followed by the drift check of every subject, or
   precondition unchanged without subjects. read is (fn [refs] -> objects)."
  [precondition refs before read]
  (if (empty? refs)
    precondition
    (fn []
      (and (if precondition (precondition) true)
           (let [why (subjects-drift refs before (subjects-fingerprints refs (read refs)))]
             (if why (refuse! why) true))))))

(defn- invalid-pick
  [{:keys [invalid]} select target]
  (or invalid (format "the pick %s does not name a %s" (pr-str target) (or (:label select) "subject"))))

(defn- named?
  "Whether x is text naming something: a string that is not blank."
  [x]
  (and (string? x) (not (str/blank? x))))

(defn- accepted?
  "Whether a spec without a :subject accepts target as its select's pick: a
   :many select a non-empty collection of names, a :namespaced select
   \"<namespace>/<name>\", any other a name. A :one pick and a pick of a
   step without a select stay free-form text the book reads itself."
  [{:keys [mode]} target]
  (case mode
    :many (and (coll? target) (not (map? target)) (seq target) (every? named? target))
    :namespaced (some? (pick/namespaced target))
    (named? target)))

(defn parse-pick
  "{:ref <ref>} for a pick the spec accepts, or {:refusal <why>} for one it
   refuses. With a :subject the ref is what subject-ref makes of target,
   which must be a non-blank string, and a target it cannot parse is refused
   with the subject's :invalid; without one the ref is nil and target must
   be what the select picks (accepted?), or it is refused in the default
   words. Pure, so run!, run-on! and every playbook step judge a pick the
   same way before anything is read."
  [spec target]
  (let [{:keys [select] :as spec} (change/normalize spec)
        subject (change/pick-subject spec)
        refused {:refusal (invalid-pick subject select target)}]
    (if subject
      (let [ref (subject-ref subject select target)]
        (if (and ref (named? target)) {:ref ref} refused))
      (if (accepted? select target) {:ref nil} refused))))

(defn- realized
  "(f ctx), realized, so a lazy seq runs its reads inside the caller's
   bindings."
  [f ctx]
  (walk/postwalk identity (f ctx)))

(defn read-named
  "{name -> value} for the named reads of a spec, each read for ctx, all at
   once (gumshoe.concurrent/values), in the caller's phase, cache and
   strictness."
  [reads ctx]
  (concurrent/values (into {} (map (fn [[name read]] [name #(realized read ctx)])) reads)))

(defn- compute-values
  "{name -> value} for the named values of a spec, each computed once for
   ctx, one after the other in the calling thread, so a value that asks the
   operator asks in the order declared."
  [values ctx]
  (into {} (map (fn [[name value]] [name (realized value ctx)])) values))

(defn read-again
  "The named read `name` of the change whose context ctx is, read now, for
   a post-check: the value the confirmation showed is (name ctx), what the
   cluster holds now is this. A name the change does not read throws."
  [ctx name]
  (if-let [read (get-in ctx [::reads name])]
    (realized read ctx)
    (throw (ex-info (format "the change reads nothing named %s" name)
                    {:name name :reads (vec (keys (::reads ctx)))}))))

(defn- subject-derive
  "The reads of the :derive phase as one function of the base context, which
   holds the :ref parse-pick made: the :pick and :target subjects ({:ref
   :object}, {:refs :objects}), then the named :reads, then the named
   :values, then what a :derive alias returns for the context holding them
   all, merged in that order. nil when the spec has none of them."
  [spec]
  (let [derive (change/derive-alias spec)
        reads (change/reads spec)
        values (change/values spec)
        pick? (some? (change/subject spec :pick))
        target? (some? (change/subject spec :target))]
    (if (or pick? target? reads values)
      (fn [{:keys [ref context] :as base}]
        (let [found (when pick? {:ref ref :object (read-subject base)})
              found (if target?
                      (let [refs (subjects-of spec (merge base found))]
                        (assoc found :refs refs :objects (read-subjects context refs)))
                      found)
              found (if reads
                      (merge found (read-named reads (merge base found)) {::reads reads})
                      found)
              found (if values
                      (merge found (compute-values values (merge base found)))
                      found)]
          (merge found (when derive (derive (merge base found))))))
      derive)))

(defn- rechecked
  "ctx as :refusal and :precondition judge it: the subject and the subjects
   read again, then the named :reads read again and merged over it, then
   what a :recheck alias returns for that merged over it. Every value is
   realized here, so its reads run in the caller's bindings."
  [spec {:keys [ref refs context] :as ctx}]
  (let [reads (change/reads spec)
        recheck (change/recheck-alias spec)
        ctx (if ref (assoc ctx :object (read-subject ctx)) ctx)
        ctx (if (seq refs) (assoc ctx :objects (read-subjects context refs)) ctx)
        ctx (if reads (merge ctx (read-named reads ctx)) ctx)]
    (if recheck
      (merge ctx (realized recheck ctx))
      ctx)))

(defn- judged-precondition
  "The :refusal of a spec as a thunk over ctx, or nil without one. It judges
   ctx as rechecked makes it; a reason is printed as \"refusing: <reason>\",
   and change/unmet refuses in gumshoe.flow's own words. A pick that does
   not parse never reaches here, parse-pick refused it."
  [{:keys [refusal] :as spec} ctx]
  (when refusal
    (fn []
      (let [why (refusal (rechecked spec ctx))]
        (cond
          (= change/unmet why) false
          why (refuse! why)
          :else true)))))

(defn object-key
  "What identifies the object a ref names, whatever else the ref carries
   (:facts, :optional?): [type namespace name]."
  [{:keys [type namespace name]}]
  [type namespace name])

(defn judged-refs
  "The refs whose objects the :judge phase of spec reads, each once, in the
   order first named: the subject's ref when a refusal reads it again, the
   pinned ref, then the subjects. Pure, so the prefetch is tested apart from
   the reads."
  [spec {:keys [ref refs]} pinned]
  (let [{:keys [refusal]} spec]
    (->> (concat (when (and refusal ref) [ref]) (when pinned [pinned]) refs)
         (reduce (fn [[seen out] r]
                   (let [k (object-key r)]
                     (if (seen k) [seen out] [(conj seen k) (conj out r)])))
                 [#{} []])
         second)))

(defn- prefetching
  "thunk preceded by a concurrent, strict read of the objects refs name,
   which only fills the phase's cache: what thunk reads of them is then
   served from it. A read that did not answer here is not kept and is
   issued again, in thunk, by the read whose contract decides what its
   failure means. With fewer than two objects nothing runs concurrently and
   the cache already serves a second read of one, so thunk runs alone."
  [context refs thunk]
  (if (and thunk (<= 2 (count refs)))
    (fn []
      (try
        (binding [kubectl/*strict-reads* true]
          (read-subjects context refs))
        (catch clojure.lang.ExceptionInfo e
          (when-not (::kubectl/unread (ex-data e)) (throw e))))
      (thunk))
    thunk))

(defn- pin-of
  "The ref the change pins, its fingerprint at the confirmation and where
   the ref comes from (:reads or :pick): the ref of its :reads subject when
   it has one, else the :pick subject's ref, whose object ctx already holds
   from the strict read before the confirmation."
  [spec {:keys [context ref object] :as ctx}]
  (let [pin (change/pin spec)
        subject (change/pick-subject spec)]
    (cond
      pin (when-let [pinned (pin ctx)]
            {:ref pinned :from :reads
             :before (ref-fingerprint pinned (fetch-pinned context pinned))})
      (and subject ref) (let [pinned (merge ref (select-keys subject [:facts :compare]))]
                          {:ref pinned :from :pick :before (ref-fingerprint pinned object)})
      :else nil)))

(defn gone-before-confirmation
  "Why the objects a change pins rule it out before the confirmation, or
   nil: the pinned object is absent - for a :pick subject, whose read was
   strict, it does not exist; for a :reads pin, whose read is not strict, it
   does not exist or the cluster did not answer - or a subject that is not
   :optional? does not exist. pinned is {:ref :from :before} as the
   confirmation takes it; refs and objects are the subjects as :derive read
   them. Drift is not judged here: before the confirmation the fingerprints
   are the ones just taken, so the only drift is an object that is not
   there. Pure."
  [pinned refs objects]
  (or (when (and (:ref pinned) (nil? (:before pinned)))
        (if (= :pick (:from pinned))
          "it does not exist"
          "it does not exist or could not be read"))
      (some (fn [[ref object]]
              (when (and (nil? object) (not (:optional? ref)))
                (str (describe-ref ref) ": it does not exist")))
            (map vector refs objects))))

(def unmet-before-confirmation
  "The reason a refusal returning change/unmet - a :precondition that
   returned false - prints before the confirmation."
  "its precondition does not hold")

(defn prejudgement
  "Why the change must not be offered for confirmation, judged over the
   evidence its :derive phase read, or nil to ask. ctx is the derived
   context, pinned what pin-of took from it and cache the :derive phase's
   kubectl/*read-cache*.

   The :refusal runs first, with the signature and the context shape it has
   after the confirmation: the derived context, with what :recheck returns
   for it merged over it, so a refusal reading a value only :recheck
   provides judges a real value, never a missing one. :recheck and
   :refusal run in the :derive *phase* with cache and strict reads, so a
   read :derive already made is served from cache and a read that did not
   answer refuses with its reason. Then gone-before-confirmation judges
   the pinned object and the subjects."
  [{:keys [refusal] :as spec} ctx pinned cache]
  (or (when refusal
        (try
          ((in-phase :derive cache
                     #(binding [kubectl/*strict-reads* true]
                        (let [recheck (change/recheck-alias spec)
                              judged (if recheck
                                       (merge ctx (realized recheck ctx))
                                       ctx)
                              why (refusal judged)]
                          (if (= change/unmet why) unmet-before-confirmation why)))))
          (catch clojure.lang.ExceptionInfo e
            (if (::kubectl/unread (ex-data e))
              (str "could not read what the precondition rests on - " (ex-message e))
              (throw e)))))
      (gone-before-confirmation pinned (:refs ctx) (:objects ctx))))

;; --- already done ---------------------------------------------------------------------

(defn- unread-message
  "The message of e when it is a strict read that did not answer, else nil."
  [e]
  (when (::kubectl/unread (ex-data e)) (ex-message e)))

(defn judge-done
  "Whether a change is already done, decided from the judgements it is
   handed and nothing else: `done` is true, or a thunk calling the spec's
   :done function with the context; `checks` are the post-checks its
   :verify returned for that context, or nil without a :verify; `attempt`
   runs one check as gumshoe.verify/attempt does: true when it held, the
   message of a read that did not answer, else false.

   Returns {:done? true :held [<description> ..]}, {:done? false} or
   {:unread <why>}. The function runs first; a false verdict ends the
   judgement, and so does the first post-check that does not hold. With
   :done true and no post-check there is nothing to judge, so it is not
   done. A read the function makes that did not answer is {:unread}, and
   so is a check whose read did not answer: a change is never done on a
   read that did not answer."
  [done checks attempt]
  (if-not done
    {:done? false}
    (let [by-fn (when (fn? done)
                  (try (if (done) :held :not-done)
                       (catch clojure.lang.ExceptionInfo e
                         (if-let [why (unread-message e)] {:unread why} (throw e)))))]
      (cond
        (map? by-fn) by-fn
        (= :not-done by-fn) {:done? false}
        (and (true? done) (empty? checks)) {:done? false}
        :else (loop [[check & more] checks held []]
                (if-not check
                  {:done? true :held held}
                  (let [result (attempt (:check check))]
                    (cond
                      (true? result) (recur more (conj held (:description check)))
                      (string? result) {:unread result}
                      :else {:done? false}))))))))

(defn done-verdict
  "judge-done for spec over ctx: its :done, with ctx, and the post-checks
   its :verify returns for ctx, attempted with strict reads. The caller
   binds the phase and the read cache; strict reads are bound here. A
   :verify whose read did not answer while it built its checks is
   {:unread}, as a check's would be."
  [spec ctx]
  (let [done (change/done spec)
        verify-fn (:verify spec)]
    (if-not done
      {:done? false}
      (try
        (binding [kubectl/*strict-reads* true]
          (judge-done (if (fn? done) #(done ctx) done)
                      (when verify-fn (vec (verify-fn ctx)))
                      verify/attempt))
        (catch clojure.lang.ExceptionInfo e
          (if-let [why (unread-message e)] {:unread why} (throw e)))))))

(defn step-name
  "What a skip, a refusal and an error name a change by: its :title, else
   the action it confirms."
  [spec]
  (or (:title spec) (get-in spec [:confirm :action])))

(defn skipped!
  "Prints a change skipped as done - each post-check that held, then that it
   was skipped - and records it with gumshoe.outcome/skipped!. Returns true,
   the result of a skipped change."
  [spec {:keys [held]}]
  (doseq [description held]
    (stdout/ok (str "already holds: " description)))
  (stdout/ok (str "skipped, already done: " (step-name spec)))
  (outcome/skipped! (step-name spec))
  true)

(defn undecided!
  "Prints that whether the change is done could not be told, why, and that
   it is judged as a change that is not done."
  [why]
  (stdout/warn (str "could not tell whether this change is already done - " why
                    " - judging it as a change that is not done")))

(defn- judged-done
  "done-verdict of spec over ctx in the :derive *phase* with cache: the
   verdict when the change is done, else nil, having said so when a read
   did not answer."
  [spec ctx cache]
  (when (change/done spec)
    (let [{:keys [done? unread] :as verdict} ((in-phase :derive cache #(done-verdict spec ctx)))]
      (cond
        done? verdict
        unread (do (undecided! unread) nil)
        :else nil))))

(defn- recording-writer
  "A Writer appending what is written to it to chunks as [stream text]."
  [chunks stream]
  (proxy [java.io.Writer] []
    (write
      ([x]
       (swap! chunks conj [stream (if (integer? x) (str (char x)) (String/valueOf x))]))
      ([buf off len]
       (swap! chunks conj [stream (if (string? buf)
                                    (subs buf off (+ off len))
                                    (String. ^chars buf (int off) (int len)))])))
    (flush [])
    (close [])))

(defn- replay!
  [chunks]
  (doseq [[stream text] chunks]
    (let [^java.io.Writer w (if (= :err stream) *err* *out*)]
      (.write w ^String text)
      (.flush w))))

(defn held
  "[what f returned, what it printed], with what f printed to *out* and
   *err* held back instead of printed: a judgement before the confirmation
   that lets the change run prints nothing, because the same judgement
   after the confirmation prints it again where it always did. A judgement
   that refuses replays its output with replay-held! before the reason, so
   the operator reads what led to the refusal. Should f throw, what it
   printed is replayed before the exception propagates."
  [f]
  (let [chunks (atom [])
        result (try
                 (binding [*out* (recording-writer chunks :out)
                           *err* (recording-writer chunks :err)]
                   (f))
                 (catch Throwable e
                   (replay! @chunks)
                   (throw e)))]
    [result @chunks]))

(defn replay-held!
  "Prints the output held returned, each part to the stream it was written
   to, in the order written."
  [chunks]
  (replay! chunks))

(defn- prejudged-reason
  "prejudgement with its output held back unless it refuses: the reason,
   printed after what the judgement printed, or nil, having printed
   nothing."
  [spec ctx pinned cache]
  (let [[why chunks] (held #(prejudgement spec ctx pinned cache))]
    (when why (replay-held! chunks))
    why))

(defn- prejudges?
  "Whether a run of spec judges its refusal before asking: a real run that
   asks for a confirmation. A rehearsal asks for none and judges the
   refusal once, as gumshoe.flow runs it; a change a playbook's
   confirmation covered (:confirmed?) asks for none either, and the
   playbook judged it before its own confirmation where it could."
  [spec]
  (not (or flow/*dry-run* (:confirmed? spec))))

(defn- connect
  "{:context :cluster :announce-as} for the spec: what its :connect returns
   for opts, or nil when that could not be reached; the kube context and its
   cluster without :connect. change/normalize refuses :connect beside any
   subject."
  [{:keys [connect]} opts]
  (if connect
    (when-let [connection (connect opts)]
      (update connection :announce-as #(or % (:cluster connection))))
    (let [context (kubectl/current-context)
          cluster (kubectl/current-cluster)]
      {:context context :cluster cluster :announce-as cluster})))

(defn fallback-guidance
  "The guidance under the FAILED banner of a change that declares no
   :recovery and applied an effect before it failed. `applied` are the
   effects that stayed applied, as gumshoe.effect/describe renders them;
   `stopped` the effect it stopped at, when one failed; `post-check-failed?`
   whether its post-check did not hold; `announced?` whether it posted to the
   changelog; `subject` what it acted on and `cluster` where."
  [{:keys [applied stopped post-check-failed? announced? subject cluster]}]
  (str/join "\n"
            (concat ["This change declares no recovery guidance. What it left behind:"
                     (format "  %d %s of this change stayed applied - nothing was rolled back:"
                             (count applied) (if (= 1 (count applied)) "effect" "effects"))]
                    (map #(str "    " %) applied)
                    (cond
                      stopped [(str "  it stopped at: " stopped)]
                      post-check-failed? ["  its post-check did not hold"]
                      :else ["  an unexpected error stopped it"])
                    ["Look at:"]
                    (cond
                      stopped ["  the messages of the effect that failed, above"]
                      post-check-failed? ["  the post-check messages above"]
                      :else ["  the error above"])
                    (when announced? ["  the changelog entry this change posted"])
                    [(str "  the current state of " subject " on " cluster)])))

(defn- left-behind
  "fallback-guidance for a change that failed after running the first
   effects of `plan`, counting from `applied-before`, the effects the run had
   applied when the change began; nil when none of its effects that ran
   changes anything, and outside a run, which records nothing. effect/run!
   applies a plan in order, stops at the first failure and counts only the
   effects that may change something, so the applied effects are the
   plan's first such ones."
  [{:keys [select announce] :as spec} {:keys [cluster target]} plan applied-before]
  (let [{:keys [applied stopped verified]} (outcome/current)
        changed (->> (when applied plan)
                     (filter effect/changes-state?)
                     (take (- (or applied 0) applied-before))
                     (mapv effect/describe))]
    (when (seq changed)
      (fallback-guidance {:applied changed
                          :stopped stopped
                          :post-check-failed? (false? verified)
                          :announced? (boolean announce)
                          :subject (str/join ", " (select/items (:mode select) target))
                          :cluster cluster}))))

(defn- prepared
  "The derived context of spec against target, whose ref parse-pick made,
   on the connection, read in the :derive *phase* with cache:
   {:ctx <the context every callback receives>} or {:unread <reason>}."
  [spec {:keys [context cluster]} target ref opts announcement-data cache]
  (let [base (cond-> {:context context :cluster cluster :target target
                      :opts opts :announcement-data announcement-data}
               (change/pick-subject spec) (assoc :ref ref))
        {:keys [values unread]} (derived (subject-derive spec) base cache)]
    (if unread {:unread unread} {:ctx (merge base values)})))

(defn- unread!
  [why]
  (stdout/error (format "could not read what the change rests on - %s" why))
  false)

(defn- run-prepared!
  "Runs spec over ctx, the context its :derive phase made with
   derive-cache: the judgement before the confirmation, then the change
   through gumshoe.flow."
  [{:keys [select confirm announce effect verify] :as spec}
   {:keys [context cluster announce-as]} target announcement-data derive-cache ctx]
  (let [applied-before (outcome/applied-count)
        plan (volatile! nil)
        failed! #(when-not flow/*dry-run*
                   (if (:recovery spec)
                     (outcome/recovery! (:recovery spec) ctx)
                     (when-let [guidance (left-behind spec ctx (effect/as-plan @plan) applied-before)]
                       (outcome/fallback! guidance ctx))))
        result (try
                 (let [{:keys [ref before] :as pinned} ((in-phase :derive derive-cache #(pin-of spec ctx)))
                       refused (when (prejudges? spec) (prejudged-reason spec ctx pinned derive-cache))
                       refs (:refs ctx)
                       subjects-before (subjects-fingerprints refs (:objects ctx))
                       items-fn (:items spec)]
                   (if refused
                     (do (refuse! refused) ::refused)
                     (flow/change!
                      {:confirmation {:action (:action confirm)
                                      :target cluster
                                      :items (if items-fn (items-fn ctx) (select/items (:mode select) target))
                                      :destructive? (:destructive? confirm)}
                       :precondition (let [guard (strict-precondition
                                                  (subjects-precondition
                                                   (pinned-precondition (judged-precondition spec ctx)
                                                                        ref before #(fetch-pinned context %))
                                                   refs subjects-before #(read-subjects context %)))]
                                       (in-phase :judge (prefetching context (judged-refs spec ctx ref) guard)))
                       :confirmed? (:confirmed? spec)
                       :announce! (when announce
                                    #(announce/announce! announce-as announcement-data (announce ctx)))
                       :effect (vreset! plan (effect ctx))
                       :step (step-name spec)
                       :post-checks (when verify (verify ctx))})))
                 (catch Throwable e (failed!) (throw e)))]
    (cond
      (= ::refused result) false
      result true
      :else (do (failed!) false))))

(defn- run-connected!
  "Runs spec against target, whose ref parse-pick made, on the connection.
   cache is the :derive phase's read cache, a fresh one when nil; derived,
   when given, is the context that phase already made, which the change
   then runs on instead of deriving it again, so its :values are computed
   once. A change whose :derive phase prejudge already made was judged not
   done there, so only a change derived here judges its :done."
  [spec connection target ref opts announcement-data cache derived on-skip]
  (let [derive-cache (or cache (kubectl/read-cache))
        {:keys [ctx unread]} (if derived
                               {:ctx derived}
                               (prepared spec connection target ref opts announcement-data derive-cache))
        done (when-not (or unread derived) (judged-done spec ctx derive-cache))]
    (cond
      unread (unread! unread)
      done (do (when on-skip (on-skip)) (skipped! spec done))
      :else (run-prepared! spec connection target announcement-data derive-cache ctx))))

(defn prejudge
  "What a playbook needs to judge a change step before its own confirmation
   covers it, against target: {:refusal <why>} when parse-pick refuses the
   pick or prejudgement refuses the change, with :printed, the output the
   refusing judgement held back (see held), {:unread <why>} when a read of
   its :derive phase did not answer, nil when what :connect names could
   not be reached (connect said why), {:done <done-verdict>} when the change
   is already done, which is judged first, else {:cache <the :derive
   phase's read cache> :derived <the context it made>}, with :undecided
   <why> when a read its :done judged did not answer. Handing the cache and
   the context to run-on! makes the step's :derive phase the one judged
   here, so the step reads nothing twice and computes its :values once; a
   step whose :done is undecided is better derived again when reached, so
   its evidence is read afresh then. Prints nothing; prejudge-printed
   prints what it returned."
  [spec target opts announcement-data]
  (let [spec (change/normalize spec)
        {:keys [ref refusal]} (parse-pick spec target)]
    (if refusal
      {:refusal refusal}
      (when-let [connection (connect spec opts)]
        (let [cache (kubectl/read-cache)
              {:keys [ctx unread]} (prepared spec connection target ref opts announcement-data cache)
              verdict (when (and (not unread) (change/done spec))
                        ((in-phase :derive cache #(done-verdict spec ctx))))]
          (cond
            unread {:unread unread}
            (:done? verdict) {:done verdict}
            :else (let [pinned ((in-phase :derive cache #(pin-of spec ctx)))
                        [why chunks] (held #(prejudgement spec ctx pinned cache))]
                    (if why
                      {:refusal why :printed chunks}
                      (cond-> {:cache cache :derived ctx}
                        (:unread verdict) (assoc :undecided (:unread verdict)))))))))))

(defn prejudge-printed
  "Prints what prejudge returned the way a standalone run prints it - what
   the refusing judgement printed, then the reason - and returns whether
   the change may be offered for confirmation."
  [{:keys [refusal printed unread cache]}]
  (cond
    refusal (do (replay-held! printed) (refuse! refusal))
    unread (unread! unread)
    :else (some? cache)))

(defn run-on!
  "Runs a mutation spec against an ALREADY-CHOSEN `target`, skipping the spec's own
   selection, through derive -> [refusal] -> confirm -> [refusal] -> announce ->
   execute -> verify. Returns true/false, false without asking for a confirmation
   when parse-pick refuses `target`, a read in :derive did not answer or the
   judgement before the confirmation refuses the change. This is what a playbook calls per step after picking its
   subject once (e.g. via gumshoe.select/pick), so the operator is not re-prompted
   for every step. `run!` is this plus selection.

   `opts` is the parsed CLI flags; `announcement-data` is the value the runbook
   action receives in its ctx (nil when the book does not announce).
   `:derive-cache` and `:derived`, in the last argument, are the read cache
   prejudge filled for this change and the context it derived, which the
   change then runs on without deriving it again; `:on-skip`, a function of
   no arguments, is called when the change is skipped as already done."
  ([spec target opts announcement-data]
   (run-on! spec target opts announcement-data nil))
  ([spec target opts announcement-data {:keys [derive-cache derived on-skip]}]
   (let [spec (change/normalize spec)
         {:keys [ref refusal]} (parse-pick spec target)]
     (if refusal
       (refuse! refusal)
       (if-let [connection (connect spec opts)]
         (run-connected! spec connection target ref opts announcement-data derive-cache derived on-skip)
         false)))))

(defn named-but-not-offered
  "Pure: the error for `named`, the target the flags name, when the select
   did not offer it and the run cannot go on, given `found`: :absent when a
   strict read found no such object, :unread when the read did not answer,
   :not-eligible when the object exists but is neither offered nor already
   done (the candidates leave out what the change cannot act on as well as
   what needs no change), :undecided when it exists and the read its :done
   judges did not answer, naming that read as `why`, or nil when nothing
   could read it. A named target is never reported as done on this path,
   and never replaced by another candidate."
  ([select named found]
   (named-but-not-offered select named found nil))
  ([{:keys [label]} named found why]
   (let [shown (if (string? named) named (str/join ", " named))]
     (case found
       :absent (format "no %s %s - not found" label shown)
       :unread (format "could not read %s %s, so whether it exists or needs no change is unknown" label shown)
       :undecided (format "could not tell whether %s %s is already done - the done-check could not be read: %s; it is not among the candidates, so nothing was changed"
                          label shown (or why "a read did not answer"))
       :not-eligible (format "%s %s is present but not eligible for this change - it is not among the candidates and not already done; nothing was changed" label shown)
       (format "%s %s not found among the candidates - this book has no typed subject to read it by, or the name lacks its namespace, so a missing one cannot be told from one that needs no change"
               label shown)))))

(defn- judge-named!
  "The named target exists but the select did not offer it. It succeeds
   only when the spec's own :done judges it already done; a refusal or an
   unanswered read is printed as a run of the change prints it, and an
   object that is neither done nor refused is not eligible, because the
   filter that left it out may be one the change relies on (cnpg restart
   offers healthy clusters only). One whose :done could not be judged,
   because a read it rests on did not answer, is neither: the run fails
   and names that read."
  [spec named opts]
  (let [{:keys [done refusal unread undecided] :as verdict} (prejudge spec named opts nil)]
    (cond
      (nil? verdict) false
      done (skipped! spec done)
      (or refusal unread) (prejudge-printed verdict)
      undecided (do (stdout/error (named-but-not-offered (:select spec) named :undecided undecided)) false)
      :else (do (stdout/error (named-but-not-offered (:select spec) named :not-eligible)) false))))

(defn- not-offered!
  "The flags name `named`, which the select did not offer. The picker is
   never shown, so an operator or an agent that named a target is never
   asked to pick a different one. A spec with a :subject reads the object
   the pick names, whatever kind its :parse derives; an object that exists
   is judged by judge-named!. A :many pick, a select without a :subject, or
   a :namespaced name without its namespace names no object a read can
   find, so the run fails closed."
  [spec named opts context]
  (let [{:keys [select]} spec
        whole? (or (not= :namespaced (:mode select))
                   (str/includes? (str named) "/"))
        {:keys [ref refusal]} (when (and (string? named) whole? (change/pick-subject spec))
                                (parse-pick spec named))]
    (if refusal
      (refuse! refusal)
      (let [found (when (and (:type ref) (:name ref)) (kubectl/presence context ref))]
        (if (= :present found)
          (judge-named! spec named opts)
          (do (stdout/error (named-but-not-offered select named found)) false))))))

(defn pick!
  "Lists the candidates of the spec's select on `context` and resolves the
   pick, honouring the flags in opts. {:target <the pick>} when there is one
   to run against, otherwise {:result <the run's result>} with the reason
   printed: true when nothing is offered and the flags name nothing, or the
   object they name is not offered and its :done judges it already done;
   false when the candidates could not be read, the named object is not
   offered and does not exist, could not be read, is refused or is not
   eligible, or the operator picks nothing. A target the flags name that
   the candidates offer is taken without asking; one they do not offer is
   judged by not-offered! and never falls back to the picker. Every change
   driver (run!, gumshoe.playbook, gumshoe.dispatch) picks through this."
  [spec opts context]
  (let [{:keys [select] :as spec} (change/normalize spec)
        {:keys [names unread]} (select/listed select context)
        named (when-not unread (select/named-target select opts))]
    (cond
      unread
      {:result (select/unread! select unread)}

      (and named (not (select/offered? select named names)))
      {:result (not-offered! spec named opts context)}

      (empty? names)
      {:result (do (stdout/ok (or (:empty-message select) "nothing to do")) true)}

      :else
      (let [target (select/choose select names opts)]
        (if (select/nothing-selected? (:mode select) target)
          {:result (do (stdout/error (format "no %s selected" (:label select))) false)}
          {:target target})))))

(defn run!
  "Selects the spec's subject (see pick!) and runs it through run-on!.
   Returns pick!'s result when it yields no target (nothing to do, a named
   target that is not offered, unreadable candidates, nothing picked), false when
   what :connect names could not be reached or parse-pick refuses the pick,
   otherwise the change's result. Use run! for a single confirmed step; use
   run-on! to compose several steps against one already-picked subject."
  [spec opts announcement-data]
  (let [spec (change/normalize spec)]
    (if-let [connection (connect spec opts)]
      (let [{:keys [target result] :as picked} (pick! spec opts (:context connection))]
        (if (contains? picked :result)
          result
          (let [{:keys [ref refusal]} (parse-pick spec target)]
            (if refusal
              (refuse! refusal)
              (run-connected! spec connection target ref opts announcement-data nil nil nil)))))
      false)))

(defn picks
  "How a select spec chooses its subject, in the words --help shows."
  [{:keys [mode label flag namespace-flag name-flag]}]
  (case mode
    :many (str "one or more of " label (when flag (str " (--" (name flag) ", or interactively)")))
    :namespaced (str "one " label " by namespace and name"
                     (when (and namespace-flag name-flag)
                       (str " (--" (name namespace-flag) " and --" (name name-flag) ", or interactively)")))
    (str "one " label (when flag (str " (--" (name flag) ", or interactively)")))))

(defn- needs
  [prerequisites]
  (when-let [lines (seq (prerequisites/summary prerequisites))]
    (concat ["" "Needs:"] (map #(str "  " %) lines))))

(defn details
  "What --help says a mutation book does, generated from its spec."
  [spec]
  (let [{:keys [select confirm refusal subjects announce verify prerequisites] :as spec} (change/normalize spec)
        recheck (change/recheck-alias spec)
        done (change/done spec)]
    (concat
     ["What it does:"
      (str "  picks " (picks select))]
     (when done [(str "  skips it, asking nothing, when "
                      (cond
                        (true? done) "its post-checks already hold"
                        verify "it is already done and its post-checks already hold"
                        :else "it is already done"))])
     [(str "  asks to confirm: " (:action confirm)
           (when (:destructive? confirm) " (destructive - type yes)"))]
     (when (or refusal recheck (seq subjects))
       ["  checks again right before acting that what was confirmed still holds"])
     (when announce ["  announces the change to the changelog"])
     [(if verify
        "  verifies the result before it reports success"
        "  has no post-check")]
     (needs prerequisites))))

(defn book
  "Builds and runs a standalone mutating runbook from a declarative spec (see the
   namespace doc). To compose several confirmed steps in one playbook, define each
   step's spec and call `run!` per step from a bespoke runbook/execute! :action."
  [spec]
  (let [{:keys [description options prerequisites announce] :as spec} (change/normalize spec)]
    (runbook/execute!
     (cond-> {:description description
              :options (or options {})
              :prerequisites prerequisites
              :details (details spec)
              :change? true
              :action (fn [opts {:keys [announcement-data]}] (run! spec opts announcement-data))}
       (false? announce) (assoc :announce? false)))))
