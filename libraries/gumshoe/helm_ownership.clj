;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.helm-ownership
  "Helm's ownership stamp, put on and taken off a list of objects.

   `helm install` refuses an object that already exists under a name the
   chart renders unless the object carries Helm's ownership metadata for
   that release: the label app.kubernetes.io/managed-by=Helm and the
   annotations meta.helm.sh/release-name and meta.helm.sh/release-namespace.
   Without them it aborts with \"invalid ownership metadata\". Stamping them
   on every such object before the install makes Helm adopt the objects in
   place; stripping them lets a release go of an object it adopted.

   An object is a ref {:type :namespace :name}, :namespace nil for a
   cluster-scoped type. The release namespace is where the release is
   installed and what the annotation records; it need not be the namespace
   of the object.

   Ownership, per object and release (ownership):
     :stamped  the label and both annotations name this release
     :bare     no part of a Helm stamp: no managed-by=Helm label and no
               release annotation (another manager's label is not a stamp)
     :partial  some of this release's stamp but not all of it, such as the
               managed-by=Helm label that `helm template | kubectl apply`
               leaves without the annotations
     :foreign  a release annotation naming another release or another
               release namespace
   A stamp plan never touches a :foreign object and throws when handed one:
   taking an object from another release would let two releases fight over
   it, and the second install or uninstall would change or delete what the
   first one owns.

   The two post-checks are separate functions because an unanswered read
   must satisfy neither. stamped? holds only when every listed object was
   read, exists and carries this release's stamp. no-stamp-confirmed? holds
   only when every listed object was read and none that exists carries any
   part of this release's stamp. (not (stamped? ..)) is not a strip's
   post-check: it is true on a read that did not answer, and would confirm a
   strip the cluster never reported.

   The patch is one JSON merge patch per object, setting the label and both
   annotations in one write (or deleting them, with nulls), so an object is
   never left half stamped between two calls, and merge patch works the
   same for built-in types and custom resources, where a strategic merge
   patch is refused.

   Collection (read-objects, stamped?, no-stamp-confirmed?) is apart from
   judgement (ownership, the *-results? predicates, the refusals and the
   plans), which is pure, so every branch is tested without a cluster."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.effect :as effect]
            [gumshoe.reads :as reads]))

(def managed-by-label "app.kubernetes.io/managed-by")
(def release-name-annotation "meta.helm.sh/release-name")
(def release-namespace-annotation "meta.helm.sh/release-namespace")

;; ---------------------------------------------------------------------------
;; names

(def ^:private release-pattern
  ;; Helm's own rule for a release name: a DNS-1123 subdomain of at most 53
  ;; characters, the room Helm leaves for the suffixes of the objects it names
  #"[a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*")

(def ^:private namespace-pattern #"[a-z0-9]([-a-z0-9]*[a-z0-9])?")

(def ^:private type-pattern #"[A-Za-z0-9]([-A-Za-z0-9.]*[A-Za-z0-9])?")

;; RBAC objects take names such as system:controller:x, so a name is wider
;; than a DNS subdomain; it never starts with a dash, which kubectl would read
;; as a flag
(def ^:private name-pattern #"[A-Za-z0-9][-A-Za-z0-9._:@]*")

(defn release-problem
  "Why release is not a Helm release name, or nil."
  [release]
  (cond
    (not (string? release)) "no release name was given"
    (str/blank? release) "no release name was given"
    (< 53 (count release)) (format "the release name %s is longer than Helm's 53 characters" (pr-str release))
    (not (re-matches release-pattern release)) (format "%s is not a Helm release name (lower-case letters, digits, - and .)" (pr-str release))
    :else nil))

(defn namespace-problem
  "Why namespace is not a namespace name, or nil."
  [namespace]
  (cond
    (or (not (string? namespace)) (str/blank? namespace)) "no namespace was given"
    (< 63 (count namespace)) (format "the namespace %s is longer than 63 characters" (pr-str namespace))
    (not (re-matches namespace-pattern namespace)) (format "%s is not a namespace name" (pr-str namespace))
    :else nil))

(defn ref-problem
  "Why ref {:type :namespace :name} names no object a patch can address, or
   nil. :namespace nil is a cluster-scoped object."
  [{:keys [type namespace name] :as ref}]
  (cond
    (not (map? ref)) (format "%s is not an object ref" (pr-str ref))
    (not (and (string? type) (re-matches type-pattern type))) (format "%s is not a resource type" (pr-str type))
    (not (and (string? name) (<= (count name) 253) (re-matches name-pattern name))) (format "%s is not an object name" (pr-str name))
    (and (some? namespace) (namespace-problem namespace)) (namespace-problem namespace)
    :else nil))

(defn describe
  "An object as the confirmation and the refusals name it: \"<type>
   <namespace>/<name>\", or \"<type> <name>\" for a cluster-scoped one."
  [{:keys [type namespace name]}]
  (str type " " (when namespace (str namespace "/")) name))

(defn parse-object
  "Pure: the ref a command-line object names, or nil. \"<type>/<name>\" is an
   object in default-namespace (the release namespace),
   \"<type>/<namespace>/<name>\" one in another namespace. The ref is not
   checked; ref-problem says what is wrong with it."
  [s default-namespace]
  (when (string? s)
    (let [parts (str/split (str/trim s) #"/" -1)]
      (case (count parts)
        2 {:type (first parts) :namespace default-namespace :name (second parts)}
        3 {:type (first parts) :namespace (second parts) :name (nth parts 2)}
        nil))))

(defn parse-cluster-object
  "Pure: the ref \"<type>/<name>\" names as a cluster-scoped object, or nil."
  [s]
  (when (string? s)
    (let [parts (str/split (str/trim s) #"/" -1)]
      (when (= 2 (count parts))
        {:type (first parts) :namespace nil :name (second parts)}))))

;; ---------------------------------------------------------------------------
;; the stamp

(defn stamp-of
  "The parts of a Helm stamp object carries: {:managed-by :release-name
   :release-namespace}, each nil when absent. The fact a pin compares, so a
   change is refused when the stamp moved after the confirmation."
  [object]
  (let [labels (get-in object [:metadata :labels])
        annotations (get-in object [:metadata :annotations])]
    ;; kubectl's JSON reader keywordizes keys, the dotted ones included
    {:managed-by (get labels (keyword managed-by-label))
     :release-name (get annotations (keyword release-name-annotation))
     :release-namespace (get annotations (keyword release-namespace-annotation))}))


(defn ownership
  "Pure: :stamped, :bare, :partial or :foreign (see the namespace doc) for
   object under the release named release in release-namespace."
  [release release-namespace object]
  (let [stamp (stamp-of object)
        named (:release-name stamp)
        recorded (:release-namespace stamp)
        helm? (= "Helm" (:managed-by stamp))]
    (cond
      (or (and (some? named) (not= release named))
          (and (some? recorded) (not= release-namespace recorded)))
      :foreign

      (and helm? (= release named) (= release-namespace recorded))
      :stamped

      (and (not helm?) (nil? named) (nil? recorded))
      :bare

      :else
      :partial)))

(defn- owner-words
  "The release a :foreign object's annotations name, in words."
  [object]
  (let [{:keys [release-name release-namespace]} (stamp-of object)]
    (str (or release-namespace "?") "/" (or release-name "?"))))

;; ---------------------------------------------------------------------------
;; patches and effects

(defn stamp-patch
  "The JSON merge patch that stamps an object for release in
   release-namespace: the managed-by label and both release annotations,
   set in one write."
  [release release-namespace]
  (json/generate-string
   {:metadata {:labels (sorted-map managed-by-label "Helm")
               :annotations (sorted-map release-name-annotation release
                                        release-namespace-annotation release-namespace)}}))

(def unstamp-patch
  "The JSON merge patch that strips the stamp: a null deletes a key."
  (json/generate-string
   {:metadata {:labels (sorted-map managed-by-label nil)
               :annotations (sorted-map release-name-annotation nil
                                        release-namespace-annotation nil)}}))

(defn- check-ref!
  [ref]
  (when-let [why (ref-problem ref)]
    (throw (IllegalArgumentException. why))))

(defn- patch-effect
  [context {:keys [type namespace name] :as ref} patch]
  (check-ref! ref)
  (apply effect/kubectl context
         (concat (when namespace ["--namespace" namespace])
                 ["patch" type name "--type=merge" "--patch" patch])))

(defn stamp-effect
  "The effect that stamps the object ref names for release in
   release-namespace. Throws for a ref, release or namespace that is not
   well formed, so nothing kubectl would read as a flag reaches it."
  [context release release-namespace ref]
  (when-let [why (or (release-problem release) (namespace-problem release-namespace))]
    (throw (IllegalArgumentException. why)))
  (patch-effect context ref (stamp-patch release release-namespace)))

(defn unstamp-effect
  "The effect that strips Helm's stamp from the object ref names."
  [context ref]
  (patch-effect context ref unstamp-patch))

(defn- foreign!
  [release release-namespace ref object]
  (throw (ex-info (format "%s belongs to release %s, not %s/%s; refusing to take it"
                          (describe ref) (owner-words object) release-namespace release)
                  {::foreign ref})))

(defn stamp-plan
  "The plan that stamps, for release in release-namespace, every object of
   objects that exists and does not carry the stamp yet. refs and objects
   are parallel, as gumshoe.mutation hands them a :subjects change (nil for
   an object that does not exist); the plan reads nothing, so it stamps
   exactly what the operator confirmed. An object stamped already and one
   that does not exist get no effect. Throws for a :foreign object rather
   than take it from its release, and for a ref that is not well formed. An
   empty plan is a note, so a change always has a plan to show."
  [context release release-namespace refs objects]
  (let [effects (vec (for [[ref object] (map vector refs objects)
                           :when object
                           :let [state (ownership release release-namespace object)]
                           :when (not= :stamped state)]
                       (if (= :foreign state)
                         (foreign! release release-namespace ref object)
                         (stamp-effect context release release-namespace ref))))]
    (if (seq effects)
      effects
      (effect/plan (effect/note "every listed object that exists carries the stamp already")))))

(defn unstamp-plan
  "The plan that strips this release's stamp from every object of objects
   that carries any part of it (:stamped or :partial), refs and objects
   parallel as in stamp-plan. A :bare object and one that does not exist get
   no effect. Throws for a :foreign object, whose stamp belongs to another
   release."
  [context release release-namespace refs objects]
  (let [effects (vec (for [[ref object] (map vector refs objects)
                           :when object
                           :let [state (ownership release release-namespace object)]
                           :when (not= :bare state)]
                       (if (= :foreign state)
                         (foreign! release release-namespace ref object)
                         (unstamp-effect context ref))))]
    (if (seq effects)
      effects
      (effect/plan (effect/note "no listed object carries the stamp")))))

;; ---------------------------------------------------------------------------
;; judgement over objects read before the effect

(defn foreign-refusal
  "Why a stamp or a strip for release in release-namespace must not run
   over objects, or nil: some object carries another release's stamp.
   refs and objects parallel."
  [release release-namespace refs objects]
  (let [foreign (for [[ref object] (map vector refs objects)
                      :when (and object (= :foreign (ownership release release-namespace object)))]
                  (str (describe ref) " (release " (owner-words object) ")"))]
    (when (seq foreign)
      (format "%s already belong%s to another Helm release; take it from that release first, never by overwriting its stamp"
              (str/join ", " foreign) (if (= 1 (count foreign)) "s" "")))))

(defn items
  "What the confirmation lists per object, refs and objects parallel: what
   the stamp (verb :stamp) or the strip (verb :unstamp) does to it."
  [verb release release-namespace refs objects]
  (let [release-words (str release-namespace "/" release)]
    (vec (for [[ref object] (map vector refs objects)
               :let [state (when object (ownership release release-namespace object))
                     manager (:managed-by (stamp-of object))
                     what (describe ref)]]
           (case [verb state]
             ([:stamp nil] [:unstamp nil]) (str what " does not exist"
                                                (when (= :stamp verb) " - helm install creates it"))
             [:stamp :stamped] (str what " carries the stamp of " release-words " already")
             ([:stamp :bare] [:stamp :partial])
             (str "stamp " what " as owned by Helm release " release-words
                  (when (and manager (not= "Helm" manager))
                    (str " (replacing managed-by=" manager ")")))
             ([:unstamp :stamped] [:unstamp :partial]) (str "strip the Helm stamp from " what)
             [:unstamp :bare] (str what " carries no Helm stamp")
             ([:stamp :foreign] [:unstamp :foreign]) (str what " belongs to release " (owner-words object)))))))

;; ---------------------------------------------------------------------------
;; post-checks: pure over gumshoe.reads results

(defn stamped-results?
  "Pure: true only when results is not empty and every result is :present
   with an object carrying this release's full stamp. False when any read
   did not answer and when any object does not exist: a caller asserts the
   stamp positively, so a read that said nothing must read as not yet
   stamped. (not (stamped-results? ..)) is therefore no evidence that the
   stamp is gone; a strip judges with no-stamp-results?."
  [release release-namespace results]
  (boolean
   (and (seq results)
        (every? #(and (reads/present? %)
                      (= :stamped (ownership release release-namespace (reads/object! %))))
                results))))

(defn no-stamp-results?
  "Pure: true only when every read answered and no object that exists
   carries any part of this release's stamp (each is :bare or :foreign); an
   object that does not exist carries none. False when any read did not
   answer: silence is no evidence that a strip took effect, which is why
   this is a function of its own rather than the negation of
   stamped-results?, which is false on silence too."
  [release release-namespace results]
  (boolean
   (every? #(and (reads/answered? %)
                 (or (reads/absent? %)
                     (#{:bare :foreign} (ownership release release-namespace (reads/object! %)))))
           results)))

;; ---------------------------------------------------------------------------
;; collection

(defn read-objects
  "The objects refs name, read now through gumshoe.reads, concurrently, as
   results in the order of refs. Never throws for a read that did not
   answer: such a result is :unread."
  [context refs]
  (concurrent/all (map (fn [ref] #(reads/object context ref)) refs)))

(defn stamped?
  "Whether every object refs name exists and carries the stamp of release
   in release-namespace, read now. False on any read that did not answer,
   on an object that does not exist, and for no refs; see
   stamped-results?."
  [context release release-namespace refs]
  (stamped-results? release release-namespace (read-objects context refs)))

(defn no-stamp-confirmed?
  "Whether every read of the objects refs name answered and none that
   exists carries any part of the stamp of release in release-namespace,
   read now: the post-check of a strip. False on any read that did not
   answer. (not (stamped? ..)) is not this check: it holds on silence and
   would confirm a strip the cluster never reported. See
   no-stamp-results?."
  [context release release-namespace refs]
  (no-stamp-results? release release-namespace (read-objects context refs)))

(defn stamped-check
  "The post-check that every object refs name exists and carries the stamp.
   A read that did not answer, or an object that is gone, never holds it."
  [context release release-namespace refs]
  {:description (format "%s carr%s the Helm stamp of %s/%s"
                        (str/join ", " (map describe refs))
                        (if (= 1 (count refs)) "ies" "y")
                        release-namespace release)
   :timeout 60 :interval 5
   :check #(stamped? context release release-namespace refs)})

(defn unstamped-check
  "The post-check that no object refs name carries the stamp any more. It
   holds for an object that does not exist, so it declares
   :verifies-absence?; a read that did not answer never holds it."
  [context release release-namespace refs]
  {:description (format "%s carr%s no Helm stamp of %s/%s"
                        (str/join ", " (map describe refs))
                        (if (= 1 (count refs)) "ies" "y")
                        release-namespace release)
   :timeout 60 :interval 5 :verifies-absence? true
   :check #(no-stamp-confirmed? context release release-namespace refs)})
