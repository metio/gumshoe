;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.reads
  "Cluster reads that keep an absence apart from a read that did not answer.

   The plain getters of gumshoe.kubectl (get-namespaced-resource,
   get-cluster-resource, get-all, get-namespaced) return nil both for an
   object that does not exist and for a read that failed, so a gate written
   on them opens on a throttled API. Every read here returns a result map in
   exactly one of three states instead:

     {:state :present :ref ref :object o}       the object exists
     {:state :absent  :ref ref}                 the cluster answered: none
     {:state :unread  :ref ref :reason \"why\"}   no answer, nothing is known

   A list read carries :items on :present (one or more objects) and :absent
   (none). An :absent result carries :unserved true when the cluster does
   not serve the type at all.

   Only a clean answer is :absent: kubectl exited 0 for a named get with
   --ignore-not-found and printed nothing, or a list answered with no items,
   or kubectl reported the type missing (\"the server doesn't have a resource
   type\") and the API server's discovery confirms it does not serve the
   type (kubectl/type-unserved?). Anything else - a non-zero exit that is not
   that message, a timeout, an RBAC denial, output that does not parse, an
   object other than the one named, a missing type discovery does not
   confirm - is :unread. kubectl prints the missing-type message also when
   discovery of the type's group failed and it carried on with what it did
   read, which is why the message alone never proves an absence. A type
   named without a group (\"listenerset\", \"deployments\") is never
   confirmed unserved, because any group may serve it: name an optional type
   as <plural>.<group> (\"listenersets.gateway.networking.x-k8s.io\") so its
   absence can be confirmed.

   Polarity. Which state a caller acts on decides whether an unanswered read
   is safe:

   | caller                               | proceeds on | test with       |
   |--------------------------------------|-------------|-----------------|
   | asserts a presence (precondition,    | :present    | present?        |
   |   post-check that X exists)          |             |                 |
   | confirms a deletion (post-check that | :absent     | absent?         |
   |   X is gone, gate on an absence)     |             |                 |
   | skips an object (acts when present,  | :present    | on-state, with  |
   |   does nothing when absent)          | / :absent   | :unread refused |

   (not (present? r)) is not an absence and (not (absent? r)) is not a
   presence: both include :unread. A caller that skips an object must
   refuse or report on :unread, never treat it as nothing to do; on-state
   makes it name all three branches.

   Unread is never a value. The result is a map, and the extractors object!,
   items! and value! throw an ex-info carrying :gumshoe.kubectl/unread for
   an :unread result, so an unanswered read cannot reach a string, a
   manifest or a comparison as if it were data. The strict-read machinery
   treats that exception as a read that did not answer: verify/eventually
   counts the check as not yet verified, and a mutation's :derive or
   :refusal fails the run. The read functions themselves never throw for a
   read that did not answer, whatever kubectl/*strict-reads* holds.

   The reads go through kubectl/*read-cache* when it is bound, and through
   the kube context a run pinned (gumshoe.kube-context/*pinned*): a nil
   context reads the pinned one, and a context other than the pinned one is
   refused with an exception rather than read.

   Collection (object, namespaced-object, cluster-object, listing) is apart
   from classification (classify-object, classify-list), which is pure over
   kubectl's raw result, so a test can judge every branch without a
   cluster."
  (:require [clojure.string :as str]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.kubectl :as kubectl]))

;; ---------------------------------------------------------------------------
;; states and predicates

(def states
  "The three states a result can be in."
  #{:present :absent :unread})

(defn- state-of
  "The state of a result; anything that is not a result map in one of the
   three states counts as :unread, so a nil or malformed value never passes
   as an answer."
  [r]
  (let [s (when (map? r) (:state r))]
    (if (contains? states s) s :unread)))

(defn present?
  "True only when the read answered and the object exists (a list: one or
   more objects matched). The test for a caller that asserts a presence: a
   precondition requiring X, a post-check that X was created. False for
   :absent and for :unread - so (not (present? r)) does NOT mean the object
   is absent, and a caller that skips or deletes on it acts on a read that
   did not answer."
  [r]
  (= :present (state-of r)))

(defn absent?
  "True only when the read answered that no such object exists (a list: it
   answered with no items), or the cluster confirmably does not serve the
   type. The test for a caller that confirms a deletion or gates on an
   absence: a post-check that X is gone, a refusal unless nothing is left.
   False for :present and for :unread - so (not (absent? r)) does NOT mean
   the object exists."
  [r]
  (= :absent (state-of r)))

(defn unread?
  "True when the read did not answer, and for anything that is not a result
   map in one of the three states. The branch a caller must refuse or report
   on: never read it as absent (a gate would open on a throttled API) nor as
   present (a post-check would pass on silence)."
  [r]
  (= :unread (state-of r)))

(defn answered?
  "True for :present and :absent: the cluster answered and the result says
   what it holds."
  [r]
  (not (unread? r)))

(defn on-state
  "Calls the branch for the result's state with the result and returns what
   it returns. branches must hold :present, :absent and :unread, each a
   function of one argument; a missing branch throws, so a caller cannot
   forget the read that did not answer. A malformed result takes the
   :unread branch.

     (reads/on-state r {:present #(delete! (reads/object! %))
                        :absent  (constantly :nothing-to-do)
                        :unread  #(refuse! (reads/unread-refusal %))})

   This is the shape for a caller that skips an object: it acts on
   :present, skips on :absent, and refuses on :unread."
  [r branches]
  (let [missing (remove #(fn? (get branches %)) [:present :absent :unread])]
    (when (seq missing)
      (throw (IllegalArgumentException.
              (str "reads/on-state needs a function for every state; missing "
                   (str/join ", " missing)))))
    ((get branches (state-of r)) r)))

;; ---------------------------------------------------------------------------
;; extractors

(defn describe
  "The read a ref names, in words an operator knows: \"deployments web/api\",
   \"nodes worker-1\", \"pods in web\", \"pods in every namespace\", \"nodes\"."
  [{:keys [type namespace name selector]}]
  (str type
       (cond
         (and name namespace) (str " " namespace "/" name)
         name (str " " name)
         (= :all namespace) " in every namespace"
         namespace (str " in " namespace)
         :else "")
       (when selector (str " matching " selector))))

(defn- did-not-answer
  [r]
  (let [reason (if (map? r) (:reason r) (str "not a read result: " (pr-str r)))]
    (ex-info (format "could not read %s: %s"
                     (if (map? r) (describe (:ref r)) "an object")
                     (or reason "the read did not answer"))
             {::kubectl/unread true ::result r})))

(defn object!
  "The object of a :present result, nil for an :absent one. Throws an
   ex-info carrying :gumshoe.kubectl/unread for an :unread result, so the
   object of a read that did not answer is never nil by accident. The nil of
   :absent is a confirmed answer; a caller that must treat absence and
   failure apart tests present?/absent? or uses on-state first."
  [r]
  (case (state-of r)
    :present (:object r)
    :absent nil
    :unread (throw (did-not-answer r))))

(defn items!
  "The items of an answered list result: a vector, empty for :absent.
   Throws an ex-info carrying :gumshoe.kubectl/unread for an :unread
   result, so a list that did not answer never reads as an empty one."
  [r]
  (case (state-of r)
    (:present :absent) (vec (:items r))
    :unread (throw (did-not-answer r))))

(defn derived
  "The result with :value set to what f makes of its answer, state kept.
   For an object result f receives the object, and only a :present result
   gets a :value. For a list result f receives the items, and every answered
   result gets one (f [] for :absent). An :unread result is returned
   unchanged: f never sees a read that did not answer. Read the value with
   value!."
  [r f]
  (let [s (state-of r)]
    (cond
      (= :unread s) r
      (contains? r :items) (assoc r :value (f (vec (:items r))))
      (= :present s) (assoc r :value (f (:object r)))
      :else r)))

(defn value!
  "The :value of a derived result: nil for an :absent object result, and
   throws an ex-info carrying :gumshoe.kubectl/unread for an :unread one.
   This is where an unanswered read would otherwise slip into a manifest or
   a host name as data; the exception stops it."
  [r]
  (if (unread? r)
    (throw (did-not-answer r))
    (:value r)))

;; ---------------------------------------------------------------------------
;; refusals, one per polarity: each returns a line for a :refusal or a
;; precondition, or nil when the caller may proceed

(defn- unread-line
  [r what because]
  (format "could not read %s%s - %s, so this refuses rather than act on a read that did not answer"
          what
          (if-let [reason (and (map? r) (:reason r))] (str " (" reason ")") "")
          (or because "acting on a wrong answer could do harm")))

(defn- what-of
  [r what]
  (or what (when (map? r) (describe (:ref r))) "the object"))

(defn unread-refusal
  "A refusal line when the read did not answer, nil when it answered either
   way. For a caller that handles :present and :absent itself and must not
   proceed on silence (one that skips an absent object). opts: :what names
   the object as the operator knows it (default: the ref described),
   :because what acting on a wrong answer would do."
  ([r] (unread-refusal r nil))
  ([r {:keys [what because]}]
   (when (unread? r)
     (unread-line r (what-of r what) because))))

(defn refusal-unless-present
  "nil only when the object exists; a refusal line when the read answered
   that it does not, and when it did not answer. For a caller that asserts a
   presence before acting on the object. opts as unread-refusal."
  ([r] (refusal-unless-present r nil))
  ([r {:keys [what because] :as opts}]
   (case (state-of r)
     :present nil
     :absent (format "%s does not exist" (what-of r what))
     :unread (unread-refusal r opts))))

(defn refusal-unless-absent
  "nil only when the read answered that the object does not exist (or the
   type is confirmably not served); a refusal line when it exists, and when
   the read did not answer. For a caller that confirms a deletion or gates
   on an absence. opts as unread-refusal."
  ([r] (refusal-unless-absent r nil))
  ([r {:keys [what] :as opts}]
   (case (state-of r)
     :absent nil
     :present (format "%s still exists" (what-of r what))
     :unread (unread-refusal r opts))))

;; ---------------------------------------------------------------------------
;; classification: pure over kubectl's raw result {:exit :out :err}

(def ^:private missing-type #"doesn't have a resource type")

(defn- failure-reason
  [{:keys [exit err]}]
  (or (not-empty (str/trim (str err))) (str "exit " exit)))

(defn- unread
  [ref reason]
  {:state :unread :ref ref :reason reason})

(defn- grouped?
  [type]
  (str/includes? (str type) "."))

(defn- missing-type-result
  "The result of a read kubectl failed. Only the missing-type message for a
   grouped type that discovery confirms is unserved is an absence; extra is
   merged into that absence (a list's empty :items)."
  [ref result confirm-unserved extra]
  (let [type (:type ref)]
    (cond
      (not (re-find missing-type (str (:err result))))
      (unread ref (str "the read did not answer: " (failure-reason result)))

      (not (grouped? type))
      (unread ref (format "the server reported no resource type %s; a type named without its group is never confirmed absent, so name it as <plural>.<group>"
                          type))

      (confirm-unserved)
      (merge {:state :absent :ref ref :unserved true} extra)

      :else
      (unread ref (format "the server reported no resource type %s, but its discovery did not confirm that the type is not served"
                          type)))))

(defn- named-object
  "The result for an object a named get answered with: :present only when
   it is the object the ref names."
  [ref o]
  (cond
    (not (map? o))
    (unread ref "the read answered with something that is not an object")

    (not= (:name ref) (kubectl/name-of o))
    (unread ref (format "the read answered with %s, not the object it named"
                        (pr-str (kubectl/name-of o))))

    (and (:namespace ref) (not= (:namespace ref) (kubectl/namespace-of o)))
    (unread ref (format "the read answered with an object in namespace %s, not %s"
                        (pr-str (kubectl/namespace-of o)) (:namespace ref)))

    ;; kubectl reads a namespaced type named without a namespace in the
    ;; context's default namespace, which is not the object the ref names
    (and (nil? (:namespace ref)) (some? (kubectl/namespace-of o)))
    (unread ref (format "the read named no namespace but answered with an object in namespace %s; read a namespaced type with namespaced-object"
                        (kubectl/namespace-of o)))

    :else
    {:state :present :ref ref :object o}))

(defn classify-object
  "Pure: the result of a named `kubectl get <type> <name> --ignore-not-found
   --output=json` for ref {:type :namespace :name}. confirm-unserved is a
   function of no arguments, called only when kubectl reported the type
   missing, that answers whether discovery confirms the type is not served.

   exit 0, no output                       :absent (--ignore-not-found)
   exit 0, an empty List                   :absent
   exit 0, the object the ref names        :present
   exit 0, any other output                :unread
   missing type, grouped, confirmed        :absent, :unserved true
   any other failure                       :unread"
  [ref {:keys [exit out] :as result} confirm-unserved]
  (if (= 0 exit)
    (if (str/blank? out)
      {:state :absent :ref ref}
      (let [doc (kubectl/parse-json out)]
        (cond
          (not (map? doc))
          (unread ref "the read answered with output that does not parse")

          (contains? doc :items)
          (let [items (:items doc)]
            (cond
              (not (sequential? items)) (unread ref "the read answered a list without items")
              (empty? items) {:state :absent :ref ref}
              (= 1 (count items)) (named-object ref (first items))
              :else (unread ref (format "the read named one object but answered %d" (count items)))))

          :else
          (named-object ref doc))))
    (missing-type-result ref result confirm-unserved nil)))

(defn classify-list
  "Pure: the result of `kubectl get <type> --output=json` listing ref
   {:type :namespace :selector}. confirm-unserved as in classify-object.

   exit 0, a list with items               :present with :items
   exit 0, a list with no items            :absent with :items []
   exit 0, any other output                :unread
   missing type, grouped, confirmed        :absent, :items [], :unserved true
   any other failure                       :unread"
  [ref {:keys [exit out] :as result} confirm-unserved]
  (if (= 0 exit)
    (let [doc (kubectl/parse-json out)
          items (when (map? doc) (:items doc))]
      (cond
        (not (map? doc)) (unread ref "the read answered with output that does not parse")
        (not (sequential? items)) (unread ref "the read answered something that is not a list")
        (empty? items) {:state :absent :ref ref :items []}
        :else {:state :present :ref ref :items (vec items)}))
    (missing-type-result ref result confirm-unserved {:items []})))

;; ---------------------------------------------------------------------------
;; collection

(defn- context-for
  "The kube context to read: context, or the pinned one when context is
   nil. A context other than the one the run pinned throws: the run's pick
   and reads were made on the pinned cluster."
  [context]
  (let [pinned (:context kube-context/*pinned*)]
    (cond
      (nil? kube-context/*pinned*) context
      (nil? context) pinned
      (= context pinned) context
      :else (throw (ex-info (format "a read names the kube context %s, but the run is pinned to %s"
                                    (pr-str context) (pr-str pinned))
                            {::context-mismatch true :context context :pinned pinned})))))

(defn- non-blank?
  [s]
  (and (string? s) (not (str/blank? s))))

(defn- check-ref!
  [{:keys [type name namespace]} name?]
  (when-not (non-blank? type)
    (throw (IllegalArgumentException. (str "a read needs a :type, got " (pr-str type)))))
  (when (and name? (not (non-blank? name)))
    (throw (IllegalArgumentException.
            (str "a named read needs a :name, got " (pr-str name)
                 " - without one kubectl would list the type instead"))))
  (when-not (or (nil? namespace) (non-blank? namespace) (and (not name?) (= :all namespace)))
    (throw (IllegalArgumentException. (str "a read's :namespace must be a name or nil, got " (pr-str namespace))))))

(defn object
  "Reads the object ref {:type :namespace :name} names, namespace nil for a
   cluster-scoped type, as a result in one of three states (see the
   namespace docstring). Never throws for a read that did not answer; throws
   for a ref without a type or a name, and for a context the run did not
   pin.

   Polarity: assert a presence with present?, confirm a deletion with
   absent?, skip an object with on-state; read the object with object!."
  [context {:keys [type namespace name] :as ref}]
  (check-ref! ref true)
  (let [context (context-for context)
        ref (select-keys ref [:type :namespace :name])
        result (apply kubectl/get-answer context
                      (concat (when namespace [(str "--namespace=" namespace)])
                              [type name "--ignore-not-found" "--output=json"]))]
    (classify-object ref result #(kubectl/type-unserved? context type))))

(defn namespaced-object
  "object for the named object of a namespaced type in namespace, which
   must not be blank. Polarity as object."
  [context namespace type name]
  (when-not (non-blank? namespace)
    (throw (IllegalArgumentException.
            (str "namespaced-object needs a namespace, got " (pr-str namespace)))))
  (object context {:type type :namespace namespace :name name}))

(defn cluster-object
  "object for the named object of a cluster-scoped type. Polarity as
   object: an object that does not exist is :absent, a read that did not
   answer :unread. A namespaced type read here is looked up in the kube
   context's default namespace; an object found there is :unread, but one
   missing there reads :absent, so never pass a namespaced type."
  [context type name]
  (object context {:type type :name name}))

(defn listing
  "Lists ref {:type :namespace :selector :field-selector} as a result in one
   of three states: :present when one or more objects matched, :absent when
   the read answered with none (or the cluster confirmably does not serve
   the type), :unread when it did not answer. :namespace is a namespace,
   :all for every namespace, or nil for a cluster-scoped type. Never throws
   for a read that did not answer.

   Polarity: present? when something must exist, absent? when nothing may
   be left (a purge's post-check), items! to read the objects, which throws
   rather than hand an unanswered list over as an empty one."
  [context {:keys [type namespace selector field-selector] :as ref}]
  (check-ref! ref false)
  (let [context (context-for context)
        ref (select-keys ref [:type :namespace :selector :field-selector])
        result (apply kubectl/get-answer context
                      (concat [type]
                              (cond
                                (= :all namespace) ["--all-namespaces"]
                                namespace [(str "--namespace=" namespace)])
                              (when selector [(str "--selector=" selector)])
                              (when field-selector [(str "--field-selector=" field-selector)])
                              ["--output=json"]))]
    (classify-list ref result #(kubectl/type-unserved? context type))))
