;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.reads
  "Cluster reads that keep an absence apart from a read that did not answer:
   the kubectl classifier of the three-state results of gumshoe.reads.result.

   The states, their predicates, on-state, the extractors, derived and the
   refusal builders live in gumshoe.reads.result, which knows nothing about
   kubectl, and are forwarded here under their usual names. What stays here
   is what only a kubectl read needs: the ref checks, the pinned kube
   context, the identity check of a named get, the unserved-type
   confirmation through discovery and the read cache.

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
   items! and value! throw an ex-info carrying :gumshoe.reads/unread and
   :gumshoe.kubectl/unread for an :unread result, so an unanswered read cannot reach a string, a
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
            [gumshoe.kubectl :as kubectl]
            [gumshoe.reads.result :as result]))

;; ---------------------------------------------------------------------------
;; the source-neutral half, forwarded from gumshoe.reads.result so every
;; caller of gumshoe.reads keeps its names

(defmacro ^:private forward
  [name source]
  (let [{:keys [doc arglists]} (meta (resolve source))]
    `(def ~(vary-meta name assoc :doc doc :arglists (list 'quote arglists)) ~source)))

(forward states result/states)
(forward present? result/present?)
(forward absent? result/absent?)
(forward unread? result/unread?)
(forward answered? result/answered?)
(forward on-state result/on-state)
(forward describe result/describe)
(forward object! result/object!)
(forward items! result/items!)
(forward derived result/derived)
(forward value! result/value!)
(forward unread-refusal result/unread-refusal)
(forward refusal-unless-present result/refusal-unless-present)
(forward refusal-unless-absent result/refusal-unless-absent)

;; ---------------------------------------------------------------------------
;; classification: pure over kubectl's raw result {:exit :out :err}

(def ^:private missing-type #"doesn't have a resource type")

(defn- failure-reason
  [{:keys [exit err]}]
  (or (not-empty (str/trim (str err))) (str "exit " exit)))

(def ^:private unread result/unread)

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
