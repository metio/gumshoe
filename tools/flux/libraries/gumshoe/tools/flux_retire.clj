;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux-retire
  "Retiring a HelmRelease without a helm uninstall, so the objects its
   release deployed survive because another writer now manages them.

   Deleting a HelmRelease that carries finalizers.fluxcd.io hands it to
   helm-controller, whose finalization runs helm uninstall unless the object
   is suspended, and an uninstall deletes every object the stored release
   manifest claims, including the ones another controller has since taken
   over by server-side apply. A retirement removes the release record and
   leaves those objects where they are.

   The plan has three effects, in this order:

     1. suspend the HelmRelease (spec.suspend=true);
     2. strip finalizers.fluxcd.io from it, keeping any other finalizer;
     3. delete it.

   The Helm release storage (sh.helm.release.v1.<release>.v<N>) is left in
   place: it is the record of what the release owned, and dropping it is the
   operator's call once the post-checks have shown every object survived.

   Why both the suspend and the strip. helm-controller's finalization skips
   the uninstall for a suspended HelmRelease and drops its finalizer, and
   its reconcile adds finalizers.fluxcd.io back to any HelmRelease that has
   none, suspended or not (fluxcd/helm-controller, Reconcile and
   reconcileDelete, read at v0.31.0 and on main on 2026-11-28). So on a
   running controller the suspend is what keeps the uninstall from running,
   and a stripped finalizer may well be back by the time the delete lands.
   The strip covers the delete landing while no controller acts on it: the
   API server then removes the object at once, where a suspended object
   with the finalizer would stay Terminating until a controller comes back.
   The suspend comes first so nothing reconciles in between, and
   suspends-before-stripping? and strips-finalizer-before-delete? hold every
   plan to that order. A suspend that an owner reverts before the delete
   would reopen the uninstall path, which is why a Kustomization or
   ResourceSet that still renders the HelmRelease and is not suspended
   refuses the retirement.

   What the release claims comes from its storage: the newest revision,
   which helm uninstall would read, and the newest deployed revision when it
   is another one. The release payload is base64 (the Secret's encoding)
   over base64 over gzip over JSON; decode-release undoes that within fixed
   bounds and manifest-claims parses the manifest within fixed bounds, both
   pure. Only the kind, group, namespace and name of each claimed object are
   kept; the payload's values and the manifest's data never leave the read.

   Another writer. An object counts as taken over when a managedFields
   entry of its main resource (no subresource such as status) names a
   manager other than Helm's own (helm, helm-controller) that wrote it with
   operation Apply - a server-side apply, which declares ownership of the
   fields it sets. A caller that knows which controller took over names it
   instead (writer-named, the --writer flag): then only that manager's
   entries count, whatever their operation. The seam is a predicate over one
   entry {:manager :operation :subresource}, so a casebook can pass its own.

   An object the release claims that exists and has no other writer refuses
   the retirement, listing every such object. No uninstall runs, so such an
   object would not be deleted; it would be orphaned: nothing would correct
   its drift or roll out its next version, it would keep a Helm stamp naming
   a release that no longer exists, and the HelmRelease that still manages it
   now is the only thing that could. A handover that has not reached an
   object yet is a handover to finish first, and an object that should go
   with the release is what helm uninstall is for. A read of a claimed
   object that did not answer refuses too: silence is neither a writer nor
   the absence of one. A claimed object that does not exist is listed and
   left alone, since there is nothing left to protect.

   Collection (read-record, read-owners and the post-checks) is apart from
   judgement (decode-release, manifest-claims, the writer predicates,
   problems, the plan and its four predicates), which is pure."
  (:require [cheshire.core :as json]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [gumshoe.deletes :as deletes]
            [gumshoe.effect :as effect]
            [gumshoe.helm-ownership :as own]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.reads :as reads]
            [gumshoe.select :as select]
            [gumshoe.tools.flux :as flux]
            [gumshoe.tools.flux-storage :as storage])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.util Base64)
           (java.util.zip GZIPInputStream)))

(def flux-finalizer
  "The finalizer helm-controller sets on a HelmRelease."
  "finalizers.fluxcd.io")

(def storage-prefix
  "Helm names each revision's storage Secret <prefix><release>.v<version>."
  "sh.helm.release.v1.")

(def helm-managers
  "The field managers Helm itself writes as: the helm CLI and helm-controller."
  #{"helm" "helm-controller"})

;; ---------------------------------------------------------------------------
;; decoding the release payload, pure and bounded

(def max-payload-chars
  "The longest .data.release accepted, in characters. A Secret holds at most
   1 MiB, which base64 grows to about 1.4 MiB, so anything longer is not a
   Helm release."
  (* 2 1024 1024))

(def max-release-bytes
  "The most bytes the gzip stream may inflate to. A release is the chart's
   templates, values and rendered manifest; a stream inflating past this is
   refused rather than held in memory."
  (* 32 1024 1024))

(defn- base64-bytes
  "The bytes s decodes to as base64, or nil."
  [^String s]
  (try (.decode (Base64/getDecoder) (str/trim s))
       (catch IllegalArgumentException _ nil)))

(defn- gzip?
  [^bytes bs]
  (and (<= 2 (alength bs))
       (= (unchecked-byte 0x1f) (aget bs 0))
       (= (unchecked-byte 0x8b) (aget bs 1))))

(defn- inflate
  "The bytes the gzip stream bs inflates to, {:too-large true} past limit, or
   nil when bs is not a gzip stream."
  [^bytes bs limit]
  (try
    (with-open [in (GZIPInputStream. (ByteArrayInputStream. bs))]
      (let [out (ByteArrayOutputStream.)
            buffer (byte-array 65536)]
        (loop [total 0]
          (let [n (.read in buffer)]
            (cond
              (neg? n) (.toByteArray out)
              (< limit (+ total n)) {:too-large true}
              :else (do (.write out buffer 0 n) (recur (+ total n))))))))
    (catch java.io.IOException _ nil)))

(defn decode-release
  "Pure: the manifest a Helm storage Secret's .data.release holds, as
   {:manifest text}, or {:problem why}. payload is the value as kubectl
   prints it: base64 of Helm's own base64 of a gzip stream (or, from very old
   Helm versions, of plain JSON) of the release, whose manifest field is the
   rendered YAML. limits holds :max-chars and :max-bytes, the bounds
   max-payload-chars and max-release-bytes by default."
  ([payload] (decode-release payload {}))
  ([payload {:keys [max-chars max-bytes] :or {max-chars max-payload-chars max-bytes max-release-bytes}}]
   (let [helm-text (when (and (string? payload) (<= (count payload) max-chars))
                     (some-> (base64-bytes payload) (String. "UTF-8")))
         raw (some-> helm-text base64-bytes)
         inflated (when raw (if (gzip? raw) (inflate raw max-bytes) raw))]
     (cond
       (not (string? payload)) {:problem "the storage Secret holds no release payload"}
       (str/blank? payload) {:problem "the storage Secret holds no release payload"}
       (< max-chars (count payload)) {:problem (format "the release payload is %d characters, more than the %d a Helm release can be" (count payload) max-chars)}
       (nil? helm-text) {:problem "the release payload is not base64"}
       (nil? raw) {:problem "the release payload is not Helm's base64 encoding"}
       (nil? inflated) {:problem "the release payload is not a readable gzip stream"}
       (:too-large inflated) {:problem (format "the release inflates to more than %d bytes" max-bytes)}
       (< max-bytes (alength ^bytes inflated)) {:problem (format "the release is more than %d bytes" max-bytes)}
       :else
       (let [release (try (json/parse-string (String. ^bytes inflated "UTF-8"))
                          (catch Exception _ ::unparsable))
             manifest (when (map? release) (get release "manifest"))]
         (cond
           (= ::unparsable release) {:problem "the release is not JSON"}
           (not (map? release)) {:problem "the release is not a JSON object"}
           (not (string? manifest)) {:problem "the release records no manifest"}
           :else {:manifest manifest}))))))

(defn- group-of
  [api-version]
  (let [parts (str/split (str api-version) #"/")]
    (when (= 2 (count parts)) (first parts))))

(defn manifest-claims
  "Pure: the objects a rendered manifest claims, as {:claims [{:kind :group
   :namespace :name}]} in the order they appear, each once, or {:problem
   why}. :group is nil for the core group and :namespace nil where the
   manifest names none (Helm puts a namespaced one in the release
   namespace). The YAML is parsed with clj-yaml's alias and nesting limits
   and at most max-release-bytes code points, unknown tags read as their
   value. A document that is empty is skipped; one that names no kind and
   name, and a List, are problems, because their objects could not be
   checked for survival."
  [manifest]
  (let [docs (try (vec (yaml/parse-string (str manifest)
                                          :load-all true
                                          :code-point-limit max-release-bytes
                                          :unknown-tag-fn :value))
                  (catch Exception e {::unparsable (or (ex-message e) (.getName (class e)))}))]
    (if (map? docs)
      {:problem (str "the manifest is not YAML: " (::unparsable docs))}
      (let [docs (remove nil? docs)
            bad (some (fn [doc]
                        (cond
                          (not (map? doc)) "a document of the manifest is not an object"
                          (str/ends-with? (str (:kind doc)) "List") (format "the manifest holds a %s, whose items are not read one by one" (:kind doc))
                          (not (and (string? (:kind doc)) (string? (get-in doc [:metadata :name]))))
                          "a document of the manifest names no kind and name"
                          :else nil))
                      docs)]
        (if bad
          {:problem bad}
          {:claims (vec (distinct (for [doc docs]
                                    {:kind (:kind doc)
                                     :group (group-of (:apiVersion doc))
                                     :namespace (some-> (get-in doc [:metadata :namespace]) str not-empty)
                                     :name (get-in doc [:metadata :name])})))})))))

(defn describe-claim
  "A claimed object as the confirmation names it: \"<Kind> <namespace>/<name>\"."
  [{:keys [kind namespace name]}]
  (str kind " " (when namespace (str namespace "/")) name))

(defn claim-type
  "The resource type kubectl reads a claim as: the lower-cased kind,
   qualified by its group when it has one (\"deployment.apps\")."
  [{:keys [kind group]}]
  (str (str/lower-case kind) (when group (str "." group))))

;; ---------------------------------------------------------------------------
;; revisions

(defn storage-name
  "The name of the Secret that stores revision version of release."
  [release version]
  (str storage-prefix release ".v" version))

(defn revisions-to-read
  "Pure: the versions whose manifests a retirement reads, newest first: the
   newest revision, which helm uninstall would read, and the newest deployed
   revision when that is another one. rows are kubectl/helm-releases rows of
   one release."
  [rows]
  (let [versioned (filter :version rows)
        newest (apply max-key :version (or (seq versioned) [{:version nil}]))
        deployed (filter #(= "deployed" (:status %)) versioned)]
    (vec (distinct (keep :version (cons newest (when (seq deployed) [(apply max-key :version deployed)])))))))

;; ---------------------------------------------------------------------------
;; writers

(defn default-writer?
  "Whether a managedFields entry is another writer by default: a
   server-side apply (operation Apply) to the main resource by a manager
   other than Helm's own."
  [{:keys [manager operation subresource]}]
  (and (string? manager)
       (not (contains? helm-managers manager))
       (= "Apply" operation)
       (str/blank? subresource)))

(defn writer-named
  "A writer predicate counting only the managers named, whatever their
   operation, on the main resource. Helm's own managers never count, even
   when named."
  [managers]
  (let [named (set (remove helm-managers managers))]
    (fn [{:keys [manager subresource]}]
      (and (contains? named manager) (str/blank? subresource)))))

(defn writers
  "Pure: the managers among a reading's managedFields entries that writer?
   counts, distinct and sorted."
  [writer? {:keys [managers]}]
  (vec (sort (distinct (keep #(when (writer? %) (:manager %)) managers)))))

(defn reading
  "Pure: what a retirement keeps of a gumshoe.reads result for a claimed
   object: its state and, when present, its uid, its managedFields entries
   reduced to {:manager :operation :subresource} and its Helm stamp. The
   object itself, a Secret's data among it, is dropped."
  [result]
  (reads/on-state result
                  {:present (fn [r]
                              (let [o (reads/object! r)]
                                {:state :present
                                 :uid (get-in o [:metadata :uid])
                                 :managers (vec (for [e (get-in o [:metadata :managedFields])]
                                                  {:manager (:manager e)
                                                   :operation (:operation e)
                                                   :subresource (:subresource e)}))
                                 :stamp (own/stamp-of o)}))
                   :absent (fn [_] {:state :absent})
                   :unread (fn [r] {:state :unread :reason (:reason r)})}))

;; ---------------------------------------------------------------------------
;; judgement

(defn- listed
  [claims]
  (str/join ", " (map describe-claim claims)))

(defn problems
  "Pure: every reason the HelmRelease must not be retired now, as messages,
   empty to proceed. helmrelease is the object as read, record what
   read-record returned for it, owners what read-owners returned and writer?
   the writer predicate."
  [helmrelease record owners writer?]
  (let [{:keys [release problem claimed in-flight? timeout]} record
        unread (filter #(= :unread (get-in % [:reading :state])) claimed)
        unwritten (filter #(and (= :present (get-in % [:reading :state]))
                                (empty? (writers writer? (:reading %))))
                          claimed)
        active (remove :suspended? owners)]
    (vec
     (keep identity
           [(when (get-in helmrelease [:metadata :deletionTimestamp])
              (format "HelmRelease %s is being deleted already, and helm-controller may be uninstalling the release; wait until it is gone or the uninstall has failed"
                      (kubectl/namespace-name-of helmrelease)))
            (when problem
              (format "the storage of release %s cannot say what it owns: %s, so nothing could confirm the objects survived"
                      release problem))
            (when in-flight?
              (format "Helm is installing or upgrading release %s right now; stripping the finalizer mid-action leaves it half applied. Wait out its timeout (%ss) and run this again"
                      release timeout))
            (when (seq active)
              (format "%s still render%s this HelmRelease and %s not suspended, so it could recreate it or revert the suspend before the delete: %s. Remove it from the source or suspend the owner first"
                      (if (= 1 (count active)) "one owner" (str (count active) " owners"))
                      (if (= 1 (count active)) "s" "")
                      (if (= 1 (count active)) "is" "are")
                      (str/join ", " (map #(format "%s %s/%s" (:kind %) (:namespace %) (:name %)) active))))
            (when (seq unread)
              (format "could not read %s, which the release claims, so whether another writer manages %s is unknown"
                      (listed (map :claim unread)) (if (= 1 (count unread)) "it" "them")))
            (when (seq unwritten)
              (format "nothing but Helm writes %s, which the release claims; retiring the release would leave %s unmanaged. Hand %s over to another writer first"
                      (listed (map :claim unwritten))
                      (if (= 1 (count unwritten)) "it" "them")
                      (if (= 1 (count unwritten)) "it" "them")))]))))

;; ---------------------------------------------------------------------------
;; the plan

(defn suspend-effect
  [context {:keys [namespace name]}]
  (effect/kubectl context (str "--namespace=" namespace) "patch" flux/helmrelease-type name
                  "--type=merge" "--patch={\"spec\":{\"suspend\":true}}"))

(defn strip-patch
  "The JSON patch that removes finalizers.fluxcd.io from finalizers, the list
   the HelmRelease carried at the confirmation, and keeps every other one. It
   tests that the list is still that one, so a finalizer another controller
   added in between is never dropped; the patch then fails, after the
   suspend, and nothing is deleted. Without finalizers it adds an empty
   list."
  [finalizers]
  (json/generate-string
   (if (seq finalizers)
     [{:op "test" :path "/metadata/finalizers" :value (vec finalizers)}
      {:op "replace" :path "/metadata/finalizers" :value (vec (remove #{flux-finalizer} finalizers))}]
     [{:op "add" :path "/metadata/finalizers" :value []}])))

(defn strip-effect
  [context {:keys [namespace name]} finalizers]
  (effect/kubectl context (str "--namespace=" namespace) "patch" flux/helmrelease-type name
                  "--type=json" (str "--patch=" (strip-patch finalizers))))

(defn delete-effect
  [context {:keys [namespace name]}]
  (deletes/effect context {:type flux/helmrelease-type :namespace namespace :name name} {:wait false}))

(defn retire-plan
  "Suspend, strip finalizers.fluxcd.io, delete; the release storage stays.
   ref names the HelmRelease, finalizers is the list it carried at the
   confirmation."
  [context ref finalizers]
  (effect/plan
   (suspend-effect context ref)
   (strip-effect context ref finalizers)
   (delete-effect context ref)))

(defn- effects
  "The effects of plan that act, nested plans flattened and notes dropped: a
   note only carries words, such as the follow-up naming the storage."
  [plan]
  (vec (remove #(= :note (first %))
               (mapcat (fn [e] (if (keyword? (first e)) [e] e)) plan))))

(defn- args-of [e] (filter string? e))

(defn- index-of
  [plan pred]
  (first (keep-indexed (fn [i e] (when (pred (args-of e)) i)) (effects plan))))

(defn- verb? [verb args] (some #{verb} args))

(defn- suspends? [args] (and (verb? "patch" args) (some #(str/includes? % "\"suspend\":true") args)))

(defn- strips? [args] (and (verb? "patch" args) (some #(str/includes? % "/metadata/finalizers") args)))

(def ^:private delete-verb
  ;; the verb as gumshoe.deletes writes it, so the predicates read a plan the
  ;; way the builder assembles it
  (first (deletes/args {:type "t" :name "n"})))

(defn- deletes? [args] (verb? delete-verb args))

(defn strips-finalizer-before-delete?
  "Pure: whether the plan strips the finalizer before it deletes. In the other
   order the delete hands the object to helm-controller with its finalizer."
  [plan]
  (let [strip (index-of plan strips?) del (index-of plan deletes?)]
    (boolean (and strip del (< strip del)))))

(defn suspends-before-stripping?
  "Pure: whether the plan suspends before it strips the finalizer, so nothing
   reconciles the HelmRelease between the two."
  [plan]
  (let [suspend (index-of plan suspends?) strip (index-of plan strips?)]
    (boolean (and suspend strip (< suspend strip)))))

(defn holds-storage?
  "Pure: whether the plan leaves the release storage alone: no effect names a
   Secret or a storage Secret's name. The storage is the only record of what
   the release owned until the post-checks pass."
  [plan]
  (not-any? (fn [e] (some #(or (str/includes? % storage-prefix)
                               (contains? #{"secret" "secrets"} %)
                               (str/starts-with? % "secrets/")
                               (str/includes? % "owner=helm"))
                          (args-of e)))
            (effects plan)))

(defn deletes-only-the-release?
  "Pure: whether the plan deletes exactly one object, the HelmRelease ref
   names: the type after the delete verb is the HelmRelease type and the name
   after it ref's name. Any other delete is an uninstall written by hand.
   Compared on the arguments, not on names alone, because a release and its
   objects routinely share a name."
  [plan {:keys [namespace name]}]
  (let [deletes (for [e (effects plan) :let [args (vec (args-of e))] :when (deletes? args)] args)]
    (boolean
     (and (= 1 (count deletes))
          (let [args (first deletes)
                [type target] (rest (drop-while #(not= delete-verb %) args))]
            (and (= flux/helmrelease-type type)
                 (= name target)
                 (some #{(str "--namespace=" namespace)} args)
                 (not-any? #(or (= "--all" %) (str/starts-with? % "--selector")) args)))))))

;; ---------------------------------------------------------------------------
;; collection

(defn- unread!
  [message]
  (throw (ex-info message {::kubectl/unread true})))

(defn read-payload
  "The .data.release of the storage Secret named name in namespace: a
   string, nil when the cluster answered that it does not exist, and throws
   an ex-info carrying :gumshoe.kubectl/unread when the read did not answer.
   The payload never leaves this namespace's decoding."
  [context namespace name]
  (let [{:keys [exit out err]} (kubectl/get-answer context (str "--namespace=" namespace) "secrets" name
                                                   "--ignore-not-found" "--output=jsonpath={.data.release}")]
    (cond
      (not= 0 exit) (unread! (format "could not read secrets %s/%s: %s" namespace name
                                     (or (not-empty (str/trim (str err))) (str "exit " exit))))
      (str/blank? out) nil
      :else out)))

(defn read-claim
  "The reading of one claimed object, read now with its managedFields. A
   claim without a namespace is read in release-namespace, and an answer
   without one is taken as the cluster-scoped object it names."
  [context release-namespace {:keys [namespace name] :as claim}]
  (let [type (claim-type claim)
        ref {:type type :namespace (or namespace release-namespace) :name name}
        raw (kubectl/get-answer context (str "--namespace=" (:namespace ref)) type name
                                "--ignore-not-found" "--show-managed-fields" "--output=json")
        unserved #(kubectl/type-unserved? context type)
        result (reads/classify-object ref raw unserved)
        cluster-scoped (when (and (nil? namespace) (reads/unread? result) (= 0 (:exit raw)))
                         (reads/classify-object (assoc ref :namespace nil) raw unserved))]
    (reading (if (and cluster-scoped (reads/present? cluster-scoped)) cluster-scoped result))))

(defn- now-seconds [] (quot (System/currentTimeMillis) 1000))

(defn read-record
  "What the release of helmrelease owns, read now: {:release
   :release-namespace :storage-namespace :versions :claims :claimed
   :in-flight? :timeout :problem}. :claimed holds {:claim :reading} per
   claim. A storage listing or payload read that did not answer throws an
   ex-info carrying :gumshoe.kubectl/unread; a storage that answered but
   cannot say what the release owns is :problem."
  [context helmrelease]
  (let [release (flux/release-name helmrelease)
        release-namespace (flux/release-namespace helmrelease)
        storage-namespace (flux/storage-namespace helmrelease)
        rows (binding [kubectl/*strict-reads* true]
               (kubectl/helm-releases context storage-namespace release))
        base {:release release :release-namespace release-namespace
              :storage-namespace storage-namespace
              :timeout (flux/helm-timeout-seconds helmrelease)
              :in-flight? (flux/action-in-flight? rows helmrelease (now-seconds))}
        versions (revisions-to-read rows)]
    (if (empty? versions)
      (assoc base :versions [] :problem "it stores no revision")
      (let [decoded (for [v versions
                          :let [payload (read-payload context storage-namespace (storage-name release v))]]
                      (if (nil? payload)
                        {:problem (format "%s is gone" (storage-name release v))}
                        (let [{:keys [manifest problem]} (decode-release payload)]
                          (if problem
                            {:problem (str (storage-name release v) ": " problem)}
                            (let [{:keys [claims problem]} (manifest-claims manifest)]
                              (if problem
                                {:problem (str (storage-name release v) ": " problem)}
                                {:claims claims}))))))
            problem (some :problem decoded)
            claims (vec (distinct (mapcat :claims decoded)))]
        (cond
          problem (assoc base :versions versions :problem problem)
          (empty? claims) (assoc base :versions versions :problem "its manifest claims no object")
          :else (assoc base
                       :versions versions
                       :claims claims
                       :claimed (mapv (fn [c] {:claim c :reading (read-claim context release-namespace c)}) claims)))))))

(defn read-owners
  "The Kustomizations and ResourceSets whose inventory lists helmrelease, as
   {:kind :namespace :name :suspended?}. A listing that did not answer
   throws an ex-info carrying :gumshoe.kubectl/unread; a cluster that does
   not serve ResourceSets has none."
  [context helmrelease]
  (let [listed (fn [type] (reads/items! (reads/listing context {:type type :namespace :all})))]
    (vec (for [{:keys [kind namespace name object]}
               (storage/owners helmrelease {"Kustomization" (listed flux/kustomization-type)
                                            "ResourceSet" (listed storage/resourceset-type)})]
           {:kind kind :namespace namespace :name name
            :suspended? (boolean (storage/owner-suspended? kind object))}))))

;; ---------------------------------------------------------------------------
;; post-checks

(defn gone-check
  "The post-check that the HelmRelease is gone, by a read that answered
   NotFound."
  [context {:keys [namespace name]}]
  {:description (format "HelmRelease %s/%s is gone" namespace name)
   :timeout 120 :interval 5 :verifies-absence? true
   :check #(kubectl/confirmed-gone? context namespace flux/helmrelease-type name)})

(defn survived?
  "Whether the HelmRelease is confirmed gone and every claimed object that
   existed at the confirmation still exists as the same object (same uid),
   read now. The HelmRelease is read first: helm-controller drops its
   finalizer only after an uninstall returns, so once the HelmRelease is
   gone no uninstall can still be on its way. A read that did not answer
   never holds it."
  [context ref release-namespace claimed]
  (boolean
   (and (kubectl/confirmed-gone? context (:namespace ref) flux/helmrelease-type (:name ref))
        (every? (fn [{:keys [claim reading]}]
                  (let [now (read-claim context release-namespace claim)]
                    (and (= :present (:state now)) (some? (:uid now)) (= (:uid reading) (:uid now)))))
                claimed))))

(defn survived-check
  [context ref release-namespace claimed]
  {:description (format "%s survived the retirement as the same object%s (an uninstall would have deleted %s)"
                        (listed (map :claim claimed))
                        (if (= 1 (count claimed)) "" "s")
                        (if (= 1 (count claimed)) "it" "them"))
   :timeout 120 :interval 5
   :check #(survived? context ref release-namespace claimed)})

(defn storage-kept?
  "Whether every revision version of release read at the confirmation is
   still stored in namespace, read now. A listing that did not answer never
   holds it."
  [context namespace release versions]
  (let [rows (kubectl/helm-releases context namespace release)]
    (boolean (and (some? rows)
                  (every? (set (keep :version rows)) versions)))))

(defn storage-kept-check
  [context namespace release versions]
  {:description (format "the Helm storage of release %s in %s still holds revision%s %s"
                        release namespace (if (= 1 (count versions)) "" "s") (str/join ", " versions))
   :timeout 60 :interval 5
   :check #(storage-kept? context namespace release versions)})

;; ---------------------------------------------------------------------------
;; the change

(defn- helmrelease-ref
  [target]
  (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
    {:type flux/helmrelease-type :namespace namespace :name name}))

(defn- finalizers-of [helmrelease] (vec (get-in helmrelease [:metadata :finalizers])))

(defn- present-claims
  [record]
  (filterv #(= :present (get-in % [:reading :state])) (:claimed record)))

(defn items
  "What the confirmation lists, in the order the plan runs, then what it
   leaves alone."
  [ref helmrelease record owners writer?]
  (let [target (str (:namespace ref) "/" (:name ref))
        {:keys [release storage-namespace versions claimed]} record]
    (vec
     (concat
      [(format "suspend HelmRelease %s, so nothing reconciles it while it is retired" target)
       (format "strip %s from HelmRelease %s, keeping %s - without it the delete reaches no helm uninstall"
               flux-finalizer target
               (let [others (remove #{flux-finalizer} (finalizers-of helmrelease))]
                 (if (seq others) (str "the finalizers " (str/join ", " others)) "no other finalizer")))
       (format "delete HelmRelease %s" target)]
      (for [{:keys [claim reading]} claimed]
        (case (:state reading)
          :present (format "keep %s, written by %s" (describe-claim claim)
                           (let [w (writers writer? reading)] (if (seq w) (str/join ", " w) "nothing but Helm")))
          :absent (format "%s, which the release claims, does not exist - nothing to keep" (describe-claim claim))
          (format "%s could not be read" (describe-claim claim))))
      (for [{:keys [kind namespace name]} (filter :suspended? owners)]
        (format "%s %s/%s still renders this HelmRelease and is suspended - resuming it would recreate the HelmRelease, and Helm would upgrade against the objects kept here"
                kind namespace name))
      [(format "leave the Helm storage of release %s in %s (revision%s %s) for you to drop once the post-checks pass"
               release storage-namespace (if (= 1 (count versions)) "" "s") (str/join ", " versions))]))))

(defn recovery
  [{:keys [target record]}]
  (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
        {:keys [release storage-namespace]} record]
    (str "The three effects run in order and nothing the release claims is deleted, so whatever was reached stands:\n"
         (format "  - suspended only: HelmRelease %s/%s is suspended and otherwise as it was. Run the book again to finish, or resume it with flux/resume.\n" namespace name)
         "  - finalizer stripped: helm-controller adds it back on its next reconcile; the suspend keeps that reconcile from acting. Run the book again to finish.\n"
         (format "  - deleted: the retirement is complete. The Helm storage of release %s in %s is left in place; drop it once the post-checks pass."
                 release storage-namespace))))

(defn follow-up
  "What the operator does once the post-checks pass."
  [{:keys [release storage-namespace claimed]}]
  (let [stamped (filter #(= release (get-in % [:reading :stamp :release-name])) claimed)]
    (str (format (str "Once the checks pass, drop the Helm storage with `kubectl --namespace %s delete secrets"
                      " --selector owner=helm,name=%s`; until then `helm --namespace %s list` still shows the release.")
                 storage-namespace release storage-namespace)
         (when (seq stamped)
           (format " %s still carr%s the Helm ownership stamp of release %s; strip it with helm/unstamp_ownership so a later release of that name does not adopt %s."
                   (listed (map :claim stamped)) (if (= 1 (count stamped)) "ies" "y") release
                   (if (= 1 (count stamped)) "it" "them"))))))

(defn retire
  "The parts of a change that retires a HelmRelease the pick names
   (\"<namespace>/<name>\") without an uninstall, writer-of a function of the
   context returning the writer predicate. Combine them with a select,
   options and prerequisites through gumshoe.change/compose, as retire-named
   does. The HelmRelease is an optional subject, pinned with its suspend,
   finalizers and deletion mark as facts; a HelmRelease that does not exist
   is retired already, so the change is done."
  [writer-of]
  {:subjects (fn [{:keys [target]}]
               [(assoc (helmrelease-ref target)
                       :optional? true
                       :facts (fn [o] {:suspend (get-in o [:spec :suspend])
                                       :finalizers (finalizers-of o)
                                       :deleting (get-in o [:metadata :deletionTimestamp])}))])
   :reads {:record (fn [{:keys [context objects]}]
                     (when-let [hr (first objects)] (read-record context hr)))
           :owners (fn [{:keys [context objects]}]
                     (when-let [hr (first objects)] (read-owners context hr)))}
   :done (fn [{:keys [objects]}] (nil? (first objects)))
   :problems (fn [{:keys [objects record owners] :as ctx}]
               (if-let [hr (first objects)]
                 (problems hr record owners (writer-of ctx))
                 []))
   :announce (fn [{:keys [target]}] (format "Retire HelmRelease %s without an uninstall" target))
   :items (fn [{:keys [target objects record owners] :as ctx}]
            (if-let [hr (first objects)]
              (items (helmrelease-ref target) hr record owners (writer-of ctx))
              [(format "HelmRelease %s does not exist" target)]))
   :effect (fn [{:keys [context target objects record]}]
             (if-let [hr (first objects)]
               (conj (retire-plan context (helmrelease-ref target) (finalizers-of hr))
                     (effect/note (follow-up record)))
               (effect/plan (effect/note (format "HelmRelease %s does not exist" target)))))
   :verify (fn [{:keys [context target objects record]}]
             (let [ref (helmrelease-ref target)]
               (if (first objects)
                 [(survived-check context ref (:release-namespace record) (present-claims record))
                  (storage-kept-check context (:storage-namespace record) (:release record) (:versions record))]
                 [(gone-check context ref)])))
   :recovery recovery})

(defn writer-from-flags
  "The writer predicate the command line names: writer-named over --writer
   when given, else default-writer?."
  [{:keys [opts]}]
  (if (seq (:writer opts)) (writer-named (:writer opts)) default-writer?))

(def selection
  {:mode :namespaced :label "HelmRelease" :namespace-flag :namespace :name-flag :name
   :candidates storage/helmrelease-picks})

(def retire-named
  "Retires the HelmRelease --namespace and --name pick without an uninstall."
  (merge {:options (select/options selection
                                   {:writer {:desc "A field manager that has taken over the release's objects, repeatable; when given, only these count as another writer"
                                             :coerce [:string]}})
          :prerequisites {:installed-tools ["kubectl" "fzf"]
                          :cluster-capabilities [:flux]
                          :kubectl-can-get [flux/helmrelease-type flux/kustomization-type]
                          :kubectl-can-patch [flux/helmrelease-type]
                          :kubectl-can-delete [flux/helmrelease-type]}
          :select selection
          :confirm {:action "retire the HelmRelease without a helm uninstall: suspend it, strip its Flux finalizer and delete it, keeping every object its release deployed and its Helm storage"
                    :destructive? true}}
         (retire writer-from-flags)))
