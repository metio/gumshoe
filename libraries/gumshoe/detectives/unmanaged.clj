;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.unmanaged
  "Objects in a namespace that nobody manages: the finish condition of a
   migration is that the old objects are gone and everything left has an
   owner.

   The evidence is a namespace inventory (gumshoe.unmanaged/inventory): every
   object of every namespaced type the API serves with a list verb, distinct
   by uid, with each type's read state. Everything here is pure over it.

   Each object is judged by the rules in order, and the first rule that
   accepts it names its owner. A rule is plain data:

     {:name        \"helm\"
      :class       :helm                 ; how the report groups what it accepts
      :description \"what it accepts and why that is an owner\"
      :accepts     (fn [object facts] ...)}

   :accepts returns nil when the rule does not apply, an owner string (or
   {:owner \"...\"}) when it accepts the object, {:undetermined \"why\"}
   when the object belongs to this rule but the evidence to judge it did not
   answer, or {:unowned \"why\"} when the rule knows the object has no owner
   and no later rule may accept it. facts is the inventory indexed by `facts`. A caller adds its own
   rules in the same shape, most simply with `matching`, and hands them to
   `detective` or `audit`; each names its owner, so the report says why an
   object is accepted instead of passing it silently.

   An object no rule accepts is unowned. A type that did not answer, or an
   API group whose discovery failed, leaves the namespace unjudged: the
   report names each one and never calls the namespace clean."
  (:require [clojure.string :as str]
            [gumshoe.helm-ownership :as helm-ownership]
            [gumshoe.subject :as subject]))

(def inventory-key
  "The evidence key the namespace inventory is collected under."
  "namespace-inventory")

;; ---------------------------------------------------------------------------
;; object accessors

(defn key-name
  "The string form of a label or annotation key. JSON keys arrive as
   keywords, and a key with a slash is a namespaced keyword whose `name`
   drops the prefix, so the printed form is used."
  [k]
  (if (keyword? k) (subs (str k) 1) (str k)))

(defn- label [object k] (get-in object [:metadata :labels (keyword k)]))
(defn- annotation [object k] (get-in object [:metadata :annotations (keyword k)]))
(defn- object-name [object] (get-in object [:metadata :name]))

(defn group-of
  "The API group of an apiVersion: \"apps/v1\" -> \"apps\", \"v1\" -> \"\"."
  [api-version]
  (let [v (str api-version)]
    (if (str/includes? v "/") (first (str/split v #"/" 2)) "")))

(defn describe
  "<Kind>/<name> for an object of the core group, <Kind>.<group>/<name>
   for any other, as the report names an object: several groups serve a
   kind of the same name (NetworkPolicy of networking.k8s.io and of
   projectcalico.org)."
  [object]
  (let [group (group-of (:apiVersion object))]
    (str (:kind object) (when-not (str/blank? group) (str "." group)) "/" (object-name object))))

(defn- owner-references [object] (seq (get-in object [:metadata :ownerReferences])))

(defn controller-reference
  "The ownerReference marked controller, else the first one, or nil."
  [object]
  (let [refs (owner-references object)]
    (or (first (filter :controller refs)) (first refs))))

;; ---------------------------------------------------------------------------
;; facts: the inventory indexed for the rules

(defn facts
  "The inventory indexed for the rules:
     :namespace  the namespace
     :by-uid     {uid object}
     :services   {name Service}
     :kinds      {[group kind] read state of the type serving it}
   A kind served by several types takes the worst state among them, so an
   owner kind is judged answered only when every type serving it answered."
  [{:keys [namespace objects types]}]
  (let [rank {:unread 0 :absent 1 :present 1}]
    {:namespace namespace
     :by-uid (into {} (keep #(when-let [uid (get-in % [:metadata :uid])] [uid %])) objects)
     :services (into {} (for [o objects :when (and (= "Service" (:kind o)) (= "" (group-of (:apiVersion o))))]
                          [(object-name o) o]))
     :kinds (reduce (fn [acc {:keys [group kind state]}]
                      (update acc [group kind] (fn [prev] (if (and prev (< (rank prev 0) (rank state 0))) prev state))))
                    {}
                    types)}))

;; ---------------------------------------------------------------------------
;; the built-in rules

(defn accepts-by-owner-reference
  "An object with an ownerReference belongs to its owner: the pods of a
   ReplicaSet, the ReplicaSets of a Deployment, a Job's pods and a Service's
   EndpointSlices all inherit whatever the owner carries, so judging them
   says nothing the owner does not. The owner is itself in the inventory and
   judged on its own, so a chain ends at its root.

   An owner of a namespaced kind whose read answered must be in the
   inventory; one that is not is gone, and the object is unowned whatever
   labels it carries, since those were inherited from the owner. An owner whose type did not answer
   leaves the object undetermined. An owner of a kind no namespaced type
   serves (a Node, a cluster-scoped custom resource) is accepted by the
   reference alone, because it cannot be in a namespace."
  [object {:keys [by-uid kinds]}]
  (when-let [ref (controller-reference object)]
    (let [kind-key [(group-of (:apiVersion ref)) (:kind ref)]
          state (get kinds kind-key)
          named (format "%s/%s" (:kind ref) (:name ref))]
      (cond
        (contains? by-uid (:uid ref)) (str named " (ownerReference)")
        (= :unread state) {:undetermined (format "its owner %s is of a type whose read did not answer" named)}
        (nil? state) (str named " (ownerReference to an object outside the namespace)")
        :else {:unowned (format "its ownerReference names %s, which is no longer in the namespace - the labels it inherited from that owner prove nothing"
                                named)}))))

(defn accepts-endpoints-of-service
  "An Endpoints object whose Service of the same name exists and selects
   pods belongs to the endpoints controller, which rewrites it from that
   Service within the second - deleting it is undone. A Service without a
   selector has its Endpoints written by whoever created them, so those are
   left to the other rules, as is an Endpoints object whose Service is gone."
  [object {:keys [services kinds]}]
  (when (and (= "Endpoints" (:kind object)) (= "" (group-of (:apiVersion object))))
    (let [service (get services (object-name object))]
      (cond
        (seq (get-in service [:spec :selector]))
        (format "endpoints controller, from Service %s" (object-name object))

        (and (nil? service) (= :unread (get kinds ["" "Service"])))
        {:undetermined "the Services read did not answer, so whether its Service exists is unknown"}

        :else nil))))

(defn accepts-root-ca-configmap
  "kube-root-ca.crt is published into every namespace by the
   root-ca-cert-publisher controller of kube-controller-manager."
  [object _facts]
  (when (and (= "ConfigMap" (:kind object)) (= "kube-root-ca.crt" (object-name object)))
    "kube-controller-manager (root-ca-cert-publisher)"))

(defn accepts-default-serviceaccount
  "The ServiceAccount default is created in every namespace by the
   serviceaccount controller of kube-controller-manager."
  [object _facts]
  (when (and (= "ServiceAccount" (:kind object)) (= "default" (object-name object)))
    "kube-controller-manager (serviceaccount controller)"))

(def ^:private legacy-token-name #"^(.+)-token-[a-z0-9]{5}$")

(defn accepts-legacy-token-secret
  "A service-account token Secret named <account>-token-<five characters>
   for that account was written by the token controller of clusters before
   Kubernetes 1.24. A token Secret named any other way was created by hand
   or by a tool, so it is left to the other rules."
  [object _facts]
  (when (and (= "Secret" (:kind object)) (= "kubernetes.io/service-account-token" (:type object)))
    (let [account (annotation object "kubernetes.io/service-account.name")
          [_ prefix] (re-find legacy-token-name (str (object-name object)))]
      (when (and account (= account prefix))
        (format "kube-controller-manager (legacy token controller), for ServiceAccount %s" account)))))

(def ^:private api-server-objects
  #{["Service" "kubernetes"] ["Endpoints" "kubernetes"] ["EndpointSlice" "kubernetes"]})

(defn accepts-kubernetes-service
  "The Service kubernetes in namespace default, with its Endpoints and
   EndpointSlice, is reconciled by the kube-apiserver itself."
  [object {:keys [namespace]}]
  (when (and (= "default" namespace) (api-server-objects [(:kind object) (object-name object)]))
    "kube-apiserver (endpoint reconciler)"))

(defn accepts-helm-release-object
  "Helm marks what it installs with its ownership stamp
   (gumshoe.helm-ownership): the label app.kubernetes.io/managed-by=Helm and
   the annotations meta.helm.sh/release-name and release-namespace, which
   Helm checks before it adopts an object. An object carrying the release
   annotation or the label is accepted; the owner names the release and
   says when the stamp is only partial, as `helm template | kubectl apply`
   leaves it."
  [object _facts]
  (let [{:keys [managed-by release-name release-namespace]} (helm-ownership/stamp-of object)
        release (str/join "/" (remove str/blank? [release-namespace release-name]))]
    (cond
      (and release-name release-namespace
           (= :stamped (helm-ownership/ownership release-name release-namespace object)))
      (format "Helm release %s" release)

      release-name
      (format "Helm release %s (partial stamp: %s)" release
              (str/join ", " (remove nil? [(when-not (= "Helm" managed-by) "no app.kubernetes.io/managed-by=Helm label")
                                           (when-not release-namespace "no release-namespace annotation")])))

      (= "Helm" managed-by)
      (format "Helm (label app.kubernetes.io/managed-by=Helm only, no release annotation%s)"
              (if-let [instance (label object "app.kubernetes.io/instance")] (str "; instance " instance) ""))

      :else nil)))

(defn accepts-helm-release-storage
  "Helm stores each release revision as a Secret of type helm.sh/release.v1
   (or a ConfigMap with the configmap driver) labelled owner=helm; it carries
   no chart labels of its own."
  [object _facts]
  (when (and (= "helm" (label object "owner"))
             (or (and (= "Secret" (:kind object)) (= "helm.sh/release.v1" (:type object)))
                 (= "ConfigMap" (:kind object))))
    (format "Helm release storage of %s" (or (label object "name") "an unnamed release"))))

(def flux-prefixes
  "The label and annotation key prefixes Flux writes on what it applies:
   kustomize-controller's and the flux-operator ResourceSet's."
  [["kustomize.toolkit.fluxcd.io/" "Flux Kustomization"]
   ["resourceset.fluxcd.controlplane.io/" "Flux ResourceSet"]])

(defn accepts-flux
  "Flux labels everything it applies: kustomize-controller with
   kustomize.toolkit.fluxcd.io/name and /namespace, the flux-operator's
   ResourceSet with resourceset.fluxcd.controlplane.io/name and /namespace.
   Any key under either prefix, as label or annotation, counts."
  [object _facts]
  (let [keys-of (fn [m] (map key-name (keys m)))
        all-keys (concat (keys-of (get-in object [:metadata :labels]))
                         (keys-of (get-in object [:metadata :annotations])))]
    (some (fn [[prefix what]]
            (when (some #(str/starts-with? % prefix) all-keys)
              (let [n (label object (str prefix "name"))
                    ns (label object (str prefix "namespace"))]
                (if n
                  (str what " " (str/join "/" (remove str/blank? [ns n])))
                  (str what " (a " prefix " key)")))))
          flux-prefixes)))

(defn accepts-argo-cd
  "Argo CD's annotation tracking writes argocd.argoproj.io/tracking-id on
   what an Application applies. Its label tracking writes only
   app.kubernetes.io/instance, which Helm charts set as well, so that label
   proves nothing and is not accepted."
  [object _facts]
  (when-let [tracking (annotation object "argocd.argoproj.io/tracking-id")]
    (str "Argo CD Application " (first (str/split tracking #":")))))

(defn accepts-stageset
  "The StageSet controller labels what it applies with stages.metio.wtf/name
   and stages.metio.wtf/namespace."
  [object _facts]
  (when-let [n (label object "stages.metio.wtf/name")]
    (str "StageSet " (str/join "/" (remove str/blank? [(label object "stages.metio.wtf/namespace") n])))))

(defn- rule
  [name class f]
  {:name name :class class :description (:doc (meta f)) :accepts @f})

(def default-rules
  "The built-in rules, in the order they are tried. An ownerReference comes
   first, so a controller-created object is judged by its owner rather than
   by the labels it inherited."
  [(rule "owner-reference" :controller #'accepts-by-owner-reference)
   (rule "endpoints-of-service" :cluster #'accepts-endpoints-of-service)
   (rule "root-ca-configmap" :cluster #'accepts-root-ca-configmap)
   (rule "default-serviceaccount" :cluster #'accepts-default-serviceaccount)
   (rule "legacy-token-secret" :cluster #'accepts-legacy-token-secret)
   (rule "kubernetes-service" :cluster #'accepts-kubernetes-service)
   (rule "helm" :helm #'accepts-helm-release-object)
   (rule "helm-release-storage" :helm #'accepts-helm-release-storage)
   (rule "flux" :gitops #'accepts-flux)
   (rule "argo-cd" :gitops #'accepts-argo-cd)
   (rule "stageset" :gitops #'accepts-stageset)])

(defn matching
  "A rule accepting the objects that match every given criterion, in the
   shape of the built-in ones:

     {:name \"metrics-server\" :owner \"metrics-server\"
      :description \"why this owner is accepted\"
      :kind \"PodMetrics\"                ; optional
      :group \"metrics.k8s.io\"           ; optional, \"\" for the core group
      :object-name \"x\"                  ; optional, exact name
      :labels {\"k\" \"v\"}                ; optional, each must be equal
      :annotations {\"k\" \"v\"}           ; optional, each must be equal
      :class :third-party}               ; optional

   A rule with no criterion at all would accept every object, so it is
   refused."
  [{:keys [name owner description kind group labels annotations class] obj-name :object-name}]
  (when (str/blank? (str name)) (throw (IllegalArgumentException. "a rule needs a :name")))
  (when (str/blank? (str owner)) (throw (IllegalArgumentException. (str "rule " name " needs an :owner"))))
  (when-not (or kind group obj-name (seq labels) (seq annotations))
    (throw (IllegalArgumentException.
            (str "rule " name " names no criterion, so it would accept every object - give it a :kind, :group, :object-name, :labels or :annotations"))))
  {:name name
   :class (or class :third-party)
   :description (or description (str "accepted as owned by " owner))
   :accepts (fn [object _facts]
              (when (and (or (nil? kind) (= kind (:kind object)))
                         (or (nil? group) (= group (group-of (:apiVersion object))))
                         (or (nil? obj-name) (= obj-name (object-name object)))
                         (every? (fn [[k v]] (= v (label object k))) labels)
                         (every? (fn [[k v]] (= v (annotation object k))) annotations))
                owner))})

(defn rules-with
  "The built-in rules followed by extra-rules."
  [extra-rules]
  (into default-rules extra-rules))

;; ---------------------------------------------------------------------------
;; classification

(defn- verdict
  [rule result]
  (let [result (if (string? result) {:owner result} result)]
    (cond
      (not (map? result))
      (throw (ex-info (format "rule %s returned %s; a rule returns nil, an owner string, {:owner ...}, {:undetermined ...} or {:unowned ...}"
                              (:name rule) (pr-str result))
                      {:rule (:name rule)}))

      (:undetermined result)
      {:class :undetermined :rule (:name rule) :reason (str (:undetermined result))}

      (:unowned result)
      {:class :unowned :rule (:name rule) :reason (str (:unowned result))}

      (str/blank? (str (:owner result)))
      (throw (ex-info (format "rule %s accepted an object without naming its owner" (:name rule)) {:rule (:name rule)}))

      :else
      {:class (or (:class result) (:class rule) :third-party) :rule (:name rule) :owner (str (:owner result))})))

(defn unowned-reason
  "Why no rule accepted object, for the report."
  [object {:keys [services]}]
  (let [applied? (some? (annotation object "kubectl.kubernetes.io/last-applied-configuration"))]
    (str (cond
           (= "Endpoints" (:kind object))
           (if (contains? services (object-name object))
             "its Service selects no pods, so the endpoints controller does not manage it, and it carries no marker of a known manager"
             "its Service is gone, so the endpoints controller no longer manages it")

           :else
           "it carries no ownerReference and no marker of a known manager")
         (when applied? " - kubectl apply wrote it (last-applied-configuration)"))))

(defn classify
  "The verdict on one object: {:class :rule :owner} from the first rule
   that accepts it, {:class :undetermined :rule :reason} when one could not
   judge it, {:class :unowned :rule :reason} when one rejected it, or
   {:class :unowned :reason} when no rule applied."
  [rules facts object]
  (or (some (fn [rule]
              (when-some [result ((:accepts rule) object facts)]
                (verdict rule result)))
            rules)
      {:class :unowned :reason (unowned-reason object facts)}))

(defn- root-of
  "The uid of the object an ownerReference chain from object ends at within
   the inventory: object's own uid when its owner is not in it."
  [by-uid object]
  (loop [o object seen #{}]
    (let [uid (get-in o [:metadata :uid])
          owner (some-> (controller-reference o) :uid by-uid)]
      (if (or (nil? owner) (seen uid))
        uid
        (recur owner (conj seen uid))))))

(defn audit
  "The verdict on every object of the inventory under rules, or nil for an
   inventory that is nil (no answer):

     {:namespace ..
      :objects [{:object o :class .. :rule .. :owner .. :reason .. :dependents n}]
      :unread-types [{:type .. :reason ..}]    ; listed by discovery, did not answer
      :unread-groups [{:group-version .. :reason ..}]  ; discovery failed
      :excluded [{:type|:group-version .. :exclusion .. :reason ..}]
      :read-types n}

   :dependents counts the objects whose ownerReference chain ends at the
   object, so a report can name the root and say what follows from it."
  [inventory rules]
  (when inventory
    (let [f (facts inventory)
          objects (:objects inventory)
          roots (frequencies (keep (fn [o] (let [r (root-of (:by-uid f) o)]
                                             (when (not= r (get-in o [:metadata :uid])) r)))
                                   objects))]
      {:namespace (:namespace inventory)
       :objects (mapv (fn [o]
                        (cond-> (assoc (classify rules f o) :object o)
                          (pos? (get roots (get-in o [:metadata :uid]) 0))
                          (assoc :dependents (get roots (get-in o [:metadata :uid])))))
                      objects)
       :unread-types (vec (for [t (:types inventory) :when (= :unread (:state t))]
                            (select-keys t [:type :reason])))
       :unread-groups (vec (:unread-groups inventory))
       :excluded (vec (:excluded inventory))
       :read-types (count (remove #(= :unread (:state %)) (:types inventory)))})))

(defn complete?
  "Whether the audit read everything it set out to: an answer, every type
   read and no API group whose discovery failed."
  [result]
  (boolean (and result (empty? (:unread-types result)) (empty? (:unread-groups result)))))

(defn of-class
  [result class]
  (filterv #(= class (:class %)) (:objects result)))

(defn clean?
  "Whether the namespace's finish condition holds: everything was read, and
   every object has an owner. False on any doubt - an audit that is nil, a
   type or group that did not answer, an object no rule accepts and one a
   rule could not judge."
  [result]
  (and (complete? result)
       (empty? (of-class result :unowned))
       (empty? (of-class result :undetermined))))

;; ---------------------------------------------------------------------------
;; findings

(defn- object-subject
  [namespace {:keys [object]}]
  (subject/subject (:kind object) namespace (object-name object)))

(defn findings
  "The findings of an audit: each type and group that did not answer and
   each unowned object at `severity`, each undetermined object as a
   warning, and each accepted object as info naming its owner and rule, so
   the report says why it passed. An accepted finding carries :owner, :rule
   and :class, an unowned one :reason, for the machine-readable formats."
  [result severity]
  (let [ns (:namespace result)]
    (concat
     (for [{:keys [type reason]} (:unread-types result)]
       {:severity :critical
        :component type
        :summary "A namespaced type did not answer, so what it holds in the namespace went unjudged"
        :hint (str reason " - the namespace is not judged clean while any type is unread")})
     (for [{:keys [group-version reason]} (:unread-groups result)]
       {:severity :critical
        :component group-version
        :summary "An API group's discovery failed, so the types it serves were never listed"
        :hint (str reason " - its types may hold objects nobody manages; exclude the group only when it can hold no stored objects")})
     (for [{:keys [object reason dependents] :as v} (of-class result :unowned)]
       {:severity severity
        :component (describe object)
        :subject (object-subject ns v)
        :class :unowned
        :reason reason
        :summary (format "%s is managed by nobody" (describe object))
        :hint (str reason
                   (when dependents (format "; %d object%s follow%s from it through ownerReferences"
                                            dependents (if (= 1 dependents) "" "s") (if (= 1 dependents) "s" "")))
                   " - delete it, or name its owner with a rule")})
     (for [{:keys [object reason rule] :as v} (of-class result :undetermined)]
       {:severity :warning
        :component (describe object)
        :subject (object-subject ns v)
        :class :undetermined
        :rule rule
        :reason reason
        :summary (format "%s could not be judged" (describe object))
        :hint (str "rule " rule ": " reason)})
     (for [{:keys [object owner rule class]} (:objects result)
           :when (not (#{:unowned :undetermined} class))]
       {:severity :info
        :component (describe object)
        :class class
        :rule rule
        :owner owner
        :summary (if (= :controller class)
                   "Follows its owner through an ownerReference"
                   (str "Owned by " owner))
        :hint (str "accepted by rule " rule)}))))

(defn detect
  "Pure: the findings for the inventory under evidence's inventory-key,
   judged by rules."
  [rules severity evidence]
  (when-let [result (audit (get evidence inventory-key) rules)]
    (vec (findings result severity))))

(defn detective
  "The unmanaged-objects detective. Options:
     :rules        the whole rule list (default default-rules)
     :extra-rules  rules tried after :rules, a caller's third-party owners
     :severity     the severity of an unowned object (default :critical,
                   so the run fails while anything is unowned)"
  ([] (detective {}))
  ([{:keys [rules extra-rules severity] :or {rules default-rules severity :critical}}]
   (let [rules (into (vec rules) extra-rules)]
     {:name "unmanaged-objects"
      :description "Objects in the namespace that no controller, Helm, Flux, Argo CD, StageSet or named owner manages"
      :requires [inventory-key]
      :detect (partial detect rules severity)})))

(def detectives
  [(detective)])
