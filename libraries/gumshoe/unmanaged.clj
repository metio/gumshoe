;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.unmanaged
  "Collects the inventory of one namespace for the unmanaged-objects
   detective (gumshoe.detectives.unmanaged): every object of every
   namespaced type the API serves with a list verb.

   `kubectl get all` covers about a dozen kinds, so an object left under a
   kind nobody looks at survives a migration unnoticed. Discovery
   (`kubectl api-resources --verbs=list --namespaced`) names every type, and
   each type is listed once, concurrently, through gumshoe.reads, so each
   keeps its own three-state answer: a type that did not answer is named as
   unread, never taken for an empty one. One combined `kubectl get a,b,c`
   would be one call, but kubectl prints what it could read and exits
   non-zero when one type fails, which cannot be attributed to a type.

   An aggregated API whose server is down fails discovery for its group;
   kubectl still prints every other group and exits non-zero. The groups it
   names on stderr are kept as unread groups, because they may serve
   exactly the objects in question. A failure that names no group leaves
   the whole inventory unread.

   Each object is kept with its metadata only (name, uid, labels,
   annotations, ownerReferences), a Secret's type and a Service's selector:
   the rules need no more, and a Secret's data never enters the evidence.
   The last-applied-configuration annotation, which holds a whole manifest
   (a Secret's data included), is kept as a marker without its value.

   Objects are distinct by uid, because several API groups serve the same
   stored object (Events through the core and the events.k8s.io group);
   one without a uid is distinct by kind and name. Distinct by kind and name
   alone would fold a NetworkPolicy of networking.k8s.io into a Calico
   NetworkPolicy of the same name, and pass one on the other's owner."
  (:require [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.detectives.unmanaged :as unmanaged]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.reads :as reads]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(def default-exclusions
  "Types and groups the inventory does not read, each with the reason, so
   the report says what was not looked at. A caller passes its own list to
   `inventory` to read them after all."
  [{:name "events"
    :types #{"events" "events.events.k8s.io"}
    :reason "Events are written by every controller, owned by nobody and expire after the API server's event TTL"}
   {:name "metrics"
    :groups #{"metrics.k8s.io"}
    :reason "metrics.k8s.io synthesises PodMetrics from running pods and stores nothing, so it holds no object anybody could leave behind"}
   {:name "calico-storage"
    :groups #{"crd.projectcalico.org"}
    :reason "crd.projectcalico.org is Calico's internal storage of the projectcalico.org/v3 objects, which are read instead; reading it is unsupported and would report every policy twice"}])

(defn- excluded-by
  [exclusions {:keys [type group]}]
  (first (filter (fn [{:keys [types groups]}]
                   (or (contains? (set types) type) (contains? (set groups) group)))
                 exclusions)))

;; ---------------------------------------------------------------------------
;; discovery: pure over kubectl's result

(def ^:private group-version
  ;; a group/version kubectl names on stderr when its discovery failed, as in
  ;; "couldn't get resource list for metrics.k8s.io/v1beta1: ..." and
  ;; "unable to retrieve the complete list of server APIs: metrics.k8s.io/v1beta1: ..."
  #"(?:^|[\s,\"=])([a-z0-9][-a-z0-9.]*/v[0-9]+(?:alpha[0-9]+|beta[0-9]+)?):\s")

(defn failed-group-versions
  "The group/versions kubectl's stderr names as failed discovery, in order."
  [err]
  (vec (distinct (map second (re-seq group-version (str err))))))

(defn parse-types
  "The rows of `kubectl api-resources --verbs=list --namespaced --no-headers`
   as [{:type :group :version :kind}], :type being the name kubectl lists it
   by (<plural>.<group>, or the plural for the core group). Columns are NAME
   [SHORTNAMES] APIVERSION NAMESPACED KIND; SHORTNAMES is optional, so the
   fields are read from the edges."
  [out]
  (vec (for [line (str/split-lines (str out))
             :let [fields (str/split (str/trim line) #"\s+")
                   n (count fields)]
             :when (and (>= n 4) (= "true" (nth fields (- n 2))))
             :let [plural (first fields)
                   api-version (nth fields (- n 3))
                   [group version] (if (str/includes? api-version "/")
                                     (str/split api-version #"/" 2)
                                     ["" api-version])]]
         {:type (if (str/blank? group) plural (str plural "." group))
          :group group
          :version version
          :kind (peek fields)})))

(defn- failure-reason
  [{:keys [exit err]}]
  (or (not-empty (str/trim (str err))) (str "exit " exit)))

(defn classify-discovery
  "Pure: the discovery result for kubectl's {:exit :out :err}:
     {:state :answered :types [..]}
     {:state :partial  :types [..] :failed [\"group/version\" ..] :reason ..}
     {:state :unread   :reason ..}
   Only an exit 0 listing types is a full answer. A non-zero exit is partial
   when kubectl still listed types and named the groups it could not read,
   and unread otherwise, since a failure naming no group may have hidden any
   type."
  [{:keys [exit out] :as result}]
  (let [types (parse-types out)
        failed (failed-group-versions (:err result))]
    (cond
      (and (= 0 exit) (seq types)) {:state :answered :types types}
      (= 0 exit) {:state :unread :reason "the discovery of namespaced types listed none"}
      (and (seq types) (seq failed)) {:state :partial :types types :failed failed :reason (failure-reason result)}
      :else {:state :unread :reason (str "the discovery of namespaced types did not answer: " (failure-reason result))})))

;; ---------------------------------------------------------------------------
;; objects: pure reductions

(def ^:private last-applied :kubectl.kubernetes.io/last-applied-configuration)

(defn reduce-object
  "object with only what the rules read: apiVersion, kind, the metadata the
   rules judge, a Secret's type and a Service's selector. The
   last-applied-configuration annotation keeps its key and loses its value,
   which is a whole manifest."
  [object]
  (let [metadata (:metadata object)
        annotations (cond-> (:annotations metadata)
                      (contains? (:annotations metadata) last-applied) (assoc last-applied "<omitted>"))]
    (cond-> {:apiVersion (:apiVersion object)
             :kind (:kind object)
             :metadata (cond-> (select-keys metadata [:name :namespace :uid :labels :ownerReferences :creationTimestamp])
                         (seq annotations) (assoc :annotations annotations))}
      (= "Secret" (:kind object)) (assoc :type (:type object))
      (= "Service" (:kind object)) (assoc :spec (select-keys (:spec object) [:selector])))))

(defn distinct-objects
  "objects, each stored object once: distinct by uid, or by kind and name
   for one without a uid. The first one listed is kept, with :served-as
   naming every type that listed it."
  [listed]
  (let [identity-of (fn [[_ o]] (or (get-in o [:metadata :uid]) [(:kind o) (get-in o [:metadata :name])]))]
    (->> listed
         (reduce (fn [{:keys [order by-id]} [type o :as entry]]
                   (let [id (identity-of entry)]
                     (if (contains? by-id id)
                       {:order order :by-id (update-in by-id [id :served-as] conj type)}
                       {:order (conj order id) :by-id (assoc by-id id (assoc o :served-as [type]))})))
                 {:order [] :by-id {}})
         ((fn [{:keys [order by-id]}] (mapv by-id order))))))

(defn assemble
  "Pure: the inventory of namespace from its discovery result and the read
   of each type it lists, {type reads-result}, after exclusions."
  [namespace discovery listings exclusions]
  (let [rows (:types discovery)
        kept (remove #(excluded-by exclusions %) rows)
        failed (for [gv (:failed discovery)
                     :let [group (first (str/split gv #"/" 2))]]
                 {:group-version gv :group group :exclusion (excluded-by exclusions {:group group})})]
    {:namespace namespace
     :types (vec (for [row kept
                       :let [r (get listings (:type row))]]
                   (cond-> (assoc row :state (if r (:state r) :unread))
                     (or (nil? r) (reads/unread? r)) (assoc :reason (if r (:reason r) "the type was not read")))))
     :unread-groups (vec (for [{:keys [group-version exclusion]} failed :when (nil? exclusion)]
                           {:group-version group-version
                            :reason (str "its discovery failed: " (:reason discovery))}))
     :excluded (vec (concat
                     (for [row rows :let [e (excluded-by exclusions row)] :when e]
                       {:type (:type row) :exclusion (:name e) :reason (:reason e)})
                     (for [{:keys [group-version exclusion]} failed :when exclusion]
                       {:group-version group-version :exclusion (:name exclusion)
                        :reason (str (:reason exclusion) " (its discovery failed as well)")})))
     :objects (distinct-objects
               (for [row kept
                     :let [r (get listings (:type row))]
                     :when (and r (reads/present? r))
                     o (:items r)]
                 [(:type row) (reduce-object o)]))}))

;; ---------------------------------------------------------------------------
;; collection

(defn discover
  "The discovery of the namespaced types context serves with a list verb."
  [context]
  (classify-discovery
   (shell/execute "kubectl" (str "--context=" context) "--request-timeout=30s"
                  "api-resources" "--verbs=list" "--namespaced=true" "--no-headers")))

(defn inventory
  "The inventory of namespace on context, or {:unread \"why\"} when it could
   not be taken: discovery did not answer, or the namespace does not exist
   or could not be read (a namespace that does not exist lists nothing
   under every type, which would read as a clean one). Options:
     :exclusions   types and groups not read (default default-exclusions)
     :parallelism  how many types are listed at once (default 8)"
  ([context namespace] (inventory context namespace {}))
  ([context namespace {:keys [exclusions parallelism] :or {exclusions default-exclusions parallelism 8}}]
   (let [ns-read (reads/cluster-object context "namespaces" namespace)]
     (cond
       (reads/absent? ns-read) {:unread (format "namespace %s does not exist" namespace)}
       (not (reads/present? ns-read)) {:unread (str "the namespace could not be read: " (:reason ns-read))}
       :else
       (let [discovery (discover context)]
         (if (= :unread (:state discovery))
           {:unread (:reason discovery)}
           (let [types (map :type (remove #(excluded-by exclusions %) (:types discovery)))
                 results (concurrent/all (for [type types]
                                           #(reads/listing context {:type type :namespace namespace}))
                                         {:parallelism parallelism})]
             (assemble namespace discovery (zipmap types results) exclusions))))))))

(defn- unread-summary
  [inv]
  (let [types (keep #(when (= :unread (:state %)) (:type %)) (:types inv))
        groups (map :group-version (:unread-groups inv))]
    (when (seq (concat types groups))
      (format "the namespace was read only in part - %s"
              (str/join "; " (remove nil? [(when (seq types) (str "types that did not answer: " (str/join ", " types)))
                                           (when (seq groups) (str "API groups whose discovery failed: " (str/join ", " groups)))]))))))

(def options
  "The options of a book running the unmanaged-objects detective."
  {:namespace {:desc "The namespace to audit"
               :alias :n
               :coerce :string}})

(defn collect
  "The :collect of a detective book: the evidence for the unmanaged-objects
   detective in the namespace --namespace names, on the current context. An
   inventory that could not be taken is nil evidence, so the detective
   counts as unread and the run reports that nothing was checked; the
   reason is printed. A partial inventory is recorded as incomplete, so the
   verdict never passes it off as findings alone. Without --namespace it
   prints why and collects nothing."
  ([opts] (collect opts {}))
  ([opts inventory-opts]
   (let [namespace (:namespace opts)]
     (if (str/blank? (str namespace))
       (do (stdout/error "--namespace is required: name the namespace to audit") nil)
       (let [inv (inventory (kubectl/current-context) namespace inventory-opts)]
         (if-let [why (:unread inv)]
           (do (stdout/error (str "nothing was audited: " why))
               {unmanaged/inventory-key nil})
           (do (when-let [partial (unread-summary inv)]
                 (outcome/incomplete! partial))
               {unmanaged/inventory-key inv})))))))
