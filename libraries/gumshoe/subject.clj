;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.subject
  "The subject of an investigation - the single object you are focused on right
   now - and the pure logic to summarize it and find what it relates to.

   Detectives tell you *what* is wrong; drilling down answers *why* by following
   the edges between objects (a pod to its node, a node to its other pods, a
   route to its backend service) and reaching for live probes. The engine does
   the live fetching and the interactive loop; everything in here is pure over
   an already-fetched object, so the graph logic is fully testable."
  (:require [clojure.string :as str]))

(defn subject
  "An investigation subject: a Kubernetes object by kind, namespace (nil for a
   cluster-scoped object), and name."
  ([kind name] (subject kind nil name))
  ([kind namespace name] {:kind kind :namespace namespace :name name}))

(defn display
  [{:keys [kind namespace name]}]
  (if (str/blank? (str namespace))
    (format "%s %s" kind name)
    (format "%s %s/%s" kind namespace name)))

(defonce ^:private kind-types
  ;; The kubectl resource type for a kind, so the engine can fetch it. A plugin
  ;; adds its CRD kinds with register-kind!.
  (atom {"Pod" "pods"
         "Node" "nodes"
         "PersistentVolumeClaim" "persistentvolumeclaims"
         "PersistentVolume" "persistentvolumes"
         "Deployment" "deployments"
         "StatefulSet" "statefulsets"
         "DaemonSet" "daemonsets"
         "ReplicaSet" "replicasets"
         "Job" "jobs"
         "Service" "services"
         "Ingress" "ingresses"
         "HTTPRoute" "httproutes.gateway.networking.k8s.io"
         "HorizontalPodAutoscaler" "horizontalpodautoscalers"}))

(defn kind->type
  "The kubectl resource type for a kind, or nil when the kind is unknown."
  [kind]
  (get @kind-types kind))

;; ---------------------------------------------------------------------------
;; Facts: a pure at-a-glance summary of an object, as ordered [label value]
;; pairs, dropping anything absent so the panel shows only what is known.

(defn- present-pairs
  [pairs]
  (vec (for [[label value] pairs
             :when (and (some? value) (not= "" (str value)))]
         [label (str value)])))

(defn- container-issue
  "The problem on a container, named - 'web: CrashLoopBackOff' - so a multi-
   container pod points at the container that is actually failing."
  [status]
  (when-let [reason (or (-> status :state :waiting :reason)
                        (-> status :state :terminated :reason))]
    (if-let [name (:name status)]
      (str name ": " reason)
      reason)))

(defn- pod-facts
  [pod]
  (let [statuses (-> pod :status :containerStatuses)
        containers (-> pod :spec :containers)
        ready (count (filter :ready statuses))
        restarts (reduce + 0 (keep :restartCount statuses))
        issues (distinct (keep container-issue statuses))
        ;; spotlight the container in trouble (its image and limits are what an
        ;; OOM or a pull error hinges on), else the first container.
        failing-name (some #(when (or (-> % :state :waiting) (-> % :state :terminated)) (:name %)) statuses)
        primary (or (first (filter #(= failing-name (:name %)) containers)) (first containers))]
    (present-pairs
     [["phase" (-> pod :status :phase)]
      ["ready" (when (seq statuses) (format "%d/%d containers" ready (count statuses)))]
      ["restarts" (when (pos? restarts) restarts)]
      ["node" (-> pod :spec :nodeName)]
      ["issue" (when (seq issues) (str/join ", " issues))]
      ["image" (:image primary)]
      ["mem limit" (-> primary :resources :limits :memory)]
      ["cpu limit" (-> primary :resources :limits :cpu)]
      ["message" (-> pod :status :message)]])))

(defn- bad-node-conditions
  [node]
  (for [condition (-> node :status :conditions)
        :let [type (:type condition) status (:status condition)]
        :when (or (and (= "Ready" type) (not= "True" status))
                  (and (not= "Ready" type) (= "True" status)))]
    type))

(defn- node-facts
  [node]
  (let [ready (first (filter #(= "Ready" (:type %)) (-> node :status :conditions)))
        bad (bad-node-conditions node)
        allocatable (-> node :status :allocatable)]
    (present-pairs
     [["ready" (:status ready)]
      ["pressure" (when (seq (remove #{"Ready"} bad)) (str/join ", " (remove #{"Ready"} bad)))]
      ["schedulable" (if (-> node :spec :unschedulable) "no (cordoned)" "yes")]
      ["allocatable" (when allocatable
                       (format "%s cpu · %s mem · %s pods"
                               (:cpu allocatable) (:memory allocatable) (:pods allocatable)))]
      ["kubelet" (-> node :status :nodeInfo :kubeletVersion)]
      ["kernel" (-> node :status :nodeInfo :kernelVersion)]])))

(defn- hpa-facts
  [hpa]
  (let [status (:status hpa)
        limited (first (filter #(and (= "ScalingLimited" (:type %)) (= "True" (:status %)))
                               (:conditions status)))
        target (-> hpa :spec :scaleTargetRef)]
    (present-pairs
     [["replicas" (when (some? (:currentReplicas status))
                    (format "%s current / %s desired" (:currentReplicas status) (:desiredReplicas status)))]
      ["range" (format "%s..%s replicas" (or (-> hpa :spec :minReplicas) 1) (-> hpa :spec :maxReplicas))]
      ["target" (when target (format "%s/%s" (:kind target) (:name target)))]
      ["limited" (when limited (:reason limited))]])))

(defn- job-facts
  [job]
  (let [failed (first (filter #(and (= "Failed" (:type %)) (= "True" (:status %)))
                              (-> job :status :conditions)))]
    (present-pairs
     [["completions" (format "%s/%s succeeded"
                             (or (-> job :status :succeeded) 0) (or (-> job :spec :completions) 1))]
      ["active" (-> job :status :active)]
      ["failed" (-> job :status :failed)]
      ["reason" (when failed (or (:reason failed) (:message failed)))]])))

(defn- pvc-facts
  [pvc]
  (present-pairs
   [["phase" (-> pvc :status :phase)]
    ["capacity" (-> pvc :status :capacity :storage)]
    ["storageClass" (-> pvc :spec :storageClassName)]
    ["volume" (-> pvc :spec :volumeName)]
    ["accessModes" (some->> pvc :spec :accessModes (str/join ", "))]]))

(defn- workload-facts
  [workload]
  (let [spec (:spec workload)
        status (:status workload)]
    (present-pairs
     [["replicas" (format "%s/%s ready"
                          (or (:readyReplicas status) 0)
                          (or (:replicas spec) 0))]
      ["updated" (:updatedReplicas status)]
      ["available" (:availableReplicas status)]
      ["unavailable" (:unavailableReplicas status)]])))

(defn- service-facts
  [service]
  (let [spec (:spec service)]
    (present-pairs
     [["type" (:type spec)]
      ["clusterIP" (:clusterIP spec)]
      ["ports" (some->> spec :ports (map #(str (:port %) "/" (:protocol %))) (str/join ", "))]
      ["selector" (some->> spec :selector (map (fn [[k v]] (str (name k) "=" v))) (str/join ","))]])))

(defn- pv-facts
  [pv]
  (let [csi (-> pv :spec :csi)
        attributes (:volumeAttributes csi)
        claim (-> pv :spec :claimRef)]
    (present-pairs
     (concat
      [["phase" (-> pv :status :phase)]
       ["capacity" (-> pv :spec :capacity :storage)]
       ["reclaimPolicy" (-> pv :spec :persistentVolumeReclaimPolicy)]
       ["storageClass" (-> pv :spec :storageClassName)]
       ["claim" (when claim (format "%s/%s" (:namespace claim) (:name claim)))]
       ["csi driver" (:driver csi)]]
      ;; the CSI volume's backing attributes (a ceph-csi pool/image, a Longhorn
      ;; handle, ...), so the operator can look the volume up on the storage side -
      ;; the storage root cause often lives there, not in Kubernetes.
      (for [[k v] attributes] [(str "csi " (name k)) v])))))

(defn- ingress-facts
  [ingress]
  (present-pairs
   [["hosts" (some->> ingress :spec :rules (keep :host) distinct seq (str/join ", "))]
    ["class" (-> ingress :spec :ingressClassName)]
    ["address" (some->> ingress :status :loadBalancer :ingress
                        (keep #(or (:ip %) (:hostname %))) seq (str/join ", "))]]))

(defn- httproute-facts
  [route]
  (let [accepted (for [parent (-> route :status :parents)
                       condition (:conditions parent)
                       :when (= "Accepted" (:type condition))]
                   (:status condition))]
    (present-pairs
     [["hostnames" (some->> route :spec :hostnames (str/join ", "))]
      ["parents" (some->> route :spec :parentRefs (map :name) (str/join ", "))]
      ["accepted" (when (seq accepted) (str/join ", " accepted))]])))

(defn- generic-facts
  [object]
  (present-pairs
   [["kind" (:kind object)]
    ["created" (-> object :metadata :creationTimestamp)]
    ["phase" (or (-> object :status :phase) (-> object :status :state))]]))

(defn facts
  "A pure, ordered [label value] summary of an object by kind - the at-a-glance
   panel shown when you focus a subject."
  [kind object]
  (case kind
    "Pod" (pod-facts object)
    "Node" (node-facts object)
    "PersistentVolumeClaim" (pvc-facts object)
    ("Deployment" "StatefulSet" "DaemonSet" "ReplicaSet") (workload-facts object)
    "Service" (service-facts object)
    "PersistentVolume" (pv-facts object)
    "Ingress" (ingress-facts object)
    "HTTPRoute" (httproute-facts object)
    "HorizontalPodAutoscaler" (hpa-facts object)
    "Job" (job-facts object)
    (generic-facts object)))

;; ---------------------------------------------------------------------------
;; Edges: the related subjects discoverable from the object itself. Edges that
;; need a cluster query (a node's pods, a service's endpoints) are added by the
;; engine; these are pure.

(defn owner-edges
  "The workloads that own this object, from its ownerReferences. Following these
   walks Pod -> ReplicaSet -> Deployment one uniform step at a time."
  [object]
  (for [ref (-> object :metadata :ownerReferences)]
    {:relation "owned by"
     :subject (subject (:kind ref) (-> object :metadata :namespace) (:name ref))}))

(defn- pod-edges
  [pod]
  (let [namespace (-> pod :metadata :namespace)]
    (concat
     (owner-edges pod)
     (when-let [node (-> pod :spec :nodeName)]
       [{:relation "runs on" :subject (subject "Node" nil node)}])
     (for [volume (-> pod :spec :volumes)
           :let [claim (-> volume :persistentVolumeClaim :claimName)]
           :when claim]
       {:relation "mounts" :subject (subject "PersistentVolumeClaim" namespace claim)}))))

(defn- httproute-edges
  [route]
  (let [namespace (-> route :metadata :namespace)]
    (distinct
     (for [rule (-> route :spec :rules)
           ref (:backendRefs rule)
           :when (contains? #{nil "Service"} (:kind ref))
           :when (:name ref)]
       {:relation "routes to"
        :subject (subject "Service" (or (:namespace ref) namespace) (:name ref))}))))

(defn- pvc-edges
  [pvc]
  (when-let [volume (-> pvc :spec :volumeName)]
    [{:relation "bound to" :subject (subject "PersistentVolume" nil volume)}]))

(defn- pv-edges
  [pv]
  (when-let [claim (-> pv :spec :claimRef)]
    [{:relation "claimed by"
      :subject (subject "PersistentVolumeClaim" (:namespace claim) (:name claim))}]))

(defn- hpa-edges
  [hpa]
  (let [ref (-> hpa :spec :scaleTargetRef)]
    (when (:name ref)
      [{:relation "scales"
        :subject (subject (:kind ref) (-> hpa :metadata :namespace) (:name ref))}])))

(defn- ingress-edges
  [ingress]
  (let [namespace (-> ingress :metadata :namespace)
        service-names (distinct
                       (concat
                        (some-> ingress :spec :defaultBackend :service :name vector)
                        (for [rule (-> ingress :spec :rules)
                              path (-> rule :http :paths)
                              :let [name (-> path :backend :service :name)]
                              :when name]
                          name)))]
    (for [name service-names]
      {:relation "routes to" :subject (subject "Service" namespace name)})))

(defonce ^:private edge-builders
  ;; kind -> (fn [object] -> seq of {:relation :subject}). A kind without a
  ;; registered builder falls back to following ownerReferences, so most
  ;; workloads need no entry. A plugin adds a CRD's edges with register-kind!.
  (atom {"Pod" pod-edges
         "HTTPRoute" httproute-edges
         "Ingress" ingress-edges
         "PersistentVolumeClaim" (fn [object] (concat (pvc-edges object) (owner-edges object)))
         "PersistentVolume" pv-edges
         "HorizontalPodAutoscaler" hpa-edges}))

(defn object-edges
  "Pure: the related subjects reachable from the object itself. Uses the kind's
   registered edge builder, or follows ownerReferences when none is registered."
  [kind object]
  (vec ((get @edge-builders kind owner-edges) object)))

(defn register-kind!
  "Makes a custom resource kind a first-class drill-down subject: its kubectl
   :type (so the engine can fetch it) and an optional :edges builder,
   (fn [object] -> seq of {:relation \"...\" :subject <subject>}), that traverses
   from it - a HelmRelease to what it manages, a Rollout to its ReplicaSets, a
   CloudNativePG Cluster to its pods. Called by a plugin, so a CRD drills down
   like the built-in kinds."
  [kind {:keys [type edges]}]
  (when type (swap! kind-types assoc kind type))
  (when edges (swap! edge-builders assoc kind edges)))

(defn exposes-service?
  "Whether a route or ingress object routes to the given Service subject - the
   pure test behind a service's 'exposed by' edges, so the traffic graph is
   walkable up (to what fronts a service) as well as down (to its pods)."
  [kind object service-subject]
  (some #(= service-subject (:subject %)) (object-edges kind object)))

(defn service-selects-pod?
  "Whether a service's selector matches a pod - the pure test behind a pod's
   'member of' edges, so from a pod you can walk up to the services that front
   it. An empty selector (a headless or externally-managed service) matches
   nothing."
  [service pod]
  (let [selector (-> service :spec :selector)]
    (boolean
     (and (seq selector)
          (every? (fn [[k v]] (= v (get-in pod [:metadata :labels k]))) selector)))))

;; ---------------------------------------------------------------------------
;; Situation: a pure classification of what looks wrong with an object right
;; now. This is the "smart" core - the engine uses it to auto-surface the most
;; useful probe and to rank the leads, so the operator sees the smoking gun
;; instead of a menu to hunt through. :ok means nothing stands out.

(defn- pod-situation
  [pod]
  (let [statuses (-> pod :status :containerStatuses)
        waiting (set (keep #(-> % :state :waiting :reason) statuses))
        terminated (set (keep #(-> % :state :terminated :reason) statuses))
        phase (-> pod :status :phase)]
    (cond
      (contains? waiting "CrashLoopBackOff") :crash-looping
      (some waiting ["ImagePullBackOff" "ErrImagePull"]) :image-pull-error
      (contains? terminated "OOMKilled") :oom-killed
      (= "Pending" phase) :pending
      (= "Failed" phase) :failed
      (and (= "Running" phase)
           (seq statuses)
           (not-every? :ready statuses)) :not-ready
      :else :ok)))

(defn- node-situation
  [node]
  (let [conditions (-> node :status :conditions)
        by-type (into {} (map (juxt :type :status) conditions))]
    (cond
      (not= "True" (get by-type "Ready")) :not-ready
      (= "True" (get by-type "DiskPressure")) :disk-pressure
      (= "True" (get by-type "MemoryPressure")) :memory-pressure
      (= "True" (get by-type "PIDPressure")) :pid-pressure
      (-> node :spec :unschedulable) :cordoned
      :else :ok)))

(defn- pvc-situation
  [pvc]
  (if (not= "Bound" (-> pvc :status :phase)) :unbound :ok))

(defn- pv-situation
  [pv]
  (case (-> pv :status :phase)
    "Failed" :volume-failed
    "Released" :released
    :ok))

(defn- workload-situation
  [workload]
  (let [desired (or (-> workload :spec :replicas) 0)
        ready (or (-> workload :status :readyReplicas) 0)]
    (cond
      (and (pos? desired) (zero? ready)) :none-ready
      (< ready desired) :degraded
      :else :ok)))

(defn- hpa-situation
  [hpa]
  (let [current (-> hpa :status :currentReplicas)
        maximum (-> hpa :spec :maxReplicas)]
    (if (and (some? current) (some? maximum) (>= current maximum)) :at-max :ok)))

(defn- job-situation
  [job]
  (if (some #(and (= "Failed" (:type %)) (= "True" (:status %))) (-> job :status :conditions))
    :job-failed
    :ok))

(defn situation
  "A pure classification of what looks wrong with an object, or :ok. Drives the
   engine's auto-surfaced probe and lead ranking."
  [kind object]
  (case kind
    "Pod" (pod-situation object)
    "Node" (node-situation object)
    "PersistentVolumeClaim" (pvc-situation object)
    "PersistentVolume" (pv-situation object)
    ("Deployment" "StatefulSet" "DaemonSet" "ReplicaSet") (workload-situation object)
    "HorizontalPodAutoscaler" (hpa-situation object)
    "Job" (job-situation object)
    :ok))

;; ---------------------------------------------------------------------------
;; Starting from a finding: infer the subject a detective finding is about, so
;; the operator can drill straight in from what a scan surfaced.

(def ^:private detective->kind
  "The object kind a detective's findings are about, keyed by the detective name,
   where the finding's :component is that object's namespace/name (or name, for a
   cluster-scoped kind). Detectives whose component is not a single object - a
   whole-cluster overcommit summary, a namespace quota - are absent, so drilling
   is only offered where it lands on a real object."
  {"pods" "Pod"
   "stuck-pods" "Pod"
   "nodes" "Node"
   "storage" "PersistentVolumeClaim"
   "deployments" "Deployment"
   "statefulsets" "StatefulSet"
   "daemonsets" "DaemonSet"
   "jobs" "Job"
   "hpas" "HorizontalPodAutoscaler"
   "httproutes" "HTTPRoute"
   "loki-health" "Deployment"})

(defn from-finding
  "The subject a finding points at, or nil when it can not be pinned to an
   object. A detective can attach an explicit :subject to a finding (when its
   :component is not the object's own name - Loki labels a finding by the
   component role, not the workload name); otherwise the kind is inferred from
   the :detective and the object taken from the \"namespace/name\" (or bare
   \"name\") :component."
  [{explicit :subject :keys [detective component]}]
  (or explicit
      (when-let [kind (get detective->kind detective)]
        (let [component (str component)]
          (if (str/includes? component "/")
            (let [[namespace name] (str/split component #"/" 2)]
              (subject kind namespace name))
            (subject kind nil component))))))
