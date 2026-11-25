;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.template-drift
  "Pods that differ from the template their controller created them from. A
   mutating webhook or a LimitRange can change a pod on admission, so what runs
   is no longer what the manifest says: another image, other requests, another
   environment.

   Each pod is compared with the template at its own revision: a ReplicaSet's
   or a Job's template is the one its pods came from, a StatefulSet pod counts
   only at the update revision, and a DaemonSet pod only at the revision of its
   newest ControllerRevision, so a rollout in progress is not drift.

   Admission routinely adds things, and those stay quiet: injected containers,
   environment variables and volumes the template does not name, a digest
   pinned to the same image, another spelling of the same image, an equal
   quantity written differently, and requests the API server copies from
   limits."
  (:require [clojure.string :as str]
            [gumshoe.evidence :as evidence]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.quantity :as quantity]
            [gumshoe.subject :as subject]))

;; --- images ----------------------------------------------------------------------

(defn- split-digest
  [image]
  (let [[name digest] (str/split (str image) #"@" 2)]
    [name digest]))

(defn normalize-image
  "An image reference without its registry defaults: docker.io and its library
   namespace are dropped, and a missing tag reads as latest."
  [image]
  (let [[name _] (split-digest image)
        bare (-> name
                 (str/replace #"^(?:index\.)?docker\.io/library/" "")
                 (str/replace #"^(?:index\.)?docker\.io/" ""))
        last-segment (last (str/split bare #"/"))]
    (if (str/includes? last-segment ":") bare (str bare ":latest"))))

(defn image-changed?
  "Whether the pod runs another image than the template names. A digest the pod
   adds to the same reference pins it and is not a change; a different digest
   is."
  [template pod]
  (let [[_ template-digest] (split-digest template)
        [_ pod-digest] (split-digest pod)]
    (or (not= (normalize-image template) (normalize-image pod))
        (boolean (and template-digest pod-digest (not= template-digest pod-digest))))))

;; --- resources -------------------------------------------------------------------

(def ^:private always-compared #{:cpu :memory})

(defn- same-quantity?
  [a b]
  (let [x (quantity/quantity->number a)
        y (quantity/quantity->number b)]
    (if (and x y)
      (<= (Math/abs (- (double x) (double y))) (* 1e-9 (max 1.0 (Math/abs (double x)))))
      (= (str a) (str b)))))

(defn- resource-diffs
  [template pod]
  (let [t (:resources template)
        p (:resources pod)]
    (for [kind [:requests :limits]
          resource (sort (into always-compared (keys (get t kind))))
          :let [tv (get-in t [kind resource])
                pv (get-in p [kind resource])
                ;; subs, not name: an extended resource keeps its domain (nvidia.com/gpu)
                label (str (subs (str resource) 1) " " (if (= kind :requests) "request" "limit"))
                defaulted-from-limit? (and (= kind :requests) (nil? tv)
                                           (some? (get-in t [:limits resource]))
                                           (same-quantity? pv (get-in p [:limits resource])))]
          diff [(cond
                  (and tv pv (not (same-quantity? tv pv))) (format "%s %s → %s" label tv pv)
                  (and (nil? tv) pv (not defaulted-from-limit?)) (format "%s %s, which the template does not set" label pv)
                  (and tv (nil? pv)) (format "%s %s removed" label tv)
                  :else nil)]
          :when diff]
      diff)))

;; --- environment and process -----------------------------------------------------

(defn- env-diffs
  [template pod]
  (let [running (into {} (map (juxt :name identity)) (:env pod))]
    (for [{:keys [name] :as wanted} (:env template)
          :let [actual (get running name)
                diff (cond
                       (nil? actual) (format "env %s removed" name)
                       (not= (dissoc wanted :name) (dissoc actual :name)) (format "env %s changed" name)
                       :else nil)]
          :when diff]
      diff)))

(defn container-diffs
  "How the pod's container differs from the template's, as short phrases."
  [template pod]
  (concat
   (when (image-changed? (:image template) (:image pod))
     [(format "image %s → %s" (:image template) (:image pod))])
   (resource-diffs template pod)
   (env-diffs template pod)
   (when (and (contains? template :command) (not= (:command template) (:command pod))) ["command changed"])
   (when (and (contains? template :args) (not= (:args template) (:args pod))) ["args changed"])))

(defn pod-diffs
  "Every difference between a pod and the template spec it came from, each
   prefixed with its container. A template container the pod lacks is a
   difference; a container only the pod has was injected and is not."
  [template-spec pod-spec]
  (for [field [:initContainers :containers]
        wanted (get template-spec field)
        :let [actual (first (filter #(= (:name wanted) (:name %)) (get pod-spec field)))]
        diff (if actual
               (map #(str (:name wanted) ": " %) (container-diffs wanted actual))
               [(str "container " (:name wanted) " removed")])]
    diff))

;; --- owners and revisions --------------------------------------------------------

(defn- controller-of
  [object]
  (first (filter :controller (-> object :metadata :ownerReferences))))

(defn- index-by-name
  [items]
  (into {} (map (juxt kubectl/namespace-name-of identity)) items))

(defn- revision-hash
  [pod]
  (get-in pod [:metadata :labels :controller-revision-hash]))

(defn- newest-revision-hashes
  "Per DaemonSet (namespace/name), the hash of its newest ControllerRevision -
   the suffix of the revision's name after the DaemonSet's."
  [revisions]
  (->> revisions
       (keep (fn [revision]
               (when-let [owner (controller-of revision)]
                 (when (= "DaemonSet" (:kind owner))
                   [(str (kubectl/namespace-of revision) "/" (:name owner)) revision]))))
       (group-by first)
       (into {} (map (fn [[k pairs]]
                       (let [newest (apply max-key #(or (:revision (second %)) 0) pairs)
                             owner-name (last (str/split k #"/"))]
                         [k (str/replace-first (kubectl/name-of (second newest)) (str owner-name "-") "")]))))))

(defn- origin
  "What a pod is compared against: {:spec template-spec :kind :name} for the
   workload to report, or nil when the pod has no comparable template - no
   controller, an unknown one, or a revision other than the current one."
  [pod {:keys [replicasets statefulsets daemonsets jobs ds-hashes]}]
  (when-let [{:keys [kind name]} (controller-of pod)]
    (let [ns (kubectl/namespace-of pod)
          key (str ns "/" name)
          lift (fn [object fallback-kind]
                 (let [owner (controller-of object)]
                   (if (and owner (#{"Deployment" "CronJob"} (:kind owner)))
                     {:kind (:kind owner) :name (:name owner)}
                     {:kind fallback-kind :name name})))]
      (case kind
        "ReplicaSet" (when-let [rs (get replicasets key)]
                       (assoc (lift rs "ReplicaSet") :spec (-> rs :spec :template :spec)))
        "Job" (when-let [job (get jobs key)]
                (assoc (lift job "Job") :spec (-> job :spec :template :spec)))
        "StatefulSet" (when-let [sts (get statefulsets key)]
                        (when (and (revision-hash pod) (= (revision-hash pod) (-> sts :status :updateRevision)))
                          {:kind "StatefulSet" :name name :spec (-> sts :spec :template :spec)}))
        "DaemonSet" (when-let [ds (get daemonsets key)]
                      (when (and (revision-hash pod) (= (revision-hash pod) (get ds-hashes key)))
                        {:kind "DaemonSet" :name name :spec (-> ds :spec :template :spec)}))
        nil))))

(def ^:private shown 4)

(def template-drift-joins
  "The pods and every owner they are compared through: without an owner's
   read its pods would have no template to differ from, and the check would
   read as passed."
  ["pods" "replicasets" "statefulsets" "daemonsets" "jobs" "controllerrevisions"])

(defn detect-template-drift
  "Workloads whose pods differ from the template they came from. Nothing is judged when a joined read did not answer."
  [evidence]
  (let [items #(kubectl/items-of (get evidence %))
        context {:replicasets (index-by-name (items "replicasets"))
                 :statefulsets (index-by-name (items "statefulsets"))
                 :daemonsets (index-by-name (items "daemonsets"))
                 :jobs (index-by-name (items "jobs"))
                 :ds-hashes (newest-revision-hashes (items "controllerrevisions"))}
        drifted (for [pod (when (evidence/joined? evidence template-drift-joins) (items "pods"))
                      :when (not (-> pod :metadata :deletionTimestamp))
                      :let [{:keys [spec] :as from} (origin pod context)]
                      :when from
                      :let [diffs (vec (distinct (pod-diffs spec (:spec pod))))]
                      :when (seq diffs)]
                  {:namespace (kubectl/namespace-of pod) :kind (:kind from) :name (:name from) :diffs diffs})]
    (for [[[namespace kind name] pods] (group-by (juxt :namespace :kind :name) drifted)
          :let [diffs (vec (distinct (mapcat :diffs pods)))
                n (count pods)]]
      {:severity :warning
       :component (str namespace "/" name)
       :subject (subject/subject kind namespace name)
       :summary "pods differ from the template they were created from"
       :hint (format "%s %s/%s: %d pod%s, %s%s - a mutating webhook or a LimitRange changed them on admission, so what runs is not what the manifest says"
                     kind namespace name n (if (= 1 n) "" "s")
                     (str/join "; " (take shown diffs))
                     (if (> (count diffs) shown) (format " (+%d more)" (- (count diffs) shown)) ""))})))

(def detectives
  [{:name "template-drift"
    :description "Pods that a mutating webhook or a LimitRange changed away from their controller's template"
    :requires template-drift-joins
    :joins template-drift-joins
    :detect detect-template-drift}])
