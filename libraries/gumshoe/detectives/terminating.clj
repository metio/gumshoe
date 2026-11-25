;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.terminating
  "Detective for objects whose deletion hangs on their finalizers. A deleted
   object stays until every finalizer is removed by whoever placed it, so a
   finalizer whose controller is down or confused keeps it forever. Pods and
   namespaces have detectives of their own."
  (:require [clojure.string :as str]
            [gumshoe.catalog :as catalog]
            [gumshoe.evidence :as evidence]
            [gumshoe.finalizers :as finalizers]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]
            [gumshoe.subject :as subject]))

(def types
  "The resource types read, with the kind each holds."
  [["deployments" "Deployment"] ["statefulsets" "StatefulSet"] ["daemonsets" "DaemonSet"]
   ["replicasets" "ReplicaSet"] ["jobs" "Job"] ["cronjobs" "CronJob"]
   ["services" "Service"] ["ingresses" "Ingress"] ["configmaps" "ConfigMap"]
   ["persistentvolumeclaims" "PersistentVolumeClaim"] ["persistentvolumes" "PersistentVolume"]])

(def ^:private stuck-minutes 10)
(def ^:private critical-minutes 30)

(defn- object-name
  "namespace/name, or the bare name of a cluster-scoped object."
  [object]
  (if (kubectl/namespace-of object) (kubectl/namespace-name-of object) (kubectl/name-of object)))

(defn- claim-users
  "The pods that mount a claim, which is what its pvc-protection waits on."
  [pods claim]
  (for [pod pods
        :when (and (= (kubectl/namespace-of claim) (kubectl/namespace-of pod))
                   (some #(= (kubectl/name-of claim) (-> % :persistentVolumeClaim :claimName))
                         (-> pod :spec :volumes)))]
    (kubectl/name-of pod)))

(defn- finalizer-hint
  [type object pods]
  (let [all (-> object :metadata :finalizers)
        removable (remove finalizers/protected? all)]
    (str/join "; "
              (concat
               (for [finalizer all
                     :let [users (when (= "kubernetes.io/pvc-protection" finalizer) (seq (claim-users pods object)))]]
                 (str finalizer " " (finalizers/explain finalizer)
                      (when users (str " - mounted by " (str/join ", " users)))))
               (when (seq removable)
                 [(str "check the controller that owns " (str/join ", " removable)
                       "; only if it is gone for good: " (catalog/run-command "runbooks/kubernetes/finalizers/remove.clj") " --pick '"
                       type " " (object-name object) " " (first removable) "'")])))))

(def subject-reads
  "The reads holding the objects judged. The pods only name who mounts a
   claim in its hint, so they judge nothing on their own."
  (mapv first types))

(defn detect-stuck-terminating
  "Objects whose deletion has waited on finalizers for over ten minutes, each
   type judged when its read answered. Nothing is judged when no subject read
   answered."
  [evidence]
  (let [now ^java.time.Instant (:now evidence)
        pods (kubectl/items-of (get evidence "pods"))]
    (for [[type kind] (when (evidence/consulted? evidence subject-reads) types)
          object (kubectl/items-of (get evidence type))
          :let [deletion (some-> object :metadata :deletionTimestamp java.time.Instant/parse)
                waiting (-> object :metadata :finalizers)
                minutes (when deletion (.toMinutes (java.time.Duration/between deletion now)))]
          :when (and deletion (seq waiting) (<= stuck-minutes minutes))]
      (onset/since
       {:severity (if (<= critical-minutes minutes) :critical :warning)
        :component (object-name object)
        :summary (format "%s is stuck terminating for %d minutes, waiting on %s"
                         kind minutes (str/join ", " waiting))
        :hint (finalizer-hint type object pods)
        :subject (subject/subject kind (kubectl/namespace-of object) (kubectl/name-of object))}
       deletion))))

(def detectives
  [{:name "stuck-terminating"
    :description "Workloads, Services, Ingresses, ConfigMaps and volumes whose deletion hangs on finalizers"
    :requires (conj subject-reads "pods")
    :consults subject-reads
    :detect detect-stuck-terminating}])
