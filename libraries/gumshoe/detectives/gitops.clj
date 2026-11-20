;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.gitops
  "Workloads no GitOps controller manages. Such a workload was applied by hand
   or by a pipeline, so nothing reconciles it back when it drifts, and a
   rebuild from the repository leaves it out.

   Each controller marks what it applies: Flux's kustomize- and helm-controller
   label every object with <group>/name and <group>/namespace, so does the
   StageSet controller, and Argo CD's default annotation tracking writes
   argocd.argoproj.io/tracking-id. Argo CD's label tracking writes only
   app.kubernetes.io/instance, which Helm charts set as well, so it cannot be
   told apart and such workloads read as unmanaged. Helm records its release in
   meta.helm.sh annotations; a Helm release outside GitOps is reproducible from
   its chart but still applied by hand, which makes it the softer finding.

   Workloads with an owner are left to their owner (an operator's custom
   resource is what GitOps would manage), and kube-system is left to the
   installer that set the cluster up."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def workload-types
  [["deployments" "Deployment"] ["statefulsets" "StatefulSet"] ["daemonsets" "DaemonSet"] ["cronjobs" "CronJob"]])

(def ^:private owner-label-groups
  [["kustomize.toolkit.fluxcd.io" "Flux Kustomization"]
   ["helm.toolkit.fluxcd.io" "Flux HelmRelease"]
   ["stages.metio.wtf" "StageSet"]])

(def ^:private skipped-namespaces #{"kube-system"})

(defn manager
  "Who applies an object: {:kind :gitops|:helm :by description}, or nil."
  [object]
  (let [{:keys [labels annotations]} (:metadata object)
        label #(get labels (keyword %))
        annotation #(get annotations (keyword %))]
    (or (some (fn [[group what]]
                (when-let [name (label (str group "/name"))]
                  {:kind :gitops
                   :by (str what " " (str/join "/" (remove str/blank? [(label (str group "/namespace")) name])))}))
              owner-label-groups)
        (when-let [tracking (annotation "argocd.argoproj.io/tracking-id")]
          {:kind :gitops :by (str "Argo CD Application " (first (str/split tracking #":")))})
        (when-let [release (or (annotation "meta.helm.sh/release-name")
                               (when (= "Helm" (label "app.kubernetes.io/managed-by")) (label "app.kubernetes.io/instance")))]
          {:kind :helm :by (str "Helm release " release)}))))

(defn- candidates
  [evidence]
  (for [[type kind] workload-types
        object (kubectl/items-of (get evidence type))
        :when (and (empty? (-> object :metadata :ownerReferences))
                   (not (skipped-namespaces (-> object :metadata :namespace))))]
    [kind object]))

(defn detect-unmanaged-workloads
  [evidence]
  (let [workloads (candidates evidence)
        managed (map (fn [[kind object]] [kind object (manager object)]) workloads)]
    (cond
      (empty? workloads) []

      (not-any? #(= :gitops (:kind (nth % 2))) managed)
      [{:severity :info
        :component "cluster"
        :summary "No workload is managed by a GitOps controller"
        :hint (format "none of the %d Deployments, StatefulSets, DaemonSets and CronJobs outside kube-system carries a Flux, Argo CD or StageSet marker - if this cluster uses GitOps, its controller does not label what it applies"
                      (count workloads))}]

      :else
      (for [[kind object who] managed
            :when (not= :gitops (:kind who))]
        (if who
          {:severity :info
           :component (kubectl/namespace-name-of object)
           :summary (format "%s is installed by Helm outside GitOps" kind)
           :hint (str (:by who) " - an upgrade or rollback is a manual helm command and nothing reverts drift; move the release into a HelmRelease or an Application")}
          {:severity :warning
           :component (kubectl/namespace-name-of object)
           :summary (format "%s is managed by no GitOps controller" kind)
           :hint "it carries no Flux, Argo CD, StageSet or Helm marker, so it was applied by hand - nothing reverts drift, and rebuilding the cluster from its repository leaves it out"})))))

(def detectives
  [{:name "gitops-coverage"
    :description "Workloads no GitOps controller manages, and Helm releases installed outside GitOps"
    :requires (mapv first workload-types)
    :detect detect-unmanaged-workloads}])
