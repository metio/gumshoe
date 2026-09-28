;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux
  "The flux tool package: everything gumshoe knows about Flux, in one place and
   registered through a single plugin/provide!. Requiring this namespace (which a
   casebook does via env.edn :plugins, or a flux book does at its top) contributes
   flux's detectives, its :flux cluster capability, the flux CLI tool profile, and
   the drill-down (its CRD kinds plus a reconcile-status probe). Nothing about
   flux lives in the engine any more."
  (:require [clojure.string]
            [gumshoe.investigation :as investigation]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]
            [gumshoe.subject :as subject]))

(def helmrelease-type "helmreleases.helm.toolkit.fluxcd.io")
(def kustomization-type "kustomizations.kustomize.toolkit.fluxcd.io")
(def gitrepository-type "gitrepositories.source.toolkit.fluxcd.io")
(def ocirepository-type "ocirepositories.source.toolkit.fluxcd.io")
(def helmchart-type "helmcharts.source.toolkit.fluxcd.io")
(def externalartifact-type "externalartifacts.source.toolkit.fluxcd.io")

(defn- ready-condition
  [resource]
  (first (filter #(= "Ready" (:type %)) (-> resource :status :conditions))))

(def ^:private reconcile-annotations
  ["kustomize.toolkit.fluxcd.io/reconcile"
   "fluxcd.controlplane.io/reconcile"])

(defn- annotation
  [resource key]
  (get-in resource [:metadata :annotations (keyword key)]))

(defn inert?
  "An object told not to reconcile. It carries every label and annotation of a
   managed object and is not managed - a gate left on after a migration reads as
   healthy from every angle except the age of what it serves."
  [resource]
  (some #(= "disabled" (annotation resource %)) reconcile-annotations))

(defn- retry-on-failure?
  [release phase]
  (= "RetryOnFailure" (get-in release [:spec phase :strategy :name])))

(defn- failures
  [release key]
  (or (get-in release [:status key]) 0))

(defn- retries
  [release phase]
  (or (get-in release [:spec phase :remediation :retries]) 0))

(defn parked?
  "A failed release that will not try again. Flux defaults both install and
   upgrade to `retries: 0`, and once they are spent the release is left failed
   until its spec or chart changes - so `status.conditions` keeps describing a
   cause that may have cleared long ago. `RetryOnFailure` keeps retrying on its
   own interval, so a release using it is never parked."
  [release]
  (boolean
   (or (and (not (retry-on-failure? release :install))
            (> (failures release :installFailures) (retries release :install)))
       (and (not (retry-on-failure? release :upgrade))
            (> (failures release :upgradeFailures) (retries release :upgrade))))))

(defn flux-findings
  [resources kind]
  (concat
   (for [resource resources
         :let [ready (ready-condition resource)]
         :when (= "False" (:status ready))]
     {:severity :critical
      :component (kubectl/namespace-name-of resource)
      :summary (format "%s is not Ready (%s)" kind (or (:reason ready) "unknown"))
      :hint (:message ready)})
   (for [resource resources
         :when (true? (-> resource :spec :suspend))]
     {:severity :info
      :component (kubectl/namespace-name-of resource)
      :summary (format "%s is suspended" kind)
      :hint "resume it once the reason for the suspension is resolved"})))

(defn detect-helmrelease-problems
  [evidence]
  (let [releases (kubectl/items-of (get evidence helmrelease-type))
        stuck (filter parked? releases)]
    (concat
     ;; a parked release is reported here instead, with the remedy
     (flux-findings (remove parked? releases) "HelmRelease")
     (for [release stuck
           :let [ready (ready-condition release)]]
       {:severity :critical
        :component (kubectl/namespace-name-of release)
        :summary (format "HelmRelease is parked after %s and will not retry"
                         (or (:reason ready) "a failure"))
        :hint (format "%s -- the message describes the last attempt (revision %s), which may no longer be true; verify the cause is still present, then clear the counters with `kubectl annotate helmrelease %s reconcile.fluxcd.io/resetAt=$(date -u +%%FT%%TZ) reconcile.fluxcd.io/requestedAt=$(date -u +%%FT%%TZ)`"
                      (or (:message ready) "no message")
                      (or (-> release :status :lastAttemptedRevision) "unknown")
                      (kubectl/name-of release))}))))

(defn detect-inert-deliveries
  [evidence]
  (for [resource (concat (kubectl/items-of (get evidence helmrelease-type))
                         (kubectl/items-of (get evidence kustomization-type)))
        :when (inert? resource)]
    {:severity :warning
     :component (kubectl/namespace-name-of resource)
     :summary "delivery is annotated to skip reconciliation"
     :hint "it will not change whatever it already applied; remove the reconcile annotation once the migration or gate it belongs to is finished"}))

(defn- condition
  [resource type]
  (first (filter #(= type (:type %)) (-> resource :status :conditions))))

(defn detect-drifted-releases
  "A release whose live objects no longer match what it applied. `Ready` stays
   True -- the Helm operation did succeed -- so nothing else reports this. With
   `driftDetection.mode: warn` the drift is named and never corrected, and a
   field whose desired value has not changed between releases is never in an
   upgrade's patch either, so it persists indefinitely."
  [evidence]
  (for [release (kubectl/items-of (get evidence helmrelease-type))
        :let [drifted (condition release "Drifted")
              mode (-> release :spec :driftDetection :mode)]
        :when (= "True" (:status drifted))]
    {:severity (if (= "enabled" mode) :warning :critical)
     :component (kubectl/namespace-name-of release)
     :summary (format "HelmRelease has drifted from what it applied (mode: %s)"
                      (or mode "warn"))
     :hint (format "%s%s"
                   (or (:message drifted) "no detail")
                   (if (= "enabled" mode)
                     " -- correction is on, so this should clear on the next reconcile"
                     " -- correction is off: nothing will fix this, and an upgrade patches only fields whose desired value changed. Diff it with `flux/drift`, then force a re-apply with reconcile.fluxcd.io/forceAt"))}))

(defn- crash-looping
  "Containers a kubelet keeps restarting. A pod can be Running and still be in
   this state, so a replica count does not show it."
  [pod]
  (for [status (concat (-> pod :status :containerStatuses)
                       (-> pod :status :initContainerStatuses))
        :let [reason (-> status :state :waiting :reason)]
        :when (contains? #{"CrashLoopBackOff" "CreateContainerConfigError" "RunContainerError"} reason)]
    {:container (:name status) :reason reason :restarts (or (:restartCount status) 0)}))

(defn detect-succeeded-over-broken
  "The contradiction worth surfacing: a release whose last Helm operation
   succeeded, over a workload that cannot run. `Ready` on a HelmRelease is a
   statement about Helm, not about the software - so a green delivery and a
   crash-looping pod coexist happily, and every dashboard reads fine."
  [evidence]
  (let [pods (kubectl/items-of (get evidence "pods"))
        broken (group-by kubectl/namespace-of
                         (filter #(seq (crash-looping %)) pods))]
    (for [release (kubectl/items-of (get evidence helmrelease-type))
          :let [ready (ready-condition release)
                namespace (kubectl/namespace-of release)
                casualties (get broken namespace)]
          :when (and (= "True" (:status ready)) (seq casualties))]
      {:severity :critical
       :component (kubectl/namespace-name-of release)
       :summary (format "HelmRelease reports success while %d pod(s) in its namespace cannot start"
                        (count casualties))
       :hint (format "%s -- Ready on a HelmRelease means the Helm operation succeeded, nothing more; look at %s"
                     (->> casualties
                          (mapcat crash-looping)
                          (map #(format "%s (%s, %d restarts)" (:container %) (:reason %) (:restarts %)))
                          distinct
                          (clojure.string/join ", "))
                     (clojure.string/join ", " (map kubectl/name-of casualties)))})))

(defn detect-kustomization-problems
  [evidence]
  (flux-findings (kubectl/items-of (get evidence kustomization-type)) "Kustomization"))

(defn detect-gitrepository-problems
  [evidence]
  (flux-findings (kubectl/items-of (get evidence gitrepository-type)) "GitRepository"))

(defn detect-ocirepository-problems
  [evidence]
  (flux-findings (kubectl/items-of (get evidence ocirepository-type)) "OCIRepository"))

(defn detect-helmchart-problems
  [evidence]
  (flux-findings (kubectl/items-of (get evidence helmchart-type)) "HelmChart"))

(def detectives
  [{:name "helmreleases"
    :description "HelmReleases that fail to reconcile or are suspended"
    :requires [helmrelease-type]
    :detect detect-helmrelease-problems}
   {:name "kustomizations"
    :description "Kustomizations that fail to reconcile or are suspended"
    :requires [kustomization-type]
    :detect detect-kustomization-problems}
   {:name "gitrepositories"
    :description "GitRepositories that fail to sync or are suspended"
    :requires [gitrepository-type]
    :detect detect-gitrepository-problems}
   {:name "ocirepositories"
    :description "OCIRepositories that fail to sync or are suspended"
    :requires [ocirepository-type]
    :detect detect-ocirepository-problems}
   {:name "helmcharts"
    :description "HelmCharts that fail to build or are suspended"
    :requires [helmchart-type]
    :detect detect-helmchart-problems}
   {:name "inert-deliveries"
    :description "HelmReleases and Kustomizations annotated to skip reconciliation"
    :requires [helmrelease-type kustomization-type]
    :detect detect-inert-deliveries}
   {:name "succeeded-over-broken"
    :description "HelmReleases reporting success over pods that cannot start"
    :requires [helmrelease-type "pods"]
    :detect detect-succeeded-over-broken}
   {:name "drifted-releases"
    :description "HelmReleases whose live objects no longer match what they applied"
    :requires [helmrelease-type]
    :detect detect-drifted-releases}])

(defn externalartifact-edges
  "The RFC-0012 back-pointer: spec.sourceRef names the object that produced this
   artifact - a renderer, a builder, anything that publishes one. The reference is
   {apiVersion, kind, name} with no namespace, because a producer publishes into
   its own namespace, so the edge stays in the artifact's. This is the hop that
   turns 'the manifests are stale' into 'this object failed to render them'."
  [artifact]
  (let [ref (-> artifact :spec :sourceRef)]
    (when (and (:kind ref) (:name ref))
      [{:relation "produced by"
        :subject (subject/subject (:kind ref) (kubectl/namespace-of artifact) (:name ref))}])))

(defn externalartifact-facts
  [artifact]
  (let [published (-> artifact :status :artifact)
        ref (-> artifact :spec :sourceRef)]
    [["revision" (:revision published)]
     ["digest" (:digest published)]
     ["url" (:url published)]
     ["last update" (:lastUpdateTime published)]
     ["produced by" (when (:name ref) (format "%s/%s" (:kind ref) (:name ref)))]]))

;; The flux CLI subcommand for a kind, so a drill-down can ask flux for its status.
(def ^:private flux-get-kind
  {"HelmRelease" ["helmrelease"]
   "Kustomization" ["kustomization"]
   "GitRepository" ["source" "git"]
   "OCIRepository" ["source" "oci"]})

(plugin/provide!
 {;; The gitops scan is flux's - it fills the :gitops scope.
  :detectives {:gitops detectives}

  ;; A cluster runs flux when it serves the flux CRDs.
  :capabilities {:flux #(kubectl/serves-crd? kustomization-type)}

  ;; The flux CLI, so any book that lists it inherits the version check.
  :tools {"flux" {:version-command ["version" "--client"] :min-version "2.0"}}

  ;; The flux CRDs become drill-down subjects (describe/yaml probes work on them);
  ;; edges default to ownerReferences.
  :kinds {"HelmRelease"      {:type helmrelease-type}
          "Kustomization"    {:type kustomization-type}
          "GitRepository"    {:type gitrepository-type}
          "OCIRepository"    {:type ocirepository-type}
          "HelmChart"        {:type helmchart-type}
          "ExternalArtifact" {:type externalartifact-type :edges externalartifact-edges}}

  ;; An ExternalArtifact has no phase or replicas, so the generic panel would
  ;; show only its creation timestamp - what matters is the revision it serves
  ;; and who produced it.
  :facts {"ExternalArtifact" externalartifact-facts}

  ;; A flux-native probe: reconcile status via the flux CLI, offered only where
  ;; flux is installed. The flux2 `get` syntax is stable across its majors, so
  ;; one form serves all; were a future major to change it, wrapping :args in
  ;; command/dispatch-by-version keeps this one package agnostic.
  :probes [{:key :flux-status :label "🔁 flux reconcile status"
            :kinds (set (keys flux-get-kind)) :tools ["flux"]
            :args (fn [context {:keys [kind namespace name]}]
                    (into ["flux" (str "--context=" context) (str "--namespace=" namespace) "get"]
                          (conj (flux-get-kind kind) name)))}]})
