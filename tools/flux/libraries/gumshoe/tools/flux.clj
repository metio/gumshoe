;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux
  "The flux tool package: everything gumshoe knows about Flux, in one place and
   registered through a single plugin/provide!. Requiring this namespace (which a
   casebook does via env.edn :plugins, or a flux book does at its top) contributes
   flux's detectives, its :flux cluster capability, the flux CLI tool profile, and
   the drill-down (its CRD kinds plus a reconcile-status probe). Nothing about
   flux lives in the engine any more."
  (:require [gumshoe.investigation :as investigation]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(def helmrelease-type "helmreleases.helm.toolkit.fluxcd.io")
(def kustomization-type "kustomizations.kustomize.toolkit.fluxcd.io")
(def gitrepository-type "gitrepositories.source.toolkit.fluxcd.io")
(def ocirepository-type "ocirepositories.source.toolkit.fluxcd.io")
(def helmchart-type "helmcharts.source.toolkit.fluxcd.io")

(defn- ready-condition
  [resource]
  (first (filter #(= "Ready" (:type %)) (-> resource :status :conditions))))

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
  (flux-findings (kubectl/items-of (get evidence helmrelease-type)) "HelmRelease"))

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
    :detect detect-helmchart-problems}])

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
  :capabilities {:flux #(kubectl/resource-exists? "customresourcedefinition" kustomization-type)}

  ;; The flux CLI, so any book that lists it inherits the version check.
  :tools {"flux" {:version-command ["version" "--client"] :min-version "2.0"}}

  ;; The flux CRDs become drill-down subjects (describe/yaml probes work on them);
  ;; edges default to ownerReferences.
  :kinds {"HelmRelease"   {:type helmrelease-type}
          "Kustomization" {:type kustomization-type}
          "GitRepository" {:type gitrepository-type}
          "OCIRepository" {:type ocirepository-type}
          "HelmChart"     {:type helmchart-type}}

  ;; A flux-native probe: reconcile status via the flux CLI, offered only where
  ;; flux is installed.
  :probes [{:key :flux-status :label "🔁 flux reconcile status"
            :kinds (set (keys flux-get-kind)) :tools ["flux"]
            :args (fn [context {:keys [kind namespace name]}]
                    (into ["flux" (str "--context=" context) (str "--namespace=" namespace) "get"]
                          (conj (flux-get-kind kind) name)))}]})
