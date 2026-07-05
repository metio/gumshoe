;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.flux
  "Detectives for flux: sources and reconciliations that are failing or
   quietly suspended."
  (:require [gumshoe.kubectl :as kubectl]))

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
