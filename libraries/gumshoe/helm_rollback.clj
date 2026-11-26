;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.helm-rollback
  "The decisions behind the Helm rollback book, pure over the release history
   kubectl/helm-releases reads from the release Secrets' labels. Helm's own
   default goes back one revision whatever its status; this goes back to the
   newest earlier revision that was deployed, and passes that revision to
   helm, so the confirmation names exactly what runs."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [gumshoe.pick :as pick]))

(def ^:private deployed-statuses #{"deployed" "superseded"})

(defn parse-pick
  "\"<namespace>/<release>\" as {:namespace :release}, or nil."
  [pick]
  (some-> (pick/namespaced pick) (set/rename-keys {:name :release})))

(defn history
  "One release's revisions, newest first."
  [releases {:keys [namespace release]}]
  (->> releases
       (filter #(and (= namespace (:namespace %)) (= release (:release %)) (:version %)))
       (sort-by (comp - :version))
       vec))

(defn rollback-target
  [revisions to]
  (let [[current & earlier] revisions]
    (if to
      (first (filter #(= to (:version %)) revisions))
      (first (filter #(and (< (:version %) (:version current)) (deployed-statuses (:status %))) earlier)))))

(defn rollback-refusal
  [revisions to]
  (let [current (first revisions)]
    (cond
      (nil? current) "the release no longer exists"
      (str/starts-with? (str (:status current)) "pending-")
      (format "revision %d is %s - another helm operation is in progress, or was interrupted and needs fixing first"
              (:version current) (:status current))
      (and to (= to (:version current))) (format "revision %d is the current one" to)
      (nil? (rollback-target revisions to))
      (if to
        (format "revision %d is not in the release's history - helm keeps only --history-max revisions" to)
        "no earlier revision of the release was deployed")
      :else nil)))

(def storage-rights
  "What helm rollback writes in the release's namespace beside the release's
   own objects: Helm's Secret storage driver creates the Secret of the new
   revision and updates the current one to superseded. The book's
   prerequisites check rights across every namespace, and a release's
   namespace is known only once it is picked, so these are asked in that
   namespace by the refusal."
  [["create" "secrets"] ["update" "secrets"]])

(defn rights-refusal
  "Why the rollback must not start when the user lacks `missing`, the
   [verb resource] pairs of storage-rights not granted in namespace, or nil.
   Without them helm fails after the confirmation, possibly having applied
   part of the release's manifests and recorded a failed revision."
  [namespace missing]
  (when (seq missing)
    (format "you may not %s in namespace %s, which helm needs to record the rollback"
            (str/join " or " (map (fn [[verb resource]] (str verb " " resource)) missing))
            namespace)))

(defn rollback-items
  [{:keys [namespace release]} revisions to]
  (let [current (first revisions)
        target (rollback-target revisions to)]
    [(format "roll Helm release %s/%s back from revision %d (%s) to revision %d (%s)"
             namespace release (:version current) (:status current) (:version target) (:status target))
     (format "helm records the rollback as revision %d; its status says the manifests were applied, not that the pods are ready"
             (inc (:version current)))
     "the release's source (Git, a pipeline) still holds the newer chart or values and will upgrade it again"
     "helm writes every object of the release as the operator running it: a right missing for one of them fails the rollback part-way"]))

(defn rollback-args
  [context {:keys [namespace release]} revision]
  ["helm" "rollback" release (str revision) (str "--namespace=" namespace) (str "--kube-context=" context)])

(defn rolled-back?
  "A revision newer than the one confirmed is deployed."
  [revisions before]
  (let [newest (first revisions)]
    (boolean (and newest (> (:version newest) before) (= "deployed" (:status newest))))))

(defn picks
  "Every release that has a revision to go back to."
  [releases]
  (->> releases
       (map #(select-keys % [:namespace :release]))
       distinct
       (filter #(nil? (rollback-refusal (history releases %) nil)))
       (map #(str (:namespace %) "/" (:release %)))
       sort
       vec))
