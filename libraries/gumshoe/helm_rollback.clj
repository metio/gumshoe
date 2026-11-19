;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.helm-rollback
  "The decisions behind the Helm rollback book, pure over the release history
   kubectl/helm-releases reads from the release Secrets' labels. Helm's own
   default goes back one revision whatever its status; this goes back to the
   newest earlier revision that was deployed, and passes that revision to
   helm, so the confirmation names exactly what runs."
  (:require [clojure.string :as str]))

(def ^:private deployed-statuses #{"deployed" "superseded"})

(defn parse-pick
  "\"<namespace>/<release>\" as {:namespace :release}, or nil."
  [pick]
  (when-let [[_ namespace release] (re-matches #"\s*([^/\s]+)/([^/\s]+)\s*" (str pick))]
    {:namespace namespace :release release}))

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

(defn rollback-items
  [{:keys [namespace release]} revisions to]
  (let [current (first revisions)
        target (rollback-target revisions to)]
    [(format "roll Helm release %s/%s back from revision %d (%s) to revision %d (%s)"
             namespace release (:version current) (:status current) (:version target) (:status target))
     (format "helm records the rollback as revision %d; its status says the manifests were applied, not that the pods are ready"
             (inc (:version current)))
     "the release's source (Git, a pipeline) still holds the newer chart or values and will upgrade it again"]))

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
