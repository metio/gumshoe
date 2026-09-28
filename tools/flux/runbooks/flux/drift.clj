;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.flux.drift
  "Diffs what a HelmRelease applied against what is live, field by field.

   The Drifted condition names which objects differ; it does not say what
   differs in them, and that is the part that decides the fix. A field only one
   side writes is drift to re-apply. A field two writers each set is a
   double-write to remove from one of them. A field the CRD defaults, which the
   chart has always set to the same value, is neither: Helm patches only what
   changed between manifests, so an upgrade never corrects it."
  (:require [clojure.string :as str]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.tools.flux]))

(def helmrelease-type "helmreleases.helm.toolkit.fluxcd.io")

(def options
  {:namespace {:desc "The namespace of the HelmRelease - interactive selection when omitted"
               :alias :n :coerce :string}
   :name {:desc "The name of the HelmRelease - interactive selection when omitted"
          :alias :r :coerce :string}})

(def prerequisites
  {:installed-tools ["kubectl" "helm" "fzf"]
   :cluster-capabilities [:flux]
   :kubectl-can-get [helmrelease-type]})

(defn drifted?
  [release]
  (boolean (some #(and (= "Drifted" (:type %)) (= "True" (:status %)))
                 (-> release :status :conditions))))

(defn drifted-first
  "Releases reporting Drifted first, so the interesting ones head the picker."
  [context]
  (let [releases (kubectl/items-of (kubectl/get-all context helmrelease-type))]
    (mapv kubectl/namespace-name-of
          (concat (filter drifted? releases) (remove drifted? releases)))))

(defn release-name
  "The Helm release name: spec.releaseName when set, the object's own name
   otherwise. `helm get manifest` takes the former."
  [release fallback]
  (or (-> release :spec :releaseName) fallback))

(defn drift-message
  [release]
  (->> (-> release :status :conditions)
       (filter #(= "Drifted" (:type %)))
       (keep :message)
       first))

(def ^:private reading
  ["a field only one side writes is drift: force a re-apply with"
   "reconcile.fluxcd.io/forceAt, or turn on driftDetection.mode: enabled."
   "a field two writers both set is a double-write: remove it from one."
   "Helm patches only what changed between manifests, so a field whose desired"
   "value never changed is never corrected by an upgrade - and an object deleted"
   "from the cluster is not recreated by one either."])

(defn report-diff
  "kubectl diff exits 1 when differences exist, which here is the answer rather
   than a failure; only above 1 is an error."
  [context namespace manifest]
  (let [{:keys [out err exit]} (shell/execute-with-stdin
                                manifest "kubectl" (str "--context=" context)
                                "-n" namespace "diff" "-f" "-")]
    (when-not (str/blank? out) (println out))
    (cond
      (> exit 1)
      (do (stdout/error (str "kubectl diff failed: " err)) false)

      (zero? exit)
      (do (stdout/ok "no differences: live matches what the release applied") true)

      :else
      (do (stdout/print-section-marker)
          (doseq [line reading] (stdout/ok line))
          true))))

(defn- show-drift
  [opts _ctx]
  (let [context (kubectl/current-context)
        target (interact/choose-namespaced "HelmRelease" (drifted-first context)
                                           (:namespace opts) (:name opts))]
    (if (nil? target)
      (do (stdout/error "no HelmRelease selected") false)
      (let [{:keys [namespace name]} (kubectl/split-namespace-name target)
            release (kubectl/get-namespaced-resource context namespace helmrelease-type name)
            helm-name (release-name release name)
            manifest (shell/stdout-of "helm" (str "--kube-context=" context)
                                      "-n" namespace "get" "manifest" helm-name)]
        (stdout/print-section-marker)
        (if-let [message (drift-message release)]
          (stdout/ok (format "%s reports drift:%n%s" target message))
          (stdout/ok (format "%s reports no drift - diffing anyway" target)))
        (stdout/print-section-marker)
        (if (str/blank? manifest)
          (do (stdout/error (format "helm has no manifest for release %s in %s"
                                    helm-name namespace))
              false)
          (report-diff context namespace manifest))))))

(runbook/execute!
 {:description "Diffs what a HelmRelease applied against what is live, field by field"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action show-drift})
