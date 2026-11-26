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
            [gumshoe.conditions :as conditions]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.tools.flux :as flux]))

(def helmrelease-select
  "The HelmRelease to compare. Its candidates are defined below, so the
   action adds them."
  {:mode :namespaced :label "HelmRelease" :namespace-flag :namespace :name-flag :name
   :empty-message "no HelmRelease exists"
   :on-empty :fail})

(def options
  (select/options helmrelease-select))

(def prerequisites
  {:installed-tools ["kubectl" "helm" "fzf"]
   :cluster-capabilities [:flux]
   :kubectl-can-get [flux/helmrelease-type]})

(defn drifted?
  [release]
  (conditions/holds? release "Drifted"))

(defn drifted-first
  "Releases reporting Drifted first, so the interesting ones head the picker."
  [context]
  (let [releases (kubectl/items-of (kubectl/get-all context flux/helmrelease-type))]
    (mapv kubectl/namespace-name-of
          (concat (filter drifted? releases) (remove drifted? releases)))))

(defn drift-message
  [release]
  (:message (conditions/condition release "Drifted")))

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

(defn read-release
  "{:release <the HelmRelease>} or {:unread <why>}: the HelmRelease read
   strictly, so a read that did not answer is never taken for a release
   without a Drifted condition, and one that no longer exists is said so."
  [context namespace name target]
  (try
    (if-let [release (binding [kubectl/*strict-reads* true]
                       (kubectl/get-namespaced-resource context namespace flux/helmrelease-type name))]
      {:release release}
      {:unread (format "HelmRelease %s no longer exists" target)})
    (catch clojure.lang.ExceptionInfo e
      (if (::kubectl/unread (ex-data e))
        {:unread (format "could not read HelmRelease %s - %s" target (ex-message e))}
        (throw e)))))

(defn read-manifest
  "{:manifest <text>} or {:unread <why>}: what Helm stored for the release.
   A helm that failed reports its own error; only one that answered with
   nothing is an absent manifest."
  [context storage helm-name]
  (let [{:keys [exit out err]} (shell/execute "helm" (str "--kube-context=" context)
                                              "-n" storage "get" "manifest" helm-name)]
    (cond
      (not (zero? exit))
      {:unread (format "helm get manifest %s in %s failed: %s" helm-name storage
                       (or (not-empty (str/trim (str err))) (str "exit " exit)))}

      (str/blank? out)
      {:unread (format "helm has no manifest for release %s in %s" helm-name storage)}

      :else {:manifest out})))

(defn compare-release
  "Reads the HelmRelease `target` and diffs what Helm stored for it against
   what is live. Stops with the reason when the release or its manifest
   could not be read, before printing anything about its drift."
  [context target]
  (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
        {:keys [release unread]} (read-release context namespace name target)]
    (if unread
      (do (stdout/error unread) false)
      ;; targetNamespace renames the release and moves its objects,
      ;; while Helm's records stay in the storage namespace
      (let [helm-name (flux/release-name release)
            storage (flux/storage-namespace release)
            objects (flux/release-namespace release)]
        (stdout/print-section-marker)
        (if-let [message (drift-message release)]
          (stdout/ok (format "%s reports drift:%n%s" target message))
          (stdout/ok (format "%s reports no drift - diffing anyway" target)))
        (stdout/print-section-marker)
        (let [{:keys [manifest unread]} (read-manifest context storage helm-name)]
          (if unread
            (do (stdout/error unread) false)
            (report-diff context objects manifest)))))))

(defn- show-drift
  [opts _ctx]
  (let [context (kubectl/current-context)
        target (select/pick (assoc helmrelease-select :candidates drifted-first) opts context)]
    (if (nil? target)
      false
      (compare-release context target))))

(runbook/execute!
 {:description "Diffs what a HelmRelease applied against what is live, field by field"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action show-drift})
