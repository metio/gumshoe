;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.argocd-actions
  "The decisions behind the Argo CD action books, pure over the Application.
   A refresh is the argocd.argoproj.io/refresh annotation, which the
   application controller removes once it has compared the Application again;
   a sync is an operation written into the Application, which the controller
   runs and reports in status.operationState."
  (:require [cheshire.core :as json]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.pick :as pick]))

(def refresh-annotation "argocd.argoproj.io/refresh")
(def refresh-types #{"normal" "hard"})

(def parse-pick
  "\"<namespace>/<name>\" as {:namespace :name}, or nil."
  pick/namespaced)

(defn source-of
  "repoURL@targetRevision of the Application's first source."
  [application]
  (let [spec (:spec application)
        source (or (:source spec) (first (:sources spec)))]
    (str (or (:repoURL source) "its source") "@" (or (not-empty (:targetRevision source)) "HEAD"))))

(defn refresh-pending?
  [application]
  (some? (get-in application [:metadata :annotations (keyword refresh-annotation)])))

(defn- operation-running?
  [application]
  (or (some? (:operation application))
      (contains? #{"Running" "Terminating"} (-> application :status :operationState :phase))))

(defn refresh-refusal
  [application type]
  (cond
    (nil? application) "the Application no longer exists"
    (not (refresh-types type)) (format "the refresh type is %s, not normal or hard" type)
    (refresh-pending? application) "a refresh is requested already and the controller has not taken it yet"
    :else nil))

(defn refresh-items
  [application type]
  [(format "refresh Application %s - the controller compares it with %s again%s"
           (kubectl/namespace-name-of application) (source-of application)
           (if (= "hard" type) ", regenerating its manifests instead of reading the manifest cache" ""))
   "a refresh changes nothing in the cluster; an automated sync may follow when it finds a difference"])

(defn refresh-arg
  [type]
  (str refresh-annotation "=" type))

(defn refreshed?
  "The controller removed the annotation, so it compared the Application."
  [application]
  (boolean (and application (not (refresh-pending? application)))))

;; --- sync ----------------------------------------------------------------------------

(defn sync-refusal
  [application]
  (cond
    (nil? application) "the Application no longer exists"
    (some? (-> application :metadata :deletionTimestamp)) "the Application is being deleted"
    (operation-running? application)
    (format "an operation is running already (%s) - wait for it, or end it with argocd app terminate-op"
            (or (-> application :status :operationState :phase) "requested"))
    :else nil))

(defn sync-items
  [application prune?]
  [(format "sync Application %s to %s (currently %s, %s)"
           (kubectl/namespace-name-of application) (source-of application)
           (or (-> application :status :sync :status) "unknown")
           (or (-> application :status :health :status) "unknown health"))
   (if prune?
     "prune: resources removed from Git are deleted from the cluster"
     "no prune: resources removed from Git stay in the cluster")])

(def request-info
  "The name of the operation info entry a sync writes its request token
   under. The application controller copies the whole operation, info
   included, into status.operationState.operation when it starts it."
  "gumshoe-request")

(defn request-token
  "A token no other operation carries, which identifies the operation a run
   requested."
  []
  (str (random-uuid)))

(defn sync-patch
  "The merge patch that requests a sync carrying token as its request info."
  [initiator prune? token]
  (json/generate-string {:operation {:initiatedBy {:username initiator}
                                     :info [{:name request-info :value token}]
                                     :sync (cond-> {:syncStrategy {:hook {}}} prune? (assoc :prune true))}}))

(defn requested-operation?
  "Whether the operation state reports the operation that carries token.
   Matching the token the run wrote, not when the operation started, keeps
   the judgement independent of the clocks of the workstation and the
   controller."
  [operation-state token]
  (boolean (and (some? token)
                (some #(and (= request-info (:name %)) (= token (:value %)))
                      (-> operation-state :operation :info)))))

(defn synced?
  "The operation that carries token finished Succeeded."
  [application token]
  (let [state (-> application :status :operationState)]
    (boolean (and (nil? (:operation application))
                  (= "Succeeded" (:phase state))
                  (requested-operation? state token)))))

(defn sync-signals
  "Why the wait can end: the failure of the operation that carries token."
  [application token]
  (let [state (-> application :status :operationState)]
    (when (and (contains? #{"Failed" "Error"} (:phase state)) (requested-operation? state token))
      [(format "the sync %s: %s" (if (= "Error" (:phase state)) "errored" "failed") (or (:message state) "no message"))])))
