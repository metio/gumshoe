;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.velero.action-books
  "The mutation-book specs of the Velero actions. Each book file merges one of
   these with its own :description; pause and resume are the two sides of one
   gumshoe.toggle declaration. The decisions live in gumshoe.velero.actions."
  (:require [gumshoe.detectives.velero :as velero]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.select :as select]
            [gumshoe.toggle :as toggle]
            [gumshoe.velero.actions :as actions]))

(def ^:private request-type "deletebackuprequests.velero.io")

(defn- picks
  "The objects of a type nothing refuses, as \"<namespace>/<name>\". refusal takes
   [context object]."
  [context type refusal]
  (vec (for [object (sort-by kubectl/namespace-name-of (kubectl/items-of (kubectl/get-all context type)))
             :when (nil? (refusal context object))]
         (kubectl/namespace-name-of object))))

(defn- locations
  [context namespace]
  (kubectl/get-namespaced context namespace velero/location-type))

(def ^:private schedule-select
  "What every Schedule book's select shares; each adds the candidates it offers."
  {:mode :one :label "Schedule" :flag :schedule})

(def ^:private schedule-option
  (select/options schedule-select {:schedule {:desc "\"<namespace>/<schedule>\" - interactive selection when omitted"}}))

(def ^:private backup-select {:mode :one :label "Backup" :flag :backup})

(def schedule-pausing
  "The pause and resume books of a Schedule, as {:on :off} (see gumshoe.toggle)."
  (toggle/pair
   {:spec {:options schedule-option
           :prerequisites {:installed-tools ["kubectl" "fzf"]
                           :cluster-capabilities []
                           :kubectl-can-get [velero/schedule-type]
                           :kubectl-can-patch [velero/schedule-type]}
           :select schedule-select
           :subject {:type velero/schedule-type
                     :parse actions/parse-pick
                     :invalid "the pick is not \"<namespace>/<schedule>\""}}
    :refusal actions/pause-refusal
    :items (fn [{:keys [object]} wanted] (actions/pause-items object wanted))
    :patch actions/pause-patch
    :applied? (fn [schedule wanted] (= wanted (actions/paused? schedule)))
    :verify {:timeout 30 :interval 3}
    :on {:verb "pause" :empty-message "every Schedule is paused already"
         :confirm {:action "pause a backup schedule" :destructive? false}
         :announce "Pause Schedule %s" :verified "Schedule %s is paused"}
    :off {:verb "resume" :empty-message "no Schedule is paused"
          :confirm {:action "resume a backup schedule" :destructive? false}
          :announce "Resume Schedule %s" :verified "Schedule %s is resumed"}}))

(defn schedule-pause
  "The book that pauses a Schedule when wanted, else the one that resumes it."
  [wanted]
  ((if wanted :on :off) schedule-pausing))

(def backup
  {:options schedule-option
   :prerequisites {:installed-tools ["kubectl" "fzf"]
                   :cluster-capabilities []
                   :kubectl-can-get [velero/schedule-type velero/location-type velero/backup-type]
                   :kubectl-can-create [velero/backup-type]}
   :select (assoc schedule-select
                  :candidates (fn [context]
                                (let [in (memoize #(locations context %))]
                                  (picks context velero/schedule-type
                                         (fn [_ s] (actions/backup-refusal s (in (kubectl/namespace-of s))))))))
   :empty-message "no Schedule can take a backup now - every one failed validation or writes to an unavailable location"
   :confirm {:action "start a backup from a schedule's template" :destructive? false}
   :subject {:type velero/schedule-type :parse actions/parse-pick
             :invalid "the pick is not \"<namespace>/<schedule>\""}
   :derive (fn [{:keys [object]}]
             {:backup-name (when object (actions/backup-name object (java.time.Instant/now)))})
   :items (fn [{:keys [object backup-name target]}]
            (if object (actions/backup-items object backup-name) [(str "back up from " target)]))
   :refusal (fn [{:keys [context ref object]}]
              (actions/backup-refusal object (locations context (:namespace ref))))
   :announce (fn [{:keys [target backup-name]}] (format "Back up from Schedule %s as %s" target backup-name))
   :effect (fn [{:keys [context object backup-name]}]
             (if object
               (effect/plan (effect/kubectl-stdin context (actions/to-json (actions/backup-manifest object backup-name))
                                                  "create" "-f" "-"))
               (effect/plan (effect/note "nothing to back up - the precondition says why"))))
   :verify (fn [{:keys [context ref backup-name]}]
             (let [current #(kubectl/get-namespaced-resource context (:namespace ref) velero/backup-type backup-name)]
               [{:description (format "Backup %s/%s completed" (:namespace ref) backup-name)
                 :timeout 3600 :interval 15
                 :abort #(first (actions/backup-signals (current)))
                 :check #(actions/backup-completed? (current))}]))})

(def delete-backup
  {:options (select/options backup-select {:backup {:desc "\"<namespace>/<backup>\" - interactive selection when omitted"}})
   :prerequisites {:installed-tools ["kubectl" "fzf"]
                   :cluster-capabilities []
                   :kubectl-can-get [velero/backup-type velero/location-type velero/restore-type request-type]
                   :kubectl-can-create [request-type]}
   :select (assoc backup-select
                  :candidates (fn [context]
                                (let [in (memoize #(locations context %))]
                                  (picks context velero/backup-type
                                         (fn [_ b] (actions/delete-refusal b (in (kubectl/namespace-of b))))))))
   :empty-message "no Backup can be deleted - each is running, being deleted, or stored where Velero cannot delete"
   :confirm {:action "delete a backup and its data" :destructive? true}
   :subject {:type velero/backup-type :parse actions/parse-pick
             :invalid "the pick is not \"<namespace>/<backup>\""}
   :derive (fn [{:keys [context ref object]}]
             {:uid (-> object :metadata :uid)
              :restores (when object (kubectl/get-namespaced context (:namespace ref) velero/restore-type))})
   :items (fn [{:keys [object restores target]}]
            (if object (actions/delete-items object restores) [(str "delete " target)]))
   :refusal (fn [{:keys [context ref object]}]
              (actions/delete-refusal object (locations context (:namespace ref))))
   :announce (fn [{:keys [target]}] (format "Delete Backup %s" target))
   :effect (fn [{:keys [context object]}]
             (if object
               (effect/plan (effect/kubectl-stdin context (actions/to-json (actions/delete-request-manifest object))
                                                  "create" "-f" "-"))
               (effect/plan (effect/note "nothing to delete - the precondition says why"))))
   :verify (fn [{:keys [context ref target uid]}]
             [{:description (format "Backup %s and its data are deleted" target)
               :timeout 1800 :interval 10 :verifies-absence? true
               :abort #(first (actions/delete-signals (kubectl/get-namespaced context (:namespace ref) request-type) uid))
               ;; gone needs a read that answered NotFound, whatever reads it
               :check #(boolean (and ref (kubectl/confirmed-gone? context (:namespace ref) (:type ref) (:name ref))))}])})
