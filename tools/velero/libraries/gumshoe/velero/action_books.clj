;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.velero.action-books
  "The mutation-book specs of the Velero actions. Each book file merges one of
   these with its own :description; pause and resume are the two sides of one
   gumshoe.toggle declaration. The decisions live in gumshoe.velero.actions."
  (:require [gumshoe.detectives.velero :as velero]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.toggle :as toggle]
            [gumshoe.velero.actions :as actions]))

(def ^:private request-type "deletebackuprequests.velero.io")

(defn- fetch
  [context type {:keys [namespace name]}]
  (kubectl/get-namespaced-resource context namespace type name))

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

(defn- refuse
  [why]
  (when why (println (str "refusing: " why)))
  (nil? why))

(def ^:private schedule-option
  {:schedule {:desc "\"<namespace>/<schedule>\" - interactive selection when omitted" :alias :s :coerce :string}})

(def schedule-pausing
  "The pause and resume books of a Schedule, as {:on :off} (see gumshoe.toggle)."
  (toggle/pair
   {:spec {:options schedule-option
           :prerequisites {:installed-tools ["kubectl" "fzf"]
                           :cluster-capabilities []
                           :kubectl-can-get [velero/schedule-type]
                           :kubectl-can-patch [velero/schedule-type]}
           :select {:mode :one :label "Schedule" :flag :schedule}
           :subject {:type velero/schedule-type
                     :parse actions/parse-pick
                     :invalid "the pick is not \"<namespace>/<schedule>\""}}
    :refusal actions/pause-refusal
    :items (fn [{:keys [object]} wanted] (actions/pause-items object wanted))
    :patch actions/pause-patch
    :applied? (fn [schedule wanted] (= wanted (actions/paused? schedule)))
    :verify {:timeout 30 :interval 3}
    :on {:verb "pause" :empty-message "every Schedule is paused already"
         :confirm {:action "pause a backup schedule"}
         :announce "Pause Schedule %s" :verified "Schedule %s is paused"}
    :off {:verb "resume" :empty-message "no Schedule is paused"
          :confirm {:action "resume a backup schedule"}
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
   :select {:mode :one :label "Schedule" :flag :schedule
            :candidates (fn [context]
                          (let [in (memoize #(locations context %))]
                            (picks context velero/schedule-type
                                   (fn [_ s] (actions/backup-refusal s (in (kubectl/namespace-of s)))))))}
   :empty-message "no Schedule can take a backup now - every one failed validation or writes to an unavailable location"
   :confirm {:action "start a backup from a schedule's template"}
   :derive (fn [{:keys [context target]}]
             (let [pick (actions/parse-pick target)
                   schedule (when pick (fetch context velero/schedule-type pick))]
               {:pick pick :schedule schedule
                :backup-name (when schedule (actions/backup-name schedule (java.time.Instant/now)))}))
   :items (fn [{:keys [schedule backup-name target]}]
            (if schedule (actions/backup-items schedule backup-name) [(str "back up from " target)]))
   :pin (fn [{:keys [pick]}] (when pick {:type velero/schedule-type :namespace (:namespace pick) :name (:name pick)}))
   :precondition (fn [{:keys [context pick]}]
                   (refuse (if pick
                             (actions/backup-refusal (fetch context velero/schedule-type pick) (locations context (:namespace pick)))
                             "the pick is not \"<namespace>/<schedule>\"")))
   :announce (fn [{:keys [target backup-name]}] (format "Back up from Schedule %s as %s" target backup-name))
   :effect (fn [{:keys [context schedule backup-name]}]
             (if schedule
               (effect/plan (effect/kubectl-stdin context (actions/to-json (actions/backup-manifest schedule backup-name))
                                                  "create" "-f" "-"))
               (effect/plan (effect/note "nothing to back up - the precondition says why"))))
   :verify (fn [{:keys [context pick backup-name]}]
             (let [current #(fetch context velero/backup-type {:namespace (:namespace pick) :name backup-name})]
               [{:description (format "Backup %s/%s completed" (:namespace pick) backup-name)
                 :timeout 3600 :interval 15
                 :abort #(first (actions/backup-signals (current)))
                 :check #(actions/backup-completed? (current))}]))})

(def delete-backup
  {:options {:backup {:desc "\"<namespace>/<backup>\" - interactive selection when omitted" :alias :b :coerce :string}}
   :prerequisites {:installed-tools ["kubectl" "fzf"]
                   :cluster-capabilities []
                   :kubectl-can-get [velero/backup-type velero/location-type velero/restore-type request-type]
                   :kubectl-can-create [request-type]}
   :select {:mode :one :label "Backup" :flag :backup
            :candidates (fn [context]
                          (let [in (memoize #(locations context %))]
                            (picks context velero/backup-type
                                   (fn [_ b] (actions/delete-refusal b (in (kubectl/namespace-of b)))))))}
   :empty-message "no Backup can be deleted - each is running, being deleted, or stored where Velero cannot delete"
   :confirm {:action "delete a backup and its data" :destructive? true}
   :derive (fn [{:keys [context target]}]
             (let [pick (actions/parse-pick target)
                   backup (when pick (fetch context velero/backup-type pick))]
               {:pick pick :backup backup
                :uid (-> backup :metadata :uid)
                :restores (when backup (kubectl/get-namespaced context (:namespace pick) velero/restore-type))}))
   :items (fn [{:keys [backup restores target]}]
            (if backup (actions/delete-items backup restores) [(str "delete " target)]))
   :pin (fn [{:keys [pick]}] (when pick {:type velero/backup-type :namespace (:namespace pick) :name (:name pick)}))
   :precondition (fn [{:keys [context pick]}]
                   (refuse (if pick
                             (actions/delete-refusal (fetch context velero/backup-type pick) (locations context (:namespace pick)))
                             "the pick is not \"<namespace>/<backup>\"")))
   :announce (fn [{:keys [target]}] (format "Delete Backup %s" target))
   :effect (fn [{:keys [context backup]}]
             (if backup
               (effect/plan (effect/kubectl-stdin context (actions/to-json (actions/delete-request-manifest backup))
                                                  "create" "-f" "-"))
               (effect/plan (effect/note "nothing to delete - the precondition says why"))))
   :verify (fn [{:keys [context pick target uid]}]
             [{:description (format "Backup %s and its data are deleted" target)
               :timeout 1800 :interval 10
               :abort #(first (actions/delete-signals (kubectl/get-namespaced context (:namespace pick) request-type) uid))
               :check #(nil? (fetch context velero/backup-type pick))}])})
