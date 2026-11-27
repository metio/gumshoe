;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.volumes.delete-released
  "Deletes PersistentVolumes stuck in Released: their claims are gone but the
   backing storage (ceph images, local LVs) still exists and is cleaned up
   according to each volume's reclaim policy - which is shown right in the
   confirmation, since a 'Delete' policy also destroys the backing storage."
  (:require [clojure.string :as str]
            [gumshoe.cleanup :as cleanup]
            [gumshoe.deletes :as deletes]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]))

(defn released-volumes
  [context]
  (kubectl/names-of (kubectl/filter-list (kubectl/get-all context "persistentvolumes")
                                         cleanup/released?)))

(defn volume-refs
  "The ref of each picked volume, which the engine reads again and pins."
  [{:keys [target]}]
  (for [volume target] {:type "persistentvolumes" :name volume}))

(defn volume-items
  "What the confirmation lists per volume: its name and reclaim policy, as
   the volumes read before the confirmation name it."
  [volumes objects]
  (map #(format "%s (%s reclaim policy)" %1 (cleanup/reclaim-policy %2)) volumes objects))

(defn delete-effect
  [context volumes]
  (apply effect/plan
         (map #(deletes/effect context {:type "persistentvolume" :name %} {:wait false}) volumes)))

(defn gone-checks
  [context volumes]
  (for [volume volumes]
    {:description (format "PersistentVolume %s is gone" volume)
     :timeout 120 :interval 10 :verifies-absence? true
     :check (fn [] (kubectl/confirmed-gone? context nil "persistentvolumes" volume))}))

(def selection
  {:mode :many :label "PersistentVolume" :flag :volumes :candidates released-volumes})

(mutation/book
 {:description "Deletes PersistentVolumes stuck in Released"
  :options (select/options selection {:volumes {:desc "Released PersistentVolumes to delete, repeatable - interactive selection when omitted"}})
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["persistentvolumes"]
                  :kubectl-can-delete ["persistentvolumes"]}
  :select selection
  :empty-message "no PersistentVolume is Released"
  :subjects volume-refs
  :items (fn [{:keys [target objects]}] (volume-items target objects))
  ;; a claim may have bound a volume again while the operator read the prompt
  :refusal (fn [{:keys [refs objects]}] (cleanup/first-refusal cleanup/volume-refusal refs objects))
  :confirm {:action "delete Released PersistentVolumes - a 'Delete' reclaim policy also removes the backing storage"
            :destructive? true}
  :announce (fn [{:keys [target]}] (format "Delete released PersistentVolumes [%s]" (str/join ", " target)))
  :effect (fn [{:keys [context target]}] (delete-effect context target))
  :verify (fn [{:keys [context target]}] (gone-checks context target))})
