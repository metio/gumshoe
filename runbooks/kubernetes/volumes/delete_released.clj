;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.volumes.delete-released
  "Deletes PersistentVolumes stuck in Released: their claims are gone but the
   backing storage (ceph images, local LVs) still exists and is cleaned up
   according to each volume's reclaim policy - which is shown right in the
   confirmation, since a 'Delete' policy also destroys the backing storage."
  (:require [clojure.string :as str]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn released-volumes
  [context]
  (kubectl/names-of (kubectl/filter-list (kubectl/get-all context "persistentvolumes")
                                         #(= "Released" (-> % :status :phase)))))

(defn reclaim-policies
  "A name -> reclaim-policy map for the chosen volumes."
  [context volumes]
  (into {} (for [volume volumes]
             [volume (-> (kubectl/get-cluster-resource context "persistentvolumes" volume)
                         :spec :persistentVolumeReclaimPolicy)])))

(defn delete-effect
  [context volumes]
  (apply effect/plan
         (map #(effect/kubectl context "delete" "persistentvolume" % "--wait=false") volumes)))

(defn gone-checks
  [context volumes]
  (for [volume volumes]
    {:description (format "PersistentVolume %s is gone" volume)
     :timeout 120 :interval 10
     :check (fn [] (nil? (kubectl/get-cluster-resource context "persistentvolumes" volume)))}))

(mutation/book
 {:description "Deletes PersistentVolumes stuck in Released"
  :options {:volumes {:desc "Released PersistentVolumes to delete, repeatable - interactive selection when omitted"
                      :alias :v :coerce [:string]}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["persistentvolumes"]
                  :kubectl-can-delete ["persistentvolumes"]}
  :select {:mode :many :label "PersistentVolume" :flag :volumes :candidates released-volumes}
  :empty-message "no PersistentVolume is Released"
  ;; look up each volume's reclaim policy once, so the confirmation can name it
  :derive (fn [{:keys [context target]}] {:policies (reclaim-policies context target)})
  :items (fn [{:keys [target policies]}]
           (map #(format "%s (%s reclaim policy)" % (get policies % "Unknown")) target))
  :confirm {:action "delete Released PersistentVolumes - a 'Delete' reclaim policy also removes the backing storage"
            :destructive? true}
  :announce (fn [{:keys [target]}] (format "Delete released PersistentVolumes [%s]" (str/join ", " target)))
  :effect (fn [{:keys [context target]}] (delete-effect context target))
  :verify (fn [{:keys [context target]}] (gone-checks context target))})
