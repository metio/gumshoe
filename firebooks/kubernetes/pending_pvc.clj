;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns firebooks.kubernetes.pending-pvc
  "Firebook: creates a PersistentVolumeClaim with a storage class that does
   not exist, so it hangs in Pending for the team to find during a drill.

   The storage detective reports a claim only once it has been Pending for
   five minutes, so the burning check waits longer than that."
  (:require [gumshoe.detectives.storage :as storage]
            [gumshoe.firebook :as firebook]))

(firebook/book
 {:description "Firebook: creates a PersistentVolumeClaim that can never bind"
  :drill "pending-pvc"
  :fire "a PersistentVolumeClaim that can never bind"
  :manifests [(firebook/pvc-manifest
               {:name "pending-pvc"
                :storage-class "this-storage-class-does-not-exist"})]
  :prerequisites {:kubectl-can-create ["persistentvolumeclaims"]}
  :expect {:detectives storage/detectives
           :fire? (fn [{:keys [summary]} _] (= "PersistentVolumeClaim is Pending" summary))
           :timeout 420
           :interval 15}})
