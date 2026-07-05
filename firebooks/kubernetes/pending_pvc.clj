;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns firebooks.kubernetes.pending-pvc
  "Firebook: creates a PersistentVolumeClaim with a storage class that does
   not exist, so it hangs in Pending for the team to find during a drill."
  (:require [infra.firebook :as firebook]
            [infra.flow :as flow]
            [infra.kubectl :as kubectl]
            [infra.announce :as announce]
            [infra.runbook :as runbook]))

(def options
  {:extinguish {:desc "Put out the fire: delete the fire-drill namespace"
                :alias :e
                :coerce :boolean}})

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-create ["namespaces" "persistentvolumeclaims"]
   :kubectl-can-delete ["namespaces"]})

(defn- pending-pvc
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)]
    (if (:extinguish opts)
      (firebook/extinguish! context)
      (flow/change!
       {:confirmation {:action "start a fire drill: a PersistentVolumeClaim that can never bind"
                       :target cluster
                       :items [(str firebook/drill-namespace "/pending-pvc")]}
        :announce! #(announce/announce! cluster announcement-data
                                                     "Fire drill started: pending-pvc")
        :execute! #(firebook/ignite! context
                                     [(firebook/pvc-manifest
                                       {:name "pending-pvc"
                                        :storage-class "this-storage-class-does-not-exist"})])
        :post-checks [(firebook/burning-check context "persistentvolumeclaims" "pending-pvc")]}))))

(runbook/execute!
 {:description "Firebook: creates a PersistentVolumeClaim that can never bind"
  :options options
  :prerequisites prerequisites
  :action pending-pvc})
