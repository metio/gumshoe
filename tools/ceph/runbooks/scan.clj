;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.ceph
  "Investigates a cephadm-managed ceph cluster over SSH: health checks, OSDs,
   placement groups, monitor quorum, capacity, services, and crashes."
  (:require [gumshoe.ceph :as ceph]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.ceph :as ceph-detectives]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]))

(def options
  (merge ceph/ssh-options detective/output-option))

(def prerequisites
  {:installed-tools ["ssh"]})

(defn- collect
  "The ceph evidence from the mgr host, or nil when it can not be reached."
  [opts]
  (let [connection (ceph/connection opts)]
    (stdout/print-section "🔌 Connection")
    (when (ssh/check-connection? connection)
      (ceph/collect-evidence! connection))))

(detective/book
 {:description "Investigates a cephadm-managed ceph cluster over SSH"
  :options options
  :prerequisites prerequisites
  :detectives ceph-detectives/detectives
  :when-to-run "Reach for this when storage is slow or degraded - the ceph cluster's own health, its OSDs, and placement-group state, straight from a mgr host."
  :collect collect})
