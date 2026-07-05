;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.csi
  "Investigates the CSI layer backed by ceph-csi: attachments, orphaned volumes, driver registration."
  (:require [infra.detective :as detective]
            [infra.detectives.csi :as csi]))

(detective/book
 {:description "Investigates the CSI layer: failing attachments, orphaned volumes, driver registration"
  :detectives csi/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["volumeattachments" "persistentvolumes" "csidrivers" "csinodes" "nodes"]}})
