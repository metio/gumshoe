;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.ceph.status
  "Shows the status and health detail of a cephadm-managed ceph cluster."
  (:require [gumshoe.ceph :as ceph]
            [gumshoe.runbook :as runbook]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]))

(def options ceph/ssh-options)

(def prerequisites
  {:installed-tools ["ssh"]})

(defn- status
  [opts _ctx]
  (let [connection (ceph/connection opts)]
    (stdout/print-section "🔌 Connection")
    (if-not (ssh/check-connection? connection)
      false
      (do
        (stdout/print-section "🐙 ceph status")
        (and (ceph/ceph-stream! connection "status")
             (do (stdout/print-section "🩺 ceph health detail")
                 (ceph/ceph-stream! connection "health" "detail")))))))

(runbook/execute!
 {:description "Shows the status and health detail of a cephadm-managed ceph cluster"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action status})
