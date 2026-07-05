;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.ceph.archive-crashes
  "Archives ceph daemon crash reports after they have been looked at, which
   clears the RECENT_CRASH health warning. The reports stay available via
   'ceph crash ls' - archiving only acknowledges them."
  (:require [infra.ceph :as ceph]
            [infra.effect :as effect]
            [infra.flow :as flow]
            [infra.announce :as announce]
            [infra.runbook :as runbook]
            [infra.ssh :as ssh]
            [infra.stdout :as stdout]))

(def options ceph/ssh-options)

(def prerequisites
  {:installed-tools ["ssh"]})

(defn- crash-label
  [crash]
  (format "%s (%s)" (:crash_id crash) (or (:entity crash) "unknown daemon")))

(defn- archive-crashes
  [opts {:keys [announcement-data]}]
  (let [connection (ceph/connection opts)]
    (stdout/print-section "🔌 Connection")
    (if-not (ssh/check-connection? connection)
      false
      (let [crashes (ceph/ceph-json connection "crash" "ls-new")]
        (if (empty? crashes)
          (do (stdout/ok "no unarchived crashes - nothing to do") true)
          (flow/change!
           {:confirmation {:action "archive crash reports - the RECENT_CRASH warning clears, make sure they were actually looked at"
                           :target (:host opts)
                           :items (map crash-label crashes)}
            :announce! #(announce/announce! (format "ceph (%s)" (:host opts)) announcement-data
                                                        (format "Archive %d ceph crash report(s)" (count crashes)))
            :effect (effect/plan (ceph/effect connection "crash" "archive-all"))
            :post-checks [{:description "no unarchived crashes remain"
                           :timeout 30
                           :check (fn [] (empty? (ceph/ceph-json connection "crash" "ls-new")))}]}))))))

(runbook/execute!
 {:description "Archives ceph daemon crash reports, clearing the RECENT_CRASH warning"
  :options options
  :prerequisites prerequisites
  :action archive-crashes})
