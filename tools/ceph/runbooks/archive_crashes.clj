;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.ceph.archive-crashes
  "Archives ceph daemon crash reports after they have been looked at, which
   clears the RECENT_CRASH health warning. The reports stay available via
   'ceph crash ls' - archiving only acknowledges them. It archives exactly
   the reports the confirmation listed, and refuses when ceph does not
   answer the crash list, before the confirmation or after it."
  (:require [gumshoe.ceph :as ceph]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.announce :as announce]
            [gumshoe.runbook :as runbook]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]))

(def options ceph/ssh-options)

(def prerequisites
  {:installed-tools ["ssh"]})

(defn- crash-label
  [crash]
  (format "%s (%s)" (:crash_id crash) (or (:entity crash) "unknown daemon")))

(defn- refuse!
  [why]
  (stdout/err-println (str "refusing: " why))
  false)

(defn- still-readable
  "The judgement after the confirmation: ceph answers the crash list again.
   The effect names each confirmed report, so a report listed since is left
   alone and needs no judgement here."
  [connection]
  (fn []
    (if-let [why (ceph/crashes-refusal (ceph/unarchived-crashes connection))]
      (refuse! why)
      true)))

(defn- archive-crashes
  [opts {:keys [announcement-data]}]
  (let [connection (ceph/connection opts)]
    (stdout/print-section "🔌 Connection")
    (if-not (ssh/check-connection? connection)
      false
      (let [{:keys [crashes] :as listed} (ceph/unarchived-crashes connection)]
        (cond
          (ceph/crashes-refusal listed)
          (refuse! (ceph/crashes-refusal listed))

          (empty? crashes)
          (do (stdout/ok "no unarchived crashes - nothing to do") true)

          :else
          (flow/change!
           {:confirmation {:action "archive crash reports - the RECENT_CRASH warning clears, make sure they were actually looked at"
                           :target (:host opts)
                           :items (map crash-label crashes)}
            :precondition (still-readable connection)
            :announce! #(announce/announce! (format "ceph (%s)" (:host opts)) announcement-data
                                            (format "Archive %d ceph crash report(s)" (count crashes)))
            :effect (apply effect/plan (ceph/archive-effects connection crashes))
            :post-checks [{:description (format "the %d confirmed crash report(s) are archived" (count crashes))
                           :timeout 30 :verifies-absence? true
                           :check #(ceph/archived? crashes (ceph/unarchived-crashes connection))}]}))))))

(runbook/execute!
 {:description "Archives ceph daemon crash reports, clearing the RECENT_CRASH warning"
  :options options
  :prerequisites prerequisites
  :action archive-crashes})
