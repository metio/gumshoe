;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.velero.phases
  "The phases of a Velero Backup, shared by the detectives that judge backups
   and the books that act on them, so both agree on which backups are still
   being worked on.")

(def running-phases
  "Backup phases in which Velero still works on the backup. New is the phase
   of every backup before the controller picks it up; Queued and ReadyToStart
   are the phases of a backup waiting for a free worker."
  #{"New" "Queued" "ReadyToStart" "InProgress" "WaitingForPluginOperations"
    "WaitingForPluginOperationsPartiallyFailed" "Finalizing" "FinalizingPartiallyFailed"})

(defn running?
  "Whether Velero still works on a backup in `phase`. A backup without a phase
   has not been picked up yet, which is New."
  [phase]
  (or (nil? phase) (= "" phase) (contains? running-phases phase)))

(defn label
  "The phase as an operator reads it: New when the backup carries none."
  [phase]
  (if (or (nil? phase) (= "" phase)) "New" phase))
