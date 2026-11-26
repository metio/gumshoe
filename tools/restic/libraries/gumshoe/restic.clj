;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.restic
  "Evidence collection for restic backups. Queries each repository's snapshots
   directly, so the detective judges whether backups are actually landing -
   not merely whether a backup job exists. The repository password travels via
   the RESTIC_PASSWORD environment variable, never in process arguments."
  (:require [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.json :as json]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

;; ---------------------------------------------------------------------------
;; pure snapshot analysis

(defn snapshot-instant
  [snapshot]
  (try
    (.toInstant (java.time.OffsetDateTime/parse (:time snapshot)))
    (catch Exception _ nil)))

(defn latest-per-target
  "Groups snapshots by host and paths, returning the newest instant and the
   snapshot count for each backup target."
  [snapshots]
  (for [[[hostname paths] snaps] (group-by (juxt :hostname :paths) snapshots)]
    {:hostname hostname
     :paths (vec paths)
     :count (count snaps)
     :latest (->> snaps (keep snapshot-instant) sort last)}))

;; ---------------------------------------------------------------------------
;; querying repositories

(def unreadable-output-error
  "Why a repository whose restic answered output that is not a snapshot list
   counts as unread."
  "restic answered output that is not a snapshot list")

(defn snapshots-answer
  "What a `restic snapshots --json` run says about its repository:
   {:reachable true :snapshots [..]} when it exited cleanly with a JSON array
   of snapshots, else {:reachable false :error ..}. An answer that does not
   parse as a whole array never reads as an empty repository: nothing in it
   was read, so it says nothing about what the repository holds."
  [{:keys [exit out err]}]
  (if (zero? exit)
    ;; restic before 0.11.0 printed null for a repository without snapshots
    ;; (restic issue 2979); every later release prints [].
    (let [snapshots (if (= "null" (str/trim (str out))) [] (json/parse-or-nil out))]
      (if (and (vector? snapshots) (every? map? snapshots))
        {:reachable true :snapshots snapshots}
        {:reachable false :error unreadable-output-error}))
    {:reachable false
     :error (-> (or err "") str/trim str/split-lines last)}))

(defn- snapshots-of
  [repository password]
  (snapshots-answer (shell/execute-env {"RESTIC_PASSWORD" password}
                                       "restic" "-r" repository "snapshots" "--json" "--no-lock")))

(defn collect-evidence!
  "Queries every repository. thresholds are the staleness limits in days."
  [{:keys [repositories password warn-days critical-days]}]
  (stdout/print-section "🔍 Evidence (restic)")
  (run! #(stdout/item :info %) repositories)
  (let [probes (concurrent/map-all (fn [repository] [repository (snapshots-of repository password)])
                                   repositories)]
    {:now (java.time.Instant/now)
     :warn-days warn-days
     :critical-days critical-days
     "repositories" (into {} probes)}))
