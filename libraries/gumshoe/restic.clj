;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.restic
  "Evidence collection for restic backups. Queries each repository's snapshots
   directly, so the detective judges whether backups are actually landing -
   not merely whether a backup job exists. The repository password travels via
   the RESTIC_PASSWORD environment variable, never in process arguments."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
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

(defn- snapshots-of
  [repository password]
  (let [result (shell/execute-env {"RESTIC_PASSWORD" password}
                                  "restic" "-r" repository "snapshots" "--json" "--no-lock")]
    (if (zero? (:exit result))
      {:reachable true
       :snapshots (try (json/parse-string (:out result) true) (catch Exception _ []))}
      {:reachable false
       :error (-> (or (:err result) "") str/trim str/split-lines last)})))

(defn collect-evidence!
  "Queries every repository. thresholds are the staleness limits in days."
  [{:keys [repositories password warn-days critical-days]}]
  (stdout/print-section "🔍 Evidence (restic)")
  (let [probes (mapv (fn [repository]
                       (stdout/err-println (format "  %s %s" (stdout/blue "▸") repository))
                       (future [repository (snapshots-of repository password)]))
                     repositories)]
    {:now (java.time.Instant/now)
     :warn-days warn-days
     :critical-days critical-days
     "repositories" (into {} (map deref) probes)}))
