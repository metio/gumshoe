;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns playbooks.ceph.upgrade
  "Playbook: upgrades a cephadm-managed ceph cluster to a target version.

   1. preflight - show status and versions, refuse on HEALTH_ERR or a health
      status that could not be read
   2. backup - capture config, osd/fs/crush/auth dumps to LOCAL files, so the
      safety net does not live on the cluster being changed; refuse to proceed
      if any backup comes back empty; a rehearsal (--dry-run) lists the files
      it would write and writes none
   3. start - ceph orch upgrade start (after confirmation)
   4. watch - poll the upgrade status until it finishes
   5. verify - every daemon reports the target version and the cluster reports
      HEALTH_OK or HEALTH_WARN

   --require-osd-release runs the one-way ratchet afterwards, separately
   confirmed. --status just shows the current upgrade progress and changes
   nothing."
  (:require [babashka.fs :as fs]
            [briko.fs :as bfs]
            [clojure.string :as str]
            [gumshoe.ceph :as ceph]
            [gumshoe.flow :as flow]
            [gumshoe.announce :as announce]
            [gumshoe.runbook :as runbook]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]))

(def options
  (merge ceph/ssh-options
         {:target-version {:desc "The ceph version to upgrade to, e.g. 17.2.9"
                           :alias :t
                           :coerce :string}
          :require-osd-release {:desc "After the upgrade, ratchet require-osd-release (e.g. quincy)"
                                :coerce :string}
          :watch-timeout {:desc "Seconds to watch the upgrade before handing back (re-run --status later)"
                          :default 7200
                          :coerce :long}
          :status {:desc "Only show the current upgrade status, change nothing"
                   :coerce :boolean}}))

(def prerequisites
  {:installed-tools ["ssh"]})

(def ^:private backup-commands
  {"config-dump" ["config" "dump"]
   "osd-dump" ["osd" "dump"]
   "fs-dump" ["fs" "dump"]
   "crush-dump" ["osd" "crush" "dump"]
   "auth-export" ["auth" "export"]
   "status" ["status"]
   "versions" ["versions"]})

(defn- timestamp
  []
  (.format (java.time.LocalDateTime/now)
           (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss")))

(def secret-backups
  "The backups that hold secrets: auth export carries the key of every ceph
   client and daemon, client.admin included."
  #{"auth-export"})

(defn take-backups!
  "Captures every backup to a local file. Returns true only when all of them
   produced content - an empty dump means an upgrade would run without a net.
   The directory is created rwx------ and must not exist yet, and every file
   is created rw-------, because auth-export holds the cluster's keys."
  [connection directory]
  (if-let [problem (try (bfs/create-private-directory! directory) nil
                        (catch Exception e (or (ex-message e) (str e))))]
    (do (stdout/error (format "could not create the backup directory %s owner-only - refusing to upgrade without backups: %s"
                              directory problem))
        false)
    (every? true?
            (mapv (fn [[name args]]
                    (let [output (apply ceph/ceph-stdout connection args)
                          file (str (fs/path directory (str name ".txt")))
                          problem (when-not (str/blank? output)
                                    (try (bfs/write-private! file output) nil
                                         (catch Exception e (or (ex-message e) (str e)))))]
                      (cond
                        (str/blank? output)
                        (do (stdout/error (format "backup '%s' produced no output - refusing to upgrade without it" name))
                            false)

                        problem
                        (do (stdout/error (format "could not write backup %s - refusing to upgrade without it: %s" file problem))
                            false)

                        :else
                        (do (stdout/check-ok "backed up" name)
                            (when (secret-backups name)
                              (stdout/warn (format "%s.txt holds the keys of every ceph client, client.admin included - keep it owner-only and never commit it" name)))
                            true))))
                  backup-commands))))

(defn rehearsed-backups
  "The lines a rehearsal prints instead of taking the backups: the directory
   and every file it would write, each with the ceph command that fills it.
   A rehearsal writes nothing, and auth-export would put every cephx key on
   local disk."
  [directory]
  (cons (str "would create " directory " (owner-only)")
        (for [[name args] backup-commands]
          (format "would write %s from: ceph %s%s"
                  (str (fs/path directory (str name ".txt")))
                  (str/join " " args)
                  (if (secret-backups name) " (holds every cephx key)" "")))))

(defn backup!
  "Takes the backups into directory, or under flow/*dry-run* prints what it
   would write and writes nothing. Returns true when the run may go on."
  [connection directory]
  (if flow/*dry-run*
    (do (doseq [line (rehearsed-backups directory)]
          (stdout/err-println (str "  " (stdout/blue "rehearsal:") " " line)))
        true)
    (when (take-backups! connection directory)
      (stdout/ok (format "backups saved locally to %s" directory))
      true)))

(defn- watch-upgrade!
  [connection timeout]
  (loop [elapsed 0]
    (let [status (ceph/upgrade-status connection)]
      (cond
        ;; upgrade-finished? treats an unreadable (nil) status as "keep waiting",
        ;; not "done", so a transient blip - a blank status while orch upgrade
        ;; restarts the mgr daemons - does not abandon an hours-long upgrade early.
        (ceph/upgrade-finished? status)
        (do (stdout/ok "cephadm reports the upgrade is no longer in progress") true)

        (>= elapsed timeout)
        (do (stdout/warn (format "upgrade still running after %ds - re-run with --status to keep watching" timeout))
            false)

        :else
        (do (stdout/item :in-progress (format "%s (%ds elapsed)"
                                              (or (:message status) (:progress status) "upgrading")
                                              elapsed))
            (Thread/sleep 30000)
            (recur (+ elapsed 30)))))))

(defn- ratchet-osd-release!
  [connection host announcement-data release]
  (flow/change!
   {:confirmation {:action (format "ratchet require-osd-release to %s - OSDs older than this can never rejoin" release)
                   :target host
                   :items [release]
                   :destructive? true}
    :announce! #(announce/announce! (format "ceph (%s)" host) announcement-data
                                                (format "Set require-osd-release to %s" release))
    :execute! #(ceph/ceph-stream! connection "osd" "require-osd-release" release)
    :post-checks [{:description (format "require_osd_release is %s" release)
                   :timeout 30
                   :check (fn [] (= release (-> (ceph/ceph-json connection "osd" "dump")
                                                :require_osd_release)))}]}))

(defn- start-upgrade!
  [connection host announcement-data opts]
  (let [target (:target-version opts)]
    (flow/change!
     {:confirmation {:action (format "upgrade the ceph cluster to %s - this rolls every daemon and can take hours" target)
                     :target host
                     :items [(format "target version: %s" target)]
                     :destructive? true}
      :announce! #(announce/announce! (format "ceph (%s)" host) announcement-data
                                                  (format "Start ceph upgrade to %s" target))
      :execute! (fn []
                  (stdout/print-section "3/5 🚀 Start")
                  (and (ceph/ceph-stream! connection "orch" "upgrade" "start" "--ceph-version" target)
                       (do (stdout/print-section "4/5 👀 Watch")
                           (watch-upgrade! connection (:watch-timeout opts)))))
      :post-checks [{:description (format "every daemon reports version %s" target)
                     :timeout 120 :interval 15
                     :check (fn [] (ceph/all-on-version? (ceph/versions connection) target))}
                    {:description "the cluster reports HEALTH_OK or HEALTH_WARN after the upgrade"
                     :timeout 120 :interval 15
                     :check (fn [] (ceph/healthy-enough? (ceph/health-result connection)))}]})))

(defn- upgrade
  [opts {:keys [announcement-data]}]
  (let [connection (ceph/connection opts)
        host (:host opts)]
    (stdout/print-section "🔌 Connection")
    (cond
      (not (ssh/check-connection? connection))
      false

      (:status opts)
      (do (stdout/print-section "📈 Upgrade status")
          (ceph/ceph-stream! connection "orch" "upgrade" "status"))

      (str/blank? (:target-version opts))
      (do (stdout/error "--target-version is required to start an upgrade (or use --status to just look)")
          false)

      :else
      (let [refusal (ceph/upgrade-refusal (ceph/health-result connection))]
        (stdout/print-section "1/5 🩺 Preflight")
        (ceph/ceph-stream! connection "status")
        (ceph/ceph-stream! connection "versions")
        (if refusal
          (do (stdout/error refusal) false)
          (do
            (stdout/print-section "2/5 💾 Backup")
            (let [directory (str (fs/path "ceph-backups" (str host "-" (timestamp))))]
              (if-not (backup! connection directory)
                false
                (let [upgraded (start-upgrade! connection host announcement-data opts)]
                  (if (and upgraded (not (str/blank? (:require-osd-release opts))))
                    (ratchet-osd-release! connection host announcement-data (:require-osd-release opts))
                    upgraded))))))))))

(runbook/execute!
 {:description "Playbook: upgrade a cephadm-managed ceph cluster to a target version"
  :options options
  :prerequisites prerequisites
  :action upgrade})
