;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns playbooks.ceph.upgrade
  "Playbook: upgrades a cephadm-managed ceph cluster to a target version.

   1. preflight - show status and versions, refuse on HEALTH_ERR
   2. backup - capture config, osd/fs/crush/auth dumps to LOCAL files, so the
      safety net does not live on the cluster being changed; refuse to proceed
      if any backup comes back empty
   3. start - ceph orch upgrade start (after confirmation)
   4. watch - poll the upgrade status until it finishes
   5. verify - every daemon reports the target version and the cluster is not
      HEALTH_ERR

   --require-osd-release runs the one-way ratchet afterwards, separately
   confirmed. --status just shows the current upgrade progress and changes
   nothing."
  (:require [babashka.fs :as fs]
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

(defn- take-backups!
  "Captures every backup to a local file. Returns true only when all of them
   produced content - an empty dump means an upgrade would run without a net."
  [connection directory]
  (fs/create-dirs directory)
  (every? true?
          (mapv (fn [[name args]]
                  (let [output (apply ceph/ceph-stdout connection args)]
                    (if (str/blank? output)
                      (do (stdout/error (format "backup '%s' produced no output - refusing to upgrade without it" name))
                          false)
                      (do (spit (str (fs/path directory (str name ".txt"))) output)
                          (stdout/check-ok "backed up" name)
                          true))))
                backup-commands)))

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
        (do (stdout/err-println (format "  ⏳ %s (%ds elapsed)"
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
                    {:description "the cluster is not HEALTH_ERR after the upgrade"
                     :timeout 120 :interval 15
                     :check (fn [] (not= "HEALTH_ERR" (ceph/health-status connection)))}]})))

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
      (let [health (ceph/health-status connection)]
        (stdout/print-section "1/5 🩺 Preflight")
        (ceph/ceph-stream! connection "status")
        (ceph/ceph-stream! connection "versions")
        (if (= "HEALTH_ERR" health)
          (do (stdout/error "cluster is HEALTH_ERR - resolve that before upgrading") false)
          (do
            (stdout/print-section "2/5 💾 Backup")
            (let [directory (str (fs/path "ceph-backups" (str host "-" (timestamp))))]
              (if-not (take-backups! connection directory)
                false
                (do
                  (stdout/ok (format "backups saved locally to %s" directory))
                  (let [upgraded (start-upgrade! connection host announcement-data opts)]
                    (if (and upgraded (not (str/blank? (:require-osd-release opts))))
                      (ratchet-osd-release! connection host announcement-data (:require-osd-release opts))
                      upgraded)))))))))))

(runbook/execute!
 {:description "Playbook: upgrade a cephadm-managed ceph cluster to a target version"
  :options options
  :prerequisites prerequisites
  :action upgrade})
