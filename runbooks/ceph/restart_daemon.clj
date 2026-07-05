;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.ceph.restart-daemon
  "Restarts a single ceph daemon via the orchestrator. On a degraded cluster
   this is blocked unless --force is given, because restarting daemons while
   ceph is already unhealthy is how small incidents become big ones."
  (:require [gumshoe.ceph :as ceph]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.announce :as announce]
            [gumshoe.runbook :as runbook]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]))

(def options
  (merge ceph/ssh-options
         {:daemon {:desc "The daemon to restart (e.g. osd.3, mon.ceph-1) - interactive selection when omitted"
                   :alias :d
                   :coerce :string}
          :force {:desc "Restart even though the cluster is not HEALTH_OK"
                  :alias :f
                  :coerce :boolean}}))

(def prerequisites
  {:installed-tools ["ssh"]})

(defn- restart-daemon
  [opts {:keys [announcement-data]}]
  (let [connection (ceph/connection opts)]
    (stdout/print-section "🔌 Connection")
    (if-not (ssh/check-connection? connection)
      false
      (let [health (ceph/health-status connection)
            forced (boolean (:force opts))
            daemons (ceph/daemons connection)
            daemon (interact/choose-one "Daemon" (sort (map :daemon_name daemons)) (:daemon opts))]
        (cond
          (nil? daemon)
          (do (stdout/error "no daemon selected") false)

          (and (not= "HEALTH_OK" health) (not forced))
          (do (stdout/error (format "cluster is %s - resolve that first, or rerun with --force" health))
              false)

          :else
          (let [before (ceph/daemon-named daemons daemon)]
            (stdout/print-data-table {:host (:host opts)
                                      :health health
                                      :daemon daemon
                                      :state (:status_desc before)})
            (flow/change!
             {:confirmation {:action (if forced
                                       (format "restart a ceph daemon ON A %s CLUSTER" health)
                                       "restart a ceph daemon - brief service interruption")
                             :target (:host opts)
                             :items [daemon]
                             :destructive? forced}
              :announce! #(announce/announce! (format "ceph (%s)" (:host opts)) announcement-data
                                                          (format "Restart ceph daemon %s" daemon))
              :effect (effect/plan (ceph/effect connection "orch" "daemon" "restart" daemon))
              :post-checks [{:description (format "daemon %s is running again after the restart" daemon)
                             :timeout 300
                             :interval 10
                             :check (fn []
                                      (let [now (ceph/daemon-named (ceph/daemons connection) daemon)]
                                        (and (ceph/daemon-running? now)
                                             (or (nil? (:started before))
                                                 (not= (:started before) (:started now))))))}]})))))))

(runbook/execute!
 {:description "Restarts a single ceph daemon via the orchestrator"
  :options options
  :prerequisites prerequisites
  :action restart-daemon})
