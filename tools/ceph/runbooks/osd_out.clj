;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.ceph.osd-out
  "Takes a single OSD out of the cluster, which starts migrating its data to
   the remaining OSDs. On a degraded cluster this is blocked unless --force
   is given, and the last in-OSD can never be taken out."
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
         {:osd {:desc "The OSD to take out (3 or osd.3) - interactive selection when omitted"
                :alias :d
                :coerce :string}
          :force {:desc "Take the OSD out even though the cluster is not HEALTH_OK"
                  :alias :f
                  :coerce :boolean}}))

(def prerequisites
  {:installed-tools ["ssh"]})

(defn- osd-out
  [opts {:keys [announcement-data]}]
  (let [connection (ceph/connection opts)]
    (stdout/print-section "🔌 Connection")
    (if-not (ssh/check-connection? connection)
      false
      (let [health (ceph/health-status connection)
            forced (boolean (:force opts))
            osds (ceph/osds connection)
            in-osds (filter ceph/osd-in? osds)
            candidates (sort (map ceph/osd-name in-osds))
            provided (some->> (:osd opts) ceph/osd-id (str "osd."))
            chosen (interact/choose-one "OSD" candidates (or provided (:osd opts)))
            id (ceph/osd-id chosen)]
        (cond
          (nil? chosen)
          (do (stdout/error "no OSD selected") false)

          (<= (count in-osds) 1)
          (do (stdout/error "refusing to take out the last in-OSD - the cluster would lose all data placement")
              false)

          (and (not= "HEALTH_OK" health) (not forced))
          (do (stdout/error (format "cluster is %s - resolve that first, or rerun with --force" health))
              false)

          :else
          (do
            (stdout/print-data-table {:host (:host opts)
                                      :health health
                                      :osd chosen
                                      (keyword "in-osds") (count in-osds)})
            (flow/change!
             {:confirmation {:action (if forced
                                       (format "take an OSD out ON A %s CLUSTER - data migration starts immediately" health)
                                       "take an OSD out - its data migrates to the remaining OSDs")
                             :target (:host opts)
                             :items [chosen]
                             :destructive? forced}
              :announce! #(announce/announce! (format "ceph (%s)" (:host opts)) announcement-data
                                                          (format "Take %s out" chosen))
              :effect (effect/plan (ceph/effect connection "osd" "out" (str id)))
              :post-checks [{:description (format "%s is marked out" chosen)
                             :timeout 30
                             :check (fn []
                                      (let [osd (ceph/osd-numbered (ceph/osds connection) id)]
                                        (and (some? osd) (not (ceph/osd-in? osd)))))}]})))))))

(runbook/execute!
 {:description "Takes a single OSD out of the cluster, migrating its data away"
  :options options
  :prerequisites prerequisites
  :action osd-out})
