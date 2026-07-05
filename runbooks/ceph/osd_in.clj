;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.ceph.osd-in
  "Brings a single OSD back into the cluster, so data migrates back onto it."
  (:require [infra.ceph :as ceph]
            [infra.effect :as effect]
            [infra.flow :as flow]
            [infra.interact :as interact]
            [infra.announce :as announce]
            [infra.runbook :as runbook]
            [infra.ssh :as ssh]
            [infra.stdout :as stdout]))

(def options
  (merge ceph/ssh-options
         {:osd {:desc "The OSD to bring back in (3 or osd.3) - interactive selection when omitted"
                :alias :d
                :coerce :string}}))

(def prerequisites
  {:installed-tools ["ssh"]})

(defn- osd-in
  [opts {:keys [announcement-data]}]
  (let [connection (ceph/connection opts)]
    (stdout/print-section "🔌 Connection")
    (if-not (ssh/check-connection? connection)
      false
      (let [osds (ceph/osds connection)
            out-osds (remove ceph/osd-in? osds)
            candidates (sort (map ceph/osd-name out-osds))
            provided (some->> (:osd opts) ceph/osd-id (str "osd."))
            chosen (interact/choose-one "OSD" candidates (or provided (:osd opts)))
            id (ceph/osd-id chosen)]
        (cond
          (empty? out-osds)
          (do (stdout/ok "every OSD is already in") true)

          (nil? chosen)
          (do (stdout/error "no OSD selected") false)

          :else
          (do
            (stdout/print-data-table {:host (:host opts)
                                      :osd chosen
                                      :up (if (some-> (ceph/osd-numbered osds id) ceph/osd-up?) "yes" "no")})
            (flow/change!
             {:confirmation {:action "bring an OSD back in - data migrates back onto it"
                             :target (:host opts)
                             :items [chosen]}
              :announce! #(announce/announce! (format "ceph (%s)" (:host opts)) announcement-data
                                                          (format "Bring %s back in" chosen))
              :effect (effect/plan (ceph/effect connection "osd" "in" (str id)))
              :post-checks [{:description (format "%s is marked in" chosen)
                             :timeout 30
                             :check (fn []
                                      (let [osd (ceph/osd-numbered (ceph/osds connection) id)]
                                        (and (some? osd) (ceph/osd-in? osd))))}]})))))))

(runbook/execute!
 {:description "Brings a single OSD back into the cluster"
  :options options
  :prerequisites prerequisites
  :action osd-in})
