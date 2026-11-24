;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.ceph.osd-in
  "Brings a single OSD back into the cluster, so data migrates back onto it."
  (:require [gumshoe.ceph :as ceph]
            [gumshoe.effect :as effect]
            [gumshoe.mutation :as mutation]))

(defn- osd-of
  "The OSD a pick names, read now, or nil when it is gone or the read did
   not answer."
  [{:keys [context target]}]
  (ceph/osd-numbered (ceph/osds context) (ceph/osd-id target)))

(mutation/book
 {:description "Brings a single OSD back into the cluster"
  :options (merge ceph/ssh-options
                  {:osd {:desc "The OSD to bring back in (3 or osd.3) - interactive selection when omitted"
                         :alias :d
                         :coerce ceph/osd-pick}})
  :prerequisites {:installed-tools ["ssh"]}
  :connect ceph/connect
  :select {:mode :one
           :label "OSD"
           :flag :osd
           :candidates (fn [connection]
                         (sort (map ceph/osd-name (remove ceph/osd-in? (ceph/osds connection)))))}
  :empty-message "every OSD is already in"
  :confirm {:action "bring an OSD back in - data migrates back onto it"}
  :precondition (fn [ctx] (some-> (osd-of ctx) ceph/osd-in? not))
  :announce (fn [{:keys [target]}] (format "Bring %s back in" target))
  :effect (fn [{:keys [context target]}]
            (effect/plan (ceph/effect context "osd" "in" (str (ceph/osd-id target)))))
  :verify (fn [{:keys [target] :as ctx}]
            [{:description (format "%s is marked in" target)
              :timeout 30
              :check (fn [] (boolean (some-> (osd-of ctx) ceph/osd-in?)))}])})
