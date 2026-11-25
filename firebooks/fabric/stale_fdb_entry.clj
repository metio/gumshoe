;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns firebooks.fabric.stale-fdb-entry
  "Firebook: pins a static fdb entry on a hypervisor's VXLAN device, so the team
   has to find a forwarding override nobody can explain.

   This is the fault an operator leaves behind after working around a fabric
   problem by hand, and the one that bites months later when a guest migrates: a
   static entry outranks whatever EVPN learns, so it keeps working exactly until
   the guest moves, and then black-holes it. Finding it is the skill being drilled,
   and `runbooks/detectives/fabric.clj` reports it under evpn-static-fdb.

   What it deliberately does not simulate: a gateway VTEP claiming a guest MAC.
   That originates on the border routers rather than on a hypervisor, so no drill
   run from here can produce it - and a drill that pinned a real guest's MAC to
   fake it would make a real machine unreachable."
  (:require [gumshoe.catalog :as catalog]
            [gumshoe.detectives.evpn :as evpn-detectives]
            [gumshoe.effect :as effect]
            [gumshoe.firebook :as firebook]
            [gumshoe.stdout :as stdout]))

(def options
  {:host {:desc "The hypervisor to light the fire on"
          :alias :H
          :require true
          :coerce :string}
   :vni {:desc "The VNI whose VXLAN device carries the entry"
         :alias :n
         :require true
         :coerce :long}
   :user {:desc "The SSH user - your ssh config decides when omitted"
          :alias :u
          :coerce :string}})

(defn connection
  [opts]
  (cond-> {:host (:host opts) :needs-sudo? true}
    (:user opts) (assoc :user (:user opts))))

(firebook/book
 {:description "Firebook: pins a static fdb entry for the team to find"
  :options options
  :prerequisites {:installed-tools ["ssh"]}
  :drill "stale fdb entry"
  :fire "a static fdb entry that outranks EVPN"
  :derive (fn [{:keys [host vni] :as opts}]
            {:conn (connection opts) :vni vni :system host :target (str host " lo." vni)})
  :ignite {:items (fn [_] [(str firebook/drill-mac " -> " firebook/drill-vtep)])
           :effect (fn [{:keys [conn vni]}] (apply effect/ssh conn (firebook/fabric-pin-args vni)))}
  :extinguish {:put-out "remove the drill's fdb entry"
               :items (fn [_] [firebook/drill-mac])
               :effect (fn [{:keys [conn vni]}] (apply effect/ssh conn (firebook/fabric-extinguish-args vni)))
               :recovery firebook/fabric-recovery}
  :expect {:detectives (filterv #(= "evpn-static-fdb" (:name %)) evpn-detectives/detectives)
           :collect (fn [{:keys [conn vni]}] (firebook/fabric-evidence conn vni))
           :fire? (fn [{:keys [component]} {:keys [conn]}]
                    (= component (str (:host conn) "/" firebook/drill-mac)))
           :timeout 30
           :out-timeout 30}
  :lit (fn [{:keys [vni]}]
         (stdout/warn
          (str "the fire is lit - find it with '" (catalog/run-command "runbooks/detectives/fabric.clj") " --vni "
               vni "', and put it out with --extinguish")))})
