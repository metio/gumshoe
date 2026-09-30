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
  (:require [gumshoe.announce :as announce]
            [gumshoe.effect :as effect]
            [gumshoe.evpn :as evpn]
            [gumshoe.firebook :as firebook]
            [gumshoe.flow :as flow]
            [gumshoe.runbook :as runbook]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

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
          :coerce :string}
   :extinguish {:desc "Put out the fire: remove the drill's fdb entry"
                :alias :e
                :coerce :boolean}})

(def prerequisites
  {:installed-tools ["ssh"]})

(defn connection
  [opts]
  (cond-> {:host (:host opts) :needs-sudo? true}
    (:user opts) (assoc :user (:user opts))))

(defn- pinned?
  "Whether the drill entry is in the host's table, read back over ssh rather than
   inferred from the exit code of the command that set it."
  [conn vni]
  (boolean
   (some #(and (= firebook/drill-mac (:mac %)) (:static? %))
         (evpn/parse-fdb vni (:host conn)
                         (apply ssh/stdout-of conn ["bridge" "fdb" "show"])))))

(defn- extinguish
  [conn vni target]
  (flow/change!
   {:confirmation {:action "put out the fire: remove the drill's fdb entry"
                   :target target
                   :items [firebook/drill-mac]}
    :effect [(apply effect/ssh conn (firebook/fabric-unpin-args vni))]
    :post-checks [{:description "the drill entry is gone"
                   :check #(not (pinned? conn vni))
                   :timeout 30}]}))

(defn- ignite
  [conn vni target announcement-data]
  (flow/change!
   {:confirmation {:action "start a fire drill: a static fdb entry that outranks EVPN"
                   :target target
                   :items [(str firebook/drill-mac " -> " firebook/drill-vtep)]}
    :announce! #(announce/announce! (:host conn) announcement-data
                                    "Fire drill started: stale fdb entry")
    :effect [(apply effect/ssh conn (firebook/fabric-pin-args vni))]
    :post-checks [{:description "the fire is burning: a static entry is in the table"
                   :check #(pinned? conn vni)
                   :timeout 30}]}))

(defn- drill
  [opts {:keys [announcement-data]}]
  (let [conn (connection opts)
        vni (:vni opts)
        target (str (:host opts) " lo." vni)
        extinguishing? (:extinguish opts)
        ok (if extinguishing?
             (extinguish conn vni target)
             (ignite conn vni target announcement-data))]
    (when (and ok (not extinguishing?))
      (stdout/warn
       (str "the fire is lit - find it with 'bb runbooks/detectives/fabric.clj --vni "
            vni "', and put it out with --extinguish")))
    ok))

(runbook/execute!
 {:description "Firebook: pins a static fdb entry for the team to find"
  :options options
  :prerequisites prerequisites
  :action drill})
