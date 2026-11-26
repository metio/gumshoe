;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.fabric
  "Investigates an EVPN-VXLAN fabric over SSH: which host owns each guest MAC,
   which VTEP the rest of the fabric believes owns it, and the local overrides
   that hide or cause a disagreement.

   Each fdb conflict - a host holding a local tap and a remote entry for one
   MAC - is read twice more from FRR on that host: `show evpn mac vni <vni> mac
   <mac>` for how long the MAC has been local (`local-uptime`), and
   `show bgp l2vpn evpn route vni <vni> mac <mac>` for whether a path from the
   stale VTEP still stands behind the remote entry (`bgp-route`). An `orphan`
   has no such path and a local delete holds; a `live-misadvertisement` has one
   and a delete is relearned. A read that did not answer reports `unread` and
   `unknown`, never an absent route.

   Every VTEP a learned remote fdb row names has its BGP paths counted on that
   host, once per VNI: `show bgp vni <vni> vtep <vtep> json`, or on an FRR
   that does not know that command `show bgp l2vpn evpn route vni <vni> vtep
   <vtep> json`. A VTEP with no path makes evpn-orphan-fdb report the rows
   naming it, one finding per host and VTEP, whether or not a local tap
   collides with them; a count that did not answer leaves them unjudged.

   Hosts, VNIs, gateway VTEPs and guest MAC prefixes each come from their
   option, else from env.edn's :evpn block for the current kubectl context.
   Without gateway VTEPs evpn-gateway-only-mac cannot judge: stderr says so,
   the report marks it unread, and an evpn-inputs finding names it."
  (:require [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.evpn :as evpn-detectives]
            [gumshoe.evpn :as evpn]
            [gumshoe.inputs :as inputs]
            [gumshoe.stdout :as stdout]))

(def options
  (merge
   {:hosts {:desc "Comma-separated fabric hosts - env.edn's :evpn :hosts when omitted"
            :alias :H
            :coerce :string}
    :vni {:desc "Comma-separated VNIs - env.edn's :evpn :vnis when omitted"
          :alias :n
          :coerce :string}
    :gateway-vteps {:desc "Comma-separated gateway VTEPs, the routers' own - env.edn's :evpn :gateway-vteps when omitted; without either, evpn-gateway-only-mac cannot judge"
                    :coerce :string}
    :guest-mac-prefixes {:desc "Comma-separated MAC prefixes of guests - env.edn's :evpn :guest-mac-prefixes when omitted; without either, every MAC counts as a guest"
                         :coerce :string}
    :user {:desc "The SSH user - your ssh config decides when omitted"
           :alias :u
           :coerce :string}}
   detective/output-option))

(def prerequisites
  {:installed-tools ["ssh"]})

(defn- collect
  "The fabric's evidence over SSH with the values it is judged against, or nil
   when no hosts or VNIs are known."
  [opts]
  (let [signals (inputs/current-signals)
        resolved (evpn/resolve-inputs opts #(config/env-value signals %))
        hosts (get-in resolved ["hosts" :value])
        vnis (get-in resolved ["vnis" :value])]
    (stdout/print-section "🔌 Fabric")
    (cond
      (empty? hosts)
      (do (stdout/warn "no fabric hosts - pass --hosts or set :evpn :hosts in env.edn")
          nil)

      (empty? vnis)
      (do (stdout/warn "no VNIs - pass --vni or set :evpn :vnis in env.edn")
          nil)

      :else
      (let [judged (evpn/input-evidence resolved)]
        (doseq [line (evpn-detectives/missing-input-lines judged)]
          (stdout/warn line))
        (merge (evpn/collect-evidence! (evpn/connections hosts {:user (:user opts)}) vnis)
               judged
               {"config" (config/active-config signals)})))))

(detective/book
 {:description "Investigates an EVPN-VXLAN fabric over SSH"
  :options options
  :prerequisites prerequisites
  :detectives evpn-detectives/detectives
  :when-to-run "Reach for this when hosts or guests are unreachable and nothing inside the cluster explains it - a guest whose MAC the fabric attributes to the wrong VTEP is up and serving, and invisible."
  :collect collect
  :extra (fn [evidence] {:inputs (get evidence "evpn-inputs")})})
