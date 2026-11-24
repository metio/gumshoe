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
   `unknown`, never an absent route."
  (:require [clojure.string :as str]
            [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.evpn :as evpn-detectives]
            [gumshoe.evpn :as evpn]
            [gumshoe.inputs :as inputs]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def options
  (merge
   {:hosts {:desc "Comma-separated fabric hosts - env.edn's :evpn :hosts when omitted"
            :alias :H
            :coerce :string}
    :vni {:desc "Comma-separated VNIs - env.edn's :evpn :vnis when omitted"
          :alias :n
          :coerce :string}
    :user {:desc "The SSH user - your ssh config decides when omitted"
           :alias :u
           :coerce :string}}
   detective/output-option))

(def prerequisites
  {:installed-tools ["ssh"]})

(defn- split-list
  [s]
  (when-not (nil? s)
    (->> (str/split (str s) #",")
         (map str/trim)
         (remove str/blank?)
         vec)))

(defn- investigate
  [opts _ctx]
  (detective/when-to-run! "Reach for this when hosts or guests are unreachable and nothing inside the cluster explains it - a guest whose MAC the fabric attributes to the wrong VTEP is up and serving, and invisible.")
  (let [signals (inputs/current-signals)
        hosts (or (seq (split-list (:hosts opts)))
                  (seq (config/env-value signals [:evpn :hosts])))
        vnis (or (some->> (split-list (:vni opts)) (map parse-long) (remove nil?) seq)
                 (seq (config/env-value signals [:evpn :vnis])))]
    (stdout/print-section "🔌 Fabric")
    (cond
      (empty? hosts)
      (do (stdout/warn "no fabric hosts - pass --hosts or set :evpn :hosts in env.edn")
          false)

      (empty? vnis)
      (do (stdout/warn "no VNIs - pass --vni or set :evpn :vnis in env.edn")
          false)

      :else
      (let [conns (evpn/connections hosts {:user (:user opts)})
            evidence (assoc (evpn/collect-evidence! conns vnis)
                            "config" (config/active-config signals))]
        (detective/report!
         evpn-detectives/detectives
         (detective/run-detectives evpn-detectives/detectives evidence)
         (:output opts "text"))))))

(runbook/execute!
 {:description "Investigates an EVPN-VXLAN fabric over SSH"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action investigate})
