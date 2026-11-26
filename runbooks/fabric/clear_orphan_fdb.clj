;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.fabric.clear-orphan-fdb
  "Deletes the learned fdb rows that send MACs to a VTEP no BGP path
   advertises - what evpn-orphan-fdb in runbooks/detectives/fabric.clj
   reports after a hypervisor was emptied and left the fabric.

   One decommission leaves rows on every peer, so the book reads every
   fabric host it is given (`--hosts`, else env.edn's :evpn :hosts) and
   clears each one whose rows name the VTEP; `--hosts` with one host clears
   that host alone. `--vtep` names the VTEP; without it the book offers the
   VTEPs a scan finds orphaned.

   A host's rows are deleted only when BGP answered an explicit count of 0
   paths for the VTEP in every VNI they lie in; a host whose table or count
   did not answer is listed as unjudged and keeps its rows, and a VTEP any
   host reads a path for is live and keeps every row. Static and permanent
   pins and every row on a guest tap stay. Per MAC it deletes the `dst
   <vtep> self` row and the MAC's bridge rows on the same overlay device,
   one command each, a row already gone counting as deleted. Every host is
   read again after the confirmation, and a VTEP advertised again or a
   touched host whose rows changed refuses the change. The post-check reads
   each touched host once more and runs evpn-orphan-fdb and
   evpn-fdb-conflict over it."
  (:require [clojure.string :as str]
            [gumshoe.change :as change]
            [gumshoe.config :as config]
            [gumshoe.evpn-clear :as clear]
            [gumshoe.inputs :as inputs]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]))

(def vtep-select
  {:mode :one :label "VTEP" :flag :vtep
   :candidates clear/offered})

(def options
  (select/options vtep-select
                  {:vtep {:desc "The VTEP whose rows to clear, by address - interactive selection among the orphaned VTEPs when omitted"}
                   :hosts {:desc "Comma-separated fabric hosts to clear - env.edn's :evpn :hosts when omitted"
                           :alias :H
                           :coerce :string}
                   :user {:desc "The SSH user - your ssh config decides when omitted"
                          :alias :u
                          :coerce :string}}))

(defn- connect
  [opts]
  (let [signals (inputs/current-signals)]
    (clear/connect opts #(config/env-value signals %))))

(defn- read-again
  [context target host]
  #(clear/read-host (clear/connection-of context host) (constantly [target]) clear/remote-result))

(mutation/book
 {:description "Deletes the learned fdb rows naming a VTEP no BGP path advertises, on one host or across the fabric"
  :options options
  :prerequisites {:installed-tools ["ssh"]}
  :connect connect
  :select vtep-select
  :empty-message "no scanned host holds a learned fdb row naming a VTEP no BGP path advertises"
  :reads {:fabric (fn [{:keys [context target]}]
                    (when-not (clear/vtep-problem target)
                      (clear/read-fabric (:connections context) target clear/remote-result)))}
  :values {:planned (fn [{:keys [fabric target]}]
                      (clear/judge-fabric (or fabric []) target))}
  :done (fn [{:keys [fabric planned]}]
          (and (seq fabric) (clear/cleared? planned)))
  :refusal (fn [{:keys [fabric target planned]}]
             (or (clear/vtep-problem target)
                 (clear/refusal (clear/judge-fabric fabric target) planned)))
  :confirm {:action "delete the fdb rows naming a VTEP no BGP path advertises - frames for those MACs stop going to the dead VTEP"
            :destructive? true}
  :items (fn [{:keys [planned]}] (clear/items planned))
  :announce (fn [{:keys [target planned]}]
              (format "Clear %d fdb rows naming the orphaned VTEP %s on %s"
                      (reduce + (map (comp count clear/deletes) (:orphaned planned)))
                      target
                      (str/join ", " (map :host (:orphaned planned)))))
  :effect (change/destructive
           "gumshoe.evpn-clear/plan"
           (fn [{:keys [context planned]}]
             (clear/plan planned #(clear/connection-of context %))))
  :verify (fn [{:keys [context target planned]}]
            (for [{:keys [host]} (:orphaned planned)]
              (clear/post-check host target (read-again context target host))))
  :recovery (fn [{:keys [target]}]
              (str "Some rows naming " target " may already be deleted and others not. Run the book again "
                   "with --vtep " target ": it reads every host afresh, skips the rows already gone and "
                   "asks again for what is left. runbooks/detectives/fabric.clj shows what still names it."))})
