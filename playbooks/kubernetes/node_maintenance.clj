;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns playbooks.kubernetes.node-maintenance
  "Playbook: takes a node out of service for maintenance and brings it back.

   1. checks for PodDisruptionBudgets that would block the drain
   2. cordons and drains the node (after confirmation)
   3. waits while you do the actual maintenance
   4. uncordons the node
   5. verifies node health with the detectives

   Every step is verified before the next one starts."
  (:require [infra.detective :as detective]
            [infra.detectives.disruption :as disruption]
            [infra.detectives.nodes :as nodes]
            [infra.interact :as interact]
            [infra.kubectl :as kubectl]
            [infra.announce :as announce]
            [infra.runbook :as runbook]
            [infra.shell :as shell]
            [infra.stdout :as stdout]
            [infra.verify :as verify]))

(def options
  {:node {:desc "The node to maintain - interactive selection when omitted"
          :alias :n
          :coerce :string}
   :grace-period {:desc "Seconds each pod is given to terminate - the pod's own value when omitted"
                  :alias :g
                  :default -1
                  :coerce :long}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["nodes" "pods" "poddisruptionbudgets"]
   :kubectl-can-patch ["nodes"]
   :kubectl-can-delete ["pods"]})

(defn- kubectl!
  [context & args]
  (apply stdout/print-command "kubectl" args)
  (zero? (apply shell/run-with-output "kubectl" (str "--context=" context) args)))

(defn- wait-for-operator!
  []
  (stdout/print-banner stdout/yellow "🔧 The node is drained - perform the maintenance now")
  (binding [*out* *err*]
    (print "Press ENTER when the maintenance is finished: ")
    (flush))
  (read-line))

(defn- drain-verified!
  [context node grace-period]
  (and (kubectl! context "drain" node
                 "--ignore-daemonsets" "--delete-emptydir-data"
                 (str "--grace-period=" grace-period))
       (verify/all [{:description (format "no evictable pods remain on node %s" node)
                     :timeout 120
                     :interval 10
                     :check (fn [] (empty? (kubectl/drainable-pods (kubectl/pods-on-node context node))))}])))

(defn- uncordon-verified!
  [context node]
  (and (kubectl! context "uncordon" node)
       (verify/all [{:description (format "node %s is schedulable again" node)
                     :check (fn [] (not (true? (-> (kubectl/get-cluster-resource context "nodes" node)
                                                   :spec :unschedulable))))}])))

(defn- maintain
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        node-list (kubectl/get-all context "nodes")
        node (interact/choose-one "Node" (kubectl/names-of node-list) (:node opts))]
    (if (nil? node)
      (do (stdout/error "no node selected") false)
      (do
        (stdout/print-section "1/5 🔬 Disruption check")
        (detective/print-report!
         (detective/run-detectives disruption/detectives
                                   (detective/collect-evidence! context disruption/detectives)))
        (cond
          (not (interact/confirm! {:action "cordon and drain a node, and uncordon it after the maintenance"
                                   :target cluster
                                   :items [node]
                                   :destructive? true}))
          false

          :else
          (do
            (announce/announce! cluster announcement-data
                                             (format "Node maintenance started on %s" node))
            (stdout/print-section "2/5 🚧 Cordon")
            (cond
              (not (kubectl! context "cordon" node))
              (do (stdout/error (format "could not cordon node %s, stopping" node)) false)

              :else
              (do
                (stdout/print-section "3/5 💨 Drain")
                (if-not (drain-verified! context node (:grace-period opts))
                  (do (stdout/error (format "node %s is not fully drained - it stays cordoned, resolve manually" node))
                      false)
                  (do
                    (wait-for-operator!)
                    (stdout/print-section "4/5 ✅ Uncordon")
                    (if-not (uncordon-verified! context node)
                      (do (stdout/error (format "node %s could not be uncordoned - it stays cordoned!" node))
                          false)
                      (do
                        (announce/announce! cluster announcement-data
                                                         (format "Node maintenance finished on %s" node))
                        (stdout/print-section "5/5 🔬 Verification")
                        (detective/investigate! context nodes/detectives)))))))))))))

(runbook/execute!
 {:description "Playbook: cordon, drain, maintain, uncordon, and verify a node"
  :options options
  :prerequisites prerequisites
  :action maintain})
