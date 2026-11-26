;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps.nodes
  "The steps that change a node's scheduling: cordon, drain and uncordon.

   Each step is a mutation spec (see gumshoe.mutation): its runbook runs it on
   its own, and a playbook composes it with others (see gumshoe.playbook). Each
   step names the node as its :subject, so the engine reads it again after the
   confirmation and refuses a node that was removed or replaced meanwhile, or
   whose spec moved (node-subject). None
   refuses a node already in the state it brings about: cordon, drain and
   uncordon are idempotent, and the maintenance playbook runs again over a node
   a failed run left cordoned. Each is :done when its post-checks already
   hold (see gumshoe.mutation), so that run skips the cordon, and the drain
   of a node no evictable pod is left on, instead of applying them again. A step
   declares the prerequisites and options it needs, so whatever runs it derives
   its own from the steps. The book that runs a step names its :description,
   where the catalog reads it without loading the book. The bespoke parts - candidates, effect, checks - are
   small named functions, tested without a cluster.

   The recovery functions say what a failed maintenance run leaves on a
   node. The steps themselves carry none, so the runbooks print what they
   always printed; a playbook attaches them, see
   playbooks/kubernetes/node_maintenance.clj."
  (:require [clojure.string :as str]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.select :as select]
            [gumshoe.verify :as verify]))

(def ^:private node-prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["nodes"]
   :kubectl-can-patch ["nodes"]})

(def ^:private node-select
  "What every step's select shares; each adds the candidates it offers."
  {:mode :one :label "Node" :flag :node})

(defn- node-option
  [verb]
  (select/options node-select {:node {:desc (format "The node to %s - interactive selection when omitted" verb)}}))

(defn schedulable-nodes
  [context]
  (kubectl/schedulable-nodes (kubectl/get-all context "nodes")))

(defn cordoned-nodes
  [context]
  (kubectl/unschedulable-nodes (kubectl/get-all context "nodes")))

(defn all-nodes
  [context]
  (kubectl/names-of (kubectl/get-all context "nodes")))

(defn- node-holds?
  "Whether the node exists and (holds? node) is true for it, read now."
  [context node holds?]
  (verify/object-holds? (kubectl/get-cluster-resource context "nodes" node) holds?))

(defn- cordoned?
  [node]
  (true? (-> node :spec :unschedulable)))

(def node-subject
  "The node a step acts on. A Node carries no metadata.generation and the
   kubelet writes its status every few seconds, so its pin compares the
   node's uid and spec (see gumshoe.change/comparisons): a heartbeat during
   the confirmation leaves it alone, a taint or cordon added meanwhile
   refuses the step."
  {:type "nodes" :compare :spec})

(defn node-refusal
  "Why a step must not act on the node named `name`, judged on the node as
   read after the confirmation: it no longer exists."
  [name node]
  (when (nil? node) (format "node %s no longer exists" name)))

(defn- refusal
  [{:keys [target object]}]
  (node-refusal target object))

(defn cordon-effect
  [context node]
  (effect/plan (effect/kubectl context "cordon" node)))

(defn unschedulable-check
  [context node]
  {:description (format "node %s is unschedulable" node)
   :check (fn [] (node-holds? context node cordoned?))})

(defn schedulable-check
  [context node]
  {:description (format "node %s is schedulable" node)
   :check (fn [] (node-holds? context node (complement cordoned?)))})

(def default-drain-timeout
  "Seconds a drain waits for its evictions. kubectl's own default (0) waits
   forever, retrying an eviction a PodDisruptionBudget refuses every few
   seconds, so the step would never fail and never print its recovery."
  300)

(defn drain-effect
  "Drains node, giving each pod grace-period seconds and the whole drain
   timeout seconds (default-drain-timeout when nil) before kubectl gives up
   with a non-zero exit."
  ([context node grace-period] (drain-effect context node grace-period nil))
  ([context node grace-period timeout]
   (effect/plan (effect/kubectl context "drain" node
                                "--ignore-daemonsets" "--delete-emptydir-data"
                                (str "--grace-period=" grace-period)
                                (str "--timeout=" (or timeout default-drain-timeout) "s")))))

(defn drain-checks
  [context node]
  [(unschedulable-check context node)
   {:description (format "no evictable pods remain on node %s" node)
    :timeout 120 :interval 10 :verifies-absence? true
    ;; only a read that answered can show the node empty; a failed one
    ;; would read as no pods and verify a drain that did not happen
    :check (fn [] (let [{:keys [ok? doc]} (kubectl/get-json-result context "pods" "--all-namespaces"
                                                                   (str "--field-selector=spec.nodeName=" node))]
                    (and ok? (empty? (kubectl/drainable-pods doc)))))}])

(def cordon
  {:options (node-option "cordon")
   :prerequisites node-prerequisites
   :select (assoc node-select :candidates schedulable-nodes :preview "kubectl describe node {}")
   :empty-message "every node is already cordoned"
   :confirm {:action "cordon (mark unschedulable) a node" :destructive? false}
   :subject node-subject
   :refusal refusal
   :done true
   :announce (fn [{:keys [target]}] (format "Cordon node %s" target))
   :effect (fn [{:keys [context target]}] (cordon-effect context target))
   :verify (fn [{:keys [context target]}] [(unschedulable-check context target)])})

(def drain
  {:options (merge (node-option "drain")
                   {:grace-period {:desc "Seconds each pod is given to terminate - the pod's own value when omitted"
                                   :alias :g
                                   :default -1
                                   :coerce :long}
                    :drain-timeout {:desc "Seconds the drain waits for its evictions before it gives up and fails - 0 waits forever"
                                    :default default-drain-timeout
                                    :coerce :long}})
   :prerequisites (merge node-prerequisites
                         {:kubectl-can-get ["nodes" "pods"]
                          :kubectl-can-delete ["pods"]})
   :select (assoc node-select :candidates all-nodes)
   :confirm {:action "drain a node - every pod on it is evicted and emptyDir data is deleted"
             :destructive? true}
   :subject node-subject
   :refusal refusal
   :done true
   :announce (fn [{:keys [target]}] (format "Drain node %s" target))
   ;; -1 lets each pod keep its own terminationGracePeriodSeconds
   :effect (fn [{:keys [context target opts]}] (drain-effect context target (:grace-period opts) (:drain-timeout opts)))
   :verify (fn [{:keys [context target]}] (drain-checks context target))})

(def uncordon
  {:options (node-option "uncordon")
   :prerequisites node-prerequisites
   :select (assoc node-select :candidates cordoned-nodes)
   :empty-message "no node is cordoned"
   :confirm {:action "uncordon (mark schedulable) a node" :destructive? false}
   :subject node-subject
   :refusal refusal
   :done true
   :announce (fn [{:keys [target]}] (format "Uncordon node %s" target))
   :effect (fn [{:keys [context target]}] (effect/plan (effect/kubectl context "uncordon" target)))
   :verify (fn [{:keys [context target]}] [(schedulable-check context target)])})

;; --- recovery ------------------------------------------------------------------------

(defn node-state
  "The node's state at a failure, read without strict reads so an unanswered
   read is reported rather than thrown: {:unschedulable? .. :evictable
   [\"ns/name\" ..]}, :evictable nil when the pods could not be read,
   {:gone true} when the cluster answered that the node no longer exists (an
   autoscaler or an operator removed it during the run), or {:unread \"why\"}
   when the node could not be read. A failed get alone does not tell a
   deleted node from a read that did not answer, so a presence read decides."
  [context node]
  (binding [kubectl/*strict-reads* false]
    (let [{node-ok? :ok? node-doc :doc} (kubectl/get-json-result context "nodes" node)]
      (if-not node-ok?
        (if (= :absent (kubectl/presence context {:type "nodes" :name node}))
          {:gone true}
          {:unread (format "the read of node %s did not answer" node)})
        (let [{:keys [ok? doc]} (kubectl/get-json-result context "pods" "--all-namespaces"
                                                         (str "--field-selector=spec.nodeName=" node))]
          {:unschedulable? (true? (-> node-doc :spec :unschedulable))
           :evictable (when ok? (mapv kubectl/namespace-name-of (kubectl/drainable-pods doc)))})))))

(defn maintenance-recovery-text
  "What a failed maintenance run leaves on `node`, from `state` as node-state
   reads it: whether it stays cordoned, the pods a drain still evicts from
   it, and the command that brings it back into service."
  [context node {:keys [gone unread unschedulable? evictable]}]
  (cond
    gone
    (str "Node " node " no longer exists: the cluster answered that it is gone, so nothing is left to uncordon."
         " Its pods were rescheduled elsewhere or went with it - check the workloads that ran there.")

    unread
    (str "The cluster did not answer (" unread "), so whether node " node " is still cordoned is unknown."
         " Read it before leaving it - a cordoned node takes no new pods: kubectl --context=" context " get node " node)

    (not unschedulable?)
    (str "Node " node " is schedulable: nothing this run did keeps it out of service.")

    :else
    (str/join "\n"
              [(str "Node " node " stays cordoned: nothing new is scheduled on it.")
               (cond
                 (nil? evictable) "Whether pods a drain evicts still run on it is unknown: the pod read did not answer."
                 (empty? evictable) "No pod a drain evicts runs on it any more."
                 :else (str "It still runs " (count evictable) " pod(s) a drain evicts: " (str/join ", " evictable)))
               (str "Bring it back into service once it is fit to run pods: kubectl --context=" context " uncordon " node)])))

(defn maintenance-recovery
  "The standing :recovery of a playbook that takes a node out of service: it
   reads the node again at the failure."
  [{:keys [context target]}]
  (maintenance-recovery-text context target (node-state context target)))

(defn cordon-recovery
  [{:keys [target]}]
  (format "Cordoning node %s did not complete or did not hold; no pod was evicted from it." target))

(defn drain-recovery
  [{:keys [target]}]
  (str "The drain stopped before every evictable pod left node " target ". The pods it evicted are gone"
       " from the node: their controllers recreate them on other nodes, a pod without a controller is"
       " not recreated, and the emptyDir data of an evicted pod is deleted. A PodDisruptionBudget that"
       " allows no disruption blocks a drain; the disruption check above names those. Remove the blocker"
       " and run the playbook again, or bring the node back as below."))

(defn uncordon-recovery
  [{:keys [target]}]
  (format "Uncordoning node %s after the maintenance did not complete or did not hold." target))

(defn verification-recovery
  [{:keys [target]}]
  (format "The detectives found issues after node %s came back; the findings above name them. Read them before taking another node out of service." target))
