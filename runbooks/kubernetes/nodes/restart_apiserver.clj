;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.nodes.restart-apiserver
  "Restarts kube-apiserver on one control-plane node by stopping its container
   over SSH, so the kubelet starts a replacement. One node per run, on purpose:
   the API stays available through the other control-plane nodes, and a node that
   does not come back has to be fixed before the next one is touched."
  (:require [gumshoe.apiserver :as apiserver]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]))

(def options
  {:node {:desc "The control-plane node to restart kube-apiserver on - interactive selection when omitted"
          :alias :n
          :coerce :string}
   :user {:desc "The SSH user - your ssh config decides when omitted"
          :alias :u
          :coerce :string}})

(defn connection
  "The node's own name is the SSH destination; sudo is needed for crictl."
  [node opts]
  {:host node :user (:user opts) :needs-sudo? true})

(defn candidates
  [context]
  (apiserver/control-plane-nodes (kubectl/get-all context "nodes")))

(defn running-container
  "The container id to stop, or nil when none is running."
  [conn]
  (apiserver/container-id
   (apply ssh/stdout-of conn (apiserver/container-id-command))))

(defn ready-now?
  [conn]
  (apiserver/ready? (apply ssh/stdout-of conn (apiserver/readyz-command))))

(defn started-at
  [conn id]
  (apply ssh/stdout-of conn (apiserver/started-at-command id)))

(defn restart-effect
  [conn id]
  (effect/plan
   (apply effect/ssh conn (apiserver/configure-command))
   (apply effect/ssh conn (apiserver/stop-command id))))

(defn replaced-check
  "The restart is done when the container id has changed and the local apiserver
   answers /readyz again. Both matter: a new container that never becomes ready
   is a failed restart, and a ready answer from the old container is no restart
   at all."
  [conn before]
  {:description (format "kube-apiserver was replaced and answers /readyz (was %s)" before)
   :timeout 180
   :interval 5
   :check (fn []
            (and (apiserver/restarted? before (running-container conn))
                 (ready-now? conn)))})

(defn- derive-state
  "Resolved once, so the effect stops the container the checks compared against."
  [{:keys [target opts]}]
  (let [conn (connection target opts)]
    {:conn conn :before (running-container conn)}))

(mutation/book
 {:description "Restarts kube-apiserver on one control-plane node (stale discovery, renewed certificates)"
  :options options
  :prerequisites {:installed-tools ["kubectl" "fzf" "ssh"]
                  :cluster-capabilities []
                  :kubectl-can-get ["nodes"]}
  :select {:mode :one :label "Control-plane node" :flag :node :candidates candidates
           :preview "kubectl describe node {}"}
  :empty-message "no node carries the control-plane role label"
  :derive derive-state
  :precondition (fn [{:keys [conn before target]}]
                  (cond
                    (not (ssh/check-connection? conn)) false

                    (nil? before)
                    (do (stdout/error "no running kube-apiserver container on this node")
                        false)

                    (not (ready-now? conn))
                    (do (stdout/error "this node's apiserver does not answer /readyz - fix that before restarting it")
                        false)

                    :else
                    (do (stdout/print-data-table {:node target
                                                  :container before
                                                  :started-at (started-at conn before)})
                        true)))
  :confirm {:action "restart kube-apiserver on a control-plane node - the API on this node is briefly unavailable"
            :destructive? true}
  :announce (fn [{:keys [target]}] (format "Restart kube-apiserver on %s" target))
  :effect (fn [{:keys [conn before]}] (restart-effect conn before))
  :verify (fn [{:keys [conn before]}] [(replaced-check conn before)])})
