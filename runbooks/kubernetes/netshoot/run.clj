;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.netshoot.run
  "Starts a standalone interactive netshoot pod in a namespace."
  (:require [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def options
  {:namespace {:desc "The namespace to run netshoot in - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :host-network {:desc "Run netshoot in the node's network namespace"
                  :alias :r
                  :coerce :boolean}})

(def prerequisites
  {:installed-tools ["kubectl" "kubectl-netshoot" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["namespaces"]
   :kubectl-can-create ["pods"]})

(defn- run-netshoot
  [opts _ctx]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        namespaces (kubectl/names-of (kubectl/get-all context "namespaces"))
        namespace (interact/choose-one "Namespace" namespaces (:namespace opts))
        host-network (boolean (:host-network opts))]
    (if (nil? namespace)
      (do (stdout/error "no namespace selected") false)
      (flow/change!
       {:confirmation {:action (if host-network
                                 "start a netshoot pod ON THE NODE NETWORK - it sees all node traffic"
                                 "start a netshoot pod")
                       :target cluster
                       :items [(str namespace "/netshoot-shell")]}
        :effect (effect/plan (effect/kubectl context "netshoot" "run" "netshoot-shell"
                                             "--namespace" namespace
                                             (str "--host-network=" host-network)))}))))

(runbook/execute!
 {:description "Starts a standalone interactive netshoot pod in a namespace"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action run-netshoot})
