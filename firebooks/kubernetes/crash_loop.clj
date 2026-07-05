;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns firebooks.kubernetes.crash-loop
  "Firebook: starts a deployment whose container exits immediately, producing
   a CrashLoopBackOff for the team to find during a drill."
  (:require [gumshoe.firebook :as firebook]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
            [gumshoe.runbook :as runbook]))

(def options
  {:extinguish {:desc "Put out the fire: delete the fire-drill namespace"
                :alias :e
                :coerce :boolean}})

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-create ["namespaces" "deployments"]
   :kubectl-can-delete ["namespaces"]})

(defn- crash-loop
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)]
    (if (:extinguish opts)
      (firebook/extinguish! context)
      (flow/change!
       {:confirmation {:action "start a fire drill: a crash-looping deployment"
                       :target cluster
                       :items [(str firebook/drill-namespace "/crash-loop")]}
        :announce! #(announce/announce! cluster announcement-data
                                                     "Fire drill started: crash-loop")
        :execute! #(firebook/ignite! context
                                     [(firebook/deployment-manifest
                                       {:name "crash-loop"
                                        :image "docker.io/library/busybox:stable"
                                        :command ["sh" "-c" "echo 'I am the fire 🔥' && exit 1"]})])
        :post-checks [(firebook/burning-check context "deployments" "crash-loop")]}))))

(runbook/execute!
 {:description "Firebook: starts a crash-looping deployment for the team to find"
  :options options
  :prerequisites prerequisites
  :action crash-loop})
