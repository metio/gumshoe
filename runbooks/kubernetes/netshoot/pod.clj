;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.netshoot.pod
  "Starts an interactive netshoot debug session next to a running pod."
  (:require [infra.effect :as effect]
            [infra.flow :as flow]
            [infra.interact :as interact]
            [infra.kubectl :as kubectl]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]))

(def options
  {:namespace {:desc "The namespace of the pod - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :pod {:desc "The pod to debug - interactive selection when omitted"
         :alias :p
         :coerce :string}})

(def prerequisites
  {:installed-tools ["kubectl" "kubectl-netshoot" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]})

(defn- debug-pod
  [opts _ctx]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        pods (kubectl/namespaces-names (kubectl/get-all context "pods"))
        target (interact/choose-namespaced "Pod" pods (:namespace opts) (:pod opts))]
    (if (nil? target)
      (do (stdout/error "no pod selected") false)
      (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
        (flow/change!
         {:confirmation {:action "attach a netshoot debug container to a running pod"
                         :target cluster
                         :items [target]}
          :effect (effect/plan (effect/kubectl context "netshoot" "debug" name
                                               "--namespace" namespace))})))))

(runbook/execute!
 {:description "Starts an interactive netshoot debug session next to a running pod"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action debug-pod})
