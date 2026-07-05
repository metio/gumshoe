;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.etcd.check-performance
  "Runs an etcd performance check through one etcd pod. The check generates
   load on the cluster, so it asks for confirmation first."
  (:require [infra.etcd :as etcd]
            [infra.interact :as interact]
            [infra.kubectl :as kubectl]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]))

(def workloads #{"s" "m" "l" "xl"})

(def options
  {:namespace {:desc "The namespace of the etcd pod - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :pod {:desc "The etcd pod to use - interactive selection when omitted"
         :alias :p
         :coerce :string}
   :workload {:desc "The workload model: s(mall), m(edium), l(arge), xl"
              :alias :l
              :default "s"
              :validate workloads
              :coerce :string}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]
   :kubectl-can-exec ["pods"]})

(defn- check-performance
  [opts _ctx]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        pods (kubectl/namespaces-names (kubectl/get-selected context "pods" etcd/pod-selector))
        target (interact/choose-namespaced "Pod" pods (:namespace opts) (:pod opts))
        workload (:workload opts)]
    (cond
      (nil? target)
      (do (stdout/error "no etcd pod selected") false)

      (not (interact/confirm! {:action (format "run an etcd performance check with workload '%s' - this generates load" workload)
                               :target cluster
                               :items [target]}))
      false

      :else
      (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
        (stdout/print-section-marker)
        (etcd/etcdctl! context namespace name "check" "perf" (str "--load=" workload))))))

(runbook/execute!
 {:description "Runs an etcd performance check through one etcd pod"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action check-performance})
