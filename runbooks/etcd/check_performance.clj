;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.etcd.check-performance
  "Runs an etcd performance check through one etcd pod. The check generates
   load on the cluster, so it asks for confirmation first, and under --dry-run
   it shows the etcdctl command without running it."
  (:require [gumshoe.effect :as effect]
            [gumshoe.etcd :as etcd]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]))

(def workloads #{"s" "m" "l" "xl"})

(def options
  (merge etcd/pod-options
         {:workload {:desc "The workload model: s(mall), m(edium), l(arge), xl"
                     :alias :l
                     :default "s"
                     :validate workloads
                     :coerce :string}}))

(defn- check-performance
  [opts _ctx]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        workload (:workload opts)]
    (if-let [target (select/pick etcd/pod-select opts context)]
      (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
        (flow/change!
         {:confirmation {:action (format "run an etcd performance check with workload '%s' - this generates load" workload)
                         :target cluster
                         :items [target]}
          :effect (effect/plan
                   (apply effect/cmd (etcd/etcdctl-args context namespace name
                                                        ["check" "perf" (str "--load=" workload)])))}))
      false)))

(runbook/execute!
 {:description "Runs an etcd performance check through one etcd pod"
  :options options
  :prerequisites etcd/pod-prerequisites
  :announce? false
  :change? true
  :action check-performance})
