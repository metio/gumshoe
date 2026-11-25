;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.etcd.check-performance
  "Runs an etcd performance check through one etcd pod. The check generates
   load on the cluster, so it asks for confirmation first."
  (:require [gumshoe.etcd :as etcd]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]))

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
        target (select/pick etcd/pod-select opts context)
        workload (:workload opts)]
    (cond
      (nil? target)
      false

      (not (interact/confirm! {:action (format "run an etcd performance check with workload '%s' - this generates load" workload)
                               :target cluster
                               :items [target]}))
      false

      :else
      (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
        (stdout/print-section-marker)
        (etcd/etcdctl! context namespace name "check" "perf" (str "--load=" workload))))))

(runbook/execute!
 {:description "Runs an etcd performance check through one etcd pod"
  :options options
  :prerequisites etcd/pod-prerequisites
  :announce? false
  :action check-performance})
