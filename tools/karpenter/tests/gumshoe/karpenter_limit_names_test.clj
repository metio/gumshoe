;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.karpenter-limit-names-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.detectives.karpenter :as karpenter]))

(defn- pool
  [limits resources]
  {:metadata {:name "gpu"}
   :spec {:limits limits}
   :status {:conditions [{:type "Ready" :status "True"}] :resources resources}})

(deftest an-extended-resource-keeps-its-prefix
  (let [nodepool (pool {:cpu "100" :nvidia.com/gpu "8" :aws.amazon.com/neuron "2"}
                       {:cpu "10" :nvidia.com/gpu "8" :aws.amazon.com/neuron "2"})]
    (is (= [["aws.amazon.com/neuron" "2" "2"] ["nvidia.com/gpu" "8" "8"]]
           (vec (karpenter/reached-limits nodepool))))
    (is (= #{"NodePool reached its nvidia.com/gpu limit (8 of 8)"
             "NodePool reached its aws.amazon.com/neuron limit (2 of 2)"}
           (set (map :summary (karpenter/nodepool-findings nodepool)))))))

(deftest a-plain-resource-reads-as-before
  (is (= [["cpu" "100" "100"]] (vec (karpenter/reached-limits (pool {:cpu "100"} {:cpu "100"}))))))
