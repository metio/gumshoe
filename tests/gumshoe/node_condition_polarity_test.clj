;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.node-condition-polarity-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.nodes :as nodes]))

(defn- node
  [name & conditions]
  {:metadata {:name name}
   :status {:conditions (vec (for [[type status] conditions]
                               {:type type :status status :message (str type " says " status)
                                :lastTransitionTime "2026-11-26T10:00:00Z"}))}})

(defn- summaries
  [& nodes]
  (set (for [finding (nodes/detect-node-problems {"nodes" {:items (vec nodes)}})]
         [(:component finding) (:summary finding)])))

(def ^:private eks-healthy
  "A node as the EKS node monitoring agent reports it when every subsystem works."
  (node "healthy" ["Ready" "True"] ["KernelReady" "True"] ["NetworkingReady" "True"]
        ["StorageReady" "True"] ["ContainerRuntimeReady" "True"] ["AcceleratedHardwareReady" "True"]
        ["MemoryPressure" "False"] ["DiskPressure" "False"] ["PIDPressure" "False"]))

(deftest the-eks-agent-conditions-are-healthy-when-true
  (is (= #{} (summaries eks-healthy)) "a healthy EKS node raises nothing"))

(deftest a-false-eks-condition-is-a-problem
  (let [broken (node "broken" ["Ready" "True"] ["KernelReady" "True"] ["StorageReady" "False"]
                     ["AcceleratedHardwareReady" "False"])]
    (is (= #{["broken" "StorageReady is False"] ["broken" "AcceleratedHardwareReady is False"]}
           (summaries eks-healthy broken)))
    (is (= :warning (:severity (first (nodes/detect-node-problems {"nodes" {:items [broken]}})))))))

(deftest a-pressure-is-a-problem-when-true
  (is (= #{["n" "MemoryPressure is active"] ["n" "DiskPressure is active"]
           ["n" "PIDPressure is active"] ["n" "NetworkUnavailable is active"]}
         (summaries (node "n" ["Ready" "True"] ["MemoryPressure" "True"] ["DiskPressure" "True"]
                          ["PIDPressure" "True"] ["NetworkUnavailable" "True"]))))
  (is (= #{} (summaries (node "n" ["Ready" "True"] ["NetworkUnavailable" "False"])))))

(deftest an-unknown-type-is-read-by-its-name
  (testing "one ending in Ready follows Ready's convention"
    (is (= :healthy-when-true (nodes/condition-polarity "GPUDriverReady")))
    (is (= #{} (summaries (node "n" ["Ready" "True"] ["GPUDriverReady" "True"]))))
    (is (= #{["n" "GPUDriverReady is False"]} (summaries (node "n" ["Ready" "True"] ["GPUDriverReady" "False"])))))
  (testing "anything else follows the node-problem-detector convention"
    (is (= :problem-when-true (nodes/condition-polarity "KernelDeadlock")))
    (is (= #{["n" "KernelDeadlock is active"]} (summaries (node "n" ["Ready" "True"] ["KernelDeadlock" "True"]))))
    (is (= #{} (summaries (node "n" ["Ready" "True"] ["KernelDeadlock" "False"]))))))

(deftest an-unknown-status-is-a-warning-either-way
  (is (= #{["n" "StorageReady is Unknown"] ["n" "DiskPressure is Unknown"]}
         (summaries (node "n" ["Ready" "True"] ["StorageReady" "Unknown"] ["DiskPressure" "Unknown"])))))

(deftest a-node-that-is-not-ready-does-not-repeat-itself-as-unknowns
  (is (= #{["down" "node is not Ready (status Unknown)"] ["down" "StorageReady is False"]}
         (summaries (node "down" ["Ready" "Unknown"] ["MemoryPressure" "Unknown"] ["DiskPressure" "Unknown"]
                          ["StorageReady" "False"])))
      "a kubelet that stopped posting turns every condition Unknown; the critical finding already says so"))
