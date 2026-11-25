;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps-nodes-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]
            [gumshoe.steps.nodes :as nodes]))

(deftest the-maintenance-recovery-names-the-state-the-node-is-left-in
  (doseq [[why state expected]
          [["an unanswered read says the state is unknown and how to read it"
            {:unread "the read of node n1 did not answer"}
            ["The cluster did not answer (the read of node n1 did not answer), so whether node n1 is still cordoned is unknown. Read it before leaving it - a cordoned node takes no new pods: kubectl --context=ctx get node n1"]]
           ["a schedulable node is left in service"
            {:unschedulable? false :evictable ["shop/web-1"]}
            ["Node n1 is schedulable: nothing this run did keeps it out of service."]]
           ["a cordoned node names the pods a drain still evicts and the uncordon"
            {:unschedulable? true :evictable ["shop/web-1" "shop/api-1"]}
            ["Node n1 stays cordoned: nothing new is scheduled on it."
             "It still runs 2 pod(s) a drain evicts: shop/web-1, shop/api-1"
             "Bring it back into service once it is fit to run pods: kubectl --context=ctx uncordon n1"]]
           ["a drained cordoned node"
            {:unschedulable? true :evictable []}
            ["Node n1 stays cordoned: nothing new is scheduled on it."
             "No pod a drain evicts runs on it any more."
             "Bring it back into service once it is fit to run pods: kubectl --context=ctx uncordon n1"]]
           ["an unanswered pod read on a cordoned node"
            {:unschedulable? true :evictable nil}
            ["Node n1 stays cordoned: nothing new is scheduled on it."
             "Whether pods a drain evicts still run on it is unknown: the pod read did not answer."
             "Bring it back into service once it is fit to run pods: kubectl --context=ctx uncordon n1"]]]]
    (testing why
      (is (= expected (str/split-lines (nodes/maintenance-recovery-text "ctx" "n1" state)))))))

(defn- answering
  "A shell/execute answering a node read with `node` and a pod read with
   `pods`; nil for either makes that read fail."
  [node pods]
  (fn [& argv]
    (let [doc (if (some #{"nodes"} argv) node pods)]
      (if doc
        {:exit 0 :out (json/generate-string doc) :err ""}
        {:exit 1 :out "" :err "Unable to connect to the server"}))))

(def ^:private pods
  {:items [{:metadata {:namespace "shop" :name "web-1" :ownerReferences [{:kind "ReplicaSet"}]}}
           {:metadata {:namespace "kube-system" :name "agent-1" :ownerReferences [{:kind "DaemonSet"}]}}]})

(deftest the-node-state-is-read-at-the-failure-without-throwing
  (doseq [[why node pod-list expected]
          [["a cordoned node with an evictable pod" {:spec {:unschedulable true}} pods
            {:unschedulable? true :evictable ["shop/web-1"]}]
           ["a schedulable node" {:spec {}} {:items []}
            {:unschedulable? false :evictable []}]
           ["a pod read that does not answer" {:spec {:unschedulable true}} nil
            {:unschedulable? true :evictable nil}]
           ["a node read that does not answer" nil pods
            {:unread "the read of node n1 did not answer"}]]]
    (testing why
      (with-redefs [shell/execute (answering node pod-list)]
        (binding [kubectl/*strict-reads* true]
          (is (= expected (nodes/node-state "ctx" "n1"))))))))

(deftest each-step-guidance-names-the-node
  (doseq [f [nodes/cordon-recovery nodes/drain-recovery nodes/uncordon-recovery nodes/verification-recovery]]
    (is (str/includes? (f {:target "n1"}) "node n1"))))

(deftest the-shared-node-steps-carry-no-recovery
  (testing "the cordon, drain and uncordon runbooks print what they always printed"
    (is (not-any? :recovery [nodes/cordon nodes/drain nodes/uncordon]))))
