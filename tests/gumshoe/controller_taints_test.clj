;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.controller-taints-test
  "nodes/remove_taint never offers a taint the node lifecycle controller
   owns, and refuses one named on the command line with what removes it for
   good."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.taints :as taints]))

(def ^:private cordoned
  {:metadata {:name "n1"}
   :spec {:unschedulable true
          :taints [{:key "node.kubernetes.io/unschedulable" :effect "NoSchedule"}
                   {:key "dedicated" :value "db" :effect "NoSchedule"}]}})

(def ^:private not-ready
  {:metadata {:name "n2"}
   :spec {:taints [{:key "node.kubernetes.io/not-ready" :effect "NoExecute"}
                   {:key "node.kubernetes.io/disk-pressure" :effect "NoSchedule"}]}})

(deftest controller-owned-taints-name-their-remedy
  (doseq [key ["node.kubernetes.io/unschedulable" "node.kubernetes.io/not-ready" "node.kubernetes.io/unreachable"
               "node.kubernetes.io/memory-pressure" "node.kubernetes.io/disk-pressure"
               "node.kubernetes.io/pid-pressure" "node.kubernetes.io/network-unavailable"]]
    (testing key
      (is (str/includes? (taints/remedy (str key ":NoSchedule")) "node lifecycle controller"))))
  (is (str/includes? (taints/remedy "node.kubernetes.io/unschedulable:NoSchedule") "uncordon"))
  (is (nil? (taints/remedy "dedicated=db:NoSchedule")))
  (is (nil? (taints/remedy "node.kubernetes.io/out-of-service=nodeshutdown:NoExecute"))
      "an operator sets out-of-service, and removes it")
  (is (= "node.kubernetes.io/not-ready" (taints/taint-key "node.kubernetes.io/not-ready:NoExecute")))
  (is (= "dedicated" (taints/taint-key "dedicated=db:NoSchedule"))))

(deftest only-removable-taints-are-offered
  (is (= ["dedicated=db:NoSchedule"] (taints/removable cordoned)))
  (is (= [] (taints/removable not-ready)))
  (is (= ["n1"] (taints/nodes-with-removable-taints {:items [cordoned not-ready]}))))

(defn- run-book
  [nodes opts]
  (let [config (binding [runbook/*loading* true] (load-file "runbooks/kubernetes/nodes/remove_taint.clj"))
        seen (atom nil)
        out (java.io.StringWriter.)]
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out (json/generate-string {:items nodes}) :err ""})
                  kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  flow/change! (fn [config] (reset! seen config) true)]
      (let [result (binding [*out* out *err* out] ((:action config) opts {}))]
        {:result result :change @seen :out (stdout/strip-colors (str out))}))))

(deftest a-named-controller-taint-is-refused-with-the-remedy
  (let [{:keys [result change out]} (run-book [cordoned] {:node "n1" :taint "node.kubernetes.io/unschedulable:NoSchedule"})]
    (is (false? result))
    (is (nil? change) "nothing is confirmed or changed")
    (is (str/includes? out "refusing: taint node.kubernetes.io/unschedulable:NoSchedule is set by the node lifecycle controller"))
    (is (str/includes? out "uncordon the node"))))

(deftest a-removable-taint-still-reaches-the-change
  (let [{:keys [change]} (run-book [cordoned] {:node "n1" :taint "dedicated=db:NoSchedule"})]
    (is (= ["dedicated=db:NoSchedule"] (get-in change [:confirmation :items])))))

(deftest a-cluster-with-only-controller-taints-offers-nothing-and-says-why
  (let [{:keys [result change out]} (run-book [not-ready] {})]
    (is (true? result))
    (is (nil? change))
    (is (str/includes? out "no node carries a taint that can be removed by hand"))
    (is (str/includes? out "n2: taint node.kubernetes.io/not-ready:NoExecute is set by the node lifecycle controller"))))
