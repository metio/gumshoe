;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.served-prerequisites-test
  ":kubectl-can-get-if-served asks get on a type only some clusters serve
   and passes where discovery confirms the type is not served, and
   gumshoe.testing.prerequisites compares the kubectl reads a run performed
   with the get permissions a book declares."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.testing.prerequisites :as prereqs]))

(def ^:private crd "resourcesets.fluxcd.controlplane.io")

(defn- answering
  "A shell answering `auth can-i` with can-i? and the discovery of the
   fluxcd.controlplane.io group with group, recording every argv in seen."
  [seen can-i? group]
  (fn [& argv]
    (let [argv (vec argv)]
      (swap! seen conj argv)
      (cond
        (some #{"can-i"} argv) {:exit (if can-i? 0 1) :out "" :err ""}
        (some #{"/apis/fluxcd.controlplane.io"} argv) group
        :else {:exit 1 :out "" :err "unexpected"}))))

(def ^:private group-not-found
  {:exit 1 :out "" :err "Error from server (NotFound): the server could not find the requested resource"})

(def ^:private group-unanswered
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 192.0.2.1:6443: i/o timeout"})

(defn- checked
  "The label and verdict of the one item :kubectl-can-get-if-served builds
   for crd, checked on kube context ctx against shell, and the argvs it ran."
  [can-i? group]
  (let [seen (atom [])]
    (with-redefs [kubectl/resource-kind (constantly nil)
                  kubectl/current-context (constantly "ctx")
                  shell/execute (answering seen can-i? group)]
      (let [items (vec (#'runbook/prerequisite-items {:kubectl-can-get-if-served [crd]} {}))
            [[heading thunk]] items
            result (thunk)]
        (is (= 1 (count items)))
        (is (not-any? #(str/includes? (first %) "unknown prerequisite") items)
            ":kubectl-can-get-if-served is a built-in key")
        {:heading heading :result result :argvs @seen}))))

(deftest a-granted-get-passes-without-asking-discovery
  (let [{:keys [heading result argvs]} (checked true group-not-found)]
    (is (= (str "Kubernetes: can GET " crd " where served") heading))
    (is (= {:ok? true :label (str "Kubernetes: can GET " crd)} result))
    (is (= [["kubectl" "auth" "can-i" "get" crd "--all-namespaces" "--quiet"]] argvs))))

(deftest a-denied-get-of-a-type-the-cluster-does-not-serve-passes
  (let [{:keys [result argvs]} (checked false group-not-found)]
    (is (= {:ok? true :label (str "Kubernetes: " crd " not served - nothing to GET")} result))
    (is (some #(some #{"/apis/fluxcd.controlplane.io"} %) argvs) "discovery decided it")))

(deftest a-denied-get-of-a-served-type-fails
  (let [served {:exit 0 :out "{\"versions\":[{\"version\":\"v1\"}]}" :err ""}
        seen (atom [])]
    (with-redefs [kubectl/resource-kind (constantly nil)
                  kubectl/current-context (constantly "ctx")
                  shell/execute (fn [& argv]
                                  (let [argv (vec argv)]
                                    (swap! seen conj argv)
                                    (cond
                                      (some #{"can-i"} argv) {:exit 1 :out "" :err ""}
                                      (some #{"/apis/fluxcd.controlplane.io"} argv) served
                                      (some #{"/apis/fluxcd.controlplane.io/v1"} argv)
                                      {:exit 0 :out "{\"resources\":[{\"name\":\"resourcesets\",\"kind\":\"ResourceSet\"}]}" :err ""}
                                      :else {:exit 1 :out "" :err "unexpected"})))]
      (let [[[_ thunk]] (#'runbook/prerequisite-items {:kubectl-can-get-if-served [crd]} {})]
        (is (= {:ok? false :label (str "Kubernetes: cannot GET " crd)} (thunk)))))))

(deftest a-denied-get-whose-discovery-does-not-answer-fails
  (is (= {:ok? false :label (str "Kubernetes: cannot GET " crd)}
         (:result (checked false group-unanswered)))
      "an unanswered discovery is no confirmation that the type is absent"))

(deftest the-summary-and-the-permission-checks-name-it
  (is (some #{(str "kubectl get, where the cluster serves it: " crd)}
            (prerequisites/summary {:kubectl-can-get-if-served [crd]})))
  (is (= [["get" "secrets"] ["get" crd]]
         (runbook/permission-checks {:kubectl-can-get ["secrets"] :kubectl-can-get-if-served [crd]}))))

;; --- gumshoe.testing.prerequisites ---------------------------------------------------

(def ^:private argvs
  [["kubectl" "--context=ctx" "--request-timeout=30s" "get" "secrets" "sh.helm.release.v1.wiki.v2"
    "--namespace=wiki" "--ignore-not-found" "--output=jsonpath={.data.release}"]
   ["kubectl" "--context=ctx" "get" crd "--all-namespaces" "--output=json"]
   ["kubectl" "--context=ctx" "get" "--raw" "/apis/fluxcd.controlplane.io"]
   ["kubectl" "--context=ctx" "auth" "can-i" "get" "nodes" "--all-namespaces" "--quiet"]
   ["kubectl" "--context=ctx" "--namespace" "wiki" "get" "configmap/settings,deployment.apps" "-o" "json"]
   ["kubectl" "--context=ctx" "--namespace=wiki" "patch" "helmreleases.helm.toolkit.fluxcd.io" "wiki"]])

(deftest the-reads-are-the-types-of-each-get
  (is (= #{"secrets" crd "configmap" "deployment.apps"} (prereqs/kubectl-reads argvs))
      "discovery, can-i and writes are not reads of a type"))

(deftest a-read-no-prerequisite-declares-is-named
  (is (= ["configmap" "deployment.apps" "secrets"]
         (prereqs/undeclared-reads {:kubectl-can-get-if-served [crd]} argvs)))
  (is (= ["deployment.apps"]
         (prereqs/undeclared-reads {:kubectl-can-get ["secrets"] :kubectl-can-get-if-served [crd]}
                                   argvs #{"configmap"})))
  (testing "negative control: a type declared for another verb is still undeclared"
    (is (= [crd] (prereqs/undeclared-reads {:kubectl-can-get ["secrets" "configmap" "deployment.apps"]
                                            :kubectl-can-patch [crd]}
                                           argvs)))))
