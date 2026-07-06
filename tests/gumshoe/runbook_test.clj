;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.runbook-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gumshoe.config :as config]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.runbook :as runbook]))

;; The prerequisites registry is a process-wide atom; reset it around each test.
(use-fixtures :each (fn [t]
                      (reset! @#'prerequisites/checks {})
                      (t)
                      (reset! @#'prerequisites/checks {})))

(def ^:private unknown-items #'runbook/unknown-prerequisite-items)
(def ^:private book-heading #'runbook/book-heading)
(def ^:private can-i-item #'runbook/can-i-item)

(deftest book-heading-test
  (testing "the kind comes from the book's path segment, not a fixed 'Runbook'"
    (is (= "📋 Playbook · node maintenance"
           (book-heading "/x/playbooks/kubernetes/node_maintenance.clj" "node maintenance")))
    (is (= "🔥 Firebook · simulate an ImagePullBackOff"
           (book-heading "/x/firebooks/kubernetes/image_pull.clj" "simulate an ImagePullBackOff")))
    (is (= "🚀 Runbook · cordon a node"
           (book-heading "/x/runbooks/kubernetes/nodes/cordon.clj" "cordon a node")))
    (is (= "📋 Playbook · ceph upgrade"
           (book-heading "/x/tools/ceph/playbooks/upgrade.clj" "ceph upgrade"))
        "a tool package's playbook is still a Playbook"))
  (testing "a blank or missing description leaves just the kind; a missing path is a runbook"
    (is (= "🔥 Firebook" (book-heading "/x/firebooks/a.clj" nil)))
    (is (= "🚀 Runbook" (book-heading nil "")))))
(def ^:private cluster-item #'runbook/cluster-item)

(defn- run-thunk [[_heading thunk]] (thunk))

(deftest kubectl-permission-label-test
  (testing "a resource the cluster knows renders as its Kind, verb upper-cased and Kubernetes-scoped"
    (with-redefs [kubectl/resource-kind {"pods" "Pod"}
                  kubectl/can-i? (constantly true)]
      (let [[heading _] (can-i-item "get" "pods")]
        (is (= "Kubernetes: can GET Pod resources" heading))
        (is (= {:ok? true :label "Kubernetes: can GET Pod resources"}
               (run-thunk (can-i-item "get" "pods"))))))
    (with-redefs [kubectl/resource-kind {"nodes" "Node"}
                  kubectl/can-i? (constantly false)]
      (is (= {:ok? false :label "Kubernetes: cannot PATCH Node resources"}
             (run-thunk (can-i-item "patch" "nodes"))))))
  (testing "an unknown or unreachable resource falls back to the verbatim name, never a fabricated Kind"
    (with-redefs [kubectl/resource-kind (constantly nil)
                  kubectl/can-i? (constantly true)]
      (is (= "Kubernetes: can GET helmreleases.helm.toolkit.fluxcd.io"
             (first (can-i-item "get" "helmreleases.helm.toolkit.fluxcd.io")))))))

(deftest cluster-label-test
  (testing "no context: names Kubernetes explicitly"
    (with-redefs [kubectl/current-cluster (constantly "")]
      (is (= {:ok? false :label "not connected to any Kubernetes cluster"}
             (run-thunk (cluster-item []))))))
  (testing "a suitable cluster: 'Kubernetes cluster: <name>'"
    (with-redefs [kubectl/current-cluster (constantly "kube.infra.run")
                  config/known-clusters (constantly [])
                  config/env-value (constantly nil)]
      (is (= "connected to a suitable Kubernetes cluster" (first (cluster-item []))))
      (is (= {:ok? true :label "Kubernetes cluster: kube.infra.run"}
             (run-thunk (cluster-item [])))))))

(deftest unknown-prerequisite-fails-closed-test
  (testing "a built-in prerequisite key is not flagged as unknown"
    (is (empty? (unknown-items {:installed-tools ["kubectl"]
                                :kubectl-can-get ["pods"]
                                :can-ping-using-ipv4 [:host]}))))
  (testing "a plugin-registered key is not flagged as unknown"
    (prerequisites/register-check! :change-window (fn [_ _] []))
    (is (empty? (unknown-items {:change-window "prod"}))))
  (testing "a declared key nothing handles becomes a fail-closed checklist item"
    (let [items (unknown-items {:no-such-prerequisite "x"})]
      (is (= 1 (count items)) "no built-in or plugin handles this key, so it is unknown")
      (is (false? (:ok? ((second (first items)))))
          "the unknown key blocks the book instead of being silently dropped"))))
