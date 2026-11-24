;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.check-reads-test
  "Post-checks that assert an absence or a negation, run through
   verify/eventually against a kubectl that does not answer. Each would read
   nil and hold if its reads were lax."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.cnpg.operations :as operations]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.steps.nodes :as nodes]
            [gumshoe.verify :as verify]
            [gumshoe.workloads :as workloads]))

;; Loading a book with *loading* bound defines its functions and runs nothing.
(binding [runbook/*loading* true]
  (run! load-file ["runbooks/kubernetes/nodes/remove_taint.clj"
                   "runbooks/kubernetes/orders/recreate.clj"
                   "tools/jaas/runbooks/jaas/resume.clj"]))

(defn- unanswered
  [& _]
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"})

(defn- verified?
  [check]
  (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
    (verify/eventually (assoc check :timeout 0 :interval 0))))

(defn- book-fn
  [sym]
  @(requiring-resolve sym))

(deftest absence-checks-do-not-hold-on-an-unanswered-read
  (with-redefs [shell/execute unanswered]
    (testing "uncordon: the node is schedulable"
      (is (false? (verified? (nodes/schedulable-check "ctx" "n1")))))
    (testing "remove-taint: the taint is gone"
      (is (false? (verified? ((book-fn 'runbooks.kubernetes.nodes.remove-taint/gone-check)
                              "ctx" "n1" "k=v:NoSchedule")))))
    (testing "orders/recreate: the invalid order is cleared"
      (is (false? (verified? (first ((book-fn 'runbooks.kubernetes.orders.recreate/cleared-checks)
                                     "ctx" ["web/order-1"]))))))
    (testing "jaas resume: the snippet is no longer suspended"
      (is (false? (verified? ((book-fn 'runbooks.jaas.resume/resumed-check) "ctx" "web" "site" "web/site")))))
    (testing "cronjobs/resume: the CronJob is resumed"
      (is (false? (verified? {:description "resumed"
                              :check #(= false (workloads/suspended?
                                                (kubectl/get-namespaced-resource "ctx" "web" "cronjobs" "nightly")))}))))
    (testing "flux resume: the object is no longer suspended"
      (is (false? (verified? {:description "resumed"
                              :check #(= false (true? (-> (kubectl/get-namespaced-resource "ctx" "flux-system" "kustomizations" "apps")
                                                          :spec :suspend)))}))))
    (testing "cnpg unfence: the instance is no longer fenced"
      (is (false? (verified? {:description "unfenced"
                              :check #(not (contains? (or (operations/fenced (kubectl/get-namespaced-resource "ctx" "db" "clusters.postgresql.cnpg.io" "pg"))
                                                          #{})
                                                      "pg-1"))}))))))

(deftest absence-checks-hold-on-not-found
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Error from server (NotFound): orders.acme.cert-manager.io \"order-1\" not found"})]
    (testing "orders/recreate: an order that is gone is cleared"
      (is (true? (verified? (first ((book-fn 'runbooks.kubernetes.orders.recreate/cleared-checks)
                                    "ctx" ["web/order-1"]))))))))
