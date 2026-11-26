;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.promote-status-rights-test
  "A promotion writes the Cluster's status subresource, so switchover and
   cnpg_console --promote refuse, before the confirmation, a user who may
   not patch clusters/status in the cluster's namespace."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cnpg.operations :as operations]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]))

(deftest the-status-subresource-is-asked-for-in-the-cluster-namespace
  (let [argv (atom nil)]
    (with-redefs [shell/execute (fn [& args] (reset! argv (vec args)) {:exit 0 :out "" :err ""})]
      (is (true? (operations/can-patch-status? "ctx" "db"))))
    (is (= ["kubectl" "--context=ctx" "auth" "can-i" "patch" "clusters.postgresql.cnpg.io"
            "--subresource=status" "--namespace=db" "--quiet"]
           @argv)))
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"})]
    (is (false? (operations/can-patch-status? "ctx" "db")) "an unanswered check is no")))

(deftest a-user-without-status-rights-is-refused
  (is (nil? (operations/status-rights-refusal (constantly true) "db")))
  (is (str/includes? (operations/status-rights-refusal (constantly false) "db") "status.targetPrimary")))

(def ^:private cluster
  {:metadata {:namespace "db" :name "pg"}
   :status {:currentPrimary "pg-1" :targetPrimary "pg-1" :phase "Cluster in healthy state"
            :instancesStatus {:healthy ["pg-1" "pg-2"]}}})

(deftest switchover-refuses-without-status-rights
  (let [{:keys [refusal]} (with-redefs [mutation/book identity] (load-file "tools/cnpg/runbooks/switchover.clj"))
        ctx {:context "ctx" :ref {:namespace "db" :cluster "pg" :name "pg" :instance "pg-2"} :object cluster}]
    (testing "rights granted"
      (with-redefs [shell/execute (fn [& _] {:exit 0 :out "" :err ""})]
        (is (nil? (refusal ctx)))))
    (testing "rights refused"
      (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err ""})]
        (is (str/includes? (refusal ctx) "clusters.postgresql.cnpg.io/status"))))
    (testing "a switchover refused for its own reason does not ask"
      (with-redefs [shell/execute (fn [& _] (throw (ex-info "asked" {})))]
        (is (str/includes? (refusal (assoc-in ctx [:ref :instance] "pg-1")) "primary already"))))))
