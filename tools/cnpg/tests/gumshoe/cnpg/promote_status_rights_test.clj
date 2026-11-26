;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.promote-status-rights-test
  "A promotion writes the Cluster's status subresource: switchover declares
   patch on clusters/status as a prerequisite, and cnpg_console --promote,
   whose prerequisites also cover read-only runs, refuses before its
   confirmation a user who may not patch it in the cluster's namespace."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cnpg.operations :as operations]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]))

(deftest the-status-subresource-is-asked-for-in-the-cluster-namespace
  (let [argv (atom nil)]
    (with-redefs [shell/execute (fn [& args] (reset! argv (vec args)) {:exit 0 :out "" :err ""})]
      (is (true? (operations/can-patch-status? "ctx" "db"))))
    (is (= ["kubectl" "--context=ctx"] (take 2 @argv)))
    (is (= ["auth" "can-i" "patch" "clusters.postgresql.cnpg.io" "--subresource=status" "--namespace=db" "--quiet"]
           (take-last 7 @argv))))
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"})]
    (is (false? (operations/can-patch-status? "ctx" "db")) "an unanswered check is no")))

(deftest a-user-without-status-rights-is-refused
  (is (nil? (operations/status-rights-refusal (constantly true) "db")))
  (is (str/includes? (operations/status-rights-refusal (constantly false) "db") "status.targetPrimary")))

(deftest switchover-declares-the-status-subresource-it-writes
  (let [{:keys [prerequisites refusal]} (with-redefs [mutation/book identity] (load-file "tools/cnpg/runbooks/switchover.clj"))]
    (is (= ["clusters.postgresql.cnpg.io/status"] (:kubectl-can-patch prerequisites)))
    (is (some #{"pods"} (:kubectl-can-get prerequisites)) "the plugin reads the instance's pod")
    (testing "the refusal asks nothing of the API server"
      (with-redefs [shell/execute (fn [& _] (throw (ex-info "asked" {})))]
        (is (nil? (refusal {:context "ctx"
                            :ref {:namespace "db" :cluster "pg" :name "pg" :instance "pg-2"}
                            :object {:metadata {:namespace "db" :name "pg"}
                                     :status {:currentPrimary "pg-1" :targetPrimary "pg-1"
                                              :instancesStatus {:healthy ["pg-1" "pg-2"]}}}})))))))
