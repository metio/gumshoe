;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.console-promote-test
  "cnpg_console --promote refuses the current or target primary, a fenced
   instance and a cluster whose primary is already changing, before its
   confirmation and again after it; it may promote an unhealthy standby."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cnpg.operations :as operations]
            [gumshoe.command :as command]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]))

(defn- cluster
  [& {:keys [primary target phase fenced healthy] :or {primary "pg-1" phase "Cluster in healthy state"
                                                         healthy ["pg-1" "pg-2"]}}]
  {:metadata (cond-> {:namespace "db" :name "pg"}
               fenced (assoc :annotations {:cnpg.io/fencedInstances fenced}))
   :status {:currentPrimary primary :targetPrimary (or target primary) :phase phase
            :instancesStatus {:healthy healthy :failed ["pg-3"]}}})

(deftest a-standby-may-be-promoted-even-when-unhealthy
  (is (nil? (operations/console-promote-refusal (cluster) "pg-2")))
  (is (nil? (operations/console-promote-refusal (cluster) "pg-3")) "the console is where an emergency promotion happens"))

(deftest the-current-or-target-primary-is-refused
  (is (= "pg-1 is the primary already" (operations/console-promote-refusal (cluster) "pg-1")))
  (is (str/includes? (operations/console-promote-refusal (cluster :target "pg-2" :phase "Switchover in progress") "pg-2")
                     "under way")))

(deftest a-change-of-primary-under-way-is-refused
  (testing "targetPrimary differs from currentPrimary"
    (is (str/includes? (operations/console-promote-refusal (cluster :target "pg-2") "pg-3") "under way")))
  (testing "the phase says so while both still agree"
    (is (str/includes? (operations/console-promote-refusal (cluster :phase "Switchover in progress") "pg-2") "Switchover in progress"))
    (is (str/includes? (operations/console-promote-refusal (cluster :phase "Failing over") "pg-2") "Failing over"))))

(deftest a-fenced-instance-is-refused
  (is (= "pg-2 is fenced and cannot be promoted" (operations/console-promote-refusal (cluster :fenced "[\"pg-2\"]") "pg-2")))
  (is (some? (operations/console-promote-refusal (cluster :fenced "[\"*\"]") "pg-2")))
  (is (str/includes? (operations/console-promote-refusal (cluster :fenced "not json") "pg-2") "not a JSON list")))

(deftest a-cluster-without-a-primary-or-gone-is-refused
  (is (= "the cluster no longer exists" (operations/console-promote-refusal nil "pg-2")))
  (is (= "the cluster reports no current primary" (operations/console-promote-refusal (cluster :primary nil) "pg-2"))))

(defn- console-book
  []
  (binding [runbook/*loading* true] (load-file "tools/cnpg/runbooks/cnpg_console.clj")))

(deftest the-console-refuses-before-its-confirmation
  (console-book)
  (let [refusal (requiring-resolve 'runbooks.cnpg.cnpg-console/promote-refusal)]
    (is (= "pg-1 is the primary already" (refusal (cluster) ["pg-1" "pg-2"] "pg-1")))
    (is (= "pg-9 is not an instance of db/pg" (refusal (cluster) ["pg-1" "pg-2"] "pg-9")))
    (is (nil? (refusal (cluster) ["pg-1" "pg-2"] "pg-2")))))

(deftest the-console-judges-the-cluster-again-after-the-confirmation
  (console-book)
  (let [promote! (ns-resolve 'runbooks.cnpg.cnpg-console 'promote!)
        seen (atom nil)]
    (with-redefs [command/installed? (constantly true)
                  flow/change! (fn [config] (reset! seen config) true)
                  kubectl/get-namespaced-resource (fn [_ _ _ _] (cluster :target "pg-3" :phase "Switchover in progress"))]
      (binding [*err* (java.io.StringWriter.)]
        (promote! "ctx" "db" "pg" "pg-2" {})
        (is (false? ((:precondition @seen))) "a switchover that began meanwhile refuses")))
    (with-redefs [command/installed? (constantly true)
                  flow/change! (fn [config] (reset! seen config) true)
                  kubectl/get-namespaced-resource (fn [_ _ _ _] (cluster))]
      (promote! "ctx" "db" "pg" "pg-2" {})
      (is (true? ((:precondition @seen)))))))
