;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.spec-test
  "Pins the load-bearing data shapes: real findings, every effect constructor,
   and the example env.edn must conform to their schemas, so a drift is caught."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.ceph :as ceph-detectives]
            [gumshoe.detectives.pods :as pod-detectives]
            [gumshoe.effect :as effect]
            [gumshoe.spec :as spec]))

(deftest finding-schema-test
  (testing "a well-formed finding conforms; malformed ones do not"
    (is (spec/valid? :gumshoe.spec/finding {:severity :warning :component "ns/x" :summary "trouble"}))
    (is (spec/valid? :gumshoe.spec/finding {:severity :info :component "x" :summary "y" :hint "do this"}))
    (is (not (spec/valid? :gumshoe.spec/finding {:severity :oops :component "x" :summary "y"})))
    (is (not (spec/valid? :gumshoe.spec/finding {:severity :info :component "x" :summary ""})))
    (is (not (spec/valid? :gumshoe.spec/finding {:severity :info :summary "no component"}))))
  (testing "real detectives emit conforming findings"
    (let [findings (concat
                    (ceph-detectives/detect-osd-problems
                     {"status" {:osdmap {:num_osds 12 :num_up_osds 11 :num_in_osds 10}}})
                    (pod-detectives/detect-unhealthy-pods
                     {"pods" {:items [{:metadata {:namespace "ns" :name "p"}
                                       :status {:containerStatuses
                                                [{:name "c" :restartCount 20
                                                  :state {:waiting {:reason "CrashLoopBackOff"}}}]}}]}}))]
      (is (seq findings) "expected the sample evidence to produce findings")
      (is (every? #(spec/valid? :gumshoe.spec/finding %) findings)
          (str "non-conforming: " (remove #(spec/valid? :gumshoe.spec/finding %) findings))))))

(deftest effect-schema-test
  (testing "every effect constructor produces a conforming effect"
    (is (spec/valid? :gumshoe.spec/effect (effect/kubectl "ctx" "cordon" "n")))
    (is (spec/valid? :gumshoe.spec/effect (effect/kubectl-stdin "ctx" "{}" "replace" "--raw" "/x" "-f" "-")))
    (is (spec/valid? :gumshoe.spec/effect (effect/ssh {:host "h"} "ceph" "-s")))
    (is (spec/valid? :gumshoe.spec/effect (effect/cmd "flux" "reconcile")))
    (is (spec/valid? :gumshoe.spec/effect (effect/note "hello"))))
  (testing "a plan of effects conforms; an unknown op does not"
    (is (spec/valid? :gumshoe.spec/plan (effect/plan (effect/kubectl "c" "get" "pods")
                                                   (effect/note "done"))))
    (is (not (spec/valid? :gumshoe.spec/effect [:teleport "somewhere"])))
    (is (not (spec/valid? :gumshoe.spec/plan [[:kubectl "c" "x"] [:teleport "y"]])))))

(deftest env-config-schema-test
  (testing "the example env.edn is well-shaped (comments stripped, first map read)"
    ;; env.edn.example carries prose between two example maps; read the first form
    (let [text (slurp "env.edn.example")
          config (edn/read-string text)]
      (is (nil? (spec/env-config-problems config))
          (str "env.edn.example has problems: " (spec/env-config-problems config)))))
  (testing "a flat config is still valid"
    (is (nil? (spec/env-config-problems {:vpn {:interface "wg0"} :ceph {:mgr-hosts ["m"]}}))))
  (testing "structural mistakes are reported, not silently accepted"
    (is (some? (spec/env-config-problems {:environments {:prod {:ceph {:mgr-hosts "not-a-list"}}}})))
    (is (some? (spec/env-config-problems {:environments "should-be-a-map"}))))
  (testing "an environment with no :select is flagged as un-auto-selectable"
    (let [issues (spec/env-config-problems {:environments {:prod {:ceph {:mgr-hosts ["m"]}}}})]
      (is (some #(re-find #"no :select" %) issues)))))

(deftest book-descriptor-schema-test
  (testing "a detective-book descriptor shape"
    (is (spec/valid? :gumshoe.spec/detective-book
                     {:description "Investigates nodes" :detectives [{:name "n"}] :prerequisites {}}))
    (is (not (spec/valid? :gumshoe.spec/detective-book
                          {:description "" :detectives [] :prerequisites {}}))))
  (testing "a mutation-book descriptor shape"
    (is (spec/valid? :gumshoe.spec/mutation-book
                     {:description "Cordon a node"
                      :select {:mode :one :label "Node" :candidates (fn [_] [])}
                      :confirm {:action "cordon a node"}
                      :effect (fn [_] [])}))
    (is (not (spec/valid? :gumshoe.spec/mutation-book
                          {:description "x"
                           :select {:mode :sideways :label "Node" :candidates (fn [_] [])}
                           :confirm {:action "x"}
                           :effect (fn [_] [])})))))
