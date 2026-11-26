;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps-nodes-gone-test
  "A node deleted during a maintenance run is reported as gone, and only a
   read that did not answer as unknown."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.shell :as shell]
            [gumshoe.steps.nodes :as nodes]))

(def ^:private not-found
  {:exit 1 :out "" :err "Error from server (NotFound): nodes \"n1\" not found"})

(def ^:private unreachable
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 192.0.2.1:6443: i/o timeout"})

(defn- cluster
  "kubectl answering a node get with get-answer and its presence read
   (--ignore-not-found) with presence-answer."
  [get-answer presence-answer]
  (fn [& argv]
    (if (some #{"--ignore-not-found"} argv) presence-answer get-answer)))

(deftest a-deleted-node-is-gone
  (with-redefs [shell/execute (cluster not-found {:exit 0 :out "" :err ""})]
    (is (= {:gone true} (nodes/node-state "ctx" "n1")))))

(deftest a-node-read-that-did-not-answer-stays-unknown
  (testing "the get and the presence read both fail"
    (with-redefs [shell/execute (cluster unreachable unreachable)]
      (is (= {:unread "the read of node n1 did not answer"} (nodes/node-state "ctx" "n1")))))
  (testing "the get fails while the presence read names the node"
    (with-redefs [shell/execute (cluster unreachable {:exit 0 :out "node/n1\n" :err ""})]
      (is (= {:unread "the read of node n1 did not answer"} (nodes/node-state "ctx" "n1"))))))

(deftest the-recovery-text-says-a-gone-node-is-gone
  (let [text (nodes/maintenance-recovery-text "ctx" "n1" {:gone true})]
    (is (str/starts-with? text "Node n1 no longer exists"))
    (is (not (str/includes? text "did not answer")))
    (is (not (str/includes? text "uncordon n1")))))
