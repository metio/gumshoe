;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.verify-test
  (:require [cheshire.core]
            [clojure.string]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]
            [gumshoe.verify :as verify]))

(deftest eventually-test
  (testing "passes immediately when the check holds"
    (is (verify/eventually {:description "holds" :timeout 0 :check (constantly true)})))
  (testing "fails when the check never holds within the timeout"
    (is (not (verify/eventually {:description "never" :timeout 0 :check (constantly false)}))))
  (testing "retries until the check holds"
    (let [attempts (atom 0)]
      (is (verify/eventually {:description "third time lucky"
                              :timeout 10
                              :interval 0
                              :check #(<= 3 (swap! attempts inc))}))
      (is (= 3 @attempts))))
  (testing "an exception inside the check counts as not-yet-verified"
    (is (not (verify/eventually {:description "throws" :timeout 0
                                 :check #(throw (ex-info "boom" {}))})))))

(deftest malformed-post-check-test
  (testing "a post-check without :description or :check is refused, never 'verified'"
    (is (not (verify/eventually {:check (constantly true)})))
    (is (not (verify/eventually {:description "no check function"})))
    (is (not (verify/eventually {:description "check is not a function" :check true})))))

(deftest soft-check-test
  (testing "a soft check that never passes times out as best-effort success, not failure"
    (is (verify/eventually {:description "soft" :timeout 0 :soft? true :check (constantly false)})))
  (testing "a soft check still returns true immediately when it passes"
    (is (verify/eventually {:description "soft-ok" :timeout 0 :soft? true :check (constantly true)})))
  (testing "a hard check that never passes still fails"
    (is (not (verify/eventually {:description "hard" :timeout 0 :check (constantly false)})))))

(deftest watch-surfaces-signals-test
  (testing "the watcher runs during the wait and new signals are gathered without failing the check"
    (let [samples (atom 0)
          watch (fn [] (swap! samples inc) ["a Warning event appeared"])]
      ;; a soft check that never passes, with a tiny interval, exercises the watch path
      (is (verify/eventually {:description "watched" :timeout 1 :interval 0 :soft? true
                              :check (constantly false) :watch watch}))
      (is (pos? @samples)))))

(deftest watch-primes-baseline-test
  (testing "the pre-existing signal backlog is not reported as if it appeared during the wait"
    (let [calls (atom 0)
          watch (fn []
                  (swap! calls inc)
                  ;; the pre-existing signal is present from the baseline sample on;
                  ;; a genuinely new one only shows up after a couple of intervals
                  (if (>= @calls 3)
                    ["pre-existing warning" "a NEW resizer error"]
                    ["pre-existing warning"]))
          err (java.io.StringWriter.)]
      (binding [*err* err]
        (verify/eventually {:description "watched" :timeout 2 :interval 0 :soft? true
                            :check (constantly false) :watch watch}))
      (let [out (str err)]
        (is (clojure.string/includes? out "a NEW resizer error")
            "a signal that appears during the wait is surfaced")
        (is (not (clojure.string/includes? out "while waiting — pre-existing warning"))
            "the pre-existing backlog is not surfaced as a during-wait signal")))))

(deftest abort-ends-the-wait-test
  (testing "an abort reason fails the check at once, a soft one included"
    (let [checks (atom 0)
          started (System/currentTimeMillis)]
      (is (not (verify/eventually {:description "doomed" :timeout 30 :interval 1 :soft? true
                                   :check #(do (swap! checks inc) false)
                                   :abort (constantly "the operation failed")})))
      (is (= 1 @checks))
      (is (< (- (System/currentTimeMillis) started) 5000) "it did not wait out the timeout")))
  (testing "the reason is printed"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (verify/eventually {:description "doomed" :timeout 0 :check (constantly false) :abort (constantly "the Backup failed: no space")}))
      (is (clojure.string/includes? (str err) "the Backup failed: no space"))))
  (testing "a check that holds wins over an abort"
    (is (verify/eventually {:description "done" :timeout 0 :check (constantly true) :abort (constantly "late failure")})))
  (testing "no reason, or a throwing abort, keeps waiting"
    (let [attempts (atom 0)]
      (is (verify/eventually {:description "eventually" :timeout 10 :interval 0
                              :check #(<= 3 (swap! attempts inc))
                              :abort #(when (> @attempts 5) "never reached")})))
    (is (verify/eventually {:description "throwing abort" :timeout 0 :soft? true
                            :check (constantly false) :abort #(throw (ex-info "boom" {}))}))))

;; A check that asserts an absence: it holds when the node cannot be found.
(defn- node-absent-check
  []
  {:description "node n1 is gone" :timeout 0 :interval 0
   :check #(nil? (kubectl/get-cluster-resource "ctx" "nodes" "n1"))})

(defn- quietly
  [f]
  (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)] (f)))

(deftest a-read-that-did-not-answer-never-verifies-a-check
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server: dial tcp: i/o timeout"})]
    (testing "a hard check fails at its timeout and names the read that did not answer"
      (let [err (java.io.StringWriter.)]
        (binding [*out* (java.io.StringWriter.) *err* err]
          (is (false? (verify/eventually (node-absent-check)))))
        (is (clojure.string/includes? (str err) "the last read did not answer"))
        (is (clojure.string/includes? (str err) "i/o timeout"))))
    (testing "the wait keeps polling until the timeout instead of passing early"
      (let [attempts (atom 0)]
        (is (false? (quietly #(verify/eventually {:description "node n1 is gone" :timeout 2 :interval 0
                                                   :check (fn [] (swap! attempts inc)
                                                            (nil? (kubectl/get-cluster-resource "ctx" "nodes" "n1")))}))))
        (is (< 1 @attempts))))
    (testing "a soft check still downgrades the timeout to a warning"
      (let [err (java.io.StringWriter.)]
        (binding [*out* (java.io.StringWriter.) *err* err]
          (is (true? (verify/eventually (assoc (node-absent-check) :soft? true)))))
        (is (clojure.string/includes? (str err) "could not confirm"))))))

(deftest a-read-that-answered-not-found-verifies-an-absence
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Error from server (NotFound): nodes \"n1\" not found"})]
    (is (true? (quietly #(verify/eventually (node-absent-check)))))))

(deftest a-check-reads-strictly-only-while-it-runs
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"})]
    (testing "the watch and abort samplers read lax, as outside a check"
      (let [watched (atom [])]
        (quietly #(verify/eventually {:description "x" :timeout 1 :interval 0 :soft? true
                                      :check (constantly false)
                                      :watch (fn [] (swap! watched conj (kubectl/get-json "ctx" "events")) [])}))
        (is (seq @watched))
        (is (every? nil? @watched))))
    (testing "outside eventually the reads stay lax"
      (is (nil? (kubectl/get-json "ctx" "nodes"))))))

(deftest all-test
  (testing "every check must hold"
    (is (verify/all [{:description "a" :timeout 0 :check (constantly true)}
                     {:description "b" :timeout 0 :check (constantly true)}]))
    (is (not (verify/all [{:description "a" :timeout 0 :check (constantly true)}
                          {:description "b" :timeout 0 :check (constantly false)}])))))

;; --- failing closed ------------------------------------------------------------------

(deftest object-holds-only-for-an-object-a-read-found
  (is (true? (verify/object-holds? {:spec {}} map?)))
  (is (false? (verify/object-holds? {:spec {}} (constantly nil))))
  (is (false? (verify/object-holds? nil (constantly true))) "nil is no object, whatever the predicate says")
  (is (false? (verify/object-holds? nil nil?))))

(deftest attempt-judges-a-check-as-eventually-does
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"})]
    (is (= "kubectl get did not answer: Unable to connect to the server"
           (verify/attempt #(nil? (kubectl/get-cluster-resource "ctx" "nodes" "n1"))))
        "a strict read that did not answer is named, not counted as nil"))
  (is (true? (verify/attempt (constantly :truthy))))
  (is (false? (verify/attempt (constantly nil))))
  (is (false? (verify/attempt #(throw (Exception. "boom"))))))

(defn- node-check
  [description holds? & {:as extra}]
  (merge {:description description
          :check #(holds? (kubectl/get-cluster-resource "ctx" "nodes" "n1"))}
         extra))

(deftest fail-closed-findings-names-each-check-that-holds-where-it-must-not
  (testing "a check that holds on nil holds on NotFound, and on a lax read that did not answer"
    (is (= [{:description "n1 is schedulable" :answer :absent}]
           (verify/fail-closed-findings [(node-check "n1 is schedulable" #(not (-> % :spec :unschedulable)))]))
        "under strict reads an unanswered read throws, so only NotFound shows it"))
  (testing "a check judged through object-holds? fails closed"
    (is (= [] (verify/fail-closed-findings
               [(node-check "n1 is schedulable" #(verify/object-holds? % (fn [n] (not (-> n :spec :unschedulable)))))]))))
  (testing ":verifies-absence? permits holding on NotFound, never on no answer"
    (is (= [] (verify/fail-closed-findings [(node-check "n1 is gone" nil? :verifies-absence? true)])))
    (is (= [{:description "anything" :answer :unanswered}]
           (verify/fail-closed-findings [{:description "anything" :verifies-absence? true :check (constantly true)}]))))
  (testing "a check that ignores its reads holds under both answers"
    (is (= [{:description "always" :answer :unanswered} {:description "always" :answer :absent}]
           (verify/fail-closed-findings [{:description "always" :check (constantly true)}]))))
  (testing "a check that reads around strict reads is caught when nothing answers"
    (is (= [{:description "lax" :answer :unanswered}]
           (verify/fail-closed-findings
            [{:description "lax" :verifies-absence? true
              :check #(clojure.string/blank? (shell/stdout-of "kubectl" "get" "nodes" "n1" "--output=json"))}])))))

(deftest a-cluster-holding-nothing-answers-as-the-api-server-does
  (let [answers (atom [])
        record (fn [check] (with-redefs [verify/attempt (fn [c] (c) false)] (verify/fail-closed-findings [check])))]
    (record {:description "reads"
             :check #(swap! answers conj
                            (mapv (fn [argv] (apply shell/execute argv))
                                  [["kubectl" "--context=ctx" "get" "nodes" "n1" "--output=json"]
                                   ["kubectl" "--context=ctx" "get" "pods" "--all-namespaces" "--output=json"]
                                   ["kubectl" "--context=ctx" "get" "nodes" "n1" "--ignore-not-found" "--output=name"]
                                   ["kubectl" "--context=ctx" "--namespace=x" "get" "secrets" "--output=jsonpath={.items}"]
                                   ["kubectl" "--context=ctx" "delete" "nodes" "n1"]
                                   ["ssh" "host" "--" "ceph" "osd" "dump"]]))})
    (let [[unanswered absent] @answers]
      (is (every? #(= 1 (:exit %)) unanswered) "nothing answers")
      (is (= [1 0 0 0 1 1] (map :exit absent)))
      (is (re-find #"\(NotFound\)" (:err (first absent))))
      (is (= [] (:items (cheshire.core/parse-string (:out (second absent)) true))) "a listing answers empty"))))
