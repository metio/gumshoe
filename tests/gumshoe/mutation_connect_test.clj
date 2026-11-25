;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-connect-test
  "A mutation spec with :connect acts on what its connect function names
   instead of the kube context, and :announce? reaches the harness."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.runbook :as runbook]))

(def ^:private host {:host "mgr-1"})

(defn- spec
  [& {:as overrides}]
  (merge {:connect (fn [opts] {:context (assoc host :user (:user opts))
                               :cluster "mgr-1"
                               :announce-as "ceph (mgr-1)"})
          :select {:mode :one :label "OSD" :flag :osd
                   :candidates (fn [connection] (when (= "mgr-1" (:host connection)) ["osd.1" "osd.3"]))}
          :confirm {:action "bring an OSD back in"}
          :announce (fn [{:keys [target]}] (str "Bring " target " back in"))
          :effect (fn [{:keys [context target]}] (effect/plan (effect/ssh context "ceph" "osd" "in" target)))
          :verify (fn [{:keys [context cluster target]}]
                    [{:description (str target " on " cluster " via " (:host context)) :check (constantly true)}])}
         overrides))

(defn- no-kubectl
  [& _]
  (throw (ex-info "kubectl must not be asked" {})))

(defmacro ^:private quietly
  [& body]
  `(let [out# (java.io.StringWriter.)]
     (binding [*out* out# *err* out#] ~@body)))

(defmacro ^:private captured
  "Runs body with kubectl unreachable, announcements returned as [system
   message], and flow/change! recorded; [result config announced], where
   announced is what the recorded change announces."
  [& body]
  `(let [seen# (atom nil)]
     (with-redefs [kubectl/current-context no-kubectl
                   kubectl/current-cluster no-kubectl
                   announce/announce! (fn [system# _# message#] [system# message#])
                   flow/change! (fn [config#] (reset! seen# config#) true)]
       (let [result# (quietly ~@body)
             announce# (:announce! @seen#)]
         [result# @seen# (when announce# (announce#))]))))

(deftest a-connected-spec-never-asks-kubectl
  (let [[result config announced] (captured (mutation/run! (spec) {:osd "osd.3" :user "admin"} nil))]
    (is (true? result))
    (testing "the confirmation names what connect named"
      (is (= {:action "bring an OSD back in" :target "mgr-1" :items ["osd.3"] :destructive? nil}
             (:confirmation config))))
    (testing "the callbacks receive connect's context"
      (is (= [[:ssh {:host "mgr-1" :user "admin"} "ceph" "osd" "in" "osd.3"]] (:effect config)))
      (is (= ["osd.3 on mgr-1 via mgr-1"] (map :description (:post-checks config)))))
    (testing "the announcement carries connect's name for the system"
      (is (= ["ceph (mgr-1)" "Bring osd.3 back in"] announced)))))

(deftest run-on-connects-too
  (let [[result config] (captured (mutation/run-on! (spec) "osd.1" {} nil))]
    (is (true? result))
    (is (= "mgr-1" (get-in config [:confirmation :target])))))

(deftest the-announcement-defaults-to-the-cluster-name
  (let [[_ _ announced] (captured (mutation/run-on! (spec :connect (constantly {:context host :cluster "mgr-1"}))
                                               "osd.1" {} nil))]
    (is (= ["mgr-1" "Bring osd.1 back in"] announced))))

(deftest a-connection-that-fails-stops-before-the-select
  (let [listed (atom false)
        s (spec :connect (constantly nil)
                :select {:mode :one :label "OSD" :flag :osd :candidates (fn [_] (reset! listed true) ["osd.1"])})]
    (doseq [[label run] [["run!" #(mutation/run! s {:osd "osd.1"} nil)]
                         ["run-on!" #(mutation/run-on! s "osd.1" {} nil)]]]
      (testing label
        (let [[result config] (captured (run))]
          (is (false? result))
          (is (nil? config) "nothing is confirmed")
          (is (false? @listed) "no candidate is listed"))))))

(deftest a-subject-or-pin-needs-the-kube-context
  (doseq [[label s] [["subject" (spec :subject {:type "cephosds"})]
                     ["pin" (spec :pin (constantly {:type "cephosds" :name "osd.1"}))]]]
    (testing label
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #":connect"
                            (captured (mutation/run-on! s "osd.1" {} nil)))))))

(deftest a-spec-without-connect-reads-the-kube-context
  (let [seen (atom nil)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  flow/change! (fn [config] (reset! seen config) true)]
      (mutation/run-on! (dissoc (spec :effect (fn [{:keys [context]}] (effect/plan (effect/kubectl context "get" "nodes"))))
                                :connect)
                        "osd.1" {} nil)
      (is (= "cluster" (get-in @seen [:confirmation :target])))
      (is (= [[:kubectl "ctx" "get" "nodes"]] (:effect @seen))))))

(deftest announce?-reaches-the-harness
  (binding [runbook/*loading* true]
    (testing "a spec that posts nothing tells the harness so"
      (is (false? (:announce? (mutation/book (-> (spec) (dissoc :announce) (assoc :announce? false))))))
      (is (false? (:announce? (mutation/book (-> (spec) (dissoc :announce) (assoc :announce false)))))))
    (testing "a spec that announces leaves the harness default, which announces"
      (is (not (contains? (mutation/book (assoc (spec) :announce? true)) :announce?)))
      (is (not (contains? (mutation/book (spec)) :announce?))))
    (testing "a changelog message beside :announce? false is refused when the book loads"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"an :announce message and :announce\? false"
                            (mutation/book (assoc (spec) :announce? false)))))))
