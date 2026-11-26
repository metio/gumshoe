;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-unread-reads-test
  "A read that did not answer leaves the cnpg evidence unread (nil), never an
   empty map or list that judges as a healthy cluster."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.cnpg.pooler-logs :as pooler-logs]
            [gumshoe.cnpg.verifier-churn :as churn]
            [gumshoe.cnpg.volumes :as volumes]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.cnpg-pooler-logs :as cnpg-pooler-logs]
            [gumshoe.detectives.cnpg-volumes :as cnpg-volumes]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]))

;; --- verifier churn ---------------------------------------------------------------

(deftest a-second-reading-that-did-not-answer-is-unread-test
  (let [before (churn/parse-snapshot "app|abc\nother|def\n")
        result (churn/verdict (churn/compare-snapshots before (churn/parse-snapshot nil)))]
    (is (= :unread (:verdict result)))
    (is (not= :stable (:verdict result)))
    (is (re-find #"says nothing about churn" (first (churn/guidance result))))))

(deftest a-first-reading-that-did-not-answer-is-unread-test
  (is (= :unread (:verdict (churn/verdict (churn/compare-snapshots nil {"app" "abc"}))))))

(deftest no-shared-role-is-incomparable-not-stable-test
  (let [result (churn/verdict (churn/compare-snapshots {"app" "abc" "other" "def"} {"new" "x"}))]
    (is (= :incomparable (:verdict result)))
    (is (= "1 role(s) appeared and 2 vanished between the readings" (:note result)))))

(deftest one-shared-unchanged-role-is-still-stable-test
  (let [result (churn/verdict (churn/compare-snapshots {"app" "abc" "other" "def"} {"app" "abc"}))]
    (is (= :stable (:verdict result)))
    (is (= "0 role(s) appeared and 1 vanished between the readings" (:note result)))))

(deftest an-answered-empty-reading-is-no-roles-test
  (is (= :no-roles (:verdict (churn/verdict (churn/compare-snapshots {} {}))))))

;; --- cnpg-volumes -----------------------------------------------------------------

(def ^:private volume-check (first (filter #(= "cnpg-volumes" (:name %)) cnpg-volumes/detectives)))

(deftest a-failed-pod-list-leaves-the-volumes-unread-test
  (let [stats (volumes/collect (constantly nil) (fn [_] (throw (ex-info "not called" {}))))]
    (is (nil? stats))
    (is (detective/unread? volume-check {volumes/evidence-key stats}))))

(deftest an-answered-empty-pod-list-is-read-test
  (let [stats (volumes/collect (constantly {:items []}) (constantly nil))]
    (is (= {:instance-pods [] :nodes [] :unread-nodes [] :volumes []} stats))
    (is (not (detective/unread? volume-check {volumes/evidence-key stats})))))

(deftest the-volume-collector-reads-the-pod-list-strictly-test
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "the server is currently unable to handle the request"})]
    (is (nil? (volumes/collector "ctx" volumes/evidence-key)))))

;; --- cnpg-pooler-logins -----------------------------------------------------------

(def ^:private pooler-check (first cnpg-pooler-logs/detectives))

(deftest a-failed-pooler-pod-list-leaves-the-logins-unread-test
  (let [rows (pooler-logs/collect (constantly nil) (fn [_ _] {:read? true :log ""}))]
    (is (nil? rows))
    (is (detective/unread? pooler-check {pooler-logs/evidence-key rows}))))

(deftest the-pooler-collector-reads-the-pod-list-strictly-test
  (testing "a list that did not answer"
    (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unauthorized"})]
      (is (nil? (pooler-logs/collector "ctx" pooler-logs/evidence-key)))))
  (testing "a list that answered with no pooler pods"
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out "{\"items\":[]}" :err ""})
                  kubectl/container-logs (fn [& _] (throw (ex-info "not called" {})))]
      (is (= [] (pooler-logs/collector "ctx" pooler-logs/evidence-key))))))
