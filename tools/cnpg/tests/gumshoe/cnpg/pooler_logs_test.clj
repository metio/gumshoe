;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.pooler-logs-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.cnpg.pooler-logs :as pooler-logs]))

(defn- pod [namespace name] {:metadata {:namespace namespace :name name}})

(deftest every-pooler-pod-becomes-one-row-test
  (let [rows (pooler-logs/collect
              (fn [] [(pod "db" "db-rw-transaction-1") (pod "db" "db-ro-session-1")])
              (fn [_namespace pod] {:read? true :log (str pod " log")}))]
    (is (= 2 (count rows)))
    (is (= [{:namespace "db" :pod "db-rw-transaction-1" :read? true
             :log "db-rw-transaction-1 log"}
            {:namespace "db" :pod "db-ro-session-1" :read? true
             :log "db-ro-session-1 log"}]
           rows))))

(deftest a-pod-whose-log-failed-to-read-is-reported-unread-test
  (let [rows (pooler-logs/collect
              (fn [] [(pod "db" "db-rw-transaction-1")])
              (fn [_namespace _pod] {:read? false :log ""}))]
    (is (= false (:read? (first rows))))
    (is (= "" (:log (first rows))) "an unread log carries no content to classify")))

(deftest a-cluster-without-poolers-yields-no-rows-test
  (is (= [] (pooler-logs/collect (fn [] []) (fn [_ _] (throw (ex-info "not called" {})))))))

(deftest the-log-is-read-for-the-pod-that-owns-it-test
  (let [asked (atom [])]
    (pooler-logs/collect
     (fn [] [(pod "one" "a") (pod "two" "b")])
     (fn [namespace pod] (swap! asked conj [namespace pod]) {:read? true :log ""}))
    (is (= [["one" "a"] ["two" "b"]] @asked))))
