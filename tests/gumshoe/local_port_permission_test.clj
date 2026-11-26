;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.local-port-permission-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]))

(deftest bind-failures-are-told-apart-by-their-message
  (is (= :in-use (kubectl/bind-failure 8080 "Address already in use")))
  (is (= :in-use (kubectl/bind-failure 80 "Address already in use (Bind failed)")))
  (is (= :in-use (kubectl/bind-failure 8080 "Address in use")) "babashka's native image words it so")
  (is (= :permission-denied (kubectl/bind-failure 80 "Permission denied")))
  (is (= :permission-denied (kubectl/bind-failure 8080 "Permission denied")))
  (testing "a message naming neither cause"
    (is (= :permission-denied (kubectl/bind-failure 443 "Bind failed")) "below 1024 it is the privilege")
    (is (= :other (kubectl/bind-failure 18080 "Cannot assign requested address")))))

(defn- problem-when-bind-throws
  [port message]
  (with-redefs-fn {#'kubectl/port-open? (constantly false)
                   #'kubectl/try-bind! (fn [_] (throw (java.net.BindException. message)))}
    #(kubectl/local-port-problem port)))

(deftest a-privileged-port-is-reported-as-denied
  (let [problem (problem-when-bind-throws 80 "Permission denied")]
    (is (str/starts-with? problem "permission denied for port 80"))
    (is (not (str/includes? problem "already in use")))))

(deftest a-held-port-is-reported-as-in-use
  (is (= "local port 18080 is already in use - pick another with --port"
         (problem-when-bind-throws 18080 "Address already in use"))))

(deftest another-bind-failure-is-named-as-itself
  (is (= "local port 18080 could not be checked: Cannot assign requested address"
         (problem-when-bind-throws 18080 "Cannot assign requested address"))))

(deftest a-real-bind-of-a-free-port-passes
  (let [port (kubectl/free-local-port)]
    (is (nil? (kubectl/local-port-problem port)))))
