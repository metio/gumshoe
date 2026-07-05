;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.secrets-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.secrets :as secrets]))

(deftest resolve-command-test
  (testing "substitutes {path} and {field} into a command template"
    (is (= ["gopass" "show" "--password" "matrix/bot"]
           (secrets/resolve-command ["gopass" "show" "--password" "{path}"] {:path "matrix/bot"})))
    (is (= ["gopass" "show" "matrix/bot" "login"]
           (secrets/resolve-command ["gopass" "show" "{path}" "{field}"] {:path "matrix/bot" :field "login"})))
    (testing "a different manager with its own path structure is just a template"
      (is (= ["op" "read" "op://ops/matrix/bot/password"]
             (secrets/resolve-command ["op" "read" "op://ops/{path}/password"] {:path "matrix/bot"})))
      (is (= ["pass" "show" "matrix/bot"]
             (secrets/resolve-command ["pass" "show" "{path}"] {:path "matrix/bot"})))))
  (testing "a placeholder with no substitution is left untouched"
    (is (= ["cmd" "{field}"] (secrets/resolve-command ["cmd" "{field}"] {:path "x"})))))

(deftest defaults-are-gopass-test
  (is (= "gopass" (first (:password secrets/defaults))))
  (is (= "gopass" (first (:search secrets/defaults)))))

(deftest built-in-providers-test
  (testing "gopass, pass, passage, pasejo, and the command escape hatch ship built in"
    (is (every? (set (secrets/registered-providers)) [:gopass :pass :passage :pasejo :command]))))

(deftest active-name-selection-test
  (testing "an explicit :provider wins"
    (is (= :pass (secrets/active-name {:provider :pass})))
    (is (= :pasejo (secrets/active-name {:provider :pasejo}))))
  (testing "no config defaults to gopass"
    (is (= :gopass (secrets/active-name {}))))
  (testing "a legacy config of bare templates is read as the :command provider"
    (is (= :command (secrets/active-name {:password ["op" "read" "op://ops/{path}/pw"]})))))

(deftest first-line-is-the-password-test
  (testing "the password is the first non-blank line (pass/passage/pasejo convention)"
    (is (= "s3cr3t" (secrets/first-line "s3cr3t\nlogin: alice\nurl: https://x")))
    (is (= "s3cr3t" (secrets/first-line "\n\ns3cr3t\n")))
    (is (nil? (secrets/first-line "   \n")))))

(deftest field-in-parses-name-value-lines-test
  (let [secret "s3cr3t\nlogin: alice\nurl: https://x:8443/path"]
    (is (= "alice" (secrets/field-in secret "login")))
    (testing "a value may itself contain colons"
      (is (= "https://x:8443/path" (secrets/field-in secret "url"))))
    (is (nil? (secrets/field-in secret "missing")))))

(deftest plugin-can-register-a-native-provider-test
  (testing "a plugin adds a backend the CLI-template model can not express"
    (secrets/register-provider! {:name :vault-native :binary "vault"
                                 :password (fn [_] "x") :field (fn [_ _] nil)
                                 :find-path (fn [_] nil) :available? (fn [_] true)})
    (is (contains? (set (secrets/registered-providers)) :vault-native))))
