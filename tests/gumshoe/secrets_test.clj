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
