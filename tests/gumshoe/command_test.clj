;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.command-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.command :as command]))

(deftest parse-version-test
  (testing "extracts the dotted number from the noise tools wrap around it"
    (is (= [1 29 3] (command/parse-version "Client Version: v1.29.3")))
    (is (= [3 0 14] (command/parse-version "OpenSSL 3.0.14 4 Jun 2024")))
    (is (= [1 12] (command/parse-version "babashka v1.12")))
    (is (= [2 43 0] (command/parse-version "git version 2.43.0"))))
  (testing "no dotted number yields nil"
    (is (nil? (command/parse-version "unknown")))
    (is (nil? (command/parse-version nil)))))

(deftest version-at-least?-test
  (testing "compares component-wise, padding missing components with zero"
    (is (command/version-at-least? "v1.29.0" "1.28"))
    (is (command/version-at-least? "v1.29" "1.29.0"))
    (is (command/version-at-least? "1.29.0" "1.29.0"))
    (is (not (command/version-at-least? "v1.27.9" "1.28")))
    (is (not (command/version-at-least? "1.29.0" "1.29.1"))))
  (testing "unparseable versions never block - a floor only catches proven-old tooling"
    (is (command/version-at-least? "some weird output" "1.28"))
    (is (command/version-at-least? "v1.29.0" "not-a-version"))
    (is (command/version-at-least? nil "1.28")))
  (testing "accepts int vectors on either side"
    (is (command/version-at-least? [1 29] [1 28]))
    (is (not (command/version-at-least? [1 28] [1 29])))))

(deftest describe-installed-test
  (testing "a tool with no readable version still gets a sensible label"
    (is (= "definitely-not-a-real-tool-xyz installed"
           (command/describe-installed "definitely-not-a-real-tool-xyz")))))
