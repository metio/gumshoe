;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ui-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.ui :as ui]))

;; Never leave a backend active for another test (it would swallow fzf).
(use-fixtures :each (fn [t] (reset! @#'ui/active nil) (t) (reset! @#'ui/active nil)))

(deftest no-backend-uses-builtin-test
  (testing "with nothing active, there is no override and the built-in fzf/gum runs"
    (is (nil? (ui/backend :select-one)))
    (is (nil? (ui/backend :confirm)))))

(deftest a-backend-overrides-selection-without-touching-callers-test
  (testing "a registered UI provider replaces the fuzzy picker - no book changes, no fzf"
    (let [stub {:name :stub :select-one (fn [_prompt _values _query] "stubbed")}]
      (ui/register-provider! stub)
      (reset! @#'ui/active stub)
      (is (= "stubbed" (fuzzy/select-single "pick one" ["a" "b" "c"]))
          "select-single routed to the backend, never shelling out to fzf"))))

(deftest a-partial-backend-falls-back-per-primitive-test
  (testing "a backend that overrides only :select-one leaves the others to the built-in"
    (reset! @#'ui/active {:name :partial :select-one (fn [_ _ _] "x")})
    (is (fn? (ui/backend :select-one)))
    (is (nil? (ui/backend :ask-text)) "unprovided primitives fall through to fzf/gum")))
