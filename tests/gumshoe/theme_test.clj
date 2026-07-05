;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.theme-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gumshoe.theme :as theme]))

;; Every test leaves the default active, so nothing leaks a theme to another test.
(use-fixtures :each (fn [t] (theme/select! :default) (t) (theme/select! :default)))

(deftest built-in-themes-test
  (testing "the three built-ins are registered"
    (is (every? (set (theme/registered)) [:default :ascii :plain]))))

(deftest select-switches-glyphs-test
  (testing "the default is emoji + colour"
    (theme/select! :default)
    (is (= "✅" (theme/token :ok)))
    (is (= "🔥" (theme/severity :critical)))
    (is (true? (theme/token :color?))))
  (testing "ascii drops the emoji but keeps colour"
    (theme/select! :ascii)
    (is (= "[OK]" (theme/token :ok)))
    (is (= "[CRIT]" (theme/severity :critical)))
    (is (true? (theme/token :color?))))
  (testing "plain drops colour too"
    (theme/select! :plain)
    (is (false? (theme/token :color?)))))

(deftest partial-theme-merges-onto-default-test
  (testing "a plugin theme need only override what it changes; the rest falls back"
    (theme/register! {:name :branded :ok "👍"})
    (theme/select! :branded)
    (is (= "👍" (theme/token :ok)))
    (is (= "❌" (theme/token :error)) "unset glyphs come from the default")
    (is (= "🔥" (theme/severity :critical)))))

(deftest partial-nested-glyph-merges-onto-default-test
  (testing "overriding one marker glyph keeps the sibling glyphs from the default"
    (theme/register! {:name :cb :marker {:critical "▲"}})
    (theme/select! :cb)
    (is (= "▲" (theme/marker :critical)) "the overridden glyph is used")
    (is (= "🟡" (theme/marker :warning)) "the untouched sibling glyph survives the merge")
    (is (= "🔵" (theme/marker :info)) "so does the other sibling")
    (is (= "🔥" (theme/severity :critical)) "and the whole untouched :severity submap")))

(deftest unknown-theme-keeps-default-test
  (testing "an unknown name keeps the default and reports it was not found"
    (is (false? (theme/select! :nope)))
    (is (= "✅" (theme/token :ok)))))

(deftest severity-and-marker-fallbacks-test
  (testing "an unknown severity falls back to the bullet"
    (theme/select! :default)
    (is (= (theme/token :bullet) (theme/severity :bogus)))
    (is (= (theme/token :bullet) (theme/marker :bogus)))))
