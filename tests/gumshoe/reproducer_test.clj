;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.reproducer-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.reproducer :as reproducer]
            [gumshoe.shell :as shell]))

(deftest script-content-test
  (let [script (reproducer/script-content "detectives/nodes"
                                          ["kubectl get nodes -o json"
                                           "kubectl get pods -o json"])]
    (testing "the script is a runnable shell file naming its source and commands"
      (is (str/starts-with? script "#!/usr/bin/env sh"))
      (is (str/includes? script "detectives/nodes"))
      (is (str/includes? script "kubectl get nodes -o json"))
      (is (str/includes? script "kubectl get pods -o json")))))

(deftest recording-captures-commands-test
  (testing "commands run through the shell layer are recorded verbatim"
    (let [before (count (shell/recording))]
      (shell/stdout-of "true" "recording-probe")
      (let [after (shell/recording)]
        (is (= (inc before) (count after)))
        (is (= "true recording-probe" (last after)))))))

(deftest a-recorded-command-replays-with-the-same-arguments
  (shell/execute "printf" "%s|" "a b;c" "{range .items[*]}{\"\\t\"}" "it's")
  (let [line (last (shell/recording))
        replay (shell/execute "sh" "-c" line)]
    (is (= "printf '%s|' 'a b;c' '{range .items[*]}{\"\\t\"}' 'it'\\''s'" line))
    (is (= "a b;c|{range .items[*]}{\"\\t\"}|it's|" (:out replay)))))
