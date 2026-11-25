;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.upterm-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.upterm :as upterm]))

(deftest host-args-test
  (testing "each named user becomes an --authorized-user flag, so only their keys can join"
    (is (= ["upterm" "host" "--authorized-user" "github:alice" "--authorized-user" "github:bob"]
           (upterm/host-args ["alice" "bob"]))))
  (testing "a value naming its provider is passed through as upterm reads it"
    (is (= ["upterm" "host" "--authorized-user" "gitlab:bob"
            "--authorized-user" "https://git.example.com/dave"]
           (upterm/host-args ["gitlab:bob" "https://git.example.com/dave"]))))
  (testing "blank and repeated names add nothing"
    (is (= ["upterm" "host" "--authorized-user" "github:alice"]
           (upterm/host-args ["alice" " " "" "alice"])))))

(deftest a-session-nobody-is-named-for-is-refused
  (testing "with no user, no command at all"
    (is (nil? (upterm/host-args [])))
    (is (nil? (upterm/host-args nil)))
    (is (nil? (upterm/host-args ["" "  "]))))
  (testing "the refusal tells the operator both ways forward"
    (let [{:keys [refused args]} (upterm/host-plan [])]
      (is (nil? args))
      (is (str/includes? refused "--with"))
      (is (str/includes? refused "--open")))))

(deftest an-open-session-needs-the-explicit-flag
  (is (= {:args ["upterm" "host"] :open? true} (upterm/host-plan [] true)))
  (testing "named users win over --open, so the session stays restricted"
    (is (= {:args ["upterm" "host" "--authorized-user" "github:alice"]}
           (upterm/host-plan ["alice"] true)))))

(deftest start-never-runs-upterm-for-a-refused-session
  (let [ran (atom [])]
    (with-redefs [shell/run-with-output (fn [& args] (swap! ran conj (vec args)) 0)]
      (binding [*err* (java.io.StringWriter.)] (is (false? (upterm/start! []))))
      (is (= [] @ran))
      (binding [*err* (java.io.StringWriter.)] (is (true? (upterm/start! [] true))))
      (binding [*err* (java.io.StringWriter.)] (is (true? (upterm/start! ["alice"]))))
      (is (= [["upterm" "host"]
              ["upterm" "host" "--authorized-user" "github:alice"]]
             @ran)))))

(deftest the-offered-command-is-restricted
  (is (= "upterm host --authorized-user github:<github-user>" upterm/offer-command)))

(deftest the-share-book-refuses-without-with-or-open
  (let [config (binding [runbook/*loading* true] (load-file "runbooks/collaborate/share.clj"))
        action (:action config)
        ran (atom [])]
    (is (contains? (:options config) :open))
    (with-redefs [shell/run-with-output (fn [& args] (swap! ran conj (vec args)) 0)]
      (binding [*err* (java.io.StringWriter.)]
        (is (false? (action {} nil)))
        (is (= [] @ran))
        (is (true? (action {:with ["alice"]} nil)))
        (is (true? (action {:open true} nil)))))
    (is (= [["upterm" "host" "--authorized-user" "github:alice"]
            ["upterm" "host"]]
           @ran))))
