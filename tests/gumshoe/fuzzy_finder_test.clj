;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.fuzzy-finder-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.fuzzy-finder :as fuzzy]))

(def ^:private single-select-args #'fuzzy/single-select-args)

(deftest single-select-args-test
  (testing "a seed query is passed to fzf"
    (is (some #{"--query=worker-1"} (single-select-args "Node" {:query "worker-1" :auto-select? false}))))
  (testing "no auto-accept when :auto-select? is false - the operator must confirm a seeded match"
    (let [args (single-select-args "Node" {:query "worker-1" :auto-select? false})]
      (is (not (some #{"--select-1"} args))
          "a mistyped name must not resolve to a lone fuzzy match without a keypress")
      (is (not (some #{"--exit-0"} args))
          "a no-match query keeps the picker open to edit rather than resolving to nothing")))
  (testing "auto-accept when :auto-select? is true (the default) - a launcher may run a unique match"
    (let [args (single-select-args "Which book?" {:query "scale up"})]
      (is (some #{"--select-1"} args))
      (is (some #{"--exit-0"} args))
      (is (some #{"--query=scale up"} args))))
  (testing "a blank query adds no --query flag"
    (is (not (some #(clojure.string/starts-with? % "--query=")
                   (single-select-args "Node" {})))))
  (testing ":preview adds a preview command and window; its absence adds neither"
    (let [args (single-select-args "Node" {:preview "kubectl describe node {}"})]
      (is (some #{"--preview=kubectl describe node {}"} args))
      (is (some #(clojure.string/starts-with? % "--preview-window=") args)))
    (is (not (some #(clojure.string/starts-with? % "--preview")
                   (single-select-args "Node" {}))))))

(def ^:private select #'fuzzy/select)

(defn- fake-fzf
  "A shell script standing in for fzf, so select runs against a real process."
  [body]
  (let [file (java.io.File/createTempFile "fake-fzf" ".sh")]
    (spit file (str "#!/bin/sh\n" body "\n"))
    (.setExecutable file true)
    (.deleteOnExit file)
    (str file)))

(def ^:private many-candidates
  ;; far beyond a pipe buffer, so a writer blocks until fzf reads or exits
  (map #(str "namespace-" (mod % 97) "/pod-" %) (range 200000)))

(deftest an-early-exit-selects-nothing
  (testing "fzf leaving before it read every candidate (ESC, no terminal) is no selection, not an IOException"
    (is (nil? (select [(fake-fzf "exit 130")] many-candidates)))
    (is (nil? (select [(fake-fzf "exec 0<&-; exit 2")] many-candidates))))
  (testing "a pick made before every candidate was read still comes back"
    (is (= "namespace-1/pod-1" (select [(fake-fzf "head -n 2 | tail -n 1")] many-candidates))))
  (testing "a pick after reading everything comes back trimmed"
    (is (= "namespace-82/pod-199999" (select [(fake-fzf "tail -n 1")] many-candidates))))
  (testing "a missing binary names what could not start"
    (let [e (try (select ["this-fzf-does-not-exist-xyz"] ["a" "b"]) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (clojure.string/includes? (ex-message e) "this-fzf-does-not-exist-xyz")))))
