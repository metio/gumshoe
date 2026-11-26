;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.front-door-options-test
  "bb gumshoe reads --dry-run and --no-color before its search terms with the
   parse bb run uses, and hands them to the book it launches."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.catalog]
            [gumshoe.flags :as flags]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.main :as main]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(def ^:private fake-catalog
  [{:path "/pin/runbooks/kubernetes/nodes/drain.clj" :id "runbooks/kubernetes/nodes/drain.clj"
    :name "kubernetes/nodes/drain" :description "Drain"}])

(defn- through-front-door
  "Runs the front door with args against the fake catalog, the finder picking
   its one book. Returns {:exit :query :launched :prompts :investigated}. The
   doors' :error-fn, which exits the process, is removed, so a refusal throws
   its ex-info here instead of ending the test run."
  [args]
  (let [launched (atom nil) query (atom :not-asked) prompts (atom []) investigated (atom nil)]
    (with-redefs [gumshoe.catalog/books (fn [& _] fake-catalog)
                  gumshoe.catalog/casebook-root (fn [] "/nonexistent-casebook")
                  shell/run-with-output (fn [& argv] (reset! launched (vec argv)) 0)
                  stdout/disable-colors! (fn [] nil)
                  main/investigate (fn [argv] (reset! investigated (vec argv)) 0)
                  fuzzy/select-single (fn [prompt rows q]
                                        (swap! prompts conj prompt)
                                        (reset! query q)
                                        (first rows))]
      (let [exit (with-redefs-fn {#'main/gumshoe-spec (dissoc @#'main/gumshoe-spec :error-fn)
                                  #'main/run-spec (dissoc @#'main/run-spec :error-fn)}
                   #(main/front-door args))]
        {:exit exit :query @query :launched @launched :prompts @prompts :investigated @investigated}))))

(deftest options-before-search-terms-reach-the-book-test
  (doseq [[args flags] [[["--dry-run" "drain"] ["--dry-run"]]
                        [["--no-color" "drain"] ["--no-color"]]
                        [["--no-color" "--dry-run" "drain" "node"] ["--dry-run" "--no-color"]]
                        [["--dry-run" "--no-color" "drain"] ["--dry-run" "--no-color"]]]]
    (testing (pr-str args)
      (let [{:keys [exit query launched]} (through-front-door args)]
        (is (zero? exit))
        (is (= (str/join " " (remove #(str/starts-with? % "-") args)) query))
        (is (= (into ["bb" "/pin/runbooks/kubernetes/nodes/drain.clj"] flags) launched))))))

(deftest a-dry-run-without-search-terms-opens-the-book-finder-test
  (let [{:keys [exit prompts launched]} (through-front-door ["--dry-run"])]
    (is (zero? exit))
    (is (= ["Which book?"] prompts) "the guided menu's scans change nothing, so a dry run picks a book")
    (is (= ["bb" "/pin/runbooks/kubernetes/nodes/drain.clj" "--dry-run"] launched))))

(deftest a-lead-passes-the-options-to-the-investigation-test
  (is (= ["--host" "h" "--dry-run" "--no-color"]
         (:investigated (through-front-door ["--dry-run" "--no-color" "--host" "h"])))))

(deftest an-option-after-a-search-term-is-still-refused-test
  (doseq [[args arg] [[["--no-color" "drain" "--dry-run"] "--dry-run"]
                      [["--dry-run" "drain" "--no-color"] "--no-color"]
                      [["--host" "h" "drain" "--dry-run"] "--dry-run"]]]
    (testing (pr-str args)
      (let [data (try (flags/parse-with-search-terms args (dissoc @#'main/gumshoe-spec :error-fn) "gumshoe")
                      nil
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= :option-after-search-term (:cause data)))
        (is (= arg (:argument data))))
      (let [{:keys [launched investigated] :as run}
            (try (through-front-door args)
                 (catch clojure.lang.ExceptionInfo _ :refused))]
        (is (= :refused run) (str "nothing launched: " (pr-str [launched investigated])))))))

(deftest the-doors-parse-the-same-options-test
  (testing "every option bb run takes before search terms, bb gumshoe takes too"
    (is (every? (set (keys (:spec @#'main/gumshoe-spec))) (keys (:spec @#'main/run-spec))))))

(defn- bb-in
  [dir path & args]
  (let [{:keys [out err exit]} (apply process/shell {:dir (str dir) :out :string :err :string :continue true
                                                     :extra-env {"PATH" path "NO_COLOR" "1"
                                                                 "KUBECONFIG" "/dev/null"
                                                                 "GUMSHOE_ENV" "/nonexistent"}}
                                      "bb" args)]
    {:out (str/trim out) :err err :exit exit}))

(deftest a-real-front-door-run-hands-dry-run-to-the-book-test
  (fs/with-temp-dir [tmp {}]
    (let [casebook (fs/path tmp "casebook")
          bin (fs/path tmp "bin")
          book (fs/path casebook "runbooks" "up" "echo.clj")
          relative #(str (fs/relativize (fs/canonicalize casebook) (fs/canonicalize %)))
          path (str bin ":" (System/getenv "PATH"))]
      (fs/create-dirs (fs/parent book))
      (fs/create-dirs bin)
      (spit (str book) "(prn *command-line-args*)\n")
      (spit (str (fs/path bin "fzf")) "#!/bin/sh\nhead -n 1\n")
      (fs/set-posix-file-permissions (fs/path bin "fzf") "rwxr-xr-x")
      (spit (str (fs/path casebook "bb.edn"))
            (format "{:paths [%s \"runbooks\"]\n :tasks {gumshoe {:requires ([gumshoe.main :as main])\n :task (System/exit (main/front-door *command-line-args*))}}}"
                    (pr-str (relative "libraries"))))
      (testing "an option before the search terms"
        (let [{:keys [exit out err]} (bb-in casebook path "gumshoe" "--dry-run" "echo")]
          (is (zero? exit) err)
          (is (= "(\"--dry-run\")" out) err)))
      (testing "an option after a search term is refused and runs nothing"
        (let [{:keys [exit out err]} (bb-in casebook path "gumshoe" "--no-color" "echo" "--dry-run")]
          (is (= 1 exit))
          (is (str/includes? err "was not read") err)
          (is (= "" out)))))))
