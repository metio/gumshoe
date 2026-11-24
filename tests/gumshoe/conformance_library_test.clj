;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.conformance-library-test
  "gumshoe.conformance run against a casebook it does not know, built in a
   temporary directory."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.conformance :as conformance]))

(def ^:private header
  (str ";; SPDX-FileCopyrightText: The casebook Authors\n"
       ";; SPDX-License-" "Identifier: Apache-2.0\n\n"))

(def ^:private good-book
  (str header
       "(ns casebook.hello.ok\n"
       "  \"A book that follows every rule. For a change, use runbook/execute! or\n"
       "   gumshoe.mutation/book instead; neither mention is a call.\"\n"
       "  (:require [gumshoe.detective]))\n\n"
       ";; (mutation/book {...}) in a comment is not a call either\n"
       "(gumshoe.detective/book\n"
       " {:description \"Always clean\"\n"
       "  :options {:namespace {:desc \"The namespace\" :alias :n}}\n"
       "  :detectives [{:name \"ok\" :detect (fn [_] [])}]})\n"))

(def ^:private spec-library
  (str header
       "(ns casebook.spec\n"
       "  \"Reads with aliased keywords, a var quote, a syntax quote and a reader conditional.\"\n"
       "  (:require [clojure.spec.alpha :as s]))\n\n"
       "(s/def ::s/name string?)\n"
       "(def kw ::local)\n"
       "(def v #'kw)\n"
       "(defmacro twice [x] (list 'do x x))\n"
       "(defmacro quoted [x] `(do ~x ~@[x]))\n"
       "(def platform #?(:bb :bb :clj :clj))\n"))

(def ^:private unbalanced-library
  (str header
       "(ns casebook.moodle.database\n"
       "  \"Has an unbalanced form.\"\n"
       "  (:require [clojure.string :as str]))\n\n"
       "(defn database-name [release]\n"
       "  (str/replace release \"-\" \"_\")\n"))

(def ^:private unbalanced-book
  (str header
       "(ns casebook.moodle.adopt.status\n"
       "  \"Has an unbalanced form.\"\n"
       "  (:require [gumshoe.detective :as detective]))\n\n"
       "(detective/book\n"
       " {:description \"Status\"\n"
       "  :detectives [{:name \"status\" :detect (fn [_] [])}]}))\n"))

(def ^:private sloppy-book
  (str ";; no license here\n"
       "(ns casebook.sloppy)\n\n"
       "(runbook/execute!\n"
       " {:options {:name {:alias :n}\n"
       "            :node {:desc \"The node\" :alias :n :colour :red}\n"
       "            :host {:desc \"The host\" :alias :h}}\n"
       "  :action (fn [_ _] true)})\n"))

(def ^:private twice-book
  (str header
       "(ns casebook.twice\n"
       "  \"Enters the harness twice, once from a helper.\"\n"
       "  (:require [gumshoe.runbook :as runbook]))\n\n"
       "(defn- again [] (runbook/execute! {:description \"two\" :action (fn [_ _] true)}))\n\n"
       "(runbook/execute! {:description \"one\" :action (fn [_ _] true)})\n"))

(defn- casebook!
  "A casebook in a temporary directory: {relative path -> content}."
  [files]
  (let [root (fs/create-temp-dir {:prefix "casebook"})]
    (doseq [[path content] files
            :let [file (fs/path root path)]]
      (fs/create-dirs (fs/parent file))
      (spit (str file) content))
    (str root)))

(defn- roots
  [root]
  {:books [(str (fs/path root "runbooks")) (str (fs/path root "playbooks"))]
   :libraries [(str (fs/path root "libraries"))]})

(defn- by-file
  "{file name -> sorted checks} of findings."
  [findings]
  (update-vals (group-by #(str (fs/file-name (:file %))) findings)
               #(vec (sort (map :check %)))))

(def ^:private reset-library
  (str header
       "(ns casebook.reset\n"
       "  \"What the casebook still defines after gumshoe took over action-in-flight?.\")\n\n"
       "(defn release-name [target] (second (re-find #\"/(.*)\" target)))\n"))

(def ^:private unresolved-book
  (str header
       "(ns casebook.moodle.reset\n"
       "  \"Calls a function its library no longer defines, inside the action.\"\n"
       "  (:require [casebook.reset :as reset]\n"
       "            [gumshoe.runbook :as runbook]))\n\n"
       "(runbook/execute!\n"
       " {:description \"Reset\"\n"
       "  :action (fn [_ _] (reset/action-in-flight? (reset/release-name \"apps/moodle\")))})\n"))

(defn- marker-book
  "A book whose action writes `marker`, so a run of it leaves a trace."
  [marker]
  (str header
       "(ns casebook.marker\n"
       "  \"Writes a marker file when it runs.\"\n"
       "  (:require [casebook.reset :as reset]\n"
       "            [gumshoe.runbook :as runbook]))\n\n"
       "(runbook/execute!\n"
       " {:description \"Marker\"\n"
       "  :action (fn [_ _] (spit " (pr-str marker) " (reset/release-name \"apps/ran\")) true)})\n"))

(deftest a-book-that-parses-but-does-not-load-is-named-with-its-symbol
  (let [root (casebook! {"libraries/casebook/reset.clj" reset-library
                         "runbooks/moodle/reset.clj" unresolved-book
                         "runbooks/hello/ok.clj" good-book})]
    (try
      (let [findings (conformance/findings-in (roots root))
            [finding] findings]
        (is (= {"reset.clj" [:load]} (by-file findings))
            "only the book that cannot load, and nothing for the library it requires")
        (is (str/ends-with? (:file finding) "runbooks/moodle/reset.clj"))
        (is (str/includes? (:message finding) "reset/action-in-flight?") (:message finding)))
      (testing "verify! fails and names the book"
        (let [err (java.io.StringWriter.)
              e (binding [*out* (java.io.StringWriter.) *err* err]
                  (try (conformance/verify! (roots root)) nil
                       (catch clojure.lang.ExceptionInfo e e)))]
          (is (= 1 (:babashka/exit (ex-data e))))
          (is (str/includes? (str err) "runbooks/moodle/reset.clj"))
          (is (str/includes? (str err) "reset/action-in-flight?"))))
      (testing ":load? false leaves loading to the caller"
        (is (= [] (conformance/findings-in (assoc (roots root) :load? false)))))
      (finally (fs/delete-tree root)))))

(deftest loading-a-book-does-not-run-it
  (let [marker (str (fs/path (fs/create-temp-dir {:prefix "marker"}) "ran"))
        root (casebook! {"libraries/casebook/reset.clj" reset-library
                         "runbooks/marker.clj" (marker-book marker)})]
    (try
      (is (= [] (conformance/findings-in (roots root))))
      (is (not (fs/exists? marker)) "the book's action never ran")
      (finally (fs/delete-tree root) (fs/delete-tree (fs/parent marker))))))

(deftest a-casebook-that-follows-every-rule-has-no-findings
  (let [root (casebook! {"runbooks/hello/ok.clj" good-book
                         "libraries/casebook/spec.clj" spec-library})]
    (try
      (is (= [] (conformance/findings-in (roots root))))
      (testing "verify! reports the files it checked"
        (is (= 2 (binding [*out* (java.io.StringWriter.)] (conformance/verify! (roots root))))))
      (finally (fs/delete-tree root)))))

(deftest a-casebook-file-with-an-unbalanced-form-is-named
  (let [root (casebook! {"runbooks/hello/ok.clj" good-book
                         "runbooks/moodle/adopt/status.clj" unbalanced-book
                         "libraries/casebook/moodle/database.clj" unbalanced-library})]
    (try
      (let [findings (conformance/findings-in (roots root))]
        (is (= {"status.clj" [:unparseable] "database.clj" [:unparseable]} (by-file findings))
            "an unparseable file is reported once and skipped by the structural checks")
        (is (every? #(str/includes? (:message %) "does not parse") findings)))
      (testing "verify! fails with exit code 1 and names each file"
        (let [err (java.io.StringWriter.)
              e (binding [*out* (java.io.StringWriter.) *err* err]
                  (try (conformance/verify! (roots root)) nil
                       (catch clojure.lang.ExceptionInfo e e)))]
          (is (= 1 (:babashka/exit (ex-data e))))
          (is (str/includes? (str err) "runbooks/moodle/adopt/status.clj"))
          (is (str/includes? (str err) "libraries/casebook/moodle/database.clj"))
          (is (not (str/includes? (str err) "ok.clj")))))
      (finally (fs/delete-tree root)))))

(deftest a-casebook-book-that-breaks-the-structural-rules-is-named
  (let [root (casebook! {"runbooks/sloppy.clj" sloppy-book
                         "playbooks/twice.clj" twice-book})]
    (try
      (let [findings (conformance/findings-in (roots root))
            messages (map :message (filter #(= "sloppy.clj" (str (fs/file-name (:file %)))) findings))]
        (is (= {"sloppy.clj" [:description :license :load :namespace-docstring :options :options :options]
                "twice.clj" [:harness]}
               (by-file findings)))
        (is (some #(str/includes? % "option :name has no :desc") messages))
        (is (some #(str/includes? % "unknown option keys [:colour]") messages))
        (is (some #(str/includes? % "colliding option aliases") messages))
        (is (some #(str/includes? % "needs a :description") messages)))
      (testing "a license the caller names must be the one each file carries"
        (is (= #{"sloppy.clj" "twice.clj"}
               (set (for [f (conformance/findings-in (assoc (roots root) :license "0BSD"))
                          :when (= :license (:check f))]
                      (str (fs/file-name (:file f))))))))
      (finally (fs/delete-tree root)))))

(deftest roots-a-casebook-does-not-have-hold-no-files
  (is (= [] (conformance/clj-files ["does-not-exist"]))))

(def ^:private pair-book
  (str header
       "(ns casebook.moodle.pair\n"
       "  \"Destructures namespace-name-map as a pair inside an effect.\"\n"
       "  (:require [gumshoe.kubectl :as kubectl]\n"
       "            [gumshoe.mutation :as mutation]))\n\n"
       "(mutation/book\n"
       " {:description \"Pair\"\n"
       "  :select {:mode :namespaced}\n"
       "  :effect (fn [{:keys [target]}]\n"
       "            (let [[ns name] (kubectl/split-namespace-name target)]\n"
       "              [ns name]))})\n"))

(def ^:private pair-library
  (str header
       "(ns casebook.targets\n"
       "  \"Destructures namespace-name-map as a pair in a for :let, and by key.\"\n"
       "  (:require [gumshoe.kubectl :as kubectl]))\n\n"
       "(defn names [targets]\n"
       "  (for [t targets :let [[_ n] (kubectl/namespace-name-map t)]] n))\n\n"
       "(defn namespaces [targets]\n"
       "  (for [t targets :let [{:keys [namespace]} (kubectl/namespace-name-map t)]] namespace))\n"))

(deftest a-vector-binding-of-namespace-name-map-is-named
  (let [root (casebook! {"runbooks/moodle/pair.clj" pair-book
                         "libraries/casebook/targets.clj" pair-library})]
    (try
      (let [findings (conformance/findings-in (roots root))]
        (is (= {"pair.clj" [:destructuring] "targets.clj" [:destructuring]} (by-file findings)))
        (is (some #(str/includes? (:message %) "binds kubectl/split-namespace-name to the vector [ns name]") findings))
        (is (some #(str/includes? (:message %) "{:namespace .. :name ..}") findings)))
      (finally (fs/delete-tree root)))))
