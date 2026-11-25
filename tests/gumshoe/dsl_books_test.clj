;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dsl-books-test
  "The gumshoe.dsl forms of four books - a toggle pair (cronjobs suspend and
   resume), a scale and a delete - kept under tests/edn-books and loaded as
   `bb run` loads them, compile to changes that keep every transcript of
   the book in tests/golden/mutation_books.edn, and that conformance judges
   as it judges the book."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.conformance :as conformance]
            [gumshoe.dsl :as dsl]
            [gumshoe.edn-book :as edn-book]
            [gumshoe.edn-reader :as reader]
            [gumshoe.mutation-books-test :as harness]
            [gumshoe.ssh :as ssh]
            [gumshoe.verify :as verify]))

(def books
  "Each book with a DSL form, by the path of its Clojure form."
  {"runbooks/kubernetes/cronjobs/suspend.clj" "tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn"
   "runbooks/kubernetes/cronjobs/resume.clj" "tests/edn-books/runbooks/kubernetes/cronjobs/resume.edn"
   "runbooks/kubernetes/workloads/scale.clj" "tests/edn-books/runbooks/kubernetes/workloads/scale.edn"
   "runbooks/kubernetes/objects/delete.clj" "tests/edn-books/runbooks/kubernetes/objects/delete.edn"})

(defn- source
  [path]
  (reader/read-book path))

(defn- compiled
  "The change the file holds, loaded as `bb run` loads it."
  [path]
  (edn-book/load-book path))

(def ^:private golden
  (delay (edn/read-string {:default (fn [_ v] v)} (slurp harness/golden-path))))

(defn- canonical-surface
  "A golden surface as the canonical change map shows it: the change map
   keeps the empty message in :select, where the book's spec writes it at
   the top, as gumshoe.change/normalize moves it."
  [{:keys [empty-message] :as surface}]
  (cond-> (assoc surface :empty-message nil)
    empty-message (assoc-in [:select :empty-message] empty-message)))

(deftest every-dsl-form-keeps-its-books-transcripts
  (doseq [[clj edn-path] books]
    (testing clj
      (let [expected (update (get @golden clj) :surface canonical-surface)
            actual (harness/book-transcripts clj (compiled edn-path))]
        (is (seq (dissoc expected :surface)) "the golden file holds the book's scenarios")
        (is (= (set (keys expected)) (set (keys actual))) "the same scenarios")
        (doseq [[scenario transcript] actual]
          (testing scenario
            (is (= (get expected scenario) transcript))))))))

(deftest a-changed-dsl-form-changes-the-transcripts
  (testing "the comparison above can fail: one changed word shows"
    (let [clj "runbooks/kubernetes/cronjobs/suspend.clj"
          changed (dsl/compile (assoc-in (source (get books clj)) [:state :says] "CronJob {target} is paused"))]
      (is (not= (dissoc (get @golden clj) :surface)
                (dissoc (harness/book-transcripts clj changed) :surface))))))

(deftest conformance-judges-each-dsl-form-as-it-judges-its-book
  (doseq [[clj edn-path] books]
    (testing clj
      (let [book (harness/load-spec clj)
            dsl (compiled edn-path)]
        (is (= (conformance/spec-problems {:spec book :select (:select book)})
               (conformance/spec-problems {:spec dsl :select (:select dsl)})))
        (is (= (conformance/spec-guard-problems {:spec book})
               (conformance/spec-guard-problems {:spec dsl})))
        (is (= [] (conformance/spec-problems {:spec dsl :select (:select dsl)})))))))

(deftest every-dsl-post-check-fails-closed
  (binding [ssh/*user-config* nil]
    (doseq [[clj edn-path] books]
      (testing clj
        (let [checks (harness/post-checks-of (compiled edn-path) (get harness/books clj))]
          (is (seq checks))
          (is (= [] (verify/fail-closed-findings checks))))))))

(deftest each-dsl-form-declares-what-its-book-declares
  (doseq [[clj edn-path] books]
    (testing clj
      (let [book (change/normalize (harness/load-spec clj))
            dsl (compiled edn-path)]
        (doseq [k [:description :options :prerequisites :confirm]]
          (is (= (get book k) (get dsl k)) (str k)))
        (is (= (dissoc (:select book) :candidates) (dissoc (:select dsl) :candidates)))
        (is (= (map #(dissoc % :parse) (:subjects book)) (map #(dissoc % :parse) (:subjects dsl))))
        (is (= (set (keys (dissoc book :reads :values :recovery))) (set (keys (dissoc dsl :reads :values :recovery))))
            "the same keys, beside the reads and values the DSL names where the book derives them")))))

(deftest the-delete-form-adds-recovery-and-opts-out-of-skipping
  (let [dsl (compiled (get books "runbooks/kubernetes/objects/delete.clj"))]
    (is (str/starts-with? ((:recovery dsl) {:target "deployments shop/web"})
                          "A deleted object is gone from the cluster: recreate deployments shop/web from"))
    (is (= {:subject-pin :on :strict-reads :on :post-check :on
            :done {:off "a pick that names no object is refused as a likely typo instead of skipped as already deleted"}
            :destructive-confirmation :on :recovery :on}
           (dsl/safeguards-of dsl)))
    (is (nil? (:done dsl)))))

(deftest each-dsl-form-is-plain-data
  (doseq [edn-path (vals books)]
    (testing edn-path
      (let [data (source edn-path)]
        (is (= data (edn/read-string (pr-str data))) "it prints and reads back unchanged")
        (is (= data (dsl/source-of (dsl/compile data))) "the compiled change carries it")))))

(defn- readme-example
  "The DSL example in the README: the first clojure block after its DSL
   heading."
  []
  (let [readme (slurp "README.md")
        from (str/index-of readme "## Changes written as data")
        block (re-find #"(?s)```clojure\n(.*?)```" (subs readme from))]
    (edn/read-string (second block))))

(deftest the-readme-example-compiles-and-is-the-suspend-form
  (is (= (source "tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn") (readme-example))))
