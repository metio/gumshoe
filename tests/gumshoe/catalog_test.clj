;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.catalog-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.catalog :as catalog]))

(deftest short-name-test
  (testing "strips the leading root and the .clj suffix"
    (is (= "kubernetes/nodes/cordon" (catalog/short-name "runbooks/kubernetes/nodes/cordon.clj")))
    (is (= "ceph/upgrade" (catalog/short-name "playbooks/ceph/upgrade.clj")))))

(deftest books-discovery-test
  (let [books (catalog/books)]
    (testing "every book on disk is discovered"
      (is (< 40 (count books))))
    (testing "each book has a path and a fuzzy-matchable name"
      (is (every? (comp string? :path) books))
      (is (every? (comp seq :name) books)))
    (testing "every book's declared description is read (none fall back to nil)"
      (is (empty? (remove :description books))
          "a book whose :description can not be read statically should be fixed or noted"))
    (testing "a known book resolves with its description"
      (let [cordon (first (filter #(= "kubernetes/nodes/cordon" (:name %)) books))]
        (is (some? cordon))
        (is (string? (:description cordon)))))))

(deftest book-at-never-throws-test
  (testing "an unparseable path still yields an entry, labelled by its path"
    (let [b (catalog/book-at "does/not/exist.clj")]
      (is (= "does/not/exist.clj" (:path b)))
      (is (nil? (:description b))))))

(deftest resolve-path-by-suffix-test
  (let [catalog [{:path "/home/x/gumshoe/tools/flux/runbooks/gitops.clj" :name "gitops"}
                 {:path "/root/.gitlibs/libs/io.github.metio/gumshoe/abc/runbooks/investigate.clj" :name "investigate"}]]
    (testing "a book-dir-relative suffix resolves to the absolute classpath path"
      (is (= "/home/x/gumshoe/tools/flux/runbooks/gitops.clj"
             (catalog/resolve-path "tools/flux/runbooks/gitops.clj" catalog)))
      (is (= "/root/.gitlibs/libs/io.github.metio/gumshoe/abc/runbooks/investigate.clj"
             (catalog/resolve-path "runbooks/investigate.clj" catalog))))
    (testing "an unknown suffix falls back to itself, so a bare monorepo path still runs"
      (is (= "runbooks/nope.clj" (catalog/resolve-path "runbooks/nope.clj" catalog))))))
