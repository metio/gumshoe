;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.catalog-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [edamame.core]
            [gumshoe.catalog :as catalog]))

(deftest short-name-test
  (testing "strips the leading root and the .clj suffix"
    (is (= "kubernetes/nodes/cordon" (catalog/short-name "runbooks/kubernetes/nodes/cordon.clj")))
    (is (= "ceph/upgrade" (catalog/short-name "playbooks/ceph/upgrade.clj")))))

(deftest descriptions-are-read-from-literals
  (let [read #(catalog/description-of (edamame.core/parse-string-all %))]
    (is (= "Cordons a node" (read "(ns x) (mutation/book {:description \"Cordons a node\" :effect f})")))
    (is (= "Restarts a cluster" (read "(mutation/book (merge books/restart {:description \"Restarts a cluster\"}))")))
    (is (= "the last wins" (read "(mutation/book (merge {:description \"first\"} spec {:description \"the last wins\"}))")))
    (is (nil? (read "(mutation/book books/restart)")) "a bare spec carries nothing readable")
    (is (nil? (read "(mutation/book {:description (format \"%s\" x)})")))))

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

(def ^:private sha-a (apply str (repeat 40 "a")))
(def ^:private sha-b (apply str (repeat 40 "b")))

(defn- pin
  "A gumshoe checkout the way tools.gitlibs lays one out for a pinned sha."
  [tmp sha]
  (str (fs/path tmp "gitlibs" "libs" "io.github.metio" "gumshoe" sha)))

(defn- write-book!
  [root rel description]
  (let [file (fs/path root rel)]
    (fs/create-dirs (fs/parent file))
    (spit (str file) (format "(ns x)\n(detective/book {:description %s})\n" (pr-str description)))
    (str (fs/canonicalize file))))

(defn- upstream!
  [tmp sha]
  (let [root (pin tmp sha)]
    (write-book! root "runbooks/detectives/fabric.clj" (str "fabric at " sha))
    (write-book! root "tools/flux/runbooks/gitops.clj" "gitops")
    root))

(defn- casebook!
  [tmp]
  (let [root (str (fs/path tmp "casebook"))]
    (write-book! root "runbooks/hello/example.clj" "hello")
    root))

(defn- roots-of
  [casebook upstream]
  [(str (fs/path casebook "runbooks"))
   (str (fs/path upstream "runbooks"))
   (str (fs/path upstream "tools" "flux" "runbooks"))])

(defn- by-id
  [books]
  (into {} (map (juxt :id identity)) books))

(deftest a-book-is-named-by-its-path-relative-to-its-checkout-test
  (fs/with-temp-dir [tmp {}]
    (let [casebook (casebook! tmp)
          upstream (upstream! tmp sha-a)
          books (by-id (catalog/books (roots-of casebook upstream) casebook))]
      (is (= #{"runbooks/hello/example.clj" "runbooks/detectives/fabric.clj" "tools/flux/runbooks/gitops.clj"}
             (set (keys books))))
      (is (= (str (fs/canonicalize (fs/path upstream "runbooks/detectives/fabric.clj")))
             (:path (books "runbooks/detectives/fabric.clj"))))
      (is (:local? (books "runbooks/hello/example.clj")))
      (is (not (:local? (books "runbooks/detectives/fabric.clj")))))))

(deftest a-book-keeps-its-name-across-gumshoe-pins-test
  (fs/with-temp-dir [tmp {}]
    (let [casebook (casebook! tmp)
          ids (fn [sha] (->> (catalog/books (roots-of casebook (upstream! tmp sha)) casebook)
                             (map :id)
                             (set)))]
      (is (= (ids sha-a) (ids sha-b)))
      (is (= (str (fs/canonicalize (fs/path (pin tmp sha-b) "runbooks/detectives/fabric.clj")))
             (:path (catalog/resolve-book "runbooks/detectives/fabric.clj"
                                          (catalog/books (roots-of casebook (pin tmp sha-b)) casebook)
                                          casebook)))))))

(deftest a-book-in-a-git-checkout-is-named-relative-to-that-checkout-test
  (fs/with-temp-dir [tmp {}]
    (let [casebook (casebook! tmp)
          checkout (str (fs/path tmp "src" "gumshoe"))]
      (fs/create-dirs (fs/path checkout ".git"))
      (write-book! checkout "tools/flux/runbooks/gitops.clj" "gitops")
      (is (= ["tools/flux/runbooks/gitops.clj"]
             (map :id (catalog/books [(str (fs/path checkout "tools" "flux" "runbooks"))] casebook)))))))

(deftest a-casebook-book-shadows-the-upstream-book-of-the-same-path-test
  (fs/with-temp-dir [tmp {}]
    (let [casebook (casebook! tmp)
          upstream (upstream! tmp sha-a)
          local (write-book! casebook "runbooks/detectives/fabric.clj" "our fabric")
          ;; upstream roots first, as a classpath may order them
          roots (reverse (roots-of casebook upstream))
          books (catalog/books roots casebook)]
      (is (= 1 (count (filter #(= "runbooks/detectives/fabric.clj" (:id %)) books))))
      (is (= local (:path ((by-id books) "runbooks/detectives/fabric.clj"))))
      (is (= "our fabric" (:description ((by-id books) "runbooks/detectives/fabric.clj"))))
      (is (= local (:path (catalog/resolve-book "runbooks/detectives/fabric.clj" books casebook)))))))

(deftest resolve-book-test
  (fs/with-temp-dir [tmp {}]
    (let [casebook (casebook! tmp)
          upstream (upstream! tmp sha-a)
          books (catalog/books (roots-of casebook upstream) casebook)
          gitops (str (fs/canonicalize (fs/path upstream "tools/flux/runbooks/gitops.clj")))]
      (testing "an upstream book resolves by its listed path"
        (is (= gitops (:path (catalog/resolve-book "tools/flux/runbooks/gitops.clj" books casebook)))))
      (testing "a local file resolves relative to the casebook root"
        (is (= (str (fs/canonicalize (fs/path casebook "runbooks/hello/example.clj")))
               (:path (catalog/resolve-book "runbooks/hello/example.clj" books casebook)))))
      (testing "an absolute path runs as given"
        (is (= gitops (:path (catalog/resolve-book gitops books casebook)))))
      (testing "an unknown path is an error naming the near matches"
        (let [{:keys [path error near]} (catalog/resolve-book "runbooks/detectives/fabrik.clj" books casebook)]
          (is (nil? path))
          (is (re-find #"runbooks/detectives/fabrik.clj" error))
          (is (= "runbooks/detectives/fabric.clj" (first near))))
        (is (some #{"runbooks/detectives/fabric.clj"}
                  (:near (catalog/resolve-book "runbooks/fabric.clj" books casebook)))
            "a book with the same file name is a near match")
        (is (empty? (:near (catalog/resolve-book "runbooks/zzzzzzzzzzzzzzzzzz/qqqq.clj" books casebook))))))))
