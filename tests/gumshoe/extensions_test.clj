;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.extensions-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.extensions :as extensions]))

(defn- write-extension!
  "Materializes a minimal extension repo on disk and returns its root."
  [root]
  (fs/create-dirs (fs/path root "libraries" "acme"))
  (fs/create-dirs (fs/path root "runbooks" "acme"))
  (spit (str (fs/path root "gumshoe.edn"))
        (pr-str {:paths ["libraries"] :book-paths ["runbooks"] :plugins ['acme.registrations]}))
  (spit (str (fs/path root "libraries" "acme" "registrations.clj")) "(ns acme.registrations)\n")
  (str root))

(deftest manifest-driven-discovery-test
  (fs/with-temp-dir [dir {}]
    (let [root (write-extension! (fs/path dir "casebook-acme"))
          roots [root]]
      (testing "book dirs come from the manifest's :book-paths, resolved under the root"
        (is (= [(str (fs/path root "runbooks"))] (extensions/book-paths roots))))
      (testing "code dirs come from :paths"
        (is (= [(str (fs/path root "libraries"))] (extensions/code-dirs roots))))
      (testing "the launch classpath includes gumshoe's own plus the extension's code"
        (let [cp (extensions/classpath-string roots)]
          (is (str/includes? cp (str (fs/path root "libraries"))))
          (is (str/includes? cp "libraries"))))
      (testing "activating returns the plugin namespaces the manifest declares"
        (is (= ['acme.registrations] (extensions/activate! roots)))))))

(deftest no-manifest-is-not-fatal-test
  (fs/with-temp-dir [dir {}]
    (let [root (str (fs/create-dirs (fs/path dir "not-an-extension")))]
      (testing "a directory without a gumshoe.edn contributes nothing and never throws"
        (is (empty? (extensions/book-paths [root])))
        (is (nil? (extensions/classpath-string [root])))))))

(deftest no-extensions-means-plain-classpath-test
  (testing "with nothing configured, launch defers to bb.edn - classpath-string is nil"
    (is (nil? (extensions/classpath-string [])))))
