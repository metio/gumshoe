;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.files-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.files :as files]))

(deftest a-private-directory-and-file-are-owner-only
  (fs/with-temp-dir [dir {}]
    (let [directory (files/create-private-directory! (fs/path dir "backups" "host-1"))
          file (files/write-private! (fs/path directory "auth-export.txt") "[client.admin]\n\tkey = s3cr3t\n")]
      (is (= "rwx------" (files/mode directory)))
      (is (= "rwx------" (files/mode (fs/path dir "backups"))) "a missing parent is created owner-only too")
      (is (= "rw-------" (files/mode file)))
      (is (= "[client.admin]\n\tkey = s3cr3t\n" (slurp file))))))

(deftest nothing-that-exists-is-reused
  (fs/with-temp-dir [dir {}]
    (let [directory (files/create-private-directory! (fs/path dir "d"))
          existing (str (fs/path directory "x.txt"))]
      (spit existing "planted")
      (testing "an existing directory may be someone else's"
        (is (thrown? java.nio.file.FileAlreadyExistsException (files/create-private-directory! directory))))
      (testing "an existing file is not written through"
        (is (thrown? java.nio.file.FileAlreadyExistsException (files/write-private! existing "secret")))
        (is (= "planted" (slurp existing))))
      (testing "nor is a symlink"
        (let [target (str (fs/path dir "elsewhere.txt"))
              link (str (fs/path directory "link.txt"))]
          (fs/create-sym-link link target)
          (is (thrown? java.nio.file.FileAlreadyExistsException (files/write-private! link "secret")))
          (is (not (fs/exists? target))))))))

(deftest an-existing-parent-keeps-its-permissions
  (fs/with-temp-dir [dir {}]
    (let [parent (str (fs/path dir "shared"))]
      (fs/create-dirs parent {:posix-file-permissions "rwxr-xr-x"})
      (let [directory (files/create-private-directory! (fs/path parent "run"))]
        (is (= "rwxr-xr-x" (files/mode parent)))
        (is (= "rwx------" (files/mode directory)))
        (is (str/starts-with? directory (str (fs/absolutize parent))))))))
