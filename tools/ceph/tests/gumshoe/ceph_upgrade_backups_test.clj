;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ceph-upgrade-backups-test
  "The ceph upgrade playbook keeps its local backups, auth export and its
   keys among them, owner-only."
  (:require [babashka.fs :as fs]
            [briko.fs :as bfs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.ceph :as ceph]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(binding [runbook/*loading* true]
  (load-file "tools/ceph/playbooks/upgrade.clj"))

(def ^:private take-backups! @(requiring-resolve 'playbooks.ceph.upgrade/take-backups!))

(defn- backed-up
  "{:result :out} of take-backups! into directory, with ceph answering
   answer for each backup's arguments."
  [directory answer]
  (let [out (java.io.StringWriter.)]
    (with-redefs [ceph/ceph-stdout (fn [_connection & args] (answer args))]
      (let [result (binding [*out* out *err* out] (take-backups! {:host "mon-1"} directory))]
        {:result result :out (stdout/strip-colors (str out))}))))

(deftest backups-are-owner-only
  (fs/with-temp-dir [dir {}]
    (let [directory (str (fs/path dir "ceph-backups" "mon-1-20261125-100000"))
          {:keys [result out]} (backed-up directory #(str "output of " (str/join " " %)))]
      (is (true? result))
      (is (= "rwx------" (bfs/permissions directory)))
      (is (= "rwx------" (bfs/permissions (fs/path dir "ceph-backups"))))
      (doseq [file (fs/list-dir directory)]
        (testing (str (fs/file-name file))
          (is (= "rw-------" (bfs/permissions file)))))
      (is (= "output of auth export" (slurp (str (fs/path directory "auth-export.txt")))))
      (is (str/includes? out "auth-export.txt holds the keys of every ceph client")))))

(deftest an-existing-backup-directory-is-refused
  (fs/with-temp-dir [dir {}]
    (let [directory (str (fs/path dir "mon-1"))]
      (fs/create-dirs directory)
      (let [{:keys [result out]} (backed-up directory (constantly "x"))]
        (is (false? result))
        (is (str/includes? out "could not create the backup directory"))
        (is (empty? (fs/list-dir directory)) "nothing is written into a directory the run did not create")))))

(deftest an-empty-backup-is-still-refused
  (fs/with-temp-dir [dir {}]
    (let [{:keys [result out]} (backed-up (str (fs/path dir "b")) #(when-not (= ["auth" "export"] %) "x"))]
      (is (false? result))
      (is (str/includes? out "backup 'auth-export' produced no output")))))
