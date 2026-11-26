;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ceph-upgrade-rehearsal-test
  "A rehearsal of the ceph upgrade writes no backup: it lists the directory
   and the files it would write, auth export among them, and asks the
   cluster for nothing."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.ceph :as ceph]
            [gumshoe.flow :as flow]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(binding [runbook/*loading* true]
  (load-file "tools/ceph/playbooks/upgrade.clj"))

(def ^:private backup! @(requiring-resolve 'playbooks.ceph.upgrade/backup!))
(def ^:private rehearsed-backups @(requiring-resolve 'playbooks.ceph.upgrade/rehearsed-backups))

(defn- backed-up
  [dry-run? directory]
  (let [out (java.io.StringWriter.)
        asked (atom [])]
    (with-redefs [ceph/ceph-stdout (fn [_connection & args] (swap! asked conj (vec args)) "output")]
      (let [result (binding [*out* out *err* out flow/*dry-run* dry-run?]
                     (backup! {:host "mon-1"} directory))]
        {:result result :asked @asked :out (stdout/strip-colors (str out))}))))

(deftest a-rehearsal-writes-nothing-and-asks-nothing
  (fs/with-temp-dir [dir {}]
    (let [directory (str (fs/path dir "ceph-backups" "mon-1-20261126-100000"))
          {:keys [result asked out]} (backed-up true directory)]
      (is (true? result))
      (is (empty? asked) "no ceph command ran")
      (is (not (fs/exists? (fs/path dir "ceph-backups"))) "no directory was created")
      (is (str/includes? out (str "would create " directory)))
      (is (str/includes? out (str "would write " (fs/path directory "auth-export.txt") " from: ceph auth export (holds every cephx key)"))))))

(deftest a-real-run-still-takes-the-backups
  (fs/with-temp-dir [dir {}]
    (let [directory (str (fs/path dir "b"))
          {:keys [result asked out]} (backed-up false directory)]
      (is (true? result))
      (is (some #{["auth" "export"]} asked))
      (is (fs/exists? (fs/path directory "auth-export.txt")))
      (is (str/includes? out (str "backups saved locally to " directory))))))

(deftest the-rehearsal-lists-every-backup
  (let [lines (rehearsed-backups "d")]
    (is (= "would create d (owner-only)" (first lines)))
    (is (= 7 (count (rest lines))))
    (testing "only auth export is marked as holding keys"
      (is (= 1 (count (filter #(str/includes? % "cephx") lines)))))))
