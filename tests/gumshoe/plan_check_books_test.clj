;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.plan-check-books-test
  "Every book of the mutation-books harness holds its dry-run plan to its
   effect: over its permitted scenario's recorded cluster, gumshoe.plan-check
   runs the book's :derive phase once and finds every object the effect
   plan touches named by the confirmation items, with :items and :effect
   reading nothing themselves."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation-books-test :as books]
            [gumshoe.plan-check :as plan-check]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]))

(def ^:private touches
  "What an effect touches where plan-check cannot tell it from the
   arguments, by book path. velero creates a DeleteBackupRequest by
   generateName; the Backup its spec names is what it deletes."
  {"tools/velero/runbooks/delete_backup.clj"
   (fn [[op _ stdin]]
     (when (and (= :kubectl-stdin op) (str/includes? (str stdin) "DeleteBackupRequest"))
       (let [[_ name] (re-find #"\"backupName\":\"([^\"]+)\"" (str stdin))]
         [{:type "backups.velero.io" :namespace "velero" :name name}])))})

(def ^:private answer @#'books/answer)

(defn- checked
  "plan-check/check of the book at path over its permitted scenario, with
   its spec passed through f and touches as the scenario's :touches."
  ([path] (checked path identity (get touches path)))
  ([path f touches]
   (let [{:keys [objects target opts ssh dry-run]} (get-in books/books [path :permitted])
         spec (f (books/load-spec path))]
     (binding [ssh/*user-config* nil]
       (books/with-book-redefs
         path
         #(with-redefs [shell/execute (fn [& argv] (answer {:objects objects :answering? true :ssh ssh :dry-run dry-run} (vec argv)))
                        kubectl/current-context (constantly "ctx")
                        kubectl/current-cluster (constantly "cluster")]
            (plan-check/check spec {:target target :opts opts :touches touches})))))))

(deftest every-book-lists-what-its-effect-touches
  (is (<= 45 (count books/books)) "the harness still holds the books")
  (doseq [path (sort (keys books/books))]
    (testing path
      (let [{:keys [ok? failure touched]} (checked path)]
        (is ok? (str/join "\n" failure))
        (is (seq touched) "the effect touches something the check compared")))))

(deftest negative-controls
  (testing "cronjobs/start listing only the CronJob leaves out the Job it creates"
    (let [{:keys [ok? missing]} (checked "runbooks/kubernetes/cronjobs/start.clj"
                                         #(assoc % :items (fn [{:keys [target]}] [target])) nil)]
      (is (not ok?))
      (is (= [{:type "job" :namespace "batch" :name "nightly-abcde"}] missing))))
  (testing "velero delete_backup without :touches cannot be told"
    (let [{:keys [ok? unidentified]} (checked "tools/velero/runbooks/delete_backup.clj" identity nil)]
      (is (not ok?))
      (is (= 1 (count unidentified)))))
  (testing "a book whose :items reads again is refused before the read"
    (let [{:keys [ok? problem]} (checked "runbooks/kubernetes/cronjobs/suspend.clj"
                                         #(assoc % :items (fn [{:keys [context]}]
                                                            [(str (kubectl/get-namespaced-resource context "batch" "cronjobs" "nightly"))]))
                                         nil)]
      (is (not ok?))
      (is (str/includes? (str problem) "its :items reached outside the process")))))
