;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.jaas-observed-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.runbook :as runbook]
            [gumshoe.mutation :as mutation]
            [gumshoe.tools.jaas :as jaas]))

(defn- snippet [generation reason observed & {:keys [status-observed]}]
  {:metadata {:namespace "ns" :name "s" :generation generation}
   :status (cond-> {:conditions [(cond-> {:type "Ready" :status "True" :reason reason}
                                   observed (assoc :observedGeneration observed))]}
             status-observed (assoc :observedGeneration status-observed))})

(deftest a-ready-reason-from-before-the-resume-is-not-a-resume
  (is (false? (boolean (jaas/resumed? (snippet 5 "Synced" 3 :status-observed 3)))))
  (is (false? (boolean (jaas/resumed? (snippet 5 "Synced" nil :status-observed 4))))
      "without a condition generation, status.observedGeneration decides"))

(deftest a-ready-reason-at-the-current-generation-is-a-resume
  (is (true? (jaas/resumed? (snippet 5 "Synced" 5))))
  (is (true? (jaas/resumed? (snippet 5 "EvaluationFailed" 6))) "any reason but Suspended")
  (is (true? (jaas/resumed? (snippet 5 "Synced" nil))) "no generation recorded says nothing is behind")
  (is (false? (boolean (jaas/resumed? (snippet 5 "Suspended" 5))))))

(deftest a-suspended-reason-must-be-observed-too
  (is (true? (jaas/suspended? (snippet 4 "Suspended" 4))))
  (is (false? (boolean (jaas/suspended? (snippet 4 "Suspended" 2)))) "left from an earlier suspend")
  (is (false? (boolean (jaas/suspended? (snippet 4 "Synced" 4))))))

(deftest the-books-post-check-on-the-observed-state
  (doseq [[path check-name] [["tools/jaas/runbooks/jaas/resume.clj" "resumed-check"]
                             ["tools/jaas/runbooks/jaas/suspend.clj" "suspended-check"]]]
    (testing path
      (with-redefs [mutation/book identity]
        (binding [runbook/*loading* true] (load-file path)))
      (let [ns-sym (symbol (str "runbooks.jaas." (if (= check-name "resumed-check") "resume" "suspend")))
            make (requiring-resolve (symbol (str ns-sym) check-name))
            stale (if (= check-name "resumed-check") (snippet 5 "Synced" 3) (snippet 4 "Suspended" 2))
            fresh (if (= check-name "resumed-check") (snippet 5 "Synced" 5) (snippet 4 "Suspended" 4))]
        (with-redefs [jaas/snippet (constantly stale)]
          (is (not ((:check (make "ctx" "ns" "s" "ns/s")))) "stale status does not pass"))
        (with-redefs [jaas/snippet (constantly fresh)]
          (is ((:check (make "ctx" "ns" "s" "ns/s"))) "observed status passes"))
        (with-redefs [jaas/snippet (constantly nil)]
          (is (not ((:check (make "ctx" "ns" "s" "ns/s")))) "no answer does not pass"))))))
