;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.operations-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.cnpg.operations :as operations]))

(defn- cluster
  [& {:keys [fenced primary target healthy replicating failed name] :or {primary "pg-1" name "pg"
                                                                          healthy ["pg-1" "pg-2" "pg-3"]}}]
  {:metadata (cond-> {:namespace "db" :name name}
               fenced (assoc :annotations {:cnpg.io/fencedInstances fenced}))
   :status {:currentPrimary primary :targetPrimary (or target primary)
            :instancesStatus (cond-> {:healthy healthy}
                               replicating (assoc :replicating replicating)
                               failed (assoc :failed failed))}})

;; --- the fence annotation ---------------------------------------------------------

(deftest the-fenced-set-reads-the-annotation
  (is (= #{} (operations/fenced (cluster))))
  (is (= #{"pg-2" "pg-3"} (operations/fenced (cluster :fenced "[\"pg-3\",\"pg-2\"]"))))
  (is (= #{"*"} (operations/fenced (cluster :fenced "[\"*\"]"))))
  (is (= #{} (operations/fenced (cluster :fenced "  "))))
  (doseq [broken ["pg-2" "{\"a\":1}" "[1,2]" "[\"pg-2\""]]
    (is (nil? (operations/fenced (cluster :fenced broken))) broken)))

(deftest fenced-reads-the-wildcard
  (is (operations/fenced? (cluster :fenced "[\"pg-2\"]") "pg-2"))
  (is (not (operations/fenced? (cluster :fenced "[\"pg-2\"]") "pg-3")))
  (is (operations/fenced? (cluster :fenced "[\"*\"]") "pg-3"))
  (is (not (operations/fenced? (cluster :fenced "nonsense") "pg-3"))))

(deftest fencing-follows-the-operator-rules
  (is (= #{"pg-2"} (operations/with-fence #{} "pg-2")))
  (is (= #{"pg-2" "pg-3"} (operations/with-fence #{"pg-3"} "pg-2")))
  (is (= #{"*"} (operations/with-fence #{"pg-2" "pg-3"} "*")) "* replaces the list")
  (is (= #{"*"} (operations/with-fence #{"*"} "pg-2")) "a name beside * changes nothing")
  (is (= #{"pg-3"} (operations/without-fence #{"pg-2" "pg-3"} "pg-2")))
  (is (= #{} (operations/without-fence #{"*"} "*")))
  (is (= #{} (operations/without-fence #{"pg-2"} "*")) "lifting * clears every fence")
  (is (nil? (operations/without-fence #{"*"} "pg-2")) "one name out of * cannot be expressed"))

(deftest the-annotation-is-written-sorted-and-removed-when-empty
  (is (= "cnpg.io/fencedInstances=[\"pg-2\",\"pg-3\"]" (operations/annotation-arg #{"pg-3" "pg-2"})))
  (is (= "cnpg.io/fencedInstances=[\"*\"]" (operations/annotation-arg #{"*"})))
  (is (= "cnpg.io/fencedInstances-" (operations/annotation-arg #{})))
  (is (thrown? clojure.lang.ExceptionInfo (operations/annotation-arg nil))
      "a change that cannot be expressed never clears the fences"))

;; --- picks --------------------------------------------------------------------------

(deftest picks-parse-like-the-plugin
  (is (= {:namespace "db" :cluster "pg" :instance "pg-2"} (operations/parse-pick "db/pg pg-2")))
  (is (= {:namespace "db" :cluster "pg" :instance "pg-2"} (operations/parse-pick " db/pg 2 ")) "a bare number")
  (is (= {:namespace "db" :cluster "pg" :instance "*"} (operations/parse-pick "db/pg *")))
  (doseq [bad [nil "" "pg pg-2" "db/pg" "db/pg pg-2 extra"]]
    (is (nil? (operations/parse-pick bad)) (pr-str bad))))

;; --- switchover -----------------------------------------------------------------------

(deftest a-switchover-goes-to-a-healthy-unfenced-replica
  (is (nil? (operations/switchover-refusal (cluster) "pg-2")))
  (is (= "pg-1 is the primary already" (operations/switchover-refusal (cluster) "pg-1")))
  (is (= "a switchover to pg-3 is under way already" (operations/switchover-refusal (cluster :target "pg-3") "pg-2")))
  (is (= "pg-2 is fenced and cannot be promoted" (operations/switchover-refusal (cluster :fenced "[\"pg-2\"]") "pg-2")))
  (is (= "pg-2 is fenced and cannot be promoted" (operations/switchover-refusal (cluster :fenced "[\"*\"]") "pg-2")))
  (is (re-find #"^pg-3 is not among the healthy instances"
               (operations/switchover-refusal (cluster :healthy ["pg-1" "pg-2"] :replicating ["pg-3"]) "pg-3")))
  (is (= "the cluster no longer exists" (operations/switchover-refusal nil "pg-2"))))

(deftest switchover-picks-list-every-safe-target
  (is (= ["db/a pg-2" "db/a pg-3" "db/pg pg-3"]
         (operations/switchover-picks {:items [(cluster :fenced "[\"pg-2\"]")
                                               (cluster :name "a")
                                               (cluster :name "b" :target "pg-2")]})))
  (is (= [] (operations/switchover-picks nil))))

(deftest the-promotion-runs-the-plugin-binary-with-its-context
  (is (= ["kubectl-cnpg" "promote" "pg" "pg-2" "--context=staging" "--namespace=db"]
         (operations/promote-args "staging" {:namespace "db" :cluster "pg" :instance "pg-2"}))))

;; --- fencing --------------------------------------------------------------------------

(deftest fence-refusals
  (is (nil? (operations/fence-refusal (cluster) "pg-2")))
  (is (nil? (operations/fence-refusal (cluster :fenced "[\"pg-3\"]") "*")))
  (is (= "pg-2 is fenced already" (operations/fence-refusal (cluster :fenced "[\"pg-2\"]") "pg-2")))
  (is (= "pg-2 is fenced already" (operations/fence-refusal (cluster :fenced "[\"*\"]") "pg-2")))
  (is (= "pg-9 is not an instance of this cluster" (operations/fence-refusal (cluster) "pg-9")))
  (is (re-find #"not a JSON list" (operations/fence-refusal (cluster :fenced "pg-2") "pg-3")))
  (is (nil? (operations/fence-refusal (cluster :healthy ["pg-2"] :failed ["pg-1"]) "pg-1")) "a failed instance can be fenced"))

(deftest unfence-refusals
  (is (nil? (operations/unfence-refusal (cluster :fenced "[\"pg-2\"]") "pg-2")))
  (is (nil? (operations/unfence-refusal (cluster :fenced "[\"*\"]") "*")))
  (is (= "pg-2 is not fenced" (operations/unfence-refusal (cluster) "pg-2")))
  (is (re-find #"lift the fence on all of them" (operations/unfence-refusal (cluster :fenced "[\"*\"]") "pg-2")))
  (is (re-find #"not a JSON list" (operations/unfence-refusal (cluster :fenced "oops") "pg-2"))))

(deftest fence-and-unfence-picks
  (is (= ["db/pg pg-1" "db/pg pg-3" "db/pg *"] (operations/fence-picks {:items [(cluster :fenced "[\"pg-2\"]")]})))
  (is (= [] (operations/fence-picks {:items [(cluster :fenced "[\"*\"]")]})) "everything is fenced")
  (is (= ["db/pg pg-2" "db/pg pg-3"] (operations/unfence-picks {:items [(cluster :fenced "[\"pg-3\",\"pg-2\"]") (cluster :name "x")]})))
  (is (= [] (operations/unfence-picks {:items [(cluster :fenced "broken")]})) "an unreadable annotation offers nothing"))
