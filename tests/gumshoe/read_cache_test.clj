;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.read-cache-test
  "kubectl/*read-cache*: within one binding a read is fetched once, and
   only a read that answered is kept."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

(def ^:private cronjob {:metadata {:namespace "batch" :name "nightly" :uid "u1" :generation 1}})

(def ^:private answers
  {:object {:exit 0 :out (json/generate-string cronjob) :err ""}
   :not-found {:exit 1 :out "" :err "Error from server (NotFound): cronjobs.batch \"nightly\" not found"}
   :unanswered {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"}
   :garbled {:exit 0 :out "{\"metadata\":" :err ""}})

(defn- counting
  "(f calls) with every kubectl command answered by (answer n), n counting
   the commands run so far from 1; calls is the atom of argvs run."
  [answer f]
  (let [calls (atom [])]
    (with-redefs [shell/execute (fn [& argv]
                                  (let [n (count (swap! calls conj (vec argv)))]
                                    (get answers (answer n))))
                  shell/stdout-of (fn [& argv]
                                    (let [n (count (swap! calls conj (vec argv)))]
                                      (:out (get answers (answer n)))))]
      (f calls))))

(defn- read-cronjob [] (kubectl/get-namespaced-resource "ctx" "batch" "cronjobs" "nightly"))

(deftest without-a-cache-every-read-runs
  (counting (constantly :object)
            (fn [calls]
              (read-cronjob)
              (read-cronjob)
              (is (= 2 (count @calls))))))

(deftest within-one-cache-an-object-is-fetched-once
  (counting (constantly :object)
            (fn [calls]
              (binding [kubectl/*read-cache* (kubectl/read-cache)]
                (is (= cronjob (read-cronjob)))
                (is (= cronjob (read-cronjob))))
              (is (= 1 (count @calls)))))
  (testing "a fresh cache reads again"
    (counting (constantly :object)
              (fn [calls]
                (binding [kubectl/*read-cache* (kubectl/read-cache)] (read-cronjob))
                (binding [kubectl/*read-cache* (kubectl/read-cache)] (read-cronjob))
                (is (= 2 (count @calls)))))))

(deftest a-read-of-something-else-is-its-own-entry
  (counting (constantly :object)
            (fn [calls]
              (binding [kubectl/*read-cache* (kubectl/read-cache)]
                (read-cronjob)
                (kubectl/get-namespaced-resource "ctx" "batch" "cronjobs" "weekly")
                (kubectl/get-namespaced-resource "ctx" "other" "cronjobs" "nightly")
                (kubectl/get-namespaced-resource "other-ctx" "batch" "cronjobs" "nightly")
                (kubectl/get-namespaced "ctx" "batch" "cronjobs"))
              (is (= 5 (count (distinct @calls))))
              (is (= 5 (count @calls))))))

(deftest a-strict-and-a-lax-read-share-an-entry-and-keep-their-contracts
  (counting (constantly :not-found)
            (fn [calls]
              (binding [kubectl/*read-cache* (kubectl/read-cache)]
                (is (nil? (read-cronjob)))
                (is (nil? (binding [kubectl/*strict-reads* true] (read-cronjob)))
                    "NotFound answered, so a strict read returns nil")
                (is (= {:ok? false :items nil}
                       (kubectl/get-namespaced-result "ctx" "batch" "cronjobs"))))
              (is (= 2 (count @calls)) "the object read once, the list once"))))

(deftest a-read-that-did-not-answer-is-not-kept
  (counting {1 :unanswered 2 :object}
            (fn [calls]
              (binding [kubectl/*read-cache* (kubectl/read-cache)]
                (is (thrown? clojure.lang.ExceptionInfo
                             (binding [kubectl/*strict-reads* true] (read-cronjob))))
                (is (= cronjob (binding [kubectl/*strict-reads* true] (read-cronjob)))
                    "the next read asks the cluster again")
                (is (= cronjob (read-cronjob)) "and its answer is kept"))
              (is (= 2 (count @calls)))))
  (testing "nor is output that does not parse"
    (counting {1 :garbled 2 :object}
              (fn [calls]
                (binding [kubectl/*read-cache* (kubectl/read-cache)]
                  (is (nil? (read-cronjob)))
                  (is (= cronjob (read-cronjob))))
                (is (= 2 (count @calls)))))))

(deftest concurrent-reads-of-one-object-share-the-read-in-flight
  (let [calls (atom 0)
        started (CountDownLatch. 1)
        release (CountDownLatch. 1)]
    (with-redefs [shell/execute (fn [& _]
                                  (swap! calls inc)
                                  (.countDown started)
                                  (.await release 5 TimeUnit/SECONDS)
                                  (:object answers))]
      (let [results (binding [kubectl/*read-cache* (kubectl/read-cache)]
                      (concurrent/all [read-cronjob
                                       (fn []
                                         (.await started 5 TimeUnit/SECONDS)
                                         (let [second-read (future (read-cronjob))]
                                           (Thread/sleep 50)
                                           (.countDown release)
                                           @second-read))]))]
        (is (= [cronjob cronjob] results))
        (is (= 1 @calls) "the second read waited for the first instead of issuing its own")))))

(deftest answered?-says-whether-a-read-answered
  (is (true? (kubectl/answered? (:object answers))))
  (is (true? (kubectl/answered? (:not-found answers))))
  (is (false? (kubectl/answered? (:unanswered answers))))
  (is (false? (kubectl/answered? (:garbled answers))))
  (is (true? (kubectl/answered? {:exit 1 :out "" :err "error: the server doesn't have a resource type \"widgets\""}))))
