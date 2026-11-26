;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.jaas-reconcile-suspended-test
  "jaas/reconcile neither offers nor accepts a suspended JsonnetSnippet: the
   operator acknowledges the reconcile token on a suspended snippet without
   evaluating it, so the post-check would pass on nothing."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.tools.jaas :as jaas]))

(defn- snippet
  [name & {:keys [suspend reason generation observed] :or {generation 2 observed 2}}]
  {:metadata {:namespace "apps" :name name :generation generation}
   :spec (cond-> {} (some? suspend) (assoc :suspend suspend))
   :status {:observedGeneration observed
            :conditions (if reason [{:type "Ready" :status "False" :reason reason :observedGeneration observed}] [])}})

(deftest a-suspended-snippet-is-refused
  (testing "its spec says so"
    (is (str/includes? (jaas/reconcile-refusal "apps/site" (snippet "site" :suspend true))
                       "is suspended - resume it first")))
  (testing "the operator reports it at the current generation"
    (is (some? (jaas/reconcile-refusal "apps/site" (snippet "site" :reason "Suspended")))))
  (testing "a Suspended reason from before a resume the operator has not seen yet"
    (is (nil? (jaas/reconcile-refusal "apps/site" (snippet "site" :suspend false :reason "Suspended" :generation 3 :observed 2)))))
  (testing "a running snippet"
    (is (nil? (jaas/reconcile-refusal "apps/site" (snippet "site" :suspend false :reason "EvaluationFailed")))))
  (testing "a snippet that no longer exists"
    (is (= "JsonnetSnippet apps/site no longer exists" (jaas/reconcile-refusal "apps/site" nil)))))

(deftest the-picker-offers-only-snippets-that-are-not-suspended
  (let [{:keys [select]} (with-redefs [mutation/book identity]
                           (load-file "tools/jaas/runbooks/jaas/reconcile.clj"))]
    (with-redefs [kubectl/get-all (fn [_ _] {:items [(snippet "a") (snippet "b" :suspend true) (snippet "c" :reason "Suspended")]})]
      (is (= ["apps/a"] ((:candidates select) "ctx"))))))
