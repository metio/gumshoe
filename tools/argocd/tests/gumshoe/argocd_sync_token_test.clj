;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.argocd-sync-token-test
  "The sync books' post-check knows its own operation by the token it wrote
   into the operation's info, so the clocks of the workstation and the
   controller play no part."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.argocd-actions :as actions]))

(def ^:private token "7b0d5c1e-0000-4000-8000-000000000001")

(defn- app
  "An Application whose operationState reports phase, started at started,
   for an operation carrying info."
  [& {:keys [phase started info operation message]}]
  (cond-> {:metadata {:namespace "argocd" :name "shop"}
           :status (cond-> {}
                     phase (assoc :operationState
                                  (cond-> {:phase phase
                                           :startedAt (or started "2026-11-18T12:00:03Z")
                                           :operation {:initiatedBy {:username "gumshoe"} :sync {}}}
                                    info (assoc-in [:operation :info] info)
                                    message (assoc :message message))))}
    operation (assoc :operation operation)))

(defn- ours [] [{:name actions/request-info :value token}])

(deftest the-sync-patch-carries-the-token
  (let [patch (json/parse-string (actions/sync-patch "gumshoe" false token))]
    (is (= {"operation" {"initiatedBy" {"username" "gumshoe"}
                         "info" [{"name" "gumshoe-request" "value" token}]
                         "sync" {"syncStrategy" {"hook" {}}}}}
           patch)))
  (is (= true (get-in (json/parse-string (actions/sync-patch "gumshoe" true token)) ["operation" "sync" "prune"]))))

(deftest every-token-is-new
  (is (= 50 (count (set (repeatedly 50 actions/request-token))))))

(deftest a-workstation-clock-ahead-of-the-controller-still-sees-its-sync
  ;; the controller started the operation at 12:00:03 by its clock, while the
  ;; workstation's clock read 12:00:05.4 when it requested it
  (testing "a succeeded operation carrying the token is the run's own"
    (is (true? (actions/synced? (app :phase "Succeeded" :started "2026-11-18T12:00:03Z" :info (ours)) token))))
  (testing "a failed one is reported as failed, not waited out"
    (is (= ["the sync failed: one or more objects failed to apply"]
           (actions/sync-signals (app :phase "Failed" :info (ours) :message "one or more objects failed to apply") token)))
    (is (= ["the sync errored: no message"] (actions/sync-signals (app :phase "Error" :info (ours)) token)))))

(deftest a-workstation-clock-behind-the-controller-never-takes-an-older-operation
  ;; an operation that finished long after the workstation's clock but before
  ;; the request carries no token, or another one
  (doseq [info [nil
                [{:name actions/request-info :value "another-run"}]
                [{:name "reason" :value token}]]]
    (testing (pr-str info)
      (is (false? (actions/synced? (app :phase "Succeeded" :started "2099-01-01T00:00:00Z" :info info) token)))
      (is (nil? (actions/sync-signals (app :phase "Failed" :started "2099-01-01T00:00:00Z" :info info) token))))))

(deftest the-own-operation-is-done-only-once-it-finished
  (is (false? (actions/synced? (app :phase "Running" :info (ours)) token)))
  (is (false? (actions/synced? (app :phase "Succeeded" :info (ours) :operation {:sync {}}) token))
      "the operation field is still there")
  (is (false? (actions/synced? (app) token)) "no operation state")
  (is (nil? (actions/sync-signals (app :phase "Succeeded" :info (ours)) token)))
  (is (false? (actions/synced? (app :phase "Succeeded" :info (ours)) nil)) "no token matches nothing"))
