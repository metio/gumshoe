;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.argocd-actions-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.argocd-actions :as actions]))

(defn- app [& {:keys [annotations operation phase started sync health deleting sources]}]
  (cond-> {:metadata (cond-> {:namespace "argocd" :name "shop"}
                       annotations (assoc :annotations annotations)
                       deleting (assoc :deletionTimestamp deleting))
           :spec (if sources
                   {:sources sources}
                   {:source {:repoURL "https://git.example/shop.git" :targetRevision "main"}})
           :status (cond-> {:sync {:status (or sync "OutOfSync")} :health {:status (or health "Healthy")}}
                     phase (assoc :operationState (cond-> {:phase phase} started (assoc :startedAt started))))}
    operation (assoc :operation operation)))

(deftest picks
  (is (= {:namespace "argocd" :name "shop"} (actions/parse-pick "argocd/shop")))
  (is (nil? (actions/parse-pick "shop"))))

(deftest the-source
  (is (= "https://git.example/shop.git@main" (actions/source-of (app))))
  (is (= "https://git.example/a.git@HEAD" (actions/source-of (app :sources [{:repoURL "https://git.example/a.git"} {:repoURL "b"}])))
      "the first of several sources, HEAD when no revision is set")
  (is (= "its source@HEAD" (actions/source-of {:spec {}}))))

;; --- refresh ---------------------------------------------------------------------

(deftest refresh-refusals
  (is (nil? (actions/refresh-refusal (app) "normal")))
  (is (nil? (actions/refresh-refusal (app) "hard")))
  (is (= "the refresh type is soft, not normal or hard" (actions/refresh-refusal (app) "soft")))
  (is (= "a refresh is requested already and the controller has not taken it yet"
         (actions/refresh-refusal (app :annotations {:argocd.argoproj.io/refresh "normal"}) "hard")))
  (is (= "the Application no longer exists" (actions/refresh-refusal nil "normal"))))

(deftest refreshing
  (is (= ["refresh Application argocd/shop - the controller compares it with https://git.example/shop.git@main again"
          "a refresh changes nothing in the cluster; an automated sync may follow when it finds a difference"]
         (actions/refresh-items (app) "normal")))
  (is (re-find #"regenerating its manifests" (first (actions/refresh-items (app) "hard"))))
  (is (= "argocd.argoproj.io/refresh=hard" (actions/refresh-arg "hard")))
  (is (actions/refreshed? (app)))
  (is (not (actions/refreshed? (app :annotations {:argocd.argoproj.io/refresh "normal"}))))
  (is (not (actions/refreshed? nil))))

;; --- sync ------------------------------------------------------------------------

(deftest sync-refusals
  (is (nil? (actions/sync-refusal (app))))
  (is (nil? (actions/sync-refusal (app :phase "Succeeded"))))
  (is (nil? (actions/sync-refusal (app :phase "Failed"))) "a failed operation is over")
  (is (= "an operation is running already (Running) - wait for it, or end it with argocd app terminate-op"
         (actions/sync-refusal (app :phase "Running"))))
  (is (re-find #"Terminating" (actions/sync-refusal (app :phase "Terminating"))))
  (is (re-find #"\(requested\)" (actions/sync-refusal (app :operation {:sync {}}))) "requested, not yet started")
  (is (= "the Application is being deleted" (actions/sync-refusal (app :deleting "2026-11-19T12:00:00Z"))))
  (is (= "the Application no longer exists" (actions/sync-refusal nil))))

(deftest the-sync-confirmation
  (is (= ["sync Application argocd/shop to https://git.example/shop.git@main (currently OutOfSync, Healthy)"
          "no prune: resources removed from Git stay in the cluster"]
         (actions/sync-items (app) false)))
  (is (= "prune: resources removed from Git are deleted from the cluster" (second (actions/sync-items (app) true)))))

(deftest the-sync-operation
  (is (= {"operation" {"initiatedBy" {"username" "gumshoe"} "sync" {"syncStrategy" {"hook" {}}}}}
         (json/parse-string (actions/sync-patch "gumshoe" false))))
  (is (= true (get-in (json/parse-string (actions/sync-patch "gumshoe" true)) ["operation" "sync" "prune"]))))

(deftest sync-completion
  (let [requested (java.time.Instant/parse "2026-11-19T12:00:00.700Z")]
    (is (actions/synced? (app :phase "Succeeded" :started "2026-11-19T12:00:00Z") requested)
        "startedAt has second precision, so the request's own second counts")
    (is (actions/synced? (app :phase "Succeeded" :started "2026-11-19T12:00:05Z") requested))
    (is (not (actions/synced? (app :phase "Succeeded" :started "2026-11-19T11:59:59Z") requested)) "an older operation")
    (is (not (actions/synced? (app :phase "Running" :started "2026-11-19T12:00:05Z") requested)))
    (is (not (actions/synced? (app :phase "Succeeded" :started "2026-11-19T12:00:05Z" :operation {:sync {}}) requested))
        "the operation field is still there")
    (is (not (actions/synced? (app) requested)))
    (testing "a failure of the requested operation is surfaced, an older one is not"
      (is (= ["the sync failed: one or more objects failed to apply"]
             (actions/sync-signals (assoc-in (app :phase "Failed" :started "2026-11-19T12:00:03Z")
                                             [:status :operationState :message] "one or more objects failed to apply")
                                   requested)))
      (is (= ["the sync errored: no message"] (actions/sync-signals (app :phase "Error" :started "2026-11-19T12:00:03Z") requested)))
      (is (nil? (actions/sync-signals (app :phase "Failed" :started "2026-11-19T11:00:00Z") requested)))
      (is (nil? (actions/sync-signals (app :phase "Succeeded" :started "2026-11-19T12:00:03Z") requested))))))
