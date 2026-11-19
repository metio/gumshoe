;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.argocd-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.argocd :as argocd]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.argocd]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- app [& {:keys [conditions health sync operation policy] :or {sync "Synced"}}]
  {:metadata {:namespace "argocd" :name "shop"}
   :spec (cond-> {} policy (assoc :syncPolicy policy))
   :status (cond-> {:sync {:status sync}}
             conditions (assoc :conditions conditions)
             health (assoc :health health)
             operation (assoc :operationState operation))})

(defn- findings [application] (argocd/detect-applications {:now now argocd/application-type {:items [application]}}))

(deftest a-healthy-synced-application-says-nothing
  (is (empty? (findings (app :health {:status "Healthy"} :operation {:phase "Succeeded" :startedAt "2026-11-19T11:00:00Z" :finishedAt "2026-11-19T11:01:00Z"}
                             :policy {:automated {:selfHeal true}})))))

(deftest conditions-by-type
  (let [result (findings (app :conditions [{:type "ComparisonError" :message "rpc error: Manifest generation error" :lastTransitionTime "2026-11-19T10:00:00Z"}
                                           {:type "SharedResourceWarning" :message "ConfigMap/x is part of applications argocd/shop and other"}
                                           {:type "OrphanedResourceWarning" :message "1 orphaned resource"}
                                           {:type "SomethingNew" :message "?"}]))]
    (is (= [[:critical "Application reports ComparisonError"]
            [:warning "Application reports SharedResourceWarning"]
            [:info "Application reports OrphanedResourceWarning"]]
           (map (juxt :severity :summary) result)))
    (is (= "rpc error: Manifest generation error" (:hint (first result))))
    (is (= "2026-11-19T10:00:00Z" (:since (first result))))
    (is (= "argocd/shop" (:component (first result)))))
  (testing "every error type is critical"
    (doseq [type ["InvalidSpecError" "ComparisonError" "SyncError" "DeletionError" "UnknownError"]]
      (is (= :critical (argocd/condition-severity type)) type))))

(deftest health
  (is (= [[:critical "Application is Degraded" "Deployment shop/web has timed out progressing"]]
         (map (juxt :severity :summary :hint) (findings (app :health {:status "Degraded" :message "Deployment shop/web has timed out progressing"})))))
  (is (str/includes? (:hint (first (findings (app :health {:status "Degraded"})))) "argocd app get"))
  (is (= "Application is Missing" (:summary (first (findings (app :health {:status "Missing"}))))))
  (doseq [status ["Healthy" "Progressing" "Suspended" "Unknown" nil]]
    (is (empty? (findings (app :health {:status status}))) (str status))))

(deftest failed-syncs
  (let [[finding] (findings (app :operation {:phase "Failed" :message "one or more objects failed to apply" :retryCount 5
                                             :startedAt "2026-11-19T11:00:00Z" :finishedAt "2026-11-19T11:02:00Z"}))]
    (is (= :critical (:severity finding)))
    (is (= "Application's last sync failed after 5 retries" (:summary finding)))
    (is (= "one or more objects failed to apply" (:hint finding)))
    (is (= "2026-11-19T11:02:00Z" (:since finding))))
  (is (= "Application's last sync errored" (:summary (first (findings (app :operation {:phase "Error" :startedAt "2026-11-19T11:00:00Z"})))))))

(deftest syncs-that-never-finish
  (let [[finding] (findings (app :operation {:phase "Running" :message "waiting for completion of hook batch/Job/migrate" :startedAt "2026-11-19T11:15:00Z"}))]
    (is (= :warning (:severity finding)))
    (is (= "Application's sync has been Running for 45 minutes" (:summary finding)))
    (is (str/starts-with? (:hint finding) "waiting for completion of hook batch/Job/migrate - "))
    (is (str/includes? (:hint finding) "terminate-op")))
  (is (empty? (findings (app :operation {:phase "Running" :startedAt "2026-11-19T11:45:00Z"}))) "a running sync is left alone")
  (is (seq (findings (app :operation {:phase "Running" :startedAt "2026-11-19T11:30:00Z"}))) "thirty minutes is stuck")
  (is (seq (findings (app :operation {:phase "Terminating" :startedAt "2026-11-19T10:00:00Z"})))))

(deftest automated-sync-that-does-not-converge
  (let [out-of-sync (fn [& {:keys [policy operation]}] (findings (app :sync "OutOfSync" :policy policy :operation operation)))
        settled {:phase "Succeeded" :startedAt "2026-11-19T11:00:00Z" :finishedAt "2026-11-19T11:01:00Z"}]
    (testing "selfHeal off: drift in the cluster is not reverted"
      (let [[finding] (out-of-sync :policy {:automated {}} :operation settled)]
        (is (= "Application stays OutOfSync although automated sync is on" (:summary finding)))
        (is (str/includes? (:hint finding) "selfHeal is off"))))
    (testing "selfHeal on"
      (is (str/includes? (:hint (first (out-of-sync :policy {:automated {:selfHeal true}} :operation settled))) "application controller")))
    (testing "never synced"
      (is (seq (out-of-sync :policy {:automated {}}))))
    (testing "a sync that just finished gets time to settle"
      (is (empty? (out-of-sync :policy {:automated {}} :operation (assoc settled :finishedAt "2026-11-19T11:55:00Z")))))
    (testing "a running or failed sync is reported on its own"
      (is (empty? (out-of-sync :policy {:automated {}} :operation {:phase "Running" :startedAt "2026-11-19T11:55:00Z"})))
      (is (= ["Application's last sync failed"]
             (map :summary (out-of-sync :policy {:automated {}} :operation {:phase "Failed" :startedAt "2026-11-19T11:00:00Z"})))))
    (testing "manual sync and disabled automation are a choice"
      (is (empty? (out-of-sync :operation settled)))
      (is (empty? (out-of-sync :policy {:automated {:enabled false}} :operation settled))))))

(deftest automation-switch
  (is (argocd/automated? {:spec {:syncPolicy {:automated {}}}}))
  (is (argocd/automated? {:spec {:syncPolicy {:automated {:enabled true}}}}))
  (is (not (argocd/automated? {:spec {:syncPolicy {:automated {:enabled false}}}})))
  (is (not (argocd/automated? {:spec {:syncPolicy {}}})))
  (is (not (argocd/automated? {:spec {}}))))

(deftest applicationsets
  (let [set-with (fn [& conditions] {:metadata {:namespace "argocd" :name "clusters"} :status {:conditions conditions}})
        detect (fn [s] (argocd/detect-applicationsets {argocd/applicationset-type {:items [s]}}))
        [finding] (detect (set-with {:type "ErrorOccurred" :status "True" :reason "ApplicationGenerationFromParamsError"
                                     :message "failed to execute go template" :lastTransitionTime "2026-11-19T09:00:00Z"}
                                    {:type "ResourcesUpToDate" :status "False" :reason "ApplicationGenerationFromParamsError" :message "x"}))]
    (is (= :critical (:severity finding)))
    (is (= "argocd/clusters" (:component finding)))
    (is (= "ApplicationSet cannot generate its Applications (ApplicationGenerationFromParamsError)" (:summary finding)))
    (is (str/starts-with? (:hint finding) "failed to execute go template - "))
    (is (= "2026-11-19T09:00:00Z" (:since finding)))
    (is (empty? (detect (set-with {:type "ErrorOccurred" :status "False" :reason "ApplicationSetUpToDate" :message ""}))))))

(deftest the-package-joins-the-gitops-scope
  (is (every? (set (map :name (registry/for-scope :gitops))) (map :name argocd/detectives)))
  (is (empty? (detective/run-detectives argocd/detectives {:now now})) "a cluster without Argo CD"))
