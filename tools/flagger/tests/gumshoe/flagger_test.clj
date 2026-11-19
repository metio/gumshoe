;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.flagger-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.flagger :as flagger]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.flagger]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- canary [phase & {:keys [failed weight at threshold suspend message target-kind]
                        :or {at "2026-11-19T11:00:00Z" threshold 5}}]
  {:metadata {:namespace "shop" :name "web"}
   :spec (cond-> {:targetRef (cond-> {:name "web"} target-kind (assoc :kind target-kind))
                  :analysis {:threshold threshold :interval "1m"}}
           suspend (assoc :suspend true))
   :status (cond-> {:phase phase :failedChecks (or failed 0) :canaryWeight (or weight 0) :lastTransitionTime at}
             message (assoc :conditions [{:type "Promoted" :status "False" :reason "Failed" :message message}]))})

(defn- one [c] (flagger/canary-findings c now))

(deftest a-failed-analysis
  (let [[finding] (one (canary "Failed" :message "Canary analysis failed, Deployment scaled to zero."))]
    (is (= :critical (:severity finding)))
    (is (= "shop/web" (:component finding)))
    (is (= "Canary analysis failed - Deployment web was rolled back" (:summary finding)))
    (is (str/starts-with? (:hint finding) "Canary analysis failed, Deployment scaled to zero. - the primary still serves"))
    (is (= "2026-11-19T11:00:00Z" (:since finding))))
  (is (= "Canary analysis failed - DaemonSet web was rolled back"
         (:summary (first (one (canary "Failed" :target-kind "DaemonSet")))))))

(deftest a-failing-analysis
  (is (= [[:warning "Canary analysis has failed 2 of 5 allowed checks (canary at 30% traffic)"]]
         (map (juxt :severity :summary) (one (canary "Progressing" :failed 2 :weight 30)))))
  (is (= "Canary analysis has failed 1 checks (canary at 0% traffic)"
         (:summary (first (one (assoc-in (canary "Progressing" :failed 1) [:spec :analysis] nil))))))
  (is (empty? (one (canary "Progressing" :weight 40))) "a healthy analysis"))

(deftest a-canary-that-never-initializes
  (is (= [[:warning "Canary has been Initializing for 60 minutes"]]
         (map (juxt :severity :summary) (one (canary "Initializing")))))
  (is (str/includes? (:hint (first (one (canary "Initializing")))) "Deployment web and its primary"))
  (is (empty? (one (canary "Initializing" :at "2026-11-19T11:50:00Z"))))
  (is (seq (one (canary "Initializing" :at "2026-11-19T11:45:00Z"))) "fifteen minutes"))

(deftest gates
  (is (= [[:info "Canary has waited 120 minutes at its confirm-rollout gate"]]
         (map (juxt :severity :summary) (one (canary "Waiting" :at "2026-11-19T10:00:00Z")))))
  (is (= "Canary has waited 60 minutes at its confirm-promotion gate"
         (:summary (first (one (canary "WaitingPromotion"))))))
  (is (empty? (one (canary "Waiting" :at "2026-11-19T11:30:00Z")))))

(deftest settled-and-suspended-canaries-say-nothing
  (doseq [phase ["Initialized" "Succeeded" "Promoting" "Finalising" "Terminating" "Terminated"]]
    (is (empty? (one (canary phase :failed 3))) phase))
  (is (empty? (one (canary "Failed" :suspend true))) "suspended on purpose"))

(deftest the-package-joins-the-delivery-scope
  (is (every? (set (map :name (registry/for-scope :delivery))) (map :name flagger/detectives)))
  (is (= 1 (count (flagger/detect-canaries {:now now flagger/canary-type {:items [(canary "Failed")]}}))))
  (is (empty? (detective/run-detectives flagger/detectives {:now now})) "a cluster without Flagger"))
