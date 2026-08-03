;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.stageset-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.investigation :as investigation]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.subject :as subject]
            [gumshoe.tools.stageset :as stageset]))

(defn- summaries [findings] (set (map :summary findings)))

(deftest stageset-severity-by-reason-test
  (testing "Ready=False maps its reason to a severity; hard failures are critical, gates warn, human-waits inform"
    (let [evidence {stageset/stageset-type
                    {:items [{:metadata {:namespace "apps" :name "web"}
                              :status {:conditions [{:type "Ready" :status "False" :reason "RollbackStoreFailed"
                                                     :message "snapshot missing"}]}}
                             {:metadata {:namespace "apps" :name "api"}
                              :status {:conditions [{:type "Ready" :status "False" :reason "AwaitingPromotion"}]}}
                             {:metadata {:namespace "apps" :name "worker"}
                              :status {:conditions [{:type "Ready" :status "False" :reason "BudgetExhausted"}]}}
                             {:metadata {:namespace "apps" :name "healthy"}
                              :status {:conditions [{:type "Ready" :status "True"}]}}]}}
          findings (stageset/detect-stageset-problems evidence)
          by-summary (into {} (map (juxt :summary :severity) findings))]
      (is (= #{"StageSet is not Ready (RollbackStoreFailed)"
               "StageSet is not Ready (AwaitingPromotion)"
               "StageSet is not Ready (BudgetExhausted)"}
             (summaries findings)) "a healthy StageSet produces nothing")
      (is (= :critical (by-summary "StageSet is not Ready (RollbackStoreFailed)")))
      (is (= :info (by-summary "StageSet is not Ready (AwaitingPromotion)")))
      (is (= :warning (by-summary "StageSet is not Ready (BudgetExhausted)"))))))

(deftest held-update-is-info-test
  (let [findings (stageset/detect-held-updates
                  {stageset/stageset-type
                   {:items [{:metadata {:namespace "apps" :name "web"}
                             :status {:pendingUpdate
                                      {:revisions {"first" "sha256:abc"}
                                       :nextWindowOpens "2026-07-06T02:00:00Z"}}}]}})]
    (is (= #{"a new revision is held by the update window"} (summaries findings)))
    (is (= "next window opens: 2026-07-06T02:00:00Z" (:hint (first findings)))
        "nextWindowOpens is a field of pendingUpdate, so the wait's end is named")))

(deftest package-registers-delivery-scope-and-capability-test
  (is (seq (registry/for-scope :delivery)) "the package fills the :delivery scan scope")
  (is (contains? (set (capabilities/registered)) :stageset)))

(deftest finding-names-its-subject-test
  (testing "a finding points at the StageSet, so a scan drills straight into it"
    (is (= (subject/subject "StageSet" "apps" "web")
           (:subject (first (stageset/detect-stageset-problems
                             {stageset/stageset-type
                              {:items [{:metadata {:namespace "apps" :name "web"}
                                        :status {:conditions [{:type "Ready" :status "False"
                                                               :reason "StageFailed"}]}}]}})))))))

(deftest stageset-edges-follow-every-stage-source-test
  (testing "a bare sourceRef is an ExternalArtifact; a producer kind lands on the producer itself"
    (is (= [{:relation "stage 'first' builds from"
             :subject (subject/subject "ExternalArtifact" "apps" "base")}
            {:relation "stage 'second' builds from"
             :subject (subject/subject "JsonnetSnippet" "apps" "dashboards")}]
           (stageset/stageset-edges
            {:metadata {:namespace "apps" :name "web"}
             :spec {:stages [{:name "first" :sourceRef {:name "base"}}
                             {:name "second" :sourceRef {:apiVersion "jaas.metio.wtf/v1"
                                                         :kind "JsonnetSnippet"
                                                         :name "dashboards"}}]}})))))

(deftest stageset-edges-honour-an-explicit-namespace-test
  (is (= [{:relation "stage 'first' builds from"
           :subject (subject/subject "ExternalArtifact" "platform" "shared")}]
         (stageset/stageset-edges
          {:metadata {:namespace "apps" :name "web"}
           :spec {:stages [{:name "first" :sourceRef {:name "shared" :namespace "platform"}}]}}))))

(deftest stageset-edges-include-the-migration-ladder-test
  (is (= [{:relation "migrations from"
           :subject (subject/subject "ExternalArtifact" "apps" "ladder")}]
         (stageset/stageset-edges
          {:metadata {:namespace "apps" :name "web"}
           :spec {:migrationsSourceRef {:sourceRef {:name "ladder"}}}}))))

(deftest stageset-facts-test
  (let [facts (into {} (stageset/stageset-facts
                        {:metadata {:namespace "apps" :name "web"}
                         :status {:version "2.1.0"
                                  :stages [{:name "first" :phase "Succeeded"}
                                           {:name "second" :phase "Progressing"}]}}))]
    (is (= "first: Succeeded, second: Progressing" (get facts "stages")))
    (is (= "2.1.0" (get facts "version")))
    (is (nil? (get facts "held until")))))

(deftest stageset-is-a-drill-down-subject-test
  (is (= stageset/stageset-type (subject/kind->type "StageSet")))
  (is (= stageset/stageinventory-type (subject/kind->type "StageInventory"))))

(deftest inventory-edges-ask-the-cluster-by-label-test
  (testing "shards are found by the stage-set label, scoped to the namespace, and each names its stage"
    (let [asked (atom nil)]
      (with-redefs [kubectl/get-selected (fn [context type selector]
                                           (reset! asked {:context context :type type :selector selector})
                                           {:items [{:metadata {:namespace "apps" :name "web-second-00-ab12cd34ef"
                                                                :labels {(keyword stageset/stage-label) "second"}}}
                                                    {:metadata {:namespace "apps" :name "web-first-00-9f8e7d6c5b"
                                                                :labels {(keyword stageset/stage-label) "first"}}}
                                                    {:metadata {:namespace "other" :name "web-first-00-ffffffffff"
                                                                :labels {(keyword stageset/stage-label) "first"}}}]})]
        (let [edges (stageset/inventory-edges
                     "ctx" (subject/subject "StageSet" "apps" "web") {})]
          (is (= {:context "ctx"
                  :type stageset/stageinventory-type
                  :selector "stages.metio.wtf/stage-set=web"}
                 @asked))
          (is (= [{:relation "stage 'first' applied via"
                   :subject (subject/subject "StageInventory" "apps" "web-first-00-9f8e7d6c5b")}
                  {:relation "stage 'second' applied via"
                   :subject (subject/subject "StageInventory" "apps" "web-second-00-ab12cd34ef")}]
                 edges)
              "sorted by name, and a same-named StageSet in another namespace is not picked up"))))))

(deftest inventory-edges-without-a-stage-label-test
  (testing "a shard missing its stage label still walks, just without the stage in the relation"
    (with-redefs [kubectl/get-selected (fn [_ _ _]
                                         {:items [{:metadata {:namespace "apps" :name "web-first-00-abc"}}]})]
      (is (= ["applied via"]
             (map :relation (stageset/inventory-edges
                             "ctx" (subject/subject "StageSet" "apps" "web") {})))))))

(deftest package-registers-the-inventory-edges-test
  (with-redefs [kubectl/get-selected (fn [_ _ _]
                                       {:items [{:metadata {:namespace "apps" :name "web-first-00-abc"}}]})]
    (is (= ["applied via"]
           (map :relation (investigation/plugin-fetched-edges
                           "ctx" (subject/subject "StageSet" "apps" "web") {})))
        "provide! wired inventory-edges into the fetched-edges seam")))
