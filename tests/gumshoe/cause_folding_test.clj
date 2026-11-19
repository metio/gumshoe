;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cause-folding-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.correlation :as correlation]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.disruption :as disruption]
            [gumshoe.detectives.events]
            [gumshoe.detectives.references :as references]
            [gumshoe.detectives.storage :as storage]
            [gumshoe.subject :as subject]))

(def ^:private claim-subject (subject/subject "PersistentVolumeClaim" "shop" "data"))

(deftest a-symptom-folds-under-the-cause-on-its-object
  (let [cause {:severity :critical :detective "storageclass-references" :component "shop/data" :subject claim-subject :cause true
               :summary "PersistentVolumeClaim uses StorageClass fast, which does not exist"}
        symptom {:severity :critical :detective "storage" :component "shop/data" :summary "PersistentVolumeClaim is Pending"}
        [c s] (correlation/correlate [cause symptom] {})]
    (is (nil? (:parent c)) "a cause stays on top")
    (is (= "PersistentVolumeClaim shop/data" (:parent s)))))

(deftest folding-never-hides-a-worse-severity
  (let [cause {:severity :warning :subject claim-subject :cause true :summary "c"}
        symptom {:severity :critical :detective "storage" :component "shop/data" :summary "s"}]
    (is (nil? (:parent (second (correlation/correlate [cause symptom] {})))))))

(deftest only-the-same-object-folds
  (let [cause {:severity :critical :subject claim-subject :cause true :summary "c"}
        other {:severity :warning :detective "storage" :component "shop/other" :summary "s"}]
    (is (nil? (:parent (second (correlation/correlate [cause other] {})))))))

(deftest causes-do-not-fold-under-each-other
  (let [a {:severity :critical :subject claim-subject :cause true :summary "a"}
        b {:severity :warning :subject claim-subject :cause true :summary "b"}]
    (is (every? nil? (map :parent (correlation/correlate [a b] {}))))))

(deftest without-a-cause-nothing-changes
  (let [plain {:severity :critical :detective "storage" :component "shop/data" :summary "s"}]
    (is (= [plain] (correlation/correlate [plain] {})))))

(deftest cause-of-directly
  (is (= claim-subject (correlation/cause-of {:severity :info :detective "storage" :component "shop/data"} {claim-subject 0})))
  (is (nil? (correlation/cause-of {:severity :info :detective "storage" :component "shop/data" :cause true} {claim-subject 0})))
  (is (nil? (correlation/cause-of {:severity :info :component "unpinned"} {claim-subject 0})) "a finding without a subject"))

(deftest a-pending-claim-folds-under-its-missing-storage-class-end-to-end
  (let [evidence {:now (java.time.Instant/parse "2026-11-19T12:00:00Z")
                  "persistentvolumeclaims" {:items [{:metadata {:namespace "shop" :name "data" :creationTimestamp "2026-11-19T10:00:00Z"}
                                                     :spec {:storageClassName "fast"} :status {:phase "Pending"}}]}
                  "storageclasses" {:items [{:metadata {:name "standard"}}]}
                  "pods" {:items []} "events" {:items []}}
        findings (detective/run-detectives (concat storage/detectives references/detectives) evidence)
        pending (first (filter #(= "PersistentVolumeClaim is Pending" (:summary %)) findings))
        missing (first (filter #(re-find #"StorageClass fast" (:summary %)) findings))]
    (is (:cause missing))
    (is (nil? (:parent missing)))
    (is (= "PersistentVolumeClaim shop/data" (:parent pending)))
    (testing "the report shows the symptom under its cause"
      (is (= "follows from it: 2 findings - PersistentVolumeClaim is Pending, PersistentVolumeClaim is not used by any pod"
             (detective/folded-line [claim-subject] (correlation/children findings)))))))

(defn- hpa [target-kind target-name & {:keys [able]}]
  {:metadata {:namespace "shop" :name "web"}
   :spec {:scaleTargetRef {:kind target-kind :name target-name}}
   :status {:conditions (if able [{:type "AbleToScale" :status "False" :reason able :message "the target is gone"}] [])}})

(deftest hpa-target-references
  (let [evidence (fn [h] {"horizontalpodautoscalers" {:items [h]}
                          "deployments" {:items [{:metadata {:namespace "shop" :name "web"}}]}
                          "statefulsets" {:items []}})]
    (is (empty? (references/detect-hpa-target-references (evidence (hpa "Deployment" "web")))))
    (let [[finding] (references/detect-hpa-target-references (evidence (hpa "Deployment" "gone")))]
      (is (= :critical (:severity finding)))
      (is (:cause finding))
      (is (= "HorizontalPodAutoscaler targets Deployment gone, which does not exist" (:summary finding)))
      (is (= (subject/subject "HorizontalPodAutoscaler" "shop" "web") (:subject finding))))
    (is (seq (references/detect-hpa-target-references (evidence (hpa "StatefulSet" "web")))) "kind matters")
    (is (empty? (references/detect-hpa-target-references (evidence (hpa "Rollout" "web")))) "kinds we do not fetch are not judged")
    (is (seq (references/detect-hpa-target-references
              {"horizontalpodautoscalers" {:items [(hpa "Deployment" "web")]}
               "deployments" {:items [{:metadata {:namespace "other" :name "web"}}]} "statefulsets" {:items []}}))
        "a namesake in another namespace is not the target")
    (is (empty? (references/detect-hpa-target-references
                 {"horizontalpodautoscalers" {:items [(hpa "Deployment" "gone")]} "statefulsets" {:items []}}))
        "without the Deployments fetched nothing is judged")))

(deftest an-hpa-that-cannot-scale-folds-under-its-missing-target
  (let [h (hpa "Deployment" "gone" :able "FailedGetScale")
        findings (detective/run-detectives (concat disruption/detectives references/detectives)
                                           {"horizontalpodautoscalers" {:items [h]}
                                            "deployments" {:items []} "statefulsets" {:items []}})
        scale (first (filter #(re-find #"cannot scale its target" (:summary %)) findings))]
    (is (= "HorizontalPodAutoscaler shop/web" (:parent scale)))))

(deftest template-and-ingress-references-are-causes
  (is (every? :cause (references/detect-template-references
                      {"deployments" {:items [{:metadata {:namespace "shop" :name "web"}
                                               :spec {:template {:spec {:serviceAccountName "ghost" :containers [{:name "a"}]}}}}]}
                       "serviceaccounts" {:items []}}))))

(deftest events-name-their-object-and-fold-under-a-cause
  (let [now (java.time.Instant/parse "2026-11-19T12:00:00Z")
        event (fn [kind name reason] {:type "Warning" :reason reason :message "m" :lastTimestamp "2026-11-19T11:50:00Z"
                                      :involvedObject {:kind kind :namespace "shop" :name name}})
        findings (detective/run-detectives
                  (concat gumshoe.detectives.events/detectives references/detectives)
                  {:now now
                   "events" {:items [(event "HorizontalPodAutoscaler" "web" "FailedGetScale")
                                     (event "Service" "web" "SyncLoadBalancerFailed")
                                     (event "Deployment" "web" "SyncLoadBalancerFailed")]}
                   "horizontalpodautoscalers" {:items [(hpa "Deployment" "gone")]}
                   "deployments" {:items []} "statefulsets" {:items []}})
        by-summary (group-by :summary findings)]
    (is (= "HorizontalPodAutoscaler shop/web" (:parent (first (by-summary "FailedGetScale seen 1x in the last hour")))))
    (is (= 2 (count (by-summary "SyncLoadBalancerFailed seen 1x in the last hour"))) "objects of different kinds stay apart")
    (is (= #{(subject/subject "Service" "shop" "web") (subject/subject "Deployment" "shop" "web")}
           (set (map :subject (by-summary "SyncLoadBalancerFailed seen 1x in the last hour")))))))
