;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cause-folding-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.correlation :as correlation]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.disruption :as disruption]
            [gumshoe.detectives.events]
            [gumshoe.detectives.pod-creation :as pod-creation]
            [gumshoe.detectives.references :as references]
            [gumshoe.detectives.storage :as storage]
            [gumshoe.detectives.workloads :as workloads]
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

;; --- controllers whose pods the API server refuses ----------------------------

(def ^:private refused-at "2026-11-24T08:00:00Z")

(def ^:private pod-security-refusal
  (str "pods \"etherpad-debugger-646675d5cb-x\" is forbidden: violates PodSecurity \"restricted:latest\":"
       " allowPrivilegeEscalation != false"))

(defn- deployment
  [name revision & {:keys [replicas ready progress-deadline-exceeded]}]
  {:apiVersion "apps/v1" :kind "Deployment"
   :metadata {:namespace "lernraum-1" :name name :uid (str name "-uid")
              :annotations {:deployment.kubernetes.io/revision (str revision)}}
   :spec {:replicas replicas}
   :status (cond-> {:readyReplicas ready}
             progress-deadline-exceeded
             (assoc :conditions [{:type "Progressing" :status "False" :reason "ProgressDeadlineExceeded"
                                  :message "ReplicaSet has timed out progressing." :lastTransitionTime refused-at}]))})

(defn- replicaset
  [owner hash revision & {:keys [refusal]}]
  {:apiVersion "apps/v1" :kind "ReplicaSet"
   :metadata {:namespace "lernraum-1" :name (str owner "-" hash)
              :annotations {:deployment.kubernetes.io/revision (str revision)}
              :ownerReferences [{:apiVersion "apps/v1" :kind "Deployment" :name owner :uid (str owner "-uid")
                                 :controller true}]}
   :status (cond-> {}
             refusal (assoc :conditions [{:type "ReplicaFailure" :status "True" :reason "FailedCreate"
                                          :message refusal :lastTransitionTime refused-at}]))})

(defn- controller-evidence
  [& {:keys [deployments replicasets statefulsets daemonsets events pods]}]
  {:now (java.time.Instant/parse "2026-11-24T08:30:00Z")
   "deployments" {:items (vec deployments)}
   "replicasets" {:items (vec replicasets)}
   "statefulsets" {:items (vec statefulsets)}
   "daemonsets" {:items (vec daemonsets)}
   "events" {:items (vec events)}
   "pods" {:items (vec pods)}
   "services" {:items []}
   "validatingwebhookconfigurations" {:items []}
   "mutatingwebhookconfigurations" {:items []}})

(defn- run-controllers
  [evidence]
  (detective/run-detectives (concat workloads/detectives pod-creation/detectives) evidence))

(defn- by-summary
  [findings pattern]
  (first (filter #(re-find pattern (:summary %)) findings)))

(deftest a-deployment-short-of-replicas-folds-under-its-replicaset-that-cannot-create-pods
  (let [findings (run-controllers
                  (controller-evidence
                   :deployments [(deployment "etherpad-debugger" 3 :replicas 1 :ready 0)]
                   :replicasets [(replicaset "etherpad-debugger" "7f6b8c9d4e" 2)
                                 (replicaset "etherpad-debugger" "646675d5cb" 3 :refusal pod-security-refusal)]))
        refusal (by-summary findings #"cannot create pods")
        replicas (by-summary findings #"replicas ready")]
    (is (= "Deployment has 0 of 1 replicas ready" (:summary replicas)))
    (is (= "ReplicaSet lernraum-1/etherpad-debugger-646675d5cb" (:parent replicas)))
    (is (nil? (:parent refusal)) "the refusal is the root")
    (testing "the report shows the replica count under the refusal"
      (is (= "follows from it: 1 finding - Deployment has 0 of 1 replicas ready"
             (detective/folded-line [(subject/subject "ReplicaSet" "lernraum-1" "etherpad-debugger-646675d5cb")]
                                    (correlation/children findings)))))))

(deftest a-stuck-rollout-folds-under-its-replicaset-that-cannot-create-pods
  (let [findings (run-controllers
                  (controller-evidence
                   :deployments [(deployment "etherpad-debugger" 3 :replicas 1 :ready 0 :progress-deadline-exceeded true)]
                   :replicasets [(replicaset "etherpad-debugger" "646675d5cb" 3 :refusal pod-security-refusal)]))]
    (is (= ["ReplicaSet lernraum-1/etherpad-debugger-646675d5cb" "ReplicaSet lernraum-1/etherpad-debugger-646675d5cb"]
           (mapv :parent [(by-summary findings #"replicas ready") (by-summary findings #"rollout is stuck")])))))

(deftest a-refusal-on-an-older-revision-is-not-the-parent
  (let [findings (run-controllers
                  (controller-evidence
                   :deployments [(deployment "etherpad-debugger" 3 :replicas 1 :ready 0)]
                   :replicasets [(replicaset "etherpad-debugger" "7f6b8c9d4e" 2 :refusal pod-security-refusal)
                                 (replicaset "etherpad-debugger" "646675d5cb" 3)]))]
    (is (some? (by-summary findings #"cannot create pods")))
    (is (nil? (:parent (by-summary findings #"replicas ready"))))))

(deftest a-replicaset-of-another-deployment-is-not-the-parent
  (let [findings (run-controllers
                  (controller-evidence
                   :deployments [(deployment "etherpad-debugger" 3 :replicas 1 :ready 0)
                                 (deployment "etherpad" 3 :replicas 1 :ready 1)]
                   :replicasets [(replicaset "etherpad" "646675d5cb" 3 :refusal pod-security-refusal)]))]
    (is (nil? (:parent (by-summary findings #"replicas ready"))))))

(deftest a-deployment-without-a-refusal-has-no-parent
  (let [findings (run-controllers
                  (controller-evidence
                   :deployments [(deployment "etherpad-debugger" 3 :replicas 1 :ready 0)]
                   :replicasets [(replicaset "etherpad-debugger" "646675d5cb" 3)]))]
    (is (= [nil] (mapv :parent findings)))))

(deftest a-milder-refusal-never-hides-a-worse-replica-count
  (let [findings (run-controllers
                  (controller-evidence
                   :deployments [(deployment "etherpad-debugger" 3 :replicas 1 :ready 0)]
                   :replicasets [(replicaset "etherpad-debugger" "646675d5cb" 3 :refusal "something nobody has seen yet")]))]
    (is (= :warning (:severity (by-summary findings #"cannot create pods"))))
    (is (nil? (:parent (by-summary findings #"replicas ready"))))))

(defn- failed-create
  [kind name]
  {:type "Warning" :reason "FailedCreate" :lastTimestamp "2026-11-24T08:20:00Z"
   :message (str "create Pod " name "-0 in " kind " " name " failed error: " pod-security-refusal)
   :involvedObject {:kind kind :namespace "lernraum-1" :name name}})

(deftest statefulset-and-daemonset-findings-fold-under-their-own-refusal
  (let [findings (run-controllers
                  (controller-evidence
                   :statefulsets [{:metadata {:namespace "lernraum-1" :name "db"} :spec {:replicas 1} :status {:readyReplicas 0}}]
                   :daemonsets [{:metadata {:namespace "lernraum-1" :name "agent"}
                                 :status {:desiredNumberScheduled 2 :currentNumberScheduled 0 :numberUnavailable 2}}]
                   :events [(failed-create "StatefulSet" "db") (failed-create "DaemonSet" "agent")]))]
    (is (= "StatefulSet lernraum-1/db" (:parent (by-summary findings #"StatefulSet has 0 of 1"))))
    (is (= ["DaemonSet lernraum-1/agent" "DaemonSet lernraum-1/agent"]
           (mapv :parent (filter #(= "daemonsets" (:detective %)) findings))))
    (is (every? (comp nil? :parent) (filter #(= "pod-creation" (:detective %)) findings)))))

(deftest a-pod-under-a-folded-deployment-still-shows-under-the-root
  (let [pod {:metadata {:namespace "lernraum-1" :name "etherpad-debugger-7f6b8c9d4e-a"
                        :labels {:pod-template-hash "7f6b8c9d4e"}
                        :ownerReferences [{:kind "ReplicaSet" :name "etherpad-debugger-7f6b8c9d4e" :controller true}]}}
        findings (correlation/correlate
                  [{:severity :critical :detective "pod-creation" :component "lernraum-1/etherpad-debugger-646675d5cb"
                    :subject (subject/subject "ReplicaSet" "lernraum-1" "etherpad-debugger-646675d5cb") :cause true
                    :summary "ReplicaSet cannot create pods: the pod violates the namespace's PodSecurity level"}
                   {:severity :critical :detective "deployments" :component "lernraum-1/etherpad-debugger"
                    :summary "Deployment has 0 of 1 replicas ready"}
                   {:severity :critical :detective "pods" :component "lernraum-1/etherpad-debugger-7f6b8c9d4e-a"
                    :summary "container app keeps restarting"}]
                  (controller-evidence
                   :deployments [(deployment "etherpad-debugger" 3 :replicas 1 :ready 0)]
                   :replicasets [(replicaset "etherpad-debugger" "7f6b8c9d4e" 2)
                                 (replicaset "etherpad-debugger" "646675d5cb" 3 :refusal pod-security-refusal)]
                   :pods [pod]))]
    (is (= [nil "ReplicaSet lernraum-1/etherpad-debugger-646675d5cb" "Deployment lernraum-1/etherpad-debugger"]
           (mapv :parent findings)))
    (is (= "follows from it: 2 findings - Deployment has 0 of 1 replicas ready, container app keeps restarting"
           (detective/folded-line [(subject/subject "ReplicaSet" "lernraum-1" "etherpad-debugger-646675d5cb")]
                                  (correlation/children findings))))))
