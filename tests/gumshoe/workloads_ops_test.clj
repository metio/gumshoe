;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.workloads-ops-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.workloads :as workloads]))

(defn- deployment
  [& {:keys [replicas paused generation observed updated available current]
      :or {replicas 3 generation 2 observed 2}}]
  {:metadata {:namespace "shop" :name "web" :generation generation}
   :spec (cond-> {:replicas replicas} paused (assoc :paused true))
   :status {:observedGeneration observed :replicas (or current replicas)
            :updatedReplicas (or updated replicas) :availableReplicas (or available replicas)}})

(deftest picks-name-kind-and-object
  (is (= {:kind "Deployment" :type "deployments" :namespace "shop" :name "web"}
         (workloads/parse-pick "Deployment shop/web")))
  (is (= {:kind "DaemonSet" :type "daemonsets" :namespace "kube-system" :name "agent"}
         (workloads/parse-pick "  DaemonSet kube-system/agent ")))
  (doseq [bad [nil "" "Deployment web" "Pod shop/web" "Deployment shop/web extra" "shop/web"]]
    (is (nil? (workloads/parse-pick bad)) (pr-str bad))))

(deftest picks-list-every-accepted-workload-sorted
  (is (= ["Deployment a/x" "Deployment b/x" "StatefulSet a/db"]
         (workloads/picks [["Deployment" {:items [{:metadata {:namespace "b" :name "x"}}
                                                  {:metadata {:namespace "a" :name "x"}}
                                                  {:metadata {:namespace "a" :name "skip"}}]}]
                           ["StatefulSet" {:items [{:metadata {:namespace "a" :name "db"}}]}]
                           ["DaemonSet" nil]]
                          (fn [_ workload] (not= "skip" (get-in workload [:metadata :name])))))))

(deftest desired-replicas-per-kind
  (is (= 3 (workloads/desired-replicas "Deployment" (deployment))))
  (is (= 1 (workloads/desired-replicas "StatefulSet" {:spec {}})) "unset means one")
  (is (= 0 (workloads/desired-replicas "Deployment" (deployment :replicas 0))))
  (is (= 4 (workloads/desired-replicas "DaemonSet" {:status {:desiredNumberScheduled 4}})))
  (is (= 0 (workloads/desired-replicas "DaemonSet" {}))))

(deftest a-deployment-is-rolled-out-once-every-pod-is-new-and-available
  (is (workloads/rolled-out? "Deployment" (deployment)))
  (is (not (workloads/rolled-out? "Deployment" (deployment :observed 1))) "the new spec is not observed yet")
  (is (not (workloads/rolled-out? "Deployment" (deployment :updated 2))))
  (is (not (workloads/rolled-out? "Deployment" (deployment :available 2))))
  (is (not (workloads/rolled-out? "Deployment" (deployment :current 4))) "an old pod still terminates")
  (is (not (workloads/rolled-out? "Deployment" nil))))

(deftest a-statefulset-is-rolled-out-on-its-update-revision
  (let [sts (fn [& {:keys [current update ready strategy]}]
              {:metadata {:generation 1}
               :spec (cond-> {:replicas 2} strategy (assoc :updateStrategy {:type strategy}))
               :status {:observedGeneration 1 :updatedReplicas 2 :readyReplicas (or ready 2)
                        :currentRevision current :updateRevision update}})]
    (is (workloads/rolled-out? "StatefulSet" (sts :current "r2" :update "r2")))
    (is (not (workloads/rolled-out? "StatefulSet" (sts :current "r1" :update "r2"))))
    (is (not (workloads/rolled-out? "StatefulSet" (sts :current "r2" :update "r2" :ready 1))))
    (is (workloads/rolled-out? "StatefulSet" (sts)) "no revisions reported")))

(deftest a-daemonset-is-rolled-out-on-every-scheduled-node
  (let [ds (fn [updated available] {:metadata {:generation 3}
                                    :status {:observedGeneration 3 :desiredNumberScheduled 3
                                             :updatedNumberScheduled updated :numberAvailable available}})]
    (is (workloads/rolled-out? "DaemonSet" (ds 3 3)))
    (is (not (workloads/rolled-out? "DaemonSet" (ds 2 3))))
    (is (not (workloads/rolled-out? "DaemonSet" (ds 3 2))))))

(deftest restart-refusals
  (is (nil? (workloads/restart-refusal "Deployment" (deployment))))
  (is (= "the workload no longer exists" (workloads/restart-refusal "Deployment" nil)))
  (is (re-find #"paused" (workloads/restart-refusal "Deployment" (deployment :paused true))))
  (is (re-find #"no pods" (workloads/restart-refusal "Deployment" (deployment :replicas 0))))
  (is (re-find #"OnDelete" (workloads/restart-refusal "StatefulSet" {:spec {:replicas 2 :updateStrategy {:type "OnDelete"}}})))
  (is (nil? (workloads/restart-refusal "StatefulSet" {:spec {:replicas 2 :updateStrategy {:type "RollingUpdate"}}})))
  (is (re-find #"no pods" (workloads/restart-refusal "DaemonSet" {:status {:desiredNumberScheduled 0}}))))

(deftest the-restart-command
  (is (= ["--namespace=shop" "rollout" "restart" "deployment/web"]
         (workloads/restart-args {:type "deployments" :namespace "shop" :name "web"})))
  (is (= ["--namespace=db" "rollout" "restart" "statefulset/pg"]
         (workloads/restart-args {:type "statefulsets" :namespace "db" :name "pg"}))))

;; --- scale ---------------------------------------------------------------------

(defn- hpa [kind name & {:keys [min max namespace] :or {min 2 max 6 namespace "shop"}}]
  {:metadata {:namespace namespace :name (str name "-hpa")}
   :spec {:scaleTargetRef {:kind kind :name name} :minReplicas min :maxReplicas max}})

(deftest the-managing-hpa
  (is (= "web-hpa" (get-in (workloads/managing-hpa {:items [(hpa "Deployment" "web")]} "Deployment" (deployment)) [:metadata :name])))
  (is (nil? (workloads/managing-hpa {:items [(hpa "StatefulSet" "web")]} "Deployment" (deployment))) "another kind")
  (is (nil? (workloads/managing-hpa {:items [(hpa "Deployment" "web" :namespace "other")]} "Deployment" (deployment))) "another namespace")
  (is (nil? (workloads/managing-hpa nil "Deployment" (deployment)))))

(deftest scale-refusals
  (is (nil? (workloads/scale-refusal "Deployment" (deployment) nil 5)))
  (is (nil? (workloads/scale-refusal "Deployment" (deployment) nil 0)) "zero is a count")
  (doseq [bad [nil -1 2.5 "3"]]
    (is (re-find #"--replicas must be" (workloads/scale-refusal "Deployment" (deployment) nil bad)) (pr-str bad)))
  (is (re-find #"not scaled by a replica count" (workloads/scale-refusal "DaemonSet" {:spec {}} nil 2)))
  (is (= "the workload no longer exists" (workloads/scale-refusal "Deployment" nil nil 2)))
  (is (= "HorizontalPodAutoscaler web-hpa manages its replicas (2 to 6) and would undo this - change the HPA instead"
         (workloads/scale-refusal "Deployment" (deployment) {:items [(hpa "Deployment" "web")]} 5)))
  (is (= "it runs 3 replica(s) already" (workloads/scale-refusal "Deployment" (deployment) nil 3))))

(deftest the-confirmation-names-the-change
  (is (= ["scale Deployment shop/web from 3 to 5 replica(s)"] (workloads/scale-items "Deployment" (deployment) 5)))
  (is (= ["scale Deployment shop/web from 3 to 0 replica(s)" "no pod serves it afterwards"]
         (workloads/scale-items "Deployment" (deployment) 0)))
  (is (= ["scale StatefulSet shop/web from 3 to 1 replica(s)"
          "the pods with the highest ordinals go first; their PersistentVolumeClaims stay"]
         (workloads/scale-items "StatefulSet" (deployment) 1))))

(deftest scaled-compares-the-wanted-count
  (let [at (fn [replicas ready & {:keys [observed] :or {observed 2}}]
             {:metadata {:generation 2} :status {:observedGeneration observed :replicas replicas :readyReplicas ready}})]
    (is (workloads/scaled? (at 5 5) 5))
    (is (not (workloads/scaled? (at 3 3) 5)) "the old count is not the wanted one")
    (is (not (workloads/scaled? (at 5 4) 5)))
    (is (not (workloads/scaled? (at 5 5 :observed 1) 5)) "the new spec is not observed yet")
    (is (workloads/scaled? {:metadata {:generation 2} :status {:observedGeneration 2}} 0) "zero reports no counts")
    (is (not (workloads/scaled? nil 0)))))

(deftest the-scale-command
  (is (= ["--namespace=shop" "scale" "deployment/web" "--replicas=0"]
         (workloads/scale-args {:type "deployments" :namespace "shop" :name "web"} 0))))

;; --- rollback ------------------------------------------------------------------

(defn- rs [name revision image & {:keys [owner uid] :or {owner "web" uid "uid-web"}}]
  {:metadata {:namespace "shop" :name name :annotations {:deployment.kubernetes.io/revision (str revision)}
              :ownerReferences [{:kind "Deployment" :name owner :uid uid}]}
   :spec {:template {:spec {:containers [{:name "app" :image image}]}}}})

(defn- web [revision image & {:keys [paused]}]
  (-> (deployment)
      (assoc-in [:metadata :uid] "uid-web")
      (assoc-in [:metadata :annotations] {:deployment.kubernetes.io/revision (str revision)})
      (assoc-in [:spec :template :spec :containers] [{:name "app" :image image}])
      (cond-> paused (assoc-in [:spec :paused] true))))

(def ^:private history
  {:items [(rs "web-a" 5 "web:1.0") (rs "web-c" 7 "web:1.2") (rs "web-b" 6 "web:1.1")
           (rs "other-x" 9 "other:1" :owner "other" :uid "uid-other")
           (rs "web-old" 4 "web:0.9" :uid "uid-of-a-deleted-web")]})

(deftest revisions-come-from-owned-replicasets-newest-first
  (is (= [{:revision 7 :replicaset "web-c" :images ["web:1.2"]}
          {:revision 6 :replicaset "web-b" :images ["web:1.1"]}
          {:revision 5 :replicaset "web-a" :images ["web:1.0"]}]
         (workloads/revisions (web 7 "web:1.2") history)))
  (testing "without a uid the owner name decides"
    (is (= 4 (count (workloads/revisions (update (web 7 "web:1.2") :metadata dissoc :uid) history))))))

(deftest the-rollback-target
  (is (= 6 (:revision (workloads/rollback-target (web 7 "web:1.2") history nil))) "the previous revision")
  (is (= 5 (:revision (workloads/rollback-target (web 7 "web:1.2") history 5))))
  (is (nil? (workloads/rollback-target (web 7 "web:1.2") history 3)))
  (is (nil? (workloads/rollback-target (web 5 "web:1.0") history nil)) "the oldest has nothing before it"))

(deftest rollback-refusals
  (is (nil? (workloads/rollback-refusal (web 7 "web:1.2") history nil)))
  (is (= "the Deployment no longer exists" (workloads/rollback-refusal nil history nil)))
  (is (re-find #"paused" (workloads/rollback-refusal (web 7 "web:1.2" :paused true) history nil)))
  (is (= "revision 7 is the current one" (workloads/rollback-refusal (web 7 "web:1.2") history 7)))
  (is (= "revision 3 is not among the Deployment's ReplicaSets" (workloads/rollback-refusal (web 7 "web:1.2") history 3)))
  (is (= "the Deployment has no earlier revision to go back to" (workloads/rollback-refusal (web 5 "web:1.0") history nil))))

(deftest the-rollback-confirmation-names-both-revisions
  (is (= ["roll Deployment shop/web back from revision 7 (web:1.2) to revision 6 (web:1.1)"
          "the rollback becomes a new revision; the Deployment's source (Git, a chart) still holds the newer one and will reapply it"]
         (workloads/rollback-items (web 7 "web:1.2") history nil))))

(deftest rolled-back-reads-images-and-rollout
  (let [target {:revision 6 :images ["web:1.1"]}]
    (is (workloads/rolled-back? (web 8 "web:1.1") target))
    (is (not (workloads/rolled-back? (web 7 "web:1.2") target)))
    (is (not (workloads/rolled-back? (assoc-in (web 8 "web:1.1") [:status :updatedReplicas] 1) target)))
    (is (not (workloads/rolled-back? nil target)))))

(deftest the-rollback-command
  (is (= ["--namespace=shop" "rollout" "undo" "deployment/web" "--to-revision=6"]
         (workloads/rollback-args {:namespace "shop" :name "web"} 6))))

;; --- cronjob suspend / resume ----------------------------------------------------

(defn- cronjob [& {:keys [suspend active deadline]}]
  {:metadata {:namespace "batch" :name "backup"}
   :spec (cond-> {:schedule "0 3 * * *"} (some? suspend) (assoc :suspend suspend) deadline (assoc :startingDeadlineSeconds deadline))
   :status (cond-> {} active (assoc :active (vec (repeat active {:kind "Job"}))))})

(deftest suspended-reads-only-true
  (is (workloads/suspended? (cronjob :suspend true)))
  (is (not (workloads/suspended? (cronjob :suspend false))))
  (is (not (workloads/suspended? (cronjob))) "unset is running"))

(deftest suspend-refusals
  (is (nil? (workloads/suspend-refusal (cronjob) true)))
  (is (nil? (workloads/suspend-refusal (cronjob :suspend true) false)))
  (is (= "it is suspended already" (workloads/suspend-refusal (cronjob :suspend true) true)))
  (is (= "it is not suspended" (workloads/suspend-refusal (cronjob) false)))
  (is (= "the CronJob no longer exists" (workloads/suspend-refusal nil true))))

(deftest suspend-and-resume-say-what-follows
  (is (= ["suspend CronJob batch/backup - no new Job starts until it is resumed"] (workloads/suspend-items (cronjob) true)))
  (is (= ["suspend CronJob batch/backup - no new Job starts until it is resumed" "2 Job(s) it started keep running"]
         (workloads/suspend-items (cronjob :active 2) true)))
  (is (= ["resume CronJob batch/backup on its schedule 0 3 * * *"
          "without startingDeadlineSeconds, a run missed while it was suspended starts at once"]
         (workloads/suspend-items (cronjob :suspend true) false)))
  (is (= ["resume CronJob batch/backup on its schedule 0 3 * * *"]
         (workloads/suspend-items (cronjob :suspend true :deadline 300) false))))

(deftest the-suspend-patch
  (is (= ["--namespace=batch" "patch" "cronjob" "backup" "--type=merge" "--patch={\"spec\":{\"suspend\":true}}"]
         (workloads/suspend-args "batch" "backup" true)))
  (is (= "--patch={\"spec\":{\"suspend\":false}}" (last (workloads/suspend-args "batch" "backup" false)))))
