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
  (is (= [{:revision 7 :source "web-c" :images ["web:1.2"]}
          {:revision 6 :source "web-b" :images ["web:1.1"]}
          {:revision 5 :source "web-a" :images ["web:1.0"]}]
         (workloads/revisions "Deployment" (web 7 "web:1.2") history)))
  (testing "without a uid the owner name decides"
    (is (= 4 (count (workloads/revisions "Deployment" (update (web 7 "web:1.2") :metadata dissoc :uid) history))))))

(deftest the-rollback-target
  (is (= 6 (:revision (workloads/rollback-target "Deployment" (web 7 "web:1.2") history nil))) "the previous revision")
  (is (= 5 (:revision (workloads/rollback-target "Deployment" (web 7 "web:1.2") history 5))))
  (is (nil? (workloads/rollback-target "Deployment" (web 7 "web:1.2") history 3)))
  (is (nil? (workloads/rollback-target "Deployment" (web 5 "web:1.0") history nil)) "the oldest has nothing before it"))

(deftest rollback-refusals
  (is (nil? (workloads/rollback-refusal "Deployment" (web 7 "web:1.2") history nil)))
  (is (= "the Deployment no longer exists" (workloads/rollback-refusal "Deployment" nil history nil)))
  (is (re-find #"paused" (workloads/rollback-refusal "Deployment" (web 7 "web:1.2" :paused true) history nil)))
  (is (= "revision 7 is the current one" (workloads/rollback-refusal "Deployment" (web 7 "web:1.2") history 7)))
  (is (= "revision 3 is not among the Deployment's ReplicaSets" (workloads/rollback-refusal "Deployment" (web 7 "web:1.2") history 3)))
  (is (= "the Deployment has no earlier revision to go back to" (workloads/rollback-refusal "Deployment" (web 5 "web:1.0") history nil))))

(deftest the-rollback-confirmation-names-both-revisions
  (is (= ["roll Deployment shop/web back from revision 7 (web:1.2) to revision 6 (web:1.1)"
          "the rollback becomes a new revision; the Deployment's source (Git, a chart) still holds the newer one and will reapply it"]
         (workloads/rollback-items "Deployment" (web 7 "web:1.2") history nil))))

(deftest rolled-back-reads-images-and-rollout
  (let [target {:revision 6 :images ["web:1.1"]}]
    (is (workloads/rolled-back? "Deployment" (web 8 "web:1.1") target))
    (is (not (workloads/rolled-back? "Deployment" (web 7 "web:1.2") target)))
    (is (not (workloads/rolled-back? "Deployment" (assoc-in (web 8 "web:1.1") [:status :updatedReplicas] 1) target)))
    (is (not (workloads/rolled-back? "Deployment" nil target)))))

(deftest the-rollback-command
  (is (= ["--namespace=shop" "rollout" "undo" "deployment/web" "--to-revision=6"]
         (workloads/rollback-args {:type "deployments" :namespace "shop" :name "web"} 6)))
  (is (= ["--namespace=db" "rollout" "undo" "statefulset/pg" "--to-revision=2"]
         (workloads/rollback-args {:type "statefulsets" :namespace "db" :name "pg"} 2)))
  (is (= ["--namespace=kube-system" "rollout" "undo" "daemonset/agent" "--to-revision=1"]
         (workloads/rollback-args {:type "daemonsets" :namespace "kube-system" :name "agent"} 1))))

;; ControllerRevisions as the API server returns them: the revision is a field,
;; the template sits under data.spec, and the restored one is renumbered highest.
(defn- revision [owner-kind owner name number image & {:keys [uid] :or {uid "uid-pg"}}]
  {:metadata {:namespace "db" :name name :ownerReferences [{:kind owner-kind :name owner :uid uid :controller true}]}
   :revision number
   :data {:spec {:template {:spec {:containers [{:name "c" :image image}]}}}}})

(defn- pg [image & {:keys [strategy partition updated] :or {updated 3}}]
  {:metadata {:namespace "db" :name "pg" :uid "uid-pg" :generation 4}
   :spec (cond-> {:replicas 3 :template {:spec {:containers [{:name "c" :image image}]}}}
           strategy (assoc-in [:updateStrategy :type] strategy)
           partition (assoc :updateStrategy {:type "RollingUpdate" :rollingUpdate {:partition partition}}))
   :status {:observedGeneration 4 :updatedReplicas updated :readyReplicas 3
            :currentRevision "pg-c" :updateRevision "pg-c"}})

(def ^:private pg-history
  {:items [(revision "StatefulSet" "pg" "pg-a" 1 "pg:16.1") (revision "StatefulSet" "pg" "pg-c" 3 "pg:16.3")
           (revision "StatefulSet" "pg" "pg-b" 2 "pg:16.2")
           (revision "DaemonSet" "pg" "pg-ds" 9 "agent:1" :uid "uid-other")
           (revision "StatefulSet" "pg" "pg-gone" 8 "pg:15" :uid "uid-of-a-deleted-pg")]})

(deftest statefulset-revisions-come-from-controllerrevisions
  (is (= [{:revision 3 :source "pg-c" :images ["pg:16.3"]}
          {:revision 2 :source "pg-b" :images ["pg:16.2"]}
          {:revision 1 :source "pg-a" :images ["pg:16.1"]}]
         (workloads/revisions "StatefulSet" (pg "pg:16.3") pg-history))
      "only its own kind and uid")
  (is (= 3 (workloads/current-revision "StatefulSet" (pg "pg:16.3") pg-history)) "the highest is the current one")
  (is (nil? (workloads/current-revision "StatefulSet" (pg "pg:16.3") {:items []}))))

(deftest statefulset-rollback-targets-and-refusals
  (is (= {:revision 2 :source "pg-b" :images ["pg:16.2"]} (workloads/rollback-target "StatefulSet" (pg "pg:16.3") pg-history nil)))
  (is (= 1 (:revision (workloads/rollback-target "StatefulSet" (pg "pg:16.3") pg-history 1))))
  (is (nil? (workloads/rollback-refusal "StatefulSet" (pg "pg:16.3") pg-history nil)))
  (is (= "the StatefulSet no longer exists" (workloads/rollback-refusal "StatefulSet" nil pg-history nil)))
  (is (= "revision 3 is the current one" (workloads/rollback-refusal "StatefulSet" (pg "pg:16.3") pg-history 3)))
  (is (= "revision 5 is not among the StatefulSet's ControllerRevisions" (workloads/rollback-refusal "StatefulSet" (pg "pg:16.3") pg-history 5)))
  (is (= "the StatefulSet has no earlier revision to go back to"
         (workloads/rollback-refusal "StatefulSet" (pg "pg:16.1") {:items [(revision "StatefulSet" "pg" "pg-a" 1 "pg:16.1")]} nil)))
  (is (= "the StatefulSet updates OnDelete, so a rollback replaces no pod until each is deleted by hand"
         (workloads/rollback-refusal "StatefulSet" (pg "pg:16.3" :strategy "OnDelete") pg-history nil)))
  (is (= "the StatefulSet's rollingUpdate.partition is 2, so pods below it stay on their revision and the rollback never completes"
         (workloads/rollback-refusal "StatefulSet" (pg "pg:16.3" :partition 2) pg-history nil)))
  (is (nil? (workloads/rollback-refusal "StatefulSet" (pg "pg:16.3" :partition 0) pg-history nil)) "partition 0 updates every pod"))

(deftest statefulset-rollback-confirmation
  (is (= ["roll StatefulSet db/pg back from revision 3 (pg:16.3) to revision 2 (pg:16.2)"
          "a StatefulSet replaces its pods one at a time from the highest ordinal, and restores only the pod template - volumes keep what the newer revision wrote"
          "the rollback becomes a new revision; the StatefulSet's source (Git, a chart) still holds the newer one and will reapply it"]
         (workloads/rollback-items "StatefulSet" (pg "pg:16.3") pg-history nil))))

(deftest statefulset-rolled-back
  (let [target {:revision 2 :images ["pg:16.2"]}]
    (is (workloads/rolled-back? "StatefulSet" (pg "pg:16.2") target))
    (is (not (workloads/rolled-back? "StatefulSet" (pg "pg:16.2" :updated 1) target)) "still replacing pods")
    (is (not (workloads/rolled-back? "StatefulSet" (pg "pg:16.3") target)))))

(def ^:private agent-history
  {:items [(revision "DaemonSet" "agent" "agent-a" 1 "agent:1" :uid "uid-agent")
           (revision "DaemonSet" "agent" "agent-b" 2 "agent:2" :uid "uid-agent")]})

(defn- agent-ds [image & {:keys [updated strategy] :or {updated 2}}]
  {:metadata {:namespace "db" :name "agent" :uid "uid-agent" :generation 2}
   :spec (cond-> {:template {:spec {:containers [{:name "c" :image image}]}}}
           strategy (assoc-in [:updateStrategy :type] strategy))
   :status {:observedGeneration 2 :desiredNumberScheduled 2 :updatedNumberScheduled updated :numberAvailable 2}})

(deftest daemonset-rollback
  (is (= 1 (:revision (workloads/rollback-target "DaemonSet" (agent-ds "agent:2") agent-history nil))))
  (is (nil? (workloads/rollback-refusal "DaemonSet" (agent-ds "agent:2") agent-history nil)))
  (is (= "the DaemonSet updates OnDelete, so a rollback replaces no pod until each is deleted by hand"
         (workloads/rollback-refusal "DaemonSet" (agent-ds "agent:2" :strategy "OnDelete") agent-history nil)))
  (is (= ["roll DaemonSet db/agent back from revision 2 (agent:2) to revision 1 (agent:1)"
          "the rollback becomes a new revision; the DaemonSet's source (Git, a chart) still holds the newer one and will reapply it"]
         (workloads/rollback-items "DaemonSet" (agent-ds "agent:2") agent-history nil)))
  (is (workloads/rolled-back? "DaemonSet" (agent-ds "agent:1") {:revision 1 :images ["agent:1"]}))
  (is (not (workloads/rolled-back? "DaemonSet" (agent-ds "agent:1" :updated 1) {:revision 1 :images ["agent:1"]}))))

(deftest every-kind-has-a-history
  (is (= (set (map second workloads/types)) (set (keys workloads/history-types)))))

;; --- deployment pause / resume ---------------------------------------------------

(deftest paused-reads-only-true
  (is (workloads/paused? (deployment :paused true)))
  (is (not (workloads/paused? (deployment))))
  (is (not (workloads/paused? (assoc-in (deployment) [:spec :paused] false)))))

(deftest pause-refusals
  (is (nil? (workloads/pause-refusal (deployment) true)))
  (is (nil? (workloads/pause-refusal (deployment :paused true) false)))
  (is (= "it is paused already" (workloads/pause-refusal (deployment :paused true) true)))
  (is (= "it is not paused" (workloads/pause-refusal (deployment) false)))
  (is (= "the Deployment no longer exists" (workloads/pause-refusal nil true)))
  (is (= "the Deployment no longer exists" (workloads/pause-refusal nil false))))

(deftest pausing-says-what-follows
  (is (= ["pause Deployment shop/web - changes to its pod template are kept but not rolled out until it is resumed"
          "rollout restart and rollback refuse a paused Deployment"]
         (workloads/pause-items (deployment) nil true)))
  (testing "a rollout in progress stops where it is"
    (is (= "its rollout in progress stops where it is (1 of 3 pods updated), old and new pods side by side"
           (second (workloads/pause-items (deployment :updated 1) nil true)))))
  (testing "an autoscaler keeps scaling"
    (is (some #{"HorizontalPodAutoscaler web keeps scaling it - pausing stops rollouts, not scaling"}
              (workloads/pause-items (deployment) "web" true))))
  (testing "resuming"
    (is (= ["resume Deployment shop/web - every pod template change made while it was paused rolls out now"]
           (workloads/pause-items (deployment :paused true) "web" false)))))

(deftest the-pause-commands
  (is (= ["--namespace=shop" "rollout" "pause" "deployment/web"] (workloads/pause-args {:namespace "shop" :name "web"} true)))
  (is (= ["--namespace=shop" "rollout" "resume" "deployment/web"] (workloads/pause-args {:namespace "shop" :name "web"} false))))

(deftest pause-applied-checks-the-controller
  (testing "paused, once observed"
    (is (workloads/pause-applied? (deployment :paused true) true))
    (is (not (workloads/pause-applied? (deployment :paused true :generation 3 :observed 2) true)) "not yet observed")
    (is (not (workloads/pause-applied? (deployment) true))))
  (testing "resumed, once rolled out"
    (is (workloads/pause-applied? (deployment) false))
    (is (not (workloads/pause-applied? (deployment :updated 1) false)) "still rolling out")
    (is (not (workloads/pause-applied? (deployment :paused true) false))))
  (is (not (workloads/pause-applied? nil true))))

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
