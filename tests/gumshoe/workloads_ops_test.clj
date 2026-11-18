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
