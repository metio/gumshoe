;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.onset-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.custom-resources :as custom-resources]
            [gumshoe.detectives.disruption :as disruption]
            [gumshoe.detectives.nodes :as nodes]
            [gumshoe.detectives.pod-startup :as pod-startup]
            [gumshoe.detectives.pods :as pods]
            [gumshoe.detectives.services :as services]
            [gumshoe.detectives.storage :as storage]
            [gumshoe.detectives.terminating :as terminating]
            [gumshoe.detectives.workloads :as workloads]
            [gumshoe.onset :as onset]
            [gumshoe.tools.flux :as flux]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(deftest since-accepts-instants-and-their-strings
  (is (= "2026-11-18T10:00:00Z" (:since (onset/since {} "2026-11-18T10:00:00Z"))))
  (is (= "2026-11-18T10:00:00Z" (:since (onset/since {} (java.time.Instant/parse "2026-11-18T10:00:00Z")))))
  (is (= "2026-11-18T10:00:00.250Z" (:since (onset/since {} "2026-11-18T10:00:00.25Z"))))
  (doseq [value [nil "" "yesterday" 42 {}]]
    (is (= {:summary "x"} (onset/since {:summary "x"} value)) (pr-str value))))

(deftest the-earliest-onset-of-several
  (is (= "2026-11-17T08:00:00Z"
         (onset/earliest [{:since "2026-11-18T10:00:00Z"} {} {:since "2026-11-17T08:00:00Z"} {:since "garbage"}])))
  (is (nil? (onset/earliest [{} {:since nil}])))
  (is (nil? (onset/earliest []))))

(deftest a-label-reads-in-the-largest-sensible-unit
  (is (= "for 0m" (onset/label "2026-11-18T12:00:00Z" now)))
  (is (= "for 59m" (onset/label "2026-11-18T11:00:01Z" now)))
  (is (= "for 1h" (onset/label "2026-11-18T11:00:00Z" now)))
  (is (= "for 47h" (onset/label "2026-11-16T12:00:01Z" now)))
  (is (= "since 2026-11-16" (onset/label "2026-11-16T12:00:00Z" now)))
  (is (= "since 2020-01-01" (onset/label "2020-01-01T00:00:00Z" now)))
  (testing "no label for an onset in the future, an unreadable one, or no clock"
    (is (nil? (onset/label "2026-11-18T12:05:00Z" now)))
    (is (nil? (onset/label "soon" now)))
    (is (nil? (onset/label nil now)))
    (is (nil? (onset/label "2026-11-18T11:00:00Z" nil)))))

(deftest a-rolled-up-entry-carries-its-earliest-onset
  (is (= ["2026-11-17T08:00:00Z" nil]
         (map :since (detective/roll-up [{:severity :critical :summary "down" :component "a" :since "2026-11-18T10:00:00Z"}
                                         {:severity :critical :summary "down" :component "b" :since "2026-11-17T08:00:00Z"}
                                         {:severity :warning :summary "slow" :component "c"}])))))

(deftest the-text-report-says-how-long
  (let [out (with-out-str
              (binding [*err* (java.io.StringWriter.)]
                (detective/report! [{:name "nodes"}]
                                   [{:detective "nodes" :severity :critical :summary "node is not Ready (status False)"
                                     :component "n1" :since "2020-01-01T00:00:00Z"}
                                    {:detective "nodes" :severity :info :summary "node is cordoned (unschedulable)"
                                     :component "n2"}]
                                   "text")))]
    (is (str/includes? out "node is not Ready (status False)  since 2020-01-01"))
    (is (re-find #"node is cordoned \(unschedulable\)\s*\n" out) "no onset, no label")))

;; --- the detectives that know a start ------------------------------------------

(defn- onsets [findings] (mapv :since findings))

(deftest nodes-take-their-condition-transitions
  (is (= ["2026-11-18T09:00:00Z" "2026-11-18T10:00:00Z" nil]
         (onsets (nodes/detect-node-problems
                  {"nodes" {:items [{:metadata {:name "n1"}
                                     :spec {:unschedulable true}
                                     :status {:conditions [{:type "Ready" :status "False" :lastTransitionTime "2026-11-18T09:00:00Z"}
                                                           {:type "DiskPressure" :status "True" :lastTransitionTime "2026-11-18T10:00:00Z"}]}}]}})))))

(deftest a-stuck-rollout-and-a-failed-job-take-their-conditions
  (is (= ["2026-11-18T08:00:00Z"]
         (onsets (workloads/detect-stalled-rollouts
                  {"deployments" {:items [{:metadata {:namespace "a" :name "d"}
                                           :status {:conditions [{:type "Progressing" :status "False" :reason "ProgressDeadlineExceeded"
                                                                  :lastTransitionTime "2026-11-18T08:00:00Z"}]}}]}}))))
  (is (= ["2026-11-18T07:00:00Z"]
         (onsets (workloads/detect-failed-jobs
                  {"jobs" {:items [{:metadata {:namespace "a" :name "j"}
                                    :status {:conditions [{:type "Failed" :status "True" :reason "BackoffLimitExceeded"
                                                           :lastTransitionTime "2026-11-18T07:00:00Z"}]}}]}})))))

(deftest pending-pods-take-their-scheduling-transition-or-creation
  (is (= ["2026-11-18T11:00:00Z" "2026-11-18T10:30:00Z"]
         (onsets (pods/detect-unhealthy-pods
                  {:now now
                   "pods" {:items [{:metadata {:namespace "a" :name "u" :creationTimestamp "2026-11-18T10:00:00Z"}
                                    :status {:phase "Pending"
                                             :conditions [{:type "PodScheduled" :status "False" :reason "Unschedulable"
                                                           :message "0/1 nodes are available: 1 Insufficient cpu."
                                                           :lastTransitionTime "2026-11-18T11:00:00Z"}]}}
                                   {:metadata {:namespace "a" :name "p" :creationTimestamp "2026-11-18T10:30:00Z"}
                                    :status {:phase "Pending"}}]}})))))

(deftest a-stalled-startup-takes-its-binding-time
  (is (= ["2026-11-18T11:00:00Z"]
         (onsets (pod-startup/detect-stalled-startups
                  {:now now "events" {:items []}
                   "pods" {:items [{:metadata {:namespace "a" :name "p" :creationTimestamp "2026-11-18T10:00:00Z"}
                                    :spec {:nodeName "n1"}
                                    :status {:phase "Pending"
                                             :conditions [{:type "PodScheduled" :status "True"
                                                           :lastTransitionTime "2026-11-18T11:00:00Z"}]}}]}})))))

(deftest a-stuck-deletion-takes-its-deletion-time
  (is (= ["2026-11-18T11:00:00Z"]
         (onsets (terminating/detect-stuck-terminating
                  {:now now "configmaps" {:items [{:metadata {:namespace "a" :name "c" :deletionTimestamp "2026-11-18T11:00:00Z"
                                                              :finalizers ["example.com/x"]}}]}})))))

(deftest storage-takes-creation-and-condition-times
  (is (= ["2026-11-18T09:00:00Z" "2026-11-18T10:00:00Z"]
         (onsets (storage/detect-storage-problems
                  {:now now
                   "storageclasses" {:items []}
                   "pods" {:items [{:metadata {:namespace "a" :name "p"}
                                    :spec {:volumes [{:persistentVolumeClaim {:claimName "pending"}}
                                                     {:persistentVolumeClaim {:claimName "resize"}}]}}]}
                   "persistentvolumeclaims"
                   {:items [{:metadata {:namespace "a" :name "pending" :creationTimestamp "2026-11-18T09:00:00Z"}
                             :status {:phase "Pending"}}
                            {:metadata {:namespace "a" :name "resize"}
                             :status {:phase "Bound"
                                      :conditions [{:type "NodeResizeError" :status "True" :lastTransitionTime "2026-11-18T10:00:00Z"}]}}]}})))))

(deftest a-load-balancer-takes-its-creation-time
  (is (= ["2026-11-18T11:00:00Z"]
         (onsets (services/detect-pending-load-balancers
                  {:now now "services" {:items [{:metadata {:namespace "a" :name "lb" :creationTimestamp "2026-11-18T11:00:00Z"}
                                                 :spec {:type "LoadBalancer"}}]}})))))

(deftest an-hpa-takes-its-condition-times
  (is (= ["2026-11-18T09:00:00Z" "2026-11-18T10:00:00Z"]
         (onsets (disruption/detect-failing-hpas
                  {"horizontalpodautoscalers"
                   {:items [{:metadata {:namespace "a" :name "h"}
                             :status {:conditions [{:type "AbleToScale" :status "False" :reason "FailedGetScale"
                                                    :lastTransitionTime "2026-11-18T09:00:00Z"}
                                                   {:type "ScalingActive" :status "False" :reason "FailedGetResourceMetric"
                                                    :lastTransitionTime "2026-11-18T10:00:00Z"}]}}]}})))))

(deftest api-services-and-custom-resources-take-their-condition-times
  (is (= ["2026-11-18T09:00:00Z"]
         (onsets (custom-resources/detect-api-services
                  {"apiservices" {:items [{:metadata {:name "v1beta1.metrics.k8s.io"}
                                           :spec {:service {:namespace "kube-system" :name "metrics-server"}}
                                           :status {:conditions [{:type "Available" :status "False"
                                                                  :lastTransitionTime "2026-11-18T09:00:00Z"}]}}]}}))))
  (is (= ["2026-11-18T09:00:00Z"]
         (onsets (custom-resources/detect-custom-resource-conditions
                  {:now now "custom-resources" [{:kind "Widget" :metadata {:namespace "a" :name "w"}
                                                 :status {:conditions [{:type "Ready" :status "False" :reason "Broken"
                                                                        :lastTransitionTime "2026-11-18T09:00:00Z"}]}}]})))))

(deftest flux-takes-its-ready-transition
  (is (= ["2026-11-18T09:00:00Z"]
         (onsets (flux/detect-kustomization-problems
                  {:now now flux/kustomization-type
                   {:items [{:metadata {:namespace "flux-system" :name "apps"}
                             :status {:conditions [{:type "Ready" :status "False" :reason "BuildFailed"
                                                    :lastTransitionTime "2026-11-18T09:00:00Z"}]}}]}})))))

(deftest a-finding-without-a-recorded-start-has-no-onset
  (is (= [nil]
         (onsets (nodes/detect-node-problems {"nodes" {:items [{:metadata {:name "n1"} :status {:conditions []}}]}})))))
