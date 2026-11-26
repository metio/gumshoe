;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pod-waiting-reasons-test
  "Pods that wait on purpose and pods that wait on a cause: a running init
   container is a pod starting, while a blocking waiting reason or a failed
   init container is named instead of the node's kubelet."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.pod-startup :as pod-startup]
            [gumshoe.detectives.pods :as pods]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(defn- pending-pod
  "A pod bound to worker-1 at bound, Pending, with the given statuses."
  [name & {:keys [bound init-containers init-statuses statuses initialized]
           :or {bound "2026-11-18T11:00:00Z"}}]
  {:metadata {:namespace "shop" :name name :creationTimestamp bound}
   :spec (cond-> {:nodeName "worker-1" :containers [{:name "app"}]}
           init-containers (assoc :initContainers init-containers))
   :status {:phase "Pending"
            :conditions (cond-> [{:type "PodScheduled" :status "True" :lastTransitionTime bound}]
                          initialized (conj {:type "Initialized" :status initialized :lastTransitionTime bound}))
            :initContainerStatuses (vec init-statuses)
            :containerStatuses (vec (or statuses [{:name "app" :restartCount 0
                                                   :state {:waiting {:reason "PodInitializing"}}}]))}})

(defn- startups [pods]
  (pod-startup/detect-stalled-startups {:now now "pods" {:items pods} "events" {:items []}}))

(defn- unhealthy [pods]
  (pods/detect-unhealthy-pods {:now now "pods" {:items pods}}))

;; --- init containers ------------------------------------------------------------

(deftest a-pod-running-its-init-container-is-starting
  (let [pod (pending-pod "web-1" :initialized "False"
                         :init-containers [{:name "wait-for-db"}]
                         :init-statuses [{:name "wait-for-db" :restartCount 0
                                          :state {:running {:startedAt "2026-11-18T11:01:00Z"}}}])]
    (is (pod-startup/initializing? pod))
    (is (empty? (startups [pod])) "an hour of waiting for a dependency is not a kubelet stall")
    (is (empty? (unhealthy [pod])))))

(deftest a-running-sidecar-does-not-make-a-stuck-pod-initializing
  (let [pod (pending-pod "web-1" :initialized "False"
                         :init-containers [{:name "proxy" :restartPolicy "Always"} {:name "migrate"}]
                         :init-statuses [{:name "proxy" :restartCount 0
                                          :state {:running {:startedAt "2026-11-18T11:01:00Z"}}}
                                         {:name "migrate" :restartCount 0
                                          :state {:waiting {:reason "ContainerCreating"}}}])]
    (is (not (pod-startup/initializing? pod)))
    (is (= ["pod is bound to node worker-1 but has not started for over 30 minutes"]
           (map :summary (startups [pod]))))))

(deftest a-pod-whose-containers-are-all-still-being-created-stalls
  (let [pod (pending-pod "web-1" :statuses [{:name "app" :restartCount 0
                                              :state {:waiting {:reason "ContainerCreating"}}}])]
    (is (= [:critical] (map :severity (startups [pod]))))))

(deftest a-failed-init-container-is-named-instead-of-the-kubelet
  (let [pod (pending-pod "web-1" :initialized "False"
                         :init-containers [{:name "migrate"}]
                         :init-statuses [{:name "migrate" :restartCount 0
                                          :state {:terminated {:exitCode 2 :reason "Error"
                                                               :finishedAt "2026-11-18T11:02:00Z"}}}])
        [finding & more] (startups [pod])]
    (is (nil? more))
    (is (= "init container migrate failed (exit code 2, Error)" (:summary finding)))
    (is (str/includes? (:hint finding) "kubectl logs -c migrate"))
    (is (not (str/includes? (:hint finding) "kubelet")))))

(deftest an-init-container-that-finished-cleanly-is-no-failure
  (let [pod (pending-pod "web-1" :initialized "True"
                         :init-containers [{:name "migrate"}]
                         :init-statuses [{:name "migrate" :restartCount 0
                                          :state {:terminated {:exitCode 0 :reason "Completed"}}}]
                         :statuses [{:name "app" :restartCount 0 :state {:waiting {:reason "ContainerCreating"}}}])]
    (is (= ["pod is bound to node worker-1 but has not started for over 30 minutes"]
           (map :summary (startups [pod]))))))

(deftest an-init-container-waiting-on-a-blocking-reason-is-named
  (let [pod (pending-pod "web-1" :initialized "False"
                         :init-containers [{:name "migrate"}]
                         :init-statuses [{:name "migrate" :restartCount 0
                                          :state {:waiting {:reason "ErrImageNeverPull"
                                                            :message "Container image \"migrate:1\" is not present with pull policy of Never"}}}])]
    (is (= ["container migrate is waiting: ErrImageNeverPull"] (map :summary (unhealthy [pod]))))
    (is (empty? (startups [pod])) "the pods detective names it, so the node is not blamed")))

;; --- waiting reasons --------------------------------------------------------------

(def ^:private immediate-reasons ["ErrImageNeverPull" "ImageInspectError" "PreStartHookError"])

(defn- running-pod
  "A Running pod whose sidecar is up and whose app container waits on reason."
  [reason & {:keys [finished]}]
  {:metadata {:namespace "shop" :name "web-1" :creationTimestamp "2026-11-18T09:00:00Z"}
   :spec {:nodeName "worker-1" :containers [{:name "app"}]}
   :status {:phase "Running"
            :conditions [{:type "PodScheduled" :status "True" :lastTransitionTime "2026-11-18T09:00:00Z"}]
            :initContainerStatuses [{:name "proxy" :restartCount 0 :ready true
                                     :state {:running {:startedAt "2026-11-18T09:01:00Z"}}}]
            :containerStatuses [(cond-> {:name "app" :restartCount 1
                                         :state {:waiting {:reason reason :message (str reason " happened")}}}
                                  finished (assoc :lastState {:terminated {:exitCode 0 :finishedAt finished}}))]}})

(deftest blocking-reasons-on-a-running-pod-are-critical
  (doseq [reason immediate-reasons]
    (testing reason
      (is (= [[:critical (str "container app is waiting: " reason)]]
             (map (juxt :severity :summary) (unhealthy [(running-pod reason)])))))))

(deftest blocking-reasons-on-a-pending-pod-are-named-and-not-blamed-on-the-node
  (doseq [reason (conj immediate-reasons "CreateContainerError")]
    (testing reason
      (let [pod (pending-pod "web-1" :statuses [{:name "app" :restartCount 0
                                                  :state {:waiting {:reason reason}}}])]
        (is (= [(str "container app is waiting: " reason)] (map :summary (unhealthy [pod]))))
        (is (empty? (startups [pod])))))))

(deftest create-container-error-is-reported-after-its-grace-period
  (testing "within five minutes of the container's last run it is the kubelet retrying"
    (is (empty? (unhealthy [(running-pod "CreateContainerError" :finished "2026-11-18T11:57:00Z")]))))
  (testing "past five minutes it is a cause"
    (is (= [[:critical "container app is waiting: CreateContainerError"]]
           (map (juxt :severity :summary)
                (unhealthy [(running-pod "CreateContainerError" :finished "2026-11-18T11:50:00Z")])))))
  (testing "without a last run, the wait counts from the pod's binding"
    (is (= 1 (count (unhealthy [(running-pod "CreateContainerError")])))))
  (testing "a pod bound a minute ago is still within the grace period"
    (is (empty? (unhealthy [(pending-pod "web-1" :bound "2026-11-18T11:59:00Z"
                                         :statuses [{:name "app" :restartCount 0
                                                     :state {:waiting {:reason "CreateContainerError"}}}])])))))

(deftest a-grace-period-without-a-clock-reports
  (is (= 1 (count (pods/detect-unhealthy-pods
                   {"pods" {:items [(running-pod "CreateContainerError" :finished "2026-11-18T11:59:00Z")]}})))))
