;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.template-drift-resize-test
  "A pod's resources differ from its template after admission and after an
   in-place resize alike, so a resource difference names both causes, and a
   pod that reports a resize says so."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.template-drift :as drift]))

(def ^:private template-spec
  {:containers [{:name "app" :image "nginx:1.27"
                 :resources {:requests {:cpu "100m" :memory "128Mi"} :limits {:memory "256Mi"}}}]})

(def ^:private resized (assoc-in template-spec [:containers 0 :resources :requests :cpu] "500m"))
(def ^:private reimaged (assoc-in template-spec [:containers 0 :image] "mirror.local/nginx:1.27"))

(defn- pod [name spec & {:keys [status]}]
  {:metadata {:namespace "web" :name name
              :ownerReferences [{:kind "ReplicaSet" :name "shop-7d9" :controller true}]}
   :spec spec
   :status (or status {:phase "Running"})})

(defn- detect [& pods]
  (vec (drift/detect-template-drift
        {"pods" {:items pods}
         "replicasets" {:items [{:metadata {:namespace "web" :name "shop-7d9"
                                            :ownerReferences [{:kind "Deployment" :name "shop" :controller true}]}
                                 :spec {:template {:spec template-spec}}}]}
         "statefulsets" {:items []} "daemonsets" {:items []} "jobs" {:items []} "controllerrevisions" {:items []}})))

(deftest a-resource-difference-names-an-in-place-resize
  (let [[finding] (detect (pod "shop-7d9-a" resized))]
    (is (str/includes? (:hint finding) "cpu request 100m → 500m"))
    (is (str/includes? (:hint finding) "changed them on admission, or they were resized in place"))
    (is (str/includes? (:hint finding) "allocatedResources"))
    (is (not (str/includes? (:hint finding) "report a resize")))))

(deftest an-image-difference-names-admission-only
  (let [[finding] (detect (pod "shop-7d9-a" reimaged))]
    (is (str/includes? (:hint finding) "a mutating webhook or a LimitRange changed them on admission, so"))
    (is (not (str/includes? (:hint finding) "resized")))))

(deftest a-pod-reporting-a-resize-is-counted
  (testing "the PodResizeInProgress condition"
    (let [[finding] (detect (pod "shop-7d9-a" resized
                                 :status {:conditions [{:type "PodResizeInProgress" :status "True"}]})
                            (pod "shop-7d9-b" resized))]
      (is (str/ends-with? (:hint finding) "; 1 pod reports a resize pending or in progress"))))
  (testing "the PodResizePending condition and the older status.resize field"
    (let [[finding] (detect (pod "shop-7d9-a" resized
                                 :status {:conditions [{:type "PodResizePending" :status "True" :reason "Deferred"}]})
                            (pod "shop-7d9-b" resized :status {:resize "InProgress"}))]
      (is (str/ends-with? (:hint finding) "; 2 pods report a resize pending or in progress")))))

(deftest resize-evidence-is-read-from-the-pod
  (is (drift/resize-reported? {:status {:resize "Proposed"}}))
  (is (not (drift/resize-reported? {:status {:conditions [{:type "PodResizePending" :status "False"}]}})))
  (is (not (drift/resize-reported? {:status {}})))
  (is (drift/resource-drift? template-spec resized))
  (is (not (drift/resource-drift? template-spec reimaged))))

(deftest the-description-names-resize
  (is (str/includes? (:description (first drift/detectives)) "in-place resize")))
