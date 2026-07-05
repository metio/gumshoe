;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.subject-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.subject :as subject]))

(deftest display-test
  (is (= "Pod moodle/web-1" (subject/display (subject/subject "Pod" "moodle" "web-1"))))
  (is (= "Node worker-3" (subject/display (subject/subject "Node" "worker-3"))))
  (is (= "Node worker-3" (subject/display (subject/subject "Node" nil "worker-3")))))

(deftest facts-test
  (testing "pod facts summarise phase, readiness, restarts, node, and the container issue"
    (let [pod {:spec {:nodeName "worker-3"}
               :status {:phase "Running"
                        :containerStatuses [{:ready false :restartCount 12
                                             :state {:waiting {:reason "CrashLoopBackOff"}}}]}}]
      (is (= [["phase" "Running"] ["ready" "0/1 containers"] ["restarts" "12"]
              ["node" "worker-3"] ["issue" "CrashLoopBackOff"]]
             (subject/facts "Pod" pod)))))
  (testing "the failing container is named, and its image and memory limit are shown - what an OOM/pull error hinges on"
    (let [pod {:spec {:containers [{:name "app" :image "registry/moodle:4.3"
                                    :resources {:limits {:memory "256Mi" :cpu "500m"}}}
                                   {:name "sidecar" :image "registry/proxy:1.0"}]}
               :status {:phase "Running"
                        :containerStatuses [{:name "app" :ready false :restartCount 3
                                             :state {:terminated {:reason "OOMKilled"}}}
                                            {:name "sidecar" :ready true}]}}
          f (into {} (subject/facts "Pod" pod))]
      (is (= "app: OOMKilled" (get f "issue")))
      (is (= "registry/moodle:4.3" (get f "image")))
      (is (= "256Mi" (get f "mem limit")))))
  (testing "absent fields are dropped, not shown blank"
    (let [pod {:status {:phase "Running"
                        :containerStatuses [{:ready true :restartCount 0 :state {:running {}}}]}}]
      (is (= [["phase" "Running"] ["ready" "1/1 containers"]] (subject/facts "Pod" pod)))))
  (testing "node facts flag pressure and cordon"
    (let [node {:spec {:unschedulable true}
                :status {:conditions [{:type "Ready" :status "True"}
                                      {:type "DiskPressure" :status "True"}]
                         :nodeInfo {:kubeletVersion "v1.30.0"}}}
          f (into {} (subject/facts "Node" node))]
      (is (= "True" (get f "ready")))
      (is (= "DiskPressure" (get f "pressure")))
      (is (= "no (cordoned)" (get f "schedulable")))))
  (testing "workload facts show ready/desired"
    (is (= "1/3 ready" (-> (subject/facts "Deployment" {:spec {:replicas 3} :status {:readyReplicas 1}})
                           (->> (into {})) (get "replicas")))))
  (testing "DaemonSet facts read the DaemonSet status fields, not replicas"
    (let [f (into {} (subject/facts "DaemonSet"
                                    {:status {:desiredNumberScheduled 5 :numberReady 2
                                              :numberUnavailable 3}}))]
      (is (= "2/5 ready" (get f "ready")))
      (is (= "3" (get f "unavailable")))))
  (testing "PV facts surface the ceph backing so the operator can chase it on the ceph side"
    (let [pv {:status {:phase "Bound"}
              :spec {:capacity {:storage "50Gi"}
                     :persistentVolumeReclaimPolicy "Retain"
                     :claimRef {:namespace "moodle" :name "moodle-data"}
                     :csi {:driver "rbd.csi.ceph.com"
                           :volumeAttributes {:pool "kubernetes" :imageName "csi-vol-abc"}}}}
          f (into {} (subject/facts "PersistentVolume" pv))]
      (is (= "Retain" (get f "reclaimPolicy")))
      (is (= "moodle/moodle-data" (get f "claim")))
      (is (= "rbd.csi.ceph.com" (get f "csi driver")))
      (is (= "kubernetes" (get f "csi pool")) "any CSI driver's volume attributes surface generically")
      (is (= "csi-vol-abc" (get f "csi imageName")))))
  (testing "ingress facts show hosts and class"
    (let [f (into {} (subject/facts "Ingress" {:spec {:ingressClassName "nginx"
                                                      :rules [{:host "moodle.example.org"}]}}))]
      (is (= "moodle.example.org" (get f "hosts")))
      (is (= "nginx" (get f "class")))))
  (testing "node facts include allocatable capacity"
    (is (= "4 cpu · 16Gi mem · 110 pods"
           (-> (subject/facts "Node" {:status {:conditions [{:type "Ready" :status "True"}]
                                               :allocatable {:cpu "4" :memory "16Gi" :pods "110"}}})
               (->> (into {})) (get "allocatable")))))
  (testing "HPA facts show current/desired, range, target, and the scaling limit"
    (let [f (into {} (subject/facts "HorizontalPodAutoscaler"
                                    {:spec {:minReplicas 2 :maxReplicas 5 :scaleTargetRef {:kind "Deployment" :name "web"}}
                                     :status {:currentReplicas 5 :desiredReplicas 5
                                              :conditions [{:type "ScalingLimited" :status "True" :reason "TooManyReplicas"}]}}))]
      (is (= "5 current / 5 desired" (get f "replicas")))
      (is (= "2..5 replicas" (get f "range")))
      (is (= "Deployment/web" (get f "target")))
      (is (= "TooManyReplicas" (get f "limited")))))
  (testing "Job facts show completions and the failure reason"
    (let [f (into {} (subject/facts "Job" {:spec {:completions 1}
                                           :status {:succeeded 0 :failed 6
                                                    :conditions [{:type "Failed" :status "True" :reason "BackoffLimitExceeded"}]}}))]
      (is (= "0/1 succeeded" (get f "completions")))
      (is (= "6" (get f "failed")))
      (is (= "BackoffLimitExceeded" (get f "reason"))))))

(deftest situation-test
  (testing "pod situations"
    (is (= :crash-looping
           (subject/situation "Pod" {:status {:containerStatuses [{:state {:waiting {:reason "CrashLoopBackOff"}}}]}})))
    (is (= :image-pull-error
           (subject/situation "Pod" {:status {:containerStatuses [{:state {:waiting {:reason "ImagePullBackOff"}}}]}})))
    (is (= :oom-killed
           (subject/situation "Pod" {:status {:containerStatuses [{:state {:terminated {:reason "OOMKilled"}}}]}})))
    (is (= :pending (subject/situation "Pod" {:status {:phase "Pending"}})))
    (is (= :not-ready
           (subject/situation "Pod" {:status {:phase "Running" :containerStatuses [{:ready false}]}})))
    (is (= :ok
           (subject/situation "Pod" {:status {:phase "Running" :containerStatuses [{:ready true}]}}))))
  (testing "node situations"
    (is (= :not-ready (subject/situation "Node" {:status {:conditions [{:type "Ready" :status "False"}]}})))
    (is (= :disk-pressure (subject/situation "Node" {:status {:conditions [{:type "Ready" :status "True"}
                                                                           {:type "DiskPressure" :status "True"}]}})))
    (is (= :ok (subject/situation "Node" {:status {:conditions [{:type "Ready" :status "True"}]}}))))
  (testing "workload situations"
    (is (= :none-ready (subject/situation "Deployment" {:spec {:replicas 3} :status {:readyReplicas 0}})))
    (is (= :degraded (subject/situation "StatefulSet" {:spec {:replicas 3} :status {:readyReplicas 1}})))
    (is (= :ok (subject/situation "Deployment" {:spec {:replicas 3} :status {:readyReplicas 3}}))))
  (testing "DaemonSet situations read the DaemonSet status fields, not replicas"
    (is (= :none-ready (subject/situation "DaemonSet" {:status {:desiredNumberScheduled 5 :numberReady 0}})))
    (is (= :degraded (subject/situation "DaemonSet" {:status {:desiredNumberScheduled 5 :numberReady 2}})))
    (is (= :ok (subject/situation "DaemonSet" {:status {:desiredNumberScheduled 5 :numberReady 5}}))))
  (testing "PV situations flag released (orphaned data) volumes"
    (is (= :released (subject/situation "PersistentVolume" {:status {:phase "Released"}})))
    (is (= :ok (subject/situation "PersistentVolume" {:status {:phase "Bound"}}))))
  (testing "an HPA pegged at its max is flagged"
    (is (= :at-max (subject/situation "HorizontalPodAutoscaler" {:spec {:maxReplicas 5} :status {:currentReplicas 5}})))
    (is (= :ok (subject/situation "HorizontalPodAutoscaler" {:spec {:maxReplicas 5} :status {:currentReplicas 2}}))))
  (testing "a failed Job is flagged"
    (is (= :job-failed (subject/situation "Job" {:status {:conditions [{:type "Failed" :status "True"}]}})))
    (is (= :ok (subject/situation "Job" {:status {:conditions [{:type "Complete" :status "True"}]}}))))
  (testing "an unknown kind is never wrongly flagged"
    (is (= :ok (subject/situation "ConfigMap" {})))))

(deftest object-edges-test
  (testing "a pod links to its owner, node, and PVCs"
    (let [pod {:metadata {:namespace "moodle" :name "web-1"
                          :ownerReferences [{:kind "ReplicaSet" :name "web-abc"}]}
               :spec {:nodeName "worker-3"
                      :volumes [{:persistentVolumeClaim {:claimName "data"}}
                                {:emptyDir {}}]}}
          edges (subject/object-edges "Pod" pod)]
      (is (= [{:relation "owned by" :subject (subject/subject "ReplicaSet" "moodle" "web-abc")}
              {:relation "runs on" :subject (subject/subject "Node" nil "worker-3")}
              {:relation "mounts" :subject (subject/subject "PersistentVolumeClaim" "moodle" "data")}]
             edges))))
  (testing "an HTTPRoute links to its backend services, de-duplicated"
    (let [route {:metadata {:namespace "moodle"}
                 :spec {:rules [{:backendRefs [{:name "web" :kind "Service"}]}
                                {:backendRefs [{:name "web"}]}]}}]
      (is (= [{:relation "routes to" :subject (subject/subject "Service" "moodle" "web")}]
             (subject/object-edges "HTTPRoute" route)))))
  (testing "a PVC links to its PV, plus any owner"
    (is (= [{:relation "bound to" :subject (subject/subject "PersistentVolume" nil "pvc-xyz")}]
           (subject/object-edges "PersistentVolumeClaim" {:spec {:volumeName "pvc-xyz"}}))))
  (testing "a PV links back to the claim that uses it - the storage chain is walkable both ways"
    (is (= [{:relation "claimed by" :subject (subject/subject "PersistentVolumeClaim" "moodle" "data")}]
           (subject/object-edges "PersistentVolume" {:spec {:claimRef {:namespace "moodle" :name "data"}}}))))
  (testing "an Ingress links to its backend services, default backend included and de-duplicated"
    (is (= [{:relation "routes to" :subject (subject/subject "Service" "moodle" "web")}]
           (subject/object-edges "Ingress"
                                 {:metadata {:namespace "moodle"}
                                  :spec {:defaultBackend {:service {:name "web"}}
                                         :rules [{:http {:paths [{:backend {:service {:name "web"}}}]}}]}}))))
  (testing "an unknown kind still offers its owners"
    (is (= [{:relation "owned by" :subject (subject/subject "Deployment" "x" "app")}]
           (subject/object-edges "ConfigMap" {:metadata {:namespace "x"
                                                         :ownerReferences [{:kind "Deployment" :name "app"}]}})))))

(deftest exposes-service?-test
  (let [service (subject/subject "Service" "moodle" "web")
        route {:metadata {:namespace "moodle"} :spec {:rules [{:backendRefs [{:name "web"}]}]}}
        ingress {:metadata {:namespace "moodle"}
                 :spec {:rules [{:http {:paths [{:backend {:service {:name "web"}}}]}}]}}
        other {:metadata {:namespace "moodle"} :spec {:rules [{:backendRefs [{:name "api"}]}]}}]
    (testing "a route or ingress that routes to the service exposes it"
      (is (subject/exposes-service? "HTTPRoute" route service))
      (is (subject/exposes-service? "Ingress" ingress service)))
    (testing "one that routes elsewhere does not"
      (is (not (subject/exposes-service? "HTTPRoute" other service))))))

(deftest hpa-edges-test
  (testing "an HPA links to the workload it scales, so you can pivot to it"
    (is (= [{:relation "scales" :subject (subject/subject "Deployment" "thanos" "query")}]
           (subject/object-edges "HorizontalPodAutoscaler"
                                 {:metadata {:namespace "thanos"}
                                  :spec {:scaleTargetRef {:kind "Deployment" :name "query"}}})))))

(deftest service-selects-pod?-test
  (let [pod {:metadata {:labels {:app "web" :tier "frontend"}}}]
    (testing "a service whose selector is a subset of the pod's labels selects it"
      (is (subject/service-selects-pod? {:spec {:selector {:app "web"}}} pod))
      (is (subject/service-selects-pod? {:spec {:selector {:app "web" :tier "frontend"}}} pod)))
    (testing "a mismatching or empty selector does not"
      (is (not (subject/service-selects-pod? {:spec {:selector {:app "api"}}} pod)))
      (is (not (subject/service-selects-pod? {:spec {:selector {}}} pod)))
      (is (not (subject/service-selects-pod? {:spec {}} pod))))))

(deftest from-finding-test
  (testing "a namespaced finding infers kind from the detective and splits the component"
    (is (= (subject/subject "Pod" "moodle" "web-1")
           (subject/from-finding {:detective "pods" :component "moodle/web-1"}))))
  (testing "a cluster-scoped finding has no namespace"
    (is (= (subject/subject "Node" nil "worker-3")
           (subject/from-finding {:detective "nodes" :component "worker-3"}))))
  (testing "the split workload detectives each map to their kind"
    (is (= "Deployment" (:kind (subject/from-finding {:detective "deployments" :component "a/b"}))))
    (is (= "StatefulSet" (:kind (subject/from-finding {:detective "statefulsets" :component "a/b"}))))
    (is (= "DaemonSet" (:kind (subject/from-finding {:detective "daemonsets" :component "a/b"}))))
    (is (= "Pod" (:kind (subject/from-finding {:detective "stuck-pods" :component "a/b"})))))
  (testing "an explicit :subject on a finding is used verbatim (Loki labels by role, not name)"
    (is (= (subject/subject "Deployment" "loki" "loki-distributor")
           (subject/from-finding {:detective "loki-health" :component "distributor"
                                  :subject (subject/subject "Deployment" "loki" "loki-distributor")}))))
  (testing "an unmappable detective yields nil rather than a wrong guess"
    (is (nil? (subject/from-finding {:detective "dns-transports" :component "dns.example.org"})))
    (is (nil? (subject/from-finding {:detective "overcommit" :component "cluster"})))))

(deftest register-kind-adds-a-custom-drill-down-target-test
  (testing "a plugin makes a CRD fetchable and gives it edges to traverse"
    (subject/register-kind! "WidgetSet"
                            {:type "widgetsets.acme.example"
                             :edges (fn [obj] [{:relation "manages"
                                                :subject (subject/subject "Pod" "ns" (get-in obj [:spec :pod]))}])})
    (is (= "widgetsets.acme.example" (subject/kind->type "WidgetSet")))
    (is (= [{:relation "manages" :subject (subject/subject "Pod" "ns" "w-1")}]
           (subject/object-edges "WidgetSet" {:spec {:pod "w-1"}}))))
  (testing "an unregistered kind falls back to following ownerReferences"
    (is (= [] (subject/object-edges "SomeUnknownKind" {})))))

(deftest registered-facts-append-to-a-kind-test
  (testing "a plugin's fact contributor enriches the built-in panel, keeping core generic"
    (subject/register-facts! "PersistentVolume"
                             (fn [pv] [["ceph pool" (-> pv :spec :csi :volumeAttributes :pool)]
                                       ["empty" nil]]))
    (let [pv {:spec {:csi {:driver "rbd.csi.ceph.com" :volumeAttributes {:pool "kubernetes"}}}}
          f (into {} (subject/facts "PersistentVolume" pv))]
      (is (= "rbd.csi.ceph.com" (get f "csi driver")) "built-in generic facts still present")
      (is (= "kubernetes" (get f "ceph pool")) "the plugin's fact is appended")
      (is (not (contains? f "empty")) "nil-valued plugin facts are dropped"))
    (reset! @#'subject/fact-contributors {})))
