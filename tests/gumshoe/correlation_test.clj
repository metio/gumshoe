;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.correlation-test
  (:require [cheshire.core]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.correlation :as correlation]
            [gumshoe.detective :as detective]
            [gumshoe.subject :as subject]))

(defn- pod
  [name & {:keys [owner node namespace] :or {namespace "shop"}}]
  {:metadata (cond-> {:namespace namespace :name name}
               owner (assoc :ownerReferences [{:kind (first owner) :name (second owner) :controller true}])
               (= "ReplicaSet" (first owner)) (assoc :labels {:pod-template-hash (last (str/split (second owner) #"-"))}))
   :spec (cond-> {} node (assoc :nodeName node))})

(def ^:private evidence
  {"pods" {:items [(pod "web-7d9-a" :owner ["ReplicaSet" "web-7d9"] :node "n1")
                   (pod "web-7d9-b" :owner ["ReplicaSet" "web-7d9"] :node "n2")
                   (pod "db-0" :owner ["StatefulSet" "db"] :node "n1")
                   (pod "static" :node "n1")
                   (pod "web-7d9-a" :owner ["ReplicaSet" "web-7d9"] :node "n3" :namespace "other")]}})

(defn- finding [detective component severity summary]
  {:detective detective :component component :severity severity :summary summary})

(defn- parents [findings] (mapv :parent (correlation/correlate findings evidence)))

(deftest a-pod-finding-folds-under-its-workload
  (is (= [nil "Deployment shop/web" "Deployment shop/web"]
         (parents [(finding "deployments" "shop/web" :critical "Deployment has no available replicas")
                   (finding "pods" "shop/web-7d9-a" :critical "container app keeps restarting")
                   (finding "pod-startup" "shop/web-7d9-b" :warning "pod is stuck starting")])))
  (testing "a StatefulSet owns its pods directly"
    (is (= [nil "StatefulSet shop/db"]
           (parents [(finding "statefulsets" "shop/db" :warning "StatefulSet is not ready")
                     (finding "pods" "shop/db-0" :warning "container db keeps restarting")])))))

(deftest a-pod-finding-folds-under-its-node-when-its-workload-is-quiet
  (is (= [nil "Node n1" "Node n1"]
         (parents [(finding "nodes" "n1" :critical "node is NotReady")
                   (finding "pods" "shop/db-0" :warning "container db keeps restarting")
                   (finding "pods" "shop/static" :critical "container is not ready")])))
  (testing "the workload wins over the node"
    (is (= "Deployment shop/web"
           (last (parents [(finding "nodes" "n1" :critical "node is NotReady")
                           (finding "deployments" "shop/web" :critical "no available replicas")
                           (finding "pods" "shop/web-7d9-a" :critical "crash")]))))))

(deftest a-worse-child-is-never-folded
  (is (= [nil nil]
         (parents [(finding "deployments" "shop/web" :info "Deployment is scaled to 0")
                   (finding "pods" "shop/web-7d9-a" :warning "crash")])))
  (testing "the worst finding of the parent counts"
    (is (= [nil nil "Deployment shop/web"]
           (parents [(finding "deployments" "shop/web" :info "scaled")
                     (finding "rollouts" "shop/web" :critical "Deployment exceeded its progress deadline")
                     (finding "pods" "shop/web-7d9-a" :critical "crash")])))))

(deftest nothing-folds-without-a-parent-finding-or-a-pod
  (is (= [nil] (parents [(finding "pods" "shop/web-7d9-a" :critical "crash")])))
  (is (= [nil nil] (parents [(finding "deployments" "shop/web" :critical "down")
                             (finding "pods" "shop/gone" :critical "crash")]))
      "a pod the evidence does not hold")
  (is (= [nil nil] (parents [(finding "deployments" "shop/web" :critical "down")
                             (finding "services" "shop/web" :critical "Service has no ready backends")]))
      "only pod findings fold")
  (is (= [nil nil] (parents [(finding "deployments" "shop/web" :critical "down")
                             (finding "pods" "other/web-7d9-a" :critical "crash")]))
      "a workload of the same name in another namespace")
  (is (= [] (correlation/correlate [] {})))
  (is (= [nil] (mapv :parent (correlation/correlate [(finding "pods" "shop/web-7d9-a" :critical "crash")] {})))))

(deftest an-explicit-subject-is-honoured
  (is (= [nil "Deployment shop/web"]
         (parents [{:detective "custom" :component "the web tier" :severity :critical :summary "down"
                    :subject (subject/subject "Deployment" "shop" "web")}
                   (finding "pods" "shop/web-7d9-a" :warning "crash")]))))

(deftest run-detectives-correlates
  (let [detectives [{:name "deployments" :detect (fn [_] [{:severity :critical :component "shop/web" :summary "down"}])}
                    {:name "pods" :detect (fn [_] [{:severity :critical :component "shop/web-7d9-a" :summary "crash"}])}]]
    (is (= [nil "Deployment shop/web"] (mapv :parent (detective/run-detectives detectives evidence))))))

(deftest the-folded-line-counts-each-summary
  (let [findings (correlation/correlate [(finding "deployments" "shop/web" :critical "down")
                                         (finding "pods" "shop/web-7d9-a" :critical "crash")
                                         (finding "pods" "shop/web-7d9-b" :critical "crash")
                                         (finding "pod-startup" "shop/web-7d9-b" :warning "stuck")]
                                        evidence)]
    (is (= "follows from it: 3 findings - crash ×2, stuck"
           (detective/folded-line [(subject/subject "Deployment" "shop" "web")] (correlation/children findings))))
    (is (nil? (detective/folded-line [(subject/subject "Deployment" "shop" "api")] (correlation/children findings))))))

(deftest the-text-report-shows-children-under-their-parent
  (let [findings (correlation/correlate [(finding "deployments" "shop/web" :critical "Deployment has no available replicas")
                                         (finding "pods" "shop/web-7d9-a" :critical "container app keeps restarting")
                                         (finding "storage" "shop/data" :info "PersistentVolumeClaim is not used by any pod")]
                                        evidence)
        out (with-out-str
              (binding [*err* (java.io.StringWriter.)]
                (detective/report! [{:name "deployments"} {:name "pods"} {:name "storage"}] findings "text")))]
    (is (str/includes? out "follows from it: 1 finding - container app keeps restarting"))
    (is (not (str/includes? out "pods (")) "the pods area is gone, its only finding is folded")
    (is (str/includes? out "2 critical, 0 warning, 1 info (1 shown under the finding they follow from)"))
    (is (not (str/includes? out "came back clean: pods")) "a folded detective is not clean")))

(deftest folding-never-changes-health-or-machine-output
  (let [findings (correlation/correlate [(finding "deployments" "shop/web" :critical "down")
                                         (finding "pods" "shop/web-7d9-a" :critical "crash")]
                                        evidence)
        out (with-out-str
              (binding [*err* (java.io.StringWriter.)]
                (detective/report! [{:name "deployments"} {:name "pods"}] findings "json")))
        parsed (cheshire.core/parse-string out true)]
    (is (false? (detective/healthy? findings)))
    (is (= 2 (count (:findings parsed))))
    (is (= "Deployment shop/web" (:parent (second (:findings parsed)))))
    (is (= {:critical 2 :warning 0 :info 0} (:summary parsed)))))
