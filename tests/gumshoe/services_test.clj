;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.services-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.services :as services]
            [gumshoe.subject :as subject]))

(defn- service
  ([name selector] (service name selector {}))
  ([name selector spec]
   {:metadata {:namespace "shop" :name name}
    :spec (merge {:selector selector :ports [{:port 80 :targetPort 8080}]} spec)}))

(defn- pod
  [name labels & {:keys [phase ready? image owner hash ports deleting?]
                  :or {phase "Running" ready? true image "registry.example/web:1.0"}}]
  {:metadata (cond-> {:namespace "shop" :name name
                      :labels (cond-> labels hash (assoc :pod-template-hash hash))}
               owner (assoc :ownerReferences [{:kind (first owner) :name (second owner) :controller true}])
               deleting? (assoc :deletionTimestamp "2026-11-18T10:00:00Z"))
   :spec {:containers [{:name "app" :image image :ports (or ports [])}]}
   :status {:phase phase
            :conditions [{:type "Ready" :status (if ready? "True" "False")}]}})

(defn- run
  [& {:keys [services pods deployments statefulsets]
       :or {services [] pods [] deployments [] statefulsets []}}]
  (services/detect-broken-backends {"services" {:items services}
                                    "pods" {:items pods}
                                    "deployments" {:items deployments}
                                    "statefulsets" {:items statefulsets}}))

(deftest a-service-with-ready-backends-is-quiet
  (is (empty? (run :services [(service "web" {:app "web"})]
                   :pods [(pod "web-1" {:app "web" :tier "front"})]))))

(deftest a-selector-matching-no-pods-is-a-warning
  (let [[finding :as findings] (run :services [(service "web" {:app.kubernetes.io/name "web"})]
                                    :pods [(pod "api-1" {:app.kubernetes.io/name "api"})])]
    (is (= 1 (count findings)))
    (is (= :warning (:severity finding)))
    (is (= "shop/web" (:component finding)))
    (is (= "Service selector matches no pods" (:summary finding)))
    (testing "the hint names the selector with its label prefix"
      (is (re-find #"app\.kubernetes\.io/name=web" (:hint finding))))))

(deftest a-pod-in-another-namespace-does-not-back-a-service
  (is (= ["Service selector matches no pods"]
         (map :summary (run :services [(service "web" {:app "web"})]
                            :pods [(assoc-in (pod "web-1" {:app "web"}) [:metadata :namespace] "other")])))))

(deftest a-workload-scaled-to-zero-explains-the-missing-pods
  (let [[finding] (run :services [(service "web" {:app "web"})]
                       :deployments [{:metadata {:namespace "shop" :name "web"}
                                      :spec {:replicas 0 :template {:metadata {:labels {:app "web"}}}}}])]
    (is (= :info (:severity finding)))
    (is (= "Service has no pods: Deployment web is scaled to 0" (:summary finding)))))

(deftest a-running-workload-that-the-selector-misses-is-no-excuse
  (let [[finding] (run :services [(service "web" {:app "web"})]
                       :deployments [{:metadata {:namespace "shop" :name "web"}
                                      :spec {:replicas 2 :template {:metadata {:labels {:app "web"}}}}}])]
    (is (= :warning (:severity finding)))))

(deftest a-selector-matching-only-finished-pods-is-a-warning
  (let [[finding] (run :services [(service "web" {:app "web"})]
                       :pods [(pod "migrate-1" {:app "web"} :phase "Succeeded" :ready? false)])]
    (is (= :warning (:severity finding)))
    (is (= "Service selects only finished pods (1)" (:summary finding)))))

(deftest no-ready-backend-is-critical
  (let [[finding :as findings] (run :services [(service "web" {:app "web"})]
                                    :pods [(pod "web-1" {:app "web"} :ready? false)
                                           (pod "web-2" {:app "web"} :ready? false)])]
    (is (= 1 (count findings)))
    (is (= :critical (:severity finding)))
    (is (= "Service has 0 of 2 selected pods ready" (:summary finding))))
  (testing "one ready pod is enough"
    (is (empty? (run :services [(service "web" {:app "web"})]
                     :pods [(pod "web-1" {:app "web"} :ready? false)
                            (pod "web-2" {:app "web"})]))))
  (testing "a terminating pod does not count as a backend"
    (is (= ["Service selects only finished pods (1)"]
           (map :summary (run :services [(service "web" {:app "web"})]
                              :pods [(pod "web-1" {:app "web"} :deleting? true)])))))
  (testing "publishNotReadyAddresses publishes unready pods on purpose"
    (is (empty? (run :services [(service "db" {:app "db"} {:publishNotReadyAddresses true})]
                     :pods [(pod "db-0" {:app "db"} :ready? false)])))))

(deftest a-named-target-port-no-backend-declares-is-critical
  (let [web (service "web" {:app "web"} {:ports [{:port 80 :targetPort "http"}
                                                 {:port 9090 :targetPort "metrics"}]})
        findings (run :services [web]
                      :pods [(pod "web-1" {:app "web"} :ports [{:name "http" :containerPort 8080}])])]
    (is (= ["Service targetPort metrics is not declared by any selected pod"] (map :summary findings)))
    (is (= [:critical] (map :severity findings))))
  (testing "a sidecar init container declares ports too"
    (is (empty? (run :services [(service "web" {:app "web"} {:ports [{:port 80 :targetPort "proxy"}]})]
                     :pods [(assoc-in (pod "web-1" {:app "web"}) [:spec :initContainers]
                                      [{:name "proxy" :image "envoy" :ports [{:name "proxy" :containerPort 15001}]}])])))))

(deftest unrelated-workloads-behind-one-service-are-a-warning
  (let [[finding :as findings]
        (run :services [(service "web" {:tier "front"})]
             :pods [(pod "web-7d9-a" {:tier "front"} :owner ["ReplicaSet" "web-7d9"] :hash "7d9"
                         :image "registry.example/web:1.0")
                    (pod "admin-55c-a" {:tier "front"} :owner ["ReplicaSet" "admin-55c"] :hash "55c"
                         :image "registry.example/admin:3.2")])]
    (is (= 1 (count findings)))
    (is (= :warning (:severity finding)))
    (is (= "Service routes to unrelated workloads: Deployment/admin, Deployment/web" (:summary finding)))))

(deftest two-revisions-of-one-application-are-not-unrelated
  (testing "two ReplicaSets of one Deployment during a rollout"
    (is (empty? (run :services [(service "web" {:app "web"})]
                     :pods [(pod "web-7d9-a" {:app "web"} :owner ["ReplicaSet" "web-7d9"] :hash "7d9"
                                 :image "registry.example/web:1.0")
                            (pod "web-8f1-a" {:app "web"} :owner ["ReplicaSet" "web-8f1"] :hash "8f1"
                                 :image "registry.example/web:1.1@sha256:abc")]))))
  (testing "a stable and a canary Deployment of the same image"
    (is (empty? (run :services [(service "web" {:app "web"})]
                     :pods [(pod "web-a" {:app "web"} :owner ["ReplicaSet" "web-7d9"] :hash "7d9"
                                 :image "registry.example:5000/web:1.0")
                            (pod "web-canary-a" {:app "web"} :owner ["ReplicaSet" "web-canary-1aa"] :hash "1aa"
                                 :image "registry.example:5000/web:1.1")]))))
  (testing "a headless Service addresses pods one by one, so mixing is expected"
    (is (empty? (run :services [(service "peers" {:tier "front"} {:clusterIP "None"})]
                     :pods [(pod "web-a" {:tier "front"} :owner ["StatefulSet" "web"] :image "web")
                            (pod "admin-a" {:tier "front"} :owner ["StatefulSet" "admin"] :image "admin")])))))

(deftest services-without-a-selector-are-skipped
  (is (empty? (run :services [(service "external" {} {:type "ExternalName"})
                              (service "manual" nil)]))))

(deftest a-load-balancer-without-an-address-is-a-warning
  (let [now (java.time.Instant/parse "2026-11-18T12:00:00Z")
        lb (fn [name created ingress]
             {:metadata {:namespace "shop" :name name :creationTimestamp created}
              :spec {:type "LoadBalancer"}
              :status {:loadBalancer (if ingress {:ingress [{:ip "192.0.2.10"}]} {})}})
        findings (services/detect-pending-load-balancers
                  {:now now
                   "services" {:items [(lb "stuck" "2026-11-18T11:00:00Z" false)
                                       (lb "fresh" "2026-11-18T11:58:00Z" false)
                                       (lb "served" "2026-11-18T11:00:00Z" true)]}})]
    (is (= ["shop/stuck"] (map :component findings)))
    (is (= [:warning] (map :severity findings)))))

(deftest a-service-finding-drills-into-the-service
  (is (= "Service" (:kind (subject/from-finding {:detective "services" :component "shop/web"}))))
  (is (= "Service" (:kind (subject/from-finding {:detective "load-balancers" :component "shop/web"})))))
