;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.healthy-cluster-test
  "A small cluster where everything is in order, judged by the whole registry:
   a detective that reports anything here raises a false alarm. Each case after
   it breaks one thing and expects exactly the detective that owns it to speak."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(defn- meta-of
  ([name] {:namespace "shop" :name name :uid (str "uid-" name) :creationTimestamp "2026-11-01T00:00:00Z"})
  ([name labels] (assoc (meta-of name) :labels labels)))

(def ^:private template
  {:metadata {:labels {:app "web"}}
   :spec {:serviceAccountName "web"
          :imagePullSecrets [{:name "registry"}]
          :containers [{:name "app" :image "registry.example/web:1.4"
                        :ports [{:name "http" :containerPort 8080}]
                        :resources {:requests {:cpu "100m" :memory "128Mi"} :limits {:memory "256Mi"}}
                        :env [{:name "REDIS_HOST" :value "redis.cache"}
                              {:name "REDIS_URL" :value "redis://redis.cache.svc.cluster.local:6379"}]}]}})

(defn- web-pod
  [suffix node]
  {:metadata (assoc (meta-of (str "web-7d9-" suffix) {:app "web" :pod-template-hash "7d9"})
                    :ownerReferences [{:kind "ReplicaSet" :name "web-7d9" :controller true}])
   :spec (assoc (:spec template) :nodeName node
                :volumes [{:name "data" :persistentVolumeClaim {:claimName "data"}}])
   :status {:phase "Running" :podIP "10.0.0.10"
            :conditions [{:type "PodScheduled" :status "True" :lastTransitionTime "2026-11-17T00:00:00Z"}
                         {:type "Ready" :status "True"}]
            :containerStatuses [{:name "app" :restartCount 0 :ready true
                                 :state {:running {:startedAt "2026-11-17T00:00:10Z"}}}]}})

(def ^:private healthy
  {:now now
   "namespaces" {:items [{:metadata {:name "shop"} :status {:phase "Active"}}
                         {:metadata {:name "cache"} :status {:phase "Active"}}]}
   "nodes" {:items [{:metadata {:name "worker-1"}
                     :status {:conditions [{:type "Ready" :status "True"}]
                              :allocatable {:cpu "4" :memory "16Gi" :pods "110"}}}
                    {:metadata {:name "worker-2"}
                     :status {:conditions [{:type "Ready" :status "True"}]
                              :allocatable {:cpu "4" :memory "16Gi" :pods "110"}}}]}
   "deployments" {:items [{:metadata (meta-of "web")
                           :spec {:replicas 2 :template template}
                           :status {:replicas 2 :readyReplicas 2 :availableReplicas 2
                                    :conditions [{:type "Progressing" :status "True" :reason "NewReplicaSetAvailable"}
                                                 {:type "Available" :status "True"}]}}]}
   "replicasets" {:items [{:metadata (meta-of "web-7d9") :spec {:replicas 2}
                           :status {:replicas 2 :readyReplicas 2}}]}
   "statefulsets" {:items []}
   "daemonsets" {:items [{:metadata (meta-of "agent")
                          :spec {:template {:spec {:containers [{:name "agent" :image "agent:1"}]}}}
                          :status {:desiredNumberScheduled 2 :currentNumberScheduled 2 :numberReady 2
                                   :numberUnavailable 0 :numberMisscheduled 0}}]}
   "jobs" {:items [{:metadata (meta-of "backup-29001")
                    :status {:conditions [{:type "Complete" :status "True"}]}}]}
   "cronjobs" {:items [{:metadata (meta-of "backup")
                        :spec {:schedule "0 3 * * *" :jobTemplate {:spec {:template template}}}
                        :status {:lastScheduleTime "2026-11-18T03:00:00Z"}}]}
   "pods" {:items [(web-pod "a" "worker-1") (web-pod "b" "worker-2")
                   {:metadata {:namespace "cache" :name "redis-0" :labels {:app "redis"}}
                    :spec {:nodeName "worker-1" :containers [{:name "redis" :image "redis:7"}]}
                    :status {:phase "Running" :conditions [{:type "Ready" :status "True"}]
                             :containerStatuses [{:name "redis" :restartCount 0 :state {:running {}}}]}}]}
   "services" {:items [{:metadata (meta-of "web")
                        :spec {:selector {:app "web"} :ports [{:port 80 :targetPort "http"}]}}
                       {:metadata (assoc (meta-of "redis") :namespace "cache")
                        :spec {:selector {:app "redis"} :ports [{:port 6379}] :clusterIP "None"}}
                       {:metadata (meta-of "edge")
                        :spec {:type "LoadBalancer" :selector {:app "web"} :ports [{:port 443 :targetPort 8080}]}
                        :status {:loadBalancer {:ingress [{:ip "192.0.2.10"}]}}}]}
   "ingresses" {:items [{:metadata (meta-of "web")
                         :spec {:ingressClassName "nginx"
                                :tls [{:hosts ["shop.example"] :secretName "web-tls"}]
                                :rules [{:host "shop.example"
                                         :http {:paths [{:path "/" :pathType "Prefix"
                                                         :backend {:service {:name "web" :port {:number 80}}}}]}}]}}]}
   "ingressclasses" {:items [{:metadata {:name "nginx"}}]}
   "secret-names" ["shop/web-tls" "shop/registry"]
   "serviceaccounts" {:items [{:metadata (meta-of "default")} {:metadata (meta-of "web")
                                                                :automountServiceAccountToken false}]}
   "persistentvolumeclaims" {:items [{:metadata (meta-of "data")
                                      :spec {:storageClassName "ceph"}
                                      :status {:phase "Bound"}}]}
   "storageclasses" {:items [{:metadata {:name "ceph"}}]}
   "configmaps" {:items [{:metadata (meta-of "web")}]}
   "persistentvolumes" {:items [{:metadata {:name "pv-data"} :status {:phase "Bound"}}]}
   "roles" {:items [{:metadata (meta-of "reader")}]}
   "clusterroles" {:items [{:metadata {:name "view"} :rules []}]}
   "rolebindings" {:items [{:metadata (meta-of "readers") :roleRef {:kind "Role" :name "reader"} :subjects []}]}
   "clusterrolebindings" {:items []}
   "validatingwebhookconfigurations" {:items [{:metadata {:name "policy"}
                                               :webhooks [{:name "v.policy.example" :failurePolicy "Fail"
                                                           :clientConfig {:service {:namespace "shop" :name "web"}}}]}]}
   "mutatingwebhookconfigurations" {:items []}
   "horizontalpodautoscalers" {:items [{:metadata (meta-of "web")
                                        :spec {:minReplicas 2 :maxReplicas 6}
                                        :status {:currentReplicas 2
                                                 :conditions [{:type "AbleToScale" :status "True"}
                                                              {:type "ScalingActive" :status "True"}]}}]}
   "poddisruptionbudgets" {:items [{:metadata (meta-of "web")
                                    :status {:disruptionsAllowed 1 :expectedPods 2}}]}
   "events" {:items []}
   "resourcequotas" {:items []}
   "networkpolicies" {:items [{:metadata (meta-of "default-deny")}]}})

(def ^:private judged
  "The detectives this fixture holds every input for."
  #{"services" "load-balancers" "pod-creation" "pod-startup" "pods" "stuck-pods" "stuck-terminating"
    "ingress-references" "template-references" "storageclass-references" "role-references" "webhook-references"
    "deployments" "statefulsets" "rollouts" "daemonsets" "jobs" "cronjobs" "hpas" "hpa-failures" "pdbs"
    "storage" "nodes" "events"})

(defn- findings
  [evidence]
  (detective/run-detectives (filter #(judged (:name %)) (registry/all)) evidence))

(defn- by-detective
  [evidence]
  (set (map :detective (findings evidence))))

(deftest a-healthy-cluster-raises-no-finding
  (is (= #{"services" "load-balancers" "pod-creation" "pod-startup" "pods" "stuck-pods" "stuck-terminating"
           "ingress-references" "template-references" "storageclass-references" "role-references"
           "webhook-references" "deployments" "statefulsets" "rollouts" "daemonsets" "jobs" "cronjobs"
           "hpas" "hpa-failures" "pdbs" "storage" "nodes" "events"}
         (set (keep (comp judged :name) (registry/all))))
      "every judged detective is registered")
  (is (= [] (findings healthy))))

(deftest each-fault-is-reported-by-its-own-detective
  (testing "a Service selector typo"
    (is (= #{"services"} (by-detective (assoc-in healthy ["services" :items 0 :spec :selector] {:app "wbe"})))))
  (testing "a missing TLS Secret"
    (is (= #{"ingress-references"} (by-detective (assoc healthy "secret-names" ["shop/registry"])))))
  (testing "a deleted ServiceAccount"
    (is (= #{"template-references"}
           (by-detective (assoc-in healthy ["serviceaccounts" :items] [{:metadata (meta-of "default")}])))))
  (testing "a rollout past its deadline"
    (is (= #{"rollouts"}
           (by-detective (assoc-in healthy ["deployments" :items 0 :status :conditions 0]
                                   {:type "Progressing" :status "False" :reason "ProgressDeadlineExceeded"})))))
  (testing "a CronJob that stopped two days ago"
    (is (= #{"cronjobs"}
           (by-detective (assoc-in healthy ["cronjobs" :items 0 :status :lastScheduleTime] "2026-11-16T03:00:00Z")))))
  (testing "an HPA without metrics"
    (is (= #{"hpa-failures"}
           (by-detective (assoc-in healthy ["horizontalpodautoscalers" :items 0 :status :conditions 1]
                                   {:type "ScalingActive" :status "False" :reason "FailedGetResourceMetric"})))))
  (testing "a webhook whose Service was deleted"
    (is (= #{"webhook-references"}
           (by-detective (assoc-in healthy ["validatingwebhookconfigurations" :items 0 :webhooks 0 :clientConfig :service :name]
                                   "gone")))))
  (testing "a ConfigMap stuck on a finalizer"
    (is (= #{"stuck-terminating"}
           (by-detective (update-in healthy ["configmaps" :items 0 :metadata] assoc
                                    :deletionTimestamp "2026-11-18T11:00:00Z" :finalizers ["example.com/sync"])))))
  (testing "a quota refusing the ReplicaSet's pods, whose Warning event the events detective lists too"
    (is (= #{"pod-creation" "events"}
           (by-detective (assoc healthy "events"
                                {:items [{:type "Warning" :reason "FailedCreate" :lastTimestamp "2026-11-18T11:55:00Z"
                                          :message "pods \"web-7d9-c\" is forbidden: exceeded quota: compute"
                                          :involvedObject {:kind "ReplicaSet" :namespace "shop" :name "web-7d9"}}]})))))
  (testing "a restart loop in one of two replicas"
    (is (contains? (by-detective (assoc-in healthy ["pods" :items 0 :status :containerStatuses 0]
                                           {:name "app" :restartCount 5 :ready false
                                            :lastState {:terminated {:exitCode 1 :reason "Error"
                                                                     :startedAt "2026-11-18T11:55:00Z"
                                                                     :finishedAt "2026-11-18T11:57:00Z"}}
                                            :state {:waiting {:reason "CrashLoopBackOff"}}}))
                   "pods"))))
