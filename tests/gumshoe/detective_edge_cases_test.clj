;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detective-edge-cases-test
  "Inputs at the edges of what the service, scheduling, pod-creation,
   pod-startup, restart-loop, terminating, reference and workload-condition
   detectives read: odd but legal objects, older message formats, and fields a
   controller leaves out."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cron :as cron]
            [gumshoe.detectives.disruption :as disruption]
            [gumshoe.detectives.pod-creation :as pod-creation]
            [gumshoe.detectives.pod-startup :as pod-startup]
            [gumshoe.detectives.pods :as pods]
            [gumshoe.detectives.references :as references]
            [gumshoe.detectives.services :as services]
            [gumshoe.detectives.terminating :as terminating]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.scheduling :as scheduling]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

;; --- services -----------------------------------------------------------------

(deftest image-references-compare-without-tag-or-digest
  (doseq [[image repository] [["nginx" "nginx"]
                              ["nginx:1.27" "nginx"]
                              ["docker.io/library/nginx:1.27" "docker.io/library/nginx"]
                              ["registry.example:5000/team/web" "registry.example:5000/team/web"]
                              ["registry.example:5000/team/web:2.0" "registry.example:5000/team/web"]
                              ["ghcr.io/org/app@sha256:0123abcd" "ghcr.io/org/app"]
                              ["ghcr.io/org/app:1.0@sha256:0123abcd" "ghcr.io/org/app"]]]
    (is (= repository (#'services/image-repository image)) image)))

(deftest a-selector-is-a-subset-match
  (is (services/selects? {:app "web"} {:app "web" :tier "front" :pod-template-hash "7d9"}))
  (is (not (services/selects? {:app "web" :tier "back"} {:app "web" :tier "front"})))
  (is (not (services/selects? {} {:app "web"})) "an empty selector selects nothing")
  (is (not (services/selects? nil {:app "web"}))))

(defn- svc-run
  [services pods & {:keys [statefulsets]}]
  (services/detect-broken-backends {"services" {:items services} "pods" {:items pods}
                                    "deployments" {:items []} "statefulsets" {:items (or statefulsets [])}}))

(deftest numeric-target-ports-are-not-checked-against-names
  (is (empty? (svc-run [{:metadata {:namespace "a" :name "s"}
                         :spec {:selector {:app "x"} :ports [{:port 80 :targetPort 8080} {:port 81}]}}]
                       [{:metadata {:namespace "a" :name "p" :labels {:app "x"}}
                         :status {:phase "Running" :conditions [{:type "Ready" :status "True"}]}}]))))

(deftest a-statefulset-at-zero-explains-its-headless-service
  (is (= ["Service has no pods: StatefulSet db is scaled to 0"]
         (map :summary (svc-run [{:metadata {:namespace "a" :name "db"} :spec {:selector {:app "db"} :clusterIP "None"}}]
                                []
                                :statefulsets [{:metadata {:namespace "a" :name "db"}
                                                :spec {:replicas 0 :template {:metadata {:labels {:app "db"}}}}}])))))

(deftest a-load-balancer-without-a-creation-time-is-not-judged
  (is (empty? (services/detect-pending-load-balancers
               {:now now "services" {:items [{:metadata {:namespace "a" :name "lb"} :spec {:type "LoadBalancer"}}]}}))))

;; --- scheduling ---------------------------------------------------------------

(deftest older-and-unusual-scheduler-messages
  (testing "the pre-1.24 taint wording"
    (is (= [:taint] (map :cause (scheduling/causes "0/3 nodes are available: 3 node(s) had taints that the pod didn't tolerate.")))))
  (testing "a message without the preemption report and with extra spaces"
    (is (= [[2 :insufficient-resource] [1 :cordoned]]
           (map (juxt :nodes :cause)
                (scheduling/causes "0/3 nodes are available:  1 node(s) were unschedulable,  2 Insufficient cpu.")))))
  (testing "an extended resource keeps its full name"
    (is (= "lack free nvidia.com/gpu"
           (:explain (first (scheduling/causes "0/2 nodes are available: 2 Insufficient nvidia.com/gpu.")))))))

(deftest a-cause-with-no-action-keeps-the-scheduler-wording
  (let [line (scheduling/describe (scheduling/causes "0/1 nodes are available: 1 node(s) had a brand new problem."))]
    (is (str/starts-with? line "1 node(s) "))
    (is (= "1 node(s) had a brand new problem" line))
    (is (not (str/includes? line " - ")) "no action is invented")))

;; --- pod creation -------------------------------------------------------------

(deftest an-empty-refusal-message-is-unknown
  (is (= :other (first (pod-creation/classify nil))))
  (is (= :other (first (pod-creation/classify "")))))

(deftest a-webhook-reached-by-url-names-the-url
  (let [[finding] (pod-creation/detect-pod-creation-failures
                   {:now now
                    "events" {:items [{:reason "FailedCreate" :lastTimestamp "2026-11-18T11:59:00Z"
                                       :message "Internal error occurred: failed calling webhook \"ext.example\": connection refused"
                                       :involvedObject {:kind "DaemonSet" :namespace "agents" :name "node-agent"}}]}
                    "validatingwebhookconfigurations"
                    {:items [{:metadata {:name "external"}
                              :webhooks [{:name "ext.example" :failurePolicy "Ignore" :timeoutSeconds 3
                                          :clientConfig {:url "https://policy.example/validate"}}]}]}})]
    (is (= "DaemonSet cannot create pods: an admission webhook cannot be reached" (:summary finding)))
    (is (str/includes? (:hint finding) "failurePolicy Ignore, timeout 3s, URL https://policy.example/validate"))))

(deftest an-event-carrying-only-event-time-counts
  (is (= 1 (count (pod-creation/detect-pod-creation-failures
                   {:now now
                    "events" {:items [{:reason "FailedCreate" :eventTime "2026-11-18T11:59:00.000000Z"
                                       :message "exceeded quota: compute"
                                       :involvedObject {:kind "Job" :namespace "batch" :name "import"}}]}})))))

;; --- pod startup --------------------------------------------------------------

(deftest a-pod-without-a-scheduled-condition-is-timed-from-its-creation
  (is (= [:warning]
         (map :severity (pod-startup/detect-stalled-startups
                         {:now now
                          "events" {:items []}
                          "pods" {:items [{:metadata {:namespace "a" :name "p" :creationTimestamp "2026-11-18T11:40:00Z"}
                                           :spec {:nodeName "n1"} :status {:phase "Pending"}}]}})))))

(deftest a-terminating-pending-pod-is-left-to-the-stuck-pods-detective
  (is (empty? (pod-startup/detect-stalled-startups
               {:now now "events" {:items []}
                "pods" {:items [{:metadata {:namespace "a" :name "p" :creationTimestamp "2026-11-18T10:00:00Z"
                                            :deletionTimestamp "2026-11-18T11:00:00Z"}
                                 :spec {:nodeName "n1"} :status {:phase "Pending"}}]}}))))

;; --- restart loops ------------------------------------------------------------

(defn- looping-pod
  [namespace name & {:keys [reason ready?] :or {reason "Error"}}]
  {:metadata {:namespace namespace :name name}
   :status {:phase "Running"
            :containerStatuses [{:name "app" :restartCount 6 :ready (boolean ready?)
                                 :lastState {:terminated {:exitCode 137 :reason reason
                                                          :startedAt "2026-11-18T11:50:00Z"
                                                          :finishedAt "2026-11-18T11:52:00Z"}}
                                 :state {:running {:startedAt "2026-11-18T11:58:00Z"}}}]}})

(deftest an-oom-loop-is-a-loop-and-an-oom-kill
  (is (= #{"container app keeps restarting (6 restarts)" "container app was OOMKilled"}
         (set (map :summary (pods/detect-unhealthy-pods {:now now "pods" {:items [(looping-pod "a" "p" :reason "OOMKilled")]}}))))))

(deftest equal-workload-names-in-two-namespaces-are-two-workloads
  (let [owned (fn [pod] (assoc-in pod [:metadata :ownerReferences] [{:kind "StatefulSet" :name "db" :controller true}]))
        healthy (fn [namespace name] (owned {:metadata {:namespace namespace :name name}
                                             :status {:phase "Running" :containerStatuses [{:name "app" :restartCount 0
                                                                                            :state {:running {}}}]}}))
        findings (pods/detect-unhealthy-pods
                  {:now now "pods" {:items [(owned (looping-pod "a" "db-0")) (healthy "a" "db-1") (healthy "a" "db-2")
                                            (owned (looping-pod "b" "db-0"))]}})]
    (is (= {"a/db-0" :warning "b/db-0" :critical}
           (into {} (map (juxt :component :severity)) findings)))))

(deftest finished-pods-do-not-dilute-the-loop-share
  (let [owned (fn [pod] (assoc-in pod [:metadata :ownerReferences] [{:kind "Job" :name "import" :controller true}]))]
    (is (= [:critical]
           (map :severity (pods/detect-unhealthy-pods
                           {:now now "pods" {:items [(owned (looping-pod "a" "import-x"))
                                                     (owned {:metadata {:namespace "a" :name "import-old"}
                                                             :status {:phase "Succeeded"}})]}}))))))

(deftest the-workload-of-a-pod
  (is (= "Deployment/web" (kubectl/workload-of {:metadata {:name "web-7d9-x" :labels {:pod-template-hash "7d9"}
                                                           :ownerReferences [{:kind "ReplicaSet" :name "web-7d9" :controller true}]}})))
  (is (= "ReplicaSet/web-7d9" (kubectl/workload-of {:metadata {:name "web-7d9-x"
                                                               :ownerReferences [{:kind "ReplicaSet" :name "web-7d9" :controller true}]}}))
      "a bare ReplicaSet without the hash label stays itself")
  (is (= "Pod/static" (kubectl/workload-of {:metadata {:name "static"}})))
  (is (= "Pod/x" (kubectl/workload-of {:metadata {:name "x" :ownerReferences [{:kind "Node" :name "n1"}]}}))
      "only a controller reference names a workload"))

;; --- terminating --------------------------------------------------------------

(deftest a-deleted-object-without-finalizers-is-quiet
  (is (empty? (terminating/detect-stuck-terminating
               {:now now "services" {:items [{:metadata {:namespace "a" :name "s" :deletionTimestamp "2026-11-18T10:00:00Z"}}]}}))))

(deftest every-type-read-has-a-kind
  (is (every? (fn [[type kind]] (and (str/ends-with? type "s") (re-matches #"[A-Z][A-Za-z]+" kind))) terminating/types)))

;; --- references ---------------------------------------------------------------

(deftest an-ingress-backend-without-a-service-is-not-judged
  (is (empty? (references/detect-ingress-references
               {"ingresses" {:items [{:metadata {:namespace "a" :name "i"}
                                      :spec {:defaultBackend {:resource {:kind "StorageBucket" :name "static"}}
                                             :rules [{:host "a.example"}]}}]}
                "services" {:items []} "ingressclasses" {:items []} "secret-names" []}))))

(deftest a-template-without-a-service-account-uses-the-default
  (is (empty? (references/detect-template-references
               {"deployments" {:items [{:metadata {:namespace "a" :name "d"} :spec {:template {:spec {:containers []}}}}]}
                "serviceaccounts" {:items []} "services" {:items []} "secret-names" [] "namespaces" {:items []}}))))

(deftest references-without-their-evidence-stay-silent
  (is (empty? (references/detect-storage-class-references
               {"persistentvolumeclaims" {:items [{:metadata {:namespace "a" :name "c"} :spec {:storageClassName "x"}}]}})))
  (is (empty? (references/detect-role-references
               {"rolebindings" {:items [{:metadata {:namespace "a" :name "b"} :roleRef {:kind "Role" :name "x"}}]}})))
  (is (empty? (references/detect-webhook-references
               {"validatingwebhookconfigurations" {:items [{:metadata {:name "w"}
                                                            :webhooks [{:name "x" :clientConfig {:service {:namespace "a" :name "s"}}}]}]}}))))

;; --- workload conditions ------------------------------------------------------

(deftest unreadable-schedules
  (doseq [schedule ["@every 5m" "0 0 * * * *" "" nil "@reboot"]]
    (is (nil? (cron/interval schedule)) (pr-str schedule))))

(deftest an-hpa-without-conditions-is-quiet
  (is (empty? (disruption/detect-failing-hpas
               {"horizontalpodautoscalers" {:items [{:metadata {:namespace "a" :name "h"} :status {}}]}}))))

(deftest an-unknown-eviction-message-still-reads-as-an-eviction
  (is (= "kubelet eviction" (pods/eviction-cause "Pod was rejected: something new")))
  (is (= "kubelet eviction" (pods/eviction-cause nil))))
