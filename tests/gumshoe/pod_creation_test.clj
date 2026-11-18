;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pod-creation-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.pod-creation :as pod-creation]
            [gumshoe.subject :as subject]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(defn- event
  [kind name message & {:keys [at] :or {at "2026-11-18T11:50:00Z"}}]
  {:type "Warning" :reason "FailedCreate" :lastTimestamp at :message message
   :involvedObject {:kind kind :namespace "shop" :name name}})

(defn- run
  [& {:keys [events replicasets validating mutating services pods]}]
  (pod-creation/detect-pod-creation-failures
   {:now now
    "events" {:items (or events [])}
    "replicasets" {:items (or replicasets [])}
    "validatingwebhookconfigurations" {:items (or validating [])}
    "mutatingwebhookconfigurations" {:items (or mutating [])}
    "services" {:items (or services [])}
    "pods" {:items (or pods [])}}))

(deftest every-refusal-is-classified
  (doseq [[message cause]
          [["Internal error occurred: failed calling webhook \"v.kyverno.svc\": failed to call webhook: Post \"https://kyverno-svc.kyverno.svc:443/validate?timeout=10s\": context deadline exceeded" :webhook-timeout]
           ["Internal error occurred: failed calling webhook \"v.kyverno.svc\": failed to call webhook: Post \"https://kyverno-svc.kyverno.svc:443/validate\": dial tcp 10.96.1.5:443: connect: connection refused" :webhook-unavailable]
           ["admission webhook \"validation.gatekeeper.sh\" denied the request: [deny-privileged] privileged containers are not allowed" :webhook-denied]
           ["pods \"web-7d9-x\" is forbidden: exceeded quota: compute, requested: limits.cpu=2, used: limits.cpu=8, limited: limits.cpu=8" :quota]
           ["pods \"web-7d9-x\" is forbidden: violates PodSecurity \"restricted:latest\": allowPrivilegeEscalation != false" :pod-security]
           ["pods \"web-7d9-x\" is forbidden: error looking up service account shop/web: serviceaccount \"web\" not found" :service-account]
           ["pods \"web-7d9-x\" is forbidden: maximum memory usage per Container is 1Gi, but limit is 2Gi" :limit-range]
           ["pods \"web-7d9-x\" is forbidden: User \"system:serviceaccount:kube-system:replicaset-controller\" cannot create resource \"pods\" in API group \"\" in the namespace \"shop\"" :rbac]
           ["something nobody has seen yet" :other]]]
    (is (= cause (first (pod-creation/classify (str "Error creating: " message)))) message)))

(deftest a-down-webhook-names-its-configuration-and-backend
  (let [message "Error creating: Internal error occurred: failed calling webhook \"v.kyverno.svc\": failed to call webhook: connect: connection refused"
        [finding :as findings]
        (run :events [(event "ReplicaSet" "web-7d9" message)]
             :validating [{:metadata {:name "kyverno-resource"}
                           :webhooks [{:name "v.kyverno.svc" :failurePolicy "Fail" :timeoutSeconds 10
                                       :clientConfig {:service {:namespace "kyverno" :name "kyverno-svc"}}}]}]
             :services [{:metadata {:namespace "kyverno" :name "kyverno-svc"}
                         :spec {:selector {:app "kyverno"}}}]
             :pods [{:metadata {:namespace "kyverno" :name "kyverno-0" :labels {:app "kyverno"}}
                     :status {:conditions [{:type "Ready" :status "False"}]}}])]
    (is (= 1 (count findings)))
    (is (= :critical (:severity finding)))
    (is (= "shop/web-7d9" (:component finding)))
    (is (= "ReplicaSet cannot create pods: an admission webhook cannot be reached" (:summary finding)))
    (is (str/starts-with? (:hint finding)
                          "webhook v.kyverno.svc of ValidatingWebhookConfiguration kyverno-resource, failurePolicy Fail, timeout 10s, its Service kyverno/kyverno-svc has 0 ready pods"))
    (is (= (subject/subject "ReplicaSet" "shop" "web-7d9") (:subject finding)))))

(deftest a-webhook-whose-service-is-gone-says-so
  (let [[finding] (run :events [(event "StatefulSet" "db" "Error creating: failed calling webhook \"m.example\": no endpoints available")]
                       :mutating [{:metadata {:name "inject"}
                                   :webhooks [{:name "m.example"
                                               :clientConfig {:service {:namespace "mesh" :name "injector"}}}]}])]
    (is (str/includes? (:hint finding) "MutatingWebhookConfiguration inject, failurePolicy Fail, timeout 10s, its Service mesh/injector does not exist"))))

(deftest a-replica-failure-condition-outlives-its-events
  (let [[finding :as findings]
        (run :replicasets [{:metadata {:namespace "shop" :name "web-7d9"}
                            :status {:conditions [{:type "ReplicaFailure" :status "True" :reason "FailedCreate"
                                                   :lastTransitionTime "2026-11-18T08:00:00Z"
                                                   :message "pods \"web-7d9-x\" is forbidden: exceeded quota: compute"}]}}])]
    (is (= 1 (count findings)))
    (is (= "ReplicaSet cannot create pods: a ResourceQuota is exhausted" (:summary finding))))
  (testing "a newer event on the same ReplicaSet wins and is reported once"
    (let [findings (run :replicasets [{:metadata {:namespace "shop" :name "web-7d9"}
                                       :status {:conditions [{:type "ReplicaFailure" :status "True"
                                                              :lastTransitionTime "2026-11-18T08:00:00Z"
                                                              :message "exceeded quota: compute"}]}}]
                        :events [(event "ReplicaSet" "web-7d9" "violates PodSecurity \"baseline:latest\"")])]
      (is (= ["ReplicaSet cannot create pods: the pod violates the namespace's PodSecurity level"]
             (map :summary findings))))))

(deftest old-and-unrelated-events-are-quiet
  (is (empty? (run :events [(event "ReplicaSet" "web-7d9" "exceeded quota" :at "2026-11-18T09:00:00Z")
                            (assoc (event "Pod" "web-1" "Back-off restarting") :reason "BackOff")]))))

(deftest an-unknown-refusal-is-a-warning-with-the-message
  (let [[finding] (run :events [(event "Job" "migrate" "Error creating: the moon is in the wrong phase")])]
    (is (= :warning (:severity finding)))
    (is (= "Job cannot create pods: the API server refuses the pod" (:summary finding)))
    (is (str/includes? (:hint finding) "the moon is in the wrong phase"))))
