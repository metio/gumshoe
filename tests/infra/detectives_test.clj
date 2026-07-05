;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives-test
  (:require [cheshire.core]
            [clojure.string]
            [clojure.test :refer [deftest is testing]]
            [infra.detective :as detective]
            [infra.subject :as subject]
            [infra.detectives.calico :as calico]
            [infra.detectives.certificates :as certificates]
            [infra.detectives.cnpg :as cnpg]
            [infra.detectives.flux :as flux]
            [infra.detectives.monitoring :as monitoring]
            [infra.detectives.nodes :as nodes]
            [infra.detectives.pods :as pods]
            [infra.detectives.registry :as registry]
            [infra.detectives.storage :as storage]
            [infra.detectives.workloads :as workloads]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest pods-detective-test
  (let [evidence {"pods" {:items [{:metadata {:namespace "moodle" :name "web-1"}
                                   :status {:containerStatuses
                                            [{:name "web"
                                              :restartCount 42
                                              :state {:waiting {:reason "CrashLoopBackOff"
                                                                :message "back-off restarting"}}}]}}
                                  {:metadata {:namespace "batch" :name "job-pod"}
                                   :status {:phase "Pending"
                                            :conditions [{:type "PodScheduled"
                                                          :status "False"
                                                          :message "0/3 nodes available"}]}}
                                  {:metadata {:namespace "fine" :name "healthy"}
                                   :status {:phase "Running"
                                            :containerStatuses [{:name "app"
                                                                 :restartCount 0
                                                                 :state {:running {}}}]}}]}}
        findings (pods/detect-unhealthy-pods evidence)]
    (testing "reports crash loops, restarts, and pending pods but not healthy ones"
      (is (= #{"container web is waiting: CrashLoopBackOff"
               "container web restarted 42 times"
               "pod is stuck in Pending"}
             (summaries findings))))
    (testing "crash loops are critical"
      (is (= :critical
             (:severity (first (filter #(= "container web is waiting: CrashLoopBackOff" (:summary %))
                                       findings))))))))

(deftest nodes-detective-test
  (let [evidence {"nodes" {:items [{:metadata {:name "worker-1"}
                                    :status {:conditions [{:type "Ready" :status "Unknown"}]}}
                                   {:metadata {:name "worker-2"}
                                    :spec {:unschedulable true}
                                    :status {:conditions [{:type "Ready" :status "True"}
                                                          {:type "DiskPressure" :status "True"}]}}]}}
        findings (nodes/detect-node-problems evidence)]
    (is (= #{"node is not Ready (status Unknown)"
             "DiskPressure is active"
             "node is cordoned (unschedulable)"}
           (summaries findings)))))

(deftest certificates-detective-test
  (let [now (java.time.Instant/parse "2026-07-04T00:00:00Z")
        evidence {:now now
                  certificates/certificate-type
                  {:items [{:metadata {:namespace "moodle" :name "tls"}
                            :status {:conditions [{:type "Ready" :status "False" :reason "Failed"}]}}
                           {:metadata {:namespace "keycloak" :name "tls"}
                            :status {:conditions [{:type "Ready" :status "True"}]
                                     :notAfter "2026-07-10T00:00:00Z"}}
                           {:metadata {:namespace "fine" :name "tls"}
                            :status {:conditions [{:type "Ready" :status "True"}]
                                     :notAfter "2026-12-31T00:00:00Z"}}]}}
        findings (certificates/detect-certificate-problems evidence)]
    (is (= #{"certificate is not Ready (Failed)"
             "certificate expires soon (2026-07-10T00:00:00Z)"}
           (summaries findings))))
  (testing "invalid and errored orders are reported"
    (is (= #{"ACME order is invalid"}
           (summaries (certificates/detect-invalid-orders
                       {certificates/order-type
                        {:items [{:metadata {:namespace "moodle" :name "order-1"}
                                  :status {:state "invalid"}}
                                 {:metadata {:namespace "fine" :name "order-2"}
                                  :status {:state "valid"}}]}}))))))

(deftest flux-detective-test
  (let [evidence {flux/helmrelease-type
                  {:items [{:metadata {:namespace "moodle" :name "moodle"}
                            :status {:conditions [{:type "Ready" :status "False" :reason "UpgradeFailed"}]}}
                           {:metadata {:namespace "keycloak" :name "keycloak"}
                            :spec {:suspend true}
                            :status {:conditions [{:type "Ready" :status "True"}]}}]}}
        findings (flux/detect-helmrelease-problems evidence)]
    (is (= #{"HelmRelease is not Ready (UpgradeFailed)"
             "HelmRelease is suspended"}
           (summaries findings)))))

(deftest cnpg-detective-test
  (let [evidence {cnpg/cluster-type
                  {:items [{:metadata {:namespace "moodle" :name "database"}
                            :spec {:instances 3}
                            :status {:phase "Failing over"
                                     :readyInstances 1
                                     :conditions [{:type "ContinuousArchiving" :status "False"
                                                   :message "WAL archive check failed"}]}}
                           {:metadata {:namespace "fine" :name "database"}
                            :spec {:instances 2}
                            :status {:phase "Cluster in healthy state"
                                     :readyInstances 2
                                     :conditions [{:type "ContinuousArchiving" :status "True"}]}}]}}
        findings (cnpg/detect-cnpg-problems evidence)]
    (is (= #{"cluster phase: Failing over"
             "only 1 of 3 instances are ready"
             "continuous WAL archiving is failing"}
           (summaries findings)))))

(deftest storage-detective-test
  (let [evidence {"persistentvolumeclaims" {:items [{:metadata {:namespace "moodle" :name "data"}
                                                     :status {:phase "Bound"}}
                                                    {:metadata {:namespace "moodle" :name "stuck"}
                                                     :status {:phase "Pending"}}]}
                  "pods" {:items [{:metadata {:namespace "moodle" :name "web-1"}
                                   :spec {:volumes [{:persistentVolumeClaim {:claimName "data"}}]}}]}}
        findings (storage/detect-storage-problems evidence)]
    (is (= #{"PersistentVolumeClaim is Pending"
             "PersistentVolumeClaim is not used by any pod"}
           (summaries findings)))))

(deftest workloads-detective-test
  (testing "deployments with no ready replicas are critical, partial ones warn"
    (let [evidence {"deployments" {:items [{:metadata {:namespace "moodle" :name "web"}
                                            :spec {:replicas 3}
                                            :status {:readyReplicas 1}}
                                           {:metadata {:namespace "keycloak" :name "kc"}
                                            :spec {:replicas 2}
                                            :status {}}
                                           {:metadata {:namespace "fine" :name "app"}
                                            :spec {:replicas 2}
                                            :status {:readyReplicas 2}}]}}
          findings (workloads/detect-deployment-problems evidence)]
      (is (= #{"Deployment has 1 of 3 replicas ready"
               "Deployment has 0 of 2 replicas ready"}
             (summaries findings)))
      (is (= :critical
             (:severity (first (filter #(= "keycloak/kc" (:component %)) findings)))))))
  (testing "failed jobs are reported"
    (is (= #{"Job failed (BackoffLimitExceeded)"}
           (summaries (workloads/detect-failed-jobs
                       {"jobs" {:items [{:metadata {:namespace "batch" :name "import"}
                                         :status {:conditions [{:type "Failed"
                                                                :status "True"
                                                                :reason "BackoffLimitExceeded"}]}}
                                        {:metadata {:namespace "fine" :name "done"}
                                         :status {:conditions [{:type "Complete"
                                                                :status "True"}]}}]}}))))))

(deftest calico-detective-test
  (let [evidence {calico/tigerastatus-type
                  {:items [{:metadata {:name "calico"}
                            :status {:conditions [{:type "Available" :status "False"
                                                   :message "not all pods are ready"}]}}
                           {:metadata {:name "apiserver"}
                            :status {:conditions [{:type "Available" :status "True"}
                                                  {:type "Degraded" :status "True"
                                                   :reason "PodFailure"}]}}]}}
        findings (calico/detect-calico-problems evidence)]
    (is (= #{"tigera component is not Available"
             "tigera component is Degraded (PodFailure)"}
           (summaries findings)))))

(deftest monitoring-detective-test
  (let [evidence {monitoring/prometheus-type
                  {:items [{:metadata {:namespace "monitoring" :name "prometheus"}
                            :status {:conditions [{:type "Available" :status "Degraded"
                                                   :reason "SomePodsNotReady"}]}}
                           {:metadata {:namespace "monitoring" :name "paused"}
                            :spec {:paused true}
                            :status {:conditions [{:type "Available" :status "True"}]}}]}}
        findings (monitoring/detect-prometheus-problems evidence)]
    (is (= #{"Prometheus is not Available (SomePodsNotReady)"
             "Prometheus is paused - the operator does not reconcile it"}
           (summaries findings)))))

(deftest registry-test
  (testing "the registry composes every scope and stays free of duplicates"
    (is (= (count (registry/all))
           (count (distinct (map :name (registry/all))))))
    (is (every? #(and (:name %) (:requires %) (fn? (:detect %))) (registry/all))))
  (testing "a scope resolves to its detectives, and :all spans them"
    (is (seq (registry/for-scope :workloads)))
    (is (= (count (registry/all)) (count (registry/for-scope :all)))))
  (testing "a plugin can register detectives into a scope, joining later scans"
    (let [before (count (registry/for-scope :workloads))
          probe {:name "plugin-probe" :description "a plugin detective" :requires [] :detect (fn [_] [])}]
      (registry/register! :workloads [probe])
      (is (= (inc before) (count (registry/for-scope :workloads))))
      (is (some #(= "plugin-probe" (:name %)) (registry/for-scope :all))))))

(deftest stuck-terminating-test
  (let [now (java.time.Instant/parse "2026-07-04T12:00:00Z")
        evidence {:now now
                  "pods" {:items [{:metadata {:namespace "moodle" :name "stuck"
                                              :deletionTimestamp "2026-07-04T11:00:00Z"}}
                                  {:metadata {:namespace "fine" :name "fresh-delete"
                                              :deletionTimestamp "2026-07-04T11:55:00Z"}}
                                  {:metadata {:namespace "fine" :name "alive"}}]}}
        findings (pods/detect-stuck-terminating evidence)]
    (is (= ["moodle/stuck"] (map :component findings)))))

(deftest cnpg-backup-detective-test
  (is (= #{"backup failed" "scheduled backup is suspended"}
         (summaries (cnpg/detect-backup-problems
                     {cnpg/backup-type
                      {:items [{:metadata {:namespace "moodle" :name "nightly-1"}
                                :status {:phase "failed" :error "connection refused"}}
                               {:metadata {:namespace "fine" :name "nightly-2"}
                                :status {:phase "completed"}}]}
                      cnpg/scheduled-backup-type
                      {:items [{:metadata {:namespace "moodle" :name "nightly"}
                                :spec {:suspend true}}]}})))))

(deftest flux-source-detective-test
  (is (= #{"OCIRepository is not Ready (PullFailed)"}
         (summaries (flux/detect-ocirepository-problems
                     {flux/ocirepository-type
                      {:items [{:metadata {:namespace "flux-system" :name "charts"}
                                :status {:conditions [{:type "Ready" :status "False"
                                                       :reason "PullFailed"}]}}]}})))))

(deftest report-format-test
  (testing "summary counts every severity"
    (is (= {:critical 1 :warning 2 :info 0}
           (detective/summary-of [{:severity :critical} {:severity :warning} {:severity :warning}]))))
  (testing "healthy means nothing critical"
    (is (detective/healthy? [{:severity :warning}]))
    (is (not (detective/healthy? [{:severity :critical}])))))

(deftest run-detectives-test
  (testing "findings are tagged with their detective and missing evidence is fine"
    (let [detectives [{:name "always" :requires ["nothing"]
                       :detect (fn [_] [{:severity :info :component "x" :summary "hello"}])}
                      {:name "empty" :requires ["nothing"]
                       :detect (fn [_] [])}]
          findings (detective/run-detectives detectives {})]
      (is (= [{:severity :info :component "x" :summary "hello" :detective "always"}]
             findings)))))

(deftest when-to-run-test
  (testing "prints the orientation to stderr, indented"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err] (detective/when-to-run! "Reach for this when DNS breaks."))
      (is (clojure.string/includes? (str err) "Reach for this when DNS breaks."))
      (is (clojure.string/includes? (str err) "When this helps"))))
  (testing "a blank or nil orientation prints nothing"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (detective/when-to-run! nil)
        (detective/when-to-run! "   "))
      (is (= "" (str err))))))

(deftest drillable-subjects-test
  (testing "findings that pin to an object become drill targets, others are dropped"
    (let [findings [{:detective "pods" :component "moodle/web-1" :summary "CrashLoopBackOff"}
                    {:detective "nodes" :component "worker-3" :summary "DiskPressure is active"}
                    {:detective "overcommit" :component "cluster" :summary "namespace overcommits memory"}]
          drillable (detective/drillable-subjects findings)]
      (is (= [(subject/subject "Pod" "moodle" "web-1")
              (subject/subject "Node" nil "worker-3")]
             (map :subject drillable)))
      (is (clojure.string/includes? (:label (first drillable)) "CrashLoopBackOff"))))
  (testing "several findings about the same object collapse to one target"
    (let [findings [{:detective "pods" :component "moodle/web-1" :summary "waiting"}
                    {:detective "pods" :component "moodle/web-1" :summary "restarted 12 times"}]]
      (is (= 1 (count (detective/drillable-subjects findings))))))
  (testing "a finding carrying an explicit :subject (Loki) is drillable straight to the workload"
    (let [findings [{:detective "loki-health" :component "distributor" :summary "component has no ready replicas (0/3)"
                     :subject (subject/subject "Deployment" "loki" "loki-distributor")}]]
      (is (= [(subject/subject "Deployment" "loki" "loki-distributor")]
             (map :subject (detective/drillable-subjects findings)))))))

(deftest roll-up-test
  (testing "identical (severity, summary) findings collapse to one entry with a count"
    (let [findings [{:severity :warning :summary "pod failed" :component "a"}
                    {:severity :warning :summary "pod failed" :component "b"}
                    {:severity :warning :summary "pod failed" :component "c"}
                    {:severity :critical :summary "node down" :component "n1"}]
          rolled (detective/roll-up findings)]
      (is (= 2 (count rolled)))
      (is (= :critical (:severity (first rolled))) "criticals sort first")
      (let [failed (first (filter #(= "pod failed" (:summary %)) rolled))]
        (is (= 3 (:count failed)))
        (is (= ["a" "b" "c"] (:components failed))))))
  (testing "areas are ordered most-severe first"
    (let [findings [{:severity :info :detective "storage" :component "x" :summary "unused"}
                    {:severity :critical :detective "pods" :component "y" :summary "crash"}]]
      (is (= ["pods" "storage"] (map first (detective/areas findings)))))))

(def ^:private sample-detectives
  [{:name "dns-transports" :description "The entry server answers over IPv4 and IPv6" :requires []}
   {:name "dns-nameservers" :description "The zone has redundant nameservers" :requires []}])

(deftest report-shows-what-was-checked-test
  (testing "text report lists every detective's description even when clean"
    (let [out (with-out-str
                (binding [*err* (java.io.StringWriter.)]
                  (detective/report! sample-detectives [] "text")))]
      (is (clojure.string/includes? out "The entry server answers over IPv4 and IPv6"))
      (is (clojure.string/includes? out "The zone has redundant nameservers"))))
  (testing "json report carries a :checked list of what ran"
    (let [out (with-out-str
                (binding [*err* (java.io.StringWriter.)]
                  (detective/report! sample-detectives [] "json")))
          parsed (cheshire.core/parse-string out true)]
      (is (= #{"dns-transports" "dns-nameservers"}
             (set (map :name (:checked parsed)))))
      (is (= {:critical 0 :warning 0 :info 0} (:summary parsed)))))
  (testing "text report groups findings by area, worst area first"
    (let [findings [{:severity :info :detective "storage" :component "pvc-1" :summary "unused PVC"}
                    {:severity :critical :detective "pods" :component "web-1" :summary "CrashLoopBackOff"}]
          out (with-out-str
                (binding [*err* (java.io.StringWriter.)]
                  (detective/report! [{:name "pods" :description "Pods are healthy"}
                                      {:name "storage" :description "PVCs are used"}
                                      {:name "nodes" :description "Nodes are Ready"}]
                                     findings "text")))
          pods-at (clojure.string/index-of out "pods (")
          storage-at (clojure.string/index-of out "storage (")]
      (is (and pods-at storage-at (< pods-at storage-at))
          "the area with a critical leads the story")
      (is (clojure.string/includes? out "1 critical, 0 warning, 1 info"))
      (is (clojure.string/includes? out "came back clean: nodes")
          "a detective with no findings is named as a clean check")))

  (testing "edn report carries :checked too, and health follows criticals"
    (let [healthy (binding [*err* (java.io.StringWriter.)]
                    (detective/report! sample-detectives [] "edn"))
          unhealthy (binding [*err* (java.io.StringWriter.)]
                      (detective/report! sample-detectives
                                         [{:severity :critical :component "x" :summary "boom"
                                           :detective "dns-transports"}]
                                         "edn"))]
      (is (true? healthy))
      (is (false? unhealthy)))))
