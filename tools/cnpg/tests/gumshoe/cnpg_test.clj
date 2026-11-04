;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.cnpg]))

(defn- summaries [findings] (set (map :summary findings)))

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

(deftest package-registers-databases-scope-and-capability-test
  (is (seq (registry/for-scope :databases)) "the package joins the :databases scan scope")
  (is (contains? (set (capabilities/registered)) :cnpg)))

;; The live object from cloudnative-pg-cluster-general on kube.bastelgenosse.de:
;; a plausible-looking cluster whose designated primary is the one instance the
;; operator does not call healthy.
(def ^:private stuck-primary-cluster
  {:metadata {:namespace "cloudnative-pg-cluster-general"
              :name "cloudnative-pg-cluster-general"}
   :spec {:instances 3}
   :status {:phase "Not enough disk space"
            :instances 3
            :readyInstances 2
            :currentPrimary "cloudnative-pg-cluster-general-2"
            :targetPrimary "cloudnative-pg-cluster-general-2"
            :instancesStatus {:healthy ["cloudnative-pg-cluster-general-1"
                                        "cloudnative-pg-cluster-general-3"]
                              :replicating ["cloudnative-pg-cluster-general-2"]}
            :conditions [{:type "Ready" :status "False" :reason "ClusterIsNotReady"
                          :message "Cluster Is Not Ready"
                          :lastTransitionTime "2026-10-30T19:56:03Z"}
                         {:type "ContinuousArchiving" :status "True"}]}})

(defn- primary-findings
  [cluster now]
  (cnpg/detect-primary-problems
   {cnpg/cluster-type {:items [cluster]} :now (java.time.Instant/parse now)}))

(deftest a-primary-outside-the-healthy-set-is-critical-test
  (let [findings (primary-findings stuck-primary-cluster "2026-11-04T20:00:00Z")]
    (is (= #{"the primary cloudnative-pg-cluster-general-2 is not healthy (listed as replicating)"
             "not Ready for 5d 0h"}
           (summaries findings)))
    (is (= [:critical :critical] (map :severity findings)))
    (is (re-find #"no switchover is under way" (:hint (first findings))))
    (is (re-find #"accept no writes" (:hint (first findings))))))

(deftest a-healthy-primary-is-quiet-test
  (is (empty? (primary-findings
               (assoc-in (assoc-in stuck-primary-cluster
                                   [:status :instancesStatus]
                                   {:healthy ["cloudnative-pg-cluster-general-1"
                                              "cloudnative-pg-cluster-general-2"
                                              "cloudnative-pg-cluster-general-3"]})
                         [:status :conditions] [{:type "Ready" :status "True"}])
               "2026-11-04T20:00:00Z"))))

(deftest a-switchover-under-way-names-its-target-test
  (let [f (first (primary-findings
                  (assoc-in stuck-primary-cluster [:status :targetPrimary]
                            "cloudnative-pg-cluster-general-3")
                  "2026-11-04T20:00:00Z"))]
    (is (re-find #"switchover to cloudnative-pg-cluster-general-3 is under way" (:hint f)))))

(deftest a-brief-not-ready-window-is-not-reported-test
  (let [findings (primary-findings
                  (assoc-in stuck-primary-cluster [:status :conditions]
                            [{:type "Ready" :status "False" :reason "ClusterIsNotReady"
                              :lastTransitionTime "2026-11-04T19:45:00Z"}])
                  "2026-11-04T20:00:00Z")]
    (is (= 1 (count findings)) "a 15-minute roll is in progress, not stuck")
    (is (re-find #"is not healthy" (:summary (first findings))))))

(deftest an-absent-now-suppresses-the-duration-rule-test
  (let [findings (cnpg/detect-primary-problems
                  {cnpg/cluster-type {:items [stuck-primary-cluster]}})]
    (is (= 1 (count findings)) "without a clock the age is unknown, not acceptable")))

(deftest a-cluster-reporting-no-instance-states-is-quiet-test
  (is (empty? (primary-findings
               (-> stuck-primary-cluster
                   (assoc-in [:status :instancesStatus] {})
                   (assoc-in [:status :conditions] [{:type "Ready" :status "True"}]))
               "2026-11-04T20:00:00Z"))))

;; The live pods from the same cluster: -2 had reached 1398 restarts over 8 days
;; while the cluster reported a storage phase and 2 of 3 instances ready.
(def ^:private crashlooping-instance-pod
  {:metadata {:namespace "cloudnative-pg-cluster-general"
              :name "cloudnative-pg-cluster-general-2"
              :labels {:cnpg.io/cluster "cloudnative-pg-cluster-general"
                       :cnpg.io/podRole "instance"
                       :cnpg.io/instanceName "cloudnative-pg-cluster-general-2"}
              :ownerReferences [{:kind "Cluster" :name "cloudnative-pg-cluster-general"}]}
   :status {:containerStatuses
            [{:name "postgres" :ready false :restartCount 1398
              :state {:waiting {:reason "CrashLoopBackOff"
                                :message "back-off 5m0s restarting failed container=postgres"}}}]}})

(def ^:private running-instance-pod
  {:metadata {:namespace "cloudnative-pg-cluster-general"
              :name "cloudnative-pg-cluster-general-1"
              :labels {:cnpg.io/cluster "cloudnative-pg-cluster-general"
                       :cnpg.io/podRole "instance"}
              :ownerReferences [{:kind "Cluster" :name "cloudnative-pg-cluster-general"}]}
   :status {:containerStatuses
            [{:name "postgres" :ready true :restartCount 0
              :state {:running {:startedAt "2026-10-27T01:39:53Z"}}}]}})

;; A pooler's own pods carry cnpg.io/cluster as well, so a crashlooping
;; PgBouncer must not be reported as a Postgres instance.
(def ^:private crashlooping-pooler-pod
  {:metadata {:namespace "cloudnative-pg-cluster-general"
              :name "cloudnative-pg-cluster-general-rw-session-5f9d546867-nnlcd"
              :labels {:cnpg.io/cluster "cloudnative-pg-cluster-general"
                       :cnpg.io/poolerName "cloudnative-pg-cluster-general-rw-session"
                       :pod-template-hash "5f9d546867"}}
   :status {:containerStatuses
            [{:name "pgbouncer" :ready false :restartCount 12
              :state {:waiting {:reason "CrashLoopBackOff"}}}]}})

(defn- crashloop-findings
  [pods clusters]
  (cnpg/detect-crashlooping-instances
   {"pods" {:items pods} cnpg/cluster-type {:items clusters}}))

(deftest a-crashlooping-instance-is-critical-test
  (let [findings (crashloop-findings [crashlooping-instance-pod] [stuck-primary-cluster])]
    (is (= 1 (count findings)))
    (is (= :critical (:severity (first findings))))
    (is (= "cloudnative-pg-cluster-general/cloudnative-pg-cluster-general-2"
           (:component (first findings))))
    (is (= #{"postgres is in CrashLoopBackOff after 1398 restarts"} (summaries findings)))))

(deftest a-crashlooping-primary-says-nothing-accepts-writes-test
  (let [finding (first (crashloop-findings [crashlooping-instance-pod] [stuck-primary-cluster]))]
    (is (str/includes? (:hint finding) "designated primary"))))

(deftest a-crashlooping-replica-does-not-claim-the-primary-test
  (let [replica (assoc-in crashlooping-instance-pod [:metadata :name]
                          "cloudnative-pg-cluster-general-3")
        finding (first (crashloop-findings [replica] [stuck-primary-cluster]))]
    (is (some? finding))
    (is (not (str/includes? (:hint finding) "designated primary")))))

(deftest a-crashlooping-pooler-pod-is-not-an-instance-test
  (is (empty? (crashloop-findings [crashlooping-pooler-pod] [stuck-primary-cluster]))))

(deftest a-running-instance-is-quiet-test
  (is (empty? (crashloop-findings [running-instance-pod] [stuck-primary-cluster]))))

(deftest an-absent-cluster-still-reports-the-crashloop-test
  (let [finding (first (crashloop-findings [crashlooping-instance-pod] []))]
    (is (= :critical (:severity finding)))
    (is (not (str/includes? (:hint finding) "designated primary")))))

(deftest a-pod-without-container-statuses-is-quiet-test
  (is (empty? (crashloop-findings [(dissoc crashlooping-instance-pod :status)]
                                  [stuck-primary-cluster]))))
