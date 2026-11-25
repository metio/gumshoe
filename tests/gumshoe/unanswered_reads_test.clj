;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.unanswered-reads-test
  "Checks whose subjects come from some of their reads while the others only
   add to a hint. Each consults the subject reads, so with none of them
   answered it is unread and silent instead of passing on reads that only
   enrich; replicas-on-one-node also joins the pods and nodes it places. The
   cases run through the table and properties of gumshoe.joins-support.
   Alongside: a DaemonSet with no available pod is critical, ephemeral
   containers are judged with the others, and the scopes a cluster scan
   cannot collect for are opt-in."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [gumshoe.detectives.pod-creation :as pod-creation]
            [gumshoe.detectives.pod-security :as pod-security]
            [gumshoe.detectives.posture :as posture]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.detectives.terminating :as terminating]
            [gumshoe.detectives.workloads :as workloads]
            [gumshoe.evidence :as evidence]
            [gumshoe.joins-support :as joins :refer [items meta-of check-named]]))

(def ^:private now (java.time.Instant/parse "2026-11-25T12:00:00Z"))

;; --- cases ------------------------------------------------------------------------

(def ^:private pod-creation-case
  {:check (check-named pod-creation/detectives "pod-creation")
   :consults ["events" "replicasets"]
   :answered {:now now
              "events" (items {:reason "FailedCreate" :lastTimestamp "2026-11-25T11:50:00Z"
                               :involvedObject {:kind "StatefulSet" :namespace "db" :name "pg"}
                               :message "pods \"pg-0\" is forbidden: exceeded quota: compute"})
              "replicasets" (items {:metadata (meta-of "web" "app-7f9")
                                    :status {:conditions [{:type "ReplicaFailure" :status "True"
                                                           :lastTransitionTime "2026-11-25T09:00:00Z"
                                                           :message "pods \"app-7f9-x\" is forbidden: violates PodSecurity \"restricted:latest\""}]}})
              "validatingwebhookconfigurations" (items)
              "mutatingwebhookconfigurations" (items)
              "services" (items)
              "pods" (items)}})

(defn- stuck [namespace name minutes & [finalizers]]
  {:metadata (cond-> {:name name
                      :deletionTimestamp (str (.minusSeconds ^java.time.Instant now (* 60 minutes)))
                      :finalizers (or finalizers ["example.org/cleanup"])}
               namespace (assoc :namespace namespace))})

(def ^:private terminating-case
  {:check (check-named terminating/detectives "stuck-terminating")
   :consults (mapv first terminating/types)
   :answered (into {:now now
                    "pods" (items {:metadata (meta-of "db" "pg-0")
                                   :spec {:volumes [{:persistentVolumeClaim {:claimName "data"}}]}})}
                   (for [[type] terminating/types]
                     [type (case type
                             "deployments" (items (stuck "web" "app" 45))
                             "persistentvolumeclaims" (items (stuck "db" "data" 15 ["kubernetes.io/pvc-protection"]))
                             "persistentvolumes" (items (stuck nil "pv-1" 60 ["kubernetes.io/pv-protection"]))
                             (items))]))})

(defn- ready-pod [namespace name owner node]
  {:metadata (assoc (meta-of namespace name)
                    :ownerReferences [(assoc owner :controller true)]
                    :labels (if (= "ReplicaSet" (:kind owner)) {:pod-template-hash "5d8"} {}))
   :spec {:nodeName node}
   :status {:phase "Running" :conditions [{:type "Ready" :status "True"}]}})

(def ^:private replicas-case
  {:check (check-named posture/detectives "replicas-on-one-node")
   :joins ["pods" "nodes"]
   :consults ["deployments" "statefulsets"]
   :answered {"deployments" (items {:metadata (meta-of "web" "app") :spec {:replicas 2}})
              "statefulsets" (items {:metadata (meta-of "db" "pg") :spec {:replicas 3}})
              "pods" (items (ready-pod "web" "app-5d8-a" {:kind "ReplicaSet" :name "app-5d8"} "n1")
                            (ready-pod "web" "app-5d8-b" {:kind "ReplicaSet" :name "app-5d8"} "n1")
                            (ready-pod "db" "pg-0" {:kind "StatefulSet" :name "pg"} "n2")
                            (ready-pod "db" "pg-1" {:kind "StatefulSet" :name "pg"} "n2"))
              "nodes" (items {:metadata (meta-of "n1")} {:metadata (meta-of "n2")})}})

(def ^:private consulting-cases [pod-creation-case terminating-case replicas-case])
(def ^:private joining-cases (filterv :joins consulting-cases))

(defn- label [case] (:name (:check case)))

(defn- summaries-on [{:keys [check]} evidence]
  (sort (map (juxt :component :summary) ((:detect check) evidence))))

;; --- declarations -----------------------------------------------------------------

(deftest every-check-declares-what-it-consults-and-joins
  (doseq [{:keys [check consults joins] :as case} consulting-cases]
    (testing (label case)
      (is (= consults (:consults check)))
      (is (= joins (:joins check)))
      (is (every? (set (:requires check)) (concat consults joins)) "every consulted and joined read is required")
      (is (not-any? (set joins) consults) "a consulted read is not also joined"))))

;; --- the table --------------------------------------------------------------------

(deftest answered-evidence-reports-the-true-findings
  (is (= [["db/pg" "StatefulSet cannot create pods: a ResourceQuota is exhausted"]
          ["web/app-7f9" "ReplicaSet cannot create pods: the pod violates the namespace's PodSecurity level"]]
         (summaries-on pod-creation-case (:answered pod-creation-case))))
  (is (= [["db/data" "PersistentVolumeClaim is stuck terminating for 15 minutes, waiting on kubernetes.io/pvc-protection"]
          ["pv-1" "PersistentVolume is stuck terminating for 60 minutes, waiting on kubernetes.io/pv-protection"]
          ["web/app" "Deployment is stuck terminating for 45 minutes, waiting on example.org/cleanup"]]
         (summaries-on terminating-case (:answered terminating-case))))
  (is (= [["db/pg" "StatefulSet runs all 2 ready replicas on node n2"]
          ["web/app" "Deployment runs all 2 ready replicas on node n1"]]
         (summaries-on replicas-case (:answered replicas-case)))))

(deftest answered-evidence-is-read
  (doseq [{:keys [check answered] :as case} consulting-cases]
    (testing (label case)
      (let [{:keys [unread? findings]} (joins/judgement check answered)]
        (is (not unread?))
        (is (seq findings))
        (is (not-any? #(= "detective failed to run - investigate its input" (:summary %)) findings))))))

(deftest no-consulted-read-answered-judges-nothing
  (doseq [{:keys [check consults answered] :as case} consulting-cases]
    (testing (label case)
      (let [evidence (joins/without answered consults)]
        (is (= {:unread? true :findings []} (joins/judgement check evidence)))
        (is (empty? ((:detect check) evidence)) "the detect function itself reports nothing")))))

(deftest every-consulted-read-answering-alone-is-judged
  (doseq [{:keys [check consults answered] :as case} consulting-cases
          [present evidence] (butlast (joins/consulted-read-cases consults answered))]
    (testing (str (label case) ": " present)
      (is (not (:unread? (joins/judgement check evidence))))
      (is (not-any? #(= "detective failed to run - investigate its input" (:summary %))
                    (:findings (joins/judgement check evidence)))))))

(deftest a-consulted-read-answering-alone-reports-what-it-alone-judges
  (let [alone (fn [case] (into {} (butlast (joins/consulted-read-cases (:consults case) (:answered case)))))]
    (is (= [["db/pg" "StatefulSet cannot create pods: a ResourceQuota is exhausted"]]
           (summaries-on pod-creation-case ((alone pod-creation-case) "only events answered"))))
    (is (= [["web/app-7f9" "ReplicaSet cannot create pods: the pod violates the namespace's PodSecurity level"]]
           (summaries-on pod-creation-case ((alone pod-creation-case) "only replicasets answered"))))
    (is (= [["web/app" "Deployment runs all 2 ready replicas on node n1"]]
           (summaries-on replicas-case ((alone replicas-case) "only deployments answered"))))
    (is (= [["pv-1" "PersistentVolume is stuck terminating for 60 minutes, waiting on kubernetes.io/pv-protection"]]
           (summaries-on terminating-case ((alone terminating-case) "only persistentvolumes answered"))))))

(deftest a-missing-joined-read-judges-nothing
  (doseq [{:keys [check joins answered] :as case} joining-cases
          [missing evidence] (joins/missing-read-cases joins answered)]
    (testing (str (label case) ": " missing)
      (is (= {:unread? true :findings []} (joins/judgement check evidence)))
      (is (empty? ((:detect check) evidence))))))

(deftest the-hint-reads-only-enrich
  ;; pods for stuck-terminating, and webhooks, Services and pods for
  ;; pod-creation, judge nothing on their own: without them the same subjects
  ;; are reported
  (is (= (summaries-on terminating-case (:answered terminating-case))
         (summaries-on terminating-case (assoc (:answered terminating-case) "pods" nil))))
  (is (= (summaries-on pod-creation-case (:answered pod-creation-case))
         (summaries-on pod-creation-case (joins/without (:answered pod-creation-case)
                                                        ["validatingwebhookconfigurations" "mutatingwebhookconfigurations"
                                                         "services" "pods"])))))

;; --- properties -------------------------------------------------------------------

(def ^:private seed 20261125)
(def ^:private trials 300)

(deftest no-unconsulted-check-yields-a-pass-or-a-finding
  (let [result (tc/quick-check trials (joins/no-unconsulted-check-judges consulting-cases) :seed seed)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :fail])))))

(deftest the-property-catches-a-check-that-forgets-what-it-consults
  ;; positive control: without :consults a hint read keeps the check read
  (doseq [case consulting-cases]
    (testing (label case)
      (is (not (:pass? (tc/quick-check trials
                                       (joins/no-unconsulted-check-judges [(update case :check dissoc :consults)])
                                       :seed seed)))))))

(deftest no-missing-joined-read-yields-a-pass-or-a-finding
  (let [result (tc/quick-check trials (joins/no-missing-joined-read-judges joining-cases) :seed seed)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :fail])))))

(deftest the-property-catches-a-check-that-forgets-its-join
  (doseq [case joining-cases]
    (testing (label case)
      (is (not (:pass? (tc/quick-check trials
                                       (joins/no-missing-joined-read-judges [(update case :check dissoc :joins)])
                                       :seed seed)))))))

(deftest every-detect-function-consults-the-guard
  ;; positive control for the guards inside the detect functions
  (with-redefs [evidence/consulted? (constantly false)]
    (doseq [{:keys [check answered] :as case} consulting-cases]
      (testing (label case)
        (is (empty? ((:detect check) answered))))))
  (with-redefs [evidence/joined? (constantly false)]
    (doseq [{:keys [check answered] :as case} joining-cases]
      (testing (label case)
        (is (empty? ((:detect check) answered)))))))

(deftest the-generated-evidence-is-not-vacuous
  (let [samples (gen/sample (joins/gen-answered consulting-cases) 1000)
        reporting (set (for [[check evidence] samples
                             :when (seq ((:detect check) evidence))]
                         (:name check)))]
    (is (= (set (map label consulting-cases)) reporting))))

;; --- DaemonSets -------------------------------------------------------------------

(defn- daemonset [status]
  {"daemonsets" (items {:metadata (meta-of "calico-system" "calico-node") :status status})})

(deftest a-daemonset-without-an-available-pod-is-critical
  (testing "numberAvailable is omitted when it is zero"
    (is (= [[:critical "DaemonSet has no available pod: 5 of 5 unavailable"]]
           (map (juxt :severity :summary)
                (workloads/detect-daemonset-problems
                 (daemonset {:desiredNumberScheduled 5 :currentNumberScheduled 5 :numberReady 0 :numberUnavailable 5}))))))
  (testing "written out as zero"
    (is (= [:critical]
           (map :severity (workloads/detect-daemonset-problems
                           (daemonset {:desiredNumberScheduled 3 :currentNumberScheduled 3 :numberAvailable 0 :numberUnavailable 3})))))))

(deftest a-daemonset-with-some-pods-available-warns
  (is (= [[:warning "DaemonSet has 2 unavailable pods"]]
         (map (juxt :severity :summary)
              (workloads/detect-daemonset-problems
               (daemonset {:desiredNumberScheduled 5 :currentNumberScheduled 5 :numberAvailable 3 :numberUnavailable 2}))))))

(deftest a-daemonset-targeting-no-node-is-no-outage
  (is (empty? (workloads/detect-daemonset-problems (daemonset {:desiredNumberScheduled 0 :currentNumberScheduled 0}))))
  (is (empty? (workloads/detect-daemonset-problems
               (daemonset {:desiredNumberScheduled 2 :currentNumberScheduled 2 :numberAvailable 2})))))

;; --- ephemeral containers ---------------------------------------------------------

(def ^:private debugged-pod
  {"pods" (items {:metadata (meta-of "web" "app-1")
                  :spec {:containers [{:name "app"}]
                         :ephemeralContainers [{:name "debugger-x7"
                                                :securityContext {:privileged true}}
                                               {:name "debugger-y2"
                                                :securityContext {:capabilities {:add ["SYS_ADMIN"]}}}
                                               {:name "debugger-z9"
                                                :securityContext {:capabilities {:add ["ALL"]}}}]}})})

(deftest ephemeral-containers-are-judged-with-the-others
  (is (= [[:critical "web/app-1" "ephemeral container debugger-x7 runs privileged"]]
         (map (juxt :severity :component :summary) (pod-security/detect-privileged-containers debugged-pod))))
  (is (= [[:warning "ephemeral container debugger-y2 adds capabilities: SYS_ADMIN"]
          [:critical "ephemeral container debugger-z9 adds ALL capabilities"]]
         (map (juxt :severity :summary) (pod-security/detect-dangerous-capabilities debugged-pod)))))

(deftest regular-containers-keep-their-label
  (is (= ["container app runs privileged" "container init runs privileged"]
         (map :summary (pod-security/detect-privileged-containers
                        {"pods" (items {:metadata (meta-of "web" "app-1")
                                        :spec {:containers [{:name "app" :securityContext {:privileged true}}]
                                               :initContainers [{:name "init" :securityContext {:privileged true}}]}})})))))

;; --- scopes -----------------------------------------------------------------------

(deftest a-cluster-scan-leaves-out-what-only-its-own-book-collects
  (let [all-names (set (map :name (registry/for-scope :all)))]
    (doseq [scope [:mail :fabric]]
      (testing scope
        (is (contains? registry/opt-in-scopes scope))
        (is (seq (registry/for-scope scope)))
        (is (not-any? all-names (map :name (registry/for-scope scope))))))))
