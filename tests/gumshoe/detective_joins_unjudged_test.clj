;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detective-joins-unjudged-test
  "Checks that correlate one read against another and, with one of those
   reads unanswered, would judge nothing or only part of what they cover
   while counting as read: a silent pass, or a partial one. Each declares
   the reads it joins as :joins, so the scan names it as unread, and its
   detect function reports nothing then. Fixtures are synthetic, shaped as
   the API server returns the objects."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [gumshoe.detectives.apiversions :as apiversions]
            [gumshoe.detectives.capacity :as capacity]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.cnpg-storage :as cnpg-storage]
            [gumshoe.detectives.csi :as csi]
            [gumshoe.detectives.network :as network]
            [gumshoe.detectives.pod-startup :as pod-startup]
            [gumshoe.detectives.pooler :as pooler]
            [gumshoe.detectives.rbac :as rbac]
            [gumshoe.detectives.references :as references]
            [gumshoe.detectives.template-drift :as template-drift]
            [gumshoe.detectives.tls-secrets :as tls-secrets]
            [gumshoe.detectives.upgrade :as upgrade]
            [gumshoe.evidence :as evidence]
            [gumshoe.joins-support :as joins :refer [items meta-of check-named]]
            [gumshoe.tools.flux :as flux]))

;; --- fixtures ---------------------------------------------------------------------

(def ^:private now (java.time.Instant/parse "2026-11-25T12:00:00Z"))

(defn- pod [namespace name & {:keys [labels owner node phase created containers statuses]}]
  {:metadata (cond-> (assoc (meta-of namespace name) :labels (or labels {}))
               owner (assoc :ownerReferences [(assoc owner :controller true)])
               created (assoc :creationTimestamp created))
   :spec (cond-> {:containers (or containers [{:name "app" :image "registry.example.org/app:1.0"}])}
           node (assoc :nodeName node))
   :status (cond-> {:phase (or phase "Running") :conditions [{:type "Ready" :status "True"}]}
             statuses (assoc :containerStatuses statuses))})

(defn- template [& {:keys [service-account pull-secret env image]}]
  {:spec (cond-> {:containers [(cond-> {:name "app" :image (or image "registry.example.org/app:1.0")}
                                 env (assoc :env env))]}
           service-account (assoc :serviceAccountName service-account)
           pull-secret (assoc :imagePullSecrets [{:name pull-secret}]))})

(defn- binding-of [kind role subject & [namespace name]]
  {:metadata (if namespace (meta-of namespace name) (meta-of (or name role)))
   :roleRef {:kind kind :name role}
   :subjects [subject]})

(def ^:private god-role
  {:metadata (meta-of "god") :rules [{:verbs ["*"] :resources ["*"] :apiGroups ["*"]}]})

;; --- the cases --------------------------------------------------------------------
;;
;; Each case: the check, the reads it must join, what its detect function
;; does with a joined read missing once its guard is disabled (:unguarded:
;; :silent when it reports nothing, :partial when it still reports what the
;; other reads allow, :false-finding when the missing side reads as objects
;; that do not exist), and answered evidence on which it reports at least one
;; finding.

(def ^:private pod-startup
  {:check (check-named pod-startup/detectives "pod-startup")
   :joins ["pods" "events"]
   :unguarded :partial
   :answered {:now now
              "pods" (items (pod "web" "stuck" :node "n1" :phase "Pending" :created "2026-11-25T11:00:00Z")
                            (pod "web" "mounting" :node "n1" :phase "Pending" :created "2026-11-25T11:55:00Z"))
              "events" (items {:involvedObject {:kind "Pod" :namespace "web" :name "mounting"}
                               :reason "FailedMount"
                               :message "MountVolume.SetUp failed for volume \"config\""
                               :lastTimestamp "2026-11-25T11:58:00Z"})}})

(def ^:private rbac-admins
  {:check (check-named rbac/detectives "rbac-admins")
   :joins ["clusterroles" "clusterrolebindings"]
   :unguarded :partial
   :answered {"clusterroles" (items god-role)
              "clusterrolebindings" (items (binding-of "ClusterRole" "cluster-admin" {:kind "User" :name "alice"} nil "humans")
                                           (binding-of "ClusterRole" "god" {:kind "Group" :name "ops"} nil "ops"))}})

(def ^:private rbac-namespace-grants
  {:check (check-named rbac/detectives "rbac-namespace-grants")
   :joins ["clusterroles" "rolebindings"]
   :unguarded :partial
   :answered {"clusterroles" (items god-role)
              "rolebindings" (items (binding-of "ClusterRole" "edit" {:kind "User" :name "bob"} "web" "editors")
                                    (binding-of "ClusterRole" "god" {:kind "User" :name "carol"} "web" "gods"))}})

(def ^:private rbac-secret-readers
  {:check (check-named rbac/detectives "rbac-secret-readers")
   :joins ["clusterroles" "clusterrolebindings"]
   :unguarded :silent
   :answered {"clusterroles" (items {:metadata (meta-of "secret-reader")
                                     :rules [{:apiGroups [""] :resources ["secrets"] :verbs ["get"]}]})
              "clusterrolebindings" (items (binding-of "ClusterRole" "secret-reader" {:kind "User" :name "dave"} nil "readers"))}})

(def ^:private template-references
  {:check (check-named references/detectives "template-references")
   :joins ["deployments" "statefulsets" "daemonsets" "cronjobs"]
   :consults ["services" "serviceaccounts" "secret-names" "namespaces"]
   :unguarded :partial
   :answered {"deployments" (items {:metadata (meta-of "web" "a") :spec {:template (template :service-account "ghost")}})
              "statefulsets" (items {:metadata (meta-of "web" "db") :spec {:template (template :pull-secret "ghost")}})
              "daemonsets" (items {:metadata (meta-of "web" "agent")
                                   :spec {:template (template :env [{:name "DB_HOST" :value "db.data"}])}})
              "cronjobs" (items {:metadata (meta-of "web" "job")
                                 :spec {:jobTemplate {:spec {:template (template :service-account "ghost")}}}})
              "services" (items)
              "serviceaccounts" (items)
              "secret-names" []
              "namespaces" (items {:metadata (meta-of "data")})}})

(def ^:private storageclass-references
  {:check (check-named references/detectives "storageclass-references")
   :joins ["persistentvolumeclaims" "storageclasses"]
   :unguarded :false-finding
   :answered {"persistentvolumeclaims" (items {:metadata (meta-of "web" "data")
                                               :spec {:storageClassName "gone"}
                                               :status {:phase "Pending"}})
              "storageclasses" (items {:metadata (meta-of "fast")})}})

(def ^:private hpa-target-references
  {:check (check-named references/detectives "hpa-target-references")
   :joins ["horizontalpodautoscalers" "deployments" "statefulsets"]
   :unguarded :partial
   :answered {"horizontalpodautoscalers" (items {:metadata (meta-of "web" "app")
                                                 :spec {:scaleTargetRef {:kind "Deployment" :name "gone"}}}
                                                {:metadata (meta-of "web" "db")
                                                 :spec {:scaleTargetRef {:kind "StatefulSet" :name "gone"}}})
              "deployments" (items)
              "statefulsets" (items)}})

(def ^:private role-references
  {:check (check-named references/security-detectives "role-references")
   :joins ["rolebindings" "clusterrolebindings" "roles" "clusterroles"]
   :unguarded :partial
   :answered {"rolebindings" (items (binding-of "Role" "gone" {:kind "User" :name "erin"} "web" "local"))
              "clusterrolebindings" (items (binding-of "ClusterRole" "gone" {:kind "User" :name "erin"} nil "global"))
              "roles" (items)
              "clusterroles" (items)}})

(def ^:private webhook-references
  {:check (check-named references/platform-detectives "webhook-references")
   :joins ["validatingwebhookconfigurations" "mutatingwebhookconfigurations" "services"]
   :unguarded :partial
   :answered {"validatingwebhookconfigurations"
              (items {:metadata (meta-of "policy")
                      :webhooks [{:name "validate.example.org" :clientConfig {:service {:namespace "infra" :name "gone"}}}]})
              "mutatingwebhookconfigurations"
              (items {:metadata (meta-of "inject")
                      :webhooks [{:name "mutate.example.org" :clientConfig {:service {:namespace "infra" :name "gone"}}}]})
              "services" (items)}})

(def ^:private overcommit
  {:check (check-named capacity/detectives "overcommit")
   :joins ["pods" "nodes"]
   :unguarded :silent
   :answered {"pods" (items (assoc-in (pod "web" "big") [:spec :containers 0 :resources :requests :cpu] "8"))
              "nodes" (items {:metadata (meta-of "n1") :status {:allocatable {:cpu "2" :memory "4Gi"}}}
                             {:metadata (meta-of "n2") :status {:allocatable {:cpu "2" :memory "4Gi"}}})}})

(def ^:private local-volumes
  {:check (check-named csi/detectives "local-volumes")
   :joins ["persistentvolumes" "nodes"]
   :unguarded :silent
   :answered {"persistentvolumes" (items {:metadata (meta-of "pv-local")
                                          :spec {:nodeAffinity {:required {:nodeSelectorTerms
                                                                           [{:matchExpressions [{:key "kubernetes.io/hostname"
                                                                                                 :operator "In"
                                                                                                 :values ["gone"]}]}]}}}})
              "nodes" (items {:metadata (meta-of "n1")})}})

(def ^:private isolated-services
  {:check (check-named network/detectives "isolated-services")
   :joins ["services" "pods" "networkpolicies" network/global-policy-type network/calico-policy-type]
   ;; a missing Calico read would leave a Calico-admitted Service reported as isolated
   :unguarded :false-finding
   :answered {"services" (items {:metadata (meta-of "web" "app") :spec {:selector {:app "app"}}})
              "pods" (items (pod "web" "app-1" :labels {:app "app"}))
              "networkpolicies" (items {:metadata (meta-of "web" "default-deny") :spec {:podSelector {}}})
              network/global-policy-type (items)
              network/calico-policy-type (items)}})

(def ^:private version-drift
  {:check (check-named apiversions/detectives "version-drift")
   :joins ["customresourcedefinitions" "api-resources"]
   :unguarded :silent
   :answered {"customresourcedefinitions" (items {:metadata (meta-of "widgets.example.org")
                                                  :spec {:group "example.org"
                                                         :names {:plural "widgets"}
                                                         :versions [{:name "v1" :served true}]}})
              "api-resources" {["example.org" "widgets"] "v1beta1"}}})

(def ^:private not-a-certificate "bm90IGEgY2VydGlmaWNhdGU=")

(def ^:private tls-secrets
  {:check (check-named tls-secrets/detectives "tls-secrets")
   :joins ["ingresses" "tls-secrets"]
   :unguarded :partial
   :answered {:now now
              "ingresses" (items {:metadata (meta-of "web" "site")
                                  :spec {:tls [{:secretName "site-tls" :hosts ["site.example.org"]}]}})
              tls-secrets/gateway-type (items {:metadata (meta-of "infra" "edge")
                                               :spec {:listeners [{:name "https" :tls {:certificateRefs [{:name "edge-tls"}]}}]}})
              "tls-secrets" [{:namespace "web" :name "site-tls" :crt not-a-certificate}
                             {:namespace "infra" :name "edge-tls" :crt not-a-certificate}]}})

(def ^:private template-drift
  {:check (check-named template-drift/detectives "template-drift")
   :joins ["pods" "replicasets" "statefulsets" "daemonsets" "jobs" "controllerrevisions"]
   :unguarded :partial
   :answered {"pods" (items (pod "web" "app-1" :owner {:kind "ReplicaSet" :name "app-1a"}
                                 :containers [{:name "app" :image "registry.example.org/app:2.0"}])
                            (pod "web" "job-1" :owner {:kind "Job" :name "job"}
                                 :containers [{:name "app" :image "registry.example.org/app:2.0"}])
                            (pod "web" "db-0" :owner {:kind "StatefulSet" :name "db"}
                                 :labels {:controller-revision-hash "db-r1"}
                                 :containers [{:name "app" :image "registry.example.org/app:2.0"}])
                            (pod "web" "agent-x" :owner {:kind "DaemonSet" :name "agent"}
                                 :labels {:controller-revision-hash "r1"}
                                 :containers [{:name "app" :image "registry.example.org/app:2.0"}]))
              "replicasets" (items {:metadata (meta-of "web" "app-1a") :spec {:template (template)}})
              "statefulsets" (items {:metadata (meta-of "web" "db") :spec {:template (template)}
                                     :status {:updateRevision "db-r1"}})
              "daemonsets" (items {:metadata (meta-of "web" "agent") :spec {:template (template)}})
              "jobs" (items {:metadata (meta-of "web" "job") :spec {:template (template)}})
              "controllerrevisions" (items {:metadata (assoc (meta-of "web" "agent-r1")
                                                             :ownerReferences [{:kind "DaemonSet" :name "agent" :controller true}])
                                            :revision 1})}})

(def ^:private kubelet-apiserver-skew
  {:check (check-named upgrade/detectives "kubelet-apiserver-skew")
   :joins ["nodes" "server-version"]
   :unguarded :silent
   :answered {"nodes" (items {:metadata (meta-of "n1") :status {:nodeInfo {:kubeletVersion "v1.27.3"}}})
              "server-version" {:major "1" :minor "31"}}})

(def ^:private cnpg-primary-labels
  {:check (check-named cnpg/detectives "cnpg-primary-labels")
   :joins [cnpg/cluster-type "pods"]
   :unguarded :silent
   :answered {cnpg/cluster-type (items {:metadata (meta-of "db" "main")
                                        :status {:currentPrimary "main-1" :targetPrimary "main-1"
                                                 :phase "Cluster in healthy state"}})
              "pods" (items (pod "db" "main-1" :labels {:cnpg.io/cluster "main" :cnpg.io/podRole "instance"
                                                        :cnpg.io/instanceRole "replica"}))}})

(def ^:private cnpg-storage-pending
  {:check (check-named cnpg-storage/detectives "cnpg-storage-pending")
   :joins [cnpg/cluster-type "persistentvolumeclaims"]
   :unguarded :silent
   :answered {cnpg/cluster-type (items {:metadata (meta-of "db" "main") :spec {:storage {:size "10Gi"}}})
              "persistentvolumeclaims" (items {:metadata (assoc (meta-of "db" "main-1") :labels {:cnpg.io/cluster "main"})
                                               :spec {:resources {:requests {:storage "5Gi"}}}
                                               :status {:capacity {:storage "5Gi"}}})}})

(def ^:private pooler-pods
  (items (pod "db" "main-1" :labels {:cnpg.io/cluster "main" :cnpg.io/podRole "instance"} :created "2026-11-20T00:00:00Z")
         (pod "db" "pool-a" :labels {:cnpg.io/poolerName "pool" :pod-template-hash "a"} :created "2026-11-01T00:00:00Z")
         (pod "db" "pool-b" :labels {:cnpg.io/poolerName "pool" :pod-template-hash "b"} :created "2026-11-01T00:00:00Z")))

(def ^:private pooler-object
  (items {:metadata (meta-of "db" "pool") :spec {:cluster {:name "main"}}}))

(def ^:private cnpg-pooler-stale
  {:check (check-named pooler/detectives "cnpg-pooler-stale")
   :joins [pooler/pooler-type pooler/cluster-type "pods"]
   :unguarded :silent
   :answered {pooler/pooler-type pooler-object
              pooler/cluster-type (items {:metadata (meta-of "db" "main")
                                          :spec {:instances 1}
                                          :status {:phase "Cluster in healthy state" :readyInstances 1}})
              "pods" pooler-pods}})

(def ^:private cnpg-pooler-rollout
  {:check (check-named pooler/detectives "cnpg-pooler-rollout")
   :joins [pooler/pooler-type "pods"]
   :unguarded :silent
   :answered {pooler/pooler-type pooler-object "pods" pooler-pods}})

(def ^:private cnpg-pooler-resources
  {:check (check-named pooler/detectives "cnpg-pooler-resources")
   :joins [pooler/pooler-type "pods"]
   :unguarded :silent
   :answered {pooler/pooler-type pooler-object "pods" pooler-pods}})

(def ^:private succeeded-over-broken
  {:check (check-named flux/detectives "succeeded-over-broken")
   :joins [flux/helmrelease-type "pods"]
   :unguarded :silent
   :answered {flux/helmrelease-type (items {:metadata (meta-of "web" "app")
                                            :status {:conditions [{:type "Ready" :status "True"}]}})
              "pods" (items (pod "web" "app-1" :statuses [{:name "app" :restartCount 7
                                                           :state {:waiting {:reason "CrashLoopBackOff"}}}]))}})

(def ^:private cases
  [pod-startup rbac-admins rbac-namespace-grants rbac-secret-readers template-references storageclass-references
   hpa-target-references role-references webhook-references overcommit local-volumes isolated-services
   version-drift tls-secrets template-drift kubelet-apiserver-skew cnpg-primary-labels cnpg-storage-pending
   cnpg-pooler-stale cnpg-pooler-rollout cnpg-pooler-resources succeeded-over-broken])

(def ^:private consulting-cases
  "The cases whose check consults several reads of which any one is enough."
  (filterv :consults cases))

(defn- label [case] (:name (:check case)))

;; --- declarations -----------------------------------------------------------------

(deftest every-check-declares-the-reads-it-joins
  (doseq [{:keys [check joins] :as case} cases]
    (testing (label case)
      (is (= joins (:joins check)))
      (is (every? (set (:requires check)) joins) "every joined read is also required"))))

(deftest every-consulting-check-declares-the-reads-it-consults
  (is (= ["template-references"] (map label consulting-cases)))
  (doseq [{:keys [check consults joins] :as case} consulting-cases]
    (testing (label case)
      (is (= consults (:consults check)))
      (is (every? (set (:requires check)) consults) "every consulted read is also required")
      (is (not-any? (set joins) consults) "a consulted read is not also joined"))))

(deftest no-check-judges-its-subjects
  ;; an empty list of subjects is an answered read whose verdict is that
  ;; nothing is wrong, so none of these checks declares :judges
  (doseq [{:keys [check] :as case} cases]
    (testing (label case)
      (is (nil? (:judges check))))))

(deftest reads-judged-on-their-own-stay-unjoined
  ;; each of these reads is judged only when it answered, or only adds to a
  ;; hint, or is a type a cluster may not serve. Calico's policy types answer
  ;; an empty list where Calico is not installed, so isolated-services joins them
  (is (= ["services" "serviceaccounts" "secret-names" "namespaces"]
         (remove (set (:joins (:check template-references))) (:requires (:check template-references)))))
  (is (= [tls-secrets/gateway-type]
         (remove (set (:joins (:check tls-secrets))) (:requires (:check tls-secrets))))))

;; --- the table --------------------------------------------------------------------

(deftest answered-evidence-reports-something
  (doseq [{:keys [check answered] :as case} cases]
    (testing (label case)
      (let [{:keys [unread? findings]} (joins/judgement check answered)]
        (is (not unread?))
        (is (seq findings))
        (is (not-any? #(= "detective failed to run - investigate its input" (:summary %)) findings))))))

(deftest a-missing-joined-read-judges-nothing
  (doseq [{:keys [check joins answered] :as case} cases
          [missing evidence] (joins/missing-read-cases joins answered)]
    (testing (str (label case) ": " missing)
      (is (= {:unread? true :findings []} (joins/judgement check evidence)))
      (is (empty? ((:detect check) evidence)) "the detect function itself reports nothing"))))

(deftest an-empty-answered-read-is-still-judged
  ;; an empty list answers the read: the check is read, and judges what it has
  (doseq [{:keys [check joins answered] :as case} cases
          read joins
          :let [value (get answered read)]
          :when (or (map? value) (vector? value))]
    (testing (str (label case) ": " read " answered empty")
      (is (not (:unread? (joins/judgement check (assoc answered read (if (vector? value) [] (empty value))))))))))

;; --- the expected verdicts on answered evidence -----------------------------------

(defn- summaries [{:keys [check answered]}]
  (sort (map (juxt :component :summary) ((:detect check) answered))))

(deftest answered-evidence-reports-only-the-true-findings
  (is (= [["web/mounting" "a volume of the pod cannot be mounted"]
          ["web/stuck" "pod is bound to node n1 but has not started for over 30 minutes"]]
         (summaries pod-startup)))
  (is (= [["humans" "User alice holds cluster-admin power (via ClusterRole cluster-admin)"]
          ["ops" "Group ops holds cluster-admin power (via ClusterRole god)"]]
         (summaries rbac-admins)))
  (is (= [["web/editors" "User bob holds edit in this namespace"]
          ["web/gods" "User carol holds god in this namespace"]]
         (summaries rbac-namespace-grants)))
  (is (= [["readers" "User dave can read secrets cluster-wide (via ClusterRole secret-reader)"]]
         (summaries rbac-secret-readers)))
  (is (= [["web/a" "Deployment uses ServiceAccount ghost, which does not exist"]
          ["web/agent" "env DB_HOST points at Service data/db, which does not exist"]
          ["web/db" "StatefulSet pulls images with Secret ghost, which does not exist"]
          ["web/job" "CronJob uses ServiceAccount ghost, which does not exist"]]
         (summaries template-references)))
  (is (= [["web/data" "PersistentVolumeClaim uses StorageClass gone, which does not exist"]]
         (summaries storageclass-references)))
  (is (= [["web/app" "HorizontalPodAutoscaler targets Deployment gone, which does not exist"]
          ["web/db" "HorizontalPodAutoscaler targets StatefulSet gone, which does not exist"]]
         (summaries hpa-target-references)))
  (is (= [["global" "ClusterRoleBinding grants ClusterRole gone, which does not exist"]
          ["web/local" "RoleBinding grants Role gone, which does not exist"]]
         (summaries role-references)))
  (is (= [["inject" "MutatingWebhookConfiguration webhook mutate.example.org calls Service infra/gone, which does not exist"]
          ["policy" "ValidatingWebhookConfiguration webhook validate.example.org calls Service infra/gone, which does not exist"]]
         (summaries webhook-references)))
  (is (= [["cluster" "CPU requests (8.0 cores) exceed total allocatable (4.0 cores)"]] (summaries overcommit)))
  (is (= [["pv-local" "local volume is pinned to missing node(s): gone"]] (summaries local-volumes)))
  (is (= [["web/app" "Service's 1 ready backend(s) admit no ingress traffic (NetworkPolicy default-deny)"]]
         (summaries isolated-services)))
  (is (= [["example.org/widgets" "discovery advertises v1beta1 but the CRD serves only v1"]] (summaries version-drift)))
  (is (= [["infra/edge-tls" "TLS Secret holds no certificate that parses"]
          ["web/site-tls" "TLS Secret holds no certificate that parses"]]
         (summaries tls-secrets)))
  (is (= [["web/agent" "pods differ from the template they were created from"]
          ["web/app-1a" "pods differ from the template they were created from"]
          ["web/db" "pods differ from the template they were created from"]
          ["web/job" "pods differ from the template they were created from"]]
         (summaries template-drift)))
  (is (= [["nodes" "kubelet 1.27 trails the API server (1.31) by 4 minors, more than the 3 the skew policy allows"]]
         (summaries kubelet-apiserver-skew)))
  (is (= [["db/main" "the designated primary main-1 does not carry the primary role label"]]
         (summaries cnpg-primary-labels)))
  (is (= [["db/main-1" "the cluster asks for 10.0GiB and this volume requests 5.0GiB"]]
         (summaries cnpg-storage-pending)))
  (is (= [["db/pool" "2 of its pods predate every instance of cluster main"]] (summaries cnpg-pooler-stale)))
  (is (= [["db/pool" "2 ready pods across 2 generations"]] (summaries cnpg-pooler-rollout)))
  (is (= [["db/pool" "pods run without resource requests or limits"]] (summaries cnpg-pooler-resources)))
  (is (= [["web/app" "HelmRelease reports success while 1 pod(s) in its namespace cannot start"]]
         (summaries succeeded-over-broken))))

;; --- no missing joined read passes ------------------------------------------------

(def ^:private seed 20261125)
(def ^:private trials 500)

(defn- property-holds? [cases]
  (:pass? (tc/quick-check trials (joins/no-missing-joined-read-judges cases) :seed seed)))

(deftest no-missing-joined-read-yields-a-pass-or-a-finding
  (let [result (tc/quick-check trials (joins/no-missing-joined-read-judges cases) :seed seed)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :fail])))))

(deftest the-property-catches-a-check-that-forgets-its-join
  ;; positive control, per check: without :joins one answered read keeps the
  ;; check from being unread, so the property finds a counterexample
  (doseq [case cases]
    (testing (label case)
      (is (not (property-holds? [(update case :check dissoc :joins)]))))))

(deftest every-detect-function-consults-the-guard
  ;; positive control for the guard inside each detect function: told that
  ;; its joins did not answer, it reports nothing on evidence that otherwise
  ;; reports something
  (with-redefs [evidence/joined? (constantly false)]
    (doseq [{:keys [check answered] :as case} cases]
      (testing (label case)
        (is (empty? ((:detect check) answered)))))))

(deftest without-the-guard-only-a-silent-check-keeps-the-property
  ;; pins what each guard holds back: a :partial or :false-finding check
  ;; reports on a missing joined read once its guard is disabled, so the
  ;; property fails; a :silent check reports nothing either way, so its
  ;; :joins alone keeps it from passing
  (with-redefs [evidence/joined? (constantly true)]
    (doseq [{:keys [unguarded] :as case} cases]
      (testing (str (label case) " " unguarded)
        (is (= (= :silent unguarded) (boolean (property-holds? [case]))))))))

(deftest the-generated-evidence-is-not-vacuous
  ;; the same generator with every read answered reaches findings for every
  ;; check, so the property's silence is the joins at work
  (let [samples (gen/sample (joins/gen-answered cases) 2000)
        reporting (set (for [[check evidence] samples
                             :when (seq ((:detect check) evidence))]
                         (:name check)))]
    (is (= (set (map label cases)) reporting))))

;; --- consulted reads ----------------------------------------------------------------
;;
;; template-references judges each template against the ServiceAccount,
;; Secret, Service and namespace reads, each only when it answered. Any one of
;; them is enough to judge on, so none is joined; with none of them answered
;; there is nothing to judge a template against, and the check is unread.

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

(defn- summaries-on [{:keys [check]} evidence]
  (sort (map (juxt :component :summary) ((:detect check) evidence))))

(deftest a-consulted-read-answering-alone-reports-what-it-alone-judges
  ;; the Service read alone reports nothing: telling a namespace from a host
  ;; in an address needs the namespace read, and the namespace read alone has
  ;; no Services to look an address up in
  (let [{:keys [consults answered]} template-references
        alone (into {} (butlast (joins/consulted-read-cases consults answered)))]
    (is (= [] (summaries-on template-references (alone "only services answered"))))
    (is (= [["web/a" "Deployment uses ServiceAccount ghost, which does not exist"]
            ["web/job" "CronJob uses ServiceAccount ghost, which does not exist"]]
           (summaries-on template-references (alone "only serviceaccounts answered"))))
    (is (= [["web/db" "StatefulSet pulls images with Secret ghost, which does not exist"]]
           (summaries-on template-references (alone "only secret-names answered"))))
    (is (= [] (summaries-on template-references (alone "only namespaces answered"))))))

(deftest every-consulted-read-answered-empty-is-still-judged
  ;; an empty list answers the read, so the check is read and judges every
  ;; template against nothing: each named ServiceAccount and Secret is missing
  (let [{:keys [check answered]} template-references
        evidence (assoc answered "services" (items) "serviceaccounts" (items) "secret-names" [] "namespaces" (items))]
    (is (not (:unread? (joins/judgement check evidence))))
    (is (= [["web/a" "Deployment uses ServiceAccount ghost, which does not exist"]
            ["web/db" "StatefulSet pulls images with Secret ghost, which does not exist"]
            ["web/job" "CronJob uses ServiceAccount ghost, which does not exist"]]
           (summaries-on template-references evidence)))))

(deftest no-unconsulted-check-yields-a-pass-or-a-finding
  (let [result (tc/quick-check trials (joins/no-unconsulted-check-judges consulting-cases) :seed seed)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :fail])))))

(deftest the-property-catches-a-check-that-forgets-what-it-consults
  ;; positive control: without :consults the answered workload reads keep the
  ;; check from being unread, so it counts as read on no context at all
  (doseq [case consulting-cases]
    (testing (label case)
      (is (not (:pass? (tc/quick-check trials
                                       (joins/no-unconsulted-check-judges [(update case :check dissoc :consults)])
                                       :seed seed))))
      (is (= {:unread? false :findings []}
             (joins/judgement (dissoc (:check case) :consults) (joins/without (:answered case) (:consults case))))))))

(deftest every-consulting-detect-function-consults-the-guard
  ;; positive control for the guard inside the detect function: told that
  ;; none of its consulted reads answered, it reports nothing on evidence
  ;; that otherwise reports something
  (with-redefs [evidence/consulted? (constantly false)]
    (doseq [{:keys [check answered] :as case} consulting-cases]
      (testing (label case)
        (is (empty? ((:detect check) answered)))))))
