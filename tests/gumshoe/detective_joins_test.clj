;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detective-joins-test
  "Checks that correlate one read against another. A read that did not answer
   is nil, and each of these checks would read a nil side as objects that do
   not exist: a Service, a NetworkPolicy, a PodDisruptionBudget, a backup.
   So each declares the reads it joins as :joins, is unread when any of them
   is missing, and its detect function reports nothing then. Fixtures are
   synthetic, shaped as the API server returns the objects."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.cnpg-recoverability :as cnpg-recoverability]
            [gumshoe.detectives.csi :as csi]
            [gumshoe.detectives.dns :as dns]
            [gumshoe.detectives.gateway :as gateway]
            [gumshoe.detectives.network :as network]
            [gumshoe.detectives.posture :as posture]
            [gumshoe.detectives.rbac :as rbac]
            [gumshoe.detectives.references :as references]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.detectives.services :as services]
            [gumshoe.detectives.storage :as storage]
            [gumshoe.detectives.traefik :as traefik]
            [gumshoe.evidence :as evidence]
            [gumshoe.joins-support :as joins]
            [gumshoe.tools.jaas :as jaas]))

;; --- fixtures ---------------------------------------------------------------------

(def ^:private now (java.time.Instant/parse "2026-11-25T12:00:00Z"))
(def ^:private long-ago "2026-01-01T00:00:00Z")

(def ^:private items joins/items)
(def ^:private meta-of joins/meta-of)
(def ^:private check-named joins/check-named)

(defn- ready-pod [namespace name labels & {:as spec}]
  {:metadata (assoc (meta-of namespace name) :labels labels)
   :spec (merge {:containers [{:name "app" :image "registry.example.org/app:1.0"}]} spec)
   :status {:phase "Running" :conditions [{:type "Ready" :status "True"}]}})

(defn- service [namespace name & {:keys [selector ports]}]
  {:metadata (meta-of namespace name)
   :spec (cond-> {:type "ClusterIP" :ports (or ports [{:name "http" :port 80}])}
           selector (assoc :selector selector))})

(defn- workload [namespace name & {:keys [replicas labels containers service-account]}]
  {:metadata (meta-of namespace name)
   :spec {:replicas replicas
          :template {:metadata {:labels (or labels {:app name})}
                     :spec (cond-> {:containers (or containers [{:name "app" :image "registry.example.org/app:1.0"}])}
                             service-account (assoc :serviceAccountName service-account))}}})

;; --- the cases --------------------------------------------------------------------
;;
;; Each case: the check, the reads it must join, and answered evidence on which
;; it reports at least one finding. Every joined read is one whose absence the
;; check would read as missing objects, so the answered evidence also holds an
;; object that a missing read would turn into a false finding.

(def ^:private ingress-references
  {:check (check-named references/detectives "ingress-references")
   :joins ["ingresses" "services"]
   :answered {"ingresses" (items {:metadata (meta-of "web" "site")
                                  :spec {:rules [{:http {:paths [{:backend {:service {:name "app" :port {:number 80}}}}
                                                                 {:backend {:service {:name "gone" :port {:number 80}}}}]}}]}})
              "ingressclasses" (items)
              "services" (items (service "web" "app" :ports [{:port 80}]))
              "secret-names" []}})

(def ^:private traefik-references
  {:check (check-named traefik/detectives "traefik-references")
   :joins ["services" traefik/ingressroute-type traefik/middleware-type traefik/traefikservice-type]
   :answered {"services" (items (service "web" "app" :ports [{:port 80}]))
              traefik/ingressroute-type (items {:metadata (meta-of "web" "site")
                                                :spec {:routes [{:services [{:name "app" :port 80}
                                                                            {:name "gone" :port 80}
                                                                            {:name "split" :kind "TraefikService"}]
                                                                 :middlewares [{:name "auth"}]}]}})
              traefik/middleware-type (items {:metadata (meta-of "web" "auth") :spec {}})
              traefik/traefikservice-type (items {:metadata (meta-of "web" "split")
                                                  :spec {:weighted {:services [{:name "app" :port 80}]}}})}})

(def ^:private listenersets
  {:check (check-named gateway/detectives "listenersets")
   :joins [gateway/gateway-type]
   :answered {gateway/gateway-type (items {:metadata (meta-of "infra" "edge")})
              "listenersets.gateway.networking.k8s.io"
              (items {:metadata (meta-of "web" "site") :spec {:parentRef {:namespace "infra" :name "edge"}}}
                     {:metadata (meta-of "web" "lost") :spec {:parentRef {:namespace "infra" :name "gone"}}})
              "xlistenersets.gateway.networking.x-k8s.io" nil}})

(def ^:private jsonnetlibrary-refs
  {:check (check-named jaas/detectives "jsonnetlibrary-refs")
   :joins [jaas/jsonnetsnippet-type jaas/jsonnetlibrary-type]
   :answered {jaas/jsonnetsnippet-type (items {:metadata (meta-of "app" "render")
                                               :spec {:libraries [{:name "lib"} {:name "gone"}]}})
              jaas/jsonnetlibrary-type (items {:metadata (meta-of "app" "lib")})}})

(def ^:private dns-nameservers
  {:check (check-named dns/detectives "dns-nameservers")
   :joins ["nameservers" "addresses"]
   :answered {"zone" "example.org"
              "nameservers" ["ns1.example.org" "ns2.example.org"]
              "addresses" {"ns1.example.org" {:a ["192.0.2.1"] :aaaa ["2001:db8::1"]}
                           "ns2.example.org" {:a ["192.0.2.2"] :aaaa []}}}})

(def ^:private services-check
  {:check (check-named services/detectives "services")
   :joins ["services" "pods" "deployments" "statefulsets"]
   :answered {"services" (items (service "web" "app" :selector {:app "app"})
                                (service "web" "idle" :selector {:app "idle"})
                                (service "web" "db" :selector {:app "db"})
                                (service "web" "broken" :selector {:app "nothing"}))
              "pods" (items (ready-pod "web" "app-1" {:app "app"}))
              "deployments" (items (workload "web" "idle" :replicas 0))
              "statefulsets" (items (workload "web" "db" :replicas 0))}})

(def ^:private network-policies
  {:check (check-named network/detectives "network-policies")
   :joins ["networkpolicies" "pods" network/global-policy-type network/calico-policy-type]
   :answered {"networkpolicies" (items {:metadata (meta-of "web" "default-deny") :spec {:podSelector {}}})
              "pods" (items (ready-pod "web" "app-1" {:app "app"}) (ready-pod "api" "api-1" {:app "api"}))
              network/global-policy-type (items)
              network/calico-policy-type (items)}})

(def ^:private csi-drivers
  {:check (check-named csi/detectives "csi-drivers")
   :joins ["csidrivers" "csinodes" "nodes"]
   :answered {"csidrivers" (items {:metadata (meta-of "rbd.csi.ceph.com")})
              "csinodes" (items {:metadata (meta-of "n1") :spec {:drivers [{:name "rbd.csi.ceph.com"}]}}
                                {:metadata (meta-of "n2") :spec {:drivers []}})
              "nodes" (items {:metadata (meta-of "n1")} {:metadata (meta-of "n2")})}})

(defn- claim [namespace name phase & {:keys [class]}]
  {:metadata (assoc (meta-of namespace name) :creationTimestamp long-ago)
   :spec (cond-> {} class (assoc :storageClassName class))
   :status {:phase phase}})

(def ^:private storage-check
  {:check (check-named storage/detectives "storage")
   :joins ["persistentvolumeclaims" "pods" "storageclasses"]
   :answered {:now now
              "persistentvolumeclaims" (items (claim "web" "data" "Bound")
                                              (claim "web" "wait" "Pending" :class "lazy")
                                              (claim "web" "orphan" "Bound"))
              "pods" (items (ready-pod "web" "app-1" {:app "app"}
                                       :nodeName "n1"
                                       :volumes [{:name "data" :persistentVolumeClaim {:claimName "data"}}]))
              "storageclasses" (items {:metadata (meta-of "lazy") :volumeBindingMode "WaitForFirstConsumer"})
              "events" (items)}})

(def ^:private rbac-default-tokens
  {:check (check-named rbac/detectives "rbac-default-tokens")
   :joins ["pods" "serviceaccounts"]
   :answered {"pods" (items (ready-pod "web" "a" {:app "a"}) (ready-pod "quiet" "b" {:app "b"}))
              "serviceaccounts" (items {:metadata (meta-of "quiet" "default") :automountServiceAccountToken false})}})

(def ^:private single-replicas
  {:check (check-named posture/detectives "single-replicas")
   :joins ["deployments" "statefulsets" "horizontalpodautoscalers"]
   :answered {"deployments" (items (workload "web" "solo" :replicas 1) (workload "web" "scaled" :replicas 1))
              "statefulsets" (items (workload "web" "db" :replicas 1))
              "horizontalpodautoscalers" (items {:metadata (meta-of "web" "scaled")
                                                 :spec {:scaleTargetRef {:kind "Deployment" :name "scaled"} :minReplicas 3}})}})

(def ^:private unprotected-workloads
  {:check (check-named posture/detectives "unprotected-workloads")
   :joins ["deployments" "statefulsets" "poddisruptionbudgets"]
   :answered {"deployments" (items (workload "web" "covered" :replicas 3) (workload "web" "bare" :replicas 2))
              "statefulsets" (items (workload "web" "db" :replicas 3))
              "poddisruptionbudgets" (items {:metadata (meta-of "web" "covered")
                                             :spec {:selector {:matchLabels {:app "covered"}}}})}})

(def ^:private bare-container [{:name "app" :image "registry.example.org/app:1.0"}])

(def ^:private missing-resources
  {:check (check-named posture/detectives "missing-resources")
   :joins ["deployments" "statefulsets" "daemonsets" "cronjobs" "limitranges"]
   :answered {"deployments" (items (workload "web" "bare" :containers bare-container)
                                   (workload "limited" "defaulted" :containers bare-container))
              "statefulsets" (items (workload "web" "db" :containers bare-container))
              "daemonsets" (items (workload "web" "agent" :containers bare-container))
              "cronjobs" (items)
              "limitranges" (items {:metadata (meta-of "limited" "defaults")
                                    :spec {:limits [{:type "Container"
                                                     :default {:cpu "500m" :memory "256Mi"}
                                                     :defaultRequest {:cpu "100m" :memory "128Mi"}}]}})}})

(def ^:private default-account-tokens
  {:check (check-named posture/detectives "default-account-tokens")
   :joins ["deployments" "statefulsets" "daemonsets" "cronjobs" "serviceaccounts"]
   :answered {"deployments" (items (workload "web" "a") (workload "quiet" "b"))
              "statefulsets" (items (workload "web" "db"))
              "daemonsets" (items (workload "web" "agent"))
              "cronjobs" (items)
              "serviceaccounts" (items {:metadata (meta-of "quiet" "default") :automountServiceAccountToken false})}})

(defn- cnpg-cluster [namespace name]
  {:metadata (meta-of namespace name)
   :spec {:backup {:barmanObjectStore {:destinationPath "s3://backups/"}}}})

(def ^:private cnpg-recoverability
  {:check (check-named cnpg-recoverability/detectives "cnpg-recoverability")
   :joins [cnpg/cluster-type cnpg/backup-type]
   :answered {cnpg/cluster-type (items (cnpg-cluster "db" "main") (cnpg-cluster "db" "fresh"))
              cnpg/backup-type (items {:metadata (meta-of "db" "main-1")
                                       :spec {:cluster {:name "main"}}
                                       :status {:phase "completed"}})}})

(def ^:private cases
  [ingress-references traefik-references listenersets jsonnetlibrary-refs dns-nameservers services-check
   network-policies csi-drivers storage-check rbac-default-tokens single-replicas unprotected-workloads
   missing-resources default-account-tokens cnpg-recoverability])

(defn- label [case] (:name (:check case)))

;; --- declarations -----------------------------------------------------------------

(deftest every-check-declares-the-reads-it-joins
  (doseq [{:keys [check joins] :as case} cases]
    (testing (label case)
      (is (= joins (:joins check)))
      (is (every? (set (:requires check)) joins) "every joined read is also required"))))

(deftest no-check-judges-its-subjects
  ;; an empty list of subjects is an answered read whose verdict is that
  ;; nothing is wrong, so none of these checks declares :judges
  (doseq [{:keys [check] :as case} cases]
    (testing (label case)
      (is (nil? (:judges check))))))

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
          :when (map? (get answered read))]
    (testing (str (label case) ": " read " answered empty")
      (is (not (:unread? (joins/judgement check (assoc answered read {:items []}))))))))

;; --- the expected verdicts on answered evidence -----------------------------------

(defn- summaries [{:keys [check answered]}]
  (sort (map (juxt :component :summary) ((:detect check) answered))))

(deftest answered-evidence-reports-only-the-true-findings
  (is (= [["web/site" "Ingress backend Service gone does not exist"]] (summaries ingress-references)))
  (is (= [["web/site" "IngressRoute uses Service web/gone, which does not exist"]] (summaries traefik-references)))
  (is (= [["web/lost" "ListenerSet points at Gateway infra/gone which does not exist"]] (summaries listenersets)))
  (is (= [["app/render" "JsonnetLibrary app/gone is missing"]] (summaries jsonnetlibrary-refs)))
  (is (= [["ns2.example.org" "nameserver has no AAAA record"]] (summaries dns-nameservers)))
  (is (= [["web/broken" "Service selector matches no pods"]
          ["web/db" "Service has no pods: StatefulSet db is scaled to 0"]
          ["web/idle" "Service has no pods: Deployment idle is scaled to 0"]]
         (summaries services-check)))
  (is (= [["api" "namespace runs pods without any NetworkPolicy - all traffic is allowed"]] (summaries network-policies)))
  (is (= [["rbd.csi.ceph.com" "driver missing on schedulable node(s): n2"]] (summaries csi-drivers)))
  (is (= [["web/orphan" "PersistentVolumeClaim is not used by any pod"]
          ["web/wait" "PersistentVolumeClaim is not used by any pod"]
          ["web/wait" "PersistentVolumeClaim waits for its first consumer (StorageClass lazy)"]]
         (summaries storage-check)))
  (is (= [["web" "1 pod(s) run with the default ServiceAccount token mounted"]] (summaries rbac-default-tokens)))
  (is (= [["web/db" "StatefulSet runs a single replica"] ["web/solo" "Deployment runs a single replica"]]
         (summaries single-replicas)))
  (is (= [["web/bare" "Deployment with 2 replicas has no PodDisruptionBudget"]
          ["web/db" "StatefulSet with 3 replicas has no PodDisruptionBudget"]]
         (summaries unprotected-workloads)))
  (is (= [["web/agent" "DaemonSet containers lack resource settings: app (cpu request, memory request, memory limit)"]
          ["web/bare" "Deployment containers lack resource settings: app (cpu request, memory request, memory limit)"]
          ["web/db" "StatefulSet containers lack resource settings: app (cpu request, memory request, memory limit)"]]
         (summaries missing-resources)))
  (is (= [["web/a" "Deployment mounts the default ServiceAccount's token"]
          ["web/agent" "DaemonSet mounts the default ServiceAccount's token"]
          ["web/db" "StatefulSet mounts the default ServiceAccount's token"]]
         (summaries default-account-tokens)))
  (is (= [["db/fresh" "a backup destination is configured but no backup has completed (0 attempted)"]]
         (summaries cnpg-recoverability))))

;; --- no missing joined read passes ------------------------------------------------

(def ^:private seed 20261125)
(def ^:private trials 500)

(deftest no-missing-joined-read-yields-a-pass-or-a-finding
  (let [result (tc/quick-check trials (joins/no-missing-joined-read-judges cases) :seed seed)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :fail])))))

(deftest the-property-catches-a-check-that-forgets-its-join
  ;; positive control, per check: without :joins one answered read keeps the
  ;; check from being unread, so the property finds a counterexample
  (doseq [case cases]
    (testing (label case)
      (is (not (:pass? (tc/quick-check trials (joins/no-missing-joined-read-judges [(update case :check dissoc :joins)])
                                       :seed seed)))))))

(deftest the-property-catches-a-detect-function-that-judges-a-missing-read
  ;; positive control for the guard inside each detect function: with it
  ;; disabled, the detect function reports on a missing joined read
  (with-redefs [evidence/joined? (constantly true)]
    (doseq [case cases]
      (testing (label case)
        (is (not (:pass? (tc/quick-check trials (joins/no-missing-joined-read-detects [case]) :seed seed))))))))

(deftest run-detectives-holds-back-an-unread-check-whose-guard-is-disabled
  ;; the engine runs no unread check, so the property holds even with every
  ;; guard inside the detect functions disabled
  (with-redefs [evidence/joined? (constantly true)]
    (is (:pass? (tc/quick-check trials (joins/no-missing-joined-read-judges cases) :seed seed)))))

(deftest the-generated-evidence-is-not-vacuous
  ;; the same generator with every read answered reaches findings for every
  ;; check, so the property's silence is the joins at work
  (let [samples (gen/sample (joins/gen-answered cases) 1000)
        reporting (set (for [[check evidence] samples
                             :when (seq ((:detect check) evidence))]
                         (:name check)))]
    (is (= (set (map label cases)) reporting))))

;; --- the guard and the registry ---------------------------------------------------

(deftest joined-holds-only-when-every-read-answered
  (is (evidence/joined? {"a" {:items []} "b" []} ["a" "b"]) "an empty answer is an answer")
  (is (not (evidence/joined? {"a" {:items []} "b" nil} ["a" "b"])))
  (is (not (evidence/joined? {"a" {:items []}} ["a" "b"])) "an absent key did not answer")
  (is (evidence/joined? {} []) "joining nothing needs nothing"))

(deftest every-registered-join-is-also-required
  ;; a joined read that is not required would never be collected, and the
  ;; check would be unread on every cluster
  (doseq [{:keys [name joins requires]} (registry/all)
          :when joins]
    (testing name
      (is (every? (set requires) joins)))))
