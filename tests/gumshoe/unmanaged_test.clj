;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.unmanaged-test
  "The unmanaged-objects detective: discovery parsing, the inventory against
   a fake cluster built from a namespace fixture, every built-in rule, caller
   rules, distinctness across API groups, and the reads that did not answer,
   in whole and in part."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.unmanaged :as u]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.reads :as reads]
            [gumshoe.shell :as shell]
            [gumshoe.unmanaged :as collect]))

(def ^:private fixture-dir "tests/fixtures/unmanaged/")
(def ^:private api-resources (slurp (str fixture-dir "api-resources.txt")))
(def ^:private partial-err (slurp (str fixture-dir "api-resources-partial.err")))
(def ^:private down-err (slurp (str fixture-dir "api-resources-down.err")))
(def ^:private shop (json/parse-string (slurp (str fixture-dir "shop.json")) true))

(defn- ok [out] {:exit 0 :out out :err ""})
(def ^:private forbidden
  {:exit 1 :out "" :err "Error from server (Forbidden): secrets is forbidden: User \"auditor\" cannot list resource \"secrets\" in API group \"\" in the namespace \"shop\""})

(defn- type-of-list-argv
  "The type and namespace of a listing argv, or nil."
  [argv]
  (let [[_ _ _ verb type & more] argv]
    (when (and (= "get" verb) (some #(str/starts-with? % "--namespace=") more))
      [type (subs (first (filter #(str/starts-with? % "--namespace=") more)) (count "--namespace="))])))

(defn- fake-cluster
  "A shell/execute stub answering the inventory's reads from a fixture.
   :discovery is the api-resources result, :namespace the Namespace object
   (nil: absent, :fail: the read fails), :lists {type [objects]}, :fail
   {type result}. Every argv is recorded in :calls."
  [{:keys [discovery namespace lists fail]}]
  (let [calls (atom [])]
    {:calls calls
     :execute (fn [& argv]
                (swap! calls conj (vec argv))
                (let [argv (vec argv)]
                  (cond
                    (some #{"api-resources"} argv) discovery

                    (= ["get" "namespaces"] (subvec argv 3 5))
                    (cond (= :fail namespace) {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 192.0.2.10:6443: i/o timeout"}
                          (nil? namespace) (ok "")
                          :else (ok (json/generate-string namespace)))

                    :else
                    (if-let [[type ns] (type-of-list-argv argv)]
                      (or (get fail type)
                          (ok (json/generate-string {:apiVersion "v1" :kind "List"
                                                     :items (vec (for [o (get lists type) :when (= ns (get-in o [:metadata :namespace]))] o))})))
                      {:exit 1 :out "" :err (str "unexpected command " argv)}))))}))

(def ^:private shop-cluster
  {:discovery (ok api-resources)
   :namespace (:namespace shop)
   :lists (into {} (map (fn [[k v]] [(name k) v])) (:lists shop))})

(defn- inventory-of
  ([cluster] (inventory-of cluster {}))
  ([cluster opts]
   (let [{:keys [execute calls]} (fake-cluster cluster)]
     (with-redefs [shell/execute execute]
       {:inventory (collect/inventory "ctx" "shop" opts) :calls @calls}))))

(defn- by-name
  [result]
  (into {} (for [v (:objects result)] [(u/describe (:object v)) (dissoc v :object)])))

;; --- discovery ----------------------------------------------------------------

(deftest discovery-rows-are-read-from-the-edges
  (let [rows (collect/parse-types api-resources)
        by-type (into {} (map (juxt :type identity)) rows)]
    (is (= 36 (count rows)))
    (testing "core types are listed by their plural, grouped ones as plural.group"
      (is (= {:type "pods" :group "" :version "v1" :kind "Pod"} (by-type "pods")))
      (is (= {:type "deployments.apps" :group "apps" :version "v1" :kind "Deployment"} (by-type "deployments.apps")))
      (is (= "PodMetrics" (:kind (by-type "pods.metrics.k8s.io")))))
    (testing "a row without short names parses like one with them"
      (is (= "Secret" (:kind (by-type "secrets"))))
      (is (= "NetworkPolicy" (:kind (by-type "networkpolicies.crd.projectcalico.org")))))
    (testing "a row not marked namespaced is dropped"
      (is (empty? (collect/parse-types "nodes   no   v1   false   Node"))))))

(deftest discovery-is-answered-partial-or-unread
  (is (= :answered (:state (collect/classify-discovery (ok api-resources)))))
  (let [partial (collect/classify-discovery {:exit 1 :out api-resources :err partial-err})]
    (is (= :partial (:state partial)))
    (is (= ["metrics.k8s.io/v1beta1" "snapshot.storage.k8s.io/v1"] (:failed partial)))
    (is (= 36 (count (:types partial)))))
  (testing "a failure naming no group may have hidden any type"
    (is (= :unread (:state (collect/classify-discovery {:exit 1 :out "" :err down-err}))))
    (is (= :unread (:state (collect/classify-discovery {:exit 1 :out api-resources :err "error: You must be logged in to the server (Unauthorized)"})))))
  (testing "a clean exit listing nothing is no answer either"
    (is (= :unread (:state (collect/classify-discovery (ok "")))))))

(deftest a-url-in-stderr-is-not-a-failed-group
  (is (= [] (collect/failed-group-versions down-err)))
  (is (= [] (collect/failed-group-versions "Get \"https://192.0.2.10:6443/apis/metrics.k8s.io/v1beta1?timeout=32s\": EOF"))))

;; --- inventory ----------------------------------------------------------------

(deftest the-inventory-lists-every-type-once-and-skips-the-excluded
  (let [{:keys [inventory calls]} (inventory-of shop-cluster)
        listed (keep (comp first type-of-list-argv) calls)]
    (testing "each type is listed exactly once"
      (is (every? #(= 1 %) (vals (frequencies listed))))
      (is (= 31 (count listed))))
    (testing "the excluded types are never read, and the report says why"
      (is (not-any? #{"events" "events.events.k8s.io" "pods.metrics.k8s.io"
                      "networkpolicies.crd.projectcalico.org" "networksets.crd.projectcalico.org"}
                    listed))
      (is (= {"events" "events" "events.events.k8s.io" "events" "pods.metrics.k8s.io" "metrics"
              "networkpolicies.crd.projectcalico.org" "calico-storage" "networksets.crd.projectcalico.org" "calico-storage"}
             (into {} (map (juxt :type :exclusion)) (:excluded inventory))))
      (is (every? (comp not str/blank? :reason) (:excluded inventory))))
    (is (every? #(not= :unread (:state %)) (:types inventory)))))

(deftest an-object-keeps-its-metadata-and-never-its-payload
  (let [{:keys [inventory]} (inventory-of shop-cluster)
        objects (into {} (map (juxt u/describe identity)) (:objects inventory))
        secret (objects "Secret/db-credentials")
        applied (objects "ConfigMap/leftover-settings")]
    (is (nil? (:data secret)))
    (is (= "Opaque" (:type secret)))
    (is (not (str/includes? (pr-str (:objects inventory)) "Z3Vtc2hvZS1zZW50aW5lbA==")))
    (is (= "<omitted>" (get-in applied [:metadata :annotations :kubectl.kubernetes.io/last-applied-configuration])))
    (is (not (str/includes? (pr-str applied) "FEATURE")))
    (is (= {:selector {:app.kubernetes.io/instance "web" :app.kubernetes.io/name "web"}} (:spec (objects "Service/web"))))))

(deftest objects-are-distinct-by-uid-across-api-groups
  (let [event {:apiVersion "v1" :kind "Event" :metadata {:name "web.1" :namespace "shop" :uid "ev-1"}}
        cluster (-> shop-cluster
                    (assoc-in [:lists "events"] [event])
                    (assoc-in [:lists "events.events.k8s.io"] [(assoc event :apiVersion "events.k8s.io/v1")]))
        {:keys [inventory]} (inventory-of cluster {:exclusions []})
        events (filter #(= "Event" (:kind %)) (:objects inventory))]
    (testing "one stored object served by two groups is one object"
      (is (= 1 (count events)))
      (is (= ["events" "events.events.k8s.io"] (:served-as (first events))))))
  (testing "two objects of one kind and name with different uids stay apart"
    (let [{:keys [inventory]} (inventory-of shop-cluster)
          policies (filter #(= ["NetworkPolicy" "deny-all"] [(:kind %) (get-in % [:metadata :name])]) (:objects inventory))]
      (is (= #{"networking.k8s.io/v1" "projectcalico.org/v3"} (set (map :apiVersion policies))))))
  (testing "an object without a uid is distinct by kind and name"
    (is (= 1 (count (collect/distinct-objects [["a" {:kind "PodMetrics" :metadata {:name "p"}}]
                                                ["b" {:kind "PodMetrics" :metadata {:name "p"}}]]))))))

;; --- the audit of the fixture namespace ----------------------------------------

(def ^:private shop-verdicts
  {"ConfigMap/kube-root-ca.crt" ["root-ca-configmap" :cluster]
   "ConfigMap/web-config" ["helm" :helm]
   "ConfigMap/leftover-settings" [nil :unowned]
   "Secret/sh.helm.release.v1.web.v3" ["helm-release-storage" :helm]
   "Secret/default-token-x7k2q" ["legacy-token-secret" :cluster]
   "Secret/ci-deployer-token" [nil :unowned]
   "Secret/db-credentials" ["flux" :gitops]
   "ServiceAccount/default" ["default-serviceaccount" :cluster]
   "ServiceAccount/web" ["helm" :helm]
   "Service/web" ["helm" :helm]
   "Service/legacy-db" ["flux" :gitops]
   "Endpoints/web" ["endpoints-of-service" :cluster]
   "Endpoints/legacy-db" [nil :unowned]
   "Endpoints/old-api" [nil :unowned]
   "EndpointSlice.discovery.k8s.io/web-7mx2p" ["owner-reference" :controller]
   "Deployment.apps/web" ["helm" :helm]
   "Deployment.apps/old-worker" [nil :unowned]
   "ReplicaSet.apps/web-6f8b9c7d5" ["owner-reference" :controller]
   "ReplicaSet.apps/old-worker-5c6b8d" ["owner-reference" :controller]
   "Pod/web-6f8b9c7d5-qz4lm" ["owner-reference" :controller]
   "Pod/old-worker-5c6b8d-r8t2n" ["owner-reference" :controller]
   "Pod/batch-1a2b3c-xk9p0" ["owner-reference" :unowned]
   "NetworkPolicy.networking.k8s.io/deny-all" ["flux" :gitops]
   "Kustomization.kustomize.toolkit.fluxcd.io/shop-apps" ["flux" :gitops]})

(deftest every-object-of-the-fixture-gets-its-verdict
  (let [result (u/audit (:inventory (inventory-of shop-cluster)) u/default-rules)
        verdicts (into {} (for [v (:objects result)
                                :let [o (:object v)]
                                :when (not= "projectcalico.org/v3" (:apiVersion o))]
                            [(u/describe o) [(:rule v) (:class v)]]))]
    (is (= shop-verdicts verdicts))
    (testing "the Calico policy of the same name is judged on its own"
      (is (= [:unowned] (for [v (:objects result) :when (= "projectcalico.org/v3" (:apiVersion (:object v)))] (:class v)))))
    (is (not (u/clean? result)))
    (is (u/complete? result))))

(deftest owners-are-named
  (let [verdicts (by-name (u/audit (:inventory (inventory-of shop-cluster)) u/default-rules))]
    (is (= "Helm release shop/web" (:owner (verdicts "ConfigMap/web-config"))))
    (is (= "Helm release storage of web" (:owner (verdicts "Secret/sh.helm.release.v1.web.v3"))))
    (is (= "Flux Kustomization flux-system/shop" (:owner (verdicts "Secret/db-credentials"))))
    (is (= "Deployment/web (ownerReference)" (:owner (verdicts "ReplicaSet.apps/web-6f8b9c7d5"))))
    (is (= "endpoints controller, from Service web" (:owner (verdicts "Endpoints/web"))))
    (is (str/includes? (:owner (verdicts "Secret/default-token-x7k2q")) "ServiceAccount default"))))

(deftest an-unowned-root-counts-what-follows-from-it
  (let [verdicts (by-name (u/audit (:inventory (inventory-of shop-cluster)) u/default-rules))]
    (is (= 2 (:dependents (verdicts "Deployment.apps/old-worker"))))
    (is (= 2 (:dependents (verdicts "Deployment.apps/web"))))
    (is (nil? (:dependents (verdicts "ConfigMap/leftover-settings"))))))

(deftest unowned-reasons-say-why
  (let [verdicts (by-name (u/audit (:inventory (inventory-of shop-cluster)) u/default-rules))]
    (is (str/includes? (:reason (verdicts "ConfigMap/leftover-settings")) "kubectl apply wrote it"))
    (is (str/includes? (:reason (verdicts "Endpoints/old-api")) "its Service is gone"))
    (is (str/includes? (:reason (verdicts "Endpoints/legacy-db")) "selects no pods"))
    (is (str/includes? (:reason (verdicts "Pod/batch-1a2b3c-xk9p0")) "ReplicaSet/batch-1a2b3c, which is no longer in the namespace"))))

;; --- the rules one by one ------------------------------------------------------

(defn- obj
  [kind name & {:keys [api-version labels annotations owners uid type spec]}]
  (cond-> {:apiVersion (or api-version "v1") :kind kind
           :metadata (cond-> {:name name :namespace "shop" :uid (or uid (str kind "-" name))}
                       labels (assoc :labels labels)
                       annotations (assoc :annotations annotations)
                       owners (assoc :ownerReferences owners))}
    type (assoc :type type)
    spec (assoc :spec spec)))

(defn- judged
  "The verdict on object within an inventory of objects whose types answered
   as types-state ({[group kind] state}, default every kind :present)."
  ([object objects] (judged object objects {}))
  ([object objects kinds]
   (let [types (concat (for [[[g k] s] kinds] {:group g :kind k :state s})
                       (for [o (cons object objects)] {:group (u/group-of (:apiVersion o)) :kind (:kind o) :state :present}))]
     (u/classify u/default-rules (u/facts {:namespace "shop" :objects (cons object objects) :types types}) object))))

(deftest an-owner-reference-wins-over-labels
  (let [rs (obj "ReplicaSet" "web-1" :api-version "apps/v1" :uid "rs-1")
        helm-labels {:app.kubernetes.io/managed-by "Helm"}
        pod (obj "Pod" "web-1-a" :labels helm-labels
                 :owners [{:apiVersion "apps/v1" :kind "ReplicaSet" :name "web-1" :uid "rs-1" :controller true}])]
    (testing "with its owner present, the object follows the owner, not its Helm label"
      (is (= :controller (:class (judged pod [rs])))))
    (testing "with its owner gone, the inherited Helm label does not save it"
      (is (= {:class :unowned :rule "owner-reference"} (select-keys (judged pod [] {["apps" "ReplicaSet"] :absent}) [:class :rule]))))
    (testing "with its owner's type unread, it is undetermined"
      (is (= :undetermined (:class (judged pod [] {["apps" "ReplicaSet"] :unread})))))
    (testing "an owner no namespaced type serves is accepted by the reference"
      (let [lease (obj "Lease" "worker-1" :api-version "coordination.k8s.io/v1"
                       :owners [{:apiVersion "v1" :kind "Node" :name "worker-1" :uid "node-1"}])]
        (is (= :controller (:class (judged lease []))))))
    (testing "the controller reference is preferred over the first one"
      (is (= "b" (:name (u/controller-reference {:metadata {:ownerReferences [{:name "a"} {:name "b" :controller true}]}})))))))

(deftest endpoints-belong-to-the-endpoints-controller-only-for-a-selecting-service
  (let [ep (obj "Endpoints" "api")
        selecting (obj "Service" "api" :spec {:selector {:app "api"}})
        bare (obj "Service" "api" :spec {})]
    (is (= "endpoints-of-service" (:rule (judged ep [selecting]))))
    (is (= :unowned (:class (judged ep [bare]))))
    (is (= :unowned (:class (judged ep []))))
    (is (= :undetermined (:class (judged ep [] {["" "Service"] :unread}))))
    (testing "an Endpoints-like object of another group is not judged by the rule"
      (is (= :unowned (:class (judged (obj "Endpoints" "api" :api-version "example.org/v1") [selecting])))))))

(deftest objects-the-control-plane-writes-are-accepted-by-name
  (is (= "root-ca-configmap" (:rule (judged (obj "ConfigMap" "kube-root-ca.crt") []))))
  (is (= "default-serviceaccount" (:rule (judged (obj "ServiceAccount" "default") []))))
  (is (= :unowned (:class (judged (obj "ServiceAccount" "deployer") []))))
  (testing "a legacy token Secret only in the token controller's naming"
    (let [token (fn [name account] (obj "Secret" name :type "kubernetes.io/service-account-token"
                                        :annotations {:kubernetes.io/service-account.name account}))]
      (is (= "legacy-token-secret" (:rule (judged (token "builder-token-abc12" "builder") []))))
      (is (= :unowned (:class (judged (token "builder-token" "builder") []))))
      (is (= :unowned (:class (judged (token "other-token-abc12" "builder") []))))
      (is (= :unowned (:class (judged (assoc (token "builder-token-abc12" "builder") :type "Opaque") []))))))
  (testing "the kubernetes Service only in namespace default"
    (let [svc (obj "Service" "kubernetes")]
      (is (= :unowned (:class (judged svc []))))
      (is (= "kubernetes-service"
             (:rule (u/classify u/default-rules (u/facts {:namespace "default" :objects [svc] :types []}) svc)))))))

(deftest helm-is-recognised-by-annotation-label-or-release-storage
  (is (= "Helm release shop/web" (u/accepts-helm-release-object
                                  (obj "ConfigMap" "c" :labels {:app.kubernetes.io/managed-by "Helm"}
                                       :annotations {:meta.helm.sh/release-name "web" :meta.helm.sh/release-namespace "shop"}) nil)))
  (testing "a partial stamp is accepted and says what it lacks"
    (is (= "Helm release shop/web (partial stamp: no app.kubernetes.io/managed-by=Helm label)"
           (u/accepts-helm-release-object
            (obj "ConfigMap" "c" :annotations {:meta.helm.sh/release-name "web" :meta.helm.sh/release-namespace "shop"}) nil)))
    (is (= "Helm release web (partial stamp: no release-namespace annotation)"
           (u/accepts-helm-release-object
            (obj "ConfigMap" "c" :labels {:app.kubernetes.io/managed-by "Helm"} :annotations {:meta.helm.sh/release-name "web"}) nil))))
  (is (str/includes? (u/accepts-helm-release-object (obj "ConfigMap" "c" :labels {:app.kubernetes.io/managed-by "Helm"
                                                                                  :app.kubernetes.io/instance "web"}) nil)
                     "label app.kubernetes.io/managed-by=Helm only"))
  (is (nil? (u/accepts-helm-release-object (obj "ConfigMap" "c" :labels {:app.kubernetes.io/managed-by "Helmfile"}) nil)))
  (testing "owner=helm marks release storage only on a release Secret or a ConfigMap"
    (is (some? (u/accepts-helm-release-storage (obj "Secret" "s" :type "helm.sh/release.v1" :labels {:owner "helm" :name "web"}) nil)))
    (is (some? (u/accepts-helm-release-storage (obj "ConfigMap" "s" :labels {:owner "helm" :name "web"}) nil)))
    (is (nil? (u/accepts-helm-release-storage (obj "Secret" "s" :type "Opaque" :labels {:owner "helm"}) nil)))
    (is (nil? (u/accepts-helm-release-storage (obj "Deployment" "s" :labels {:owner "helm"}) nil)))))

(deftest gitops-controllers-are-recognised-by-their-marks
  (is (= "Flux ResourceSet flux-system/shop"
         (u/accepts-flux (obj "ConfigMap" "c" :labels {:resourceset.fluxcd.controlplane.io/name "shop"
                                                      :resourceset.fluxcd.controlplane.io/namespace "flux-system"}) nil)))
  (is (str/starts-with? (u/accepts-flux (obj "ConfigMap" "c" :annotations {:kustomize.toolkit.fluxcd.io/prune "disabled"}) nil)
                        "Flux Kustomization (a kustomize.toolkit.fluxcd.io/ key)"))
  (is (nil? (u/accepts-flux (obj "ConfigMap" "c" :labels {:helm.toolkit.fluxcd.io/name "web"}) nil)))
  (is (= "Argo CD Application shop" (u/accepts-argo-cd (obj "ConfigMap" "c" :annotations {:argocd.argoproj.io/tracking-id "shop:/ConfigMap:shop/c"}) nil)))
  (is (nil? (u/accepts-argo-cd (obj "ConfigMap" "c" :labels {:app.kubernetes.io/instance "shop"}) nil)))
  (is (= "StageSet apps/web" (u/accepts-stageset (obj "ConfigMap" "c" :labels {:stages.metio.wtf/name "web" :stages.metio.wtf/namespace "apps"}) nil))))

(deftest every-built-in-rule-is-named-and-described
  (is (= (count u/default-rules) (count (distinct (map :name u/default-rules)))))
  (doseq [{:keys [name class description accepts]} u/default-rules]
    (is (not (str/blank? name)))
    (is (keyword? class) name)
    (is (> (count (str description)) 40) name)
    (is (fn? accepts) name)))

;; --- caller rules ----------------------------------------------------------------

(def ^:private metrics-rule
  (u/matching {:name "fahrplan" :owner "fahrplan (operator decision 2026-11)"
               :labels {"app.kubernetes.io/managed-by" "fahrplan"}}))

(deftest a-caller-rule-names-its-owner-in-the-report
  (let [o (obj "ConfigMap" "x" :labels {:app.kubernetes.io/managed-by "fahrplan"})
        f (u/facts {:namespace "shop" :objects [o] :types []})]
    (is (= :unowned (:class (u/classify u/default-rules f o))))
    (is (= {:class :third-party :rule "fahrplan" :owner "fahrplan (operator decision 2026-11)"}
           (u/classify (u/rules-with [metrics-rule]) f o)))
    (testing "through the detective, the finding carries the owner and the rule"
      (let [d (u/detective {:extra-rules [metrics-rule]})
            findings ((:detect d) {u/inventory-key {:namespace "shop" :objects [o] :types [{:type "configmaps" :group "" :kind "ConfigMap" :state :present}]}})]
        (is (= [{:severity :info :owner "fahrplan (operator decision 2026-11)" :rule "fahrplan" :class :third-party
                 :summary "Owned by fahrplan (operator decision 2026-11)"}]
               (map #(select-keys % [:severity :owner :rule :class :summary]) findings)))))))

(deftest a-matching-rule-holds-every-criterion
  (let [r (u/matching {:name "pm" :owner "metrics-server" :kind "PodMetrics" :group "metrics.k8s.io"})]
    (is (= "metrics-server" ((:accepts r) {:apiVersion "metrics.k8s.io/v1beta1" :kind "PodMetrics" :metadata {:name "p"}} nil)))
    (is (nil? ((:accepts r) {:apiVersion "example.org/v1" :kind "PodMetrics" :metadata {:name "p"}} nil))))
  (let [r (u/matching {:name "n" :owner "o" :object-name "keep-me" :annotations {"team" "db"}})]
    (is (= "o" ((:accepts r) (obj "ConfigMap" "keep-me" :annotations {:team "db"}) nil)))
    (is (nil? ((:accepts r) (obj "ConfigMap" "keep-me") nil)))
    (is (nil? ((:accepts r) (obj "ConfigMap" "other" :annotations {:team "db"}) nil)))))

(deftest a-rule-without-criteria-or-owner-is-refused
  (is (thrown? IllegalArgumentException (u/matching {:name "all" :owner "x"})))
  (is (thrown? IllegalArgumentException (u/matching {:name "n" :kind "ConfigMap"})))
  (is (thrown? IllegalArgumentException (u/matching {:owner "x" :kind "ConfigMap"}))))

(deftest a-rule-returning-nonsense-fails-the-run
  (let [bad {:name "bad" :class :third-party :description "returns a number" :accepts (fn [_ _] 42)}
        silent {:name "silent" :class :third-party :description "accepts without an owner" :accepts (fn [_ _] {:owner ""})}
        inv {:namespace "shop" :objects [(obj "ConfigMap" "x")] :types [{:type "configmaps" :group "" :kind "ConfigMap" :state :present}]}]
    (doseq [rule [bad silent]]
      (let [d (u/detective {:extra-rules [rule]})
            findings (detective/run-detectives [d] {u/inventory-key inv})]
        (is (= [true] (map :crashed findings)) (:name rule))))))

;; --- reads that did not answer -------------------------------------------------

(deftest a-type-that-did-not-answer-leaves-the-namespace-unjudged
  (let [{:keys [inventory]} (inventory-of (assoc shop-cluster :fail {"secrets" forbidden}))
        result (u/audit inventory u/default-rules)
        findings (u/findings result :critical)]
    (is (= [{:type "secrets" :reason (str "the read did not answer: " (:err forbidden))}] (:unread-types result)))
    (is (not (u/complete? result)))
    (is (not (u/clean? result)))
    (is (some #(and (= :critical (:severity %)) (= "secrets" (:component %))) findings))
    (testing "the other types are still judged"
      (is (some #(= "Deployment.apps/old-worker" (:component %)) findings)))))

(deftest only-the-unread-type-keeps-a-clean-namespace-from-being-clean
  (let [clean-lists {"configmaps" [(obj "ConfigMap" "kube-root-ca.crt")]
                     "serviceaccounts" [(obj "ServiceAccount" "default")]}
        cluster (assoc shop-cluster :lists clean-lists)]
    (testing "positive control: a namespace holding only what the control plane writes is clean"
      (let [result (u/audit (:inventory (inventory-of cluster)) u/default-rules)]
        (is (u/clean? result))
        (is (every? #(= :info (:severity %)) (u/findings result :critical)))))
    (testing "the same namespace with one type unanswered is not"
      (let [result (u/audit (:inventory (inventory-of (assoc cluster :fail {"jobs.batch" {:exit 1 :out "" :err "Error from server (TooManyRequests)"}})))
                            u/default-rules)]
        (is (not (u/clean? result)))
        (is (= ["jobs.batch"] (map :type (:unread-types result))))))
    (testing "control: were an unanswered list taken as empty, the test above would pass the namespace"
      (let [listing reads/listing]
        (with-redefs [reads/listing (fn [ctx ref] (let [r (listing ctx ref)]
                                                    (if (reads/unread? r) {:state :absent :ref (:ref r) :items []} r)))]
          (is (u/clean? (u/audit (:inventory (inventory-of (assoc cluster :fail {"jobs.batch" {:exit 1 :out "" :err "x"}})))
                                 u/default-rules))))))))

(deftest a-failed-api-group-is-named-and-an-excluded-one-is-not-counted
  (let [{:keys [inventory]} (inventory-of (assoc shop-cluster :discovery {:exit 1 :out api-resources :err partial-err}))
        result (u/audit inventory u/default-rules)]
    (is (= ["snapshot.storage.k8s.io/v1"] (map :group-version (:unread-groups result))))
    (is (some #(= {:group-version "metrics.k8s.io/v1beta1" :exclusion "metrics"} (select-keys % [:group-version :exclusion]))
              (:excluded result)))
    (is (not (u/clean? result)))
    (is (some #(= "snapshot.storage.k8s.io/v1" (:component %)) (u/findings result :critical))))
  (testing "with every failed group excluded, the partial discovery still judges"
    (let [err "error: unable to retrieve the complete list of server APIs: metrics.k8s.io/v1beta1: the server is currently unable to handle the request"
          {:keys [inventory]} (inventory-of (assoc shop-cluster
                                                   :discovery {:exit 1 :out api-resources :err err}
                                                   :lists {"serviceaccounts" [(obj "ServiceAccount" "default")]}))]
      (is (u/clean? (u/audit inventory u/default-rules))))))

(deftest an-inventory-that-could-not-be-taken-is-unread
  (testing "discovery did not answer"
    (is (str/includes? (:unread (:inventory (inventory-of (assoc shop-cluster :discovery {:exit 1 :out "" :err down-err}))))
                       "did not answer")))
  (testing "the namespace does not exist, so every type would list nothing"
    (let [{:keys [inventory calls]} (inventory-of (assoc shop-cluster :namespace nil))]
      (is (= {:unread "namespace shop does not exist"} inventory))
      (is (= 1 (count calls)))))
  (testing "the namespace could not be read"
    (is (str/starts-with? (:unread (:inventory (inventory-of (assoc shop-cluster :namespace :fail))))
                          "the namespace could not be read")))
  (testing "an audit of no inventory is no audit, and never clean"
    (is (nil? (u/audit nil u/default-rules)))
    (is (not (u/clean? nil)))))

;; --- the scan, end to end --------------------------------------------------------

(defn- scan
  "The json report of a scan of namespace shop against cluster, with the
   verdict's record."
  [cluster opts]
  (let [{:keys [execute]} (fake-cluster cluster)
        record (outcome/fresh)
        out (with-redefs [shell/execute execute
                          kubectl/current-context (constantly "ctx")]
              (binding [outcome/*record* record]
                (with-out-str
                  (binding [*err* (java.io.StringWriter.)]
                    (detective/scan! {:collect collect/collect} u/detectives (merge {:output "json"} opts))))))]
    {:report (json/parse-string out true) :record @record}))

(deftest a-scan-reports-the-unowned-and-fails
  (let [{:keys [report]} (scan shop-cluster {:namespace "shop"})
        critical (filter #(= "critical" (:severity %)) (:findings report))]
    (is (= #{"ConfigMap/leftover-settings" "Secret/ci-deployer-token" "Endpoints/legacy-db" "Endpoints/old-api"
             "Deployment.apps/old-worker" "Pod/batch-1a2b3c-xk9p0" "NetworkPolicy.projectcalico.org/deny-all"}
           (set (map :component critical))))
    (is (every? #(= "unmanaged-objects" (:detective %)) critical))
    (is (= {:kind "Deployment" :namespace "shop" :name "old-worker"}
           (:subject (first (filter #(= "Deployment.apps/old-worker" (:component %)) critical)))))
    (is (not (:nothing-checked report)))))

(deftest a-scan-whose-inventory-was-not-taken-checks-nothing
  (let [{:keys [report record]} (scan (assoc shop-cluster :discovery {:exit 1 :out "" :err down-err}) {:namespace "shop"})]
    (is (true? (:nothing-checked report)))
    (is (empty? (:findings report)))
    (is (= [true] (map :unread (:checked report))))
    (is (some #{"nothing was checked"} (:incomplete record)))))

(deftest a-scan-of-a-partly-read-namespace-is-incomplete
  (let [{:keys [report record]} (scan (assoc shop-cluster :fail {"secrets" forbidden}) {:namespace "shop"})]
    (is (some #(= "secrets" (:component %)) (:findings report)))
    (is (some #(str/includes? % "types that did not answer: secrets") (:incomplete record)))))

(deftest a-scan-without-a-namespace-collects-nothing
  (let [{:keys [execute calls]} (fake-cluster shop-cluster)]
    (with-redefs [shell/execute execute]
      (binding [*err* (java.io.StringWriter.)]
        (is (false? (detective/scan! {:collect collect/collect} u/detectives {:output "json"})))))
    (is (empty? @calls))))

;; --- controls: the tests above can fail ------------------------------------------

(deftest control-labels-before-owner-references-change-the-verdicts
  (let [helm? #(= "helm" (:name %))
        helm-first (concat (filter helm? u/default-rules) (remove helm? u/default-rules))
        verdicts (by-name (u/audit (:inventory (inventory-of shop-cluster)) helm-first))]
    (testing "the orphaned pod's inherited Helm label would pass it"
      (is (= :helm (:class (verdicts "Pod/batch-1a2b3c-xk9p0")))))
    (testing "and the EndpointSlice would follow its label, not its Service"
      (is (= :helm (:class (verdicts "EndpointSlice.discovery.k8s.io/web-7mx2p")))))))

(deftest control-distinct-by-kind-and-name-would-hide-the-calico-policy
  (let [by-kind-and-name (fn [listed]
                           (vals (reduce (fn [acc [_ o]] (update acc [(:kind o) (get-in o [:metadata :name])] #(or % o)))
                                         (array-map) listed)))]
    (with-redefs [collect/distinct-objects by-kind-and-name]
      (let [result (u/audit (:inventory (inventory-of shop-cluster)) u/default-rules)]
        (is (not-any? #(= "projectcalico.org/v3" (:apiVersion (:object %))) (:objects result)))))))
