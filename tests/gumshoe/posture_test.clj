;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.posture-test
  (:require [cheshire.core]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.posture :as posture]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.kubectl :as kubectl]))

(defn- container
  [name & {:keys [image ports probe] :or {image "registry.example/app:1.0"}}]
  (cond-> {:name name :image image}
    ports (assoc :ports [{:containerPort 8080}])
    probe (assoc :readinessProbe {:httpGet {:path "/ready" :port 8080}})))

(defn- workload
  [name & {:keys [replicas containers init labels] :or {labels {:app "web"}}}]
  {:metadata {:namespace "shop" :name name}
   :spec (cond-> {:template {:metadata {:labels labels}
                             :spec (cond-> {:containers (or containers [(container "app")])}
                                     init (assoc :initContainers init))}}
           replicas (assoc :replicas replicas))})

(defn- summaries [findings] (mapv :summary findings))

;; --- readiness probes -----------------------------------------------------------

(deftest a-container-serving-ports-needs-a-readiness-probe
  (is (= ["Deployment container app serves ports without a readiness probe"
          "DaemonSet container agent serves ports without a readiness probe"]
         (summaries (posture/detect-missing-readiness-probes
                     {"deployments" {:items [(workload "web" :containers [(container "app" :ports true)
                                                                           (container "sidecar")
                                                                           (container "ok" :ports true :probe true)])]}
                      "daemonsets" {:items [(workload "agent" :containers [(container "agent" :ports true)])]}}))))
  (is (= [:info] (map :severity (posture/detect-missing-readiness-probes
                                 {"statefulsets" {:items [(workload "db" :containers [(container "db" :ports true)])]}})))))

;; --- image tags -----------------------------------------------------------------

(deftest mutable-tags
  (doseq [[image tag] [["nginx" "no tag"]
                       ["nginx:latest" "latest"]
                       ["registry.example:5000/team/web" "no tag"]
                       ["registry.example:5000/team/web:latest" "latest"]
                       ["nginx:1.27" nil]
                       ["registry.example:5000/team/web:2.0" nil]
                       ["ghcr.io/org/app@sha256:0123" nil]
                       ["ghcr.io/org/app:latest@sha256:0123" nil]
                       ["nginx:latest-alpine" nil]]]
    (is (= tag (posture/mutable-tag image)) image)))

(deftest every-pod-spec-is-read-for-images
  (let [findings (posture/detect-mutable-images
                  {"deployments" {:items [(workload "web" :containers [(container "app" :image "nginx")]
                                                    :init [(container "migrate" :image "migrate:latest")])]}
                   "cronjobs" {:items [{:metadata {:namespace "shop" :name "backup"}
                                        :spec {:jobTemplate {:spec {:template {:spec {:containers [(container "backup" :image "restic")]}}}}}}]}
                   "statefulsets" {:items [(workload "db" :containers [(container "db" :image "postgres:17.2")])]}})]
    (is (= ["Deployment container migrate uses a mutable image tag (latest)"
            "Deployment container app uses a mutable image tag (no tag)"
            "CronJob container backup uses a mutable image tag (no tag)"]
           (summaries findings)))
    (is (every? #(= :warning (:severity %)) findings))))

;; --- replicas -------------------------------------------------------------------

(deftest a-single-replica-is-info
  (is (= ["shop/one" "shop/default"]
         (map :component (posture/detect-single-replicas
                          {"deployments" {:items [(workload "one" :replicas 1) (workload "default")
                                                  (workload "two" :replicas 2) (workload "off" :replicas 0)]}
                           "statefulsets" {:items []}
                           "horizontalpodautoscalers" {:items []}})))))

(deftest an-hpa-floor-above-one-counts
  (let [hpa (fn [kind name min] {:metadata {:namespace "shop" :name name}
                                 :spec (cond-> {:scaleTargetRef {:kind kind :name name}} min (assoc :minReplicas min))})]
    (is (empty? (posture/detect-single-replicas
                 {"deployments" {:items [(workload "web" :replicas 1)]}
                  "statefulsets" {:items []}
                  "horizontalpodautoscalers" {:items [(hpa "Deployment" "web" 2)]}})))
    (is (= 1 (count (posture/detect-single-replicas
                     {"deployments" {:items [(workload "web" :replicas 1)]}
                      "statefulsets" {:items []}
                      "horizontalpodautoscalers" {:items [(hpa "Deployment" "web" nil)]}})))
        "an HPA without minReplicas keeps one")
    (is (= 1 (count (posture/detect-single-replicas
                     {"deployments" {:items [(workload "web" :replicas 1)]}
                      "statefulsets" {:items []}
                      "horizontalpodautoscalers" {:items [(hpa "StatefulSet" "web" 3)]}})))
        "an HPA for another kind of the same name")))

;; --- disruption budgets ---------------------------------------------------------

(deftest selectors-follow-the-api
  (is (kubectl/label-selector-matches? {} {:app "web"}) "empty selects everything")
  (is (not (kubectl/label-selector-matches? nil {:app "web"})) "nil selects nothing")
  (is (kubectl/label-selector-matches? {:matchLabels {:app "web"}} {:app "web" :tier "front"}))
  (is (not (kubectl/label-selector-matches? {:matchLabels {:app "web"}} {:app "api"})))
  (is (kubectl/label-selector-matches? {:matchLabels {:app.kubernetes.io/name "web"}} {:app.kubernetes.io/name "web"}))
  (is (kubectl/label-selector-matches? {:matchExpressions [{:key "app.kubernetes.io/name" :operator "In" :values ["web"]}]}
                                       (cheshire.core/parse-string "{\"app.kubernetes.io/name\": \"web\"}" true))
      "a prefixed key from an expression matches a label parsed from JSON")
  (is (not (kubectl/label-selector-matches? {:matchLabels {:app.kubernetes.io/name "web"}} {:name "web"}))
      "the prefix is part of the key")
  (doseq [[expression labels expected] [[{:key "tier" :operator "In" :values ["front" "edge"]} {:tier "front"} true]
                                        [{:key "tier" :operator "In" :values ["edge"]} {:tier "front"} false]
                                        [{:key "tier" :operator "NotIn" :values ["edge"]} {:tier "front"} true]
                                        [{:key "tier" :operator "NotIn" :values ["edge"]} {} true]
                                        [{:key "tier" :operator "Exists"} {:tier "x"} true]
                                        [{:key "tier" :operator "Exists"} {} false]
                                        [{:key "tier" :operator "DoesNotExist"} {} true]
                                        [{:key "tier" :operator "Bogus"} {:tier "x"} false]]]
    (is (= expected (kubectl/label-selector-matches? {:matchExpressions [expression]} labels)) (pr-str expression labels))))

(deftest a-replicated-workload-without-a-budget
  (let [pdb (fn [namespace selector] {:metadata {:namespace namespace :name "pdb"} :spec {:selector selector}})
        run (fn [budgets] (posture/detect-unprotected-workloads
                           {"deployments" {:items [(workload "web" :replicas 3) (workload "solo" :replicas 1 :labels {:app "solo"})]}
                            "statefulsets" {:items []}
                            "poddisruptionbudgets" {:items budgets}}))]
    (is (= ["Deployment with 3 replicas has no PodDisruptionBudget"] (summaries (run []))))
    (is (empty? (run [(pdb "shop" {:matchLabels {:app "web"}})])))
    (is (= 1 (count (run [(pdb "other" {:matchLabels {:app "web"}})]))) "a budget in another namespace")
    (is (= 1 (count (run [(pdb "shop" {:matchLabels {:app "api"}})]))))
    (is (empty? (run [(pdb "shop" {})])) "an empty selector covers every pod of its namespace")))

;; --- deprecated APIs ------------------------------------------------------------

(def ^:private metrics
  "# HELP apiserver_requested_deprecated_apis [STABLE] Gauge of deprecated APIs that have been requested
# TYPE apiserver_requested_deprecated_apis gauge
apiserver_requested_deprecated_apis{group=\"flowcontrol.apiserver.k8s.io\",removed_release=\"1.32\",resource=\"flowschemas\",subresource=\"\",version=\"v1beta3\"} 1
apiserver_requested_deprecated_apis{group=\"\",removed_release=\"\",resource=\"componentstatuses\",subresource=\"\",version=\"v1\"} 1
apiserver_requested_deprecated_apis{group=\"flowcontrol.apiserver.k8s.io\",removed_release=\"1.32\",resource=\"flowschemas\",subresource=\"\",version=\"v1beta3\"} 1
apiserver_request_total{code=\"200\"} 5
")

(deftest the-metric-parses-into-apis
  (is (= [{:group "flowcontrol.apiserver.k8s.io" :version "v1beta3" :resource "flowschemas" :subresource "" :removed_release "1.32"}
          {:group "" :version "v1" :resource "componentstatuses" :subresource "" :removed_release ""}]
         (posture/parse-deprecated-requests metrics)))
  (is (= [] (posture/parse-deprecated-requests nil)))
  (is (= [] (posture/parse-deprecated-requests "apiserver_request_total 1"))))

(deftest a-requested-deprecated-api-is-a-warning
  (is (= [["flowcontrol.apiserver.k8s.io/v1beta3 flowschemas" "clients request the deprecated API flowcontrol.apiserver.k8s.io/v1beta3, removed in 1.32"]
          ["v1 componentstatuses" "clients request the deprecated API v1"]
          ["apps/v1beta2 deployments/scale" "clients request the deprecated API apps/v1beta2"]]
         (map (juxt :component :summary)
              (posture/detect-deprecated-api-requests
               {"deprecated-api-requests" (conj (posture/parse-deprecated-requests metrics)
                                                {:group "apps" :version "v1beta2" :resource "deployments" :subresource "scale"})})))))

(deftest a-crd-storing-an-old-version
  (let [crd (fn [stored versions] {:metadata {:name "widgets.example.com"}
                                   :spec {:versions versions} :status {:storedVersions stored}})
        run (fn [& crds] (summaries (posture/detect-deprecated-stored-versions {"customresourcedefinitions" {:items crds}})))]
    (is (empty? (run (crd ["v1"] [{:name "v1" :served true :storage true}]))))
    (is (= ["CustomResourceDefinition still stores objects at the deprecated version v1beta1"]
           (run (crd ["v1beta1" "v1"] [{:name "v1beta1" :served true :deprecated true} {:name "v1" :served true :storage true}]))))
    (is (= ["CustomResourceDefinition still stores objects at the unserved version v1alpha1"]
           (run (crd ["v1alpha1" "v1"] [{:name "v1alpha1" :served false} {:name "v1" :served true :storage true}]))))
    (is (= ["CustomResourceDefinition still stores objects at the removed version v1alpha1"]
           (run (crd ["v1alpha1" "v1"] [{:name "v1" :served true :storage true}]))))
    (is (re-find #"^use v1 - rewrite"
                 (:hint (first (posture/detect-deprecated-stored-versions
                                {"customresourcedefinitions"
                                 {:items [(crd ["v1beta1"] [{:name "v1beta1" :served true :deprecated true
                                                             :deprecationWarning "use v1"}])]}})))))))

;; --- the scope ------------------------------------------------------------------

(deftest posture-runs-only-when-asked-for
  (let [names (set (map :name posture/detectives))]
    (is (= (conj names "template-drift" "gitops-coverage") (set (map :name (registry/for-scope :posture)))))
    (is (not-any? names (map :name (registry/for-scope :all))))
    (is (every? (set (map :name (registry/all))) names) "still registered, so every property test covers it")))
