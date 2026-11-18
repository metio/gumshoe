;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.deletion-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.deletion :as deletion]))

(defn- meta-of [name & {:keys [namespace owners labels deleting] :or {namespace "shop"}}]
  (cond-> {:namespace namespace :name name}
    owners (assoc :ownerReferences owners)
    labels (assoc :labels labels)
    deleting (assoc :deletionTimestamp "2026-11-19T10:00:00Z")))

(defn- deployment-pod [name deployment hash & {:keys [namespace volumes] :or {namespace "shop"}}]
  {:metadata (meta-of name :namespace namespace :labels {:pod-template-hash hash}
                      :owners [{:kind "ReplicaSet" :name (str deployment "-" hash) :controller true}])
   :spec {:volumes volumes}})

(deftest picks-parse-known-types-only
  (is (= {:type "configmaps" :namespace "shop" :name "settings"} (deletion/parse-pick "configmaps shop/settings")))
  (doseq [bad [nil "" "secrets shop/x" "configmaps settings" "configmaps shop/a b" "shop/x"]]
    (is (nil? (deletion/parse-pick bad)) (pr-str bad))))

(deftest picks-skip-objects-already-terminating
  (is (= ["deployments shop/a" "services shop/b"]
         (deletion/picks {"deployments" {:items [{:metadata (meta-of "a")} {:metadata (meta-of "z" :deleting true)}]}
                          "services" {:items [{:metadata (meta-of "b")}]}
                          "unknown" {:items [{:metadata (meta-of "c")}]}}))))

(deftest a-workload-takes-its-pods-along
  (let [evidence {"pods" {:items [(deployment-pod "web-1-x" "web" "1") (deployment-pod "web-1-y" "web" "1")
                                  (deployment-pod "webapp-2-z" "webapp" "2")
                                  (deployment-pod "web-1-q" "web" "1" :namespace "other")]}}]
    (is (= ["its 2 pod(s) are deleted with it: web-1-x, web-1-y"]
           (deletion/dependents {:type "deployments" :namespace "shop" :name "web"} evidence)))
    (is (nil? (deletion/dependents {:type "deployments" :namespace "shop" :name "idle"} evidence)))))

(deftest a-cronjob-takes-its-jobs-along
  (is (= ["its 1 Job(s) are deleted with it: backup-123"]
         (deletion/dependents {:type "cronjobs" :namespace "shop" :name "backup"}
                              {"jobs" {:items [{:metadata (meta-of "backup-123" :owners [{:kind "CronJob" :name "backup"}])}
                                               {:metadata (meta-of "manual" :owners [])}]}}))))

(deftest a-mounted-claim-lingers
  (is (= ["pods mount it, so it stays Terminating until they are gone: db-0"]
         (deletion/dependents {:type "persistentvolumeclaims" :namespace "shop" :name "data-db-0"}
                              {"pods" {:items [{:metadata (meta-of "db-0") :spec {:volumes [{:persistentVolumeClaim {:claimName "data-db-0"}}]}}
                                               {:metadata (meta-of "web") :spec {:volumes [{:emptyDir {}}]}}]}}))))

(deftest a-routed-service-leaves-ingresses-answering-503
  (let [ingress (fn [name backend default?]
                  {:metadata (meta-of name)
                   :spec (if default?
                           {:defaultBackend {:service {:name backend}}}
                           {:rules [{:http {:paths [{:backend {:service {:name backend}}}]}}]})})]
    (is (= ["Ingresses route to it and will answer 503: fallback, shop"]
           (deletion/dependents {:type "services" :namespace "shop" :name "web"}
                                {"ingresses" {:items [(ingress "shop" "web" false) (ingress "fallback" "web" true)
                                                      (ingress "api" "api" false)]}})))))

(deftest a-read-configmap-blocks-new-pods
  (let [template (fn [spec] {:spec {:template {:spec spec}}})
        evidence {"deployments" {:items [(merge {:metadata (meta-of "volume")} (template {:volumes [{:configMap {:name "settings"}}]}))
                                         (merge {:metadata (meta-of "unrelated")} (template {:containers [{:name "c"}]}))]}
                  "statefulsets" {:items [(merge {:metadata (meta-of "envfrom")} (template {:containers [{:envFrom [{:configMapRef {:name "settings"}}]}]}))]}
                  "daemonsets" {:items [(merge {:metadata (meta-of "projected")} (template {:volumes [{:projected {:sources [{:configMap {:name "settings"}}]}}]}))]}
                  "jobs" {:items [(merge {:metadata (meta-of "init")} (template {:initContainers [{:env [{:valueFrom {:configMapKeyRef {:name "settings" :key "k"}}}]}]}))]}
                  "cronjobs" {:items [{:metadata (meta-of "nightly")
                                       :spec {:jobTemplate {:spec {:template {:spec {:volumes [{:configMap {:name "settings"}}]}}}}}}
                                      {:metadata (meta-of "elsewhere" :namespace "other")
                                       :spec {:jobTemplate {:spec {:template {:spec {:volumes [{:configMap {:name "settings"}}]}}}}}}]}}]
    (is (= ["workloads read it, and their new pods will not start: CronJob nightly, DaemonSet projected, Deployment volume, Job init, StatefulSet envfrom"]
           (deletion/dependents {:type "configmaps" :namespace "shop" :name "settings"} evidence)))
    (is (nil? (deletion/dependents {:type "configmaps" :namespace "shop" :name "unused"} evidence)))))

(deftest types-without-dependents
  (is (nil? (deletion/dependents {:type "pods" :namespace "shop" :name "x"} {}))))

(deftest the-dry-run-and-the-delete
  (is (= ["--namespace=shop" "delete" "services" "web" "--dry-run=server"]
         (deletion/dry-run-args {:type "services" :namespace "shop" :name "web"})))
  (is (= ["--namespace=shop" "delete" "services" "web" "--wait=false"]
         (deletion/delete-args {:type "services" :namespace "shop" :name "web"})))
  (testing "the outcome reads stdout on success and stderr on refusal"
    (is (= {:ok? true :message "service \"web\" deleted (server dry run)"}
           (deletion/dry-run-result {:exit 0 :out "service \"web\" deleted (server dry run)\n" :err ""})))
    (is (= {:ok? false :message "admission webhook denied"}
           (deletion/dry-run-result {:exit 1 :out "" :err "  admission webhook denied\n"})))))

(deftest the-confirmation-shows-the-dry-run-and-dependents
  (is (= ["delete services shop/web" "the API server accepted a dry run: deleted" "Ingresses route to it"]
         (deletion/deletion-items "services shop/web" {:ok? true :message "deleted"} ["Ingresses route to it"])))
  (is (= ["delete pods shop/x" "the API server refused a dry run: forbidden"]
         (deletion/deletion-items "pods shop/x" {:ok? false :message "forbidden"} nil))))

(deftest deleted-means-gone-or-terminating
  (is (deletion/deleted? nil))
  (is (deletion/deleted? {:metadata (meta-of "x" :deleting true)}))
  (is (not (deletion/deleted? {:metadata (meta-of "x")}))))
