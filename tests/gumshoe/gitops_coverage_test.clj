;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.gitops-coverage-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.gitops :as gitops]
            [gumshoe.detectives.registry :as registry]))

(defn- workload [namespace name & {:keys [labels annotations owners]}]
  {:metadata (cond-> {:namespace namespace :name name :labels (or labels {}) :annotations (or annotations {})}
               owners (assoc :ownerReferences owners))})

(def ^:private flux (workload "apps" "flux-app" :labels {:kustomize.toolkit.fluxcd.io/name "apps"
                                                         :kustomize.toolkit.fluxcd.io/namespace "flux-system"}))

(defn- detect [& {:keys [deployments statefulsets daemonsets cronjobs]}]
  (vec (gitops/detect-unmanaged-workloads {"deployments" {:items (vec deployments)}
                                           "statefulsets" {:items (vec statefulsets)}
                                           "daemonsets" {:items (vec daemonsets)}
                                           "cronjobs" {:items (vec cronjobs)}})))

(deftest managers-are-read-from-their-markers
  (is (= {:kind :gitops :by "Flux Kustomization flux-system/apps"} (gitops/manager flux)))
  (is (= {:kind :gitops :by "Flux HelmRelease flux-system/podinfo"}
         (gitops/manager (workload "a" "b" :labels {:helm.toolkit.fluxcd.io/name "podinfo"
                                                    :helm.toolkit.fluxcd.io/namespace "flux-system"
                                                    :app.kubernetes.io/managed-by "Helm"}
                                   :annotations {:meta.helm.sh/release-name "podinfo"})))
      "a Flux HelmRelease also carries Helm's markers, and counts as GitOps")
  (is (= {:kind :gitops :by "StageSet web"} (gitops/manager (workload "a" "b" :labels {:stages.metio.wtf/name "web"}))))
  (is (= {:kind :gitops :by "Argo CD Application guestbook"}
         (gitops/manager (workload "a" "b" :annotations {:argocd.argoproj.io/tracking-id "guestbook:apps/Deployment:a/b"}))))
  (is (= {:kind :helm :by "Helm release redis"} (gitops/manager (workload "a" "b" :annotations {:meta.helm.sh/release-name "redis"}))))
  (is (= {:kind :helm :by "Helm release redis"}
         (gitops/manager (workload "a" "b" :labels {:app.kubernetes.io/managed-by "Helm" :app.kubernetes.io/instance "redis"}))))
  (is (nil? (gitops/manager (workload "a" "b" :labels {:app.kubernetes.io/instance "guestbook"})))
      "Argo CD's label tracking cannot be told apart from a chart's instance label"))

(deftest a-gitops-cluster-reports-what-it-misses
  (let [findings (detect :deployments [flux (workload "apps" "by-hand")]
                         :statefulsets [(workload "data" "redis" :annotations {:meta.helm.sh/release-name "redis"})])]
    (is (= [{:severity :warning
             :component "apps/by-hand"
             :summary "Deployment is managed by no GitOps controller"
             :hint "it carries no Flux, Argo CD, StageSet or Helm marker, so it was applied by hand - nothing reverts drift, and rebuilding the cluster from its repository leaves it out"}
            {:severity :info
             :component "data/redis"
             :summary "StatefulSet is installed by Helm outside GitOps"
             :hint "Helm release redis - an upgrade or rollback is a manual helm command and nothing reverts drift; move the release into a HelmRelease or an Application"}]
           findings))))

(deftest every-workload-kind-is-covered
  (is (= #{"Deployment is managed by no GitOps controller" "StatefulSet is managed by no GitOps controller"
           "DaemonSet is managed by no GitOps controller" "CronJob is managed by no GitOps controller"}
         (set (map :summary (detect :deployments [flux (workload "a" "d")] :statefulsets [(workload "a" "s")]
                                    :daemonsets [(workload "a" "ds")] :cronjobs [(workload "a" "c")]))))))

(deftest owned-and-kube-system-workloads-are-left-out
  (is (= [] (detect :deployments [flux
                                  (workload "apps" "operated" :owners [{:kind "Cluster" :name "pg"}])
                                  (workload "kube-system" "coredns")]))))

(deftest a-cluster-without-gitops-gets-one-finding
  (let [findings (detect :deployments [(workload "a" "x") (workload "b" "y" :annotations {:meta.helm.sh/release-name "y"})])]
    (is (= 1 (count findings)))
    (is (= {:severity :info :component "cluster" :summary "No workload is managed by a GitOps controller"}
           (select-keys (first findings) [:severity :component :summary])))
    (is (re-find #"none of the 2 " (:hint (first findings))))))

(deftest nothing-to-judge-is-quiet
  (is (= [] (detect)))
  (is (= [] (detect :deployments [(workload "kube-system" "coredns")]))))

(deftest joins-the-posture-scope-only
  (testing "posture findings are opt-in"
    (is (some #(= "gitops-coverage" (:name %)) (registry/for-scope :posture)))
    (is (not-any? #(= "gitops-coverage" (:name %)) (registry/for-scope :all)))))
