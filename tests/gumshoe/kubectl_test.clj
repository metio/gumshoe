;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.kubectl-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.shell :as shell]
            [gumshoe.kubectl :as kubectl]))

(def nodes
  {:items [{:metadata {:name "worker-1"}
            :spec {:taints [{:key "dedicated" :value "database" :effect "NoSchedule"}
                            {:key "node.kubernetes.io/unreachable" :effect "NoExecute"}]}}
           {:metadata {:name "worker-2"}
            :spec {:unschedulable true}}]})

(def pods
  {:items [{:metadata {:namespace "moodle" :name "web-1"}
            :spec {:volumes [{:persistentVolumeClaim {:claimName "moodle-data"}}
                             {:configMap {:name "config"}}]
                   :containers [{:image "docker.io/library/nginx:1.27"}]}}
           {:metadata {:namespace "keycloak" :name "keycloak-0"}
            :spec {:containers [{:image "quay.io/keycloak/keycloak:26.0"}
                                {:image "docker.io/library/nginx:1.27"}]}}]})

(def pvcs
  {:items [{:metadata {:namespace "moodle" :name "moodle-data"}}
           {:metadata {:namespace "moodle" :name "moodle-backup"}}]})

;; `kubectl api-resources --no-headers` - a core row with a short name, a core
;; row without one, a compound-cased core Kind, and a CRD row carrying its group.
(def api-resources-output
  (str "pods                              po   v1                        true    Pod\n"
       "nodes                                  v1                        false   Node\n"
       "persistentvolumeclaims            pvc  v1                        true    PersistentVolumeClaim\n"
       "helmreleases                      hr   helm.toolkit.fluxcd.io/v2 true    HelmRelease\n"))

(deftest parse-api-resources-test
  (let [kinds (kubectl/parse-api-resources api-resources-output)]
    (testing "the KIND column maps from the bare plural, with the cluster's own casing"
      (is (= "Pod" (get kinds "pods")))
      (is (= "Node" (get kinds "nodes")))
      (is (= "PersistentVolumeClaim" (get kinds "persistentvolumeclaims"))
          "a compound Kind keeps the cluster's casing - no lossy title-casing"))
    (testing "a grouped resource is also keyed by its plural.group form"
      (is (= "HelmRelease" (get kinds "helmreleases")))
      (is (= "HelmRelease" (get kinds "helmreleases.helm.toolkit.fluxcd.io"))))
    (testing "a core (groupless) resource is not given a spurious dotted key"
      (is (nil? (get kinds "pods.v1")))))
  (testing "empty or unreachable output parses to an empty map, never throws"
    (is (= {} (kubectl/parse-api-resources "")))
    (is (= {} (kubectl/parse-api-resources nil)))))

(deftest names-test
  (testing "names-of"
    (is (= ["worker-1" "worker-2"] (kubectl/names-of nodes))))
  (testing "namespaces-names"
    (is (= ["moodle/web-1" "keycloak/keycloak-0"] (kubectl/namespaces-names pods))))
  (testing "namespaces-of"
    (is (= ["moodle" "keycloak"] (kubectl/namespaces-of pods))))
  (testing "plain seqs work like parsed lists"
    (is (= ["worker-1" "worker-2"] (kubectl/names-of (:items nodes))))))

(deftest find-item-test
  (testing "finds namespaced items"
    (is (= "web-1" (kubectl/name-of (kubectl/find-item pods "moodle/web-1")))))
  (testing "finds cluster-scoped items"
    (is (= "worker-2" (kubectl/name-of (kubectl/find-item nodes "worker-2"))))))

(deftest split-namespace-name-test
  (is (= {:namespace "moodle" :name "web-1"} (kubectl/split-namespace-name "moodle/web-1"))))

(deftest taints-test
  (testing "renders key=value:effect"
    (is (= "dedicated=database:NoSchedule"
           (kubectl/taint->str {:key "dedicated" :value "database" :effect "NoSchedule"}))))
  (testing "renders key:effect when there is no value"
    (is (= "node.kubernetes.io/unreachable:NoExecute"
           (kubectl/taint->str {:key "node.kubernetes.io/unreachable" :effect "NoExecute"}))))
  (testing "lists nodes with taints"
    (is (= ["worker-1"] (kubectl/nodes-with-taints nodes))))
  (testing "removal spec drops the value - kubectl removes by key:effect only"
    (is (= "dedicated:NoSchedule" (kubectl/taint-removal-spec "dedicated=database:NoSchedule")))
    (is (= "node.kubernetes.io/unreachable:NoExecute"
           (kubectl/taint-removal-spec "node.kubernetes.io/unreachable:NoExecute")))))

(deftest schedulability-test
  (is (= ["worker-2"] (kubectl/unschedulable-nodes nodes)))
  (is (= ["worker-1"] (kubectl/schedulable-nodes nodes))))

(deftest unused-pvcs-test
  (testing "claims referenced by no pod"
    (is (= ["moodle/moodle-backup"] (kubectl/unused-pvcs pvcs pods)))))

(deftest terminating-namespaces-test
  (is (= ["stuck"]
         (kubectl/terminating-namespaces
          {:items [{:metadata {:name "stuck"} :status {:phase "Terminating"}}
                   {:metadata {:name "fine"} :status {:phase "Active"}}]}))))

(deftest remove-kubernetes-finalizer-test
  (is (= {:spec {:finalizers ["custom"]}}
         (kubectl/remove-kubernetes-finalizer {:spec {:finalizers ["kubernetes" "custom"]}}))))

(deftest service-port-test
  (let [service {:spec {:ports [{:name "http" :port 9090}
                                {:name "grpc" :port 10901}]}}]
    (is (= 9090 (kubectl/service-port service "http")))
    (is (nil? (kubectl/service-port service "https")))))

(deftest image-counts-test
  (is (= {"docker.io/library/nginx:1.27" 2
          "quay.io/keycloak/keycloak:26.0" 1}
         (kubectl/image-counts pods))))

(deftest pods-with-image-pull-back-off-test
  (is (= ["broken/pod-1"]
         (kubectl/pods-with-image-pull-back-off
          {:items [{:metadata {:namespace "broken" :name "pod-1"}
                    :status {:containerStatuses [{:state {:waiting {:reason "ImagePullBackOff"}}}]}}
                   {:metadata {:namespace "fine" :name "pod-2"}
                    :status {:containerStatuses [{:state {:running {}}}]}}]}))))

(deftest drainable-pods-test
  (let [node-pods {:items [{:metadata {:namespace "app" :name "web-1"}}
                           {:metadata {:namespace "kube-system" :name "calico-abc"
                                       :ownerReferences [{:kind "DaemonSet" :name "calico"}]}}
                           {:metadata {:namespace "kube-system" :name "etcd-cp1"
                                       :annotations {(keyword "kubernetes.io/config.mirror") "hash"}}}]}]
    (testing "daemonset and mirror pods never block a drain"
      (is (= ["app/web-1"]
             (kubectl/namespaces-names (kubectl/drainable-pods node-pods)))))))

(deftest namespaces-with-ingress-host-test
  (let [ingresses {:items [{:metadata {:namespace "moodle" :name "web"}
                            :spec {:rules [{:host "moodle.example.org"}]}}
                           {:metadata {:namespace "keycloak" :name "sso"}
                            :spec {:rules [{:host "sso.example.org"}]}}]}]
    (is (= ["moodle"] (kubectl/namespaces-with-ingress-host ingresses "moodle.example.org")))
    (is (= [] (kubectl/namespaces-with-ingress-host ingresses "nope.example.org")))))

(deftest hpa-scaling-metrics-test
  (let [hpas {:items [{:spec {:metrics [{:type "Resource"
                                         :resource {:name "cpu" :target {:type "Utilization"}}}
                                        {:type "Pods"
                                         :pods {:metric {:name "requests_per_second"}
                                                :target {:type "AverageValue"}}}]}}]}]
    (is (= ["cpu/Utilization (Resource)"
            "requests_per_second/AverageValue (Pods)"]
           (kubectl/hpa-scaling-metrics hpas)))))

(deftest any-matching?-test
  (testing "a non-empty name list means at least one resource matches the selector"
    (with-redefs [shell/stdout-of (fn [& _] "deployment.apps/loki\nstatefulset.apps/loki-backend")]
      (is (kubectl/any-matching? "deployment,statefulset" "app.kubernetes.io/name=loki"))))
  (testing "empty output - no match, or an unreachable API - reads as absent, never throws"
    (with-redefs [shell/stdout-of (fn [& _] "")]
      (is (not (kubectl/any-matching? "deployment" "app.kubernetes.io/name=loki"))))))

(deftest serves-crd?-test
  (testing "serves-crd? checks the named CRD via resource-exists?"
    (with-redefs [shell/exit-code-of
                  (fn [& args]
                    (if (= (vec args) ["kubectl" "get" "customresourcedefinition"
                                       "certificates.cert-manager.io" "--request-timeout=5s"])
                      0 1))]
      (is (kubectl/serves-crd? "certificates.cert-manager.io"))
      (is (not (kubectl/serves-crd? "absent.example.com"))))))
