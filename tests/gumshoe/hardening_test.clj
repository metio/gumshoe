;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.hardening-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.pod-security :as pod-security]
            [gumshoe.detectives.posture :as posture]
            [gumshoe.detectives.registry :as registry]))

(defn- deployment [spec & {:keys [namespace] :or {namespace "shop"}}]
  {:metadata {:namespace namespace :name "web"} :spec {:template {:spec spec}}})

(def ^:private hardened
  {:securityContext {:runAsNonRoot true}
   :automountServiceAccountToken false
   :containers [{:name "app" :securityContext {:readOnlyRootFilesystem true}}]})

(defn- run [detect & {:keys [deployments cronjobs serviceaccounts]}]
  (vec (detect {"deployments" {:items (vec deployments)} "statefulsets" {:items []} "daemonsets" {:items []}
                "cronjobs" {:items (vec cronjobs)} "serviceaccounts" {:items (vec serviceaccounts)}})))

(deftest may-run-as-root-follows-the-effective-security-context
  (is (posture/may-run-as-root? {} {:name "a"}))
  (is (not (posture/may-run-as-root? {:securityContext {:runAsNonRoot true}} {:name "a"})) "inherited from the pod")
  (is (not (posture/may-run-as-root? {} {:securityContext {:runAsUser 1000}})))
  (is (posture/may-run-as-root? {} {:securityContext {:runAsUser 0}}) "uid 0 is root")
  (is (posture/may-run-as-root? {:securityContext {:runAsNonRoot true}} {:securityContext {:runAsNonRoot false}})
      "the container's setting wins over the pod's")
  (is (not (posture/may-run-as-root? {:securityContext {:runAsUser 0}} {:securityContext {:runAsUser 1001}}))))

(deftest root-containers-are-listed-per-workload
  (let [[finding & more] (run posture/detect-root-containers
                              :deployments [(deployment {:securityContext {:runAsUser 1000}
                                                         :initContainers [{:name "init" :securityContext {:runAsUser 0}}]
                                                         :containers [{:name "app"} {:name "sidecar" :securityContext {:runAsNonRoot true}}]})])]
    (is (nil? more))
    (is (= :info (:severity finding)))
    (is (= "shop/web" (:component finding)))
    (is (= "Deployment containers may run as root: init" (:summary finding)))))

(deftest cronjob-templates-count
  (is (= ["CronJob containers may run as root: job"]
         (map :summary (run posture/detect-root-containers
                            :cronjobs [{:metadata {:namespace "shop" :name "nightly"}
                                        :spec {:jobTemplate {:spec {:template {:spec {:containers [{:name "job"}]}}}}}}])))))

(deftest platform-namespaces-are-left-out
  (doseq [detect [posture/detect-root-containers posture/detect-writable-root-filesystems posture/detect-default-account-tokens]]
    (is (empty? (run detect :deployments [(deployment {:containers [{:name "cni"}]} :namespace "kube-system")])))))

(deftest a-hardened-workload-is-silent
  (doseq [detect [posture/detect-root-containers posture/detect-writable-root-filesystems posture/detect-default-account-tokens]]
    (is (empty? (run detect :deployments [(deployment hardened)])) (str detect))))

(deftest writable-root-filesystems
  (is (= ["Deployment containers keep a writable root filesystem: init, app"]
         (map :summary (run posture/detect-writable-root-filesystems
                            :deployments [(deployment {:initContainers [{:name "init"}]
                                                       :containers [{:name "app" :securityContext {:readOnlyRootFilesystem false}}
                                                                    {:name "ok" :securityContext {:readOnlyRootFilesystem true}}]})])))))

(deftest default-account-tokens
  (testing "the default account mounts unless something says no"
    (is (= ["Deployment mounts the default ServiceAccount's token"]
           (map :summary (run posture/detect-default-account-tokens :deployments [(deployment {:containers [{:name "a"}]})]))))
    (is (seq (run posture/detect-default-account-tokens :deployments [(deployment {:serviceAccountName "default" :containers [{:name "a"}]})]))))
  (testing "the account can switch it off, the pod overrides the account"
    (let [off [{:metadata {:namespace "shop" :name "default"} :automountServiceAccountToken false}]]
      (is (empty? (run posture/detect-default-account-tokens :deployments [(deployment {:containers [{:name "a"}]})] :serviceaccounts off)))
      (is (seq (run posture/detect-default-account-tokens
                    :deployments [(deployment {:automountServiceAccountToken true :containers [{:name "a"}]})] :serviceaccounts off)))
      (is (seq (run posture/detect-default-account-tokens
                    :deployments [(deployment {:containers [{:name "a"}]})]
                    :serviceaccounts [{:metadata {:namespace "other" :name "default"} :automountServiceAccountToken false}]))
          "another namespace's account does not count")))
  (testing "a dedicated account is the workload's own choice"
    (is (empty? (run posture/detect-default-account-tokens :deployments [(deployment {:serviceAccountName "web" :containers [{:name "a"}]})])))))

(deftest node-takeover-paths
  (doseq [path ["/" "/var/run/docker.sock" "/run/containerd/containerd.sock" "/var/run/crio/crio.sock" "/var/run" "/run/" "//"]]
    (is (pod-security/node-takeover-path? path) path))
  (doseq [path ["/var/log" "/etc/ssl/certs" "/var/run/secrets" "/var/lib/kubelet" "/run/containerd-shim"]]
    (is (not (pod-security/node-takeover-path? path)) path)))

(deftest hostpath-severity-follows-the-path
  (let [pod (fn [& paths] {:metadata {:namespace "ci" :name "p"} :spec {:volumes (for [p paths] {:hostPath {:path p}})}})
        detect #(vec (pod-security/detect-hostpath-volumes {"pods" {:items [%]}}))]
    (is (= [:critical "pod mounts the node's container runtime or root: /run/containerd/containerd.sock"]
           ((juxt :severity :summary) (first (detect (pod "/var/log" "/run/containerd/containerd.sock"))))))
    (is (= [:warning "pod mounts hostPath: /var/log, /etc/ssl"]
           ((juxt :severity :summary) (first (detect (pod "/var/log" "/etc/ssl"))))))
    (is (empty? (detect {:metadata {:namespace "kube-system" :name "p"} :spec {:volumes [{:hostPath {:path "/"}}]}})))))

(deftest the-hardening-checks-live-in-the-posture-scope
  (let [posture-names (set (map :name (registry/for-scope :posture)))
        all-names (set (map :name (registry/for-scope :all)))]
    (doseq [name ["root-containers" "writable-root-filesystems" "default-account-tokens"]]
      (is (contains? posture-names name))
      (is (not (contains? all-names name)) "left out of whole-cluster scans"))))
