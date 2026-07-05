;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.security-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.network :as network]
            [gumshoe.detectives.pod-security :as pod-security]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest privileged-containers-test
  (let [evidence {"pods" {:items [{:metadata {:namespace "app" :name "root-pod"}
                                   :spec {:containers [{:name "web"
                                                        :securityContext {:privileged true}}]}}
                                  {:metadata {:namespace "app" :name "sneaky-init"}
                                   :spec {:containers [{:name "web"}]
                                          :initContainers [{:name "setup"
                                                            :securityContext {:privileged true}}]}}
                                  {:metadata {:namespace "kube-system" :name "kube-proxy"}
                                   :spec {:containers [{:name "proxy"
                                                        :securityContext {:privileged true}}]}}
                                  {:metadata {:namespace "app" :name "fine"}
                                   :spec {:containers [{:name "web"
                                                        :securityContext {:privileged false}}]}}]}}
        findings (pod-security/detect-privileged-containers evidence)]
    (testing "privileged app and init containers are critical, platform namespaces are excluded"
      (is (= #{"container web runs privileged" "container setup runs privileged"}
             (summaries findings)))
      (is (every? #(= :critical (:severity %)) findings)))))

(deftest host-namespaces-test
  (is (= #{"pod shares host namespaces: hostNetwork, hostPID"}
         (summaries (pod-security/detect-host-namespaces
                     {"pods" {:items [{:metadata {:namespace "app" :name "nosy"}
                                       :spec {:hostNetwork true :hostPID true}}
                                      {:metadata {:namespace "app" :name "fine"}
                                       :spec {:hostNetwork false}}]}})))))

(deftest hostpath-test
  (is (= #{"pod mounts hostPath: /var/run/docker.sock"}
         (summaries (pod-security/detect-hostpath-volumes
                     {"pods" {:items [{:metadata {:namespace "ci" :name "docker-user"}
                                       :spec {:volumes [{:name "sock"
                                                         :hostPath {:path "/var/run/docker.sock"}}
                                                        {:name "data"
                                                         :persistentVolumeClaim {:claimName "x"}}]}}]}})))))

(deftest dangerous-capabilities-test
  (let [findings (pod-security/detect-dangerous-capabilities
                  {"pods" {:items [{:metadata {:namespace "app" :name "cap-pod"}
                                    :spec {:containers [{:name "web"
                                                         :securityContext
                                                         {:capabilities {:add ["NET_ADMIN" "SYS_ADMIN" "CHOWN"]}}}]}}]}})]
    (testing "only the dangerous capabilities are named, harmless ones are ignored"
      (is (= #{"container web adds capabilities: NET_ADMIN, SYS_ADMIN"}
             (summaries findings))))))

(deftest network-policy-test
  (testing "namespaces with pods and no policy are reported, kube-* and fire-drill are not"
    (is (= ["exposed"]
           (map :component
                (network/detect-unprotected-namespaces
                 {"networkpolicies" {:items [{:metadata {:namespace "guarded" :name "default-deny"}}]}
                  "pods" {:items [{:metadata {:namespace "exposed" :name "a"}}
                                  {:metadata {:namespace "guarded" :name "b"}}
                                  {:metadata {:namespace "kube-system" :name "c"}}
                                  {:metadata {:namespace "fire-drill" :name "d"}}]}
                  network/global-policy-type {:items []}})))))
  (testing "calico GlobalNetworkPolicies suppress the per-namespace findings"
    (let [findings (network/detect-unprotected-namespaces
                    {"networkpolicies" {:items []}
                     "pods" {:items [{:metadata {:namespace "exposed" :name "a"}}]}
                     network/global-policy-type {:items [{:metadata {:name "deny-all"}}]}})]
      (is (= ["cluster"] (map :component findings)))
      (is (= #{"1 calico GlobalNetworkPolicies govern cluster traffic"}
             (summaries findings))))))
