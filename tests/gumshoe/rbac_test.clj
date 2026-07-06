;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.rbac-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.rbac :as rbac]))

(def clusterroles
  {:items [{:metadata {:name "cluster-admin"}
            :rules [{:apiGroups ["*"] :resources ["*"] :verbs ["*"]}]}
           {:metadata {:name "sneaky-admin"}
            :rules [{:apiGroups ["*"] :resources ["*"] :verbs ["*"]}]}
           {:metadata {:name "secret-peeker"}
            :rules [{:apiGroups [""] :resources ["secrets"] :verbs ["get" "list"]}]}
           {:metadata {:name "role-granter"}
            :rules [{:apiGroups ["rbac.authorization.k8s.io"]
                     :resources ["clusterroles"]
                     :verbs ["bind" "escalate"]}]}
           {:metadata {:name "system:controller:something"}
            :rules [{:apiGroups ["*"] :resources ["*"] :verbs ["*"]}]}
           {:metadata {:name "harmless"}
            :rules [{:apiGroups [""] :resources ["pods"] :verbs ["get" "list"]}]}]})

(def clusterrolebindings
  {:items [{:metadata {:name "cluster-admin"}
            :roleRef {:name "cluster-admin"}
            :subjects [{:kind "Group" :name "system:masters"}]}
           {:metadata {:name "give-alice-everything"}
            :roleRef {:name "cluster-admin"}
            :subjects [{:kind "User" :name "alice"}]}
           {:metadata {:name "sneaky-binding"}
            :roleRef {:name "sneaky-admin"}
            :subjects [{:kind "ServiceAccount" :namespace "tools" :name "bot"}]}
           {:metadata {:name "let-ci-read-secrets"}
            :roleRef {:name "secret-peeker"}
            :subjects [{:kind "ServiceAccount" :namespace "ci" :name "runner"}]}]})

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest wildcard-roles-test
  (testing "custom full-wildcard roles are reported, cluster-admin and system: are not"
    (is (= ["sneaky-admin"]
           (map :component (rbac/detect-wildcard-roles {"clusterroles" clusterroles})))))
  (testing "a role that enumerates every verb on */* is cluster-admin by another name"
    (let [roles {:items [{:metadata {:name "spelled-out-admin"}
                          :rules [{:apiGroups ["*"] :resources ["*"]
                                   :verbs ["get" "list" "watch" "create" "update" "patch" "delete"]}]}
                         {:metadata {:name "read-only"}
                          :rules [{:apiGroups ["*"] :resources ["*"] :verbs ["get" "list" "watch"]}]}]}]
      (is (rbac/wildcard-rule? (-> roles :items first :rules first)))
      (is (not (rbac/wildcard-rule? (-> roles :items second :rules first))))
      (is (= ["spelled-out-admin"]
             (map :component (rbac/detect-wildcard-roles {"clusterroles" roles})))
          "enumerated-verb god-mode is caught, a read-only role is not"))))

(deftest admin-bindings-test
  (let [findings (rbac/detect-admin-bindings {"clusterroles" clusterroles
                                              "clusterrolebindings" clusterrolebindings})
        by-summary (into {} (map (juxt :summary identity) findings))]
    (testing "cluster-admin and wildcard-equivalent bindings are reported, system:masters is not"
      (is (= #{"User alice holds cluster-admin power (via ClusterRole cluster-admin)"
               "ServiceAccount tools/bot holds cluster-admin power (via ClusterRole sneaky-admin)"}
             (summaries findings))))
    (testing "a human (User/Group) with cluster-admin is critical - the real audit signal"
      (is (= :critical (:severity (by-summary "User alice holds cluster-admin power (via ClusterRole cluster-admin)")))))
    (testing "a ServiceAccount (usually an operator) is a warning you can vet away"
      (is (= :warning (:severity (by-summary "ServiceAccount tools/bot holds cluster-admin power (via ClusterRole sneaky-admin)")))))))

(deftest admin-bindings-allowlist-test
  (testing "a vetted cluster-admin listed in :rbac :expected-cluster-admins is dropped"
    (let [evidence {"clusterroles" clusterroles
                    "clusterrolebindings" clusterrolebindings
                    "config" {:rbac {:expected-cluster-admins ["ServiceAccount tools/bot"]}}}
          findings (rbac/detect-admin-bindings evidence)]
      (is (= #{"User alice holds cluster-admin power (via ClusterRole cluster-admin)"}
             (summaries findings)))))
  (testing "a namespace glob allowlists every ServiceAccount in that namespace"
    (is (rbac/expected-admin? #{"ServiceAccount kube-system/*"} "ServiceAccount kube-system/operator-x"))
    (is (not (rbac/expected-admin? #{"ServiceAccount kube-system/*"} "ServiceAccount other/operator-x")))
    (is (rbac/expected-admin? #{"Group platform-admins"} "Group platform-admins"))
    (is (not (rbac/expected-admin? #{} "User alice")))))

(deftest escalation-verbs-test
  (is (= #{"ClusterRole carries privilege-escalation verbs: bind, escalate"}
         (summaries (rbac/detect-escalation-verbs {"clusterroles" clusterroles})))))

(deftest secret-readers-test
  (let [findings (rbac/detect-secret-readers {"clusterroles" clusterroles
                                              "clusterrolebindings" clusterrolebindings})]
    (testing "secret readers are reported, admin bindings are not duplicated"
      (is (= #{"ServiceAccount ci/runner can read secrets cluster-wide (via ClusterRole secret-peeker)"}
             (summaries findings))))))

(deftest powerful-rolebindings-test
  (let [rolebindings {:items [{:metadata {:namespace "kube-system" :name "bob-admin"}
                               :roleRef {:kind "ClusterRole" :name "admin"}
                               :subjects [{:kind "User" :name "bob"}]}
                              {:metadata {:namespace "moodle" :name "app-edit"}
                               :roleRef {:kind "ClusterRole" :name "edit"}
                               :subjects [{:kind "ServiceAccount" :namespace "moodle" :name "deployer"}]}
                              {:metadata {:namespace "moodle" :name "scoped"}
                               :roleRef {:kind "Role" :name "custom-narrow"}
                               :subjects [{:kind "User" :name "carol"}]}
                              {:metadata {:namespace "flux-system" :name "system:managed"}
                               :roleRef {:kind "ClusterRole" :name "admin"}
                               :subjects [{:kind "User" :name "dave"}]}]}
        findings (rbac/detect-powerful-rolebindings {"clusterroles" clusterroles
                                                     "rolebindings" rolebindings})]
    (testing "admin in kube-system is critical, edit elsewhere warns, Role refs and system: bindings are skipped"
      (is (= #{"User bob holds admin in this namespace"
               "ServiceAccount moodle/deployer holds edit in this namespace"}
             (summaries findings)))
      (is (= :critical (:severity (first (filter #(= "kube-system/bob-admin" (:component %)) findings)))))
      (is (= :warning (:severity (first (filter #(= "moodle/app-edit" (:component %)) findings))))))))

(deftest default-token-pods-test
  (let [evidence {"serviceaccounts" {:items [{:metadata {:namespace "hardened" :name "default"}
                                              :automountServiceAccountToken false}
                                             {:metadata {:namespace "sloppy" :name "default"}}]}
                  "pods" {:items [{:metadata {:namespace "sloppy" :name "a"} :spec {}}
                                  {:metadata {:namespace "sloppy" :name "b"}
                                   :spec {:serviceAccountName "default"}}
                                  {:metadata {:namespace "hardened" :name "c"} :spec {}}
                                  {:metadata {:namespace "fine" :name "d"}
                                   :spec {:serviceAccountName "dedicated"}}
                                  {:metadata {:namespace "fine" :name "e"}
                                   :spec {:automountServiceAccountToken false}}]}}
        findings (rbac/detect-default-token-pods evidence)]
    (testing "only pods with the default token actually mounted are counted, per namespace"
      (is (= [{:component "sloppy"
               :summary "2 pod(s) run with the default ServiceAccount token mounted"}]
             (map #(select-keys % [:component :summary]) findings))))))
