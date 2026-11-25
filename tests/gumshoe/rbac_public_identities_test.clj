;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.rbac-public-identities-test
  "The RBAC checks leave out the control plane's own identities and nothing
   else that carries a system: name. The identities that stand for everyone -
   User system:anonymous, Group system:unauthenticated and Group
   system:authenticated - are reported as critical in all three binding
   checks, whatever the binding is called and whatever env.edn vets."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.rbac :as rbac]))

(def ^:private clusterroles
  {:items [{:metadata {:name "cluster-admin"}
            :rules [{:apiGroups ["*"] :resources ["*"] :verbs ["*"]}]}
           {:metadata {:name "god"}
            :rules [{:apiGroups ["*"] :resources ["*"] :verbs ["*"]}]}
           {:metadata {:name "secret-reader"}
            :rules [{:apiGroups [""] :resources ["secrets"] :verbs ["get" "list"]}]}]})

(def ^:private public-subjects
  [{:kind "User" :name "system:anonymous"}
   {:kind "Group" :name "system:unauthenticated"}
   {:kind "Group" :name "system:authenticated"}])

(def ^:private control-plane-subjects
  [{:kind "Group" :name "system:masters"}
   {:kind "Group" :name "system:nodes"}
   {:kind "User" :name "system:kube-controller-manager"}
   {:kind "User" :name "system:kube-scheduler"}
   {:kind "User" :name "system:kube-proxy"}
   {:kind "User" :name "system:node:worker-1"}])

(defn- cluster-binding [name role subject]
  {:metadata {:name name} :roleRef {:kind "ClusterRole" :name role} :subjects [subject]})

(defn- role-binding [namespace name role subject]
  {:metadata {:namespace namespace :name name} :roleRef {:kind "ClusterRole" :name role} :subjects [subject]})

(defn- label [subject] (str (:kind subject) " " (:name subject)))

(defn- admins [bindings & [config]]
  (rbac/detect-admin-bindings (cond-> {"clusterroles" clusterroles "clusterrolebindings" {:items bindings}}
                                config (assoc "config" config))))

(defn- secret-readers [bindings]
  (rbac/detect-secret-readers {"clusterroles" clusterroles "clusterrolebindings" {:items bindings}}))

(defn- namespace-grants [bindings]
  (rbac/detect-powerful-rolebindings {"clusterroles" clusterroles "rolebindings" {:items bindings}}))

(deftest an-identity-for-everyone-holding-cluster-admin-is-critical
  (doseq [subject public-subjects
          role ["cluster-admin" "god"]]
    (testing (str (label subject) " via " role)
      (let [[finding :as findings] (admins [(cluster-binding "open-door" role subject)])]
        (is (= 1 (count findings)))
        (is (= :critical (:severity finding)))
        (is (= (format "%s holds cluster-admin power (via ClusterRole %s)" (label subject) role) (:summary finding)))
        (is (re-find #"everyone" (:hint finding)))))))

(deftest an-identity-for-everyone-cannot-be-vetted
  (doseq [subject public-subjects]
    (testing (label subject)
      (is (= [:critical]
             (map :severity (admins [(cluster-binding "open-door" "cluster-admin" subject)]
                                    {:rbac {:expected-cluster-admins [(label subject)]}})))))))

(deftest an-identity-for-everyone-reading-secrets-cluster-wide-is-critical
  (doseq [subject public-subjects
          binding-name ["open-secrets" "system:looks-like-bootstrap"]]
    (testing (str (label subject) " in " binding-name)
      (let [[finding :as findings] (secret-readers [(cluster-binding binding-name "secret-reader" subject)])]
        (is (= 1 (count findings)))
        (is (= :critical (:severity finding)))
        (is (= (format "%s can read secrets cluster-wide (via ClusterRole secret-reader)" (label subject))
               (:summary finding)))))))

(deftest an-identity-for-everyone-holding-namespace-admin-is-critical
  (doseq [subject public-subjects
          role ["admin" "edit" "god"]
          binding-name ["open-namespace" "system:looks-like-bootstrap"]]
    (testing (str (label subject) " holds " role " via " binding-name)
      (let [[finding :as findings] (namespace-grants [(role-binding "web" binding-name role subject)])]
        (is (= 1 (count findings)))
        (is (= :critical (:severity finding)))
        (is (= (format "%s holds %s in this namespace" (label subject) role) (:summary finding)))))))

(deftest the-control-plane-identities-stay-exempt
  (doseq [subject control-plane-subjects]
    (testing (label subject)
      (is (empty? (admins [(cluster-binding "component" "cluster-admin" subject)])))
      (is (empty? (secret-readers [(cluster-binding "component" "secret-reader" subject)])))
      (is (empty? (namespace-grants [(role-binding "web" "component" "admin" subject)]))))))

(deftest other-system-identities-are-judged-like-any-subject
  (testing "every ServiceAccount of the cluster or of a namespace is a Group, so critical"
    (is (= [[:critical "Group system:serviceaccounts holds cluster-admin power (via ClusterRole cluster-admin)"]]
           (map (juxt :severity :summary)
                (admins [(cluster-binding "all-bots" "cluster-admin" {:kind "Group" :name "system:serviceaccounts"})]))))
    (is (= [[:warning "Group system:serviceaccounts:web holds edit in this namespace"]]
           (map (juxt :severity :summary)
                (namespace-grants [(role-binding "web" "bots" "edit" {:kind "Group" :name "system:serviceaccounts:web"})])))))
  (testing "a bootstrap token group is no control-plane identity"
    (is (= [:critical]
           (map :severity (admins [(cluster-binding "boot" "cluster-admin"
                                                    {:kind "Group" :name "system:bootstrappers:kubeadm:default-node-token"})]))))))

(deftest a-service-account-written-as-a-user-is-read-as-the-service-account
  (let [subject {:kind "User" :name "system:serviceaccount:tools:bot"}]
    (testing "it is reported as the ServiceAccount it is, at a ServiceAccount's severity"
      (is (= [[:warning "ServiceAccount tools/bot holds cluster-admin power (via ClusterRole cluster-admin)"]]
             (map (juxt :severity :summary) (admins [(cluster-binding "bot" "cluster-admin" subject)])))))
    (testing "and vetted under its ServiceAccount id"
      (is (empty? (admins [(cluster-binding "bot" "cluster-admin" subject)]
                          {:rbac {:expected-cluster-admins ["ServiceAccount tools/*"]}}))))
    (testing "a kube-system ServiceAccount is judged like every other one"
      (is (= ["ServiceAccount kube-system/operator holds cluster-admin power (via ClusterRole cluster-admin)"]
             (map :summary (admins [(cluster-binding "op" "cluster-admin"
                                                     {:kind "User" :name "system:serviceaccount:kube-system:operator"})])))))
    (testing "in a cluster-wide secret read"
      (is (= ["ServiceAccount tools/bot can read secrets cluster-wide (via ClusterRole secret-reader)"]
             (map :summary (secret-readers [(cluster-binding "bot-reads" "secret-reader" subject)])))))))

(deftest the-bootstrap-binding-names-still-hide-other-subjects
  ;; the API server's own bindings are named system:, and only an identity
  ;; for everyone breaks through that exemption
  (is (empty? (secret-readers [(cluster-binding "system:controller:x" "secret-reader"
                                                {:kind "ServiceAccount" :namespace "kube-system" :name "x"})])))
  (is (empty? (namespace-grants [(role-binding "web" "system:managed" "admin" {:kind "User" :name "dave"})]))))

(deftest one-binding-reports-each-identity-for-everyone-it-names
  (let [binding {:metadata {:name "open"} :roleRef {:kind "ClusterRole" :name "cluster-admin"}
                 :subjects (into [{:kind "Group" :name "system:masters"}] public-subjects)}]
    (is (= (sort (map #(format "%s holds cluster-admin power (via ClusterRole cluster-admin)" (label %)) public-subjects))
           (sort (map :summary (admins [binding])))))))

(deftest the-predicates-name-the-identities
  (is (every? rbac/public-subject? public-subjects))
  (is (not-any? rbac/public-subject? control-plane-subjects))
  (is (every? rbac/control-plane-subject? control-plane-subjects))
  (is (not-any? rbac/control-plane-subject? public-subjects))
  (testing "the kind matters: a ServiceAccount named like a group is no group"
    (is (not (rbac/public-subject? {:kind "ServiceAccount" :namespace "x" :name "system:authenticated"})))
    (is (not (rbac/control-plane-subject? {:kind "User" :name "system:masters"})))))
