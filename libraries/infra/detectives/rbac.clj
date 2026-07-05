;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.rbac
  "Detectives for RBAC hygiene: who holds cluster-admin power (under any
   name), roles with wildcard or privilege-escalation verbs, who can read
   every secret, and workloads that mount the default ServiceAccount token
   they never asked for."
  (:require [clojure.string :as str]
            [infra.kubectl :as kubectl]))

(defn- system-name?
  [name]
  (str/starts-with? (str name) "system:"))

(defn- system-subject?
  [subject]
  (system-name? (:name subject)))

(defn- subject->str
  [subject]
  (if (= "ServiceAccount" (:kind subject))
    (format "ServiceAccount %s/%s" (:namespace subject) (:name subject))
    (format "%s %s" (:kind subject) (:name subject))))

(defn wildcard-rule?
  [rule]
  (boolean (and (some #{"*"} (:verbs rule))
                (some #{"*"} (:resources rule))
                (some #{"*"} (:apiGroups rule)))))

(defn full-wildcard-roles
  "Names of every ClusterRole that is cluster-admin by another name."
  [clusterroles]
  (set (for [role (kubectl/items-of clusterroles)
             :when (some wildcard-rule? (:rules role))]
         (kubectl/name-of role))))

(defn detect-wildcard-roles
  [evidence]
  (for [role-name (sort (full-wildcard-roles (get evidence "clusterroles")))
        :when (and (not= "cluster-admin" role-name)
                   (not (system-name? role-name)))]
    {:severity :warning
     :component role-name
     :summary "ClusterRole grants * on * in every api group"
     :hint "scope it down - a full wildcard is cluster-admin by another name"}))

(defn expected-admin?
  "Whether a subject id is a known, vetted cluster-admin holder - listed exactly
   in :rbac :expected-cluster-admins, or covered by a namespace glob there
   (\"ServiceAccount kube-system/*\" allowlists every ServiceAccount in
   kube-system). Vetted holders are dropped so the finding shows only the
   surprising ones."
  [expected id]
  (boolean
   (or (contains? expected id)
       (some (fn [pattern]
               (and (str/ends-with? (str pattern) "/*")
                    (str/starts-with? id (subs (str pattern) 0 (dec (count (str pattern)))))))
             expected))))

(defn detect-admin-bindings
  "cluster-admin (or a wildcard equivalent) is a fact of life on a real cluster:
   every operator's ServiceAccount holds it. So this separates the audit signal
   from the noise. A User or Group with cluster-admin is a standing risk - a
   human with god mode - and stays critical. A ServiceAccount is almost always
   an operator and is a warning you can silence by vetting it into env.edn's
   :rbac :expected-cluster-admins. Once the known operators are listed, what is
   left is exactly the surprising access worth looking at."
  [evidence]
  (let [admin-roles (conj (full-wildcard-roles (get evidence "clusterroles")) "cluster-admin")
        expected (set (get-in evidence ["config" :rbac :expected-cluster-admins]))]
    (for [binding (kubectl/items-of (get evidence "clusterrolebindings"))
          :when (contains? admin-roles (-> binding :roleRef :name))
          subject (:subjects binding)
          :when (not (system-subject? subject))
          :let [id (subject->str subject)]
          :when (not (expected-admin? expected id))
          :let [human? (contains? #{"User" "Group"} (:kind subject))]]
      {:severity (if human? :critical :warning)
       :component (kubectl/name-of binding)
       :summary (format "%s holds cluster-admin power (via ClusterRole %s)"
                        id (-> binding :roleRef :name))
       :hint (if human?
               "a user or group with cluster-admin is a standing audit risk - confirm this person still needs it"
               "usually an operator's ServiceAccount - if it is expected, add it to :rbac :expected-cluster-admins in env.edn to silence this")})))

(def ^:private escalation-verbs #{"escalate" "bind" "impersonate"})

(defn detect-escalation-verbs
  [evidence]
  (for [role (kubectl/items-of (get evidence "clusterroles"))
        :let [role-name (kubectl/name-of role)]
        :when (not (system-name? role-name))
        :when (not= "cluster-admin" role-name)
        :let [verbs (set (mapcat :verbs (:rules role)))
              found (sort (filter verbs escalation-verbs))]
        :when (seq found)]
    {:severity :warning
     :component role-name
     :summary (format "ClusterRole carries privilege-escalation verbs: %s" (str/join ", " found))
     :hint "escalate/bind/impersonate let holders grant themselves more power"}))

(defn- reads-secrets?
  [rule]
  (boolean (and (some #{"" "*"} (:apiGroups rule))
                (some #{"secrets" "*"} (:resources rule))
                (some #{"get" "list" "watch" "*"} (:verbs rule)))))

(defn detect-secret-readers
  [evidence]
  (let [clusterroles (get evidence "clusterroles")
        admin-roles (conj (full-wildcard-roles clusterroles) "cluster-admin")
        secret-roles (set (for [role (kubectl/items-of clusterroles)
                                :when (some reads-secrets? (:rules role))]
                            (kubectl/name-of role)))]
    (for [binding (kubectl/items-of (get evidence "clusterrolebindings"))
          :let [role (-> binding :roleRef :name)]
          :when (contains? secret-roles role)
          ;; admin bindings are already reported as critical
          :when (not (contains? admin-roles role))
          :when (not (system-name? (kubectl/name-of binding)))
          subject (:subjects binding)
          :when (not (system-subject? subject))]
      {:severity :info
       :component (kubectl/name-of binding)
       :summary (format "%s can read secrets cluster-wide (via ClusterRole %s)"
                        (subject->str subject) role)
       :hint "review - cluster-wide secret read equals owning every credential in the cluster"})))

(def ^:private powerful-clusterroles #{"cluster-admin" "admin" "edit"})

(defn detect-powerful-rolebindings
  "RoleBindings that hand out admin/edit/cluster-admin (or wildcard
   equivalents) inside a namespace."
  [evidence]
  (let [powerful (into powerful-clusterroles (full-wildcard-roles (get evidence "clusterroles")))]
    (for [binding (kubectl/items-of (get evidence "rolebindings"))
          :let [ref (:roleRef binding)]
          :when (= "ClusterRole" (:kind ref))
          :when (contains? powerful (:name ref))
          :when (not (system-name? (kubectl/name-of binding)))
          subject (:subjects binding)
          :when (not (system-subject? subject))]
      {:severity (if (= "kube-system" (kubectl/namespace-of binding)) :critical :warning)
       :component (kubectl/namespace-name-of binding)
       :summary (format "%s holds %s in this namespace" (subject->str subject) (:name ref))
       :hint "namespace admin/edit reads all its secrets and modifies workloads - verify it is intended"})))

(defn- default-sa-automount-off?
  [serviceaccounts namespace]
  (let [account (first (filter #(and (= namespace (kubectl/namespace-of %))
                                     (= "default" (kubectl/name-of %)))
                               (kubectl/items-of serviceaccounts)))]
    (false? (:automountServiceAccountToken account))))

(defn detect-default-token-pods
  [evidence]
  (let [serviceaccounts (get evidence "serviceaccounts")
        offenders (for [pod (kubectl/items-of (get evidence "pods"))
                        :let [spec (:spec pod)]
                        :when (contains? #{nil "default"} (:serviceAccountName spec))
                        :when (not (false? (:automountServiceAccountToken spec)))
                        :when (not (default-sa-automount-off? serviceaccounts (kubectl/namespace-of pod)))]
                    (kubectl/namespace-of pod))]
    (for [[namespace pod-count] (sort (frequencies offenders))]
      {:severity :info
       :component namespace
       :summary (format "%d pod(s) run with the default ServiceAccount token mounted" pod-count)
       :hint "set automountServiceAccountToken: false or give workloads dedicated ServiceAccounts"})))

(def detectives
  [{:name "rbac-admins"
    :description "Cluster-admin held by a human (critical) or an un-vetted ServiceAccount (warning)"
    :requires ["clusterroles" "clusterrolebindings"]
    :detect detect-admin-bindings}
   {:name "rbac-wildcards"
    :description "ClusterRoles granting * on * in every api group"
    :requires ["clusterroles"]
    :detect detect-wildcard-roles}
   {:name "rbac-escalation"
    :description "ClusterRoles with escalate/bind/impersonate verbs"
    :requires ["clusterroles"]
    :detect detect-escalation-verbs}
   {:name "rbac-secret-readers"
    :description "Bindings that allow reading every secret in the cluster"
    :requires ["clusterroles" "clusterrolebindings"]
    :detect detect-secret-readers}
   {:name "rbac-namespace-grants"
    :description "RoleBindings handing out admin/edit power inside namespaces"
    :requires ["clusterroles" "rolebindings"]
    :detect detect-powerful-rolebindings}
   {:name "rbac-default-tokens"
    :description "Pods running with the default ServiceAccount token mounted"
    :requires ["pods" "serviceaccounts"]
    :detect detect-default-token-pods}])
