;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.rbac
  "Detectives for RBAC hygiene: who holds cluster-admin power (under any
   name), roles with wildcard or privilege-escalation verbs, who can read
   every secret, and workloads that mount the default ServiceAccount token
   they never asked for."
  (:require [clojure.string :as str]
            [gumshoe.evidence :as evidence]
            [gumshoe.kubectl :as kubectl]))

(defn- system-name?
  [name]
  (str/starts-with? (str name) "system:"))

(def public-identities
  "The identities that stand for everyone: every request without credentials,
   and every request with any. A binding that hands one of them admin power or
   cluster-wide secret read gives it to the whole network or to every holder
   of any token, so it is always reported as critical, whatever the binding is
   called and whatever :rbac :expected-cluster-admins vets."
  #{["User" "system:anonymous"] ["Group" "system:unauthenticated"] ["Group" "system:authenticated"]})

(def control-plane-users
  "The Users the control plane's own components authenticate as, which the
   default bindings grant what those components need."
  #{"system:kube-controller-manager" "system:kube-scheduler" "system:kube-proxy" "system:apiserver"})

(def control-plane-groups
  "The Groups of the control plane: the break-glass group the API server
   grants every permission, the kubelets and the metrics endpoint readers."
  #{"system:masters" "system:nodes" "system:monitoring"})

(defn public-subject?
  "Whether a subject stands for everyone (see public-identities)."
  [subject]
  (contains? public-identities [(:kind subject) (:name subject)]))

(defn- service-account-user
  "A User subject named system:serviceaccount:<namespace>:<name> is that
   ServiceAccount's identity, written the way the API server authenticates it;
   it is read as the ServiceAccount, so it is judged and vetted like one."
  [subject]
  (if-let [[_ namespace name] (and (= "User" (:kind subject))
                                   (re-matches #"system:serviceaccount:([^:]+):([^:]+)" (str (:name subject))))]
    {:kind "ServiceAccount" :namespace namespace :name name}
    subject))

(defn control-plane-subject?
  "Whether a subject is one of the control plane's own identities: a
   component User, a kubelet's system:node:<name>, or one of the control
   plane Groups. Every other system: name is judged like any other subject,
   so an identity the cluster defines for everyone, every ServiceAccount
   (Group system:serviceaccounts) or a namespace's ServiceAccounts is still
   reported."
  [subject]
  (case (:kind subject)
    "User" (let [name (str (:name subject))]
             (or (contains? control-plane-users name)
                 (str/starts-with? name "system:node:")))
    "Group" (contains? control-plane-groups (:name subject))
    false))

(defn- judged-subjects
  "The binding's subjects a check reports on, each read as the identity it
   authenticates as, with control-plane identities left out."
  [binding]
  (for [subject (:subjects binding)
        :when (not (control-plane-subject? subject))]
    (service-account-user subject)))

(defn- subject->str
  [subject]
  (if (= "ServiceAccount" (:kind subject))
    (format "ServiceAccount %s/%s" (:namespace subject) (:name subject))
    (format "%s %s" (:kind subject) (:name subject))))

(def ^:private standard-verbs
  "The verbs a rule must cover to be write-everything, even without a literal *."
  #{"get" "list" "watch" "create" "update" "patch" "delete"})

(defn- covers-all-verbs?
  [verbs]
  (let [verbs (set verbs)]
    (or (contains? verbs "*")
        (every? verbs standard-verbs))))

(defn wildcard-rule?
  "Whether a rule grants god-mode on every resource: a literal * over
   resources and apiGroups, and either a * verb or all the standard verbs
   spelled out (a role that enumerates get/list/.../delete is cluster-admin by
   another name)."
  [rule]
  (boolean (and (covers-all-verbs? (:verbs rule))
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

(def ^:private public-hint
  "this identity stands for everyone who can reach the API server (or everyone holding any token) - remove the subject from the binding now")

(def admin-binding-joins
  "The ClusterRoles name the wildcard equivalents of cluster-admin, so
   without them only bindings to cluster-admin itself would be judged."
  ["clusterroles" "clusterrolebindings"])

(defn detect-admin-bindings
  "cluster-admin (or a wildcard equivalent) is a fact of life on a real cluster:
   every operator's ServiceAccount holds it. So this separates the audit signal
   from the noise. A User or Group with cluster-admin is a standing risk - a
   human with god mode - and stays critical. A ServiceAccount is almost always
   an operator and is a warning you can silence by vetting it into env.edn's
   :rbac :expected-cluster-admins. Once the known operators are listed, what is
   left is exactly the surprising access worth looking at. An identity that
   stands for everyone is critical and cannot be vetted, and the control
   plane's own identities are left out. Nothing is judged when a joined read
   did not answer."
  [evidence]
  (let [admin-roles (conj (full-wildcard-roles (get evidence "clusterroles")) "cluster-admin")
        expected (set (get-in evidence ["config" :rbac :expected-cluster-admins]))]
    (for [binding (when (evidence/joined? evidence admin-binding-joins)
                    (kubectl/items-of (get evidence "clusterrolebindings")))
          :when (contains? admin-roles (-> binding :roleRef :name))
          subject (judged-subjects binding)
          :let [id (subject->str subject)
                public? (public-subject? subject)]
          :when (or public? (not (expected-admin? expected id)))
          :let [human? (contains? #{"User" "Group"} (:kind subject))]]
      {:severity (if human? :critical :warning)
       :component (kubectl/name-of binding)
       :summary (format "%s holds cluster-admin power (via ClusterRole %s)"
                        id (-> binding :roleRef :name))
       :hint (cond
               public? public-hint
               human? "a user or group with cluster-admin is a standing audit risk - confirm this person still needs it"
               :else "usually an operator's ServiceAccount - if it is expected, add it to :rbac :expected-cluster-admins in env.edn to silence this")})))

(def ^:private escalation-verbs #{"escalate" "bind" "impersonate"})

(defn- covers?
  [values wanted]
  (boolean (some (conj (set wanted) "*") values)))

(defn- escalation-verbs-of
  "The escalation verbs a rule grants: those it names, and, through a * verb,
   escalate and bind on RBAC roles and impersonate on identities."
  [rule]
  (let [named (filter (set (:verbs rule)) escalation-verbs)]
    (if (some #{"*"} (:verbs rule))
      (concat named
              (when (and (covers? (:apiGroups rule) ["rbac.authorization.k8s.io"])
                         (covers? (:resources rule) ["roles" "clusterroles"]))
                ["escalate" "bind"])
              (when (and (covers? (:apiGroups rule) ["" "authentication.k8s.io"])
                         (covers? (:resources rule) ["users" "groups" "serviceaccounts" "userextras" "uids"]))
                ["impersonate"]))
      named)))

(defn detect-escalation-verbs
  [evidence]
  (let [clusterroles (get evidence "clusterroles")
        ;; a full wildcard is reported as cluster-admin by another name
        admins (full-wildcard-roles clusterroles)]
    (for [role (kubectl/items-of clusterroles)
          :let [role-name (kubectl/name-of role)]
          :when (not (system-name? role-name))
          :when (not= "cluster-admin" role-name)
          :when (not (contains? admins role-name))
          :let [found (sort (distinct (mapcat escalation-verbs-of (:rules role))))]
          :when (seq found)]
      {:severity :warning
       :component role-name
       :summary (format "ClusterRole carries privilege-escalation verbs: %s" (str/join ", " found))
       :hint "escalate/bind/impersonate let holders grant themselves more power"})))

(defn- reads-secrets?
  [rule]
  (boolean (and (some #{"" "*"} (:apiGroups rule))
                (some #{"secrets" "*"} (:resources rule))
                (some #{"get" "list" "watch" "*"} (:verbs rule)))))

(def secret-reader-joins
  "The ClusterRoles say which roles read Secrets, and the bindings who holds
   them."
  ["clusterroles" "clusterrolebindings"])

(defn detect-secret-readers
  "Subjects bound cluster-wide to a role that reads Secrets; an identity
   that stands for everyone is critical. Nothing is judged when a joined read
   did not answer."
  [evidence]
  (let [clusterroles (get evidence "clusterroles")
        admin-roles (conj (full-wildcard-roles clusterroles) "cluster-admin")
        secret-roles (set (for [role (kubectl/items-of clusterroles)
                                :when (some reads-secrets? (:rules role))]
                            (kubectl/name-of role)))]
    (for [binding (when (evidence/joined? evidence secret-reader-joins)
                    (kubectl/items-of (get evidence "clusterrolebindings")))
          :let [role (-> binding :roleRef :name)]
          :when (contains? secret-roles role)
          ;; admin bindings are already reported as critical
          :when (not (contains? admin-roles role))
          subject (judged-subjects binding)
          :let [public? (public-subject? subject)]
          ;; the bootstrap bindings the API server creates are named system:
          :when (or public? (not (system-name? (kubectl/name-of binding))))]
      {:severity (if public? :critical :info)
       :component (kubectl/name-of binding)
       :summary (format "%s can read secrets cluster-wide (via ClusterRole %s)"
                        (subject->str subject) role)
       :hint (if public?
               public-hint
               "review - cluster-wide secret read equals owning every credential in the cluster")})))

(def ^:private powerful-clusterroles #{"cluster-admin" "admin" "edit"})

(def namespace-grant-joins
  "The ClusterRoles name the wildcard equivalents of admin and edit, so
   without them only bindings to the built-in roles would be judged."
  ["clusterroles" "rolebindings"])

(defn detect-powerful-rolebindings
  "RoleBindings that hand out admin/edit/cluster-admin (or wildcard
   equivalents) inside a namespace; an identity that stands for everyone is
   critical. Nothing is judged when a joined read did not answer."
  [evidence]
  (let [powerful (into powerful-clusterroles (full-wildcard-roles (get evidence "clusterroles")))]
    (for [binding (when (evidence/joined? evidence namespace-grant-joins)
                    (kubectl/items-of (get evidence "rolebindings")))
          :let [ref (:roleRef binding)]
          :when (= "ClusterRole" (:kind ref))
          :when (contains? powerful (:name ref))
          subject (judged-subjects binding)
          :let [public? (public-subject? subject)]
          :when (or public? (not (system-name? (kubectl/name-of binding))))]
      {:severity (if (or public? (= "kube-system" (kubectl/namespace-of binding))) :critical :warning)
       :component (kubectl/namespace-name-of binding)
       :summary (format "%s holds %s in this namespace" (subject->str subject) (:name ref))
       :hint (if public?
               public-hint
               "namespace admin/edit reads all its secrets and modifies workloads - verify it is intended")})))

(defn- default-sa-automount-off?
  [serviceaccounts namespace]
  (let [account (first (filter #(and (= namespace (kubectl/namespace-of %))
                                     (= "default" (kubectl/name-of %)))
                               (kubectl/items-of serviceaccounts)))]
    (false? (:automountServiceAccountToken account))))

(def default-token-joins
  "The two reads the default token check joins: a namespace's default
   ServiceAccount may switch the token off for every pod, so without the
   ServiceAccounts every such pod would read as mounting it."
  ["pods" "serviceaccounts"])

(defn detect-default-token-pods
  "Namespaces whose pods run with the default ServiceAccount token mounted.
   Nothing is judged when a joined read did not answer."
  [evidence]
  (let [serviceaccounts (get evidence "serviceaccounts")
        offenders (for [pod (when (evidence/joined? evidence default-token-joins) (kubectl/items-of (get evidence "pods")))
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
    :requires admin-binding-joins
    :joins admin-binding-joins
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
    :requires secret-reader-joins
    :joins secret-reader-joins
    :detect detect-secret-readers}
   {:name "rbac-namespace-grants"
    :description "RoleBindings handing out admin/edit power inside namespaces"
    :requires namespace-grant-joins
    :joins namespace-grant-joins
    :detect detect-powerful-rolebindings}
   {:name "rbac-default-tokens"
    :description "Pods running with the default ServiceAccount token mounted"
    :requires default-token-joins
    :joins default-token-joins
    :detect detect-default-token-pods}])
