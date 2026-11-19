;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.references
  "Detectives for references to objects that do not exist. Kubernetes accepts
   a name that points nowhere and fails only when the reference is used: an
   Ingress returns 503, a pod is never created, a claim never binds, a webhook
   refuses every request. Each detective here compares the names an object
   refers to with the objects that exist."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.subject :as subject]))

(defn- names-in
  "The set of namespace/name keys of a resource list."
  [list]
  (set (map kubectl/namespace-name-of (kubectl/items-of list))))

(defn- cluster-names
  [list]
  (set (map kubectl/name-of (kubectl/items-of list))))

(defn- by-key
  [list]
  (into {} (map (juxt kubectl/namespace-name-of identity)) (kubectl/items-of list)))

;; --- Ingresses ---------------------------------------------------------------

(defn- ingress-backends
  [ingress]
  (concat (some-> ingress :spec :defaultBackend :service vector)
          (for [rule (-> ingress :spec :rules)
                path (-> rule :http :paths)
                :let [service (-> path :backend :service)]
                :when service]
            service)))

(defn- port-exists?
  [service {:keys [number name]}]
  (some #(or (and number (= number (:port %))) (and name (= name (:name %))))
        (-> service :spec :ports)))

(defn detect-ingress-references
  [{:strs [ingresses ingressclasses services secret-names]}]
  (let [services (by-key services)
        classes (cluster-names ingressclasses)
        secrets (set secret-names)]
    (for [ingress (kubectl/items-of ingresses)
          :let [namespace (kubectl/namespace-of ingress)
                component (kubectl/namespace-name-of ingress)
                class (-> ingress :spec :ingressClassName)]
          finding (concat
                   (distinct
                    (for [{:keys [name port]} (ingress-backends ingress)
                          :let [service (get services (str namespace "/" name))]
                          :when (or (nil? service) (and port (not (port-exists? service port))))]
                      {:severity :critical
                       :component component
                       :summary (if service
                                  (format "Ingress backend Service %s has no port %s" name (or (:number port) (:name port)))
                                  (format "Ingress backend Service %s does not exist" name))
                       :hint "requests routed to this backend fail with 503"}))
                   (when secret-names
                     (for [tls (-> ingress :spec :tls)
                           :let [secret (:secretName tls)]
                           :when (and secret (not (contains? secrets (str namespace "/" secret))))]
                       {:severity :warning
                        :component component
                        :summary (format "Ingress TLS Secret %s does not exist" secret)
                        :hint (str "the controller serves its default certificate for " (str/join ", " (:hosts tls))
                                   " - cert-manager creates the Secret once its Certificate is issued")}))
                   (when (and class ingressclasses (not (contains? classes class)))
                     [{:severity :warning
                       :component component
                       :summary (format "IngressClass %s does not exist" class)
                       :hint "no ingress controller picks this Ingress up"}]))]
      (assoc finding :subject (subject/subject "Ingress" namespace (kubectl/name-of ingress)) :cause true))))

;; --- pod templates ------------------------------------------------------------

(def ^:private template-types
  [["deployments" "Deployment" [:spec :template]]
   ["statefulsets" "StatefulSet" [:spec :template]]
   ["daemonsets" "DaemonSet" [:spec :template]]
   ["cronjobs" "CronJob" [:spec :jobTemplate :spec :template]]])

(def ^:private address-variable #"(?:_HOST|_URL|_ADDR|_ADDRESS|_ENDPOINT)$")

(defn- service-address
  "The Service a cluster-internal address names, as [namespace name port], or
   nil for an address that is not one: a Service is reached as name,
   name.namespace or name.namespace.svc[.cluster.local], optionally with a
   scheme, port and path. name.namespace looks like any two-label host name,
   so the caller trusts it only when that namespace exists."
  [default-namespace value]
  (when-let [[_ host port] (re-find #"^(?:[a-z][a-z0-9+.-]*://)?(?:[^@/]*@)?([a-z0-9.-]+)(?::(\d+))?(?:/.*)?$" (str value))]
    (let [labels (str/split host #"\.")
          port (some-> port parse-long)]
      (cond
        (or (str/includes? value "$(") (= "localhost" host)) nil
        (= 1 (count labels)) nil ; a bare word may be a search-domain alias, too ambiguous to judge
        (= 2 (count labels)) [(second labels) (first labels) port]
        (and (<= 3 (count labels)) (= "svc" (nth labels 2))
             (or (= 3 (count labels)) (= ["cluster" "local"] (drop 3 labels))))
        [(second labels) (first labels) port]
        :else nil))))

(defn- template-findings
  [kind workload template {:keys [services service-accounts secrets namespaces]}]
  (let [namespace (kubectl/namespace-of workload)
        component (kubectl/namespace-name-of workload)
        spec (:spec template)
        account (or (:serviceAccountName spec) (:serviceAccount spec))]
    (concat
     (when (and service-accounts account (not (contains? service-accounts (str namespace "/" account))))
       [{:severity :critical
         :component component
         :summary (format "%s uses ServiceAccount %s, which does not exist" kind account)
         :hint "the API server refuses its pods until the ServiceAccount exists"}])
     (when secrets
       (for [{secret :name} (:imagePullSecrets spec)
             :when (and secret (not (contains? secrets (str namespace "/" secret))))]
         {:severity :warning
          :component component
          :summary (format "%s pulls images with Secret %s, which does not exist" kind secret)
          :hint "pulls from a private registry fail with ImagePullBackOff"}))
     (when services
       (distinct
        (for [container (concat (:initContainers spec) (:containers spec))
              {:keys [name value]} (:env container)
              :when (and value (re-find address-variable (str name)))
              :let [[target-namespace service port] (service-address namespace value)]
              :when (and service (contains? namespaces target-namespace))
              :let [existing (get services (str target-namespace "/" service))]
              :when (or (nil? existing)
                        (and port (not (some #(= port (:port %)) (-> existing :spec :ports)))))]
          {:severity :warning
           :component component
           :summary (if existing
                      (format "env %s points at port %d, which Service %s/%s does not expose" name port target-namespace service)
                      (format "env %s points at Service %s/%s, which does not exist" name target-namespace service))
           :hint (str "container " (:name container) " reaches " value " - fix the address or create the Service")}))))))

(defn detect-template-references
  [evidence]
  (let [context {:services (some-> (get evidence "services") by-key)
                 :service-accounts (some-> (get evidence "serviceaccounts") names-in)
                 :secrets (some-> (get evidence "secret-names") set)
                 :namespaces (some-> (get evidence "namespaces") cluster-names)}]
    (for [[type kind path] template-types
          workload (kubectl/items-of (get evidence type))
          finding (template-findings kind workload (get-in workload path) context)]
      (assoc finding
             :subject (subject/subject kind (kubectl/namespace-of workload) (kubectl/name-of workload))
             :cause true))))

;; --- storage ------------------------------------------------------------------

(defn detect-storage-class-references
  [{:strs [persistentvolumeclaims storageclasses]}]
  (when storageclasses
    (let [classes (cluster-names storageclasses)]
      (for [claim (kubectl/items-of persistentvolumeclaims)
            :let [class (-> claim :spec :storageClassName)]
            :when (and (seq class) (not (contains? classes class)))]
        {:severity (if (= "Pending" (-> claim :status :phase)) :critical :info)
         :component (kubectl/namespace-name-of claim)
         :subject (subject/subject "PersistentVolumeClaim" (kubectl/namespace-of claim) (kubectl/name-of claim))
         :cause true
         :summary (format "PersistentVolumeClaim uses StorageClass %s, which does not exist" class)
         :hint (if (= "Pending" (-> claim :status :phase))
                 "no provisioner will create its volume - point it at an existing StorageClass"
                 "it is bound already, but a copy of it or a resize will fail")}))))

;; --- RBAC ---------------------------------------------------------------------

(defn detect-role-references
  [{:strs [rolebindings clusterrolebindings roles clusterroles]}]
  (when (and roles clusterroles)
    (let [roles (names-in roles)
          clusterroles (cluster-names clusterroles)
          exists? (fn [namespace {:keys [kind name]}]
                    (if (= "Role" kind) (contains? roles (str namespace "/" name)) (contains? clusterroles name)))]
      (for [[kind bindings] [["RoleBinding" rolebindings] ["ClusterRoleBinding" clusterrolebindings]]
            binding (kubectl/items-of bindings)
            :let [namespace (kubectl/namespace-of binding)
                  ref (:roleRef binding)]
            :when (and ref (not (exists? namespace ref)))]
        {:severity :warning
         :component (if namespace (kubectl/namespace-name-of binding) (kubectl/name-of binding))
         :summary (format "%s grants %s %s, which does not exist" kind (:kind ref) (:name ref))
         :hint "the binding grants nothing, and whoever creates a role of that name later gets its subjects' trust"}))))

;; --- admission webhooks -------------------------------------------------------

(defn detect-webhook-references
  [{:strs [validatingwebhookconfigurations mutatingwebhookconfigurations services]}]
  (when services
    (let [services (names-in services)]
      (for [[kind list] [["ValidatingWebhookConfiguration" validatingwebhookconfigurations]
                         ["MutatingWebhookConfiguration" mutatingwebhookconfigurations]]
            configuration (kubectl/items-of list)
            webhook (:webhooks configuration)
            :let [{:keys [namespace name]} (-> webhook :clientConfig :service)
                  fails-closed? (not= "Ignore" (:failurePolicy webhook))]
            :when (and name (not (contains? services (str namespace "/" name))))]
        {:severity (if fails-closed? :critical :warning)
         :component (kubectl/name-of configuration)
         :summary (format "%s webhook %s calls Service %s/%s, which does not exist" kind (:name webhook) namespace name)
         :hint (if fails-closed?
                 "failurePolicy Fail - every request the webhook matches is refused; restore the Service or delete the configuration"
                 "failurePolicy Ignore - requests pass unchecked while the Service is missing")}))))

;; --- autoscaler targets -----------------------------------------------------------------

(def ^:private scalable-targets
  {"Deployment" "deployments" "StatefulSet" "statefulsets"})

(defn detect-hpa-target-references
  [evidence]
  (for [hpa (kubectl/items-of (get evidence "horizontalpodautoscalers"))
        :let [{:keys [kind name]} (-> hpa :spec :scaleTargetRef)
              type (scalable-targets kind)]
        :when (and type name (get evidence type))
        :let [namespace (kubectl/namespace-of hpa)]
        :when (not-any? #(and (= namespace (kubectl/namespace-of %)) (= name (kubectl/name-of %)))
                        (kubectl/items-of (get evidence type)))]
    {:severity :critical
     :component (kubectl/namespace-name-of hpa)
     :subject (subject/subject "HorizontalPodAutoscaler" namespace (kubectl/name-of hpa))
     :cause true
     :summary (format "HorizontalPodAutoscaler targets %s %s, which does not exist" kind name)
     :hint "nothing is scaled - fix scaleTargetRef or delete the autoscaler"}))

(def detectives
  [{:name "ingress-references"
    :description "Ingresses pointing at missing Services, ports, TLS Secrets or IngressClasses"
    :requires ["ingresses" "ingressclasses" "services" "secret-names"]
    :detect detect-ingress-references}
   {:name "template-references"
    :description "Pod templates using missing ServiceAccounts, imagePullSecrets or Service addresses"
    :requires ["deployments" "statefulsets" "daemonsets" "cronjobs" "services" "serviceaccounts"
               "secret-names" "namespaces"]
    :detect detect-template-references}
   {:name "storageclass-references"
    :description "PersistentVolumeClaims using a StorageClass that does not exist"
    :requires ["persistentvolumeclaims" "storageclasses"]
    :detect detect-storage-class-references}
   {:name "hpa-target-references"
    :description "HorizontalPodAutoscalers whose scale target does not exist"
    :requires ["horizontalpodautoscalers" "deployments" "statefulsets"]
    :detect detect-hpa-target-references}])

(def security-detectives
  [{:name "role-references"
    :description "RoleBindings and ClusterRoleBindings granting a role that does not exist"
    :requires ["rolebindings" "clusterrolebindings" "roles" "clusterroles"]
    :detect detect-role-references}])

(def platform-detectives
  [{:name "webhook-references"
    :description "Admission webhooks calling a Service that does not exist"
    :requires ["validatingwebhookconfigurations" "mutatingwebhookconfigurations" "services"]
    :detect detect-webhook-references}])
