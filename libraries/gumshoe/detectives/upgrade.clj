;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.upgrade
  "Detectives that answer whether a cluster can take its next upgrade:
   admission webhooks whose backend has no ready endpoint (an upgrade
   recreates every pod, and a webhook that fails closed refuses them), and
   kubelets the version skew policy already forbids or that the next
   control-plane minor would leave behind."
  (:require [clojure.string :as str]
            [gumshoe.detectives.nodes :as nodes]
            [gumshoe.kubectl :as kubectl]))

;; --- webhook backends ------------------------------------------------------------

(def ^:private service-name-label :kubernetes.io/service-name)

(defn- ready-endpoint?
  "Whether an EndpointSlice endpoint takes traffic; an unset ready means true."
  [endpoint]
  (not (false? (-> endpoint :conditions :ready))))

(defn ready-backends
  "The namespace/name of every Service with at least one ready endpoint."
  [endpointslices]
  (set (for [slice (kubectl/items-of endpointslices)
             :let [service (get-in slice [:metadata :labels service-name-label])]
             :when (and service (some ready-endpoint? (:endpoints slice)))]
         (str (kubectl/namespace-of slice) "/" service))))

(defn detect-webhook-backends
  "Webhooks whose Service exists but has no ready endpoint. A missing Service
   is webhook-references' finding, and an ExternalName Service has no
   endpoints to judge."
  [{:strs [validatingwebhookconfigurations mutatingwebhookconfigurations services endpointslices]}]
  (when (and services endpointslices)
    (let [by-key (into {} (map (juxt kubectl/namespace-name-of identity)) (kubectl/items-of services))
          ready (ready-backends endpointslices)]
      (for [[kind list] [["ValidatingWebhookConfiguration" validatingwebhookconfigurations]
                         ["MutatingWebhookConfiguration" mutatingwebhookconfigurations]]
            configuration (kubectl/items-of list)
            webhook (:webhooks configuration)
            :let [{:keys [namespace name]} (-> webhook :clientConfig :service)
                  key (str namespace "/" name)
                  service (get by-key key)
                  fails-closed? (not= "Ignore" (:failurePolicy webhook))]
            :when (and service (not= "ExternalName" (-> service :spec :type)) (not (contains? ready key)))]
        {:severity (if fails-closed? :critical :warning)
         :component (kubectl/name-of configuration)
         :summary (format "%s webhook %s calls Service %s, which has no ready endpoint" kind (:name webhook) key)
         :hint (if fails-closed?
                 "failurePolicy Fail - every request it matches is refused, and an upgrade that recreates pods stalls on it; bring its pods up or delete the configuration"
                 (format "failurePolicy Ignore - every request it matches waits %ss before it passes unchecked"
                         (or (:timeoutSeconds webhook) 10)))}))))

;; --- kubelet against the API server -------------------------------------------------

(defn server-minor
  "The API server's minor version from its /version document, or nil. Some
   providers suffix it (\"30+\"), so only the digits count."
  [version]
  (some->> version :minor str (re-find #"^\d+") parse-long))

(defn allowed-lag
  "How many minors a kubelet may trail the API server: three, or two for a
   kubelet older than 1.25."
  [kubelet-minor]
  (if (< kubelet-minor 25) 2 3))

(defn- sample
  [names]
  (let [shown (take 3 (sort names))]
    (str (str/join ", " shown) (when (< 3 (count names)) (format " and %d more" (- (count names) 3))))))

(defn detect-kubelet-apiserver-skew
  [evidence]
  (when-let [server (server-minor (get evidence "server-version"))]
    (for [[minor nodes] (group-by nodes/kubelet-minor (kubectl/items-of (get evidence "nodes")))
          :when minor
          :let [[major kubelet] (map parse-long (str/split minor #"\."))
                lag (- server kubelet)
                allowed (allowed-lag kubelet)
                names (map kubectl/name-of nodes)]
          :when (and (= 1 major) (or (neg? lag) (>= lag allowed)))]
      {:severity (if (= lag allowed) :warning :critical)
       :component "nodes"
       :summary (cond
                  (neg? lag) (format "kubelet %s is newer than the API server (1.%d)" minor server)
                  (> lag allowed) (format "kubelet %s trails the API server (1.%d) by %d minors, more than the %d the skew policy allows" minor server lag allowed)
                  :else (format "kubelet %s trails the API server (1.%d) by %d minors, the most the skew policy allows" minor server lag))
       :hint (str (sample names) " - "
                  (if (= lag allowed)
                    "upgrade these nodes before the control plane's next minor, which would leave them unsupported"
                    "this combination is unsupported - upgrade these nodes' kubelets (or the control plane first, if the kubelet is newer)"))})))

(def detectives
  [{:name "webhook-backends"
    :description "Admission webhooks whose Service has no ready endpoint"
    :requires ["validatingwebhookconfigurations" "mutatingwebhookconfigurations" "services" "endpointslices"]
    :detect detect-webhook-backends}
   {:name "kubelet-apiserver-skew"
    :description "Kubelets the version skew policy forbids, or that the next control-plane minor would leave behind"
    :requires ["nodes" "server-version"]
    :detect detect-kubelet-apiserver-skew}])
