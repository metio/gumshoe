;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.pod-creation
  "Detective for controllers that cannot create their pods. The API server
   refuses the create before a pod exists, so no pod detective can see it: the
   refusal shows only as a FailedCreate event on the controller and, for a
   ReplicaSet, a ReplicaFailure condition. The refusal message names the cause -
   an admission webhook, a quota, PodSecurity, a LimitRange, a missing
   ServiceAccount or RBAC - and for a webhook the detective looks up the
   webhook's configuration and backend. A refusal is the cause of the
   controller's other findings: its missing replicas follow from it."
  (:require [clojure.string :as str]
            [gumshoe.detectives.services :as services]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.subject :as subject]))

(def ^:private timeout-pattern
  #"context deadline exceeded|i/o timeout|Client\.Timeout|TLS handshake timeout")

(def ^:private causes
  "Ordered: the first match names the cause, so a webhook timeout is told apart
   from a webhook that refuses connections."
  [[:webhook-timeout #(and (str/includes? % "failed calling webhook") (re-find timeout-pattern %))
    "an admission webhook does not answer in time"]
   [:webhook-unavailable #(str/includes? % "failed calling webhook")
    "an admission webhook cannot be reached"]
   [:webhook-denied #(re-find #"admission webhook \"[^\"]+\" denied" %)
    "an admission webhook denies the pod"]
   [:quota #(str/includes? % "exceeded quota")
    "a ResourceQuota is exhausted"]
   [:pod-security #(str/includes? % "violates PodSecurity")
    "the pod violates the namespace's PodSecurity level"]
   [:service-account #(re-find #"error looking up service account|serviceaccount \"[^\"]+\" not found" %)
    "its ServiceAccount does not exist"]
   [:limit-range #(re-find #"(?:maximum|minimum) \S+ usage per (?:Container|Pod)|LimitRange" %)
    "a LimitRange rejects its requests or limits"]
   [:rbac #(re-find #"cannot create resource \"pods\"" %)
    "the controller is not allowed to create pods"]])

(defn classify
  "The cause of a pod-creation refusal message, as [cause explanation]."
  [message]
  (let [message (str message)]
    (or (some (fn [[cause matches? explain]] (when (matches? message) [cause explain])) causes)
        [:other "the API server refuses the pod"])))

(def ^:private advice
  {:webhook-timeout "the webhook's backend is slow or a NetworkPolicy drops the API server's traffic to it"
   :webhook-unavailable "the webhook's backend is down - with failurePolicy Fail every matching create is refused until it is back"
   :webhook-denied "a policy refuses the pod - the message names the rule; fix the pod spec or the policy"
   :quota "raise the quota or lower the pod's requests and limits"
   :pod-security "fix the pod's securityContext, or label the namespace with a laxer pod-security.kubernetes.io/enforce level"
   :service-account "create the ServiceAccount, or point the pod template at an existing one"
   :limit-range "fit the pod's requests and limits into the namespace's LimitRange"
   :rbac "grant the controller's ServiceAccount create on pods"})

(defn- webhook-name
  [message]
  (second (re-find #"webhook \"([^\"]+)\"" (str message))))

(defn- webhook-facts
  "What the cluster says about the named webhook: its configuration, failure
   policy, timeout and how many ready pods back its Service."
  [evidence name]
  (first
   (for [[kind key] [["ValidatingWebhookConfiguration" "validatingwebhookconfigurations"]
                     ["MutatingWebhookConfiguration" "mutatingwebhookconfigurations"]]
         configuration (kubectl/items-of (get evidence key))
         webhook (:webhooks configuration)
         :when (= name (:name webhook))]
     (let [{service-namespace :namespace service-name :name} (-> webhook :clientConfig :service)
           service (when service-name
                     (first (filter #(and (= service-namespace (kubectl/namespace-of %))
                                          (= service-name (kubectl/name-of %)))
                                    (kubectl/items-of (get evidence "services")))))
           ready (when service
                   (count (filter #(and (= service-namespace (kubectl/namespace-of %))
                                        (services/selects? (-> service :spec :selector) (-> % :metadata :labels))
                                        (services/ready? %))
                                  (kubectl/items-of (get evidence "pods")))))]
       (str (format "webhook %s of %s %s, failurePolicy %s, timeout %ss"
                    name kind (kubectl/name-of configuration)
                    (or (:failurePolicy webhook) "Fail") (or (:timeoutSeconds webhook) 10))
            (cond
              (nil? service-name) (str ", URL " (-> webhook :clientConfig :url))
              (nil? service) (format ", its Service %s/%s does not exist" service-namespace service-name)
              :else (format ", its Service %s/%s has %d ready pods" service-namespace service-name ready)))))))

(def ^:private recent-window (java.time.Duration/ofHours 1))

(defn- recent?
  [now event]
  (when-let [timestamp (or (:lastTimestamp event) (:eventTime event))]
    (.isAfter (java.time.Instant/parse timestamp) (.minus ^java.time.Instant now recent-window))))

(defn- refusals
  "Every controller's latest pod-creation refusal, keyed by [kind namespace
   name]. A ReplicaSet's ReplicaFailure condition outlives its events, which
   expire after an hour; a FailedCreate event from the last hour covers the
   other controllers."
  [evidence]
  (let [now (:now evidence)
        from-events (for [event (kubectl/items-of (get evidence "events"))
                          :when (and (= "FailedCreate" (:reason event)) (recent? now event))
                          :let [{:keys [kind namespace name]} (:involvedObject event)]]
                      [[kind namespace name] [(or (:lastTimestamp event) (:eventTime event)) (:message event)]])
        from-conditions (for [replicaset (kubectl/items-of (get evidence "replicasets"))
                              condition (-> replicaset :status :conditions)
                              :when (and (= "ReplicaFailure" (:type condition)) (= "True" (:status condition)))]
                          [["ReplicaSet" (kubectl/namespace-of replicaset) (kubectl/name-of replicaset)]
                           [(or (:lastTransitionTime condition) "") (:message condition)]])]
    (reduce (fn [acc [key [at message]]]
              (if (pos? (compare at (first (get acc key)))) (assoc acc key [at message]) acc))
            {}
            (concat from-conditions from-events))))

(defn detect-pod-creation-failures
  [evidence]
  (for [[[kind namespace name] [_ message]] (sort-by key (refusals evidence))
        :let [[cause explain] (classify message)
              webhook (when (#{:webhook-timeout :webhook-unavailable :webhook-denied} cause)
                        (some->> (webhook-name message) (webhook-facts evidence)))]]
    {:severity (if (= :other cause) :warning :critical)
     :component (format "%s/%s" namespace name)
     :summary (format "%s cannot create pods: %s" kind explain)
     :hint (str/join " - " (remove nil? [webhook (advice cause)
                                         (str/replace (str message) #"\s+" " ")]))
     :subject (subject/subject kind namespace name)
     :cause true}))

(def detectives
  [{:name "pod-creation"
    :description "Controllers whose pods the API server refuses: webhooks, quota, PodSecurity, LimitRange, RBAC"
    :requires ["events" "replicasets" "validatingwebhookconfigurations" "mutatingwebhookconfigurations"
               "services" "pods"]
    :detect detect-pod-creation-failures}])
