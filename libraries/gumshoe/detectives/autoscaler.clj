;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.autoscaler
  "The cluster autoscaler's own account of itself, from the status ConfigMap it
   rewrites every loop (kube-system/cluster-autoscaler-status, the default of
   its --status-config-map-name). It is the only in-cluster record of node
   group bounds, scale-up backoff and the autoscaler's health verdict, so no
   cloud API is needed to tell why nodes do not arrive.

   The status is YAML under data.status. A payload that is not the
   structured status - the human-readable text older releases wrote - was
   not read: the check reports staleness but counts as unread, never as
   passed, because its parsed-status evidence is nil and the check joins it.
   The text format has no specification to parse it against. Staleness rests
   on the ConfigMap's annotation alone, which did answer, so that finding is
   marked :reports-unread and a scan keeps it while it drops everything else
   an unread check concludes."
  (:require [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [gumshoe.onset :as onset]))

(def parsed-status-key
  "The evidence key holding data.status parsed as the structured status, nil
   when the ConfigMap is missing or its status is in a format this check does
   not read. The check joins it, so such a status leaves the check unread."
  "cluster-autoscaler-parsed-status")

(def last-updated-annotation "cluster-autoscaler.kubernetes.io/last-updated")

(def stale-after
  "How old the status may get before the autoscaler counts as stopped. The
   autoscaler rewrites it every loop, about every 10 seconds by default."
  (java.time.Duration/ofMinutes 10))

(defn parse-update-time
  "The instant of a timestamp in the autoscaler's annotation format, Go's
   \"2006-01-02 15:04:05.999999999 -0700 MST\", or nil. The fraction is
   optional, because Go drops trailing zeros."
  [value]
  (when-let [[_ date time fraction hours minutes]
             (re-find #"^(\d{4}-\d{2}-\d{2}) (\d{2}:\d{2}:\d{2})(\.\d+)? ([+-]\d{2})(\d{2})" (str value))]
    (try
      (.toInstant (java.time.OffsetDateTime/parse (str date "T" time fraction hours ":" minutes)))
      (catch Exception _ nil))))

(defn- since
  [finding value]
  (onset/since finding (some-> (onset/instant value) str)))

(defn parse-status
  "data.status as a map, or nil when it is absent or not the structured
   status. Text status can parse as YAML too, so the map must carry one of the
   structured status's own keys."
  [configmap]
  (let [text (get-in configmap [:data :status])]
    (when-not (str/blank? text)
      (let [parsed (try (yaml/parse-string text) (catch Exception _ nil))]
        (when (and (map? parsed) (some #(contains? parsed %) [:autoscalerStatus :clusterWide :nodeGroups]))
          parsed)))))

(defn- counts
  [{:keys [registered longUnregistered unregistered]}]
  (format "%s of %s registered nodes ready%s%s"
          (or (:ready registered) 0) (or (:total registered) 0)
          (if (pos? (or unregistered 0)) (format ", %d not registered yet" unregistered) "")
          (if (pos? (or longUnregistered 0)) (format ", %d never registered" longUnregistered) "")))

(defn- stale-finding
  [configmap now]
  (let [updated (parse-update-time (get-in configmap [:metadata :annotations (keyword last-updated-annotation)]))]
    (when (and updated now (.isAfter ^java.time.Instant now (.plus ^java.time.Instant updated stale-after)))
      (onset/since
       {:severity :critical
        ;; read from the annotation, never from the status a check may not read
        :reports-unread true
        :component "kube-system/cluster-autoscaler-status"
        :summary "the cluster autoscaler has stopped updating its status"
        :hint (str "it rewrites the status every loop; an old one means the autoscaler is not running, "
                   "or cannot write the ConfigMap - check its pods and logs")}
       (str updated)))))

(defn- cluster-finding
  [status]
  (let [health (get-in status [:clusterWide :health])]
    (when (= "Unhealthy" (:status health))
      (since {:severity :critical
              :component "cluster-autoscaler"
              :summary "the cluster autoscaler halts all scaling: too many nodes are unready"
              :hint (str (counts (:nodeCounts health))
                         " - it stops once unready nodes exceed both --ok-total-unready-count (3 by default)"
                         " and --max-total-unready-percentage (45% by default); find out why the nodes are not Ready")}
             (:lastTransitionTime health)))))

(defn- group-findings
  [{:keys [name health scaleUp]}]
  (let [component (str "node group " name)
        bounds (format "target %s, min %s, max %s"
                       (:cloudProviderTarget health) (:minSize health) (:maxSize health))]
    (remove nil?
            [(when (= "Backoff" (:status scaleUp))
               (let [{:keys [errorCode errorMessage]} (:backoffInfo scaleUp)]
                 (since {:severity :critical
                         :component component
                         :summary (str "the cluster autoscaler backs off scaling this node group up"
                                       (when-not (str/blank? errorCode) (str " (" errorCode ")")))
                         :hint (str/join " - " (remove str/blank? [errorMessage
                                                                   "a scale-up failed, so pods waiting for this group's nodes stay Pending until the backoff ends"
                                                                   bounds]))}
                        (:lastTransitionTime scaleUp))))
             (when (= "Unhealthy" (:status health))
               (since {:severity :warning
                       :component component
                       :summary "the cluster autoscaler does not scale this node group up: it has fewer ready nodes than its minimum"
                       :hint (str (counts (:nodeCounts health)) ", " bounds
                                  " - nodes the cloud provider created are not registering or not becoming Ready")}
                      (:lastTransitionTime health)))])))

(defn detect-autoscaler-status
  [evidence]
  (let [configmap (get evidence "cluster-autoscaler-status")
        status (or (get evidence parsed-status-key) (parse-status configmap))
        stale (stale-finding configmap (get evidence :now))]
    (cond
      (nil? configmap) []
      ;; a stopped autoscaler's verdicts are as old as its last loop
      stale [stale]
      :else (concat (keep identity [(cluster-finding status)])
                    (mapcat group-findings (:nodeGroups status))))))

(def detectives
  [{:name "cluster-autoscaler"
    :description "The cluster autoscaler's status: stopped, halted by unready nodes, node groups in scale-up backoff or below their minimum"
    :requires ["cluster-autoscaler-status" parsed-status-key]
    :joins [parsed-status-key]
    :detect detect-autoscaler-status}])
