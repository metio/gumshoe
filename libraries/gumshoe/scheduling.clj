;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.scheduling
  "Reads the scheduler's verdict on a pod it cannot place. The PodScheduled
   condition carries a message such as

     0/6 nodes are available: 2 Insufficient memory, 1 node(s) had untolerated
     taint {node-role.kubernetes.io/control-plane: }, 3 node(s) didn't match
     Pod's node affinity/selector. preemption: ...

   which names one cause per group of nodes. `causes` turns it into data, ranked
   by how many nodes each cause rejected, with what to do about each. `lead`
   reorders them for one pod, so that what rejected the nodes the pod would
   accept comes before the nodes its own placement rules exclude."
  (:require [clojure.set]
            [clojure.string :as str]))

(def ^:private classes
  "Ordered: the first pattern that matches a clause names its cause, so the
   more specific anti-affinity pattern precedes plain pod affinity."
  [{:cause :insufficient-resource
    :pattern #"^Insufficient (\S+)"
    :explain (fn [[_ resource]] (format "lack free %s" resource))
    :action (fn [[_ resource]] (format "lower the pod's %s request or add capacity" resource))}
   {:cause :max-pods
    :pattern #"Too many pods"
    :explain (constantly "are at their pod limit")
    :action (constantly "add nodes or raise the kubelet's maxPods")}
   {:cause :taint
    :pattern #"(?:untolerated taint|had taint)s?\s*(\{[^}]*\})?"
    :explain (fn [[_ taint]] (str "have a taint the pod does not tolerate" (when taint (str " " taint))))
    :action (constantly "add a toleration to the pod, or free nodes without that taint")}
   {:cause :node-affinity
    :pattern #"(?<!volume )node affinity|node\(s\) didn't match node selector|plugin\(s\) \[[^\]]*NodeAffinity"
    :explain (constantly "lack labels the pod's nodeSelector or node affinity requires")
    :action (constantly "compare the pod's nodeSelector/affinity with the node labels")}
   {:cause :pod-anti-affinity
    :pattern #"anti-affinity"
    :explain (constantly "run pods its anti-affinity rules exclude")
    :action (constantly "add nodes, or relax requiredDuringScheduling anti-affinity to preferred")}
   {:cause :pod-affinity
    :pattern #"pod affinity"
    :explain (constantly "run none of the pods its affinity rules require")
    :action (constantly "check that the pods it must run next to exist and are scheduled")}
   {:cause :topology-spread
    :pattern #"topology spread constraints"
    :explain (constantly "would break a topology spread constraint")
    :action (constantly "add capacity in the short domain, or set whenUnsatisfiable: ScheduleAnyway")}
   {:cause :volume-node-affinity
    :pattern #"volume node affinity conflict"
    :explain (constantly "cannot reach its bound volume, which lives in another zone or on another node")
    :action (constantly "run the pod where its volume is, or move the volume")}
   {:cause :volume-binding
    :pattern #"persistent volumes to bind|unbound immediate PersistentVolumeClaims|persistentvolumeclaim .* not found"
    :explain (constantly "have no volume to bind for its claim")
    :action (constantly "check the PersistentVolumeClaim and its StorageClass provisioner")}
   {:cause :volume-count
    :pattern #"max volume count"
    :explain (constantly "are at their limit of attached volumes")
    :action (constantly "spread the volumes over more nodes or use a node type with a higher attach limit")}
   {:cause :host-ports
    :pattern #"free ports"
    :explain (constantly "have the host port it asks for taken")
    :action (constantly "drop hostPort, or run fewer replicas than nodes")}
   {:cause :cordoned
    :pattern #"unschedulable"
    :explain (constantly "are cordoned")
    :action (constantly "uncordon nodes that are done with maintenance")}])

(defn- classify
  [text]
  (or (some (fn [{:keys [pattern explain action] :as class}]
              (when-let [match (re-find pattern text)]
                (let [match (if (string? match) [match] match)]
                  {:cause (:cause class) :explain (explain match) :action (action match)})))
            classes)
      ;; the count already says node(s), so the scheduler's own prefix goes
      {:cause :other :explain (str/replace text #"^node\(s\)\s+" "") :action nil}))

(defn- verdict
  "The node verdicts of a message: the part after 'nodes are available:' up to
   the preemption report, or nil when the message has none."
  [message]
  (when-let [[_ body] (re-find #"nodes are available:\s*(.*?)(?:\.\s*preemption:.*)?\.?\s*$" (str message))]
    body))

(defn causes
  "The causes in an unschedulable pod's message, most rejected nodes first, as
   {:nodes n :cause kw :explain str :action str-or-nil}. Empty when the message
   names no node verdicts."
  [message]
  (->> (some-> (verdict message) (str/split #",\s+(?=\d+\s)"))
       (keep (fn [clause]
               (when-let [[_ n text] (re-find #"^(\d+)\s+(.*)$" (str/trim clause))]
                 (assoc (classify text) :nodes (parse-long n)))))
       (sort-by :nodes >)))

(defn total
  "How many nodes the scheduler considered, read from '0/52 nodes are
   available', or nil when the message does not say."
  [message]
  (some-> (re-find #"\d+/(\d+) nodes are available" (str message)) second parse-long))

(defn- pinned-term
  "The node names one nodeSelectorTerm admits by matchFields metadata.name In,
   or nil when the term selects by anything else as well."
  [{:keys [matchFields matchExpressions]}]
  (let [[field & more] matchFields]
    (when (and field (empty? more) (empty? matchExpressions)
               (= "metadata.name" (:key field)) (= "In" (:operator field)))
      (set (:values field)))))

(defn placement
  "What a pod's required node selection says about where it may run:
   :restricted? when it has a nodeSelector or required node affinity, and
   :pinned, the set of node names it admits, when every required term names
   its nodes by matchFields metadata.name, as a DaemonSet pod's does."
  [pod]
  (let [terms (-> pod :spec :affinity :nodeAffinity
                  :requiredDuringSchedulingIgnoredDuringExecution :nodeSelectorTerms)
        pinned (map pinned-term terms)]
    {:pinned (when (and (seq terms) (every? some? pinned)) (apply clojure.set/union pinned))
     :restricted? (boolean (or (seq terms) (seq (-> pod :spec :nodeSelector))))}))

(defn- structural?
  "Whether a node-affinity cause only counts nodes the pod's own placement
   excludes. It is when the pod restricts its nodes, another cause rejected
   the nodes it accepts, and, for a pinned pod, its count fits within the
   nodes outside the pin, so none of the pinned nodes is among them."
  [cause others {:keys [pinned restricted?]} total]
  (and (= :node-affinity (:cause cause))
       restricted?
       (boolean (some #(not= :node-affinity (:cause %)) others))
       (or (nil? pinned) (nil? total) (<= (:nodes cause) (- total (count pinned))))))

(defn lead
  "The causes of a pod's message, each marked :structural when it only counts
   nodes the pod's own placement excludes, with those last. The first cause
   is what blocks the pod on the nodes that could host it."
  [message pod]
  (let [causes (causes message)
        place (placement pod)
        n (total message)
        marked (map #(assoc % :structural (structural? % causes place n)) causes)]
    (concat (remove :structural marked) (filter :structural marked))))

(defn describe
  "One line per cause, for a hint. A structural cause says that the pod's own
   node selection excludes those nodes instead of suggesting an action."
  [causes]
  (str/join "; " (for [{:keys [nodes explain action structural]} causes]
                   (str nodes " node(s) " explain
                        (cond structural " - excluded by the pod's own node selection"
                              action (str " - " action))))))
