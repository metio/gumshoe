;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.services
  "Detectives for Services that cannot deliver traffic: a selector that matches
   no pods, only finished pods or only unready pods, a named targetPort no
   backend declares, backends that are unrelated workloads, and LoadBalancers
   that never received an address."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [gumshoe.conditions :as conditions]
            [gumshoe.evidence :as evidence]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(defn selects?
  "Whether a selector matches a label map. An empty selector selects nothing,
   because a Service without one has its endpoints managed by hand."
  [selector labels]
  (and (seq selector)
       (every? (fn [[k v]] (= v (get labels k))) selector)))

(defn- finished?
  [pod]
  (contains? #{"Succeeded" "Failed"} (-> pod :status :phase)))

(defn- terminating?
  [pod]
  (some? (-> pod :metadata :deletionTimestamp)))

(defn ready?
  [pod]
  (conditions/ready? pod))

(defn- in-namespace
  [items namespace]
  (filter #(= namespace (kubectl/namespace-of %)) items))

(defn- scaled-to-zero
  "The Deployments and StatefulSets at zero replicas whose pod template the
   selector matches: the reason a Service has no pods, when it was meant."
  [selector workloads]
  (for [workload workloads
        :when (and (= 0 (-> workload :spec :replicas))
                   (selects? selector (-> workload :spec :template :metadata :labels)))]
    (format "%s %s" (:kind workload) (kubectl/name-of workload))))

(defn- declared-port-names
  "Every container port name a pod declares. Init containers count, because a
   sidecar runs as one with restartPolicy Always."
  [pod]
  (set (for [container (concat (-> pod :spec :containers) (-> pod :spec :initContainers))
             port (:ports container)
             :when (:name port)]
         (:name port))))

(defn- image-repository
  "An image reference without its tag or digest, so two versions of one image
   compare equal."
  [image]
  (let [image (first (str/split image #"@"))
        slash (str/last-index-of image "/")
        colon (str/last-index-of image ":")]
    (if (and colon (or (nil? slash) (> colon slash)))
      (subs image 0 colon)
      image)))

(defn- unrelated-workloads
  "The workloads behind a Service when they run different software, else nil.
   Images every workload runs (a mesh sidecar, a log shipper) are ignored; the
   backends are unrelated when two of them share none of the remaining images.
   Two revisions of one application run the same image and never count."
  [pods]
  (let [images (reduce (fn [acc pod]
                         (update acc (kubectl/workload-of pod) (fnil into #{})
                                 (map (comp image-repository :image) (-> pod :spec :containers))))
                       {} pods)]
    (when (< 1 (count images))
      (let [common (apply set/intersection (vals images))
            own (into {} (for [[workload repos] images
                               :let [left (set/difference repos common)]
                               :when (seq left)]
                           [workload left]))]
        (when (some (fn [[a b]] (empty? (set/intersection (own a) (own b))))
                    (for [a (keys own) b (keys own) :when (neg? (compare a b))] [a b]))
          (sort (keys images)))))))

(defn- backend-findings
  [service pods workloads]
  (let [selector (-> service :spec :selector)
        component (kubectl/namespace-name-of service)
        matched (filter #(selects? selector (-> % :metadata :labels)) pods)
        live (remove #(or (finished? %) (terminating? %)) matched)
        ready (filter ready? live)
        headless? (= "None" (-> service :spec :clusterIP))]
    (cond
      (empty? matched)
      (if-let [idle (seq (scaled-to-zero selector workloads))]
        [{:severity :info
          :component component
          :summary (format "Service has no pods: %s is scaled to 0" (str/join ", " idle))}]
        [{:severity :warning
          :component component
          :summary "Service selector matches no pods"
          :hint (format "selector %s - compare it with the pod labels of the workload it should reach"
                        ;; label keys are parsed into keywords, and a prefixed
                        ;; key such as app.kubernetes.io/name keeps its prefix
                        ;; only in the keyword's full text
                        (str/join "," (for [[k v] selector] (str (subs (str k) 1) "=" v))))}])

      (empty? live)
      [{:severity :warning
        :component component
        :summary (format "Service selects only finished pods (%d)" (count matched))
        :hint "a Service needs running pods - its selector may match a Job's pods"}]

      :else
      (concat
       (when (and (empty? ready) (not (-> service :spec :publishNotReadyAddresses)))
         [{:severity :critical
           :component component
           :summary (format "Service has 0 of %d selected pods ready" (count live))
           :hint "no endpoint receives traffic - the pods' readiness probes are failing"}])
       (let [declared (into #{} (mapcat declared-port-names) live)]
         (for [port (-> service :spec :ports)
               :let [target (:targetPort port)]
               :when (and (string? target) (not (contains? declared target)))]
           {:severity :critical
            :component component
            :summary (format "Service targetPort %s is not declared by any selected pod" target)
            :hint "the port gets no endpoints - name a container port so, or use the port number"}))
       (when-not headless?
         (when-let [backends (unrelated-workloads ready)]
           [{:severity :warning
             :component component
             :summary (format "Service routes to unrelated workloads: %s" (str/join ", " backends))
             :hint "the selector is too broad - requests are spread across different applications"}]))))))

(def backend-joins
  "The reads the backend check joins: a Service selects pods, and a selector
   that matches none is explained by a Deployment or StatefulSet scaled to
   zero. Without the pods every Service would read as selecting nothing, and
   without the workloads an intended scale-down would read as a broken
   selector."
  ["services" "pods" "deployments" "statefulsets"])

(defn detect-broken-backends
  "Services whose selector reaches no pod, only finished or unready pods, an
   undeclared port, or unrelated workloads. Nothing is judged when a joined
   read did not answer."
  [evidence]
  (let [pods (kubectl/items-of (get evidence "pods"))
        workloads (concat (map #(assoc % :kind "Deployment") (kubectl/items-of (get evidence "deployments")))
                          (map #(assoc % :kind "StatefulSet") (kubectl/items-of (get evidence "statefulsets"))))]
    (for [service (when (evidence/joined? evidence backend-joins) (kubectl/items-of (get evidence "services")))
          :when (and (not= "ExternalName" (-> service :spec :type))
                     (seq (-> service :spec :selector)))
          :let [namespace (kubectl/namespace-of service)]
          finding (backend-findings service (in-namespace pods namespace) (in-namespace workloads namespace))]
      finding)))

(def ^:private load-balancer-grace-minutes 5)

(defn detect-pending-load-balancers
  [evidence]
  (let [cutoff (.minus ^java.time.Instant (:now evidence)
                       (java.time.Duration/ofMinutes load-balancer-grace-minutes))]
    (for [service (kubectl/items-of (get evidence "services"))
          :let [created (-> service :metadata :creationTimestamp)]
          :when (and (= "LoadBalancer" (-> service :spec :type))
                     (empty? (-> service :status :loadBalancer :ingress))
                     created
                     (.isBefore (java.time.Instant/parse created) cutoff))]
      (onset/since
       {:severity :warning
        :component (kubectl/namespace-name-of service)
        :summary (format "LoadBalancer Service has no address after %d minutes" load-balancer-grace-minutes)
        :hint "no load-balancer controller picked it up - check the cloud controller or MetalLB/kube-vip and the Service's events"}
       created))))

(def detectives
  [{:name "services"
    :description "Services whose selector reaches no ready pod, an undeclared port, or unrelated workloads"
    :requires backend-joins
    :joins backend-joins
    :detect detect-broken-backends}
   {:name "load-balancers"
    :description "LoadBalancer Services that never received an address"
    :requires ["services"]
    :detect detect-pending-load-balancers}])
