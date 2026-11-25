;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.rightsizing
  "Right-sizing container requests from observed usage. Prometheus supplies,
   per container, the 95th percentile of its CPU rate and the peak of its
   memory working set over a window; the pod specs supply the requests. Rows
   are grouped per workload and container, taking the highest usage of any of
   its pods, because a request has to fit the busiest replica.

   Everything here is pure: the runbook fetches, these functions judge."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.quantity :as quantity]))

;; --- queries ---------------------------------------------------------------------

(defn- selector
  [namespace]
  (str "{container!=\"\",container!=\"POD\"" (when namespace (format ",namespace=\"%s\"" namespace)) "}"))

(defn cpu-query
  "The p95 of each container's CPU rate (cores) over the window."
  [window namespace]
  (format "quantile_over_time(0.95, sum by (namespace, pod, container) (rate(container_cpu_usage_seconds_total%s[5m]))[%s:1m])"
          (selector namespace) window))

(defn memory-query
  "The peak of each container's memory working set (bytes) over the window."
  [window namespace]
  (format "max_over_time(sum by (namespace, pod, container) (container_memory_working_set_bytes%s)[%s:1m])"
          (selector namespace) window))

(defn samples
  "A Prometheus instant-vector response as {[namespace pod container] value};
   any other shape, and NaN or unparsable values, yield nothing."
  [response]
  (if (and (= "success" (:status response)) (= "vector" (-> response :data :resultType)))
    (into {}
          (keep (fn [{:keys [metric value]}]
                  (let [number (try (Double/parseDouble (str (second value))) (catch Exception _ nil))]
                    (when (and number (not (Double/isNaN number)) (:pod metric) (:container metric))
                      [[(:namespace metric) (:pod metric) (:container metric)] number]))))
          (-> response :data :result))
    {}))

;; --- usage per workload container -----------------------------------------------------

(defn- request-of
  [container resource]
  (quantity/quantity->number (get-in container [:resources :requests resource])))

(defn- unreadable-requests
  "The resources whose request the container sets to something that is not a
   quantity, so the request is unknown rather than unset."
  [container]
  (set (for [resource [:cpu :memory]
             :let [raw (get-in container [:resources :requests resource])]
             :when (and (some? raw) (nil? (request-of container resource)))]
         resource)))

(defn usage-rows
  "One row per workload and container that Prometheus has usage for:
   {:namespace :workload :container :pods :cpu-request :memory-request :cpu
    :memory}, requests in cores and bytes (nil when unset), usage the highest of
   any pod. A row whose request some replica sets to a value that is not a
   quantity also carries :unreadable-requests, the set of those resources, so
   an unparseable request is never judged as an unset one. Init containers
   are left out; their requests apply only while they run."
  [pods cpu memory]
  (let [entries (for [pod (kubectl/items-of pods)
                      :when (not (#{"Succeeded" "Failed"} (-> pod :status :phase)))
                      container (-> pod :spec :containers)
                      :let [key [(kubectl/namespace-of pod) (kubectl/name-of pod) (:name container)]]
                      :when (or (contains? cpu key) (contains? memory key))]
                  {:namespace (kubectl/namespace-of pod)
                   :workload (kubectl/workload-of pod)
                   :container (:name container)
                   :cpu-request (request-of container :cpu)
                   :memory-request (request-of container :memory)
                   :unreadable-requests (unreadable-requests container)
                   :cpu (get cpu key)
                   :memory (get memory key)})
        maximum (fn [values] (when-let [present (seq (remove nil? values))] (apply max present)))]
    (->> entries
         (group-by (juxt :namespace :workload :container))
         (map (fn [[[namespace workload container] group]]
                (let [unreadable (reduce into #{} (map :unreadable-requests group))]
                  (cond-> {:namespace namespace :workload workload :container container :pods (count group)
                           :cpu-request (maximum (map :cpu-request group))
                           :memory-request (maximum (map :memory-request group))
                           :cpu (maximum (map :cpu group))
                           :memory (maximum (map :memory group))}
                    (seq unreadable) (assoc :unreadable-requests unreadable)))))
         (sort-by (juxt :namespace :workload :container)))))

;; --- recommendations -------------------------------------------------------------------

(def headroom
  "The margin added on top of observed usage."
  1.15)

(def ^:private mebibyte 1048576.0)

(defn recommend-cpu
  "Cores: the p95 plus headroom, rounded up to 5m, at least 10m."
  [cpu]
  (when cpu (max 0.01 (/ (Math/ceil (/ (* cpu headroom) 0.005)) 200.0))))

(defn recommend-memory
  "Bytes: the peak plus headroom, rounded up to a mebibyte, at least 16Mi."
  [memory]
  (when memory (max (* 16 mebibyte) (* mebibyte (Math/ceil (/ (* memory headroom) mebibyte))))))

(def ^:private waste-floor
  "Below these, an oversized request is not worth reporting."
  {:cpu 0.1 :memory (* 128 mebibyte)})

(defn verdict
  "How a request compares with its recommendation: :missing when unset, :under
   when usage already exceeds it (CPU is throttled only by limits, but the
   scheduler placed the pod as if it needed less; memory beyond the request
   makes the pod an early eviction candidate), :over when it is at least twice
   the recommendation and wastes more than the floor, else :fits."
  [resource request usage recommendation]
  (cond
    (nil? usage) nil
    (nil? request) :missing
    (> usage request) :under
    (and (>= request (* 2 recommendation)) (>= (- request recommendation) (waste-floor resource))) :over
    :else :fits))

(defn- row-verdict
  "verdict for one resource of a row, or :unreadable when a replica sets that
   request to a value that is not a quantity."
  [row resource request usage recommendation]
  (cond
    (nil? usage) nil
    (contains? (:unreadable-requests row) resource) :unreadable
    :else (verdict resource request usage recommendation)))

(defn judge
  "A usage row with its recommendations and per-resource verdicts."
  [row]
  (let [cpu (recommend-cpu (:cpu row))
        memory (recommend-memory (:memory row))]
    (assoc row
           :cpu-recommendation cpu
           :memory-recommendation memory
           :cpu-verdict (row-verdict row :cpu (:cpu-request row) (:cpu row) cpu)
           :memory-verdict (row-verdict row :memory (:memory-request row) (:memory row) memory))))

(defn actionable?
  [row]
  (boolean (some #{:missing :unreadable :under :over} [(:cpu-verdict row) (:memory-verdict row)])))

;; --- formatting -------------------------------------------------------------------------

(defn format-cpu
  [cores]
  (if cores (str (long (Math/round (* cores 1000.0))) "m") "-"))

(defn format-memory
  [bytes]
  (if bytes (str (long (Math/ceil (/ bytes mebibyte))) "Mi") "-"))

(defn format-request
  "A row's request of resource for the table: \"?\" when a replica sets it to
   a value that is not a quantity, \"-\" when it is unset."
  [row resource]
  (cond
    (contains? (:unreadable-requests row) resource) "?"
    (= :cpu resource) (format-cpu (:cpu-request row))
    :else (format-memory (:memory-request row))))

(defn patch-line
  "The kubectl set resources command that applies a row's recommendations, or
   nil for a bare pod, which has no template to change."
  [{:keys [namespace workload container cpu-recommendation memory-recommendation]}]
  (when-not (str/starts-with? workload "Pod/")
    (str "kubectl -n " namespace " set resources " (str/lower-case (first (str/split workload #"/"))) "/"
         (second (str/split workload #"/" 2)) " -c " container
         " --requests="
         (str/join "," (remove nil? [(when cpu-recommendation (str "cpu=" (format-cpu cpu-recommendation)))
                                     (when memory-recommendation (str "memory=" (format-memory memory-recommendation)))])))))
