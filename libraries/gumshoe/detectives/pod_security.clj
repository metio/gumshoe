;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.pod-security
  "Detectives for workload security posture: privileged containers, shared
   host namespaces, hostPath mounts, and dangerous added capabilities.
   Platform namespaces where CNI/CSI/kube components legitimately need these
   powers are excluded to keep the signal clean."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def platform-namespaces
  "Privileged system workloads are expected here - tune as the platform grows."
  #{"kube-system" "calico-system" "calico-apiserver" "tigera-operator"})

(defn- workload-pods
  [evidence]
  (remove #(contains? platform-namespaces (kubectl/namespace-of %))
          (kubectl/items-of (get evidence "pods"))))

(defn- all-containers
  [pod]
  (concat (-> pod :spec :containers)
          (-> pod :spec :initContainers)))

(defn detect-privileged-containers
  [evidence]
  (for [pod (workload-pods evidence)
        container (all-containers pod)
        :when (true? (-> container :securityContext :privileged))]
    {:severity :critical
     :component (kubectl/namespace-name-of pod)
     :summary (format "container %s runs privileged" (:name container))
     :hint "a privileged container owns its node - outside CNI/CSI this is almost never right"}))

(defn detect-host-namespaces
  [evidence]
  (for [pod (workload-pods evidence)
        :let [spec (:spec pod)
              shared (cond-> []
                       (true? (:hostNetwork spec)) (conj "hostNetwork")
                       (true? (:hostPID spec)) (conj "hostPID")
                       (true? (:hostIPC spec)) (conj "hostIPC"))]
        :when (seq shared)]
    {:severity :warning
     :component (kubectl/namespace-name-of pod)
     :summary (format "pod shares host namespaces: %s" (str/join ", " shared))
     :hint "host namespaces bypass pod isolation and NetworkPolicies"}))

(defn detect-hostpath-volumes
  [evidence]
  (for [pod (workload-pods evidence)
        :let [paths (keep #(-> % :hostPath :path) (-> pod :spec :volumes))]
        :when (seq paths)]
    {:severity :warning
     :component (kubectl/namespace-name-of pod)
     :summary (format "pod mounts hostPath: %s" (str/join ", " paths))
     :hint "a writable hostPath can own the node - prefer volumes or read-only mounts"}))

(def ^:private dangerous-capabilities
  #{"SYS_ADMIN" "NET_ADMIN" "SYS_PTRACE" "SYS_MODULE" "BPF"})

(defn detect-dangerous-capabilities
  [evidence]
  (for [pod (workload-pods evidence)
        container (all-containers pod)
        :let [added (set (map str (-> container :securityContext :capabilities :add)))
              all? (contains? added "ALL")
              found (sort (filter added dangerous-capabilities))]
        :when (or all? (seq found))]
    (if all?
      ;; "ALL" grants every Linux capability - equivalent to privileged, so it
      ;; outranks any specific-capability warning and must not slip through just
      ;; because the literal "ALL" is not in the dangerous set.
      {:severity :critical
       :component (kubectl/namespace-name-of pod)
       :summary (format "container %s adds ALL capabilities" (:name container))
       :hint "adding ALL is equivalent to a privileged container - a node-takeover primitive"}
      {:severity :warning
       :component (kubectl/namespace-name-of pod)
       :summary (format "container %s adds capabilities: %s" (:name container) (str/join ", " found))
       :hint "these capabilities are node-takeover primitives - drop them if the workload survives without"})))

(def detectives
  [{:name "privileged"
    :description "Privileged containers outside platform namespaces"
    :requires ["pods"]
    :detect detect-privileged-containers}
   {:name "host-namespaces"
    :description "Pods sharing hostNetwork/hostPID/hostIPC"
    :requires ["pods"]
    :detect detect-host-namespaces}
   {:name "hostpath"
    :description "Pods mounting hostPath volumes"
    :requires ["pods"]
    :detect detect-hostpath-volumes}
   {:name "capabilities"
    :description "Containers adding dangerous capabilities"
    :requires ["pods"]
    :detect detect-dangerous-capabilities}])
