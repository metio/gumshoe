;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.firebook
  "Shared plumbing for firebooks. Every drill burns inside its own namespace,
   so extinguishing is a single namespace delete, and a stray drill can never
   touch production workloads. The drill is over when the detectives are
   green again."
  (:require [cheshire.core :as json]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

(def drill-namespace "fire-drill")

(defn namespace-manifest
  []
  {:apiVersion "v1"
   :kind "Namespace"
   :metadata {:name drill-namespace
              :labels {:app.kubernetes.io/managed-by "gumshoe"}}})

(defn deployment-manifest
  [{:keys [name image command replicas] :or {replicas 1}}]
  {:apiVersion "apps/v1"
   :kind "Deployment"
   :metadata {:name name :namespace drill-namespace}
   :spec {:replicas replicas
          :selector {:matchLabels {:app name}}
          :template {:metadata {:labels {:app name}}
                     :spec {:containers [(cond-> {:name name :image image}
                                           command (assoc :command command))]}}}})

(defn pvc-manifest
  [{:keys [name storage-class size] :or {size "1Gi"}}]
  {:apiVersion "v1"
   :kind "PersistentVolumeClaim"
   :metadata {:name name :namespace drill-namespace}
   :spec {:accessModes ["ReadWriteOnce"]
          :storageClassName storage-class
          :resources {:requests {:storage size}}}})

(defn ignite!
  "Applies the drill manifests inside the drill namespace. Returns true when
   the fire is burning."
  [context manifests]
  (let [manifest-list {:apiVersion "v1"
                       :kind "List"
                       :items (vec (cons (namespace-manifest) manifests))}
        result (shell/execute-with-stdin (json/encode manifest-list)
                                         "kubectl" (str "--context=" context)
                                         "apply" "--filename" "-")]
    (if (zero? (:exit result))
      (do (stdout/ok (format "fire started in namespace %s" drill-namespace))
          (stdout/err-println "🔥 Let the team find it with the detectives, e.g.:")
          (stdout/err-println "   bb runbooks/detectives/cluster.clj")
          (stdout/err-println "   The drill is over when the findings are green again.")
          (stdout/err-println "   Extinguish with --extinguish once you are done.")
          true)
      (do (stdout/error (format "could not start the fire:\n%s" (:err result)))
          false))))

(defn extinguish!
  "Deletes the drill namespace and everything in it, then verifies it is gone."
  [context]
  (let [result (shell/execute "kubectl" (str "--context=" context)
                              "delete" "namespace" drill-namespace "--ignore-not-found")]
    (if-not (zero? (:exit result))
      (do (stdout/error (format "could not extinguish the fire:\n%s" (:err result))) false)
      (do (stdout/ok "fire extinguished, drill namespace deleted")
          (stdout/print-section "🔎 Post-check")
          (verify/all [{:description "the fire-drill namespace is gone"
                        :timeout 180
                        :interval 10
                        :check #(nil? (kubectl/get-cluster-resource context "namespaces" drill-namespace))}])))))

(defn burning-check
  "Post-check map proving that a drill resource exists inside the namespace."
  [context type name]
  {:description (format "%s %s/%s exists - the fire is burning" type drill-namespace name)
   :timeout 30
   :check #(some? (kubectl/get-namespaced-resource context drill-namespace type name))})
