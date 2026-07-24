;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.apiversions
  "Detective for CRD version drift: a resource whose discovery-advertised version
   is not one its CRD actually serves. That is stale aggregated discovery, and it
   silently breaks every client that resolves the kind through discovery (kubectl
   404s it, Flux health-checks time out with NotFound) even though the objects are
   healthy and server-side apply succeeds. Evidence comes from
   gumshoe.apiversions/collect-evidence!; these fns are pure over it."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(defn served-version-names
  "The set of served version names a CRD declares (storage-only or unserved
   versions do not appear in discovery, so they can not drift)."
  [crd]
  (into #{} (comp (filter :served) (map :name))
        (-> crd :spec :versions)))

(defn detect-version-drift
  "One critical per resource whose advertised version the CRD does not serve."
  [evidence]
  (let [advertised (get evidence "api-resources")]
    (for [crd (kubectl/items-of (get evidence "customresourcedefinitions"))
          :let [group (-> crd :spec :group)
                plural (-> crd :spec :names :plural)
                served (served-version-names crd)
                shown (get advertised [group plural])]
          ;; only flag a genuine disagreement: discovery names a version, the CRD
          ;; serves at least one, and the advertised one is not among them.
          :when (and shown (seq served) (not (contains? served shown)))]
      {:severity :critical
       :component (str group "/" plural)
       :summary (format "discovery advertises %s but the CRD serves only %s"
                        shown (str/join ", " (sort served)))
       :hint (str "stale aggregated discovery -- clients that resolve this kind "
                  "through discovery will 404 it (Flux health-checks time out as "
                  "NotFound). Force a refresh: annotate the CRD "
                  "(kubectl annotate crd " plural "." group " refresh=$(date +%s) --overwrite) "
                  "or restart the kube-apiserver, then re-check kubectl api-resources.")})))

(def detectives
  [{:name "version-drift"
    :description "CRDs whose discovery-advertised version is not one the CRD serves (stale aggregated discovery)"
    ;; "customresourcedefinitions" is a plain get; "api-resources" is a custom
    ;; evidence source (registry/register-evidence-source!) backed by
    ;; `kubectl api-resources`. Listing both lets this detective compose into the
    ;; platform scan on the standard collector.
    :requires ["customresourcedefinitions" "api-resources"]
    :detect detect-version-drift}])
