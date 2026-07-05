;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.quotas
  "Detective for ResourceQuotas nearing or at their limit. Maps to the
   kube-prometheus KubeQuotaAlmostFull / KubeQuotaFullyUsed alerts, computed
   directly from each quota's used-versus-hard status."
  (:require [infra.kubectl :as kubectl]
            [infra.quantity :as quantity]))

(def ^:private warn-ratio 0.9)

(defn detect-quota-pressure
  [evidence]
  (for [quota (kubectl/items-of (get evidence "resourcequotas"))
        resource (keys (-> quota :status :hard))
        :let [used (get-in quota [:status :used resource])
              hard (get-in quota [:status :hard resource])
              ratio (quantity/ratio used hard)]
        :when (and ratio (>= ratio warn-ratio))]
    {:severity (if (>= ratio 1.0) :critical :warning)
     :component (format "%s (%s)" (kubectl/namespace-name-of quota) (name resource))
     :summary (format "quota is %.0f%% used (%s of %s)" (* 100 ratio) used hard)
     :hint (if (>= ratio 1.0)
             "the quota is exhausted - new objects of this kind are rejected"
             "raise the quota or free usage before it blocks new objects")}))

(def detectives
  [{:name "resource-quotas"
    :description "ResourceQuotas nearing or at their limit"
    :requires ["resourcequotas"]
    :detect detect-quota-pressure}])
