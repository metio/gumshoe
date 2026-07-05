;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.registry
  "The registry of detectives, grouped into scopes that compose: a book runs one
   scope, or the whole registry. It is a plugin seam - a plugin adds its own
   detectives to a scope (or a brand new scope) with `register!`, and because a
   detective book resolves its scope when it runs (after plugins load), those
   plugin detectives join the scan with no change to the core."
  (:require [infra.detectives.calico :as calico]
            [infra.detectives.capacity :as capacity]
            [infra.detectives.certificates :as certificates]
            [infra.detectives.cnpg :as cnpg]
            [infra.detectives.controlplane :as controlplane]
            [infra.detectives.csi :as csi]
            [infra.detectives.db-operator :as db-operator]
            [infra.detectives.disruption :as disruption]
            [infra.detectives.events :as events]
            [infra.detectives.flux :as flux]
            [infra.detectives.gateway :as gateway]
            [infra.detectives.mail :as mail]
            [infra.detectives.monitoring :as monitoring]
            [infra.detectives.network :as network]
            [infra.detectives.nodes :as nodes]
            [infra.detectives.pod-security :as pod-security]
            [infra.detectives.pods :as pods]
            [infra.detectives.quotas :as quotas]
            [infra.detectives.rbac :as rbac]
            [infra.detectives.storage :as storage]
            [infra.detectives.workloads :as controllers]))

(defonce ^:private registry (atom {}))

(defn register!
  "Adds detectives to a scope, creating it if new. Built-ins register the core
   scopes below; a plugin registers into an existing scope (to enrich a scan) or
   a new one (to add its own)."
  [scope detectives]
  (swap! registry update scope (fnil into []) (vec detectives)))

(defn all
  "Every registered detective, de-duplicated by :name (in case a plugin and a
   built-in share a name)."
  []
  (reduce (fn [acc detective]
            (if (some #(= (:name %) (:name detective)) acc) acc (conj acc detective)))
          []
          (apply concat (vals @registry))))

(defn for-scope
  "The detectives in a scope; :all spans every scope. Resolved when a book runs,
   so plugin-registered detectives are included."
  [scope]
  (if (= scope :all) (all) (vec (get @registry scope []))))

;; --- the built-in scopes ---------------------------------------------------

(register! :platform (concat controlplane/detectives nodes/detectives calico/detectives csi/detectives))
(register! :workloads (concat controllers/detectives pods/detectives storage/detectives disruption/detectives
                              quotas/detectives capacity/detectives))
(register! :tls certificates/detectives)
(register! :gitops flux/detectives)
(register! :databases (concat cnpg/detectives db-operator/detectives))
(register! :observability monitoring/detectives)
(register! :signals events/detectives)
(register! :security (concat rbac/detectives pod-security/detectives network/detectives))
(register! :traffic gateway/detectives)
(register! :mail mail/detectives)
