;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.registry
  "The registry of detectives, grouped into scopes that compose: a book runs one
   scope, or the whole registry. It is a plugin seam - a plugin adds its own
   detectives to a scope (or a brand new scope) with `register!`, and because a
   detective book resolves its scope when it runs (after plugins load), those
   plugin detectives join the scan with no change to the core."
  (:require [gumshoe.detectives.calico :as calico]
            [gumshoe.detectives.capacity :as capacity]
            [gumshoe.detectives.certificates :as certificates]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.controlplane :as controlplane]
            [gumshoe.detectives.csi :as csi]
            [gumshoe.detectives.db-operator :as db-operator]
            [gumshoe.detectives.disruption :as disruption]
            [gumshoe.detectives.events :as events]
            [gumshoe.detectives.gateway :as gateway]
            [gumshoe.detectives.mail :as mail]
            [gumshoe.detectives.monitoring :as monitoring]
            [gumshoe.detectives.network :as network]
            [gumshoe.detectives.nodes :as nodes]
            [gumshoe.detectives.pod-security :as pod-security]
            [gumshoe.detectives.pods :as pods]
            [gumshoe.detectives.quotas :as quotas]
            [gumshoe.detectives.rbac :as rbac]
            [gumshoe.detectives.storage :as storage]
            [gumshoe.detectives.workloads :as controllers]))

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
(register! :databases (concat cnpg/detectives db-operator/detectives))
(register! :observability monitoring/detectives)
(register! :signals events/detectives)
(register! :security (concat rbac/detectives pod-security/detectives network/detectives))
(register! :traffic gateway/detectives)
(register! :mail mail/detectives)
