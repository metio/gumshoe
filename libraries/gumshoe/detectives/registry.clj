;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.registry
  "The registry of detectives, grouped into scopes that compose: a book runs one
   scope, or the whole registry. It is a plugin seam - a plugin adds its own
   detectives to a scope (or a brand new scope) with `register!`, and because a
   detective book resolves its scope when it runs (after plugins load), those
   plugin detectives join the scan with no change to the core."
  (:require [gumshoe.apiversions :as apiversions]
            [gumshoe.detectives.apiversions :as apiversions-detectives]
            [gumshoe.detectives.capacity :as capacity]
            [gumshoe.detectives.controlplane :as controlplane]
            [gumshoe.detectives.csi :as csi]
            [gumshoe.detectives.disruption :as disruption]
            [gumshoe.detectives.events :as events]
            [gumshoe.detectives.expectations :as expectations]
            [gumshoe.detectives.ipfamily :as ipfamily]
            [gumshoe.detectives.mail :as mail]
            [gumshoe.detectives.network :as network]
            [gumshoe.detectives.nodes :as nodes]
            [gumshoe.detectives.pod-security :as pod-security]
            [gumshoe.detectives.pods :as pods]
            [gumshoe.detectives.quotas :as quotas]
            [gumshoe.detectives.rbac :as rbac]
            [gumshoe.detectives.storage :as storage]
            [gumshoe.detectives.workloads :as controllers]
            [gumshoe.kubectl :as kubectl]))

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

;; --- evidence collectors ----------------------------------------------------

(defn- get-all-collector
  "The default collector: read every object of a resource type. Used for any
   evidence key with no collector of its own, i.e. every plain resource type."
  [context type]
  (kubectl/get-all context type))

(defonce ^:private collectors (atom {}))

(defn register-collector!
  "Registers how the evidence for a key is fetched. collect-evidence! resolves a
   collector for each :requires key and calls (collector context key); the plain
   resource reads and a custom read (e.g. one backed by `kubectl api-resources`)
   go through the same seam, so a detective needing custom evidence composes into
   a shared scan. A collector takes [context key] - a custom one that ignores the
   key is fine."
  [key collector]
  (swap! collectors assoc key collector))

(defn collector-for
  "The collector for an evidence key: its registered one, or the get-all default."
  [key]
  (get @collectors key get-all-collector))

;; --- the built-in scopes ---------------------------------------------------

(register! :platform (concat controlplane/detectives nodes/detectives csi/detectives expectations/detectives
                             ipfamily/detectives apiversions-detectives/detectives))
;; the api-version-drift detective reads `kubectl api-resources` (not a get), so
;; it registers that collector for every scan it joins. The collector takes
;; [context key]; the key is fixed here, so it is ignored.
(register-collector! "api-resources" (fn [context _key] (apiversions/advertised-versions context)))
(register! :workloads (concat controllers/detectives pods/detectives storage/detectives disruption/detectives
                              quotas/detectives capacity/detectives))
(register! :signals events/detectives)
(register! :security (concat rbac/detectives pod-security/detectives network/detectives))
(register! :mail mail/detectives)
