;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.inputs
  "The registry of host-specific inputs the books need, and how each is
   resolved. A book must never hard-code an operator's hostnames: every such
   value is declared here once - where it lives in env.edn, the flag that
   overrides it, and the prompt the setup wizard uses to capture it. One
   declaration drives three things: a book's runtime lookup, the setup wizard's
   questions, and env.edn migration (which inputs a stored file is missing).

   Resolution order for a value: the book's CLI flag, then env.edn for the
   active environment, then the declared default. A book therefore reads
   `(inputs/value :matrix-host opts)` and gets the operator's value wherever it
   lives, with the flag always winning for a one-off override."
  (:require [clojure.string :as str]
            [gumshoe.config :as config]
            [gumshoe.kubectl :as kubectl]))

(def registry
  "Every host-specific input, keyed. :flag is the book option that overrides it;
   :env-path is where it lives in env.edn; :prompt drives the setup wizard;
   :default is the last resort (its absence means the operator must set it or
   the book asks each run)."
  ;; The Matrix DETECTIVE investigates a homeserver (:matrix). Change
  ;; ANNOUNCERS (:announce, a list) are configured by hand in env.edn - a list
  ;; of typed announcers does not fit the flat scalar-input wizard - so they are
  ;; documented in env.edn.example rather than asked for here.
  ;; No baked-in host defaults: every host is the operator's own, captured by the
  ;; wizard or a flag. An unset value falls through to the book asking (or a
  ;; clear error), never to a guessed hostname.
  [{:key :matrix-domain :flag :domain :env-path [:matrix :domain]
    :prompt "Matrix server_name to investigate, e.g. example.org (where .well-known lives)"}
   {:key :matrix-host :flag :host :env-path [:matrix :host]
    :prompt "Matrix delegated homeserver host to investigate, e.g. synapse.example.org"}
   {:key :loki-namespace :flag :namespace :env-path [:loki :namespace]
    :prompt "Namespace the Loki microservices run in"}
   {:key :restic-repository :flag :repository :env-path [:restic :repository]
    :prompt "Default restic repository (leave unset to be asked each run)"}
   {:key :opennebula-frontend :flag :frontend :env-path [:opennebula :frontend]
    :prompt "OpenNebula frontend host to SSH into"}
   {:key :grafana-url :flag :grafana :env-path [:grafana :url]
    :prompt "Grafana base URL, e.g. https://grafana.example.org"}
   {:key :alertmanager-host :flag :alertmanager :env-path [:alertmanager :host]
    :prompt "Alertmanager host, e.g. alertmanager.example.org"}
   {:key :prometheus-url :flag :prometheus :env-path [:prometheus :url]
    :prompt "Prometheus base URL, e.g. https://prometheus.example.org"}
   {:key :prometheus-rules-directory :env-path [:prometheus :rules-directory]
    :prompt "Directory with Prometheus alerting rule files"}
   {:key :dns-zone :flag :zone :env-path [:dns :zone]
    :prompt "Primary DNS zone to investigate, e.g. example.org"}
   {:key :dns-server :flag :server :env-path [:dns :server]
    :prompt "DNS server used as the resolution entry point, e.g. dns.example.org"}
   {:key :mail-domain :flag :domain :env-path [:mail :domain]
    :prompt "Mail domain for MX/SPF/DMARC/DKIM checks, e.g. example.org"}
   {:key :mail-host :flag :host :env-path [:mail :host]
    :prompt "Mail server host to probe, e.g. mail.example.org"}])

(def ^:private by-key
  (into {} (map (juxt :key identity)) registry))

(defn- present
  "A non-blank value, or nil - so a blank flag or env value counts as absent and
   resolution falls through to the next source."
  [v]
  (when (and (some? v) (not (str/blank? (str v)))) v))

(defn resolve-one
  "Pure resolution of one input: the book's flag (from opts), then env.edn (via
   env-lookup, a (fn [path] ...)), then the declared default. Injecting the
   lookup keeps this free of I/O and trivially testable."
  [entry opts env-lookup]
  (or (present (get opts (:flag entry (:key entry))))
      (present (env-lookup (:env-path entry)))
      (:default entry)))

(defn current-signals
  "The signals that pick the active environment - the cluster you are on - safe
   to call when kubectl is absent or there is no current cluster."
  []
  {:kubernetes-cluster (try (kubectl/current-cluster) (catch Exception _ nil))})

(defn value
  "The resolved value for a registered input key given a book's parsed opts:
   the flag, then env.edn for the active environment, then the declared default."
  ([key opts] (value key opts (current-signals)))
  ([key opts signals]
   (resolve-one (get by-key key) opts (fn [path] (config/env-value signals path)))))

(defn missing
  "The registered inputs the given config does not set for the active
   environment - what a stored env.edn is missing since the registry grew. This
   drives the wizard's migration pass: exactly the inputs worth asking for now."
  [config active]
  (filter #(nil? (present (config/resolve-value config active (:env-path %) nil)))
          registry))
