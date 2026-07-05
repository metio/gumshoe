;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.config
  "Host-specific configuration from an env.edn file, so each operator can adapt
   the books to their machine without touching code. The file is optional and
   gitignored; a missing or broken one degrades to an empty config, never an
   error.

   Search order (first found wins): the GUMSHOE_ENV path, ./env.edn,
   $XDG_CONFIG_HOME/gumshoe/env.edn, ~/.config/gumshoe/env.edn.

   ## Environments

   Related infrastructure travels together: the staging Kubernetes cluster, the
   staging ceph cluster, and the staging VPN are one bundle. Declare each bundle
   as a named environment, and pick which one is active by the kubectl context
   you are already on - select the cluster and the ceph host, VPN, and the rest
   come with it.

     {:environments
      {:production {:select {:kubernetes-cluster \"kube.example.org\"}
                    :vpn  {:interface \"wg0\"}
                    :ceph {:mgr-hosts [\"mgr-1.example.org\"]}}
       :staging    {:select {:kubernetes-cluster \"kube.staging.example.org\"}
                    :vpn  {:interface \"wg-staging\"}
                    :ceph {:mgr-hosts [\"mgr-1.staging.example.org\"]}}}
      :defaults {:vpn {:interface \"wg0\"}}}   ; shared fallback

   `env-value` resolves a key against the active environment, then :defaults,
   then the top-level of the map (so a flat, single-environment env.edn keeps
   working unchanged), then the caller's default. The active environment is the
   one whose :select criteria all match the given signals (e.g. the current
   cluster), or an explicit GUMSHOE_ENVIRONMENT override.

   A flat env.edn - no :environments key - is still valid and is read directly:
     {:vpn {:interface \"wg0\"} :ceph {:mgr-hosts [\"mgr-1.example.org\"]}}"
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(def primary-path
  "Where the setup wizard writes env.edn: the working directory, so it sits
   next to the books the operator runs."
  "env.edn")

(defn- search-paths
  []
  (remove nil?
          [(System/getenv "GUMSHOE_ENV")
           "env.edn"
           (some-> (System/getenv "XDG_CONFIG_HOME") (str "/gumshoe/env.edn"))
           (some-> (System/getProperty "user.home") (str "/.config/gumshoe/env.edn"))]))

(defn load-file-safely
  "Reads and parses an env.edn path, or nil when it is missing or unreadable."
  [path]
  (when (and path (fs/exists? path))
    (try
      (let [data (edn/read-string (slurp (str path)))]
        (when (map? data) data))
      (catch Exception _ nil))))

(def ^:private loaded
  (delay (or (some load-file-safely (search-paths)) {})))

(defn all
  []
  @loaded)

(defn value
  "The value at a key path in the config, or default (nil) when absent. This
   reads the top level only; for values that vary per environment use env-value."
  ([path] (value path nil))
  ([path default]
   (let [v (get-in @loaded path)]
     (if (nil? v) default v))))

(defn environments
  "The declared environments, a name -> environment map (empty when none)."
  []
  (get (all) :environments {}))

(defn known-clusters
  "The Kubernetes cluster names the operator has configured: each environment's
   :select :kubernetes-cluster, plus an optional top-level :clusters allow-list.
   A book that requires being on a known cluster checks the current context
   against this set. Empty when nothing is configured - any current cluster then
   passes, since there is no allow-list to enforce."
  ([] (known-clusters (all)))
  ([config]
   ;; :clusters may be written as a single string; concat would then splice it
   ;; character-by-character into the allow-list, so coerce a bare string to a
   ;; one-element sequence.
   (let [clusters (:clusters config)
         clusters (if (string? clusters) [clusters] clusters)]
     (distinct
      (concat (keep #(get-in % [:select :kubernetes-cluster]) (vals (:environments config)))
              clusters)))))

(defn- matches-signals?
  [env signals]
  (let [select (:select env)]
    (and (seq select)
         (every? (fn [[k v]] (= v (get signals k))) select))))

(defn select-environment
  "Pure: the name of the environment whose :select criteria all match the given
   signals (e.g. {:kubernetes-cluster \"kube.example.org\"}), or nil. When several
   match, the most specific one - the most :select criteria - wins, ties broken
   by name. The GUMSHOE_ENVIRONMENT override is applied on top of
   this by active-environment."
  [envs signals]
  (->> envs
       (filter (fn [[_ env]] (matches-signals? env signals)))
       (sort-by (fn [[nm env]] [(- (count (:select env))) (str nm)]))
       ffirst))

(defn active-environment
  "The name of the active environment, or nil: an explicit
   GUMSHOE_ENVIRONMENT override when it names a declared
   environment, otherwise the one selected from the signals."
  [signals]
  (let [envs (environments)
        override (some-> (System/getenv "GUMSHOE_ENVIRONMENT")
                         str/trim
                         (as-> s (when-not (str/blank? s) (keyword s))))]
    (if (contains? envs override)
      override
      (select-environment envs signals))))

(defn resolve-value
  "Pure: a key path resolved against a config map for a given active
   environment name - the environment first, then :defaults, then the top level
   (so a flat env.edn keeps working), then the caller's default."
  [config active path default]
  (let [candidates [(when active (get-in config (into [:environments active] path)))
                    (get-in config (into [:defaults] path))
                    (get-in config path)]]
    (if-let [v (first (filter some? candidates))]
      v
      default)))

(defn env-value
  "A key path resolved for the active environment (chosen from signals, e.g.
   {:kubernetes-cluster cluster}), falling back to :defaults, then the top-level
   of the config (so a flat env.edn keeps working), then the caller's default."
  ([signals path] (env-value signals path nil))
  ([signals path default]
   (resolve-value (all) (active-environment signals) path default)))

(defn active-config
  "The config as one map for the active environment: the top level, then
   :defaults, then the active environment layered on top (later wins). Lets a
   detective read operator expectations - e.g. which cluster-admins are known -
   for wherever it is pointed, without knowing about environments. A shallow
   merge, so a given top-level key is taken whole from the most specific layer
   that sets it."
  ([signals] (active-config (all) (active-environment signals)))
  ([config active]
   (merge (dissoc config :environments :defaults)
          (:defaults config)
          (get-in config [:environments active]))))

(defn- blank->nil [s] (when-not (str/blank? (str s)) (str s)))

(defn from-answers
  "Builds an env.edn body from the wizard's answers, omitting anything the
   operator skipped (a nil or blank answer). :inputs is a seq of [env-path value]
   pairs from the registry-driven questions, each assoc'd at its path, so the
   wizard grows simply by declaring another input. With an :environment name it
   produces an :environments entry keyed by that name (auto-selected by the
   :kubernetes-cluster it is tied to); without one it produces a flat, single
   environment config. An all-skipped run yields an empty map."
  [{:keys [environment kubernetes-cluster vpn-interface ceph-mgr-host capabilities inputs]}]
  (let [body (cond-> {}
               (blank->nil kubernetes-cluster) (assoc-in [:select :kubernetes-cluster] (blank->nil kubernetes-cluster))
               (seq capabilities) (assoc :capabilities (vec capabilities))
               (blank->nil vpn-interface) (assoc-in [:vpn :interface] (blank->nil vpn-interface))
               (blank->nil ceph-mgr-host) (assoc-in [:ceph :mgr-hosts] [(blank->nil ceph-mgr-host)]))
        body (reduce (fn [b [path v]]
                       (if-let [vv (blank->nil v)] (assoc-in b path vv) b))
                     body
                     inputs)]
    (cond
      (empty? body) {}
      (blank->nil environment) {:environments {(keyword (blank->nil environment)) body}}
      ;; no environment name - a flat config, so the cluster selector is moot
      :else (dissoc body :select))))

(defn merge-answers
  "Merges a new environment body (from from-answers) into an existing config,
   so running the wizard once per environment accumulates them rather than
   overwriting. New values win on conflict."
  [existing new-config]
  (if (contains? new-config :environments)
    (update existing :environments merge (:environments new-config))
    (merge existing new-config)))

(defn render
  "Pretty EDN for an env.edn map, with the license header."
  [config]
  (str ";; SPDX-FileCopyrightText: The gumshoe Authors\n"
       ";; SPDX-License-Identifier: 0BSD\n\n"
       ";; Written by runbooks/setup/init.clj - edit freely.\n"
       (with-out-str (pprint/pprint config))))

(defn write!
  "Writes an env.edn map to the primary path and returns the path."
  [config]
  (spit primary-path (render config))
  primary-path)

(defn exists?
  []
  (fs/exists? primary-path))
