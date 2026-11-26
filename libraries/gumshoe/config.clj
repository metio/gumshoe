;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.config
  "Host-specific configuration from an env.edn file, so each operator can adapt
   the books to their machine without touching code. The file is optional and
   gitignored; a missing one degrades to an empty config, never an error.

   Search order (the first file that exists wins): the GUMSHOE_ENV path,
   ./env.edn, $XDG_CONFIG_HOME/gumshoe/env.edn, ~/.config/gumshoe/env.edn. A
   file that exists but cannot be read is never skipped for the next one: it
   is named on stderr, nothing of it applies, and the cluster allow-list
   refuses every cluster until it is fixed (see load-first).

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

(defn clusters-problem
  "Pure: why the top-level :clusters allow-list of config can not be read as
   cluster names, or nil when it is absent or readable. A readable :clusters
   is one non-blank string or a list or set of them. An unquoted hostname
   reads as a symbol in EDN, and a symbol, keyword, number or map names no
   cluster this check could compare by string."
  [config]
  (let [clusters (:clusters config)
        name? #(and (string? %) (not (str/blank? %)))]
    (when-not (or (nil? clusters)
                  (name? clusters)
                  (and (or (sequential? clusters) (set? clusters))
                       (every? name? clusters)))
      (format ":clusters is a cluster name string or a list of them, such as [\"kube.example.org\"], not %s"
              (pr-str clusters)))))

(defn normalize
  "Pure: a config map with every part of the wrong shape dropped, as
   {:config .. :warnings [..]}. Every reader below destructures :environments
   as a map of name to environment map, each :select as a map of signal to
   value, and merges :defaults as a map; a part that is not would throw in
   each of them. So a non-map :environments or :defaults is dropped, an
   environment that is not a map is dropped, and a :select that is not a map
   is dropped from its environment, which then is only usable through
   GUMSHOE_ENVIRONMENT. Each drop is named in a warning.

   A dropped :environments, environment or :select may have named a cluster
   of the allow-list known-clusters builds, and a malformed :clusters names
   none that can be read. Either is kept under ::unread-clusters, so the
   allow-list matches no cluster instead of losing those names and letting
   any cluster pass."
  [config]
  (let [warnings (transient [])
        unread (transient [])
        warn! (fn [fmt & args] (conj! warnings (apply format fmt args)))
        unread! (fn [fmt & args]
                  (let [message (apply format fmt args)]
                    (conj! warnings message)
                    (conj! unread message)))
        envs (:environments config)
        envs (cond
               (nil? envs) nil
               (not (map? envs)) (do (unread! ":environments is a map of name to environment, not %s - ignored" (pr-str envs))
                                     nil)
               :else (into {}
                           (keep (fn [[nm env]]
                                   (cond
                                     (not (map? env))
                                     (do (unread! ":environments -> %s is a map, not %s - ignored" (pr-str nm) (pr-str env))
                                         nil)
                                     (and (contains? env :select) (not (map? (:select env))))
                                     (do (unread! ":environments -> %s -> :select is a map of signal to value, such as {:kubernetes-cluster \"kube.example.org\"}, not %s - ignored"
                                                (pr-str nm) (pr-str (:select env)))
                                         [nm (dissoc env :select)])
                                     :else [nm env])))
                           envs))
        _ (when-let [problem (clusters-problem config)] (unread! "%s" problem))
        defaults (:defaults config)
        config (cond-> config
                 (contains? config :environments) (dissoc :environments)
                 (some? envs) (assoc :environments envs)
                 (and (some? defaults) (not (map? defaults)))
                 (as-> c (do (warn! ":defaults is a map, not %s - ignored" (pr-str defaults))
                             (dissoc c :defaults))))
        unread (persistent! unread)
        config (cond-> config (seq unread) (assoc ::unread-clusters unread))]
    {:config config :warnings (persistent! warnings)}))

(defn- read-existing
  "Reads the env.edn at an existing path as {:config ..} or {:problem ..}.
   An empty file is an empty config; a file that is not EDN, not a map or not
   readable at all is a problem. A part of the wrong shape is dropped with a
   warning on stderr (see normalize), so the rest of the file still applies."
  [path]
  (try
    (let [data (edn/read-string (slurp (str path)))]
      (cond
        (nil? data) {:config {}}
        (not (map? data)) {:problem (format "the file holds %s, not a map" (pr-str data))}
        :else (let [{:keys [config warnings]} (normalize data)]
                (binding [*out* *err*]
                  (doseq [warning warnings]
                    (println (format "warning: %s: %s" path warning))))
                {:config config})))
    (catch Exception e
      {:problem (str "the file is not readable EDN: " (or (ex-message e) (str e)))})))

(defn load-file-safely
  "Reads and parses an env.edn path, or nil when it is missing or unreadable.
   A part of the wrong shape is dropped with a warning on stderr (see
   normalize), so the rest of the file still applies."
  [path]
  (when (and path (fs/exists? path))
    (let [{:keys [config problem]} (read-existing path)]
      (when-not problem config))))

(defn load-first
  "The config of the first of `paths` that exists, or {} when none does. The
   first existing file wins even when it cannot be read: falling through to
   the next file would apply another operator's allow-list and hosts without
   a word. A file that cannot be read is named on stderr with the reason and
   yields a config whose cluster allow-list matches no cluster, so every book
   that checks the cluster refuses until the file is fixed."
  [paths]
  (if-let [path (first (filter #(and % (fs/exists? %)) paths))]
    (let [{:keys [config problem]} (read-existing path)]
      (if problem
        (let [reason (format "%s: %s" path problem)]
          (binding [*out* *err*]
            (println (format "warning: %s - no configuration applies, and every cluster is refused until it is fixed" reason)))
          {::unread-clusters [reason]})
        config))
    {}))

(defn file-problems
  "Why the env.edn at path can not be rewritten from what load-file-safely
   reads, or nil when it is missing or reads whole. load-file-safely drops a
   file that is not EDN or not a map, and every part normalize warns about,
   so writing its result back would erase those parts from disk."
  [path]
  (when (and path (fs/exists? path))
    (try
      (let [data (edn/read-string (slurp (str path)))]
        (cond
          (nil? data) nil
          (not (map? data)) [(format "the file holds %s, not a map" (pr-str data))]
          :else (seq (:warnings (normalize data)))))
      (catch Exception e
        [(str "the file is not readable EDN: " (or (ex-message e) (str e)))]))))

(def ^:private loaded
  (delay (load-first (search-paths))))

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

(defn- environments-problems
  "Pure: the parts of a raw :environments that could name a cluster but are
   not of a shape known-clusters reads."
  [config]
  (let [envs (:environments config)]
    (cond
      (nil? envs) nil
      (not (map? envs)) [":environments is not a map of name to environment"]
      :else (for [[nm env] envs
                  :when (or (not (map? env))
                            (and (contains? env :select) (not (map? (:select env)))))]
              (format ":environments -> %s is not an environment with a :select map" (pr-str nm))))))

(defrecord UnreadAllowList [reason]
  Object
  (toString [_] (str "(none that can be read: " reason ")")))

(defn unread-allow-list?
  "Whether x is the UnreadAllowList entry known-clusters adds."
  [x]
  (instance? UnreadAllowList x))

(defn known-clusters
  "The Kubernetes cluster names the operator has configured: each environment's
   :select :kubernetes-cluster, plus an optional top-level :clusters allow-list.
   A book that requires being on a known cluster checks the current context
   against this set. Empty when nothing is configured - any current cluster then
   passes, since there is no allow-list to enforce.

   A part of the configuration that should name clusters but can not be read -
   a malformed :clusters, or an environment normalize dropped - adds an
   UnreadAllowList entry, which equals no cluster name. The allow-list is then
   never empty, so the gate stays closed for every cluster the readable part
   does not name instead of opening for all of them, and the entry prints the
   reason where the gate names the configured clusters."
  ([] (known-clusters (all)))
  ([config]
   ;; :clusters may be written as a single string; concat would then splice it
   ;; character-by-character into the allow-list, so coerce a bare string to a
   ;; one-element sequence. A :select :kubernetes-cluster may itself be a set of
   ;; aliases (see matches-signals?); splice its members in rather than letting
   ;; the whole set count as one entry.
   (let [clusters (:clusters config)
         readable (cond (string? clusters) [clusters]
                        (or (sequential? clusters) (set? clusters)) (filter string? clusters))
         problems (distinct (concat (::unread-clusters config)
                                    (keep identity [(clusters-problem config)])
                                    (environments-problems config)))
         select-clusters (mapcat (fn [env]
                                   (let [c (get-in env [:select :kubernetes-cluster])]
                                     (cond (set? c) c
                                           (some? c) [c])))
                                 (let [envs (:environments config)]
                                   (when (map? envs) (filter map? (vals envs)))))]
     (cond-> (vec (distinct (concat select-clusters readable)))
       (seq problems) (conj (->UnreadAllowList (str/join "; " problems)))))))

(defn- signal-matches?
  "Whether one :select value matches the live signal. A scalar matches on
   equality; a set matches when it contains the signal - so an environment can
   list several acceptable names (e.g. a cluster's full name and a short alias)
   that all resolve to it."
  [v signal]
  (if (set? v)
    (contains? v signal)
    (= v signal)))

(defn- matches-signals?
  [env signals]
  (let [select (:select env)]
    (and (map? select)
         (seq select)
         (every? (fn [[k v]] (signal-matches? v (get signals k))) select))))

(defn select-environment
  "Pure: the name of the environment whose :select criteria all match the given
   signals (e.g. {:kubernetes-cluster \"kube.example.org\"}), or nil. When several
   match, the most specific one - the most :select criteria - wins, ties broken
   by name. The GUMSHOE_ENVIRONMENT override is applied on top of
   this by active-environment."
  [envs signals]
  (->> (when (map? envs) envs)
       (filter (fn [[_ env]] (and (map? env) (matches-signals? env signals))))
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
   (let [layer #(when (map? %) %)]
     (merge (dissoc config :environments :defaults ::unread-clusters)
            (layer (:defaults config))
            (layer (when (map? (:environments config)) (get (:environments config) active)))))))

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

(defn deep-merge
  "b merged into a: maps are merged key by key at every depth, and any other
   value of b - a string, a vector, a map replacing a non-map - wins."
  [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

(defn merge-answers
  "Merges a new environment body (from from-answers) into an existing config,
   so running the wizard once per environment accumulates them rather than
   overwriting. The merge is deep: the wizard asks only for what the config
   does not set yet, so a run naming an environment again holds only the new
   answers, and every value it did not ask about must stay. A new value wins
   where both set one."
  [existing new-config]
  (deep-merge existing new-config))

(defn- leaf-paths
  "The path of every value in m that is not itself a non-empty map, to its
   value."
  [m]
  (into {} (mapcat (fn [[k v]]
                     (if (and (map? v) (seq v))
                       (map (fn [[path leaf]] [(into [k] path) leaf]) (leaf-paths v))
                       [[[k] v]]))
                   m)))

(defn changes
  "What writing after over before changes, as {:added [path ..] :changed
   [path ..] :removed [path ..]}, each a sorted vector of the key paths of
   the values that differ."
  [before after]
  (let [b (leaf-paths (or before {}))
        a (leaf-paths (or after {}))
        sorted #(vec (sort-by pr-str %))]
    {:added (sorted (remove #(contains? b %) (keys a)))
     :changed (sorted (filter #(and (contains? b %) (not= (b %) (a %))) (keys a)))
     :removed (sorted (remove #(contains? a %) (keys b)))}))

(defn change-items
  "The confirmation lines naming each value a write adds, changes or
   removes, as changes reports them."
  [{:keys [added changed removed]}]
  (let [line (fn [verb path] (str verb " " (str/join " " (map pr-str path))))]
    (concat (map (partial line "adds") added)
            (map (partial line "changes") changed)
            (map (partial line "REMOVES") removed))))

(defn render
  "Pretty EDN for an env.edn map, with the license header."
  [config]
  ;; REUSE-IgnoreStart -- the strings below are the license header written INTO a
  ;; generated env.edn, not a license declaration for this source file.
  (str ";; SPDX-FileCopyrightText: The gumshoe Authors\n"
       ";; SPDX-License-Identifier: 0BSD\n\n"
       ";; Written by runbooks/setup/init.clj - edit freely.\n"
       (with-out-str (pprint/pprint (dissoc config ::unread-clusters)))))
;; REUSE-IgnoreEnd

(defn backup-dir
  "Where env.edn backups are kept: $XDG_STATE_HOME/gumshoe/env-backups, or
   ~/.local/state/gumshoe/env-backups when XDG_STATE_HOME is unset, empty or
   relative (the XDG spec says to ignore a relative one). The directory is
   outside every working tree, so a backup of an env.edn holding a webhook
   URL or another secret is never committed by accident, whatever the
   casebook's .gitignore says."
  ([] (backup-dir (System/getenv "XDG_STATE_HOME") (System/getProperty "user.home")))
  ([xdg-state-home home]
   (let [base (if (and (not (str/blank? xdg-state-home)) (fs/absolute? xdg-state-home))
                xdg-state-home
                (str (fs/path home ".local" "state")))]
     (str (fs/path base "gumshoe" "env-backups")))))

(defn- sha256-hex
  [^String s]
  (let [digest (.digest (java.security.MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn backup-path
  "Where the env.edn at path is kept before it is rewritten at instant: in
   dir (backup-dir by default), named after a hash of path's absolute form
   and the time to the millisecond, so backups of different casebooks never
   collide and each rewrite keeps its own copy. All backups of one env.edn
   share the hash prefix."
  ([path instant] (backup-path (backup-dir) path instant))
  ([dir path ^java.time.LocalDateTime instant]
   (let [absolute (str (fs/normalize (fs/absolutize path)))]
     (str (fs/path dir (str (subs (sha256-hex absolute) 0 16) "-"
                            (.format instant (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss-SSS"))
                            ".edn.bak"))))))

(defn- keep-backup!
  "Copies the file at path to backup, creating backup with mode 0600 before
   any content goes in and its directory with mode 0700, so no other user
   can read a copy of a secret at any moment. Refuses to overwrite an
   existing file at backup."
  [path backup]
  (let [dir (fs/parent backup)]
    (fs/create-dirs dir {:posix-file-permissions "rwx------"})
    (fs/set-posix-file-permissions dir "rwx------")
    (fs/create-file backup {:posix-file-permissions "rw-------"})
    (fs/write-bytes backup (fs/read-all-bytes path))
    backup))

(defn write!
  "Writes an env.edn map to path (the primary path by default) and returns
   {:path path :backup <where the previous file was copied, or nil>}. A file
   already at path is copied to backup (backup-path for now by default)
   first and never overwritten, so a write can always be undone; when the
   copy fails nothing is written. A caller that names the backup in its
   confirmation passes the same backup here."
  ([config] (write! config primary-path))
  ([config path] (write! config path (backup-path path (java.time.LocalDateTime/now))))
  ([config path backup]
   (let [kept (when (fs/exists? path)
                (keep-backup! path backup))]
     (spit path (render config))
     {:path path :backup kept})))

(defn exists?
  []
  (fs/exists? primary-path))
