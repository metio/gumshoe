;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.secrets
  "Reading secrets from a password manager at runtime - nothing is ever
   persisted. A plugin seam: a secrets provider knows how to read a password, a
   named field, search for a path, and check existence for one manager. The
   built-ins are :gopass (default), :pass, :passage, and :pasejo; a plugin
   registers more with `register-provider!` - a native Vault/HTTP backend, an
   in-house store - and env.edn `:secrets {:provider :pass}` selects one.

   For a manager not worth a provider, `:secrets {:provider :command ...}` drives
   any CLI by command templates with {path}/{field} substituted in (the :command
   provider). A legacy `:secrets` with bare templates and no :provider is read as
   :command, so older configs keep working."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [gumshoe.config :as config]
            [gumshoe.shell :as shell]))

;; --- pure helpers ----------------------------------------------------------

(defn resolve-command
  "Pure: fills {path}/{field} placeholders in a command template (an argv vector)
   with the given substitutions, e.g. {:path \"matrix/bot\" :field \"login\"}."
  [template substitutions]
  (mapv (fn [arg]
          (reduce (fn [a [k v]] (str/replace a (str "{" (name k) "}") (str v)))
                  arg substitutions))
        template))

(defn first-line
  "The first non-blank line of a secret - the password, by the pass convention
   that the first line holds it."
  [s]
  (some->> (str/split-lines (str s)) (remove str/blank?) first))

(defn field-in
  "The value of a `name: value` line in a multi-line secret, or nil - how pass,
   passage, and pasejo carry named fields (a login, a token) below the password."
  [s field-name]
  (some (fn [line]
          (let [[k v] (str/split line #":" 2)]
            (when (and v (= (str/trim k) field-name)) (str/trim v))))
        (str/split-lines (str s))))

(defn- run [& argv] (apply shell/stdout-of (remove nil? argv)))

;; --- built-in providers ----------------------------------------------------
;; Each is {:name :binary :password :field :find-path :available?}; a plugin adds
;; more with the same shape.

(def gopass
  "gopass has a native password flag, field getter, and fuzzy search."
  {:name :gopass :binary "gopass"
   :password (fn [path] (run "gopass" "show" "--password" path))
   :field (fn [path field] (run "gopass" "show" path field))
   :find-path (fn [term] (first-line (run "gopass" "search" term)))
   :available? (fn [path] (not (str/blank? (run "gopass" "search" path))))})

(defn- home-relative
  [env-var default-subpath]
  (or (not-empty (System/getenv env-var))
      (str (fs/path (System/getProperty "user.home") default-subpath))))

(defn- pass-like
  "pass and passage: `<binary> show <path>` prints the secret (first line the
   password, `name: value` lines below). No decrypting search, so path lookup and
   existence read the on-disk store directly - fast and prompt-free."
  [nm binary dir ext]
  {:name nm :binary binary
   :password (fn [path] (first-line (run binary "show" path)))
   :field (fn [path field] (field-in (run binary "show" path) field))
   :find-path (fn [term]
                (->> (fs/glob dir (str "**." ext))
                     (map #(-> (fs/relativize dir %) str (str/replace (re-pattern (str "\\." ext "$")) "")))
                     (filter #(str/includes? % term))
                     sort first))
   :available? (fn [path] (fs/exists? (fs/path dir (str path "." ext))))})

(def pass (pass-like :pass "pass" (home-relative "PASSWORD_STORE_DIR" ".password-store") "gpg"))
(def passage (pass-like :passage "passage" (home-relative "PASSAGE_DIR" ".passage/store") "age"))

(def pasejo
  "pasejo (metio's age-based, team-oriented passage re-implementation): the first
   line via `--line 1` (like gopass --password), fields parsed from the full
   secret, and `secret list` for search and existence (no decrypt)."
  {:name :pasejo :binary "pasejo"
   :password (fn [path] (first-line (run "pasejo" "secret" "show" path "--line" "1")))
   :field (fn [path field] (field-in (run "pasejo" "secret" "show" path) field))
   :find-path (fn [term]
                (->> (str/split-lines (run "pasejo" "secret" "list"))
                     (map str/trim)
                     (filter #(str/includes? % term))
                     first
                     not-empty))
   :available? (fn [path]
                 (some #(= (str/trim %) path)
                       (str/split-lines (run "pasejo" "secret" "list"))))})

(def command-defaults
  "The :command provider's default templates (gopass), used when :secrets sets
   templates but not a specific one."
  {:password ["gopass" "show" "--password" "{path}"]
   :field ["gopass" "show" "{path}" "{field}"]
   :search ["gopass" "search" "{path}"]})

;; kept for older configs / tests that referred to it
(def defaults command-defaults)

(defn- command-template
  [key]
  (get (config/value [:secrets] {}) key (get command-defaults key)))

(def command
  "The escape hatch: any CLI, driven by env.edn command templates with
   {path}/{field} substituted. Reads the templates from config on each call, so a
   config edit takes effect without a code change."
  {:name :command
   :binary nil ; resolved from the configured :password template's first word
   :password (fn [path] (apply run (resolve-command (command-template :password) {:path path})))
   :field (fn [path field] (apply run (resolve-command (command-template :field) {:path path :field field})))
   :find-path (fn [term] (first-line (apply run (resolve-command (command-template :search) {:path term}))))
   :available? (fn [path] (not (str/blank? (apply run (resolve-command (command-template :search) {:path path})))))})

(defonce ^:private providers (atom {}))

(defn register-provider!
  "Registers a secrets provider - a map with :name and the fns :password, :field,
   :find-path, :available? (and optionally :binary for the Prerequisites check).
   A plugin adds a native backend the CLI-template model can not express."
  [provider]
  (swap! providers assoc (:name provider) provider))

(defn registered-providers [] (sort (keys @providers)))

(doseq [p [gopass pass passage pasejo command]] (register-provider! p))

;; --- selection + public API ------------------------------------------------

(defn active-name
  "Pure: which provider a :secrets config selects - an explicit :provider, else
   :command when it carries bare templates (a legacy config), else :gopass."
  [secrets-config]
  (cond
    (:provider secrets-config) (:provider secrets-config)
    (:password secrets-config) :command
    :else :gopass))

(defn- active []
  (let [name (active-name (config/value [:secrets] {}))]
    (or (get @providers name) gopass)))

(defn command-name
  "The password-manager binary the books need installed - the active provider's,
   or the first word of the :command provider's configured retrieval command."
  []
  (or (:binary (active))
      (first (command-template :password))))

(defn password
  "The password for a secret path, or nil when it is absent or empty."
  [path]
  (not-empty ((:password (active)) path)))

(defn field
  "A named field of a secret (its login, an access token), or nil when absent."
  [path field-name]
  (when-let [f (:field (active))]
    (not-empty (f path field-name))))

(defn find-path
  "The secret path matching a fuzzy search term (a hostname), or nil - so a book
   locates a secret without hard-coding its full path."
  [term]
  (when-let [f (:find-path (active))]
    (not-empty (f term))))

(defn available?
  "Whether the secret exists - the prerequisites check."
  [path]
  (boolean ((:available? (active)) path)))
