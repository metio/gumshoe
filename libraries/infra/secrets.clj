;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.secrets
  "Reading secrets from a password manager at runtime - nothing is ever
   persisted. gopass by default; another manager (pass, 1Password, Bitwarden,
   Vault, ...) is configured in env.edn under :secrets by giving the command
   templates, with {path} and {field} substituted in. So a different CLI, its
   own flags, and even a different secret-path structure are all just
   configuration - no book changes to switch managers."
  (:require [clojure.string :as str]
            [infra.config :as config]
            [infra.shell :as shell]))

(def defaults
  "The gopass commands, used when env.edn does not configure :secrets. {path} is
   the secret's path; {field} the name of a field within it. :search locates a
   secret by a fuzzy term (a hostname) and its first line is taken as the path."
  {:password ["gopass" "show" "--password" "{path}"]
   :field ["gopass" "show" "{path}" "{field}"]
   :search ["gopass" "search" "{path}"]})

(defn resolve-command
  "Pure: fills {path}/{field} placeholders in a command template with the given
   substitutions (a map like {:path \"matrix/bot\" :field \"login\"})."
  [template substitutions]
  (mapv (fn [arg]
          (reduce (fn [a [k v]] (str/replace a (str "{" (name k) "}") (str v)))
                  arg substitutions))
        template))

(defn- command
  [key substitutions]
  (resolve-command (config/value [:secrets key] (get defaults key)) substitutions))

(defn command-name
  "The password-manager binary the books need installed - the first word of the
   configured (or default gopass) retrieval command."
  []
  (first (config/value [:secrets :password] (:password defaults))))

(defn password
  "The password for a secret path, or nil when it is absent or empty."
  [path]
  (not-empty (apply shell/stdout-of (command :password {:path path}))))

(defn field
  "A named field of a secret (its login, an access token), or nil when absent."
  [path field-name]
  (not-empty (apply shell/stdout-of (command :field {:path path :field field-name}))))

(defn find-path
  "The secret path matching a fuzzy search term (a hostname), or nil - so a book
   locates a secret without hard-coding its full path. The first matching line
   is taken as the path."
  [term]
  (-> (apply shell/stdout-of (command :search {:path term}))
      str/split-lines
      first
      not-empty))

(defn available?
  "Whether the secret exists - the prerequisites check. True when the configured
   search finds it (non-empty output)."
  [path]
  (not (str/blank? (apply shell/stdout-of (command :search {:path path})))))
