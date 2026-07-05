;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.plugins
  "Loading external plugins - extra announcers, tool support, whole runbook
   collections - that other teams ship in their own repos. A plugin is a Clojure
   namespace on the classpath (pulled in as a git dep via bb.edn :deps, or dropped
   into a directory on :paths) that registers itself when required: an
   `announce-via` defmethod, a detective registration, and so on. env.edn lists
   which to load:

     :plugins [acme.announce.irc storage-team.runbooks]

   Requiring the namespace runs its registrations. A plugin that fails to load is
   a warning, never fatal - a broken third-party plugin must never stop the core."
  (:require [infra.config :as config]
            [infra.stdout :as stdout]))

(defn load!
  "Requires every namespace listed in env.edn :plugins (or the given list), so
   their registrations take effect. Safe and best-effort: a plugin that can not
   be loaded is warned about and skipped."
  ([] (load! (config/value [:plugins] [])))
  ([plugins]
   (doseq [plugin plugins]
     (try
       (require (symbol (str plugin)))
       (catch Exception e
         (stdout/warn (format "could not load plugin %s: %s" plugin (ex-message e))))))))
