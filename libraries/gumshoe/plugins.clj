;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.plugins
  "Loading external plugins - extra announcers, tool support, whole runbook
   collections - that other teams ship in their own repos. A plugin is a Clojure
   namespace on the classpath (pulled in as a git dep via bb.edn :deps, or dropped
   into a directory on :paths) that registers itself when required: an
   `announce-via` defmethod, a detective registration, and so on. env.edn lists
   which to load:

     :plugins [acme.announce.irc storage-team.runbooks]

   Requiring the namespace runs its registrations. A plugin that fails to load is
   a warning, never fatal - a broken third-party plugin must never stop the core."
  (:require [gumshoe.config :as config]
            [gumshoe.stdout :as stdout]))

(defn plugin-names
  "The configured :plugins value as a list of namespace names and the warnings
   it gives. A single symbol or string names one plugin, nil names none, and a
   value that is neither a name nor a list of them names none and warns."
  [plugins]
  (cond
    (nil? plugins) {:names []}
    (or (symbol? plugins) (string? plugins)) {:names [plugins]}
    (or (sequential? plugins) (set? plugins)) {:names (vec plugins)}
    :else {:names []
           :warnings [(format ":plugins in env.edn is a list of plugin namespaces, not %s - no plugin was loaded"
                              (pr-str plugins))]}))

(defn load!
  "Requires every namespace listed in env.edn :plugins (or the given list), so
   their registrations take effect. Safe and best-effort: a plugin that can not
   be loaded is warned about and skipped, and a :plugins value of the wrong
   shape is warned about and loads nothing."
  ([] (load! (config/value [:plugins] [])))
  ([plugins]
   (let [{:keys [names warnings]} (plugin-names plugins)]
     (doseq [warning warnings] (stdout/warn warning))
     (doseq [plugin names]
       (try
         (require (symbol (str plugin)))
         ;; Catch Throwable, not just Exception: a plugin whose top-level
         ;; registration trips an assert (subject/register-facts! has :pre
         ;; conditions) throws AssertionError, an Error - which would otherwise
         ;; abort the whole core instead of skipping the one bad plugin.
         (catch Throwable e
           (stdout/warn (format "could not load plugin %s: %s" plugin (ex-message e)))))))))
