;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ui
  "The interactive backend - a plugin seam. gumshoe asks the operator to pick from
   a list, enter text, and confirm a change; by default those shell out to fzf and
   gum. A UI provider overrides any of these primitives, so a plugin can supply a
   babashka-native TUI (charm.clj runs under bb, dropping the fzf/gum binary
   dependencies) or, on a full-JVM build, a widget toolkit (TamboUI is JVM-only) -
   without touching a single book, which only ever calls fuzzy-finder/interact,
   never the backend. env.edn :ui selects a registered provider; unset keeps the
   built-in fzf/gum."
  (:require [gumshoe.config :as config]))

(defonce ^:private providers (atom {}))
(defonce ^:private active (atom nil))

(defn register-provider!
  "Registers a UI backend: {:name kw} plus any primitives it overrides -
   :select-one   (fn [prompt values query] -> chosen or nil)
   :select-multi (fn [prompt values]       -> [chosen] or nil)
   :ask-text     (fn [prompt]              -> string or nil)
   :confirm      (fn []                    -> boolean).
   A primitive it does not provide falls back to the built-in fzf/gum."
  [provider]
  (swap! providers assoc (:name provider) provider))

(defn registered
  "Every registered UI backend name, sorted."
  []
  (sort (keys @providers)))

(defn activate!
  "Selects the UI backend named by env.edn :ui. An unset or unknown name leaves
   no override active, so the built-in fzf/gum is used."
  []
  (reset! active (get @providers (config/value [:ui]))))

(defn backend
  "The active provider's override for op (:select-one/:select-multi/:ask-text/
   :confirm), or nil to use the built-in."
  [op]
  (get @active op))
