;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.theme
  "How gumshoe's output looks - a plugin seam. A theme is a small map of glyphs
   plus a colour flag. The built-ins are :default (emoji + colour), :ascii (no
   emoji - for log files, CI, and terminals that render emoji poorly), and :plain
   (:ascii with colour off). env.edn `:theme` selects one; a plugin registers more
   with `register!` (a branded or colourblind-friendly set). Everything
   user-facing - stdout, progress, detective - reads its glyphs here, so one
   setting restyles the whole tool. The same table is where translated strings
   would live if i18n is added later."
  (:require [gumshoe.config :as config]))

(def default-theme
  {:name :default
   :ok "✅" :error "❌" :warn "🔶"
   :check-ok "✓" :check-error "✗" :pending "○" :bullet "▸"
   :severity {:critical "🔥" :warning "🔶" :info "💡"}
   :marker {:critical "🔴" :warning "🟡" :info "🔵"}
   :spinner ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
   :rule "─"
   :color? true})

(def ascii-theme
  {:name :ascii
   :ok "[OK]" :error "[ERROR]" :warn "[WARN]"
   :check-ok "+" :check-error "x" :pending "." :bullet ">"
   :severity {:critical "[CRIT]" :warning "[WARN]" :info "[INFO]"}
   :marker {:critical "!!" :warning "!" :info "-"}
   :spinner ["|" "/" "-" "\\"]
   :rule "-"
   :color? true})

(def plain-theme (assoc ascii-theme :name :plain :color? false))

(defonce ^:private themes (atom {}))
(defonce ^:private active (atom default-theme))

(defn register!
  "Registers a theme - a map with a :name and any glyphs that override the
   default. A partial theme need only set what it changes."
  [theme]
  (swap! themes assoc (:name theme) theme))

(defn registered
  "Every registered theme name, sorted."
  []
  (sort (keys @themes)))

(defn select!
  "Activates the named theme, merged onto the default so a partial theme overrides
   only what it sets. An unknown name keeps the default and returns false."
  [wanted]
  (let [chosen (get @themes wanted)]
    (reset! active (merge default-theme chosen))
    (some? chosen)))

(defn apply!
  "Activates the theme named by env.edn :theme (default :default)."
  []
  (select! (config/value [:theme] :default)))

(defn token
  "A glyph (or the colour flag) from the active theme."
  ([k] (get @active k))
  ([k not-found] (get @active k not-found)))

(defn severity
  "The glyph for a finding severity (:critical/:warning/:info), falling back to
   the bullet for an unknown severity."
  [sev]
  (get-in @active [:severity sev] (token :bullet)))

(defn marker
  "The compact drill-down list marker for a finding severity (the colour carries
   the meaning; the glyph just makes it scannable), bullet for unknown."
  [sev]
  (get-in @active [:marker sev] (token :bullet)))

(register! default-theme)
(register! ascii-theme)
(register! plain-theme)
