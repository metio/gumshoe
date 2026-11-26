;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.theme
  "How gumshoe's output looks - a plugin seam. A theme names every outcome a
   book reports and gives each a glyph, a colour and, where the glyphs differ
   only by colour, a plain fallback. The outcomes are

     :success :failure :finding :info            what a check concluded
     :done :not-yet :in-progress :skipped        how far a piece of work got

   A theme also carries the finding severities, the drill-down markers, the
   spinner frames, the rule character and a colour flag. The built-ins are
   :default (emoji + colour), :ascii (no emoji - for log files, CI, and
   terminals that render emoji poorly), and :plain (:ascii with colour off).
   env.edn `:theme` selects one; a plugin registers more with `register!` (a
   branded or colourblind-friendly set). Every outcome mark gumshoe prints -
   prerequisites, post-checks, detective reports, task lists and a book's own
   lines through gumshoe.stdout - is read here, so one setting restyles every
   book. The theme is the only place a glyph literal belongs; conformance
   reports one written into a printed string anywhere else (see
   gumshoe.conformance/hand-written-glyphs)."
  (:require [gumshoe.config :as config]))

(def outcomes
  "Every outcome a theme names, in the order the documentation lists them."
  [:success :failure :finding :info :done :not-yet :in-progress :skipped])

(def colors
  "The colours an outcome may name. gumshoe.render turns each into its ANSI
   escape."
  #{:red :green :yellow :blue :bold :dim})

(def default-theme
  "Emoji marks and colour - what a terminal shows unless env.edn picks another."
  {:name :default
   :outcome {:success {:glyph "✅" :color :green}
             :failure {:glyph "❌" :color :red}
             :finding {:glyph "🔶" :color :yellow}
             :info {:glyph "▸" :color :blue}
             :done {:glyph "✅" :color :green}
             :not-yet {:glyph "⬜" :color :blue}
             :in-progress {:glyph "⏳" :color :blue}
             :skipped {:glyph "⏭" :color :yellow}}
   :severity {:critical "🔥" :warning "🔶" :info "💡"}
   :marker {:critical "🔴" :warning "🟡" :info "🔵"}
   :spinner ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
   :rule "─"
   :color? true})

(def ascii-theme
  "Words in brackets instead of emoji, colour kept - for log files, CI and
   terminals that render emoji poorly."
  {:name :ascii
   :outcome {:success {:glyph "[OK]"}
             :failure {:glyph "[ERROR]"}
             :finding {:glyph "[WARN]"}
             :info {:glyph ">"}
             :done {:glyph "[OK]"}
             :not-yet {:glyph "[TODO]"}
             :in-progress {:glyph "[WAIT]"}
             :skipped {:glyph "[SKIP]"}}
   :severity {:critical "[CRIT]" :warning "[WARN]" :info "[INFO]"}
   :marker {:critical "!!" :warning "!" :info "-"}
   :spinner ["|" "/" "-" "\\"]
   :rule "-"
   :color? true})

(def plain-theme
  "The ascii theme with colour off."
  (assoc ascii-theme :name :plain :color? false))

(def ^:private legacy-keys
  "The single-glyph keys themes used before outcomes were named, and the
   outcome each now reads. A theme that still sets one keeps working: the glyph
   lands on that outcome unless the theme names the outcome itself."
  [[:ok :success] [:error :failure] [:warn :finding]
   [:check-ok :done] [:check-error :failure] [:pending :not-yet] [:bullet :info]])

(defn- deep-merge
  "Recursively merges nested maps, so overriding one glyph in :severity/:marker
   or one outcome's colour keeps the sibling values instead of replacing the
   whole submap."
  [a b]
  (merge-with (fn [x y] (if (and (map? x) (map? y)) (deep-merge x y) y)) a b))

(defn normalize
  "Pure: a theme with every legacy single-glyph key (:ok, :check-ok, :bullet
   ...) moved onto the outcome it names, so a theme written either way reads
   the same. An outcome the theme names explicitly wins over a legacy key; of
   two legacy keys naming one outcome the first in `legacy-keys` wins."
  [theme]
  (reduce (fn [t [legacy outcome]]
            (let [glyph (get theme legacy)]
              (cond-> (dissoc t legacy)
                (and glyph (not (get-in theme [:outcome outcome :glyph]))
                     (not (get-in t [:outcome outcome :glyph])))
                (assoc-in [:outcome outcome :glyph] glyph))))
          theme
          legacy-keys))

(defn resolve-theme
  "Pure: the full theme a partial one stands for - normalised and merged onto
   the default, so it need only set what it changes."
  [theme]
  (deep-merge default-theme (normalize (or theme {}))))

(defonce ^:private themes (atom {}))
(defonce ^:private active (atom default-theme))

(defn register!
  "Registers a theme - a map with a :name and whatever it overrides of the
   default: outcomes (`{:outcome {:success {:glyph \"👍\"}}}`), severities,
   markers, spinner, rule or :color?. A partial theme need only set what it
   changes."
  [theme]
  (swap! themes assoc (:name theme) theme))

(defn registered
  "Every registered theme name, sorted."
  []
  (sort (keys @themes)))

(defn named
  "The resolved theme registered under `wanted`, or nil when none is."
  [wanted]
  (some-> (get @themes wanted) resolve-theme))

(defn select!
  "Activates the named theme, merged onto the default so a partial theme overrides
   only what it sets. An unknown name keeps the default and returns false."
  [wanted]
  (let [chosen (get @themes wanted)]
    (reset! active (resolve-theme chosen))
    (some? chosen)))

(defn apply!
  "Activates the theme named by env.edn :theme (default :default)."
  []
  (select! (config/value [:theme] :default)))

(defn active-theme
  "The resolved theme in effect, as data - what gumshoe.render takes."
  []
  @active)

(defn outcome-in
  "Pure: {:glyph :color :plain} for an outcome of a resolved theme. An unknown
   outcome reads as :info, so a misspelt outcome still prints its text."
  [theme outcome]
  (or (get-in theme [:outcome outcome])
      (get-in theme [:outcome :info])))

(defn outcome
  "{:glyph :color :plain} for an outcome of the active theme."
  [k]
  (outcome-in @active k))

(defn glyph
  "The glyph of an outcome of the active theme, uncoloured."
  [k]
  (:glyph (outcome k)))

(defn token
  "A value of the active theme: :spinner, :rule, :color? or :name, or the glyph
   behind a legacy key (:ok :error :warn :check-ok :check-error :pending
   :bullet), which reads the outcome that key names."
  ([k] (token k nil))
  ([k not-found]
   (if-let [outcome-key (some (fn [[legacy o]] (when (= legacy k) o)) legacy-keys)]
     (or (glyph outcome-key) not-found)
     (get @active k not-found))))

(defn severity
  "The glyph for a finding severity (:critical/:warning/:info), falling back to
   the info glyph for an unknown severity."
  [sev]
  (get-in @active [:severity sev] (glyph :info)))

(defn marker
  "The compact drill-down list marker for a finding severity (the colour carries
   the meaning; the glyph just makes it scannable), the info glyph for unknown."
  [sev]
  (get-in @active [:marker sev] (glyph :info)))

(defn outcome-glyphs
  "Pure: every glyph that marks an outcome in the given resolved themes and is
   not plain ASCII, plus the common hand-written variants of a tick, a cross, a
   warning or a box. Severities and drill-down markers are left out: a 🔥 in a
   firebook's prose names the fire, not a result. These are the literals
   conformance looks for in printed strings; an ASCII glyph such as \"+\" or
   \"[OK]\" is ordinary text and cannot be told apart from it."
  [resolved-themes]
  (let [from-theme (for [t resolved-themes
                         g (concat (keep :glyph (vals (:outcome t)))
                                   (keep :plain (vals (:outcome t))))]
                     g)
        variants ["✓" "✔" "✅" "☑" "✗" "✘" "✖" "❌" "❎" "⚠" "🔶" "⏳" "⌛" "⏭" "⬜" "☐" "○" "▸"]]
    (into (sorted-set)
          (remove #(re-matches #"[\x00-\x7F]*" %))
          (concat from-theme variants))))

(register! default-theme)
(register! ascii-theme)
(register! plain-theme)
