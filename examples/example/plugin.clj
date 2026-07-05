;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns example.plugin
  "A worked example of a gumshoe plugin: one namespace, one `plugin/provide!`
   call, extending every seam at once. Copy this shape into your own repo and
   list its namespace in env.edn :plugins - the loader requires it, which runs
   the provide! below. The only other namespaces it needs are the helpers used to
   build values (a prerequisite item, a drill-down subject)."
  (:require [gumshoe.command :as command]
            [gumshoe.plugin :as plugin]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.subject :as subject]))

(plugin/provide!
 {;; A change-announcement target: env.edn :announce [{:type :example …}].
  :announcers
  {:example (fn [_announcer system {:keys [actor]} message]
             (println (format "[example announcer] %s: %s by %s" system message actor)))}

  ;; A detective that joins the workloads scan.
  :detectives
  {:workloads [{:name "example-check"
                :description "An example plugin detective that never complains"
                :requires ["pods"]
                :detect (fn [_evidence] [])}]}

  ;; A cluster capability the setup wizard can detect and books can require.
  :capabilities
  {:example-mesh (fn [] false)}

  ;; A tool profile - any book that lists "example-tool" in :installed-tools
  ;; inherits its version floor and its brought check for free.
  :tools
  {"example-tool" {:version-command ["--version"]
                   :min-version "2.0"
                   :prerequisites (fn [_opts]
                                    [(prerequisites/check "example-tool: service reachable"
                                                          (fn [] true)
                                                          {:pass "example-tool service is reachable"
                                                           :fail "example-tool service is unreachable"})])}}

  ;; Observe every finished book (push a metric, forward the recording).
  :post-hooks
  [(fn [{:keys [description outcome]}]
     (println (format "[example hook] \"%s\" finished: %s" description (name outcome))))]

  ;; A global gate: block changes during a freeze, let read-only books through.
  :pre-hooks
  [(fn [{:keys [change?]}]
     (if (and change? (System/getenv "EXAMPLE_CHANGE_FREEZE"))
       {:allow? false :reason "change freeze in effect (EXAMPLE_CHANGE_FREEZE set)"}
       true))]

  ;; A custom prerequisite a book gates on with :change-window in :prerequisites.
  :prerequisites
  {:change-window (fn [window _opts]
                    [(prerequisites/check (str "change window: " window)
                                          (fn [] true)
                                          {:pass (str "change window '" window "' is open")
                                           :fail (str "change window '" window "' is closed - not now")})])}

  ;; A drill-down action for a custom kind, offered only when its tool is present.
  ;; Version-aware: example-tool renamed "status" to "inspect" at v2, so
  ;; dispatch-by-version picks the right subcommand for whichever major is
  ;; installed - one package supports both, the operator never has to care.
  :probes
  [{:key :widget-status :label "🔧 widget status" :kinds #{"WidgetSet"} :tools ["example-tool"]
    :args (fn [_ctx {:keys [name]}]
            ["example-tool" (command/dispatch-by-version "example-tool" {"2.0" "inspect" "1.0" "status"}) name])}]

  ;; A CRD the drill-down can fetch and traverse.
  :kinds
  {"WidgetSet" {:type "widgetsets.acme.example"
                :edges (fn [object]
                         [{:relation "manages"
                           :subject (subject/subject "Pod"
                                                     (get-in object [:metadata :namespace])
                                                     (get-in object [:spec :pod]))}])}}

  ;; A report format selected with --output tally: one line per severity, the
  ;; shape a CI job greps. Real plugins add sarif or junit the same way.
  :report-formats
  {"tally" (fn [{:keys [summary]}]
             (println (format "critical=%d warning=%d info=%d"
                              (:critical summary 0) (:warning summary 0) (:info summary 0))))}

  ;; A custom effect verb, so a book can (effect/plan [:example-webhook url event])
  ;; and have it flow through --dry-run, the confirmation preview, and recordings
  ;; like any built-in. This is what earns a custom effect type: the action is NOT
  ;; a plain shell command (it would POST over HTTP), so :cmd does not fit - and the
  ;; :describe keeps the dry-run/preview honest about what will happen.
  :effect-types
  {:example-webhook
   {:describe (fn [[url event]] (format "POST %s to %s" event url))
    :perform (fn [[url event]] (println "would POST" event "to" url) true)}}})
