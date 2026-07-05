;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns example.plugin
  "A worked example of a gumshoe plugin. Copy this shape into your own repo,
   publish it, and list its namespace in env.edn :plugins - the plugin loader
   requires it, which runs the registrations below. One namespace can extend
   several seams at once."
  (:require [gumshoe.announce :as announce]
            [gumshoe.command :as command]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.hooks :as hooks]
            [gumshoe.prerequisites :as prerequisites]))

;; Seam 1 - a new announcer type. With this loaded, an env.edn announcer of
;; {:type :example ...} is dispatched here instead of warning "unknown type".
(defmethod announce/announce-via :example
  [_announcer system {:keys [actor]} message]
  (println (format "[example announcer] %s: %s by %s" system message actor)))

;; Seam 2 - a detective that joins the workloads scan (or register a whole new
;; scope; a detective is pure data over evidence, see gumshoe.detective).
(registry/register!
 :workloads
 [{:name "example-check"
   :description "An example plugin detective that never complains"
   :requires ["pods"]
   :detect (fn [_evidence] [])}])

;; Seam 3 - a tool profile: how to read its version, a version floor every book
;; that uses it inherits, and a prerequisite the tool brings along. A book that
;; lists "example-tool" in :installed-tools gets all three for free, without
;; repeating the tool's requirements.
(command/register-tool!
 "example-tool"
 {:version-command ["--version"]
  :min-version "2.0"
  :prerequisites (fn [_opts]
                   [(prerequisites/check "example-tool: service reachable"
                                         (fn [] true)
                                         {:pass "example-tool service is reachable"
                                          :fail "example-tool service is unreachable"})])})

;; Seam 4 - a post-execution hook: observe every finished book (push a metric,
;; forward the recording, update a status page). It never changes the outcome,
;; and it is time-bounded, so a slow one can not block the book's exit.
(hooks/register-post-hook!
 (fn [{:keys [description outcome]}]
   (println (format "[example hook] \"%s\" finished: %s" description (name outcome)))))

;; Seam 5 - a custom prerequisite check. A book that declares :change-window in
;; its :prerequisites is gated on it, animating in the Prerequisites checklist
;; like the built-ins. A real one would query a change calendar; this passes.
(prerequisites/register-check!
 :change-window
 (fn [window _opts]
   [(prerequisites/check (str "change window: " window)
                         (fn [] true)
                         {:pass (str "change window '" window "' is open")
                          :fail (str "change window '" window "' is closed - not now")})]))

;; Seam 6 - a pre-execution gate that applies to EVERY book (unlike a declared
;; prerequisite). This blocks changes while EXAMPLE_CHANGE_FREEZE is set, but lets
;; read-only books through. Return false or {:allow? false :reason ...} to veto.
(hooks/register-pre-hook!
 (fn [{:keys [change?]}]
   (if (and change? (System/getenv "EXAMPLE_CHANGE_FREEZE"))
     {:allow? false :reason "change freeze in effect (EXAMPLE_CHANGE_FREEZE set)"}
     true)))
