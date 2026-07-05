;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.plugin
  "One entry point for a plugin to extend gumshoe. Rather than calling each seam's
   own register function across a dozen namespaces, a plugin declares everything
   it provides in a single map and calls `provide!` once - so we only ever talk
   about 'plugins', and one plugin can extend every seam at the same time.

   Every seam is one optional key here; adding a seam adds a key. The per-seam
   register functions stay available for direct or conditional use - `provide!`
   is the convenience that unifies them."
  (:require [gumshoe.announce :as announce]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.command :as command]
            [gumshoe.detectives.registry :as detectives]
            [gumshoe.hooks :as hooks]
            [gumshoe.investigation :as investigation]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.secrets :as secrets]
            [gumshoe.subject :as subject]
            [gumshoe.summary :as summary]
            [gumshoe.theme :as theme]
            [gumshoe.watch :as watch]))

(defn provide!
  "Registers everything a plugin contributes, from one manifest map. Every key is
   optional:

     :announcers    {:irc      (fn [announcer system data message] …)}  ; a change target
     :detectives    {:workloads [detective …]}                          ; join a scan scope
     :capabilities  {:ceph     (fn [] boolean)}                         ; detect a cluster capability
     :tools         {\"amtool\" {:version-command […] :min-version \"…\"  ; a tool profile
                               :prerequisites (fn [opts] …)}}
     :summary-providers [provider …]                                    ; where findings go
     :themes        [theme …]                                           ; how output looks
     :pre-hooks     [(fn [ctx] …) …]                                    ; gate a run (may veto)
     :post-hooks    [(fn [ctx] …) …]                                    ; observe a finished run
     :secrets       [provider …]                                        ; a password-manager backend
     :prerequisites {:change-window (fn [value opts] items)}            ; a custom gate type
     :probes        [probe …]                                           ; a drill-down action
     :kinds         {\"HelmRelease\" {:type \"…\" :edges (fn [object] …)}} ; a CRD drill-down target
     :resize-watchers [(fn [signals] …)]                                ; a log source for storage resizes

   Order within the map does not matter; registrations are independent."
  [{:keys [announcers detectives capabilities tools summary-providers themes
           pre-hooks post-hooks secrets prerequisites probes kinds resize-watchers]}]
  (doseq [[type f] announcers] (announce/register-announcer! type f))
  (doseq [[scope ds] detectives] (detectives/register! scope ds))
  (doseq [[cap f] capabilities] (capabilities/register-detector! cap f))
  (doseq [[binary profile] tools] (command/register-tool! binary profile))
  (doseq [provider summary-providers] (summary/register-provider! provider))
  (doseq [t themes] (theme/register! t))
  (doseq [h pre-hooks] (hooks/register-pre-hook! h))
  (doseq [h post-hooks] (hooks/register-post-hook! h))
  (doseq [provider secrets] (secrets/register-provider! provider))
  (doseq [[k f] prerequisites] (prerequisites/register-check! k f))
  (doseq [p probes] (investigation/register-probe! p))
  (doseq [[kind spec] kinds] (subject/register-kind! kind spec))
  (doseq [b resize-watchers] (watch/register-resize-watcher! b))
  nil)
