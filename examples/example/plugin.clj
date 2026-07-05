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
            [gumshoe.hooks :as hooks]))

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

;; Seam 3 - teach the core how to read a tool's version, so it shows up in the
;; Prerequisites checklist for books that require it.
(command/register-version-command! "example-tool" ["--version"])

;; Seam 4 - a post-execution hook: observe every finished book (push a metric,
;; forward the recording, update a status page). It never changes the outcome,
;; and it is time-bounded, so a slow one can not block the book's exit.
(hooks/register-post-hook!
 (fn [{:keys [description outcome]}]
   (println (format "[example hook] \"%s\" finished: %s" description (name outcome)))))
