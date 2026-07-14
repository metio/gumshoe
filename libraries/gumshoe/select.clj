;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.select
  "Choosing the subject of a book: run a candidate query, let the operator pick
   one (or several), and hand back the choice. gumshoe.mutation composes this into
   its confirmed/announced/verified flow, but the same picker serves a read-only
   or bespoke book that just needs 'let the operator choose a namespace' - without
   re-implementing the candidate/empty/nothing-picked dance every time.

   A select spec (the same map mutation/book takes as :select):
     {:mode :one | :many | :namespaced
      :label \"Node\"
      :candidates (fn [context] -> seq of names)
      :flag :node                      ; :one / :many - the opts key holding the pick
      :namespace-flag :namespace       ; :namespaced
      :name-flag :name                 ; :namespaced
      :preview \"kubectl describe node {}\"  ; :one, optional fzf side pane
      :empty-message \"every node is already cordoned\"}  ; shown by pick when empty"
  (:require [gumshoe.interact :as interact]
            [gumshoe.stdout :as stdout]))

(defn nothing-selected?
  "Whether the selection is empty for the mode - nil for a single pick, an empty
   seq for a multi pick."
  [mode target]
  (if (= :many mode)
    (empty? target)
    (nil? target)))

(defn items
  "The confirmation items for a selection: the picks as a vector."
  [mode target]
  (if (= :many mode) (vec target) [target]))

(defn resolve-target
  "Runs the candidate query and the interactive pick for a select spec, returning
   {:candidates <names> :target <the pick, or nil/[] when nothing was chosen>}.
   The lowest-level primitive - use it when you need the raw candidate list too
   (e.g. to distinguish 'nothing to pick' from 'nothing picked' yourself)."
  [{:keys [mode label candidates flag namespace-flag name-flag preview]} opts context]
  (let [names (candidates context)]
    {:candidates names
     :target (case mode
               :one (interact/choose-one label names (get opts flag) preview)
               :many (interact/choose-many label names (get opts flag))
               :namespaced (interact/choose-namespaced label names
                                                       (get opts namespace-flag)
                                                       (get opts name-flag)))}))

(defn pick
  "Discovers candidates for a select spec and lets the operator choose, returning
   the chosen target or nil. Prints the spec's :empty-message when there is
   nothing to pick, and an error when the pick is empty - so a read-only or
   bespoke book selects its subject in one line:

     (when-let [ns (select/pick namespace-select opts context)]
       ...)

   mutation/book uses resolve-target instead, because it maps empty candidates to
   its own success (nothing-to-do) rather than to nil."
  [{:keys [mode label empty-message] :as spec} opts context]
  (let [{:keys [candidates target]} (resolve-target spec opts context)]
    (cond
      (empty? candidates) (do (stdout/ok (or empty-message "nothing to select")) nil)
      (nothing-selected? mode target) (do (stdout/error (format "no %s selected" label)) nil)
      :else target)))
