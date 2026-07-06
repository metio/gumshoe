;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation
  "A declarative shape for the many books that are the same underneath: list a
   resource, let the operator pick one (or several), then run a confirmed,
   announced, verified change. Such a book is a map - what to select, what to
   confirm, the effect plan, the post-checks - and this engine composes it out
   of gumshoe.interact, gumshoe.flow, and gumshoe.effect. The bespoke pieces (the
   candidate query, the effect, the checks) stay small, named, and testable;
   the flow around them is written and tested once, here.

   Spec:
     {:description   \"...\"
      :options       {..cli options for the target flags..}
      :prerequisites {..}
      :select {:mode :one | :many | :namespaced
               :label \"Node\"
               :candidates (fn [context] -> seq of names)
               :flag :node                      ; :one / :many
               :namespace-flag :namespace       ; :namespaced
               :name-flag :name                 ; :namespaced
               :preview \"kubectl describe node {}\"}  ; :one, optional fzf side pane
      :empty-message \"every node is already cordoned\"   ; when nothing to pick
      :confirm  {:action \"...\" :destructive? false}
      :announce (fn [ctx] -> changelog message)   ; optional
      :effect   (fn [ctx] -> effect plan)
      :verify   (fn [ctx] -> seq of post-check maps)}   ; optional

   The :announce, :effect, and :verify callbacks each receive one context map,
   so a book destructures exactly what it needs:
     {:context .. :cluster .. :target <the pick(s)> :opts <parsed flags>}"
  (:require [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(defn nothing-selected?
  "Whether the selection is empty for the mode - nil for a single pick, an
   empty seq for a multi pick."
  [mode target]
  (if (= :many mode)
    (empty? target)
    (nil? target)))

(defn items
  "The confirmation items for a selection: the picks as a vector."
  [mode target]
  (if (= :many mode) (vec target) [target]))

(defn- select-target
  [{:keys [mode label candidates flag namespace-flag name-flag preview]} opts context]
  (let [names (candidates context)]
    {:candidates names
     :target (case mode
               :one (interact/choose-one label names (get opts flag) preview)
               :many (interact/choose-many label names (get opts flag))
               :namespaced (interact/choose-namespaced label names
                                                       (get opts namespace-flag)
                                                       (get opts name-flag)))}))

(defn book
  "Builds and runs a mutating runbook from a declarative spec.

   Optional hooks beyond the core keys:
     :derive (fn [ctx] -> map)  values computed once and merged into ctx, so a
                                per-run value (a random job name) is shared by
                                :effect and :verify.
     :items  (fn [ctx] -> seq)  overrides the confirmation items, e.g. to add
                                a policy or a warning next to each pick."
  [{:keys [description options prerequisites select empty-message confirm
           derive announce effect verify] :as spec}]
  (runbook/execute!
   {:description description
    :options (or options {})
    :prerequisites prerequisites
    :action
    (fn [opts {:keys [announcement-data]}]
      (let [context (kubectl/current-context)
            cluster (kubectl/current-cluster)
            {:keys [candidates target]} (select-target select opts context)]
        (cond
          (empty? candidates)
          (do (stdout/ok (or empty-message "nothing to do")) true)

          (nothing-selected? (:mode select) target)
          (do (stdout/error (format "no %s selected" (:label select))) false)

          :else
          (let [base {:context context :cluster cluster :target target
                      :opts opts :announcement-data announcement-data}
                ctx (merge base (when derive (derive base)))
                items-fn (:items spec)]
            (flow/change!
             {:confirmation {:action (:action confirm)
                             :target cluster
                             :items (if items-fn (items-fn ctx) (items (:mode select) target))
                             :destructive? (:destructive? confirm)}
              :announce! (when announce
                           #(announce/announce! cluster announcement-data (announce ctx)))
              :effect (effect ctx)
              :post-checks (when verify (verify ctx))})))))}))
