;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.flux.reset
  "Clears a parked HelmRelease's failure counters and asks for a reconcile.

   Flux defaults install and upgrade to `retries: 0`, so once they are spent the
   release is left failed until its spec or chart changes. Its conditions then
   keep describing a cause that may have cleared long ago, and the interval does
   not re-attempt it. `reconcile.fluxcd.io/requestedAt` alone triggers a
   reconcile and leaves the exhausted counters in place; `resetAt` is the one
   that clears them.

   Verify the cause is actually gone first -- the message is a snapshot of the
   last attempt, not a statement about now. `flux/drift` diffs the stored
   manifest against live when the cause is not obvious."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]
            [gumshoe.steps.flux :as flux-steps]
            [gumshoe.tools.flux :as flux]))

(defn- helmreleases
  [context]
  (flux/parked-first (kubectl/items-of (kubectl/get-all context flux/helmrelease-type))))

(defn reset-handled?
  "Whether helm-controller handled the reset stamped at `token` and the
   release is Ready. A release that was Ready before the request stays Ready
   whether or not the request was handled, so Ready alone proves nothing.
   lastHandledReconcileAt carries the requestedAt the controller acted on; a
   controller that also writes lastHandledResetAt writes it in the same
   reconcile, so a value there other than the token means the counters were
   not cleared."
  [release token]
  (let [reset-at (-> release :status :lastHandledResetAt)]
    (and (flux-steps/handled? release token)
         (or (nil? reset-at) (= token reset-at))
         (flux-steps/ready? release))))

(defn handled-check
  [context namespace name target token]
  {:description (format "HelmRelease %s handled the reset and is Ready" target)
   :timeout 300 :interval 15
   :check #(reset-handled? (kubectl/get-namespaced-resource context namespace flux/helmrelease-type name)
                           token)})

(def selection
  {:mode :namespaced :label "HelmRelease" :namespace-flag :namespace :name-flag :name
   :candidates helmreleases})

(mutation/book
 {:description "Clears a parked HelmRelease's failure counters and reconciles it"
  :options (select/options selection)
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities [:flux]
                  :kubectl-can-get [flux/helmrelease-type]
                  :kubectl-can-patch [flux/helmrelease-type]}
  :select selection
  :confirm {:action "clear the failure counters and reconcile" :destructive? false}
  :subject {:type flux/helmrelease-type}
  :refusal (fn [{:keys [target object]}] (flux/helmrelease-refusal target object))
  :announce (fn [{:keys [target]}] (format "Reset HelmRelease %s" target))
  ;; one timestamp per run, so the two annotations agree and a recorded
  ;; reproducer replays the same command
  :values {:timestamp (fn [_] (str (java.time.Instant/now)))}
  :effect (fn [{:keys [context target timestamp]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              (flux/reset-effect context namespace name timestamp)))
  :verify (fn [{:keys [context target timestamp]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              [(handled-check context namespace name target timestamp)]))})
