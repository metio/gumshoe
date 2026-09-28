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
            [gumshoe.tools.flux :as flux]))

(def helmrelease-type flux/helmrelease-type)

(defn- helmreleases
  [context]
  (flux/parked-first (kubectl/items-of (kubectl/get-all context helmrelease-type))))

(defn ready-check
  [context namespace name target]
  {:description (format "HelmRelease %s is Ready" target)
   :timeout 300 :interval 15
   :check (fn []
            (->> (-> (kubectl/get-namespaced-resource context namespace helmrelease-type name)
                     :status :conditions)
                 (some #(and (= "Ready" (:type %)) (= "True" (:status %))))
                 boolean))})

(mutation/book
 {:description "Clears a parked HelmRelease's failure counters and reconciles it"
  :options {:namespace {:desc "The namespace of the HelmRelease - interactive selection when omitted"
                        :alias :n :coerce :string}
            :name {:desc "The name of the HelmRelease - interactive selection when omitted"
                   :alias :r :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities [:flux]
                  :kubectl-can-get [helmrelease-type]
                  :kubectl-can-patch [helmrelease-type]}
  :select {:mode :namespaced :label "HelmRelease" :namespace-flag :namespace :name-flag :name
           :candidates helmreleases}
  :confirm {:action "clear the failure counters and reconcile"}
  :announce (fn [{:keys [target]}] (format "Reset HelmRelease %s" target))
  ;; one timestamp per run, so the two annotations agree and a recorded
  ;; reproducer replays the same command
  :derive (fn [_] {:timestamp (str (java.time.Instant/now))})
  :effect (fn [{:keys [context target timestamp]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              (flux/reset-effect context namespace name timestamp)))
  :verify (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              [(ready-check context namespace name target)]))})
