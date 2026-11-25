;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.stageset.reconcile
  "Requests a StageSet reconciliation by stamping the
   reconcile.fluxcd.io/requestedAt annotation, the trigger stagesetctl
   reconcile uses, optionally with the update-now override so a held
   revision rolls now."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]
            [gumshoe.tools.stageset :as stageset]))

(defn stagesets
  [context]
  (kubectl/namespaces-names (kubectl/get-all context stageset/stageset-type)))

(defn handled-check
  "Waits until the controller handled the request stamped at `token` (see
   stageset/reconcile-handled?). stagesetctl's own --wait gives up after
   five minutes; so does this."
  [context {:keys [namespace name]} target token update-now?]
  {:description (format (if update-now?
                          "StageSet %s handled the reconcile request and rolled the held update"
                          "StageSet %s handled the reconcile request")
                        target)
   :timeout 300 :interval 5
   :check #(stageset/reconcile-handled?
            (kubectl/get-namespaced-resource context namespace stageset/stageset-type name)
            token update-now?)})

(def selection
  {:mode :namespaced :label "StageSet" :namespace-flag :namespace :name-flag :name
   :candidates stagesets})

(mutation/book
 {:description "Requests a StageSet reconciliation"
  :options (select/options selection {:update-now {:desc "Override the update window and roll a held revision now"
                                                   :coerce :boolean}})
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities [:stageset]
                  :kubectl-can-get [stageset/stageset-type]
                  :kubectl-can-patch [stageset/stageset-type]}
  :select selection
  :subject {:type stageset/stageset-type}
  :refusal (fn [{:keys [target object]}] (stageset/reconcile-refusal target object))
  ;; The token is a value, computed once, so the effect that stamps it and the
  ;; post-check that waits for it are looking at the same value.
  :values {:token (fn [_] (str (java.time.Instant/now)))}
  :confirm {:action "trigger a StageSet reconciliation" :destructive? false}
  :announce (fn [{:keys [target]}] (format "Reconcile StageSet %s" target))
  :effect (fn [{:keys [context ref token opts]}]
            (effect/plan
             (apply effect/kubectl context (str "--namespace=" (:namespace ref))
                    "annotate" stageset/stageset-type (:name ref)
                    (concat (stageset/reconcile-annotations token (boolean (:update-now opts)))
                            ["--overwrite"]))))
  :verify (fn [{:keys [context ref target token opts]}]
            [(handled-check context ref target token (boolean (:update-now opts)))])})
