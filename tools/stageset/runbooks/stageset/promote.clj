;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.stageset.promote
  "Manually promotes a StageSet stage that is held at its promotion gate at
   the revision being rolled out (soaking, analysing, blocked or awaiting a
   manual promotion), by stamping the promote annotation and, in the same
   patch, the promote-revision annotation that binds the token to the
   revision the operator confirmed. Only a held stage is offered or
   accepted. The post-check fails at once when the controller records the
   token as ignored, and its guidance names the reason; on a controller that
   predates that record, a token that was not consumed stays armed, and the
   guidance says how to remove it. The book does not remove it itself."
  (:require [gumshoe.effect :as effect]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]
            [gumshoe.tools.stageset :as stageset]))

(defn stagesets
  [context]
  (kubectl/namespaces-names (kubectl/get-all context stageset/stageset-type)))

(defn- read-stageset
  [context target]
  (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
    (kubectl/get-namespaced-resource context namespace stageset/stageset-type name)))

(defn handled-check
  "Waits until the stage acted on the promotion stamped with `token`, and
   ends at once when the controller records that token as ignored.
   stagesetctl's own --wait gives up after five minutes; so does this."
  [context target stage revision token]
  {:description (format "stage %s of StageSet %s acted on the promotion of revision %s" stage target revision)
   :timeout 300 :interval 5
   :check #(stageset/promotion-handled? (read-stageset context target) stage token)
   :abort #(stageset/ignored-promotion-reason (read-stageset context target) stage token)})

(def selection
  {:mode :namespaced :label "StageSet" :namespace-flag :namespace :name-flag :name
   :candidates stagesets})

(mutation/book
 {:description "Manually promotes a held StageSet stage, bound to the revision it is held at"
  :options (select/options selection {:stage {:desc "The stage to promote - interactive selection when omitted"
                                              :coerce :string}})
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities [:stageset]
                  :kubectl-can-get [stageset/stageset-type]
                  :kubectl-can-patch [stageset/stageset-type]}
  :select selection
  :subject {:type stageset/stageset-type}
  ;; A stage is picked by name among those held at their gate now. The token
  ;; and the held revisions are values, computed once, so the confirmation,
  ;; the effect that stamps them and the post-check that waits for the token
  ;; all see the same ones.
  :values {:stage (fn [{:keys [object opts]}]
                    (interact/choose-one "Stage held at its gate" (stageset/held-stages object) (:stage opts)))
           :revisions (fn [{:keys [object]}] (stageset/held-revisions object))
           :token (fn [_] (str (java.time.Instant/now)))}
  :refusal (fn [{:keys [target object stage opts revisions]}]
             (or (stageset/promotion-refusal target object stage (:stage opts))
                 (stageset/moved-revision target object stage (get revisions stage))))
  :confirm {:action "promote a StageSet stage held at its gate - it skips the rest of its soak, analysis or manual gate" :destructive? false}
  :items (fn [{:keys [target stage revisions]}]
           [(if stage
              (format "stage %s of StageSet %s at revision %s" stage target (get revisions stage))
              (format "StageSet %s" target))])
  :announce (fn [{:keys [target stage revisions]}]
              (format "Promote stage %s of StageSet %s at revision %s" stage target (get revisions stage)))
  :effect (fn [{:keys [context ref stage revisions token]}]
            (if-let [revision (when stage (get revisions stage))]
              (effect/plan
               (apply effect/kubectl context (str "--namespace=" (:namespace ref))
                      "annotate" stageset/stageset-type (:name ref)
                      (concat (stageset/promote-annotations stage revision token) ["--overwrite"])))
              (effect/plan (effect/note "no held stage was chosen; nothing is promoted"))))
  :verify (fn [{:keys [context target stage revisions token]}]
            [(handled-check context target stage (get revisions stage) token)])
  :recovery (fn [{:keys [context target stage token]}]
              (stageset/promotion-recovery context target (read-stageset context target) stage token))})
