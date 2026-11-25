;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.toggle
  "The two mutation specs of a pair of books that set one boolean state of an
   object and set it back - suspend and resume, pause and resume - from one
   declaration. The two sides differ in the wanted state and in their words;
   what the state means, how to change it and how to judge it is declared
   once, as functions of wanted.

     (toggle/pair
      {:spec       {:options .. :prerequisites ..
                    :select  {:mode :one :label \"Schedule\" :flag :schedule}
                    :subject {:type \"schedules.velero.io\"}}  ; and :reads, :values, ..
       :candidates (fn [context wanted] -> picks)   ; optional, see below
       :refusal    (fn [object wanted] -> reason or nil)
       :items      (fn [ctx wanted] -> items)       ; once the subject was read
       :effect     (fn [ctx wanted] -> effect plan)
       :patch      (fn [wanted] -> merge patch)     ; instead of :effect
       :applied?   (fn [object wanted] -> bool)     ; the subject read again
       :verify     {:timeout 30 :interval 3}
       :on  {:verb \"pause\"                          ; wanted true
             :empty-message \"every Schedule is paused already\"
             :confirm  {:action \"pause a backup schedule\"}
             :announce \"Pause Schedule %s\"          ; formatted with the pick
             :verified \"Schedule %s is paused\"      ; formatted with the pick
             :select {..} :verify {..}}             ; optional, merged into the shared ones
       :off {..}})                                  ; wanted false
     => {:on <mutation spec> :off <mutation spec>}

   Each side is :spec with the rest filled in: the select offers the
   candidates, the confirmation shows :items for the object read before it,
   or \"<verb> <pick>\" when there is none, :refusal judges the object read
   again right before the change, :announce and :effect make the change, and
   one post-check waits until :applied? holds for the subject read again.
   :applied? judges an object that exists: a subject the read does not find
   verifies neither side, since an object that is gone is not resumed either.

   :candidates defaults to the objects of the subject's type in every
   namespace that :refusal lets through, sorted, as \"<namespace>/<name>\".
   :patch makes the effect a merge patch of the subject; the type in the plan
   is the subject's :type, so the plan names it even for a pick that does not
   parse, which the refusal stops before the plan runs."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.verify :as verify]))

(defn unrefused-picks
  "The objects of type in every namespace for which (refusal object wanted)
   is nil, sorted, as \"<namespace>/<name>\"."
  [context type refusal wanted]
  (vec (for [object (sort-by kubectl/namespace-name-of (kubectl/items-of (kubectl/get-all context type)))
             :when (nil? (refusal object wanted))]
         (kubectl/namespace-name-of object))))

(defn merge-patch-plan
  "The plan that merge-patches the object of type a ref names."
  [context type {:keys [namespace name]} patch]
  (effect/plan (effect/kubectl context (str "--namespace=" namespace) "patch" type name
                               "--type=merge" (str "--patch=" patch))))

(defn side
  "The mutation spec of one side of a pair declaration, for wanted and its
   words (see the namespace doc)."
  [{:keys [spec candidates refusal items effect patch applied? verify]} wanted
   {:keys [verb announce verified] :as words}]
  (let [type (get-in spec [:subject :type])]
    (merge spec
           {:select (merge (:select spec) (:select words)
                           {:candidates (if candidates
                                          #(candidates % wanted)
                                          #(unrefused-picks % type refusal wanted))})
            :empty-message (:empty-message words)
            :confirm (:confirm words)
            :items (fn [{:keys [object target] :as ctx}]
                     (if object (items ctx wanted) [(str verb " " target)]))
            :refusal (fn [{:keys [object]}] (refusal object wanted))
            :announce (fn [{:keys [target]}] (format announce target))
            :effect (if effect
                      #(effect % wanted)
                      (fn [{:keys [context ref]}] (merge-patch-plan context type ref (patch wanted))))
            :verify (fn [{:keys [target] :as ctx}]
                      [(merge verify (:verify words)
                              {:description (format verified target)
                               :check (fn [] (verify/object-holds? (mutation/read-subject ctx) #(applied? % wanted)))})])})))

(defn pair
  "{:on <spec> :off <spec>}, the mutation specs a pair declaration makes
   (see the namespace doc): :on sets the state, :off sets it back."
  [{:keys [on off] :as declaration}]
  {:on (side declaration true on)
   :off (side declaration false off)})
