;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps.objects
  "Steps over a list of Kubernetes objects, each {:type :namespace :name}.

   A builder returns the parts of a step that act - :items, :effect and
   :verify - and the caller combines them with its own title, confirmation,
   prerequisites, reads, values, :refusal and :recovery through
   gumshoe.change/compose (see gumshoe.playbook). The objects come from a function of the step's
   context, so a step reads them from whatever its reads and values
   observed.

   A builder whose effect destroys what it acts on marks the effect with
   gumshoe.change/destructive instead of returning a :confirm, so the step
   is confirmed with a typed yes, and a caller that merges its own :confirm
   with the fragment keeps its action. A caller saying :destructive? false
   beside such a step is refused when the step is normalized or composed."
  (:require [gumshoe.change :as change]
            [gumshoe.deletes :as deletes]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]))

(defn line
  "An object as the steps show it: <type> <namespace>/<name>."
  [{:keys [type namespace name]}]
  (format "%s %s/%s" type namespace name))

(defn delete-effect
  "Deletes each object without waiting for its finalizers, ignoring one that
   is gone already, or notes `none` when there is nothing to delete."
  [context objects none]
  (if (empty? objects)
    (effect/plan (effect/note none))
    (vec (for [object objects]
           (deletes/effect context (select-keys object [:type :namespace :name]) {:wait false})))))

(defn gone-checks
  "One post-check per object, holding once a read answered and found no such
   object, so a read that did not answer never passes for a deletion."
  ([context objects timeout] (gone-checks context objects timeout 5))
  ([context objects timeout interval]
   (for [{:keys [type namespace name] :as object} objects]
     {:description (str (line object) " is gone")
      :timeout timeout :interval interval :verifies-absence? true
      :check #(kubectl/confirmed-gone? context namespace type name)})))

(defn delete
  "The :items, :effect and :verify of a step that deletes the objects
   `(objects ctx)` returns and proves each one gone. The effect is marked
   destructive (gumshoe.change/destructive), so the step always asks for a
   typed yes, and a :confirm :destructive? false beside it is refused.

     :objects  (fn [ctx] -> [{:type :namespace :name} ..])
     :none     what there is when there are no objects, as in
               \"the release has no workloads\"; the items add \" to delete\"
     :timeout  seconds each object may take to go, or (fn [ctx] -> seconds)
     :interval seconds between reads, 5 when absent
     :item     (fn [line] -> item) the confirmation shows per object,
               \"delete <line>\" when absent"
  [{:keys [objects none timeout interval item]
    :or {interval 5 item #(str "delete " %)}}]
  (let [timeout-of (if (number? timeout) (constantly timeout) timeout)]
    {:items (fn [ctx]
              (or (seq (map (comp item line) (objects ctx)))
                  [(str none " to delete")]))
     :effect (change/destructive
              "gumshoe.steps.objects/delete"
              (fn [{:keys [context] :as ctx}]
                (delete-effect context (objects ctx) none)))
     :verify (fn [{:keys [context] :as ctx}]
               (gone-checks context (objects ctx) (timeout-of ctx) interval))}))
