;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps.objects
  "Steps over a list of Kubernetes objects, each {:type :namespace :name}.

   A builder returns the parts of a step that act - :items, :effect and
   :verify - and the caller merges them into a step map with its own title,
   confirmation, prerequisites, :derive, :precondition and :recovery (see
   gumshoe.playbook). The objects come from a function of the step's
   context, so a step reads them from whatever its :derive observed."
  (:require [gumshoe.effect :as effect]
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
    (vec (for [{:keys [type namespace name]} objects]
           (effect/kubectl context "-n" namespace "delete" type name
                           "--ignore-not-found" "--wait=false")))))

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
   `(objects ctx)` returns and proves each one gone.

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
     :effect (fn [{:keys [context] :as ctx}]
               (delete-effect context (objects ctx) none))
     :verify (fn [{:keys [context] :as ctx}]
               (gone-checks context (objects ctx) (timeout-of ctx) interval))}))
