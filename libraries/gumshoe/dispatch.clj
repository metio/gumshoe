;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dispatch
  "A dispatcher picks a subject, asks it what it is, and runs the playbook that
   kind of subject routes to - one front door for many procedures that start
   from the same kind of subject, e.g. adopting any namespace by the product its
   label names.

   Spec:
     {:description \"...\"
      :select   {..}                          ; picks the subject, see gumshoe.select
      :classify (fn [ctx target] -> key)      ; nil when the subject can not say
      :routes   {key {:playbook {..}          ; a playbook spec, or one mutation spec
                      :note \"...\"}}          ; optional, shown before it runs
      :options       {..}                     ; optional, the dispatcher's own
      :prerequisites {..}}                    ; optional, joined with every route's

   ctx is {:context :cluster :opts}. The dispatcher offers every route's
   options, which must agree on :coerce and :alias, without their defaults:
   two routes may default one option differently, and a route's defaults apply
   once the subject named that route. It checks every route's prerequisites
   before it picks, since the route is not known until then. The chosen
   playbook runs in this process against the picked subject, from its leading
   checks and its one confirmation."
  (:refer-clojure :exclude [run!])
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.playbook :as playbook]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]))

(defn playbook-of
  "A route's playbook; a lone mutation spec becomes a playbook of one step."
  [{:keys [playbook]}]
  (if (contains? playbook :steps)
    playbook
    {:select (:select playbook) :confirm (:confirm playbook) :steps [playbook]}))

(defn- defaults
  [options]
  (into {} (keep (fn [[k v]] (when (contains? v :default) [k (:default v)]))) options))

(defn all-options
  "Every route's options without their defaults, and the dispatcher's own."
  [{:keys [routes options]}]
  (merge (playbook/all-options
          {:steps (vec (for [route (vals routes)]
                         {:check identity
                          :options (update-vals (playbook/all-options (playbook-of route))
                                                #(dissoc % :default))}))})
         options))

(defn all-prerequisites
  "Everything any route needs, and the dispatcher's own."
  [{:keys [routes prerequisites]}]
  (apply prerequisites/union
         (concat (map (comp playbook/all-prerequisites playbook-of) (vals routes))
                 [prerequisites])))

(defn with-defaults
  "The options a route's playbook runs with: its defaults under what was given."
  [route opts]
  (merge (defaults (playbook/all-options (playbook-of route)))
         (into {} (remove (comp nil? val)) opts)))

(defn- route-names
  [routes]
  (str/join ", " (sort (map name (keys routes)))))

(defn run-on!
  "Classifies an already-picked subject and runs its route. Returns false when
   the subject names no route."
  [{:keys [classify routes]} target opts announcement-data]
  (let [ctx {:context (kubectl/current-context) :cluster (kubectl/current-cluster) :opts opts}
        key (classify ctx target)
        route (get routes key)]
    (cond
      (nil? key)
      (do (stdout/error (format "%s does not say what it is, so no route applies" target)) false)

      (nil? route)
      (do (stdout/error (format "%s is a %s, and %s has no route" target (name key) (name key)))
          (stdout/err-println (str "   routes: " (route-names routes)))
          false)

      :else
      (do (stdout/print-section (format "🎯 %s is a %s" target (name key)))
          (when-let [note (:note route)] (stdout/err-println (str "   " note)))
          (playbook/run-on! (playbook-of route) target (with-defaults route opts) announcement-data)))))

(defn run!
  "Picks the subject, honouring the flags, and runs its route. Returns true
   when there is nothing to pick, false when nothing was picked."
  [{:keys [select] :as dispatcher} opts announcement-data]
  (let [{:keys [candidates target]} (select/resolve-target select opts (kubectl/current-context))]
    (cond
      (empty? candidates)
      (do (stdout/ok (or (:empty-message select) "nothing to do")) true)

      (select/nothing-selected? (:mode select) target)
      (do (stdout/error (format "no %s selected" (:label select))) false)

      :else
      (run-on! dispatcher target opts announcement-data))))

(defn details
  "What --help says a dispatcher does, generated from its routes."
  [{:keys [select routes] :as dispatcher}]
  (let [needs (prerequisites/summary (all-prerequisites dispatcher))]
    (concat
     ["What it does:"
      (str "  picks " (mutation/picks select) " and runs the route for what it is:")]
     (for [[key route] (sort-by (comp name key) routes)]
       (str "  " (name key) " - " (get-in (playbook-of route) [:confirm :action])
            (when-let [note (:note route)] (str " (" note ")"))))
     ["  each route's --help lists its steps"]
     (when (seq needs)
       (concat ["" "Needs:"] (map #(str "  " %) needs))))))

(def ^:private segment-option
  {:from-segment {:desc "Start the route's playbook at this segment, once the operator acted on the pause that opens it"
                  :coerce :long}})

(defn book
  "Builds and runs a dispatcher from its spec."
  [{:keys [description] :as dispatcher}]
  (runbook/execute!
   {:description description
    :options (merge (all-options dispatcher) segment-option)
    :prerequisites (all-prerequisites dispatcher)
    :details (details dispatcher)
    :action (fn [opts {:keys [announcement-data]}]
              (run! dispatcher opts announcement-data))}))
