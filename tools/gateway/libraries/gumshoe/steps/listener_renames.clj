;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps.listener-renames
  "The step that renames a ListenerSet's live listeners to the section
   names its routes attach to. The decisions live in
   gumshoe.listener-renames.

   rename builds the parts of a step that act - :subjects, :reads,
   :values, :refusal, :done, :items, :announce, :effect and :verify - from
   one function of the step's context naming the ListenerSet, so a
   casebook that knows its ListenerSet combines them with its own title,
   select, confirmation and prerequisites through gumshoe.change/compose:

     (change/compose {:select .. :confirm .. :prerequisites ..}
                     (steps/rename (fn [ctx] {:ref {:type \"listenersets.gateway.networking.k8s.io\"
                                                    :namespace \"wiki\" :name \"wiki\"}})))

   The function returns {:ref ref} and may add :problems, lines saying why
   the request is malformed, which refuse the step. The ListenerSet is the
   step's subject: it is read before the confirmation and again right
   before the effect, and the step is refused when it was replaced or its
   spec changed meanwhile. Every route type is listed across all
   namespaces, before the confirmation and again after it; the plan is
   computed once, before the confirmation, and the step is refused when
   the plan the fresh reads give differs from the one the operator
   confirmed. A read that did not answer refuses the step.

   rename-picked is the whole spec over the command line: --pick names the
   ListenerSet as \"<Kind> <namespace>/<name>\", Kind ListenerSet or
   XListenerSet, interactive selection when omitted."
  (:require [clojure.string :as str]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.listener-renames :as renames]
            [gumshoe.select :as select]))

(defn- request
  [listener-set ctx]
  (let [{:keys [ref problems]} (listener-set ctx)]
    {:ref ref
     :problems (vec (concat problems (when (and (nil? ref) (empty? problems)) ["no ListenerSet was named"])))}))

(defn- plan-of
  "The plan over the ListenerSet object and the routes of ctx."
  [ref object routes]
  (renames/plan (-> object :spec :listeners) (renames/attachments ref routes)))

(defn- rights-refusal
  "Why the user may not patch the ListenerSet, or nil. A check that did
   not answer counts as missing."
  [context {:keys [type namespace]}]
  (when (seq (kubectl/missing-rights context namespace [["patch" type]]))
    (format "you may not patch %s in namespace %s, which renaming its listeners needs" type namespace)))

(defn- refusal
  [listener-set]
  (fn [{:keys [context objects routes planned] :as ctx}]
    (let [{:keys [ref problems]} (request listener-set ctx)]
      (cond
        (seq problems) (str/join "; " problems)
        (nil? (first objects)) (format "%s does not exist" (renames/describe ref))
        :else
        (let [fresh (plan-of ref (first objects) routes)
              reasons (concat
                       (renames/problems ref fresh)
                       (when (not= (:renames planned) (:renames fresh))
                         [(format "the listeners or the routes of %s changed since the plan was made, which now renames %s"
                                  (renames/describe ref)
                                  (if (seq (:renames fresh))
                                    (str/join ", " (for [{:keys [from to]} (:renames fresh)] (str from " to " to)))
                                    "nothing"))]))]
          (if (seq reasons)
            (str/join "; " reasons)
            (rights-refusal context ref)))))))

(defn- rename-line
  [ref attachments {:keys [from to]}]
  (let [routes (distinct (for [a attachments :when (= to (:section a))] (:route a)))]
    (format "rename listener \"%s\" of %s to \"%s\", which %s attach%s to"
            from (renames/describe ref) to (str/join ", " routes) (if (= 1 (count routes)) "es" ""))))

(defn rename
  "The parts of a step that renames the listeners of the ListenerSet
   (listener-set ctx) names (see the namespace doc). The step is done when
   every section its routes attach to exists; its post-check holds once
   every route attaching to a renamed section reports it Accepted."
  [listener-set]
  {:subjects (fn [ctx]
               (when-let [ref (:ref (request listener-set ctx))]
                 [(select-keys ref [:type :namespace :name])]))
   :reads {:routes (fn [{:keys [context]}]
                     (renames/route-items (renames/read-route-results context)))}
   :values {:planned (fn [{:keys [objects routes] :as ctx}]
                       (when-let [ref (:ref (request listener-set ctx))]
                         (plan-of ref (first objects) routes)))}
   :refusal (refusal listener-set)
   :done (fn [{:keys [objects planned] :as ctx}]
           (and (empty? (:problems (request listener-set ctx)))
                (some? (first objects))
                (some? planned)
                (renames/done? planned)))
   :items (fn [{:keys [routes planned] :as ctx}]
            (let [{:keys [ref]} (request listener-set ctx)]
              (if (and ref (seq (:renames planned)))
                (let [attachments (renames/attachments ref routes)]
                  (concat
                   (map #(rename-line ref attachments %) (:renames planned))
                   [(format "patch only the listener names of %s: hostnames, TLS and annotations stay as they are"
                            (renames/describe ref))]
                   (for [section (:unmatched planned)]
                     (format "routes also attach to \"%s\" of %s, which no listener carries in any form; it stays missing"
                             section (renames/describe ref)))))
                ["rename a ListenerSet's listeners"])))
   :announce (fn [{:keys [planned] :as ctx}]
               (let [{:keys [ref]} (request listener-set ctx)]
                 (format "Rename the listeners of %s: %s" (renames/describe ref)
                         (str/join ", " (for [{:keys [from to]} (:renames planned)] (str from " to " to))))))
   :effect (fn [{:keys [context planned] :as ctx}]
             (let [{:keys [ref problems]} (request listener-set ctx)]
               (if (or (seq problems) (empty? (:renames planned)))
                 [[:note "nothing to rename"]]
                 (effect/plan (renames/rename-effect context ref (:renames planned))))))
   :verify (fn [{:keys [context planned] :as ctx}]
             (let [{:keys [ref]} (request listener-set ctx)]
               (if (and ref (seq (:renames planned)))
                 [(renames/accepted-check context ref (:renames planned))]
                 [])))})

;; ---------------------------------------------------------------------------
;; the command line

(defn from-pick
  "Pure: the ListenerSet the pick names, for rename. A pick that does not
   parse is a problem, so the step refuses rather than guess."
  [{:keys [target]}]
  (if-let [ref (renames/parse-pick target)]
    {:ref ref}
    {:problems [(format "the pick %s is not \"<Kind> <namespace>/<name>\" of a ListenerSet or XListenerSet"
                        (pr-str target))]}))

(def ^:private selection
  {:mode :one :label "ListenerSet" :flag :pick :candidates renames/picks})

(def rename-picked
  "Renames the listeners of the ListenerSet --pick names to the section
   names its routes attach to."
  (merge {:options (select/options selection
                                   {:pick {:desc "\"<Kind> <namespace>/<name>\" of a ListenerSet or XListenerSet - interactive selection when omitted"}})
          :prerequisites {:installed-tools ["kubectl" "fzf"]
                          :cluster-capabilities []
                          :kubectl-can-get (vec renames/route-types)}
          :select selection
          :empty-message "the cluster lists no ListenerSet or XListenerSet"
          :confirm {:action "rename a ListenerSet's listeners to the section names its routes attach to"
                    :destructive? false}}
         (rename from-pick)))
