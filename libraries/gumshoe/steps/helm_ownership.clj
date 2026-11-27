;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps.helm-ownership
  "The steps that stamp Helm's ownership onto a list of objects before a
   chart adopts them, and strip it again. The decisions live in
   gumshoe.helm-ownership.

   stamp and unstamp build the parts of a step that act - :subjects,
   :refusal, :done, :items, :effect and :verify - from one function of the
   step's context, so a casebook names only its own list and combines the
   parts with its title, select, confirmation and prerequisites through
   gumshoe.change/compose:

     (change/compose {:select .. :confirm .. :prerequisites ..}
                     (steps/stamp (fn [ctx] {:release \"wiki\"
                                             :release-namespace \"wiki\"
                                             :objects [{:type \"persistentvolumeclaims\"
                                                        :namespace \"wiki\" :name \"data\"}]})))

   The function returns {:release :release-namespace :objects [refs]} and
   may add :problems, lines saying why the request is malformed, which
   refuse the step. Each object becomes an optional subject: it is read
   before the confirmation and again right before the effect, and the step
   is refused when one was replaced, its spec changed, its stamp moved, or
   one that did not exist was created meanwhile. An object that does not
   exist is skipped - helm install creates it - and an object carrying
   another release's stamp refuses the step.

   stamp-named and unstamp-named are whole specs over the command line:
   --namespace picks the release namespace, --release names the release,
   and --object (repeatable) names each object as <type>/<name> in the
   release namespace or <type>/<namespace>/<name>, --cluster-object as
   <type>/<name> for a cluster-scoped one."
  (:require [clojure.string :as str]
            [gumshoe.helm-ownership :as own]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.select :as select]))

(defn request
  "Pure: what (ownership ctx) asks for, checked: {:release
   :release-namespace :refs :problems}. :refs are the distinct well-formed
   objects in the order given; :problems holds every reason the request
   cannot run, the malformed objects among them."
  [ownership ctx]
  (let [{:keys [release release-namespace objects problems]} (ownership ctx)
        objects (vec (distinct objects))
        bad (keep own/ref-problem objects)]
    {:release release
     :release-namespace release-namespace
     :refs (filterv (complement own/ref-problem) objects)
     :problems (vec (concat problems
                            (keep identity [(own/release-problem release)
                                            (own/namespace-problem release-namespace)])
                            (when (and (empty? objects) (empty? problems)) ["no object was named"])
                            bad))}))

(defn- subject-refs
  [ownership ctx]
  (for [ref (:refs (request ownership ctx))]
    (assoc (select-keys ref [:type :namespace :name])
           :optional? true
           :facts own/stamp-of)))

(defn- rights-refusal
  "Why the user may not patch the objects the plan writes, or nil. Asked
   before the confirmation and again after it, so a step never stops halfway
   through its plan on a denied write. A check that did not answer counts
   as missing."
  [context targets]
  (let [missing (for [[namespace refs] (group-by :namespace targets)
                      [_verb type] (kubectl/missing-rights context namespace
                                                           (vec (distinct (for [r refs] ["patch" (:type r)]))))]
                  (str "patch " type (if namespace (str " in namespace " namespace) " across the cluster")))]
    (when (seq missing)
      (str "you may not " (str/join ", " missing) ", which writing the stamp needs"))))

(defn- written
  "The refs among refs whose objects the plan for verb writes."
  [verb release release-namespace refs objects]
  (let [writes? (case verb
                  :stamp #{:bare :partial}
                  :unstamp #{:stamped :partial})]
    (vec (for [[ref object] (map vector refs objects)
               :when (and object (writes? (own/ownership release release-namespace object)))]
           ref))))

(defn- refusal
  [verb ownership]
  (fn [{:keys [context refs objects] :as ctx}]
    (let [{:keys [release release-namespace problems]} (request ownership ctx)]
      (cond
        (seq problems) (str/join "; " problems)
        :else (or (own/foreign-refusal release release-namespace refs objects)
                  (rights-refusal context (written verb release release-namespace refs objects)))))))

(defn stamp
  "The parts of a step that stamps Helm's ownership of a release onto the
   objects (ownership ctx) names (see the namespace doc). The step is done
   when every object that exists carries the stamp; its post-check holds
   once every object that existed at the confirmation carries it."
  [ownership]
  {:subjects (fn [ctx] (subject-refs ownership ctx))
   :refusal (refusal :stamp ownership)
   :done (fn [{:keys [objects] :as ctx}]
           (let [{:keys [release release-namespace problems]} (request ownership ctx)]
             (and (empty? problems)
                  (every? #(or (nil? %) (= :stamped (own/ownership release release-namespace %))) objects))))
   :items (fn [{:keys [refs objects] :as ctx}]
            (let [{:keys [release release-namespace]} (request ownership ctx)]
              (own/items :stamp release release-namespace refs objects)))
   :announce (fn [{:keys [refs objects] :as ctx}]
               (let [{:keys [release release-namespace]} (request ownership ctx)]
                 (format "Stamp Helm ownership of release %s/%s onto %s" release-namespace release
                         (str/join ", " (map own/describe (keep (fn [[r o]] (when o r)) (map vector refs objects)))))))
   :effect (fn [{:keys [context refs objects] :as ctx}]
             (let [{:keys [release release-namespace problems]} (request ownership ctx)]
               (if (seq problems)
                 [[:note (str "nothing to stamp: " (str/join "; " problems))]]
                 (own/stamp-plan context release release-namespace refs objects))))
   :verify (fn [{:keys [context refs objects] :as ctx}]
             (let [{:keys [release release-namespace]} (request ownership ctx)
                   existing (vec (keep (fn [[r o]] (when o (select-keys r [:type :namespace :name])))
                                       (map vector refs objects)))]
               (if (seq existing)
                 [(own/stamped-check context release release-namespace existing)]
                 [])))})

(defn unstamp
  "The parts of a step that strips the stamp of a release from the objects
   (ownership ctx) names. The step is done when no object that exists
   carries any part of the stamp; its post-check holds once every read
   answered and none does, and never on a read that did not answer."
  [ownership]
  {:subjects (fn [ctx] (subject-refs ownership ctx))
   :refusal (refusal :unstamp ownership)
   :done (fn [{:keys [objects] :as ctx}]
           (let [{:keys [release release-namespace problems]} (request ownership ctx)]
             (and (empty? problems)
                  (every? #(or (nil? %) (= :bare (own/ownership release release-namespace %))) objects))))
   :items (fn [{:keys [refs objects] :as ctx}]
            (let [{:keys [release release-namespace]} (request ownership ctx)]
              (own/items :unstamp release release-namespace refs objects)))
   :announce (fn [{:keys [refs] :as ctx}]
               (let [{:keys [release release-namespace]} (request ownership ctx)]
                 (format "Strip Helm ownership of release %s/%s from %s" release-namespace release
                         (str/join ", " (map own/describe refs)))))
   :effect (fn [{:keys [context refs objects] :as ctx}]
             (let [{:keys [release release-namespace problems]} (request ownership ctx)]
               (if (seq problems)
                 [[:note (str "nothing to strip: " (str/join "; " problems))]]
                 (own/unstamp-plan context release release-namespace refs objects))))
   :verify (fn [{:keys [context refs] :as ctx}]
             (let [{:keys [release release-namespace]} (request ownership ctx)]
               (if (seq refs)
                 [(own/unstamped-check context release release-namespace
                                       (mapv #(select-keys % [:type :namespace :name]) refs))]
                 [])))})

;; ---------------------------------------------------------------------------
;; the command line

(defn from-flags
  "Pure: the ownership request the command line names, for stamp and
   unstamp: target is the release namespace the select picked, opts the
   parsed flags (:release, :object and :cluster-object, the last two
   repeatable). An object that does not parse is a problem, so the step
   refuses rather than skip it: a skipped adoptee is one helm install aborts
   on."
  [{:keys [target opts]}]
  (let [namespace target
        named (for [s (:object opts)] [s (own/parse-object s namespace)])
        cluster (for [s (:cluster-object opts)] [s (own/parse-cluster-object s)])
        unparsed (concat
                  (for [[s ref] named :when (nil? ref)]
                    (format "--object %s is not <type>/<name> or <type>/<namespace>/<name>" (pr-str s)))
                  (for [[s ref] cluster :when (nil? ref)]
                    (format "--cluster-object %s is not <type>/<name>" (pr-str s))))]
    {:release (:release opts)
     :release-namespace namespace
     :objects (vec (keep second (concat named cluster)))
     :problems (vec unparsed)}))

(defn- namespaces
  [context]
  (kubectl/names-of (kubectl/get-json context "namespaces")))

(def ^:private namespace-select
  {:mode :one :label "release namespace" :flag :namespace :candidates namespaces})

(def ^:private options
  (select/options namespace-select
                  {:namespace {:desc "The namespace the release is installed in, which the stamp records - interactive selection when omitted"}
                   :release {:desc "The Helm release that adopts the objects" :alias :r}
                   :object {:desc "An object the chart renders, <type>/<name> in the release namespace or <type>/<namespace>/<name>, repeatable"
                            :coerce [:string]}
                   :cluster-object {:desc "A cluster-scoped object the chart renders, <type>/<name>, repeatable"
                                    :coerce [:string]}}))

(def ^:private prerequisites
  ;; the types are known only from the flags, so the patch rights are asked
  ;; per object in the refusal instead of here
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["namespaces"]})

(def stamp-named
  "Stamps Helm's ownership of the release --release names in the namespace
   --namespace picks onto every object --object and --cluster-object name."
  (merge {:options options
          :prerequisites prerequisites
          :select namespace-select
          :empty-message "the cluster lists no namespace"
          :confirm {:action "stamp Helm ownership onto existing objects, so the next helm install of the release adopts them in place"
                    :destructive? false}}
         (stamp from-flags)))

(def unstamp-named
  "Strips Helm's ownership of the release --release names in the namespace
   --namespace picks from every object --object and --cluster-object name."
  (merge {:options options
          :prerequisites prerequisites
          :select namespace-select
          :empty-message "the cluster lists no namespace"
          :confirm {:action "strip Helm ownership from objects, so the release no longer adopts or owns them"
                    :destructive? false}}
         (unstamp from-flags)))
