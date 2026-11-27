;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.listener-renames
  "Renaming a ListenerSet's live listeners to the section names its routes
   attach to.

   A route attaches to one listener of a ListenerSet by naming it in
   parentRefs[].sectionName. When no listener carries that name, the route
   reports Accepted=False with reason NoMatchingParent and the gateway
   answers 404 while every other condition is green. Helm cannot repair
   this: it computes its patch from the difference between two release
   revisions, so a ListenerSet that is byte-identical across revisions is
   never corrected, and an adopted ListenerSet keeps whatever listener
   names its first creator chose (a numbered `https-0` per hostname) while
   the chart's routes ask for `https`.

   The plan here renames a listener only when the evidence names exactly
   one candidate. A section the routes attach to and no listener carries
   is a missing name. Its candidates are the listeners whose name, with a
   trailing -<digits> stripped, equals it. With one candidate, which no
   route attaches to under its own name, that listener is renamed. With
   several candidates the plan refuses: a host serving several hostnames
   carries a numbered listener per hostname, and nothing outside the
   cluster can decide which one the routes mean. A candidate that routes
   attach to under its current name is refused as well, because the
   rename would detach them. A missing name without a candidate is
   reported and left alone.

   The rename patches the name alone, with an RFC 6902 JSON patch that
   tests the old name at the listener's index before replacing it. The
   hostnames, TLS settings and cert-manager annotations stay as they are:
   they are the chart's to own, and an adopted object's issuer annotation
   is not safe to re-derive outside the cluster. A JSON merge patch would
   replace the whole listener list and re-assert every one of them.

   Everything here is pure over objects already read, except the reads
   themselves (read-route-results), which go through gumshoe.reads and
   keep a read that did not answer apart from an empty list."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.reads :as reads]))

;; ---------------------------------------------------------------------------
;; the ListenerSet kinds and the route types that attach to them

(def kinds
  "The ListenerSet kinds by Kind: the experimental XListenerSet and the
   graduated ListenerSet, each with the resource type gumshoe.reads names
   it by and the API group a route's parentRef names it with."
  {"XListenerSet" {:type "xlistenersets.gateway.networking.x-k8s.io"
                   :group "gateway.networking.x-k8s.io"}
   "ListenerSet" {:type "listenersets.gateway.networking.k8s.io"
                  :group "gateway.networking.k8s.io"}})

(def kind-of-type
  "The Kind of each ListenerSet resource type."
  (into {} (for [[kind {:keys [type]}] kinds] [type kind])))

(def route-types
  "Every route type that may attach to a listener by sectionName. All of
   them are read: a route of a type left out would make the listener it
   attaches to look unreferenced, and the plan could rename it away from
   that route. A type the cluster does not serve reads as an empty list."
  ["httproutes.gateway.networking.k8s.io"
   "grpcroutes.gateway.networking.k8s.io"
   "tlsroutes.gateway.networking.k8s.io"
   "tcproutes.gateway.networking.k8s.io"
   "udproutes.gateway.networking.k8s.io"])

(def ^:private default-parent-group "gateway.networking.k8s.io")

(defn describe
  "A ListenerSet ref as an operator reads it: \"ListenerSet shop/web\"."
  [{:keys [type namespace name]}]
  (str (get kind-of-type type type) " " namespace "/" name))

;; ---------------------------------------------------------------------------
;; which routes attach to which section

(defn attaches?
  "Whether parent-ref, a parentRef of a route in route-namespace, names
   the ListenerSet ref names. The Kind and group must both match the
   ref's type: parentRef's group defaults to gateway.networking.k8s.io, its
   kind to Gateway, its namespace to the route's own."
  [{:keys [type namespace name]} route-namespace parent-ref]
  (let [{:keys [group]} (get kinds (get kind-of-type type))]
    (and (some? group)
         (= (get kind-of-type type) (:kind parent-ref))
         (= group (or (:group parent-ref) default-parent-group))
         (= name (:name parent-ref))
         (= namespace (or (:namespace parent-ref) route-namespace)))))

(defn attachments
  "Every parentRef of routes that names the ListenerSet ref names, as
   {:route \"<Kind> <namespace>/<name>\" :section <sectionName or nil>}, in
   route order. A parentRef without a sectionName attaches to every
   listener and asks for no name."
  [ref routes]
  (vec (for [route routes
             :let [route-namespace (kubectl/namespace-of route)]
             parent-ref (-> route :spec :parentRefs)
             :when (attaches? ref route-namespace parent-ref)]
         {:route (str (:kind route) " " route-namespace "/" (kubectl/name-of route))
          :section (:sectionName parent-ref)})))

(defn wanted
  "The section names the attachments ask for."
  [attachments]
  (into #{} (keep :section attachments)))

;; ---------------------------------------------------------------------------
;; the plan

(defn base-name
  "name without a trailing -<digits>: \"https-0\" is \"https\", \"https-v2\"
   and \"https\" stay as they are."
  [name]
  (str/replace name #"-\d+$" ""))

(defn plan
  "Pure: the renames listeners (the ListenerSet's spec.listeners, in
   order) need so that every section the attachments ask for exists, as
   {:missing [names] :renames [{:index :from :to}] :ambiguous [{:to :from
   [names]}] :held [{:to :from}] :unmatched [names]}, each sorted by the
   missing name.

   :missing are the sections the routes attach to and no listener
   carries. For each, the candidates are the listeners whose base-name is
   that section. One candidate no route attaches to under its own name
   becomes a rename at its own index in spec.listeners. Several
   candidates are :ambiguous, one that routes do attach to is :held, and
   a section without a candidate is :unmatched. A listener without a name
   is never a candidate."
  [listeners attachments]
  (let [asked (wanted attachments)
        named (keep-indexed (fn [i l] (when-let [n (:name l)] [i n])) listeners)
        live (into #{} (map second named))
        missing (sort (remove live asked))
        judged (for [section missing
                     :let [candidates (filter (fn [[_ n]] (= section (base-name n))) named)]]
                 (cond
                   (empty? candidates) [:unmatched section]
                   (< 1 (count candidates)) [:ambiguous {:to section :from (mapv second candidates)}]
                   (contains? asked (second (first candidates))) [:held {:to section :from (second (first candidates))}]
                   :else (let [[i n] (first candidates)] [:renames {:index i :from n :to section}])))]
    (reduce (fn [acc [k v]] (update acc k conj v))
            {:missing (vec missing) :renames [] :ambiguous [] :held [] :unmatched []}
            judged)))

(defn done?
  "Whether every section the routes attach to exists already."
  [{:keys [missing]}]
  (empty? missing))

(defn- names
  [xs]
  (let [quoted (map #(str "\"" % "\"") xs)]
    (if (< (count quoted) 2)
      (str/join quoted)
      (str (str/join ", " (butlast quoted)) " and " (last quoted)))))

(defn problems
  "Pure: every reason the plan must not be applied, as lines naming the
   ListenerSet ref names; empty when it may run. An :ambiguous or :held
   section refuses the whole plan, so no listener is renamed on a partial
   reading of the host. A plan with no rename at all for a missing section
   refuses too, since there is nothing it could do."
  [ref {:keys [missing renames ambiguous held unmatched]}]
  (let [ls (describe ref)]
    (vec (concat
          (for [{:keys [to from]} ambiguous]
            (format "%s carries the listeners %s and the routes attach to \"%s\": nothing outside the cluster can decide which one they mean, so rename one by hand"
                    ls (names from) to))
          (for [{:keys [to from]} held]
            (format "routes attach to \"%s\" of %s, and other routes to \"%s\", its only numbered form, which renaming it would detach"
                    to ls from))
          (when (and (seq missing) (empty? renames) (empty? ambiguous) (empty? held))
            [(format "the routes attach to %s of %s, which no listener carries under that name or a numbered form of it, so there is nothing to rename"
                     (names unmatched) ls)])))))

;; ---------------------------------------------------------------------------
;; the patch

(defn patch-operations
  "The RFC 6902 operations of renames: per rename, a test of the old name
   at the listener's index and a replace of that name. The API server
   applies them all or none, and a listener list that moved since it was
   read fails the test instead of renaming another listener."
  [renames]
  (vec (mapcat (fn [{:keys [index from to]}]
                 (let [path (str "/spec/listeners/" index "/name")]
                   [{:op "test" :path path :value from}
                    {:op "replace" :path path :value to}]))
               renames)))

(defn rename-effect
  "The one kubectl patch that applies renames to the ListenerSet ref
   names."
  [context {:keys [type namespace name]} renames]
  (effect/kubectl context "--namespace" namespace "patch" type name
                  "--type=json" "--patch" (json/generate-string (patch-operations renames))))

;; ---------------------------------------------------------------------------
;; reads and the post-check

(defn read-route-results
  "Every route type listed across all namespaces, as gumshoe.reads results
   in the order of route-types."
  [context]
  (mapv #(reads/listing context {:type % :namespace :all}) route-types))

(defn route-items
  "The routes of results, every type together. Throws an ex-info carrying
   :gumshoe.kubectl/unread when one did not answer, so an unanswered list
   never reads as a type without routes."
  [results]
  (vec (mapcat reads/items! results)))

(defn- accepted-by?
  "Whether route's status reports the parentRef naming section of the
   ListenerSet ref as Accepted."
  [ref section route]
  (let [route-namespace (kubectl/namespace-of route)]
    (boolean
     (some (fn [{:keys [parentRef conditions]}]
             (and (attaches? ref route-namespace parentRef)
                  (= section (:sectionName parentRef))
                  (some #(and (= "Accepted" (:type %)) (= "True" (:status %))) conditions)))
           (-> route :status :parents)))))

(defn accepted-results?
  "Whether, over route results read now, every section names lists has at
   least one route attaching to it on the ListenerSet ref names, and every
   such route reports that parent Accepted. False when a read did not
   answer and when a section has no route left."
  [ref sections results]
  (boolean
   (and (seq sections)
        (every? reads/answered? results)
        (let [routes (route-items results)]
          (every? (fn [section]
                    (let [attaching (filter (fn [route]
                                              (some #(and (attaches? ref (kubectl/namespace-of route) %)
                                                          (= section (:sectionName %)))
                                                    (-> route :spec :parentRefs)))
                                            routes)]
                      (and (seq attaching)
                           (every? #(accepted-by? ref section %) attaching))))
                  sections)))))

(defn accepted-check
  "The post-check that the routes attaching to the renamed sections are
   Accepted on the ListenerSet ref names. A read that did not answer never
   holds it."
  [context ref renames]
  (let [sections (mapv :to renames)]
    {:description (format "the routes attaching to %s of %s are Accepted" (names sections) (describe ref))
     :timeout 120 :interval 5
     :check #(accepted-results? ref sections (read-route-results context))}))

;; ---------------------------------------------------------------------------
;; picking a ListenerSet

(defn pick
  "A ListenerSet as the select offers it: \"<Kind> <namespace>/<name>\"."
  [kind listener-set]
  (str kind " " (kubectl/namespace-name-of listener-set)))

(defn parse-pick
  "\"<Kind> <namespace>/<name>\" of a ListenerSet kind as {:type :namespace
   :name}, or nil."
  [s]
  (when (string? s)
    (when-let [[_ kind namespace name] (re-matches #"\s*(\w+)\s+([^/\s]+)/(\S+)\s*" s)]
      (when-let [{:keys [type]} (get kinds kind)]
        {:type type :namespace namespace :name name}))))

(defn picks
  "Every ListenerSet of both kinds in context, as picks. A kind the
   cluster does not serve offers none; a list that did not answer throws."
  [context]
  (vec (for [[kind {:keys [type]}] (sort-by key kinds)
             listener-set (sort-by kubectl/namespace-name-of
                                   (reads/items! (reads/listing context {:type type :namespace :all})))]
         (pick kind listener-set))))
