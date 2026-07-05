;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.gateway
  "Detectives for the Gateway API: Gateways and their listeners, the ListenerSets
   that attach to them, and the HTTPRoutes that route through both.

   Routing here is indirect: an HTTPRoute attaches to a ListenerSet, the
   ListenerSet attaches to a Gateway, and the Gateway decides which ListenerSets
   it allows. A ListenerSet whose Accepted condition is False is one the Gateway
   refused - it is not in the allowed set - which the controller reports for us,
   so there is no need to reimplement the allow rules here.

   Every Gateway carries one mandatory dummy listener (a Gateway with no
   listeners is rejected); real traffic flows through ListenerSets, so that dummy
   listener legitimately has zero attached routes and is skipped rather than
   reported. Its name defaults to \"dummy\" and can be set with
   [:gateway :dummy-listener] in env.edn."
  (:require [infra.kubectl :as kubectl]))

(def gateway-type "gateways.gateway.networking.k8s.io")
(def httproute-type "httproutes.gateway.networking.k8s.io")

(def listenerset-types
  "The ListenerSet resource under both the experimental and graduated API group
   names - whichever the cluster serves yields evidence, the other is empty."
  ["xlistenersets.gateway.networking.x-k8s.io"
   "listenersets.gateway.networking.k8s.io"])

(defn- listenersets
  [evidence]
  (mapcat #(kubectl/items-of (get evidence %)) listenerset-types))

(defn- dummy-listener-name
  [evidence]
  (get-in evidence ["config" :gateway :dummy-listener] "dummy"))

(defn- false-conditions
  "The conditions of the given types that are explicitly False."
  [conditions types]
  (filter #(and (contains? types (:type %)) (= "False" (:status %))) conditions))

(defn detect-gateway-problems
  [evidence]
  (let [gateways (kubectl/items-of (get evidence gateway-type))
        dummy (dummy-listener-name evidence)]
    (concat
     (for [gateway gateways
           condition (false-conditions (-> gateway :status :conditions)
                                       #{"Accepted" "Programmed"})]
       {:severity :critical
        :component (kubectl/namespace-name-of gateway)
        :summary (format "Gateway is not %s (%s)" (:type condition) (or (:reason condition) "unknown"))
        :hint (:message condition)})
     (for [gateway gateways
           listener (-> gateway :status :listeners)
           condition (false-conditions (:conditions listener)
                                       #{"Accepted" "Programmed" "ResolvedRefs"})]
       {:severity :critical
        :component (kubectl/namespace-name-of gateway)
        :summary (format "listener %s is not %s (%s)"
                         (:name listener) (:type condition) (or (:reason condition) "unknown"))
        :hint (:message condition)})
     ;; the mandatory dummy listener has zero routes by design - real traffic
     ;; attaches to ListenerSets - so it is skipped here rather than reported.
     (for [gateway gateways
           listener (-> gateway :status :listeners)
           :when (not= dummy (:name listener))
           :when (= 0 (:attachedRoutes listener))]
       {:severity :info
        :component (kubectl/namespace-name-of gateway)
        :summary (format "listener %s has no attached routes" (:name listener))
        :hint "either it is brand new, or the routes and ListenerSets meant to attach here fail to"}))))

(defn- gateway-index
  "Gateways keyed by [namespace name], to resolve a ListenerSet's parentRef."
  [evidence]
  (into {} (for [gateway (kubectl/items-of (get evidence gateway-type))]
             [[(kubectl/namespace-of gateway) (kubectl/name-of gateway)] gateway])))

(defn detect-listenerset-problems
  [evidence]
  (let [sets (listenersets evidence)
        gateways (gateway-index evidence)]
    (concat
     ;; A ListenerSet the Gateway refused reports Accepted=False (reason
     ;; NotAllowed) - that is precisely "not in the Gateway's allowed set",
     ;; already decided by the controller.
     (for [ls sets
           condition (false-conditions (-> ls :status :conditions)
                                       #{"Accepted" "Programmed"})]
       {:severity :critical
        :component (kubectl/namespace-name-of ls)
        :summary (format "ListenerSet is not %s (%s)"
                         (:type condition) (or (:reason condition) "unknown"))
        :hint (or (:message condition)
                  "the parent Gateway does not allow this ListenerSet")})
     (for [ls sets
           listener (-> ls :status :listeners)
           condition (false-conditions (:conditions listener)
                                       #{"Accepted" "Programmed" "ResolvedRefs"})]
       {:severity :critical
        :component (kubectl/namespace-name-of ls)
        :summary (format "ListenerSet listener %s is not %s (%s)"
                         (:name listener) (:type condition) (or (:reason condition) "unknown"))
        :hint (:message condition)})
     ;; the parentRef must resolve to a real Gateway, or the ListenerSet - and
     ;; every route through it - attaches to nothing.
     (for [ls sets
           :let [parent (-> ls :spec :parentRef)
                 parent-namespace (or (:namespace parent) (kubectl/namespace-of ls))
                 parent-name (:name parent)]
           :when parent-name
           :when (nil? (get gateways [parent-namespace parent-name]))]
       {:severity :critical
        :component (kubectl/namespace-name-of ls)
        :summary (format "ListenerSet points at Gateway %s/%s which does not exist"
                         parent-namespace parent-name)
        :hint "the parentRef is dangling - the ListenerSet attaches to nothing and its routes serve no traffic"}))))

(defn detect-httproute-problems
  [evidence]
  (let [routes (kubectl/items-of (get evidence httproute-type))]
    (concat
     (for [route routes
           parent (-> route :status :parents)
           condition (false-conditions (:conditions parent)
                                       #{"Accepted" "ResolvedRefs"})]
       {:severity :critical
        :component (kubectl/namespace-name-of route)
        :summary (format "HTTPRoute is not %s by %s (%s)"
                         (:type condition)
                         (-> parent :parentRef :name)
                         (or (:reason condition) "unknown"))
        :hint (:message condition)})
     (for [route routes
           :when (empty? (-> route :status :parents))]
       {:severity :warning
        :component (kubectl/namespace-name-of route)
        :summary "HTTPRoute is not accepted by any Gateway or ListenerSet"
        :hint "its parentRefs point nowhere that accepts it - the route serves no traffic"}))))

(def detectives
  [{:name "gateways"
    :description "Gateways and listeners that are not accepted, programmed, or resolved"
    :requires [gateway-type]
    :detect detect-gateway-problems}
   {:name "listenersets"
    :description "ListenerSets the Gateway refused or that point at a missing Gateway"
    :requires (into [gateway-type] listenerset-types)
    :detect detect-listenerset-problems}
   {:name "httproutes"
    :description "HTTPRoutes that no Gateway or ListenerSet accepts"
    :requires [httproute-type]
    :detect detect-httproute-problems}])
