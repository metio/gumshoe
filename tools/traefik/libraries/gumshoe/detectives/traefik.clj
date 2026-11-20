;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.traefik
  "Traefik objects (traefik.io) that refer to something that does not exist.
   Traefik has no validating webhook, so an IngressRoute, Middleware or
   TraefikService naming a missing Service, TraefikService or Middleware - or
   a Service port the Service does not have - is accepted and only shows up in
   Traefik's log and dashboard.

   A reference resolves the way Traefik resolves it: an empty kind is a
   Service, an empty namespace is the referring object's, and a name containing
   @ belongs to another provider and is not checked here."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def ingressroute-type "ingressroutes.traefik.io")
(def middleware-type "middlewares.traefik.io")
(def traefikservice-type "traefikservices.traefik.io")

(defn- reference
  [kind parent-namespace {:keys [name namespace]} where]
  {:kind kind :namespace (if (str/blank? namespace) parent-namespace namespace) :name name :where where})

(defn- middleware-refs
  [parent-namespace refs where]
  (for [ref refs :when (not (str/includes? (str (:name ref)) "@"))]
    (reference "Middleware" parent-namespace ref where)))

(defn- service-refs
  "The references of one load-balancer spec: the Service or TraefikService it
   names, and the Middlewares it applies."
  [parent-namespace {:keys [kind port middlewares] :as spec} where]
  (concat
   (when-not (str/includes? (str (:name spec)) "@")
     [(cond-> (reference (if (= "TraefikService" kind) "TraefikService" "Service") parent-namespace spec where)
        (not= "TraefikService" kind) (assoc :port port))])
   (middleware-refs parent-namespace middlewares where)))

(defn references
  "Every reference an IngressRoute, Middleware or TraefikService makes, each
   with :where naming the part of the object that makes it."
  [kind object]
  (let [ns (kubectl/namespace-of object)
        spec (:spec object)]
    (case kind
      "IngressRoute"
      (apply concat
             (map-indexed (fn [i route]
                            (let [where (str "route " (inc i))]
                              (concat (mapcat #(service-refs ns % where) (:services route))
                                      (middleware-refs ns (:middlewares route) where))))
                          (:routes spec)))
      "Middleware"
      (concat (middleware-refs ns (get-in spec [:chain :middlewares]) "chain")
              (when-let [service (get-in spec [:errors :service])]
                (service-refs ns service "errors")))
      "TraefikService"
      (concat (mapcat #(service-refs ns % "weighted") (get-in spec [:weighted :services]))
              (mapcat #(service-refs ns % "highestRandomWeight") (get-in spec [:highestRandomWeight :services]))
              (when-let [mirroring (:mirroring spec)]
                (concat (service-refs ns mirroring "mirroring")
                        (mapcat #(service-refs ns % "mirror") (:mirrors mirroring)))))
      [])))

(defn- port-problem
  "Why a Service does not offer the port, matching it as Traefik does: a number
   against the ports' numbers, a name against their names. An ExternalName
   Service accepts any number."
  [service port]
  (let [ports (-> service :spec :ports)
        named? (string? port)]
    (cond
      (or (nil? port) (= 0 port) (= "" port)) :unset
      (some #(if named? (= port (:name %)) (= port (:port %))) ports) nil
      (and (= "ExternalName" (-> service :spec :type)) (not named?)) nil
      :else :unknown)))

(defn problem
  "What a reference is missing, as the object of \"<kind> uses …\", or nil.
   index maps [kind namespace name] to the object."
  [index {:keys [kind namespace name port]}]
  (let [target (get index [kind namespace name])]
    (cond
      (nil? target) (format "%s %s/%s, which does not exist" kind namespace name)
      (= "Service" kind) (case (port-problem target port)
                           :unset (format "Service %s/%s without a port" namespace name)
                           :unknown (format "Service %s/%s port %s, which the Service does not have" namespace name port)
                           nil)
      :else nil)))

(def ^:private consequences
  {"IngressRoute" "Traefik does not load a router whose service, service port or middleware is missing, so these routes do not serve"
   "Middleware" "every router that uses this middleware fails to load"
   "TraefikService" "the routes sending traffic through it fail"})

(defn detect-dangling-references
  [evidence]
  (let [items #(kubectl/items-of (get evidence %))
        index (into {}
                    (for [[type kind] [["services" "Service"] [middleware-type "Middleware"] [traefikservice-type "TraefikService"]]
                          object (items type)]
                      [[kind (kubectl/namespace-of object) (kubectl/name-of object)] object]))]
    ;; one finding per missing target, named in the summary, so a roll-up over
    ;; several objects keeps what each one misses
    (for [[type kind] [[ingressroute-type "IngressRoute"] [middleware-type "Middleware"] [traefikservice-type "TraefikService"]]
          object (items type)
          [missing refs] (group-by #(problem index %) (references kind object))
          :when missing]
      {:severity :critical
       :component (kubectl/namespace-name-of object)
       :summary (format "%s uses %s" kind missing)
       :hint (str "in " (str/join ", " (distinct (map :where refs))) " - " (get consequences kind))})))

(def detectives
  [{:name "traefik-references"
    :description "Traefik IngressRoutes, Middlewares and TraefikServices that refer to a missing Service, Service port, TraefikService or Middleware"
    :requires ["services" ingressroute-type middleware-type traefikservice-type]
    :detect detect-dangling-references}])
