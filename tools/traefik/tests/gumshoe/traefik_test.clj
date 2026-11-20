;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.traefik-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.detectives.traefik :as traefik]
            [gumshoe.tools.traefik]))

(defn- object [namespace name spec] {:metadata {:namespace namespace :name name} :spec spec})

(def ^:private services
  [(object "web" "api" {:ports [{:name "http" :port 80} {:name "metrics" :port 9090}]})
   (object "web" "external" {:type "ExternalName" :externalName "example.org" :ports [{:port 443}]})
   (object "shared" "auth" {:ports [{:name "http" :port 8080}]})])

(defn- evidence [& {:keys [routes middlewares traefikservices]}]
  {"services" {:items services}
   traefik/ingressroute-type {:items routes}
   traefik/middleware-type {:items (concat [(object "web" "strip" {:stripPrefix {:prefixes ["/api"]}})] middlewares)}
   traefik/traefikservice-type {:items (concat [(object "web" "split" {:weighted {:services [{:name "api" :port 80}]}})] traefikservices)}})

(defn- detect [& args] (vec (traefik/detect-dangling-references (apply evidence args))))

(defn- route [name & routes] (object "web" name {:routes (vec routes)}))

;; --- references ------------------------------------------------------------------

(deftest references-resolve-as-traefik-resolves-them
  (is (= [{:kind "Service" :namespace "web" :name "api" :where "route 1" :port 80}
          {:kind "TraefikService" :namespace "web" :name "split" :where "route 1"}
          {:kind "Service" :namespace "shared" :name "auth" :where "route 2" :port "http"}
          {:kind "Middleware" :namespace "web" :name "strip" :where "route 2"}
          {:kind "Middleware" :namespace "web" :name "strip" :where "route 2"}]
         (traefik/references "IngressRoute"
                             (route "r"
                                    {:services [{:name "api" :port 80} {:name "split" :kind "TraefikService"}]}
                                    {:services [{:name "auth" :namespace "shared" :port "http" :middlewares [{:name "strip"}]}]
                                     :middlewares [{:name "strip"}]})))))

(deftest other-providers-are-skipped
  (is (empty? (traefik/references "IngressRoute"
                                  (route "r" {:services [{:name "api@file" :port 80} {:name "x@internal" :kind "TraefikService"}]
                                              :middlewares [{:name "auth@file"} {:name "web-strip@kubernetescrd"}]})))))

(deftest middleware-and-traefikservice-references
  (is (= [["Middleware" "chain"] ["Service" "errors"]]
         (map (juxt :kind :where)
              (traefik/references "Middleware" (object "web" "m" {:chain {:middlewares [{:name "strip"}]}
                                                                    :errors {:service {:name "api" :port 80}}})))))
  (is (= [["Service" "weighted"] ["Service" "highestRandomWeight"] ["Service" "mirroring"] ["TraefikService" "mirror"]]
         (map (juxt :kind :where)
              (traefik/references "TraefikService"
                                  (object "web" "t" {:weighted {:services [{:name "a" :port 80}]}
                                                     :highestRandomWeight {:services [{:name "b" :port 80}]}
                                                     :mirroring {:name "c" :port 80 :mirrors [{:name "d" :kind "TraefikService"}]}}))))))

;; --- problems --------------------------------------------------------------------

(deftest everything-resolving-is-quiet
  (is (= [] (detect :routes [(route "ok" {:services [{:name "api" :port 80} {:name "api" :port "metrics"}
                                                     {:name "split" :kind "TraefikService"}
                                                     {:name "auth" :namespace "shared" :port 8080}]
                                          :middlewares [{:name "strip"}]})]))))

(deftest a-missing-service-and-middleware
  (let [findings (detect :routes [(route "shop"
                                         {:services [{:name "gone" :port 80}]}
                                         {:services [{:name "api" :port 80}] :middlewares [{:name "auth"}]})])]
    (is (= [{:severity :critical
             :component "web/shop"
             :summary "IngressRoute uses Service web/gone, which does not exist"
             :hint "in route 1 - Traefik does not load a router whose service, service port or middleware is missing, so these routes do not serve"}
            {:severity :critical
             :component "web/shop"
             :summary "IngressRoute uses Middleware web/auth, which does not exist"
             :hint "in route 2 - Traefik does not load a router whose service, service port or middleware is missing, so these routes do not serve"}]
           findings))))

(deftest ports-match-as-traefik-matches-them
  (let [summary #(:summary (first (detect :routes [(route "r" {:services [%]})])))]
    (is (= "IngressRoute uses Service web/api port 8080, which the Service does not have" (summary {:name "api" :port 8080})))
    (is (= "IngressRoute uses Service web/api port https, which the Service does not have" (summary {:name "api" :port "https"})))
    (is (= "IngressRoute uses Service web/api without a port" (summary {:name "api"})))
    (is (nil? (summary {:name "external" :port 8443})) "an ExternalName Service takes any number")
    (is (some? (summary {:name "external" :port "https"})) "but not a name it lacks")))

(deftest a-namespace-is-not-borrowed
  (is (= "IngressRoute uses Service web/auth, which does not exist"
         (:summary (first (detect :routes [(route "r" {:services [{:name "auth" :port 8080}]})]))))
      "auth lives in shared, and an empty namespace means the route's own"))

(deftest middlewares-and-traefikservices-are-checked-too
  (let [findings (detect :middlewares [(object "web" "chained" {:chain {:middlewares [{:name "strip"} {:name "gone"}]}})
                                       (object "web" "errs" {:errors {:service {:name "error-pages" :port 80}}})]
                         :traefikservices [(object "web" "canary" {:weighted {:services [{:name "api" :port 80}
                                                                                        {:name "api-v2" :port 80}]}})])]
    (is (= #{["Middleware uses Middleware web/gone, which does not exist" "web/chained" "in chain - every router that uses this middleware fails to load"]
             ["Middleware uses Service web/error-pages, which does not exist" "web/errs" "in errors - every router that uses this middleware fails to load"]
             ["TraefikService uses Service web/api-v2, which does not exist" "web/canary" "in weighted - the routes sending traffic through it fail"]}
           (set (map (juxt :summary :component :hint) findings))))))

(deftest one-missing-target-is-one-finding-naming-every-place
  (is (= [["IngressRoute uses Service web/gone, which does not exist"
           "in route 1, route 3 - Traefik does not load a router whose service, service port or middleware is missing, so these routes do not serve"]]
         (map (juxt :summary :hint)
              (detect :routes [(route "r" {:services [{:name "gone" :port 80} {:name "gone" :port 80}]}
                                      {:services [{:name "api" :port 80}]}
                                      {:services [{:name "gone" :port 80}]})])))))

(deftest no-traefik-no-findings
  (is (= [] (vec (traefik/detect-dangling-references {"services" {:items services}})))))

(deftest joins-the-traffic-scope-with-its-capability
  (let [registered (filter #(= "traefik-references" (:name %)) (registry/for-scope :traffic))]
    (is (seq registered))
    (testing "tagged from the package's manifest"
      (is (every? #(= [:traefik] (:capabilities %)) registered)))))
