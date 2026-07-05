;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.external-dns
  "Evidence collection for the external-dns detective: harvests every
   hostname the cluster wants to exist (Ingresses, HTTPRoutes, Gateway
   listeners, DNSEndpoints) and resolves each one against the DNS server.
   The comparison itself is a pure detective."
  (:require [clojure.string :as str]
            [infra.dns :as dns]
            [infra.kubectl :as kubectl]
            [infra.stdout :as stdout]))

(def dnsendpoint-type "dnsendpoints.externaldns.k8s.io")
(def gateway-type "gateways.gateway.networking.k8s.io")
(def httproute-type "httproutes.gateway.networking.k8s.io")

;; ---------------------------------------------------------------------------
;; pure hostname harvesting

(defn ingress-hosts
  [ingresses]
  (for [ingress (kubectl/items-of ingresses)
        rule (-> ingress :spec :rules)
        :when (:host rule)]
    [(:host rule) "Ingress"]))

(defn httproute-hosts
  [routes]
  (for [route (kubectl/items-of routes)
        hostname (-> route :spec :hostnames)]
    [hostname "HTTPRoute"]))

(defn gateway-hosts
  [gateways]
  (for [gateway (kubectl/items-of gateways)
        listener (-> gateway :spec :listeners)
        :when (:hostname listener)]
    [(:hostname listener) "Gateway"]))

(defn dnsendpoint-hosts
  [endpoints]
  (for [endpoint (kubectl/items-of endpoints)
        record (-> endpoint :spec :endpoints)
        :when (:dnsName record)]
    [(:dnsName record) "DNSEndpoint"]))

(defn hostname-sources
  "Every hostname the cluster expects to exist, with the kinds that claim it."
  [{:keys [ingresses routes gateways endpoints]}]
  (reduce (fn [sources [host kind]]
            (update sources host (fnil conj (sorted-set)) kind))
          (sorted-map)
          (concat (ingress-hosts ingresses)
                  (httproute-hosts routes)
                  (gateway-hosts gateways)
                  (dnsendpoint-hosts endpoints))))

(defn probe-name
  "Wildcard hostnames are resolved through a synthesized label."
  [hostname]
  (str/replace (str hostname) "*" "bookstore-probe"))

;; ---------------------------------------------------------------------------
;; evidence collection

(defn collect-evidence!
  [context server]
  (stdout/print-section "🔍 Evidence (cluster + dig)")
  (let [ingresses (future (kubectl/get-all context "ingresses"))
        routes (future (kubectl/get-all context httproute-type))
        gateways (future (kubectl/get-all context gateway-type))
        endpoints (future (kubectl/get-all context dnsendpoint-type))
        sources (hostname-sources {:ingresses @ingresses
                                   :routes @routes
                                   :gateways @gateways
                                   :endpoints @endpoints})
        resolutions (mapv (fn [host]
                            (future [host {:a (dns/query {:server server} "A" (probe-name host))
                                           :aaaa (dns/query {:server server} "AAAA" (probe-name host))}]))
                          (keys sources))]
    (stdout/err-println (format "  %s %d hostnames via %s" (stdout/blue "▸") (count sources) server))
    {:now (java.time.Instant/now)
     "server" server
     "sources" sources
     "resolved" (into {} (map deref) resolutions)}))
