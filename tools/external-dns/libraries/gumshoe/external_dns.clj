;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.external-dns
  "Evidence collection for the external-dns detective: harvests every
   hostname the cluster wants to exist (Ingresses, HTTPRoutes, Gateway
   listeners, DNSEndpoints) and resolves each one against the DNS server.
   The comparison itself is a pure detective."
  (:require [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.dns :as dns]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.reads.result :as result]
            [gumshoe.stdout :as stdout]))

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

(def ^:private address-record-types
  "The record types an A/AAAA lookup answers for; a CNAME's answer carries
   the addresses it leads to. external-dns reads a missing recordType as A."
  #{nil "A" "AAAA" "CNAME"})

(defn dnsendpoint-hosts
  "Names of the DNSEndpoint records an address lookup can confirm. A TXT, MX,
   SRV, NS or CAA record has no address even when it is synced, so it is left
   out rather than reported as unresolved."
  [endpoints]
  (for [endpoint (kubectl/items-of endpoints)
        record (-> endpoint :spec :endpoints)
        :when (:dnsName record)
        :when (contains? address-record-types (:recordType record))]
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

(defn answered-sources
  "The hostname sources of reads, a map of each source kind's read to its
   document, or nil when any read did not answer (nil). A kind the cluster
   does not serve answers with no objects; a read that did not answer would
   drop its hostnames from the judgement unseen, so the check is unread
   instead of judging the rest as if they were all."
  [reads]
  (when (every? some? (vals reads))
    (hostname-sources reads)))

(defn probe-name
  "Wildcard hostnames are resolved through a synthesized label."
  [hostname]
  (str/replace (str hostname) "*" "bookstore-probe"))

(defn resolution
  "What resolving host found, with query (fn [type name]) asking the DNS
   server: {:a .. :aaaa ..}, or {:invalid true} for a host whose probe name
   dig may not be asked about (dns/query-name?), which is never queried. dns/query
   answers such a name with the same nil as a server that did not answer, so
   the two are told apart here, before any query."
  [query host]
  (let [probe (probe-name host)]
    (if (dns/query-name? probe)
      {:a (query "A" probe) :aaaa (query "AAAA" probe)}
      {:invalid true})))

;; ---------------------------------------------------------------------------
;; evidence collection

(defn- read-source
  "Every object of one source kind: the document when the read answered, an
   empty list when the cluster does not serve the kind, and nil when the read
   did not answer. A strict read tells the last two apart, which a plain read
   collapses into the same nil."
  [context type]
  (try
    (binding [kubectl/*strict-reads* true]
      (let [{:keys [ok? doc]} (kubectl/get-json-result context type "--all-namespaces")]
        (if ok? doc {:items []})))
    (catch clojure.lang.ExceptionInfo e
      (if (result/unread-exception? e) nil (throw e)))))

(def ^:private source-types
  "Each source kind's key in hostname-sources' argument and the type read for
   it. A vector of pairs: conformance reads a library map with a :routes key
   as a dispatcher's routes."
  [[:ingresses "ingresses"]
   [:routes httproute-type]
   [:gateways gateway-type]
   [:endpoints dnsendpoint-type]])

(defn collect-evidence!
  [context server]
  (stdout/print-section "🔍 Evidence (cluster + dig)")
  (let [reads (into {} (concurrent/map-all (fn [[k type]] [k (read-source context type)]) source-types))
        unread (sort (keep (fn [[k type]] (when (nil? (get reads k)) type)) source-types))
        sources (answered-sources reads)
        resolve (fn [host]
                  [host (resolution #(dns/query {:server server} %1 %2) host)])]
    (when (seq unread)
      (stdout/item :failure (format "could not read %s - nothing is judged" (str/join ", " unread))))
    (stdout/item :info (format "%d hostnames via %s" (count sources) server))
    {:now (java.time.Instant/now)
     "server" server
     "sources" sources
     "resolved" (into {} (concurrent/map-all resolve (keys sources)))}))
