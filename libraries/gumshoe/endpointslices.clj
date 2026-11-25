;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.endpointslices
  "What a Service resolves to, read from its discovery.k8s.io/v1
   EndpointSlices. A Service's slices carry the label
   kubernetes.io/service-name, and a Service may have several, one per address
   family and one per hundred endpoints, so its addresses are the union over
   every slice of its namespace carrying its name. The deprecated v1 Endpoints
   object is never read.

   Pure over a kubectl list document of EndpointSlices."
  (:require [gumshoe.kubectl :as kubectl]))

(def service-name-label :kubernetes.io/service-name)

(defn ready?
  "Whether an endpoint takes new connections. The API reads an unset ready as
   true. A terminating endpoint takes none, whatever ready says: the
   EndpointSlice controller reports ready false for it, and serving says
   only that it still answers the connections it holds."
  [endpoint]
  (let [{:keys [ready terminating]} (:conditions endpoint)]
    (and (not (false? ready)) (not (true? terminating)))))

(defn slices-of
  "The EndpointSlices of the Service namespace/service."
  [endpointslices namespace service]
  (filter #(and (= namespace (kubectl/namespace-of %))
                (= service (get-in % [:metadata :labels service-name-label])))
          (kubectl/items-of endpointslices)))

(defn ready-addresses
  "The distinct addresses of every ready endpoint of the Service
   namespace/service, as a set; empty when it has no slice."
  [endpointslices namespace service]
  (set (for [slice (slices-of endpointslices namespace service)
             endpoint (:endpoints slice)
             :when (ready? endpoint)
             address (:addresses endpoint)]
         address)))

(defn ready-backends
  "The namespace/name of every Service with at least one ready endpoint."
  [endpointslices]
  (set (for [slice (kubectl/items-of endpointslices)
             :let [service (get-in slice [:metadata :labels service-name-label])]
             :when (and service (some ready? (:endpoints slice)))]
         (str (kubectl/namespace-of slice) "/" service))))
