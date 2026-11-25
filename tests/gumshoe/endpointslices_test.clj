;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.endpointslices-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.endpointslices :as endpointslices]))

(defn- endpoint [address conditions]
  (cond-> {:addresses [address]} conditions (assoc :conditions conditions)))

(defn- slice [namespace service name & endpoints]
  {:apiVersion "discovery.k8s.io/v1"
   :kind "EndpointSlice"
   :metadata {:namespace namespace :name name :labels {:kubernetes.io/service-name service}}
   :endpoints (vec endpoints)})

(deftest an-endpoint-takes-traffic-when-ready-and-not-terminating
  (doseq [[label conditions ready?]
          [["ready" {:ready true} true]
           ["ready unset, which the API reads as ready" nil true]
           ["ready unset beside serving" {:serving true} true]
           ["not ready" {:ready false} false]
           ["terminating, still serving" {:ready false :serving true :terminating true} false]
           ["terminating with ready unset" {:terminating true} false]
           ["terminating while it claims ready" {:ready true :terminating true} false]]]
    (testing label
      (is (= ready? (endpointslices/ready? (endpoint "10.0.0.1" conditions)))))))

(deftest ready-addresses-unite-every-slice-of-a-service
  (let [slices {:items [(slice "db" "pooler" "pooler-a" (endpoint "10.0.0.1" {:ready true}))
                        (slice "db" "pooler" "pooler-b"
                               (endpoint "10.0.0.2" {:ready true})
                               (endpoint "10.0.0.3" {:ready false}))
                        (slice "db" "pooler" "pooler-c" (endpoint "10.0.0.1" {:ready true}))
                        (slice "other" "pooler" "pooler-x" (endpoint "10.0.9.9" {:ready true}))
                        (slice "db" "web" "web-a" (endpoint "10.0.1.1" {:ready true}))
                        {:metadata {:namespace "db" :name "unlabelled"}
                         :endpoints [(endpoint "10.0.2.2" {:ready true})]}]}]
    (is (= #{"10.0.0.1" "10.0.0.2"} (endpointslices/ready-addresses slices "db" "pooler"))
        "an address two slices repeat counts once, other namespaces and Services never")
    (is (= #{} (endpointslices/ready-addresses slices "db" "missing")) "a Service without a slice resolves to nothing")
    (is (= #{} (endpointslices/ready-addresses {:items []} "db" "pooler")))))

(deftest ready-backends-names-each-service-with-a-ready-endpoint
  (is (= #{"db/pooler"}
         (endpointslices/ready-backends
          {:items [(slice "db" "pooler" "p-a" (endpoint "10.0.0.1" nil))
                   (slice "db" "down" "d-a" (endpoint "10.0.0.2" {:ready false}))
                   (slice "db" "leaving" "l-a" (endpoint "10.0.0.3" {:terminating true}))]}))))
