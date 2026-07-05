;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.certificates
  "Detectives for cert-manager: certificates that are not ready or expire
   soon, and ACME orders that went sour."
  (:require [infra.kubectl :as kubectl]))

(def certificate-type "certificates.cert-manager.io")
(def order-type "orders.acme.cert-manager.io")

(def ^:private expiry-warning-days 14)

(defn- ready-condition
  [resource]
  (first (filter #(= "Ready" (:type %)) (-> resource :status :conditions))))

(defn expires-within?
  [now days not-after]
  (.isBefore (java.time.Instant/parse not-after)
             (.plus now (java.time.Duration/ofDays days))))

(defn detect-certificate-problems
  [evidence]
  (let [now (:now evidence)
        certificates (kubectl/items-of (get evidence certificate-type))]
    (concat
     (for [certificate certificates
           :let [ready (ready-condition certificate)]
           :when (= "False" (:status ready))]
       {:severity :critical
        :component (kubectl/namespace-name-of certificate)
        :summary (format "certificate is not Ready (%s)" (or (:reason ready) "unknown"))
        :hint (:message ready)})
     (for [certificate certificates
           :let [not-after (-> certificate :status :notAfter)]
           :when (and not-after (expires-within? now expiry-warning-days not-after))]
       {:severity :warning
        :component (kubectl/namespace-name-of certificate)
        :summary (format "certificate expires soon (%s)" not-after)
        :hint "renew with runbooks/kubernetes/certificates/renew.clj"}))))

(defn detect-invalid-orders
  [evidence]
  (for [order (kubectl/items-of (get evidence order-type))
        :let [state (-> order :status :state)]
        :when (contains? #{"invalid" "errored"} state)]
    {:severity :warning
     :component (kubectl/namespace-name-of order)
     :summary (format "ACME order is %s" state)
     :hint "recreate with runbooks/kubernetes/orders/recreate.clj"}))

(def detectives
  [{:name "certificates"
    :description "cert-manager certificates that are not ready or expire soon"
    :requires [certificate-type]
    :detect detect-certificate-problems}
   {:name "acme-orders"
    :description "ACME orders in invalid or errored state"
    :requires [order-type]
    :detect detect-invalid-orders}])
