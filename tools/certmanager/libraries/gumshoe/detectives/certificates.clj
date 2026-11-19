;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.certificates
  "Detectives for cert-manager: certificates that are not ready, whose
   renewal keeps failing, or that expire soon, and ACME orders that went sour."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

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

(defn- condition
  [resource type]
  (first (filter #(= type (:type %)) (-> resource :status :conditions))))

(defn- instant
  [value]
  (try (some-> value java.time.Instant/parse) (catch Exception _ nil)))

(defn failing-renewal
  "The renewal failure of a certificate that is still Ready, or nil: cert-manager
   tried to renew it after its renewal time and failed, and retries with a
   growing backoff while the current certificate runs out. Ready stays True
   until notAfter, so the not-ready check cannot see this."
  [certificate now]
  (let [{:keys [failedIssuanceAttempts lastFailureTime renewalTime notBefore notAfter]} (:status certificate)
        ready (condition certificate "Ready")
        issuing (condition certificate "Issuing")
        renewal (instant renewalTime) failure (instant lastFailureTime)
        before (instant notBefore) after (instant notAfter)]
    (when (and (pos? (or failedIssuanceAttempts 0))
               (= "True" (:status ready))
               (or (nil? (:observedGeneration ready))
                   (= (:observedGeneration ready) (-> certificate :metadata :generation)))
               issuing renewal failure after now
               (or (nil? before) (not (.isAfter ^java.time.Instant before renewal)))
               (.isBefore ^java.time.Instant renewal after)
               (not (.isBefore ^java.time.Instant failure renewal))
               (not (.isAfter ^java.time.Instant failure now)))
      {:attempts failedIssuanceAttempts
       :reason (:reason issuing)
       :message (:message issuing)
       :renewal renewalTime
       :not-after notAfter
       :expired? (not (.isAfter ^java.time.Instant after now))
       :days-left (.toDays (java.time.Duration/between ^java.time.Instant now ^java.time.Instant after))})))

(defn- renewal-finding
  [certificate {:keys [attempts reason message renewal not-after expired? days-left]}]
  (onset/since
   {:severity (if expired? :critical :warning)
    :component (kubectl/namespace-name-of certificate)
    :summary (if expired?
               (format "certificate expired %s after %d failed renewal attempt(s)" not-after attempts)
               (format "certificate renewal failed %d time(s); it expires in %d day(s) (%s)" attempts days-left not-after))
    :hint (str (when (or reason message) (str "Issuing: " (or reason "") (when message (str " - " message)) "; "))
               "cert-manager retries with a growing backoff - fix the cause, then renew with runbooks/kubernetes/certificates/renew.clj")}
   renewal))

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
           :let [failure (failing-renewal certificate now)]
           :when failure]
       (renewal-finding certificate failure))
     (for [certificate certificates
           :let [not-after (-> certificate :status :notAfter)]
           ;; a failing renewal already says when it runs out
           :when (and not-after (expires-within? now expiry-warning-days not-after)
                      (not (failing-renewal certificate now)))]
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
    :description "cert-manager certificates that are not ready, fail to renew, or expire soon"
    :requires [certificate-type]
    :detect detect-certificate-problems}
   {:name "acme-orders"
    :description "ACME orders in invalid or errored state"
    :requires [order-type]
    :detect detect-invalid-orders}])
