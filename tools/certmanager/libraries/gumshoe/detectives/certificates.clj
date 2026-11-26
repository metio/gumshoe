;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.certificates
  "Detectives for cert-manager: certificates that are not ready, whose
   renewal keeps failing or is overdue, or that expired, and ACME orders that
   went sour."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def certificate-type "certificates.cert-manager.io")
(def order-type "orders.acme.cert-manager.io")

(def ^:private expiry-warning-days 14)

(def renewal-grace
  "How long past its renewalTime a certificate may stay unrenewed before the
   renewal counts as overdue. cert-manager starts issuing at renewalTime, and
   an issuance normally completes within minutes."
  (java.time.Duration/ofHours 1))

(defn expires-within?
  [now days not-after]
  (.isBefore (java.time.Instant/parse not-after)
             (.plus now (java.time.Duration/ofDays days))))

(defn expiry-state
  "How a certificate's expiry reads at now, judged by its renewalTime, or nil
   when there is nothing to say. cert-manager renews at renewalTime (by
   default two thirds through the lifetime) and moves it forward once
   renewed, so a renewalTime in the future means renewal is scheduled however
   close notAfter is, which keeps short-lived certificates quiet.

   :expired once notAfter has passed; :overdue when renewalTime passed more
   than renewal-grace ago, or passed at all while notAfter is within the
   warning window; :expires-soon when no renewalTime is recorded and notAfter
   is within the window."
  [certificate now]
  (let [{:keys [notAfter renewalTime]} (:status certificate)
        after (onset/instant notAfter)
        renewal (onset/instant renewalTime)
        near? (and after (expires-within? now expiry-warning-days (str after)))]
    (cond
      (or (nil? after) (nil? now)) nil
      (not (.isAfter ^java.time.Instant after now)) :expired
      (and renewal (.isBefore ^java.time.Instant renewal now)
           (or near? (.isBefore ^java.time.Instant (.plus ^java.time.Instant renewal ^java.time.Duration renewal-grace) now)))
      :overdue
      (and (nil? renewal) near?) :expires-soon
      :else nil)))

(defn- expiry-finding
  [certificate state now]
  (let [{:keys [notAfter renewalTime]} (:status certificate)
        base {:component (kubectl/namespace-name-of certificate)
              :hint "renew with runbooks/kubernetes/certificates/renew.clj"}]
    (case state
      :expired
      (onset/since (assoc base :severity :critical
                          :summary (format "certificate expired (%s)" notAfter))
                   notAfter)
      :overdue
      (onset/since (assoc base :severity :warning
                          :summary (format "certificate renewal is overdue since %s; it expires in %d day(s) (%s)"
                                           renewalTime
                                           (.toDays (java.time.Duration/between ^java.time.Instant now ^java.time.Instant (onset/instant notAfter)))
                                           notAfter)
                          :hint (str "cert-manager should have renewed it at its renewalTime and has not; "
                                     "its Issuing condition, CertificateRequests and logs say why. "
                                     "Renew with runbooks/kubernetes/certificates/renew.clj"))
                   renewalTime)
      :expires-soon
      (assoc base :severity :warning
             :summary (format "certificate expires soon (%s)" notAfter)))))

(defn failing-renewal
  "The renewal failure of a certificate that is still Ready, or nil: cert-manager
   tried to renew it after its renewal time and failed, and retries with a
   growing backoff while the current certificate runs out. Ready stays True
   until notAfter, so the not-ready check cannot see this."
  [certificate now]
  (let [{:keys [failedIssuanceAttempts lastFailureTime renewalTime notBefore notAfter]} (:status certificate)
        ready (conditions/condition certificate "Ready")
        issuing (conditions/condition certificate "Issuing")
        renewal (onset/instant renewalTime) failure (onset/instant lastFailureTime)
        before (onset/instant notBefore) after (onset/instant notAfter)]
    (when (and (pos? (or failedIssuanceAttempts 0))
               (= "True" (:status ready))
               (not (conditions/stale? certificate ready))
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
           :let [ready (conditions/condition certificate "Ready")]
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
           :let [state (expiry-state certificate now)
                 ready (conditions/condition certificate "Ready")]
           ;; a failing renewal already says when it runs out, and a
           ;; certificate cert-manager marks not Ready as Expired already
           ;; reads as expired in the not-Ready finding
           :when (and state
                      (not (failing-renewal certificate now))
                      (not (and (= :expired state) (= "False" (:status ready)) (= "Expired" (:reason ready)))))]
       (expiry-finding certificate state now)))))

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
