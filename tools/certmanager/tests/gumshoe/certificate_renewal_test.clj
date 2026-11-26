;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.certificate-renewal-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.detectives.certificates :as certificates]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- certificate
  [& {:keys [attempts ready issuing renewal failure not-before not-after ready-generation generation]
      :or {attempts 3 ready "True" issuing {:type "Issuing" :status "False" :reason "Failed" :message "ACME challenge failed"}
           renewal "2026-11-01T00:00:00Z" failure "2026-11-18T00:00:00Z"
           not-before "2026-09-01T00:00:00Z" not-after "2026-12-01T00:00:00Z" generation 2}}]
  {:metadata {:namespace "shop" :name "tls" :generation generation}
   :status (cond-> {:conditions (cond-> [(cond-> {:type "Ready" :status ready} ready-generation (assoc :observedGeneration ready-generation))]
                                  issuing (conj issuing))
                    :renewalTime renewal :lastFailureTime failure :notBefore not-before :notAfter not-after}
             attempts (assoc :failedIssuanceAttempts attempts))})

(defn- detect [& certs]
  (vec (certificates/detect-certificate-problems {:now now certificates/certificate-type {:items certs}})))

(deftest a-ready-certificate-whose-renewal-fails-is-reported
  (let [[finding & more] (detect (certificate))]
    (is (nil? more) "the expires-soon warning folds into it")
    (is (= :warning (:severity finding)))
    (is (= "shop/tls" (:component finding)))
    (is (= "certificate renewal failed 3 time(s); it expires in 11 day(s) (2026-12-01T00:00:00Z)" (:summary finding)))
    (is (= "Issuing: Failed - ACME challenge failed; cert-manager retries with a growing backoff - fix the cause, then renew with runbooks/kubernetes/certificates/renew.clj"
           (:hint finding)))
    (is (= "2026-11-01T00:00:00Z" (str (:since finding))) "since the renewal was due")))

(deftest renewal-failure-needs-every-piece-of-evidence
  (doseq [[why cert] [["no failed attempts" (certificate :attempts nil)]
                      ["zero attempts" (certificate :attempts 0)]
                      ["no Issuing condition" (certificate :issuing nil)]
                      ["Ready for an older generation" (certificate :ready-generation 1)]
                      ["the failure precedes the renewal time" (certificate :failure "2026-10-20T00:00:00Z")]
                      ["the failure lies in the future" (certificate :failure "2026-11-20T00:00:00Z")]
                      ["renewal time after notAfter" (certificate :renewal "2026-12-05T00:00:00Z" :failure "2026-12-06T00:00:00Z" :not-after "2026-12-01T00:00:00Z")]
                      ["notBefore after renewal" (certificate :not-before "2026-11-05T00:00:00Z")]
                      ["unparseable times" (certificate :renewal "soon")]]]
    (is (nil? (certificates/failing-renewal cert now)) why)))

(deftest ready-for-the-current-generation-counts
  (is (certificates/failing-renewal (certificate :ready-generation 2) now)))

(deftest not-ready-stays-with-the-not-ready-finding
  (is (= #{"certificate is not Ready (unknown)" "certificate renewal is overdue since 2026-11-01T00:00:00Z; it expires in 11 day(s) (2026-12-01T00:00:00Z)"}
         (set (map :summary (detect (certificate :ready "False")))))
      "no renewal finding while Ready is False"))

(deftest a-failing-renewal-past-expiry-is-critical
  ;; Ready may lag notAfter; the evidence still says the renewal failed
  (let [[finding] (detect (certificate :not-after "2026-11-19T00:00:00Z" :failure "2026-11-18T00:00:00Z"))]
    (is (= :critical (:severity finding)))
    (is (= "certificate expired 2026-11-19T00:00:00Z after 3 failed renewal attempt(s)" (:summary finding)))))

(deftest the-hint-without-an-issuing-reason
  (let [[finding] (detect (certificate :issuing {:type "Issuing" :status "True"}))]
    (is (re-find #"^cert-manager retries" (:hint finding)))))

(deftest an-overdue-renewal-still-fires-without-a-failure
  (is (= ["certificate renewal is overdue since 2026-11-01T00:00:00Z; it expires in 11 day(s) (2026-12-01T00:00:00Z)"]
         (map :summary (detect (certificate :attempts nil))))))

(deftest a-healthy-certificate-far-from-expiry-is-silent
  (is (empty? (detect (certificate :attempts nil :renewal "2027-02-01T00:00:00Z" :not-after "2027-06-01T00:00:00Z")))))
