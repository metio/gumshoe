;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.certificate-expiry-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.detectives.certificates :as certificates]))

(def ^:private now (java.time.Instant/parse "2026-11-26T12:00:00Z"))

(defn- certificate [& {:keys [not-before not-after renewal ready reason]
                       :or {not-before "2026-11-25T12:00:00Z" ready "True"}}]
  {:metadata {:namespace "app" :name "short" :generation 1}
   :status (cond-> {:conditions [(cond-> {:type "Ready" :status ready :observedGeneration 1}
                                   reason (assoc :reason reason))]
                    :notBefore not-before}
             not-after (assoc :notAfter not-after)
             renewal (assoc :renewalTime renewal))})

(defn- detect [cert]
  (vec (certificates/detect-certificate-problems {:now now certificates/certificate-type {:items [cert]}})))

(deftest a-short-lived-certificate-with-renewal-scheduled-is-silent
  (is (empty? (detect (certificate :not-after "2026-12-02T12:00:00Z" :renewal "2026-11-30T04:00:00Z")))))

(deftest a-renewal-past-due-and-not-renewed-warns
  (let [[finding & more] (detect (certificate :not-before "2026-09-01T00:00:00Z" :not-after "2026-12-30T00:00:00Z"
                                              :renewal "2026-11-26T09:00:00Z"))]
    (is (nil? more))
    (is (= :warning (:severity finding)))
    (is (= "certificate renewal is overdue since 2026-11-26T09:00:00Z; it expires in 33 day(s) (2026-12-30T00:00:00Z)"
           (:summary finding)))
    (is (= "2026-11-26T09:00:00Z" (:since finding)))))

(deftest a-renewal-just-due-is-given-the-grace
  (is (empty? (detect (certificate :not-before "2026-09-01T00:00:00Z" :not-after "2026-12-30T00:00:00Z"
                                   :renewal "2026-11-26T11:30:00Z")))))

(deftest a-renewal-due-with-expiry-near-warns-at-once
  (is (= [:warning] (map :severity (detect (certificate :not-after "2026-11-28T12:00:00Z" :renewal "2026-11-26T11:30:00Z"))))))

(deftest an-expired-certificate-reads-as-expired
  (let [[finding & more] (detect (certificate :not-before "2026-11-01T00:00:00Z" :not-after "2026-11-25T00:00:00Z"
                                              :renewal "2026-11-17T00:00:00Z"))]
    (is (nil? more))
    (is (= :critical (:severity finding)))
    (is (= "certificate expired (2026-11-25T00:00:00Z)" (:summary finding)))))

(deftest cert-managers-expired-reason-already-says-it
  (is (= ["certificate is not Ready (Expired)"]
         (map :summary (detect (certificate :not-before "2026-11-01T00:00:00Z" :not-after "2026-11-25T00:00:00Z"
                                            :ready "False" :reason "Expired"))))))

(deftest without-a-renewal-time-the-window-decides
  (is (= ["certificate expires soon (2026-12-02T12:00:00Z)"]
         (map :summary (detect (certificate :not-after "2026-12-02T12:00:00Z")))))
  (is (empty? (detect (certificate :not-after "2027-06-01T00:00:00Z")))))
