;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.certificates-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detectives.certificates :as certificates]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.certmanager]))

(defn- summaries [findings] (set (map :summary findings)))

(deftest certificates-detective-test
  (let [now (java.time.Instant/parse "2026-07-04T00:00:00Z")
        evidence {:now now
                  certificates/certificate-type
                  {:items [{:metadata {:namespace "moodle" :name "tls"}
                            :status {:conditions [{:type "Ready" :status "False" :reason "Failed"}]}}
                           {:metadata {:namespace "keycloak" :name "tls"}
                            :status {:conditions [{:type "Ready" :status "True"}]
                                     :notAfter "2026-07-10T00:00:00Z"}}
                           {:metadata {:namespace "fine" :name "tls"}
                            :status {:conditions [{:type "Ready" :status "True"}]
                                     :notAfter "2026-12-31T00:00:00Z"}}]}}
        findings (certificates/detect-certificate-problems evidence)]
    (is (= #{"certificate is not Ready (Failed)"
             "certificate expires soon (2026-07-10T00:00:00Z)"}
           (summaries findings))))
  (testing "invalid and errored orders are reported"
    (is (= #{"ACME order is invalid"}
           (summaries (certificates/detect-invalid-orders
                       {certificates/order-type
                        {:items [{:metadata {:namespace "moodle" :name "order-1"}
                                  :status {:state "invalid"}}
                                 {:metadata {:namespace "fine" :name "order-2"}
                                  :status {:state "valid"}}]}}))))))

(deftest package-registers-tls-scope-and-capability-test
  (is (seq (registry/for-scope :tls)) "the package fills the :tls scan scope")
  (is (contains? (set (capabilities/registered)) :cert-manager)))
