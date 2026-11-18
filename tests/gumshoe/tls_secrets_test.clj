;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tls-secrets-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.tls-secrets :as tls-secrets]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.tls :as tls]))

;; Self-signed test certificates without their keys. The leaf is valid from
;; 2026-11-18T22:47:53Z to 2027-02-16T22:47:53Z for shop.example and
;; www.shop.example; the intermediate is valid for ten years from the same day.
(def ^:private leaf "LS0tLS1CRUdJTiBDRVJUSUZJQ0FURS0tLS0tCk1JSUJyakNDQVZTZ0F3SUJBZ0lVRjJaQngzTFhuU3pQbzdqMjNMbC9iWVRTckU4d0NnWUlLb1pJemowRUF3SXcKRnpFVk1CTUdBMVVFQXd3TWMyaHZjQzVsZUdGdGNHeGxNQjRYRFRJMk1URXhPREl5TkRjMU0xb1hEVEkzTURJeApOakl5TkRjMU0xb3dGekVWTUJNR0ExVUVBd3dNYzJodmNDNWxlR0Z0Y0d4bE1Ga3dFd1lIS29aSXpqMENBUVlJCktvWkl6ajBEQVFjRFFnQUUxWnVaWVRqSzBubXJ1WHkwNTVIczNET0krWlppa0d2M21Ic2UzdFFuekpaVXlYUVgKMDNFTytueWF1a0lHcWJEN3lRYnBTUHRkYlMrcjk3bHUzd0t3QTZOK01Id3dIUVlEVlIwT0JCWUVGS2JialVtTQpacFhMOUpXTDZyeWNzNW5Hd01FWE1COEdBMVVkSXdRWU1CYUFGS2JialVtTVpwWEw5SldMNnJ5Y3M1bkd3TUVYCk1BOEdBMVVkRXdFQi93UUZNQU1CQWY4d0tRWURWUjBSQkNJd0lJSU1jMmh2Y0M1bGVHRnRjR3hsZ2hCM2QzY3UKYzJodmNDNWxlR0Z0Y0d4bE1Bb0dDQ3FHU000OUJBTUNBMGdBTUVVQ0lFT3BqVXRhWDJ2OTdNYWw0YSsvVFVxOQptbjNoWXpQa1RBSE9DR3ZvKzV6a0FpRUF3Wm5keCtybDNXYy96UEJFeEw2V3hGNkxkbkQwaE1ldXZiMlVPalZtCjFkQT0KLS0tLS1FTkQgQ0VSVElGSUNBVEUtLS0tLQo=")
(def ^:private bundle "LS0tLS1CRUdJTiBDRVJUSUZJQ0FURS0tLS0tCk1JSUJyakNDQVZTZ0F3SUJBZ0lVRjJaQngzTFhuU3pQbzdqMjNMbC9iWVRTckU4d0NnWUlLb1pJemowRUF3SXcKRnpFVk1CTUdBMVVFQXd3TWMyaHZjQzVsZUdGdGNHeGxNQjRYRFRJMk1URXhPREl5TkRjMU0xb1hEVEkzTURJeApOakl5TkRjMU0xb3dGekVWTUJNR0ExVUVBd3dNYzJodmNDNWxlR0Z0Y0d4bE1Ga3dFd1lIS29aSXpqMENBUVlJCktvWkl6ajBEQVFjRFFnQUUxWnVaWVRqSzBubXJ1WHkwNTVIczNET0krWlppa0d2M21Ic2UzdFFuekpaVXlYUVgKMDNFTytueWF1a0lHcWJEN3lRYnBTUHRkYlMrcjk3bHUzd0t3QTZOK01Id3dIUVlEVlIwT0JCWUVGS2JialVtTQpacFhMOUpXTDZyeWNzNW5Hd01FWE1COEdBMVVkSXdRWU1CYUFGS2JialVtTVpwWEw5SldMNnJ5Y3M1bkd3TUVYCk1BOEdBMVVkRXdFQi93UUZNQU1CQWY4d0tRWURWUjBSQkNJd0lJSU1jMmh2Y0M1bGVHRnRjR3hsZ2hCM2QzY3UKYzJodmNDNWxlR0Z0Y0d4bE1Bb0dDQ3FHU000OUJBTUNBMGdBTUVVQ0lFT3BqVXRhWDJ2OTdNYWw0YSsvVFVxOQptbjNoWXpQa1RBSE9DR3ZvKzV6a0FpRUF3Wm5keCtybDNXYy96UEJFeEw2V3hGNkxkbkQwaE1ldXZiMlVPalZtCjFkQT0KLS0tLS1FTkQgQ0VSVElGSUNBVEUtLS0tLQotLS0tLUJFR0lOIENFUlRJRklDQVRFLS0tLS0KTUlJQmt6Q0NBVG1nQXdJQkFnSVVQdElzK0R6azdnbjYvM09GZlZQMFRoNkxJcFl3Q2dZSUtvWkl6ajBFQXdJdwpIekVkTUJzR0ExVUVBd3dVUlhoaGJYQnNaU0JKYm5SbGNtMWxaR2xoZEdVd0hoY05Nall4TVRFNE1qSTFNREkxCldoY05Nell4TVRFMU1qSTFNREkxV2pBZk1SMHdHd1lEVlFRRERCUkZlR0Z0Y0d4bElFbHVkR1Z5YldWa2FXRjAKWlRCWk1CTUdCeXFHU000OUFnRUdDQ3FHU000OUF3RUhBMElBQkY2ZXMzWjZHdms1L3JabUJCTUt1U2lCbDg0VgpYczg4ZHpLQld2cE91MmJqRElDb2dMU1RYcVFRQ280ZW5Kd2w5c1ZHY3NyVG5OUmY5b0x0anJTK0c3cWpVekJSCk1CMEdBMVVkRGdRV0JCUWlmVjhvU2ZXZVUyNlA5MWYvNjlDZWZYQURZakFmQmdOVkhTTUVHREFXZ0JRaWZWOG8KU2ZXZVUyNlA5MWYvNjlDZWZYQURZakFQQmdOVkhSTUJBZjhFQlRBREFRSC9NQW9HQ0NxR1NNNDlCQU1DQTBnQQpNRVVDSUVHZWFJRnhEWnJqYUJhYklRbDdIUW91M0ZlWVRrZExWYlpMc2tuQVBsMTBBaUVBdWpIdkw0UExVNDlyCnUrRDE0RjJSMU04ZVZBTC9VM0pWVk9tVGl2N0pBK289Ci0tLS0tRU5EIENFUlRJRklDQVRFLS0tLS0K")
(def ^:private intermediate "LS0tLS1CRUdJTiBDRVJUSUZJQ0FURS0tLS0tCk1JSUJrekNDQVRtZ0F3SUJBZ0lVUHRJcytEems3Z242LzNPRmZWUDBUaDZMSXBZd0NnWUlLb1pJemowRUF3SXcKSHpFZE1Cc0dBMVVFQXd3VVJYaGhiWEJzWlNCSmJuUmxjbTFsWkdsaGRHVXdIaGNOTWpZeE1URTRNakkxTURJMQpXaGNOTXpZeE1URTFNakkxTURJMVdqQWZNUjB3R3dZRFZRUUREQlJGZUdGdGNHeGxJRWx1ZEdWeWJXVmthV0YwClpUQlpNQk1HQnlxR1NNNDlBZ0VHQ0NxR1NNNDlBd0VIQTBJQUJGNmVzM1o2R3ZrNS9yWm1CQk1LdVNpQmw4NFYKWHM4OGR6S0JXdnBPdTJiakRJQ29nTFNUWHFRUUNvNGVuSndsOXNWR2NzclRuTlJmOW9MdGpyUytHN3FqVXpCUgpNQjBHQTFVZERnUVdCQlFpZlY4b1NmV2VVMjZQOTFmLzY5Q2VmWEFEWWpBZkJnTlZIU01FR0RBV2dCUWlmVjhvClNmV2VVMjZQOTFmLzY5Q2VmWEFEWWpBUEJnTlZIUk1CQWY4RUJUQURBUUgvTUFvR0NDcUdTTTQ5QkFNQ0EwZ0EKTUVVQ0lFR2VhSUZ4RFpyamFCYWJJUWw3SFFvdTNGZVlUa2RMVmJaTHNrbkFQbDEwQWlFQXVqSHZMNFBMVTQ5cgp1K0QxNEYyUjFNOGVWQUwvVTNKVlZPbVRpdjdKQStvPQotLS0tLUVORCBDRVJUSUZJQ0FURS0tLS0tCg==")

(defn- at [s] (java.time.Instant/parse s))

(deftest a-certificate-reads-as-data
  (let [certificate (tls/certificate-base64 leaf)]
    (is (= "CN=shop.example" (:subject certificate)))
    (is (= (at "2026-11-18T22:47:53Z") (:not-before certificate)))
    (is (= (at "2027-02-16T22:47:53Z") (:not-after certificate)))
    (is (= ["shop.example" "www.shop.example"] (:dns-names certificate)))))

(deftest the-leaf-of-a-bundle-is-judged
  (is (= "CN=shop.example" (:subject (tls/certificate-base64 bundle))))
  (is (= "CN=Example Intermediate" (:subject (tls/certificate-base64 intermediate))))
  (is (= [] (:dns-names (tls/certificate-base64 intermediate))) "no subject alternative names"))

(deftest unreadable-input-is-no-certificate
  (doseq [input [nil "" "bm9wZQ==" "not base64 at all!" (subs leaf 0 200)]]
    (is (nil? (tls/certificate-base64 input)) (pr-str input)))
  (is (nil? (tls/certificate "-----BEGIN CERTIFICATE-----\nAAAA\n-----END CERTIFICATE-----\n"))))

(deftest base64-with-line-breaks-still-reads
  (is (= "CN=shop.example"
         (:subject (tls/certificate-base64 (str/join "\n" (map #(apply str %) (partition-all 76 leaf))))))))

(deftest the-listing-parses-into-secrets
  (is (= [{:namespace "shop" :name "web-tls" :certificate-name nil :crt "QUJD"}
          {:namespace "shop" :name "api-tls" :certificate-name "api" :crt nil}]
         (kubectl/parse-tls-listing "shop\tweb-tls\t\tQUJD\nshop\tapi-tls\tapi\t\n\n")))
  (is (= [] (kubectl/parse-tls-listing nil)))
  (is (= [] (kubectl/parse-tls-listing ""))))

(defn- ingress
  [namespace name & secrets]
  {:metadata {:namespace namespace :name name}
   :spec {:tls (vec (for [s secrets] {:hosts ["shop.example"] :secretName s}))}})

(defn- gateway
  [namespace name listeners]
  {:metadata {:namespace namespace :name name}
   :spec {:listeners listeners}})

(defn- secret
  [namespace name crt & {:keys [managed-by]}]
  {:namespace namespace :name name :crt crt :certificate-name managed-by})

(defn- run
  [now & {:keys [ingresses gateways secrets]}]
  (tls-secrets/detect-tls-secret-problems {:now (at now)
                                           "ingresses" {:items (or ingresses [])}
                                           tls-secrets/gateway-type (when gateways {:items gateways})
                                           "tls-secrets" secrets}))

(def ^:private served-leaf
  {:ingresses [(ingress "shop" "web" "web-tls")] :secrets [(secret "shop" "web-tls" leaf)]})

(defn- judge [now] (map (juxt :severity :summary) (apply run now (mapcat identity served-leaf))))

(deftest a-certificate-is-judged-by-its-validity
  (testing "valid for weeks"
    (is (empty? (judge "2026-12-01T00:00:00Z"))))
  (testing "fourteen days and more left is quiet"
    (is (empty? (judge "2027-02-02T22:47:53Z"))))
  (testing "under fourteen days left"
    (is (= [[:warning "TLS certificate expires in 13 days"]] (judge "2027-02-02T22:47:54Z")) "whole days left are counted")
    (is (= [[:warning "TLS certificate expires in 0 days"]] (judge "2027-02-16T22:00:00Z"))))
  (testing "expired"
    (is (= [[:critical "TLS certificate expired 2027-02-16T22:47:53Z"]] (judge "2027-02-17T00:00:00Z"))))
  (testing "not valid yet"
    (is (= [[:critical "TLS certificate is not valid before 2026-11-18T22:47:53Z"]] (judge "2026-11-18T12:00:00Z")))))

(deftest a-finding-names-the-secret-and-who-serves-it
  (let [[finding] (run "2027-03-01T00:00:00Z"
                       :ingresses [(ingress "shop" "web" "web-tls") (ingress "shop" "admin" "web-tls")]
                       :gateways [(gateway "edge" "public" [{:name "https" :tls {:certificateRefs [{:name "web-tls" :namespace "shop"}]}}])]
                       :secrets [(secret "shop" "web-tls" leaf)])]
    (is (= "shop/web-tls" (:component finding)))
    (is (= "served by Gateway edge/public listener https, Ingress admin, Ingress web - CN=shop.example; replace the certificate"
           (:hint finding)))))

(deftest gateway-references-resolve-to-the-gateways-namespace
  (is (= ["edge/edge-tls"]
         (map :component (run "2027-03-01T00:00:00Z"
                              :gateways [(gateway "edge" "public" [{:name "https" :tls {:certificateRefs [{:name "edge-tls"}]}}
                                                                   {:name "http"}])]
                              :secrets [(secret "edge" "edge-tls" leaf) (secret "shop" "edge-tls" leaf)]))))
  (testing "a reference to something other than a core Secret is not ours to read"
    (is (empty? (run "2027-03-01T00:00:00Z"
                     :gateways [(gateway "edge" "public" [{:name "https" :tls {:certificateRefs [{:name "edge-tls" :kind "ConfigMap"}
                                                                                                 {:name "edge-tls" :group "vault.example" :kind "Secret"}]}}])]
                     :secrets [(secret "edge" "edge-tls" leaf)])))))

(deftest only-served-and-unmanaged-secrets-are-judged
  (testing "an expired Secret nobody serves"
    (is (empty? (run "2027-03-01T00:00:00Z" :secrets [(secret "shop" "old-tls" leaf)]))))
  (testing "an expired Secret cert-manager renews"
    (is (empty? (run "2027-03-01T00:00:00Z"
                     :ingresses [(ingress "shop" "web" "web-tls")]
                     :secrets [(secret "shop" "web-tls" leaf :managed-by "web")]))))
  (testing "a served Secret that does not exist is left to the ingress-references detective"
    (is (empty? (run "2027-03-01T00:00:00Z" :ingresses [(ingress "shop" "web" "web-tls")] :secrets []))))
  (testing "an Ingress TLS entry without a Secret uses the controller default"
    (is (empty? (run "2027-03-01T00:00:00Z"
                     :ingresses [{:metadata {:namespace "shop" :name "web"} :spec {:tls [{:hosts ["shop.example"]}]}}]
                     :secrets [(secret "shop" "web-tls" leaf)])))))

(deftest a-secret-without-a-certificate-is-a-warning
  (is (= [[:warning "TLS Secret holds no certificate that parses"]]
         (map (juxt :severity :summary)
              (run "2026-12-01T00:00:00Z" :ingresses [(ingress "shop" "web" "web-tls")] :secrets [(secret "shop" "web-tls" nil)])))))

(deftest one-finding-per-secret-in-name-order
  (is (= ["a/tls" "b/tls"]
         (map :component (run "2027-03-01T00:00:00Z"
                              :ingresses [(ingress "b" "web" "tls") (ingress "a" "web" "tls" "tls")]
                              :secrets [(secret "b" "tls" leaf) (secret "a" "tls" bundle)])))))

(deftest without-its-evidence-the-detective-is-quiet
  (is (empty? (tls-secrets/detect-tls-secret-problems {"ingresses" {:items [(ingress "shop" "web" "web-tls")]}
                                                       "tls-secrets" [(secret "shop" "web-tls" leaf)]}))
      "no clock, no judgement")
  (is (empty? (run "2027-03-01T00:00:00Z" :ingresses [(ingress "shop" "web" "web-tls")] :secrets nil))
      "Secrets that could not be listed"))
