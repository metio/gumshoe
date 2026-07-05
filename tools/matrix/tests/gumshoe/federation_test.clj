;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.federation-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.matrix :as matrix]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(def now (java.time.Instant/parse "2026-07-04T00:00:00Z"))

(deftest client-api-test
  (is (= #{"the Matrix client API is unreachable"}
         (summaries (matrix/detect-client-api {"host" "synapse.example.org"
                                               "client-versions" {:reachable false :error "refused"}}))))
  (is (= #{"the client API returned HTTP 502"}
         (summaries (matrix/detect-client-api {"host" "synapse.example.org"
                                               "client-versions" {:reachable true :status 502}}))))
  (is (empty? (matrix/detect-client-api {"host" "synapse.example.org"
                                         "client-versions" {:reachable true :status 200}}))))

(deftest federation-api-test
  (is (= #{"the federation API is unreachable"}
         (summaries (matrix/detect-federation-api {"host" "synapse.example.org"
                                                   "federation-version" {:reachable false :error "timeout"}}))))
  (is (= #{"the federation API returned HTTP 404"}
         (summaries (matrix/detect-federation-api {"host" "synapse.example.org"
                                                   "federation-version" {:reachable true :status 404}}))))
  (is (empty? (matrix/detect-federation-api {"host" "synapse.example.org"
                                             "federation-version" {:reachable true :status 200}}))))

(deftest health-test
  (testing "an unreachable /health is not judged - it is optional behind proxies"
    (is (empty? (matrix/detect-health {"host" "h" "health" {:reachable false :error "refused"}}))))
  (testing "a present but failing /health is critical"
    (is (= #{"the health endpoint returned HTTP 503"}
           (summaries (matrix/detect-health {"host" "h" "health" {:reachable true :status 503}})))))
  (testing "a 404 means the /health route is simply not there - not a finding"
    (is (empty? (matrix/detect-health {"host" "h" "health" {:reachable true :status 404}})))))

(deftest signing-keys-test
  (testing "expired keys are critical"
    (is (= #{"the published signing keys have expired"}
           (summaries (matrix/detect-signing-keys
                       {:now now "host" "h"
                        "server-keys" {:reachable true :status 200
                                       :json {:valid_until_ts (.toEpochMilli
                                                               (java.time.Instant/parse "2026-07-03T00:00:00Z"))}}})))))
  (testing "keys expiring soon warn"
    (is (= #{"the signing keys are valid for only 3 more days"}
           (summaries (matrix/detect-signing-keys
                       {:now now "host" "h"
                        "server-keys" {:reachable true :status 200
                                       :json {:valid_until_ts (.toEpochMilli
                                                               (java.time.Instant/parse "2026-07-07T00:00:00Z"))}}})))))
  (testing "keys valid for a long time are silent"
    (is (empty? (matrix/detect-signing-keys
                 {:now now "host" "h"
                  "server-keys" {:reachable true :status 200
                                 :json {:valid_until_ts (.toEpochMilli
                                                         (java.time.Instant/parse "2026-12-31T00:00:00Z"))}}}))))
  (testing "an unreachable keys endpoint is critical"
    (is (= #{"the signing keys endpoint is unreachable"}
           (summaries (matrix/detect-signing-keys {:now now "host" "h"
                                                   "server-keys" {:reachable false :error "refused"}})))))
  (testing "a reachable-but-erroring keys endpoint is critical, not silently green"
    (is (= #{"the signing keys endpoint returned HTTP 500"}
           (summaries (matrix/detect-signing-keys {:now now "host" "h"
                                                   "server-keys" {:reachable true :status 500 :json nil}}))))))

(deftest wellknown-test
  (testing "absent delegation is fine (SRV/direct setups exist)"
    (is (empty? (matrix/detect-wellknown {"domain" "example.org"
                                          "wellknown-server" {:reachable false :error "404"}})))
    (is (empty? (matrix/detect-wellknown {"domain" "example.org"
                                          "wellknown-server" {:reachable true :status 404}}))))
  (testing "served-but-broken delegation warns"
    (is (= #{".well-known/matrix/server is served but has no m.server field"}
           (summaries (matrix/detect-wellknown {"domain" "example.org"
                                                "wellknown-server" {:reachable true :status 200
                                                                    :json {:something "else"}}})))))
  (testing "valid delegation is silent"
    (is (empty? (matrix/detect-wellknown {"domain" "example.org"
                                          "wellknown-server" {:reachable true :status 200
                                                              :json {(keyword "m.server") "synapse.example.org:443"}}})))))

(deftest federation-destinations-test
  (testing "without an admin token the check yields nothing"
    (is (nil? (matrix/detect-federation-destinations {"admin?" false "destinations" nil}))))
  (testing "destinations with a failure timestamp are reported, healthy ones are not"
    (let [evidence {"admin?" true
                    "destinations" {:json {:destinations [{:destination "good.org" :failure_ts nil}
                                                          {:destination "dead.org" :failure_ts 1720000000000
                                                           :retry_interval 3600000}]}}}
          findings (matrix/detect-federation-destinations evidence)]
      (is (= ["dead.org"] (map :component findings)))
      (is (= #{"federation to this server is currently failing"} (summaries findings))))))
