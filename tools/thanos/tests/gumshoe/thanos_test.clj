;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.thanos-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.thanos :as thanos]
            [gumshoe.thanos :as collect]))

(defn- summaries [findings] (set (map :summary findings)))

(deftest thanos-readiness-test
  (is (= #{"Thanos Query is unreachable"}
         (summaries (thanos/detect-readiness {"url" "http://localhost:19090"
                                              "ready" {:reachable false :error "refused"}}))))
  (is (= #{"readiness endpoint returned HTTP 503"}
         (summaries (thanos/detect-readiness {"url" "http://localhost:19090"
                                              "ready" {:reachable true :status 503}}))))
  (is (empty? (thanos/detect-readiness {"url" "http://localhost:19090"
                                        "ready" {:reachable true :status 200}}))))

(deftest thanos-stores-test
  (testing "a 2xx response listing zero stores is critical"
    (is (= #{"Thanos Query has no connected stores"}
           (summaries (thanos/detect-stores {"url" "http://localhost:19090"
                                             "stores" {:reachable true :status 200 :json {:data {}}}})))))
  (testing "a non-2xx stores response is reported as an API error, not 'no stores'"
    (is (= #{"the stores endpoint returned HTTP 502"}
           (summaries (thanos/detect-stores {"url" "http://localhost:19090"
                                             "stores" {:reachable true :status 502 :json nil}})))))
  (testing "a 2xx but non-JSON response is an unexpected-response error, not 'no stores'"
    (is (= #{"the stores endpoint returned an unexpected response"}
           (summaries (thanos/detect-stores {"url" "http://localhost:19090"
                                             "stores" {:reachable true :status 200 :json nil}})))))
  (testing "a store endpoint with a lastError is critical, healthy ones are silent"
    (let [evidence {"url" "http://localhost:19090"
                    "stores" {:reachable true :status 200
                              :json {:data {:sidecar [{:name "1.2.3.4:10901" :lastError nil}]
                                            :store [{:name "5.6.7.8:10901"
                                                     :lastError "rpc error: connection timeout"}]}}}}
          findings (thanos/detect-stores evidence)]
      (is (= #{"store store endpoint reports an error"} (summaries findings)))
      (is (= "5.6.7.8:10901" (:component (first findings)))))))

(deftest thanos-rules-test
  (is (= #{"rule fails to evaluate"}
         (summaries (thanos/detect-rules
                     {"rules" {:reachable true :status 200
                               :json {:data {:groups [{:name "recording"
                                                       :rules [{:name "job:up" :health "ok"}
                                                               {:name "job:errs" :health "err"
                                                                :lastError "parse error"}]}]}}}}))))
  (is (empty? (thanos/detect-rules
               {"rules" {:reachable true :status 200
                         :json {:data {:groups [{:name "g" :rules [{:name "r" :health "ok"}]}]}}}}))))

(deftest a-rules-endpoint-that-errors-is-not-a-clean-pass
  (is (= ["the rules endpoint returned HTTP 502"]
         (map :summary (thanos/detect-rules {"url" "https://thanos.example.org"
                                             "rules" {:reachable true :status 502 :body "bad gateway"}}))))
  (is (= ["the rules endpoint returned an unexpected response"]
         (map :summary (thanos/detect-rules {"url" "https://thanos.example.org"
                                             "rules" {:reachable true :status 200 :body "<html>"}}))))
  (is (empty? (thanos/detect-rules {"url" "u" "rules" nil}))
      "a rules read that did not answer is unread, not judged"))

(defn- unread-names [evidence]
  (set (map :name (filter :unread (detective/mark-unread thanos/detectives evidence)))))

(deftest an-endpoint-that-did-not-answer-is-nil-in-the-evidence
  (is (nil? (collect/answered {:reachable false :error "body did not arrive in time"})))
  (is (= {:reachable true :status 502} (collect/answered {:reachable true :status 502}))
      "an endpoint that answered with an error is a read")
  (let [down {:reachable false :error "refused"}
        evidence (collect/evidence "http://localhost:19090" down down down)]
    (is (= down (get evidence "ready")) "readiness keeps the response it judges")
    (is (nil? (get evidence "stores")))
    (is (nil? (get evidence "rules")))))

(deftest stores-and-rules-are-unread-when-their-endpoint-did-not-answer-though-ready-did
  (let [evidence (collect/evidence "http://localhost:19090"
                                   {:reachable true :status 200}
                                   {:reachable false :error "body did not arrive in time"}
                                   {:reachable false :error "connection reset"})]
    (is (= #{"thanos-stores" "thanos-rules"} (unread-names evidence)))
    (is (empty? (thanos/detect-stores evidence)))
    (is (empty? (thanos/detect-rules evidence))))
  (testing "endpoints that answered are judged"
    (is (empty? (unread-names (collect/evidence "u" {:reachable true :status 200}
                                                {:reachable true :status 502}
                                                {:reachable true :status 502}))))))
