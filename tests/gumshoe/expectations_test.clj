;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.expectations-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detectives.expectations :as expectations]))

(deftest missing-capabilities-test
  (testing "a declared capability with a detector that is not present is missing"
    (is (= [:calico :flux]
           (expectations/missing-capabilities [:calico :flux :ceph]
                                              #{:calico :flux :ceph}   ; all have detectors
                                              #{:ceph}))))              ; only ceph is present
  (testing "a declared capability with no detector is skipped - it can't be verified, not an outage"
    (is (= []
           (expectations/missing-capabilities [:gateway-api]           ; declared
                                              #{:calico :ceph}          ; but no detector for it
                                              #{}))))
  (testing "nothing declared means nothing is checked (opt-in)"
    (is (= [] (expectations/missing-capabilities nil #{:calico} #{})))
    (is (= [] (expectations/missing-capabilities [] #{:calico} #{})))))

(deftest detect-missing-capabilities-test
  (with-redefs [capabilities/registered (constantly #{:calico :ceph})
                capabilities/detect-present (constantly #{:ceph})]
    (testing "a declared-but-absent capability becomes a critical finding named by capability"
      (let [findings (expectations/detect-missing-capabilities
                      {"config" {:capabilities [:calico :ceph]}})]
        (is (= 1 (count findings)))
        (is (= "calico" (:component (first findings))))
        (is (= :critical (:severity (first findings))))))
    (testing "when every declared capability is present, there are no findings"
      (is (empty? (expectations/detect-missing-capabilities
                   {"config" {:capabilities [:ceph]}}))))))
