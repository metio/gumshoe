;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.gateway-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detectives.gateway :as gateway]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.gateway]))

(defn- summaries [findings] (set (map :summary findings)))

(deftest gateway-detective-test
  (let [evidence {gateway/gateway-type
                  {:items [{:metadata {:namespace "traffic" :name "main"}
                            :status {:conditions [{:type "Accepted" :status "True"}
                                                  {:type "Programmed" :status "False"
                                                   :reason "AddressNotAssigned"}]
                                     :listeners [{:name "https"
                                                  :attachedRoutes 3
                                                  :conditions [{:type "ResolvedRefs" :status "False"
                                                                :reason "InvalidCertificateRef"}]}
                                                 {:name "unused"
                                                  :attachedRoutes 0
                                                  :conditions [{:type "Accepted" :status "True"}]}]}}]}}
        findings (gateway/detect-gateway-problems evidence)]
    (is (= #{"Gateway is not Programmed (AddressNotAssigned)"
             "listener https is not ResolvedRefs (InvalidCertificateRef)"
             "listener unused has no attached routes"}
           (summaries findings)))))

(deftest httproute-detective-test
  (let [evidence {gateway/httproute-type
                  {:items [{:metadata {:namespace "moodle" :name "web"}
                            :status {:parents [{:parentRef {:name "main"}
                                                :conditions [{:type "Accepted" :status "False"
                                                              :reason "NoMatchingListenerHostname"}]}]}}
                           {:metadata {:namespace "orphan" :name "lost"}
                            :status {:parents []}}
                           {:metadata {:namespace "fine" :name "ok"}
                            :status {:parents [{:parentRef {:name "main"}
                                                :conditions [{:type "Accepted" :status "True"}]}]}}]}}
        findings (gateway/detect-httproute-problems evidence)]
    (is (= #{"HTTPRoute is not Accepted by main (NoMatchingListenerHostname)"
             "HTTPRoute is not accepted by any Gateway or ListenerSet"}
           (summaries findings)))))

(deftest dummy-listener-skipped-test
  (testing "the mandatory dummy listener (zero routes by design) is not reported, others still are"
    (let [evidence {gateway/gateway-type
                    {:items [{:metadata {:namespace "traffic" :name "main"}
                              :status {:conditions [{:type "Accepted" :status "True"}
                                                    {:type "Programmed" :status "True"}]
                                       :listeners [{:name "dummy" :attachedRoutes 0
                                                    :conditions [{:type "Accepted" :status "True"}]}
                                                   {:name "real" :attachedRoutes 0
                                                    :conditions [{:type "Accepted" :status "True"}]}]}}]}}
          findings (gateway/detect-gateway-problems evidence)]
      (is (= #{"listener real has no attached routes"} (summaries findings)))))
  (testing "the dummy listener name is configurable through env.edn"
    (let [evidence {"config" {:gateway {:dummy-listener "placeholder"}}
                    gateway/gateway-type
                    {:items [{:metadata {:namespace "traffic" :name "main"}
                              :status {:listeners [{:name "placeholder" :attachedRoutes 0}]}}]}}]
      (is (empty? (gateway/detect-gateway-problems evidence))))))

(deftest listenerset-detective-test
  (let [gateways {:items [{:metadata {:namespace "traffic" :name "main"}
                           :status {:conditions [{:type "Accepted" :status "True"}]}}]}
        listenersets {:items [{:metadata {:namespace "moodle" :name "refused"}
                               :spec {:parentRef {:namespace "traffic" :name "main"}}
                               :status {:conditions [{:type "Accepted" :status "False"
                                                      :reason "NotAllowed"}]}}
                              {:metadata {:namespace "moodle" :name "dangling"}
                               :spec {:parentRef {:namespace "traffic" :name "gone"}}
                               :status {:conditions [{:type "Accepted" :status "True"}]}}
                              {:metadata {:namespace "moodle" :name "healthy"}
                               :spec {:parentRef {:namespace "traffic" :name "main"}}
                               :status {:conditions [{:type "Accepted" :status "True"}
                                                     {:type "Programmed" :status "True"}]}}]}
        evidence {gateway/gateway-type gateways
                  "xlistenersets.gateway.networking.x-k8s.io" listenersets}
        findings (gateway/detect-listenerset-problems evidence)]
    (testing "a ListenerSet the Gateway refused (Accepted=False) is reported - not in the allowed set"
      (is (contains? (summaries findings) "ListenerSet is not Accepted (NotAllowed)")))
    (testing "a ListenerSet whose parent Gateway does not exist is reported"
      (is (contains? (summaries findings)
                     "ListenerSet points at Gateway traffic/gone which does not exist")))
    (testing "a healthy ListenerSet attached to a real Gateway is silent"
      (is (= 2 (count findings))))))

(deftest package-registers-traffic-scope-and-capability-test
  (is (seq (registry/for-scope :traffic)) "the package fills the :traffic scan scope")
  (is (contains? (set (capabilities/registered)) :gateway-api)))
