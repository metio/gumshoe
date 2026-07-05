;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.calico-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detectives.calico :as calico]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.calico]))

(defn- summaries [findings] (set (map :summary findings)))

(deftest calico-detective-test
  (let [evidence {calico/tigerastatus-type
                  {:items [{:metadata {:name "calico"}
                            :status {:conditions [{:type "Available" :status "False"
                                                   :message "not all pods are ready"}]}}
                           {:metadata {:name "apiserver"}
                            :status {:conditions [{:type "Available" :status "True"}
                                                  {:type "Degraded" :status "True"
                                                   :reason "PodFailure"}]}}]}}
        findings (calico/detect-calico-problems evidence)]
    (is (= #{"tigera component is not Available"
             "tigera component is Degraded (PodFailure)"}
           (summaries findings)))))

(deftest package-registers-platform-scope-and-capability-test
  (is (seq (registry/for-scope :platform)) "the package joins the :platform scan scope")
  (is (contains? (set (capabilities/registered)) :calico)))
