;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pod-level-resources-test
  "Pod-level resources (spec.resources) are listed on a row of their own, so
   a pod budgeted only at that level does not read as one without requests."
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.kubectl :as kubectl]))

(def ^:private pod
  {:metadata {:namespace "shop" :name "web-1"}
   :spec {:resources {:requests {:cpu "1" :memory "1Gi"} :limits {:memory "2Gi"}}
          :containers [{:name "web" :image "app:1"}
                       {:name "worker" :image "app:1"}]}})

(deftest pod-level-resources-get-a-pod-row-first
  (is (= [{:namespace "shop" :pod "web-1" :container "-" :role :pod
           :memory-request "1Gi" :memory-limit "2Gi" :cpu-request "1" :cpu-limit nil}
          {:namespace "shop" :pod "web-1" :container "web" :role :container
           :memory-request nil :memory-limit nil :cpu-request nil :cpu-limit nil}
          {:namespace "shop" :pod "web-1" :container "worker" :role :container
           :memory-request nil :memory-limit nil :cpu-request nil :cpu-limit nil}]
         (kubectl/container-resource-rows {:items [pod]}))))

(deftest a-pod-without-pod-level-resources-has-no-pod-row
  (is (= ["web" "worker"]
         (map :container (kubectl/container-resource-rows {:items [(update pod :spec dissoc :resources)]}))))
  (is (= ["web" "worker"]
         (map :container (kubectl/container-resource-rows {:items [(assoc-in pod [:spec :resources] {})]})))))

(deftest the-pod-row-never-counts-as-an-image
  (is (= {"app:1" 2} (kubectl/image-counts {:items [pod]}))))
