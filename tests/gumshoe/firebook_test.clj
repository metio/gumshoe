;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.firebook-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.firebook :as firebook]))

(deftest manifest-test
  (testing "every drill resource lives in the drill namespace"
    (is (= firebook/drill-namespace
           (-> (firebook/deployment-manifest {:name "crash-loop" :image "img"})
               :metadata :namespace)))
    (is (= firebook/drill-namespace
           (-> (firebook/pvc-manifest {:name "pvc" :storage-class "none"})
               :metadata :namespace))))
  (testing "a command is only set when given"
    (is (= ["sh" "-c" "exit 1"]
           (-> (firebook/deployment-manifest {:name "x" :image "img" :command ["sh" "-c" "exit 1"]})
               :spec :template :spec :containers first :command)))
    (is (nil? (-> (firebook/deployment-manifest {:name "x" :image "img"})
                  :spec :template :spec :containers first :command))))
  (testing "the pvc requests the broken storage class"
    (is (= "none"
           (-> (firebook/pvc-manifest {:name "pvc" :storage-class "none"})
               :spec :storageClassName)))))
