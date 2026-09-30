;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.firebook-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.evpn :as evpn-detectives]
            [gumshoe.evpn :as evpn]
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

(deftest fabric-drill-values-cannot-collide-with-a-real-fabric
  (testing "the MAC derives from RFC 5737 documentation space, so no guest can hold it"
    (is (= "02:00:cb:00:71:01" firebook/drill-mac))
    (is (str/starts-with? firebook/drill-mac "02:00:") "still an OpenNebula-shaped guest MAC"))
  (testing "the destination is not a VTEP anywhere, so nothing is attracted to it"
    (is (= "192.0.2.1" firebook/drill-vtep))))

(deftest fabric-drill-command-assembly
  (testing "replace, so relighting a burning drill is not an error"
    (is (= ["sudo" "bridge" "fdb" "replace" "02:00:cb:00:71:01" "dev" "lo.4208"
            "dst" "192.0.2.1" "self" "static"]
           (firebook/fabric-pin-args 4208))))
  (testing "putting it out names the same device and MAC"
    (is (= ["sudo" "bridge" "fdb" "del" "02:00:cb:00:71:01" "dev" "lo.4208"]
           (firebook/fabric-unpin-args 4208))))
  (testing "the drill entry is what the fabric detective reports"
    (let [row (str firebook/drill-mac " dev lo.4208 dst " firebook/drill-vtep " self static\n")
          rows (vec (evpn/parse-fdb 4208 "host-a" row))
          findings (evpn-detectives/detect-static-fdb {"evpn-fdb" rows})]
      (is (= 1 (count findings)))
      (is (= :warning (:severity (first findings))))
      (is (str/includes? (:summary (first findings)) firebook/drill-vtep)))))
