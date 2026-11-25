;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pick-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.pick :as pick]))

(deftest a-namespaced-pick
  (is (= {:namespace "shop" :name "web"} (pick/namespaced "shop/web")))
  (is (= {:namespace "shop" :name "web"} (pick/namespaced "  shop/web ")))
  (doseq [bad [nil "" "web" "shop/" "/web" "shop/web/x" "shop /web" "Deployment shop/web"]]
    (is (nil? (pick/namespaced bad)) (pr-str bad))))

(deftest a-cluster-scoped-pick
  (is (= {:name "worker-1"} (pick/cluster-scoped "worker-1")))
  (is (= {:name "worker-1"} (pick/cluster-scoped " worker-1 ")))
  (doseq [bad [nil "" "  " "shop/web" "two words"]]
    (is (nil? (pick/cluster-scoped bad)) (pr-str bad))))

(deftest a-kinded-pick
  (let [parse (pick/kinded {"Deployment" "deployments" "StatefulSet" "statefulsets"})]
    (is (= {:kind "Deployment" :type "deployments" :namespace "shop" :name "web"} (parse "Deployment shop/web")))
    (is (= {:kind "StatefulSet" :type "statefulsets" :namespace "db" :name "pg"} (parse " StatefulSet  db/pg ")))
    (doseq [bad [nil "" "DaemonSet shop/web" "Deployment web" "shop/web" "Deployment shop/"]]
      (is (nil? (parse bad)) (pr-str bad)))))

(deftest a-typed-pick
  (let [parse (pick/typed #{"deployments" "pods"})]
    (is (= {:type "pods" :namespace "shop" :name "web-1"} (parse "pods shop/web-1")))
    (doseq [bad [nil "secrets shop/web" "pods web-1" "pods"]]
      (is (nil? (parse bad)) (pr-str bad)))))

(deftest only-a-string-is-a-pick
  (let [kinded (pick/kinded {"Deployment" "deployments"})
        typed (pick/typed #{"pods"})]
    (doseq [[parse values] [[pick/namespaced [:shop/web 'shop/web ["shop/web"] '("shop/web") {:shop "web"}]]
                            [pick/cluster-scoped [:web 'web ["web"] #{"web"} 42]]
                            [kinded [["Deployment shop/web"] (symbol "Deployment shop/web")]]
                            [typed [["pods shop/web-1"] (symbol "pods shop/web-1")]]]
            value values]
      (is (nil? (parse value)) (pr-str value)))))
