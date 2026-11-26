;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-unread-node-stats-test
  "A node whose kubelet stats read failed served nothing, so its instance
   pods are reported as unread with that node named, never as pods whose
   containers are not running."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.cnpg.volumes :as volumes]
            [gumshoe.detectives.cnpg-volumes :as cnpg-volumes]))

(defn- instance
  [name node]
  {:metadata {:namespace "db" :name name
              :labels {:cnpg.io/cluster "pg" :cnpg.io/podRole "instance" :cnpg.io/instanceName name}
              :ownerReferences [{:kind "Cluster" :name "pg" :controller true}]}
   :spec {:nodeName node}
   :status {:phase "Running"}})

(defn- summary-without-pods [] {:node {:nodeName "n2"} :pods []})

(defn- collected
  []
  (volumes/collect (constantly {:items [(instance "pg-1" "n1") (instance "pg-2" "n2")]})
                   {"n1" nil "n2" (summary-without-pods)}))

(deftest a-failed-node-read-is-listed-as-unread
  (let [stats (collected)]
    (is (= ["n1" "n2"] (:nodes stats)))
    (is (= ["n1"] (:unread-nodes stats)))
    (is (= [] (:volumes stats)))))

(deftest pods-on-an-unread-node-name-the-failed-read
  (let [findings (cnpg-volumes/detect-volume-pressure {volumes/evidence-key (collected)})
        by-pod (into {} (map (juxt :component identity)) findings)
        unread (get by-pod "db/pg-1")
        omitted (get by-pod "db/pg-2")]
    (is (= :warning (:severity unread)))
    (is (= "volume usage unknown - the stats read of node n1 failed" (:summary unread)))
    (is (str/includes? (:hint unread) "nodes/proxy"))
    (is (not (str/includes? (:hint unread) "containers are not running")))
    (is (= "volume usage unknown - the kubelet reported none" (:summary omitted)))
    (is (str/includes? (:hint omitted) "containers are not running"))))
