;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-volumes-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.cnpg.volumes :as volumes]
            [gumshoe.detectives.cnpg-volumes :as cnpg-volumes]))

(defn- summaries [findings] (set (map :summary findings)))

(defn- instance-pod
  [name node]
  {:metadata {:namespace "cloudnative-pg-cluster-general"
              :name name
              :labels {:cnpg.io/cluster "cloudnative-pg-cluster-general"
                       :cnpg.io/podRole "instance"}}
   :spec {:nodeName node}})

;; The live reading from kube.bastelgenosse.de: -3 at 97% of its volume, -1 with
;; room to spare, and -2 - the crashlooping primary, the pod in question -
;; absent from the kubelet's answer entirely.
(def ^:private pods
  [(instance-pod "cloudnative-pg-cluster-general-1" "localstorage-grcuxyti")
   (instance-pod "cloudnative-pg-cluster-general-2" "localstorage-weukhdtc")
   (instance-pod "cloudnative-pg-cluster-general-3" "localstorage-ybbc8gki")])

(defn- row
  [pod volume used capacity]
  {:namespace "cloudnative-pg-cluster-general" :pod pod :volume volume
   :used-bytes used :capacity-bytes capacity})

(def ^:private stats
  {:instance-pods [{:namespace "cloudnative-pg-cluster-general"
                    :pod "cloudnative-pg-cluster-general-1"
                    :node "localstorage-grcuxyti"}
                   {:namespace "cloudnative-pg-cluster-general"
                    :pod "cloudnative-pg-cluster-general-2"
                    :node "localstorage-weukhdtc"}
                   {:namespace "cloudnative-pg-cluster-general"
                    :pod "cloudnative-pg-cluster-general-3"
                    :node "localstorage-ybbc8gki"}]
   :nodes ["localstorage-grcuxyti" "localstorage-weukhdtc" "localstorage-ybbc8gki"]
   :volumes [(row "cloudnative-pg-cluster-general-1" "pgdata" 1535000000 21020000000)
             (row "cloudnative-pg-cluster-general-3" "pgdata" 20410000000 20960000000)]})

(defn- findings [] (cnpg-volumes/detect-volume-pressure {volumes/evidence-key stats}))

(deftest a-nearly-full-instance-volume-is-critical-test
  (let [full (first (filter #(= :critical (:severity %)) (findings)))]
    (is (= "cloudnative-pg-cluster-general/cloudnative-pg-cluster-general-3"
           (:component full)))
    (is (= "pgdata is 97% full (19.0Gi of 19.5Gi)" (:summary full)))))

(deftest a-roomy-instance-volume-is-quiet-test
  (is (not-any? #(str/includes? (:component %) "general-1") (findings))))

(deftest a-pod-the-kubelet-omitted-is-reported-as-unknown-test
  (let [unknown (first (filter #(str/includes? (:component %) "general-2") (findings)))]
    (is (= :warning (:severity unknown)))
    (is (= "volume usage unknown - the kubelet reported none" (:summary unknown)))
    (is (str/includes? (:hint unknown) "localstorage-weukhdtc"))
    (is (str/includes? (:hint unknown) "unmeasured, not low"))))

(deftest a-pooler-pod-on-the-same-node-is-not-reported-test
  (let [with-pooler (update stats :volumes conj
                            (row "cloudnative-pg-cluster-general-rw-session-5f9d546867-nnlcd"
                                 "scratch-data" 20000000000 20960000000))
        reported (cnpg-volumes/detect-volume-pressure {volumes/evidence-key with-pooler})]
    (is (not-any? #(str/includes? (:component %) "rw-session") reported))))

(deftest the-warning-band-is-distinct-from-critical-test
  (is (= :warning (volumes/severity-for (row "p" "pgdata" 85 100))))
  (is (= :critical (volumes/severity-for (row "p" "pgdata" 90 100))))
  (is (nil? (volumes/severity-for (row "p" "pgdata" 10 100)))))

(deftest a-volume-with-no-byte-counts-is-dropped-test
  (is (empty? (volumes/summary->volumes
               {:pods [{:podRef {:namespace "n" :name "p"}
                        :volume [{:name "pgdata"}
                                 {:name "shm" :usedBytes 0 :capacityBytes 0}]}]}))))

(deftest a-node-summary-flattens-to-rows-test
  (is (= [{:namespace "n" :pod "p" :volume "pgdata"
           :used-bytes 1000 :capacity-bytes 2000}]
         (volumes/summary->volumes
          {:pods [{:podRef {:namespace "n" :name "p"}
                   :volume [{:name "pgdata" :usedBytes 1000 :capacityBytes 2000}]}]}))))

(deftest collect-queries-only-nodes-hosting-instance-pods-test
  (let [unrelated {:metadata {:namespace "other" :name "web"} :spec {:nodeName "worker-xnsjiqfw"}}
        asked (atom [])
        collected (volumes/collect (fn [] {:items (conj pods unrelated)})
                                  (fn [node] (swap! asked conj node) nil))]
    (is (= ["localstorage-grcuxyti" "localstorage-weukhdtc" "localstorage-ybbc8gki"]
           (:nodes collected)))
    (is (= 3 (count (:instance-pods collected))) "the unrelated pod is not an instance")
    (is (not (some #{"worker-xnsjiqfw"} @asked)) "a node with no instance pod is not queried")))

(deftest collect-keeps-the-pods-it-looked-for-when-a-node-answers-nothing-test
  (let [collected (volumes/collect (fn [] {:items pods}) (fn [_node] nil))]
    (is (empty? (:volumes collected)))
    (is (= 3 (count (:instance-pods collected)))
        "a failed stats read leaves the pods unknown, not absent")))
