;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.investigation-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.investigation :as investigation]
            [gumshoe.subject :as subject]))

(deftest recommended-probe-test
  (testing "each broken situation points at the probe that explains it"
    (is (= :logs-previous (investigation/recommended-probe :crash-looping)))
    (is (= :describe (investigation/recommended-probe :pending)))
    (is (= :logs (investigation/recommended-probe :failed))))
  (testing "a healthy subject has no recommendation"
    (is (nil? (investigation/recommended-probe :ok)))))

(deftest applicable-probes-test
  (testing "pod probes include logs and describe, with the recommended one first"
    (let [keys (map :key (investigation/applicable-probes "Pod" :logs-previous))]
      (is (= :logs-previous (first keys)))
      (is (contains? (set keys) :describe))
      (is (contains? (set keys) :top))))
  (testing "a node has describe/top/yaml but no pod-log probes"
    (let [keys (set (map :key (investigation/applicable-probes "Node" nil)))]
      (is (contains? keys :describe))
      (is (contains? keys :top))
      (is (not (contains? keys :logs)))
      (is (not (contains? keys :logs-previous)))))
  (testing "a PVC gets only the kind-agnostic probes"
    (let [keys (set (map :key (investigation/applicable-probes "PersistentVolumeClaim" nil)))]
      (is (= #{:describe :yaml} keys)))))

(deftest host-matches?-test
  (testing "exact and wildcard hostnames"
    (is (investigation/host-matches? "moodle.example.org" "moodle.example.org"))
    (is (investigation/host-matches? "*.apps.example.org" "moodle.apps.example.org"))
    (is (not (investigation/host-matches? "*.apps.example.org" "apps.example.org"))
        "a wildcard does not match the bare parent domain")
    (is (not (investigation/host-matches? "other.example.org" "moodle.example.org")))))

(deftest routes-for-host-test
  (let [routes [{:metadata {:namespace "moodle" :name "web"}
                 :spec {:hostnames ["moodle.example.org"]}}
                {:metadata {:namespace "wiki" :name "wiki"}
                 :spec {:hostnames ["wiki.example.org" "*.apps.example.org"]}}]]
    (testing "matches the route serving the host, exact or wildcard"
      (is (= ["web"] (map #(-> % :metadata :name) (investigation/routes-for-host routes "moodle.example.org"))))
      (is (= ["wiki"] (map #(-> % :metadata :name) (investigation/routes-for-host routes "anything.apps.example.org")))))
    (testing "an unrelated host matches nothing"
      (is (empty? (investigation/routes-for-host routes "nope.example.org"))))))

(deftest ingresses-for-host-test
  (let [ingresses [{:metadata {:namespace "moodle" :name "moodle"}
                    :spec {:rules [{:host "moodle.example.org"} {:host "www.moodle.example.org"}]}}
                   {:metadata {:namespace "wiki" :name "wiki"}
                    :spec {:rules [{:host "wiki.example.org"}]}}]]
    (testing "matches the ingress serving the host across its rules"
      (is (= ["moodle"] (map #(-> % :metadata :name)
                             (investigation/ingresses-for-host ingresses "www.moodle.example.org")))))
    (testing "an unrelated host matches nothing"
      (is (empty? (investigation/ingresses-for-host ingresses "nope.example.org"))))))

(deftest parse-top-usage-test
  (testing "a pod top line yields cpu and memory"
    (is (= "cpu 250m · mem 498Mi" (investigation/parse-top-usage "Pod" "web-1   250m   498Mi"))))
  (testing "a node top line yields cpu and memory with percentages"
    (is (= "cpu 1200m (30%) · mem 8Gi (60%)"
           (investigation/parse-top-usage "Node" "worker-3   1200m   30%   8Gi   60%"))))
  (testing "an empty or malformed line (no metrics-server, a dead pod) yields nil"
    (is (nil? (investigation/parse-top-usage "Pod" "")))
    (is (nil? (investigation/parse-top-usage "Pod" "just-a-name")))
    (is (nil? (investigation/parse-top-usage "Node" "worker-3   1200m")))))

(deftest trail-round-trip-test
  (testing "a saved trail loads back as the same subjects, so --resume picks up where it left off"
    (let [path (str (fs/path (fs/temp-dir) "bookstore-trail-test" "last.edn"))
          trail [(subject/subject "Pod" "moodle" "web-1")
                 (subject/subject "Node" nil "worker-3")]]
      (investigation/save-trail! path trail)
      (is (= trail (investigation/load-trail path)))))
  (testing "loading a path that does not exist is nil, never an error"
    (is (nil? (investigation/load-trail (str (fs/path (fs/temp-dir) "does-not-exist-xyz.edn")))))))

(deftest menu-items-test
  (let [focus {:subject (subject/subject "Pod" "moodle" "web-1")
               :situation :crash-looping
               :edges [{:relation "runs on" :subject (subject/subject "Node" nil "worker-3") :situation :disk-pressure}]}]
    (testing "the recommended probe leads the menu and is marked"
      (let [items (investigation/menu-items focus [])
            first-item (first items)]
        (is (= :probe (:type first-item)))
        (is (= :logs-previous (-> first-item :probe :key)))
        (is (str/includes? (:label first-item) "recommended"))))
    (testing "related objects appear as pivots, carrying their own situation badge"
      (let [pivot (first (filter #(= :pivot (:type %)) (investigation/menu-items focus [])))]
        (is (= (subject/subject "Node" nil "worker-3") (:subject pivot)))
        (is (str/includes? (:label pivot) "disk pressure"))))
    (testing "back appears only when there is a trail; done is always last"
      (is (not-any? #(= :back (:type %)) (investigation/menu-items focus [])))
      (is (some #(= :back (:type %)) (investigation/menu-items focus [(subject/subject "Node" nil "x")])))
      (is (= :done (:type (last (investigation/menu-items focus []))))))))

(deftest register-probe-and-tool-gating-test
  (reset! @#'investigation/extra-probes [])
  (testing "a plugin probe appears in the drill-down menu for its kind"
    (investigation/register-probe! {:key :widget-status :label "widget status"
                                    :kinds #{"WidgetSet"} :args (fn [_ _] ["echo" "ok"])})
    (is (some #(= :widget-status (:key %)) (investigation/applicable-probes "WidgetSet" nil))))
  (testing "a probe whose tools are not installed is not offered"
    (investigation/register-probe! {:key :needs-missing :label "x" :kinds :any
                                    :tools ["definitely-not-a-real-tool-xyz"]
                                    :args (fn [_ _] [])})
    (is (not (some #(= :needs-missing (:key %)) (investigation/applicable-probes "Pod" nil)))))
  (reset! @#'investigation/extra-probes []))
