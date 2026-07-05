;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.grafana-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.grafana :as grafana]))

(def folders
  {:items [{:metadata {:name "folder-uid-1"} :spec {:title "Kubernetes"}}
           {:metadata {:name "folder-uid-2"} :spec {:title "Applications"}}]})

(def dashboards
  {:items [{:metadata {:name "dash-uid-1"
                       :annotations {(keyword "grafana.app/folder") "folder-uid-1"}}
            :spec {:title "Node Overview"}}
           {:metadata {:name "dash-uid-2"
                       :annotations {(keyword "grafana.app/folder") "folder-uid-2"}}
            :spec {:title "Moodle"}}]})

(deftest titles-test
  (is (= ["Applications" "Kubernetes"] (grafana/titles folders))))

(deftest find-by-title-test
  (is (= "folder-uid-1" (grafana/uid-of (grafana/find-by-title folders "Kubernetes"))))
  (is (nil? (grafana/find-by-title folders "Nope"))))

(deftest folder-annotation-test
  (testing "dashboards are filtered by their folder annotation"
    (is (= ["Node Overview"]
           (grafana/titles (grafana/in-folder dashboards "folder-uid-1"))))))
