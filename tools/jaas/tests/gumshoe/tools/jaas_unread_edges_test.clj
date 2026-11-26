;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.jaas-unread-edges-test
  "Who imports a JsonnetLibrary is only known when the snippet list answered:
   a refused list is named as unread in the drill-down, never shown as a
   library nobody imports."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.investigation :as investigation]
            [gumshoe.shell :as shell]
            [gumshoe.subject :as subject]
            [gumshoe.tools.jaas]))

(def ^:private forbidden
  {:exit 1 :out ""
   :err "Error from server (Forbidden): jsonnetsnippets.jaas.metio.wtf is forbidden: User \"viewer\" cannot list resource \"jsonnetsnippets\" at the cluster scope"})

(deftest a-refused-snippet-list-is-unread-never-no-consumers
  (with-redefs [shell/execute (fn [& _] forbidden)]
    (let [{:keys [edges unread]} (investigation/plugin-fetched-edges-result
                                  "ctx" (subject/subject "JsonnetLibrary" "apps" "grafonnet") {})]
      (is (empty? edges))
      (is (= ["what relates to this JsonnetLibrary"] (map :what unread)))
      (is (str/includes? (:why (first unread)) "Forbidden")))))

(deftest an-answered-empty-snippet-list-has-no-consumers
  (with-redefs [shell/execute (fn [& _] {:exit 0 :out "{\"items\":[]}" :err ""})]
    (is (= {:edges [] :unread []}
           (investigation/plugin-fetched-edges-result
            "ctx" (subject/subject "JsonnetLibrary" "apps" "grafonnet") {})))))
