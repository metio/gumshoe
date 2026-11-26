;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.stageset-unread-edges-test
  "What a StageSet applied through is only known when the StageInventory
   list answered: a refused list is named as unread in the drill-down, never
   shown as a StageSet without inventories."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.investigation :as investigation]
            [gumshoe.shell :as shell]
            [gumshoe.subject :as subject]
            [gumshoe.tools.stageset]))

(def ^:private forbidden
  {:exit 1 :out ""
   :err "Error from server (Forbidden): stageinventories.stages.metio.wtf is forbidden: User \"viewer\" cannot list resource \"stageinventories\" at the cluster scope"})

(deftest a-refused-inventory-list-is-unread-never-no-inventories
  (with-redefs [shell/execute (fn [& _] forbidden)]
    (let [{:keys [edges unread]} (investigation/plugin-fetched-edges-result
                                  "ctx" (subject/subject "StageSet" "apps" "web") {})]
      (is (empty? edges))
      (is (= ["what relates to this StageSet"] (map :what unread)))
      (is (str/includes? (:why (first unread)) "Forbidden")))))

(deftest an-answered-empty-inventory-list-has-no-inventories
  (with-redefs [shell/execute (fn [& _] {:exit 0 :out "{\"items\":[]}" :err ""})]
    (is (= {:edges [] :unread []}
           (investigation/plugin-fetched-edges-result
            "ctx" (subject/subject "StageSet" "apps" "web") {})))))
