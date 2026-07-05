;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.plugin-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.command :as command]
            [gumshoe.plugin :as plugin]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.theme :as theme]))

(deftest provide-dispatches-to-every-seam-test
  (testing "one manifest registers into several seams at once"
    (plugin/provide!
     {:announcers    {:test-sink (fn [_ _ _ _] :sent)}
      :tools         {"gizmoctl" {:version-command ["version"] :min-version "1.5"}}
      :themes        [{:name :test-theme :ok "🧪"}]
      :prerequisites {:test-gate (fn [_ _] [["gate" (constantly {:ok? true})]])}})
    (is (some? (get-method announce/announce-via :test-sink)) "announcer registered")
    (is (= "1.5" (command/tool-min-version "gizmoctl")) "tool profile registered")
    (is (contains? (set (theme/registered)) :test-theme) "theme registered")
    (is (contains? (set (prerequisites/registered-checks)) :test-gate) "prerequisite check registered")))

(deftest empty-and-partial-manifests-are-fine-test
  (testing "provide! never throws on an empty or partial manifest"
    (is (nil? (plugin/provide! {})))
    (is (nil? (plugin/provide! {:themes []})))))
