;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.statefulset-orphan-delete-test
  "Where an orphan-deleted StatefulSet stands when the resize stopped at its
   delete: being deleted, still there, gone or unread, each read through
   kubectl as the recovery text needs it."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.shell :as shell]
            [gumshoe.storage :as storage]))

(defn- state-when
  "statefulset-state against a kubectl that answers a get of the
   StatefulSet with get-answer and the presence read with presence-answer."
  [get-answer presence-answer]
  (with-redefs [shell/execute (fn [& argv]
                                (if (some #{"--output=name"} argv) presence-answer get-answer))]
    (storage/statefulset-state "ctx" "shop" "web")))

(def ^:private unanswered {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"})

(deftest a-statefulset-marked-for-deletion-is-being-deleted
  (is (= :deleting (state-when {:exit 0 :err ""
                                :out (json/generate-string {:metadata {:name "web" :deletionTimestamp "2026-11-26T10:00:00Z"
                                                                       :finalizers ["orphan"]}})}
                               nil))))

(deftest a-statefulset-without-a-deletion-timestamp-is-present
  (is (= :present (state-when {:exit 0 :err "" :out (json/generate-string {:metadata {:name "web"}})} nil))))

(deftest a-statefulset-the-cluster-says-is-gone-is-gone
  (is (= :gone (state-when {:exit 1 :out "" :err "Error from server (NotFound): statefulsets.apps \"web\" not found"}
                           {:exit 0 :out "" :err ""}))))

(deftest a-read-that-did-not-answer-is-unread
  (is (= :unread (state-when unanswered unanswered))))

(deftest the-recovery-reads-the-statefulset-only-after-a-stop-at-the-delete
  (let [statefulset {:metadata {:namespace "shop" :name "web"}
                     :spec {:volumeClaimTemplates [{:metadata {:name "data"}}]}}
        steps (storage/statefulset-resize-steps "ctx" statefulset "data" "20Gi"
                                                [{:pvc "data-web-0" :current "10Gi" :target "20Gi"}])]
    (testing "every stage a stop at the delete can leave"
      (is (= [:claims :delete :gone :recreate] (mapv :stage steps))))
    (testing "a stalled garbage collector stops the wait, and the text says the StatefulSet is being deleted"
      (let [text (storage/statefulset-recovery {:context "ctx" :target "shop/web" :state :deleting} steps 2)]
        (is (str/includes? text "IS BEING DELETED"))
        (is (str/includes? text "kube-controller-manager"))))))
