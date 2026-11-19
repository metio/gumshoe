;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.released-volumes-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.csi :as csi]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- volume [& {:keys [phase policy since message] :or {phase "Released"}}]
  {:metadata {:name "pv-1"}
   :spec (cond-> {} policy (assoc :persistentVolumeReclaimPolicy policy))
   :status (cond-> {:phase phase} since (assoc :lastPhaseTransitionTime since) message (assoc :message message))})

(deftest a-delete-volume-that-stays-released-failed-to-delete
  (let [finding (csi/released-finding (volume :policy "Delete" :since "2026-11-19T10:30:00Z" :message "backend busy") now)]
    (is (= :warning (:severity finding)))
    (is (= "pv-1" (:component finding)))
    (is (= "PersistentVolume with reclaim policy Delete stayed Released for 90 minutes - deleting it failed" (:summary finding)))
    (is (re-find #"^backend busy - the provisioner could not delete" (:hint finding)))
    (is (= "2026-11-19T10:30:00Z" (:since finding))))
  (testing "within the grace the provisioner is still at it"
    (is (nil? (csi/released-finding (volume :policy "Delete" :since "2026-11-19T11:30:00Z") now))))
  (testing "exactly an hour counts"
    (is (some? (csi/released-finding (volume :policy "Delete" :since "2026-11-19T11:00:00Z") now))))
  (testing "without a transition time it cannot wait, so it reports"
    (is (= "PersistentVolume with reclaim policy Delete stayed Released - deleting it failed"
           (:summary (csi/released-finding (volume :policy "Delete") now))))))

(deftest a-retained-volume-is-kept-on-purpose
  (doseq [policy ["Retain" nil "Recycle"]]
    (let [finding (csi/released-finding (volume :policy policy :since "2026-11-19T11:59:00Z") now)]
      (is (= :info (:severity finding)) (str policy))
      (is (= "PersistentVolume is Released - its claim is gone but the data still exists" (:summary finding)))
      (is (re-find (re-pattern (str "reclaim policy " (or policy "Retain") " keeps the data")) (:hint finding)))
      (is (not (re-find #"(?i)ceph" (:hint finding))) "no storage backend is assumed"))))

(deftest the-volume-detective
  (let [findings (csi/detect-volume-problems
                  {:now now
                   "persistentvolumes" {:items [(volume :phase "Failed" :since "2026-11-19T09:00:00Z" :message "recycler failed")
                                                (volume :policy "Delete" :since "2026-11-19T11:50:00Z")
                                                (volume :policy "Delete" :since "2026-11-19T08:00:00Z")
                                                (volume :policy "Retain")
                                                (volume :phase "Bound")]}})]
    (is (= [:critical :warning :info] (map :severity findings)))
    (is (= "2026-11-19T09:00:00Z" (:since (first findings))) "a Failed volume says since when")))
