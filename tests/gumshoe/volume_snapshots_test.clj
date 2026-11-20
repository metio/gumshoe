;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.volume-snapshots-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.detectives.snapshots :as snapshots]))

(def ^:private now (java.time.Instant/parse "2026-11-20T12:00:00Z"))

(defn- snapshot [name & {:keys [ready error bound class created deleting]
                         :or {bound "snapcontent-1" class "csi-hostpath" created "2026-11-20T11:00:00Z"}}]
  {:metadata (cond-> {:namespace "db" :name name :creationTimestamp created}
               deleting (assoc :deletionTimestamp "2026-11-20T11:59:00Z"))
   :spec {:source {:persistentVolumeClaimName "data-0"} :volumeSnapshotClassName class}
   :status (cond-> {}
             (some? ready) (assoc :readyToUse ready)
             bound (assoc :boundVolumeSnapshotContentName bound)
             error (assoc :error error))})

(defn- detect [& snaps]
  (vec (snapshots/detect-snapshot-problems
        {snapshots/snapshot-type {:items snaps}
         snapshots/class-type {:items [{:metadata {:name "csi-hostpath"}}]}
         :now now})))

(deftest ready-and-in-progress-snapshots-are-quiet
  (is (= [] (detect (snapshot "ok" :ready true)
                    (snapshot "cutting" :ready false)
                    (snapshot "unknown")))
      "bound without an error is a driver still at work"))

(deftest an-error-is-the-current-failure
  (let [[finding] (detect (snapshot "nightly" :ready false
                                    :error {:message "failed to take snapshot: rpc error: code = ResourceExhausted"
                                            :time "2026-11-20T11:30:00Z"}))]
    (is (= {:severity :warning
            :component "db/nightly"
            :summary "VolumeSnapshot is not ready: the snapshot controller reports an error"
            :hint (str "failed to take snapshot: rpc error: code = ResourceExhausted - snapshot of PersistentVolumeClaim data-0; "
                       "the controller retries, and restores or backups from it fail until it succeeds")
            :since "2026-11-20T11:30:00Z"}
           finding)))
  (testing "an error left on a ready snapshot is not reported"
    (is (= [] (detect (snapshot "fine" :ready true :error {:message "old"}))))))

(deftest a-snapshot-nobody-picked-up
  (let [[finding] (detect (snapshot "orphan" :bound nil))]
    (is (= "VolumeSnapshot has no VolumeSnapshotContent" (:summary finding)))
    (is (= "2026-11-20T11:00:00Z" (:since finding)))
    (is (re-find #"has not picked it up" (:hint finding))))
  (is (re-find #"its VolumeSnapshotClass gone does not exist"
               (:hint (first (detect (snapshot "orphan" :bound nil :class "gone"))))))
  (is (= [] (detect (snapshot "new" :bound nil :created "2026-11-20T11:55:00Z"))) "five minutes old"))

(deftest pre-provisioned-sources-are-named
  (let [snap (assoc-in (snapshot "imported" :ready false :error {:message "content not found"})
                       [:spec :source] {:volumeSnapshotContentName "existing-content"})]
    (is (re-find #"snapshot of VolumeSnapshotContent existing-content" (:hint (first (detect snap)))))))

(deftest deleting-snapshots-are-left-alone
  (is (= [] (detect (snapshot "going" :ready false :error {:message "x"} :deleting true)))))

(deftest no-class-evidence-falls-back-to-the-generic-hint
  (is (re-find #"has not picked it up"
               (:hint (first (snapshots/detect-snapshot-problems
                              {snapshots/snapshot-type {:items [(snapshot "o" :bound nil :class "gone")]} :now now}))))))

(deftest joins-the-workloads-scope
  (is (some #(= "volume-snapshots" (:name %)) (registry/for-scope :workloads))))
