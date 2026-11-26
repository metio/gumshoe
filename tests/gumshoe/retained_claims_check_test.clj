;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.retained-claims-check-test
  "The claims a scaled-down StatefulSet keeps are mounted by no pod, so a
   driver that grows the filesystem on the node leaves them at
   FileSystemResizePending with the old capacity until a pod mounts them.
   statefulset-checks confirms the claims of the running replicas by their
   capacity, as before, and counts a kept claim as done once it accepted
   the resize, without waiting for a capacity it cannot report yet; a kept
   claim whose resize was rejected still fails, at once."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.shell :as shell]
            [gumshoe.storage :as storage]
            [gumshoe.verify :as verify]))

(def ^:private since (java.time.Instant/parse "2026-11-26T10:00:00Z"))

(defn- claim
  [name & {:keys [requested reported conditions allocated] :or {requested "20Gi" reported "10Gi"}}]
  {:metadata {:namespace "db" :name name :uid (str "uid-" name)}
   :spec {:resources {:requests {:storage requested}}}
   :status (cond-> {:capacity {:storage reported}}
             conditions (assoc :conditions conditions)
             allocated (assoc :allocatedResourceStatuses allocated))})

(def ^:private pending [{:type "FileSystemResizePending" :status "True"
                         :message "Waiting for user to (re-)start a pod to finish file system resize of volume on node."}])

(defn- event
  [claim-name reason at & {:keys [type] :or {type "Warning"}}]
  {:type type :reason reason :message (str reason " for " claim-name "\n  rpc error: quota exceeded")
   :lastTimestamp at
   :involvedObject {:kind "PersistentVolumeClaim" :name claim-name}})

(defn- verdict
  [claim & [events]]
  (storage/retained-claim-verdict "data-web-3" claim (or events []) since "20Gi"))

(deftest a-kept-claim-is-judged-by-what-it-accepted
  (testing "capacity reached"
    (is (= {:claim "data-web-3" :state :grown :line "kept claim data-web-3: grew to 20Gi"}
           (verdict (claim "data-web-3" :reported "20Gi")))))
  (testing "FileSystemResizePending: the filesystem grows on the next mount"
    (is (= {:claim "data-web-3" :state :accepted
            :line "kept claim data-web-3: resize accepted, the filesystem grows when a pod mounts it"}
           (verdict (claim "data-web-3" :conditions pending)))))
  (testing "requested, the resizer has not reported yet"
    (is (= :requested (:state (verdict (claim "data-web-3"))))))
  (testing "an old capacity alone is not a rejection while the request is the new size"
    (is (not= :rejected (:state (verdict (claim "data-web-3" :reported "10Gi")))))))

(deftest a-kept-claim-whose-resize-was-rejected-is-not-done
  (testing "the request was not updated"
    (let [{:keys [state line]} (verdict (claim "data-web-3" :requested "10Gi"))]
      (is (= :rejected state))
      (is (= "kept claim data-web-3: requests 10Gi, not 20Gi - the resize was not accepted" line))))
  (testing "a resize error condition"
    (let [{:keys [state line]} (verdict (claim "data-web-3" :conditions [{:type "ControllerResizeError" :status "True"
                                                                          :message "resize volume failed: quota"}]))]
      (is (= :rejected state))
      (is (str/includes? line "resize rejected - ControllerResizeError: resize volume failed: quota"))))
  (testing "an infeasible allocation status"
    (is (= :rejected (:state (verdict (claim "data-web-3" :allocated {:storage "ControllerResizeInfeasible"}))))))
  (testing "a resize failure event since the change"
    (let [{:keys [state line]} (verdict (claim "data-web-3" :conditions pending)
                                        [(event "data-web-3" "VolumeResizeFailed" "2026-11-26T10:00:30Z")])]
      (is (= :rejected state))
      (is (str/includes? line "VolumeResizeFailed: VolumeResizeFailed for data-web-3 rpc error: quota exceeded") line)))
  (testing "the claim no longer exists"
    (is (= :rejected (:state (verdict nil))))))

(deftest only-this-resizes-failure-events-reject-a-kept-claim
  (is (= :accepted (:state (verdict (claim "data-web-3" :conditions pending)
                                    [(event "data-web-3" "VolumeResizeFailed" "2026-11-26T09:30:00Z")])))
      "an event older than the change belongs to an earlier resize")
  (is (= :accepted (:state (verdict (claim "data-web-3" :conditions pending)
                                    [(event "data-web-4" "VolumeResizeFailed" "2026-11-26T10:00:30Z")
                                     (event "data-web-3" "VolumeResizeFailed" "2026-11-26T10:00:30Z" :type "Normal")
                                     (event "data-web-3" "Resizing" "2026-11-26T10:00:30Z")]))))
  (is (= :accepted (:state (verdict (claim "data-web-3" :conditions pending)
                                    [(dissoc (event "data-web-3" "VolumeResizeFailed" nil) :lastTimestamp)])))
      "an event without a timestamp is not judged"))

;; --- the checks against a recorded cluster -----------------------------------------

(def ^:private statefulset
  {:metadata {:namespace "db" :name "web" :uid "uid-web"}
   :spec {:replicas 2
          :volumeClaimTemplates [{:metadata {:name "data"} :spec {:resources {:requests {:storage "20Gi"}}}}]}
   :status {:readyReplicas 2}})

(def ^:private plan [{:pvc "data-web-0"} {:pvc "data-web-1"} {:pvc "data-web-2"} {:pvc "data-web-3"}])

(def ^:private retained #{"data-web-2" "data-web-3"})

(defn- answer
  "A cluster holding statefulset, the claims by name and the events."
  [claims events]
  (fn [& argv]
    (let [argv (vec (filter string? argv))
          json (fn [x] {:exit 0 :out (json/generate-string x) :err ""})]
      (cond
        (some #{"events"} argv) (json {:items events})
        (some #{"statefulsets"} argv) (json statefulset)
        (some #{"persistentvolumeclaims"} argv)
        (if-let [c (get claims (last (remove #(str/starts-with? % "-") argv)))]
          (json c)
          {:exit 1 :out "" :err "Error from server (NotFound): persistentvolumeclaims not found"})
        :else {:exit 1 :out "" :err "the test cluster does not answer this"}))))

(defn- verified
  "Runs the checks through verify/all against the recorded cluster, with
   every wait cut to at most one attempt past what eventually decides at
   once. Returns the verdict and the printed lines."
  [claims events]
  (let [out (java.io.StringWriter.)
        checks (storage/statefulset-checks "ctx" "cluster" "db" "web" "data" "20Gi" plan retained)
        result (binding [*out* out *err* out]
                 (with-redefs [shell/execute (answer claims events)]
                   (verify/all (mapv #(dissoc % :watch) checks))))]
    {:result result :printed (str out) :checks checks}))

(def ^:private running-grown
  {"data-web-0" (claim "data-web-0" :reported "20Gi")
   "data-web-1" (claim "data-web-1" :reported "20Gi")})

(deftest the-running-claims-are-checked-by-capacity-and-the-kept-ones-apart
  (let [checks (storage/statefulset-checks "ctx" "cluster" "db" "web" "data" "20Gi" plan retained)]
    (is (= ["all 2 PVCs report capacity 20Gi"
            "the 2 kept claims of replicas that do not run (data-web-2, data-web-3) accepted the resize to 20Gi"
            "StatefulSet db/web exists again with the new template"
            "every pod of the StatefulSet is Running"]
           (mapv :description checks)))
    (is (= [[300 15 true] [60 5 nil]] (mapv (juxt :timeout :interval :soft?) (take 2 checks)))
        "a rejected kept claim fails the run instead of passing as best effort")
    (is (= [] (verify/fail-closed-findings checks))))
  (testing "without retained claims every claim must report capacity, as before"
    (is (= "all 4 PVCs report capacity 20Gi"
           (:description (first (storage/statefulset-checks "ctx" "cluster" "db" "web" "data" "20Gi" plan)))))))

(deftest running-claims-confirm-at-once-while-kept-claims-wait-for-a-mount
  (let [{:keys [result printed]} (verified (assoc running-grown
                                                  "data-web-2" (claim "data-web-2" :conditions pending)
                                                  "data-web-3" (claim "data-web-3"))
                                           [])]
    (is (true? result) printed)
    (is (str/includes? printed "verified: all 2 PVCs report capacity 20Gi") printed)
    (is (str/includes? printed "kept claim data-web-2: resize accepted, the filesystem grows when a pod mounts it") printed)
    (is (str/includes? printed "kept claim data-web-3: requests 20Gi") printed)
    (is (str/includes? printed "verified: the 2 kept claims") printed)
    (is (not (str/includes? printed "waiting for")) "nothing waits for a capacity a kept claim cannot report yet")
    (is (not (str/includes? printed "best effort")) printed)))

(deftest a-rejected-kept-claim-fails-at-once
  (let [{:keys [result printed]} (verified (assoc running-grown
                                                  "data-web-2" (claim "data-web-2" :conditions pending)
                                                  "data-web-3" (claim "data-web-3" :requested "10Gi"))
                                           [])]
    (is (false? result) printed)
    (is (str/includes? printed "verified: all 2 PVCs report capacity 20Gi") printed)
    (is (str/includes? printed "NOT verified: the 2 kept claims") printed)
    (is (str/includes? printed "kept claim data-web-3: requests 10Gi, not 20Gi - the resize was not accepted") printed)
    (is (not (str/includes? printed "data-web-2: resize accepted")) "only the rejected claim is named in the failure")
    (is (not (str/includes? printed "waiting for: the 2 kept claims")) "a rejection ends the wait at once")))

(deftest a-kept-claim-a-resizer-failed-fails-at-once
  (let [{:keys [result printed]} (verified (assoc running-grown
                                                  "data-web-2" (claim "data-web-2" :conditions pending)
                                                  "data-web-3" (claim "data-web-3" :conditions pending))
                                           ;; after the instant the checks are built at, as the resizer's answer to the patch is
                                           [(event "data-web-3" "VolumeResizeFailed" (str (.plusSeconds (java.time.Instant/now) 60)))])]
    (is (false? result) printed)
    (is (str/includes? printed "kept claim data-web-3: resize rejected - VolumeResizeFailed") printed)))

(deftest every-claim-grown-verifies-everything
  (let [{:keys [result printed]} (verified (assoc running-grown
                                                  "data-web-2" (claim "data-web-2" :reported "20Gi")
                                                  "data-web-3" (claim "data-web-3" :reported "20Gi"))
                                           [])]
    (is (true? result) printed)
    (is (str/includes? printed "kept claim data-web-2: grew to 20Gi") printed)
    (is (str/includes? printed "kept claim data-web-3: grew to 20Gi") printed)
    (is (= 4 (count (re-seq #"verified: " printed))) printed)
    (is (not (str/includes? printed "waiting for")) printed)))

(deftest only-kept-claims-skip-the-capacity-check
  (let [checks (storage/statefulset-checks "ctx" "cluster" "db" "web" "data" "20Gi"
                                           [{:pvc "data-web-2"}] #{"data-web-2"})]
    (is (= ["the 1 kept claim of replicas that do not run (data-web-2) accepted the resize to 20Gi"
            "StatefulSet db/web exists again with the new template"
            "every pod of the StatefulSet is Running"]
           (mapv :description checks)))))
