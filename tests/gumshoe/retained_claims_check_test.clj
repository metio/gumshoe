;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.retained-claims-check-test
  "The claims a scaled-down StatefulSet keeps are mounted by no pod. Our own
   patch sets only their request, so a kept claim is done once the cluster
   shows it took the resize up, rejected at once only on evidence it did not
   carry right before the effect, and otherwise waited for until the
   timeout fails the run. statefulset-checks confirms the claims of the
   running replicas by their capacity, as before."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.shell :as shell]
            [gumshoe.storage :as storage]
            [gumshoe.verify :as verify]))

(defn- claim
  [name & {:keys [requested reported conditions statuses allocated] :or {requested "20Gi" reported "10Gi"}}]
  {:metadata {:namespace "db" :name name :uid (str "uid-" name)}
   :spec {:resources {:requests {:storage requested}}}
   :status (cond-> {:capacity {:storage reported}}
             conditions (assoc :conditions conditions)
             statuses (assoc :allocatedResourceStatuses statuses)
             allocated (assoc :allocatedResources {:storage allocated}))})

(defn- condition
  [type at & {:keys [status message] :or {status "True"}}]
  (cond-> {:type type :status status :lastTransitionTime at}
    message (assoc :message message)))

(def ^:private fs-pending-old (condition "FileSystemResizePending" "2026-11-01T00:00:00Z"))
(def ^:private fs-pending-new (condition "FileSystemResizePending" "2026-11-26T10:00:05Z"))
(def ^:private error-old (condition "ControllerResizeError" "2026-11-01T00:00:00Z" :message "quota exceeded (resize to 15Gi)"))
(def ^:private error-new (condition "ControllerResizeError" "2026-11-26T10:00:05Z" :message "quota exceeded\n (resize to 20Gi)"))

(defn- event
  [claim-name reason uid & {:keys [type count] :or {type "Warning" count 1}}]
  {:metadata {:uid uid}
   :type type :reason reason :count count
   :message (str reason " for " claim-name "\n  rpc error: quota exceeded")
   :lastTimestamp "2026-11-26T10:00:05Z"
   :involvedObject {:kind "PersistentVolumeClaim" :name claim-name}})

(def ^:private old-event (event "data-web-3" "VolumeResizeFailed" "e-old"))

(defn- baseline
  "What kept-claims-baseline reads right before the effect: the claim before
   it, at 10Gi, and the events then."
  [before & [events]]
  {:claims {"data-web-3" before}
   :events (set (map #'storage/event-key events))})

(def ^:private untouched (claim "data-web-3" :requested "10Gi"))

(def ^:private states
  "Every state a kept claim resized to 20Gi can be read in, the verdict and
   what the line names. Each row is [label claim-now events baseline state
   line-fragment]."
  [["requested only: our patch, nothing from the cluster"
    (claim "data-web-3") [] (baseline untouched) :pending "requests 20Gi, the cluster has not taken the resize up (capacity 10Gi)"]
   ["capacity reached"
    (claim "data-web-3" :reported "20Gi") [] (baseline untouched) :done "grew to 20Gi"]
   ["capacity beyond the request, as a provider rounding up reports"
    (claim "data-web-3" :reported "21Gi") [] (baseline untouched) :done "grew to 21Gi"]
   ["capacity reached without a baseline"
    (claim "data-web-3" :reported "20Gi") [] nil :done "grew to 20Gi"]
   ["1024Gi read back as 1Ti"
    (claim "data-web-3" :requested "1Ti" :reported "1Ti") [] (baseline untouched) :done "grew to 1Ti"]
   ["FileSystemResizePending written after the effect"
    (claim "data-web-3" :conditions [fs-pending-new]) [] (baseline untouched) :done "FileSystemResizePending"]
   ["FileSystemResizePending carried over from an earlier resize"
    (claim "data-web-3" :conditions [fs-pending-old]) [] (baseline (claim "data-web-3" :requested "10Gi" :conditions [fs-pending-old]))
    :pending "the cluster has not taken the resize up"]
   ["FileSystemResizePending without a baseline"
    (claim "data-web-3" :conditions [fs-pending-new]) [] nil :pending "the state before the change was not recorded"]
   ["FileSystemResizePending set to False"
    (claim "data-web-3" :conditions [(condition "FileSystemResizePending" "2026-11-26T10:00:05Z" :status "False")]) [] (baseline untouched)
    :pending "has not taken the resize up"]
   ["controller done, node expansion pending"
    (claim "data-web-3" :allocated "20Gi" :statuses {:storage "NodeResizePending"}) [] (baseline untouched)
    :done "the volume grew to 20Gi, its filesystem grows when a pod mounts it (NodeResizePending)"]
   ["node expansion in progress"
    (claim "data-web-3" :allocated "20Gi" :statuses {:storage "NodeResizeInProgress"}) [] (baseline untouched) :done "NodeResizeInProgress"]
   ["node expansion pending for a smaller size"
    (claim "data-web-3" :allocated "15Gi" :statuses {:storage "NodeResizePending"}) [] (baseline untouched) :pending "NodeResizePending"]
   ["controller resize in progress"
    (claim "data-web-3" :allocated "20Gi" :statuses {:storage "ControllerResizeInProgress"}) [] (baseline untouched)
    :pending "(capacity 10Gi, ControllerResizeInProgress)"]
   ["ControllerResizeInfeasible written after the effect"
    (claim "data-web-3" :statuses {:storage "ControllerResizeInfeasible"}) [] (baseline untouched)
    :rejected "resize rejected - ControllerResizeInfeasible"]
   ["NodeResizeInfeasible written after the effect"
    (claim "data-web-3" :statuses {:storage "NodeResizeInfeasible"}) [] (baseline untouched)
    :rejected "resize rejected - NodeResizeInfeasible"]
   ["ControllerResizeInfeasible carried over"
    (claim "data-web-3" :statuses {:storage "ControllerResizeInfeasible"}) []
    (baseline (claim "data-web-3" :requested "10Gi" :statuses {:storage "ControllerResizeInfeasible"}))
    :pending "ControllerResizeInfeasible from before the change"]
   ["ControllerResizeInfeasible without a baseline"
    (claim "data-web-3" :statuses {:storage "ControllerResizeInfeasible"}) [] nil :pending "ControllerResizeInfeasible of unknown age"]
   ["ControllerResizeError written after the effect"
    (claim "data-web-3" :conditions [error-new]) [] (baseline untouched)
    :rejected "resize rejected - ControllerResizeError: quota exceeded (resize to 20Gi)"]
   ["ControllerResizeError carried over, unchanged"
    (claim "data-web-3" :conditions [error-old]) [] (baseline (claim "data-web-3" :requested "10Gi" :conditions [error-old]))
    :pending "a ControllerResizeError: quota exceeded (resize to 15Gi) from before the change"]
   ["ControllerResizeError carried over, then rewritten after the effect"
    (claim "data-web-3" :conditions [error-new]) [] (baseline (claim "data-web-3" :requested "10Gi" :conditions [error-old]))
    :rejected "resize rejected - ControllerResizeError"]
   ["ControllerResizeError without a baseline"
    (claim "data-web-3" :conditions [error-new]) [] nil :pending "a ControllerResizeError: quota exceeded (resize to 20Gi) of unknown age"]
   ["ControllerResizeError set to False"
    (claim "data-web-3" :conditions [(condition "ControllerResizeError" "2026-11-26T10:00:05Z" :status "False")]) [] (baseline untouched)
    :pending "has not taken the resize up"]
   ["NodeResizeError written after the effect"
    (claim "data-web-3" :conditions [(condition "NodeResizeError" "2026-11-26T10:00:05Z")]) [] (baseline untouched)
    :rejected "resize rejected - NodeResizeError"]
   ["a stale error does not hide a capacity reached"
    (claim "data-web-3" :reported "20Gi" :conditions [error-old]) [] (baseline (claim "data-web-3" :requested "10Gi" :conditions [error-old]))
    :done "grew to 20Gi"]
   ["a failure event after the effect"
    (claim "data-web-3") [old-event (event "data-web-3" "VolumeResizeFailed" "e-new")] (baseline untouched [old-event])
    :rejected "resize rejected - VolumeResizeFailed: VolumeResizeFailed for data-web-3 rpc error: quota exceeded"]
   ["a failure event repeated after the effect"
    (claim "data-web-3") [(assoc old-event :count 2)] (baseline untouched [old-event]) :rejected "VolumeResizeFailed"]
   ["a failure event from before the effect"
    (claim "data-web-3") [old-event] (baseline untouched [old-event]) :pending "has not taken the resize up"]
   ["a failure event without a baseline"
    (claim "data-web-3") [old-event] nil :pending "has not taken the resize up"]
   ["events that are not this claim's resize failure"
    (claim "data-web-3") [(event "data-web-4" "VolumeResizeFailed" "e-1")
                          (event "data-web-3" "VolumeResizeFailed" "e-2" :type "Normal")
                          (event "data-web-3" "Resizing" "e-3")]
    (baseline untouched) :pending "has not taken the resize up"]
   ["the request is below the new size"
    (claim "data-web-3" :requested "10Gi") [] (baseline untouched) :rejected "requests 10Gi, not 20Gi - the resize was not accepted"]
   ["the request does not parse"
    (claim "data-web-3" :requested "lots") [] (baseline untouched) :rejected "requests lots, not 20Gi"]
   ["the request beyond the new size, capacity reached"
    (claim "data-web-3" :requested "30Gi" :reported "30Gi") [] (baseline untouched) :done "grew to 30Gi"]
   ["the claim is missing"
    nil [] (baseline untouched) :rejected "not found, so it was not resized"]
   ["the claim read unanswered"
    :unread [] (baseline untouched) :pending "could not be read"]
   ["the events read unanswered"
    (claim "data-web-3" :reported "20Gi") :unread (baseline untouched) :pending "the namespace's events could not be read"]
   ["the events read unanswered does not hide a rejection on the claim"
    (claim "data-web-3" :conditions [error-new]) :unread (baseline untouched) :rejected "ControllerResizeError"]])

(deftest every-state-of-a-kept-claim-has-one-verdict
  (doseq [[label claim-now events before state fragment] states]
    (testing label
      (let [verdict (storage/kept-claim-verdict {:name "data-web-3" :capacity "20Gi"
                                                 :claim claim-now :events events :baseline before})]
        (is (= state (:state verdict)) (:line verdict))
        (is (= "data-web-3" (:claim verdict)))
        (is (str/starts-with? (:line verdict) "kept claim data-web-3: ") (:line verdict))
        (is (str/includes? (:line verdict) fragment) (:line verdict))))))

(deftest a-request-our-own-patch-set-is-never-done
  (doseq [before [nil (baseline untouched)]
          capacity ["20Gi" "1024Gi"]]
    (is (= :pending (:state (storage/kept-claim-verdict
                             {:name "data-web-3" :capacity capacity :events [] :baseline before
                              :claim (claim "data-web-3" :requested (if (= "1024Gi" capacity) "1Ti" capacity))})))
        (str capacity " " (some? before)))))

(deftest the-baseline-records-the-claims-and-the-events-before-the-effect
  (let [answer (fn [& argv]
                 (let [argv (vec (filter string? argv))]
                   (cond
                     (some #{"events"} argv) {:exit 0 :out (json/generate-string {:items [old-event]}) :err ""}
                     (some #{"data-web-3"} argv) {:exit 0 :out (json/generate-string untouched) :err ""}
                     :else {:exit 1 :out "" :err "Error from server (NotFound): persistentvolumeclaims \"data-web-9\" not found"})))
        recorded (with-redefs [shell/execute answer]
                   (storage/kept-claims-baseline "ctx" "db" ["data-web-3" "data-web-9"]))]
    (is (= {"data-web-3" untouched "data-web-9" nil} (:claims recorded)))
    (is (= #{(#'storage/event-key old-event)} (:events recorded)))
    (is (= "the kept claim data-web-9 no longer exists" (storage/kept-claims-refusal recorded)))
    (is (nil? (storage/kept-claims-refusal (baseline untouched))))
    (is (= "the kept claims a, b no longer exist" (storage/kept-claims-refusal {:claims {"b" nil "a" nil "c" untouched}})))))

;; --- the checks against a recorded cluster -----------------------------------------

(def ^:private statefulset
  {:metadata {:namespace "db" :name "web" :uid "uid-web"}
   :spec {:replicas 2
          :volumeClaimTemplates [{:metadata {:name "data"} :spec {:resources {:requests {:storage "20Gi"}}}}]}
   :status {:readyReplicas 2}})

(def ^:private plan [{:pvc "data-web-0"} {:pvc "data-web-1"} {:pvc "data-web-2"} {:pvc "data-web-3"}])

(def ^:private retained #{"data-web-2" "data-web-3"})

(defn- answer
  "A cluster holding statefulset, the claims by name and the events; nil
   events do not answer."
  [claims events]
  (fn [& argv]
    (let [argv (vec (filter string? argv))
          json (fn [x] {:exit 0 :out (json/generate-string x) :err ""})]
      (cond
        (some #{"events"} argv) (if events (json {:items events}) {:exit 1 :out "" :err "Unable to connect to the server"})
        (some #{"statefulsets"} argv) (json statefulset)
        (some #{"persistentvolumeclaims"} argv)
        (if-let [c (get claims (last (remove #(str/starts-with? % "-") argv)))]
          (json c)
          {:exit 1 :out "" :err "Error from server (NotFound): persistentvolumeclaims not found"})
        :else {:exit 1 :out "" :err "the test cluster does not answer this"}))))

(def ^:private kept-before
  {:claims {"data-web-2" (claim "data-web-2" :requested "10Gi")
            "data-web-3" (claim "data-web-3" :requested "10Gi" :conditions [error-old])}
   :events #{}})

(defn- verified
  "Runs the checks through verify/all against the recorded cluster, each
   attempted once. Returns the verdict and the printed lines."
  [claims events & [before]]
  (let [out (java.io.StringWriter.)
        checks (storage/statefulset-checks "ctx" "cluster" "db" "web" "data" "20Gi" plan retained
                                           (atom (or before kept-before)))
        eventually verify/eventually
        result (binding [*out* out *err* out]
                 (with-redefs [shell/execute (answer claims events)
                               verify/eventually #(eventually (assoc % :timeout 0))]
                   (verify/all (mapv #(dissoc % :watch) checks))))]
    {:result result :printed (str out) :checks checks}))

(def ^:private running-grown
  {"data-web-0" (claim "data-web-0" :reported "20Gi")
   "data-web-1" (claim "data-web-1" :reported "20Gi")})

(deftest the-running-claims-are-checked-by-capacity-and-the-kept-ones-apart
  (let [checks (storage/statefulset-checks "ctx" "cluster" "db" "web" "data" "20Gi" plan retained)]
    (is (= ["all 2 PVCs report capacity 20Gi"
            "the 2 kept claims of replicas that do not run (data-web-2, data-web-3) took up the resize to 20Gi"
            "StatefulSet db/web exists again with the new template"
            "every pod of the StatefulSet is Running"]
           (mapv :description checks)))
    (is (= [[300 15 true] [300 10 nil]] (mapv (juxt :timeout :interval :soft?) (take 2 checks)))
        "a kept claim the cluster never took up fails the run instead of passing as best effort")
    (is (= [] (verify/fail-closed-findings checks))))
  (testing "without retained claims every claim must report capacity, as before"
    (is (= "all 4 PVCs report capacity 20Gi"
           (:description (first (storage/statefulset-checks "ctx" "cluster" "db" "web" "data" "20Gi" plan)))))))

(deftest kept-claims-the-cluster-took-up-verify
  (let [{:keys [result printed]} (verified (assoc running-grown
                                                  "data-web-2" (claim "data-web-2" :conditions [fs-pending-new])
                                                  "data-web-3" (claim "data-web-3" :allocated "20Gi"
                                                                      :statuses {:storage "NodeResizePending"}
                                                                      :conditions [error-old]))
                                           [])]
    (is (true? result) printed)
    (is (str/includes? printed "verified: all 2 PVCs report capacity 20Gi") printed)
    (is (str/includes? printed "kept claim data-web-2: the volume grew, its filesystem grows when a pod mounts it (FileSystemResizePending)") printed)
    (is (str/includes? printed "kept claim data-web-3: the volume grew to 20Gi") printed)
    (is (str/includes? printed "verified: the 2 kept claims") printed)))

(deftest kept-claims-that-only-requested-the-new-size-fail-at-the-timeout
  (let [{:keys [result printed]} (verified (assoc running-grown
                                                  "data-web-2" (claim "data-web-2")
                                                  "data-web-3" (claim "data-web-3" :conditions [error-old]))
                                           [])]
    (is (false? result) printed)
    (is (str/includes? printed "NOT verified within 0s: the 2 kept claims") printed)
    (is (str/includes? printed "last seen: kept claim data-web-2: requests 20Gi, the cluster has not taken the resize up (capacity 10Gi)") printed)
    (is (str/includes? printed "kept claim data-web-3: requests 20Gi, the cluster has not taken the resize up (capacity 10Gi), a ControllerResizeError: quota exceeded (resize to 15Gi) from before the change") printed)
    (is (not (str/includes? printed "resize rejected")) "a stale error is no rejection")))

(deftest a-rejected-kept-claim-fails-at-once
  (let [{:keys [result printed]} (verified (assoc running-grown
                                                  "data-web-2" (claim "data-web-2" :conditions [fs-pending-new])
                                                  "data-web-3" (claim "data-web-3" :requested "10Gi"))
                                           [])]
    (is (false? result) printed)
    (is (str/includes? printed "NOT verified: the 2 kept claims") printed)
    (is (str/includes? printed "kept claim data-web-3: requests 10Gi, not 20Gi - the resize was not accepted") printed)
    (is (not (str/includes? printed "data-web-2: the volume grew")) "only the rejected claim is named in the failure")
    (is (not (str/includes? printed "NOT verified within")) "a rejection ends the wait at once")))

(deftest a-kept-claim-a-resizer-failed-after-the-effect-fails-at-once
  (let [{:keys [result printed]} (verified (assoc running-grown
                                                  "data-web-2" (claim "data-web-2" :conditions [fs-pending-new])
                                                  "data-web-3" (claim "data-web-3"))
                                           [(event "data-web-3" "VolumeResizeFailed" "e-new")])]
    (is (false? result) printed)
    (is (str/includes? printed "kept claim data-web-3: resize rejected - VolumeResizeFailed") printed)))

(deftest a-check-without-a-baseline-trusts-only-capacity
  (let [out (java.io.StringWriter.)
        eventually verify/eventually
        checks (storage/statefulset-checks "ctx" "cluster" "db" "web" "data" "20Gi" plan retained)
        result (binding [*out* out *err* out]
                 (with-redefs [shell/execute (answer (assoc running-grown
                                                            "data-web-2" (claim "data-web-2" :conditions [fs-pending-new])
                                                            "data-web-3" (claim "data-web-3" :reported "20Gi"))
                                                     [])
                               verify/eventually #(eventually (assoc % :timeout 0))]
                   (verify/all (mapv #(dissoc % :watch) checks))))]
    (is (false? result) (str out))
    (is (str/includes? (str out) "kept claim data-web-2: requests 20Gi, the cluster has not taken the resize up (capacity 10Gi), the state before the change was not recorded") (str out))))

(deftest unanswered-reads-are-named-at-the-timeout
  (let [{:keys [result printed]} (verified (assoc running-grown
                                                  "data-web-2" (claim "data-web-2" :reported "20Gi")
                                                  "data-web-3" (claim "data-web-3" :reported "20Gi"))
                                           nil)]
    (is (false? result) printed)
    (is (str/includes? printed "kept claim data-web-2: the namespace's events could not be read") printed)))

(deftest every-claim-grown-verifies-everything
  (let [{:keys [result printed]} (verified (assoc running-grown
                                                  "data-web-2" (claim "data-web-2" :reported "20Gi")
                                                  "data-web-3" (claim "data-web-3" :reported "20Gi"))
                                           [])]
    (is (true? result) printed)
    (is (str/includes? printed "kept claim data-web-2: grew to 20Gi") printed)
    (is (str/includes? printed "kept claim data-web-3: grew to 20Gi") printed)
    (is (= 4 (count (re-seq #"verified: " printed))) printed)))

(deftest only-kept-claims-skip-the-capacity-check
  (let [checks (storage/statefulset-checks "ctx" "cluster" "db" "web" "data" "20Gi"
                                           [{:pvc "data-web-2"}] #{"data-web-2"})]
    (is (= ["the 1 kept claim of replicas that do not run (data-web-2) took up the resize to 20Gi"
            "StatefulSet db/web exists again with the new template"
            "every pod of the StatefulSet is Running"]
           (mapv :description checks)))))

(deftest running-claims-compare-capacities-as-quantities
  (let [check (storage/claims-capacity-check "ctx" "cluster" "db" [{:pvc "data-web-0"}] "1024Gi")]
    (with-redefs [shell/execute (answer {"data-web-0" (claim "data-web-0" :requested "1Ti" :reported "1Ti")} [])]
      (is (true? (verify/attempt (:check check)))))
    (with-redefs [shell/execute (answer {"data-web-0" (claim "data-web-0" :requested "1Ti" :reported "1000Gi")} [])]
      (is (false? (verify/attempt (:check check)))))))
