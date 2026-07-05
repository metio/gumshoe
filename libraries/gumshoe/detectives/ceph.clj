;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.ceph
  "Detectives for a cephadm-managed ceph cluster. Evidence comes from
   gumshoe.ceph/collect-evidence! over SSH instead of kubectl - the detect
   functions stay pure either way."
  (:require [clojure.string :as str]))

(def ^:private critical-pg-states
  #"down|incomplete|inconsistent|stale|peering|unknown")

(defn detect-health-checks
  [evidence]
  (for [[check data] (-> (get evidence "status") :health :checks)]
    {:severity (if (= "HEALTH_ERR" (:severity data)) :critical :warning)
     :component (name check)
     :summary (or (-> data :summary :message) "health check is failing")}))

(defn detect-osd-problems
  [evidence]
  (let [osdmap (-> (get evidence "status") :osdmap)
        total (:num_osds osdmap)
        up (:num_up_osds osdmap)
        in (:num_in_osds osdmap)]
    (concat
     (when (and total up (< up total))
       [{:severity :critical
         :component "osds"
         :summary (format "%d of %d OSDs are down" (- total up) total)
         :hint "ceph osd tree shows which ones - check their hosts and daemons"}])
     (when (and total in (< in total))
       [{:severity :warning
         :component "osds"
         :summary (format "%d of %d OSDs are out" (- total in) total)
         :hint "out OSDs no longer hold data - rebalancing may be in progress"}]))))

(defn detect-pg-problems
  [evidence]
  (for [pg-state (-> (get evidence "status") :pgmap :pgs_by_state)
        :let [state (:state_name pg-state)]
        :when (not (str/starts-with? (str state) "active+clean"))]
    {:severity (if (re-find critical-pg-states (str state)) :critical :warning)
     :component "pgs"
     :summary (format "%d PGs are %s" (:count pg-state) state)}))

(defn detect-quorum-problems
  [evidence]
  (let [status (get evidence "status")
        quorum (count (:quorum_names status))
        mons (-> status :monmap :num_mons)]
    (when (and mons (pos? mons) (< quorum mons))
      [{:severity :critical
        :component "mons"
        :summary (format "only %d of %d monitors are in quorum" quorum mons)
        :hint "a second monitor loss may freeze the cluster - investigate immediately"}])))

(defn detect-capacity-problems
  [evidence]
  (let [df (get evidence "df")
        ratio (-> df :stats :total_used_raw_ratio)]
    (concat
     (cond
       (and ratio (> ratio 0.85))
       [{:severity :critical
         :component "cluster"
         :summary (format "raw capacity is %.0f%% used" (* 100 (double ratio)))
         :hint "add OSDs before ceph reaches its full ratio and stops all writes"}]

       (and ratio (> ratio 0.75))
       [{:severity :warning
         :component "cluster"
         :summary (format "raw capacity is %.0f%% used" (* 100 (double ratio)))
         :hint "plan capacity now - full clusters stop writing"}]

       :else
       [])
     (for [pool (:pools df)
           :let [used (-> pool :stats :percent_used)]
           :when (and used (> (double used) 0.75))]
       {:severity (if (> (double used) 0.85) :critical :warning)
        :component (:name pool)
        :summary (format "pool is %.0f%% used" (* 100 (double used)))}))))

(defn detect-osd-utilization
  [evidence]
  (for [node (:nodes (get evidence "osd-df"))
        :let [utilization (:utilization node)]
        :when (and utilization (> (double utilization) 80))]
    {:severity (if (> (double utilization) 90) :critical :warning)
     :component (:name node)
     :summary (format "OSD is %.0f%% full" (double utilization))
     :hint "rebalance or add capacity - one full OSD stops its whole pool"}))

(defn detect-service-problems
  [evidence]
  (for [service (get evidence "orch-ls")
        :let [running (-> service :status :running)
              size (-> service :status :size)]
        :when (and running size (< running size))]
    {:severity :critical
     :component (:service_name service)
     :summary (format "%d of %d daemons are running" running size)
     :hint "ceph orch ps shows the failed daemons"}))

(defn detect-new-crashes
  [evidence]
  (let [crashes (get evidence "crash-ls-new")]
    (when (seq crashes)
      [{:severity :warning
        :component "crashes"
        :summary (format "%d unarchived daemon crash(es)" (count crashes))
        :hint "inspect and archive with runbooks/ceph/archive_crashes.clj"}])))

(def detectives
  [{:name "ceph-health"
    :description "Failing ceph health checks"
    :requires ["status"]
    :detect detect-health-checks}
   {:name "ceph-osds"
    :description "OSDs that are down or out"
    :requires ["status"]
    :detect detect-osd-problems}
   {:name "ceph-pgs"
    :description "Placement groups that are not active+clean"
    :requires ["status"]
    :detect detect-pg-problems}
   {:name "ceph-mons"
    :description "Monitors missing from the quorum"
    :requires ["status"]
    :detect detect-quorum-problems}
   {:name "ceph-capacity"
    :description "Cluster and pool capacity running out"
    :requires ["df"]
    :detect detect-capacity-problems}
   {:name "ceph-osd-utilization"
    :description "Individual OSDs running full"
    :requires ["osd-df"]
    :detect detect-osd-utilization}
   {:name "ceph-services"
    :description "cephadm services with missing daemons"
    :requires ["orch-ls"]
    :detect detect-service-problems}
   {:name "ceph-crashes"
    :description "Unarchived daemon crashes"
    :requires ["crash-ls-new"]
    :detect detect-new-crashes}])
