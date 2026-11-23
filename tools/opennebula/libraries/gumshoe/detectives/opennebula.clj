;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.opennebula
  "Detectives for OpenNebula: hosts in error or offline states, VMs that
   failed, and datastores running out of space. States arrive as numeric
   codes; the maps below name them."
  (:require [gumshoe.opennebula :as opennebula]))

(def host-states
  "OpenNebula host STATE codes -> [name severity]. Healthy and transient
   states map to nil."
  {"0" nil                       ; INIT
   "1" nil                       ; MONITORING_MONITORED
   "2" nil                       ; MONITORED
   "3" ["ERROR" :critical]
   "4" ["DISABLED" :info]
   "5" ["MONITORING_ERROR" :critical]
   "6" nil                       ; MONITORING_INIT
   "7" ["MONITORING_DISABLED" :info]
   "8" ["OFFLINE" :warning]})

(def vm-failure-states
  "VM STATE codes that mean a VM is broken. 7 (FAILED) is retired upstream and
   kept for older frontends."
  {"7" "FAILED"
   "11" "CLONING_FAILURE"})

(defn detect-host-problems
  [evidence]
  (for [host (get evidence "hosts")
        :let [[state-name severity] (get host-states (str (:STATE host)))]
        :when severity]
    {:severity severity
     :component (:NAME host)
     :summary (format "host is %s" state-name)
     :hint (when (= :critical severity)
             "the frontend can not manage this host - VMs on it are unmanaged")}))

(def ^:private active-state "3")

(def lcm-failure-states
  "LCM_STATE codes of an ACTIVE VM -> [name severity]. A failed operation
   leaves the VM in a *_FAILURE state; UNKNOWN means the frontend lost
   contact with it."
  {"16" ["UNKNOWN" :warning]
   "19" ["BOOT_UNKNOWN" :warning]
   "36" ["BOOT_FAILURE" :critical]
   "37" ["BOOT_MIGRATE_FAILURE" :critical]
   "38" ["PROLOG_MIGRATE_FAILURE" :critical]
   "39" ["PROLOG_FAILURE" :critical]
   "40" ["EPILOG_FAILURE" :critical]
   "41" ["EPILOG_STOP_FAILURE" :critical]
   "42" ["EPILOG_UNDEPLOY_FAILURE" :critical]
   "44" ["PROLOG_MIGRATE_POWEROFF_FAILURE" :critical]
   "46" ["PROLOG_MIGRATE_SUSPEND_FAILURE" :critical]
   "47" ["BOOT_UNDEPLOY_FAILURE" :critical]
   "48" ["BOOT_STOPPED_FAILURE" :critical]
   "49" ["PROLOG_RESUME_FAILURE" :critical]
   "50" ["PROLOG_UNDEPLOY_FAILURE" :critical]
   "60" ["PROLOG_MIGRATE_UNKNOWN" :warning]
   "61" ["PROLOG_MIGRATE_UNKNOWN_FAILURE" :critical]})

(defn- vm-problem
  [vm]
  (if-let [failure (get vm-failure-states (str (:STATE vm)))]
    [failure :critical]
    (when (= active-state (str (:STATE vm)))
      (get lcm-failure-states (str (:LCM_STATE vm))))))

(defn detect-vm-problems
  [evidence]
  (for [vm (get evidence "vms")
        :let [[failure severity] (vm-problem vm)]
        :when failure]
    {:severity severity
     :component (format "%s (id %s)" (:NAME vm) (:ID vm))
     :summary (format "VM is %s" failure)
     :hint (if (= :critical severity)
             "recover or delete it - a failed VM holds its resources without running"
             "the frontend lost contact with it - check its host")}))

(defn datastore-usage
  "Percent used of a datastore, or nil when the totals are unreadable."
  [datastore]
  (let [total (parse-long (str (:TOTAL_MB datastore)))
        free (parse-long (str (:FREE_MB datastore)))]
    (when (and total free (pos? total))
      (double (* 100 (/ (- total free) total))))))

(defn detect-datastore-problems
  [evidence]
  (for [datastore (get evidence "datastores")
        :let [usage (datastore-usage datastore)]
        :when (and usage (> usage 75))]
    {:severity (if (> usage 85) :critical :warning)
     :component (:NAME datastore)
     :summary (format "datastore is %.0f%% full" usage)
     :hint "a full datastore blocks new VMs and image operations"}))

(def detectives
  [{:name "opennebula-hosts"
    :description "Hosts in error or offline states"
    :requires ["hosts"]
    :detect detect-host-problems}
   {:name "opennebula-vms"
    :description "VMs that failed or failed to clone"
    :requires ["vms"]
    :detect detect-vm-problems}
   {:name "opennebula-datastores"
    :description "Datastores running out of space"
    :requires ["datastores"]
    :detect detect-datastore-problems}])
