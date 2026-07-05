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
  "VM STATE codes that mean a VM is broken."
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

(defn detect-vm-problems
  [evidence]
  (for [vm (get evidence "vms")
        :let [failure (get vm-failure-states (str (:STATE vm)))]
        :when failure]
    {:severity :critical
     :component (format "%s (id %s)" (:NAME vm) (:ID vm))
     :summary (format "VM is %s" failure)
     :hint "recover or delete it - a failed VM holds its resources without running"}))

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
