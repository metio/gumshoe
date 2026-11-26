;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.opennebula
  "Talking to OpenNebula over SSH: the one* CLI tools run on the frontend with
   its configured credentials, so nothing but ssh is assumed here. Every list
   command is asked for --json and normalized into a flat seq."
  (:require [cheshire.core :as json]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]))

(defn as-seq
  "OpenNebula returns a single element as a bare object and many as an array;
   normalize both to a seq."
  [value]
  (cond
    (nil? value) []
    (sequential? value) value
    :else [value]))

(defn pool-elements
  "Pure: the elements of one pool from the result of a `<command> list --json`
   run, or nil when the command did not answer - it exited non-zero, printed
   nothing, or printed something other than a document holding pool-key. A
   pool that answered empty is an empty list, so a check reads it; a command
   that failed (oned down, an authentication error, a dropped connection)
   leaves the evidence nil and the check unread instead of passing on a pool
   nobody read."
  [{:keys [exit out]} pool-key element-key]
  (when (= 0 exit)
    (let [parsed (try (json/parse-string (str out) true) (catch Exception _ nil))]
      (when (and (map? parsed) (contains? parsed pool-key))
        (as-seq (get-in parsed [pool-key element-key]))))))

(defn one-json
  [connection command pool-key element-key]
  (pool-elements (apply shell/execute (ssh/ssh-args connection [command "list" "--json"]))
                 pool-key element-key))

(defn collect-evidence!
  [connection]
  (stdout/print-section "🔍 Evidence (OpenNebula via SSH)")
  (doseq [command ["onehost" "onevm" "onedatastore"]]
    (stdout/item :info (str command " list --json")))
  (let [[hosts vms datastores] (concurrent/all [#(one-json connection "onehost" :HOST_POOL :HOST)
                                                #(one-json connection "onevm" :VM_POOL :VM)
                                                #(one-json connection "onedatastore" :DATASTORE_POOL :DATASTORE)])]
    {:now (java.time.Instant/now)
     "hosts" hosts
     "vms" vms
     "datastores" datastores}))
