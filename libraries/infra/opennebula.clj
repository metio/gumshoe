;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.opennebula
  "Talking to OpenNebula over SSH: the one* CLI tools run on the frontend with
   its configured credentials, so nothing but ssh is assumed here. Every list
   command is asked for --json and normalized into a flat seq."
  (:require [cheshire.core :as json]
            [infra.ssh :as ssh]
            [infra.stdout :as stdout]))

(defn as-seq
  "OpenNebula returns a single element as a bare object and many as an array;
   normalize both to a seq."
  [value]
  (cond
    (nil? value) []
    (sequential? value) value
    :else [value]))

(defn one-json
  [connection command pool-key element-key]
  (let [output (ssh/stdout-of connection command "list" "--json")
        parsed (try (json/parse-string output true) (catch Exception _ nil))]
    (as-seq (get-in parsed [pool-key element-key]))))

(defn collect-evidence!
  [connection]
  (stdout/print-section "🔍 Evidence (OpenNebula via SSH)")
  (doseq [command ["onehost" "onevm" "onedatastore"]]
    (stdout/err-println (str "  " (stdout/blue "▸") " " command " list --json")))
  (let [hosts (future (one-json connection "onehost" :HOST_POOL :HOST))
        vms (future (one-json connection "onevm" :VM_POOL :VM))
        datastores (future (one-json connection "onedatastore" :DATASTORE_POOL :DATASTORE))]
    {:now (java.time.Instant/now)
     "hosts" @hosts
     "vms" @vms
     "datastores" @datastores}))
