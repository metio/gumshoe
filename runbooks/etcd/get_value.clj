;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.etcd.get-value
  "Reads a single key from etcd through one etcd pod."
  (:require [clojure.string :as str]
            [gumshoe.etcd :as etcd]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]))

(def options
  (merge etcd/pod-options
         {:key {:desc "The etcd key to read - interactive selection when omitted"
                :alias :k
                :coerce :string}}))

(defn- get-value
  [opts _ctx]
  (let [context (kubectl/current-context)
        target (select/pick etcd/pod-select opts context)]
    (if (nil? target)
      false
      (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
            keys (->> (etcd/etcdctl-stdout context namespace name "get" "--from-key" "" "--keys-only")
                      str/split-lines
                      (remove str/blank?))
            key (interact/choose-one "Key" keys (:key opts))]
        (if (nil? key)
          (do (stdout/error "no key selected") false)
          (do (stdout/print-section-marker)
              (etcd/etcdctl! context namespace name "get" key "--print-value-only")))))))

(runbook/execute!
 {:description "Reads a single key from etcd through one etcd pod"
  :options options
  :prerequisites etcd/pod-prerequisites
  :announce? false
  :action get-value})
