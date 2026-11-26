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
            provided (:key opts)
            {:keys [lines unread]} (etcd/key-candidates context namespace name provided)]
        (cond
          unread
          (do (stdout/error (format "could not %s - %s"
                                    (if (str/blank? provided) "list the etcd keys" (str "read the etcd key " provided))
                                    unread))
              false)

          (and (not (str/blank? provided)) (empty? lines))
          (do (stdout/error (format "etcd holds no key %s" provided)) false)

          :else
          (if-let [key (interact/choose-one "Key" lines provided)]
            (do (stdout/print-section-marker)
                (etcd/etcdctl! context namespace name "get" key "--print-value-only"))
            (do (stdout/error (if (empty? lines) "etcd answered with no keys" "no key selected")) false)))))))

(runbook/execute!
 {:description "Reads a single key from etcd through one etcd pod"
  :options options
  :prerequisites etcd/pod-prerequisites
  :announce? false
  :action get-value})
