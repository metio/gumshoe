;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.etcd.get-value
  "Reads a single key from etcd through one etcd pod."
  (:require [clojure.string :as str]
            [infra.etcd :as etcd]
            [infra.interact :as interact]
            [infra.kubectl :as kubectl]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]))

(def options
  {:namespace {:desc "The namespace of the etcd pod - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :pod {:desc "The etcd pod to use - interactive selection when omitted"
         :alias :p
         :coerce :string}
   :key {:desc "The etcd key to read - interactive selection when omitted"
         :alias :k
         :coerce :string}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]
   :kubectl-can-exec ["pods"]})

(defn- get-value
  [opts _ctx]
  (let [context (kubectl/current-context)
        pods (kubectl/namespaces-names (kubectl/get-selected context "pods" etcd/pod-selector))
        target (interact/choose-namespaced "Pod" pods (:namespace opts) (:pod opts))]
    (if (nil? target)
      (do (stdout/error "no etcd pod selected") false)
      (let [{:keys [namespace name]} (kubectl/split-namespace-name target)
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
  :prerequisites prerequisites
  :announce? false
  :action get-value})
