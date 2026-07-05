;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.etcd.member-list
  "Shows the etcd member list as seen by one etcd pod."
  (:require [gumshoe.etcd :as etcd]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def options
  {:namespace {:desc "The namespace of the etcd pod - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :pod {:desc "The etcd pod to use - interactive selection when omitted"
         :alias :p
         :coerce :string}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]
   :kubectl-can-exec ["pods"]})

(defn- member-list
  [opts _ctx]
  (let [context (kubectl/current-context)
        pods (kubectl/namespaces-names (kubectl/get-selected context "pods" etcd/pod-selector))
        target (interact/choose-namespaced "Pod" pods (:namespace opts) (:pod opts))]
    (if (nil? target)
      (do (stdout/error "no etcd pod selected") false)
      (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
        (stdout/print-section-marker)
        (etcd/etcdctl! context namespace name "member" "list" "--write-out=table")))))

(runbook/execute!
 {:description "Shows the etcd member list as seen by one etcd pod"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action member-list})
