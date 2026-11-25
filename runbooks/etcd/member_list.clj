;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.etcd.member-list
  "Shows the etcd member list as seen by one etcd pod."
  (:require [gumshoe.etcd :as etcd]
            [gumshoe.runbook :as runbook]))

(runbook/execute!
 {:description "Shows the etcd member list as seen by one etcd pod"
  :options etcd/pod-options
  :prerequisites etcd/pod-prerequisites
  :announce? false
  :action (etcd/etcdctl-action "member" "list" "--write-out=table")})
