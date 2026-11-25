;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.etcd.endpoint-status
  "Shows the status of the etcd endpoint of one etcd pod."
  (:require [gumshoe.etcd :as etcd]
            [gumshoe.runbook :as runbook]))

(runbook/execute!
 {:description "Shows the status of the etcd endpoint of one etcd pod"
  :options etcd/pod-options
  :prerequisites etcd/pod-prerequisites
  :announce? false
  :action (etcd/etcdctl-action "endpoint" "status" "--write-out=table")})
