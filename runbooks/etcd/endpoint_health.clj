;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.etcd.endpoint-health
  "Checks the health of the etcd endpoint of one etcd pod."
  (:require [gumshoe.etcd :as etcd]
            [gumshoe.runbook :as runbook]))

(runbook/execute!
 {:description "Checks the health of the etcd endpoint of one etcd pod"
  :options etcd/pod-options
  :prerequisites etcd/pod-prerequisites
  :announce? false
  :action (etcd/etcdctl-action "endpoint" "health" "--write-out=table")})
