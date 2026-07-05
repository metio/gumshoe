;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.etcd-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.etcd :as etcd]))

(deftest etcdctl-args-test
  (testing "assembles the full kubectl-exec-etcdctl command"
    (is (= ["kubectl" "--context=production" "--namespace=kube-system"
            "exec" "etcd-cp-1" "--" "etcdctl"
            "--cacert=/etc/kubernetes/pki/etcd/ca.crt"
            "--cert=/etc/kubernetes/pki/etcd/server.crt"
            "--key=/etc/kubernetes/pki/etcd/server.key"
            "member" "list"]
           (etcd/etcdctl-args "production" "kube-system" "etcd-cp-1" ["member" "list"])))))
