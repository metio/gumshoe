;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.etcd
  "Runs etcdctl inside kubeadm etcd pods using the certificates on the node."
  (:require [infra.shell :as shell]))

(def pod-selector "component=etcd")

(def ^:private certificate-flags
  ["--cacert=/etc/kubernetes/pki/etcd/ca.crt"
   "--cert=/etc/kubernetes/pki/etcd/server.crt"
   "--key=/etc/kubernetes/pki/etcd/server.key"])

(defn etcdctl-args
  "Pure assembly of the full kubectl-exec-etcdctl command."
  [context namespace pod arguments]
  (vec (concat ["kubectl" (str "--context=" context) (str "--namespace=" namespace)
                "exec" pod "--" "etcdctl"]
               certificate-flags
               arguments)))

(defn etcdctl!
  "Streams etcdctl output to the terminal; returns true on a clean exit."
  [context namespace pod & arguments]
  (zero? (apply shell/run-with-output (etcdctl-args context namespace pod arguments))))

(defn etcdctl-stdout
  [context namespace pod & arguments]
  (apply shell/stdout-of (etcdctl-args context namespace pod arguments)))
