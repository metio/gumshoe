;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.etcd
  "Runs etcdctl inside kubeadm etcd pods using the certificates on the node,
   and the pick of the etcd pod every etcd book starts with."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.select :as select]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

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

(defn etcdctl-lines
  "The non-blank output lines of etcdctl with arguments as {:lines [..]}, or
   {:unread <why>} when the exec did not exit cleanly, so a failed read is
   never mistaken for an empty answer."
  [context namespace pod & arguments]
  (let [{:keys [exit out err]} (apply shell/execute (etcdctl-args context namespace pod arguments))]
    (if (zero? exit)
      {:lines (into [] (remove str/blank?) (str/split-lines (str out)))}
      {:unread (let [err (str/trim (str err))]
                 (if (str/blank? err) (str "etcdctl exited with " exit) err))})))

(defn key-candidates
  "The etcd keys to choose from, as etcdctl-lines returns them: only the key
   the operator named when they named one, which must exist, else every key.
   A named key is read on its own, so a large keyspace is listed only when
   the operator has to pick from it."
  [context namespace pod provided]
  (if (str/blank? provided)
    (etcdctl-lines context namespace pod "get" "--from-key" "" "--keys-only")
    (etcdctl-lines context namespace pod "get" provided "--keys-only")))

(def pod-prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]
   :kubectl-can-exec ["pods"]})

(defn pod-candidates
  "The etcd pods as namespace/name."
  [context]
  (kubectl/namespaces-names (kubectl/get-selected context "pods" pod-selector)))

(def pod-select
  "The pick of one etcd pod through pod-options. A book cannot run etcdctl
   without one, so finding none fails the run."
  {:mode :namespaced
   :label "etcd pod"
   :namespace-flag :namespace
   :name-flag :pod
   :candidates pod-candidates
   :empty-message (format "no pod matches %s - is this a kubeadm cluster with stacked etcd?" pod-selector)
   :on-empty :fail})

(def pod-options
  "The flags that name the etcd pod."
  (select/options pod-select
                  {:namespace {:desc "The namespace of the etcd pod - interactive selection when omitted"}
                   :pod {:desc "The etcd pod to use - interactive selection when omitted"}}))

(defn etcdctl-action
  "A runbook action that picks an etcd pod and streams etcdctl with arguments
   through it."
  [& arguments]
  (fn [opts _ctx]
    (let [context (kubectl/current-context)]
      (if-let [target (select/pick pod-select opts context)]
        (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
          (stdout/print-section-marker)
          (apply etcdctl! context namespace name arguments))
        false))))
