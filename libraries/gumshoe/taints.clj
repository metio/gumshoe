;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.taints
  "Which taints of a node an operator can remove. The node lifecycle
   controller owns the taints that mirror a node's conditions and its
   spec.unschedulable: it recomputes them on every update of the node, so the
   update that removes one is itself the event that adds it back. Removing
   such a taint by hand cannot last; the remedy is to change what it
   mirrors."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def controller-owned
  "The keys of the taints the node lifecycle controller sets and adds back,
   each with what removes it for good."
  {"node.kubernetes.io/unschedulable"
   "it mirrors spec.unschedulable - uncordon the node instead (runbooks/kubernetes/nodes/uncordon.clj)"
   "node.kubernetes.io/not-ready"
   "it mirrors the node's Ready condition - fix the kubelet or the node so it reports Ready"
   "node.kubernetes.io/unreachable"
   "it mirrors a node whose kubelet stopped reporting - restore the kubelet or the node's connection to the API server"
   "node.kubernetes.io/memory-pressure"
   "it mirrors the MemoryPressure condition - free memory on the node until the kubelet clears it"
   "node.kubernetes.io/disk-pressure"
   "it mirrors the DiskPressure condition - free disk space on the node until the kubelet clears it"
   "node.kubernetes.io/pid-pressure"
   "it mirrors the PIDPressure condition - reduce the processes on the node until the kubelet clears it"
   "node.kubernetes.io/network-unavailable"
   "it mirrors the NetworkUnavailable condition - fix the node's network plugin or routes until it clears"})

(defn taint-key
  "The key of a taint in its 'key[=value]:effect' display form."
  [taint]
  (-> (str taint) (str/split #":" 2) first (str/split #"=" 2) first))

(defn remedy
  "Why taint, in its display form, cannot be removed by hand, with what
   removes it for good, or nil for a taint an operator may remove."
  [taint]
  (when-let [why (get controller-owned (taint-key taint))]
    (format "taint %s is set by the node lifecycle controller, which adds it back on the very update that removes it: %s"
            taint why)))

(defn removable
  "The taints of node, in display form, that an operator may remove."
  [node]
  (vec (remove remedy (kubectl/taints-of node))))

(defn nodes-with-removable-taints
  "The names of the nodes of nodes-list carrying a taint an operator may
   remove."
  [nodes-list]
  (vec (for [node (kubectl/items-of nodes-list)
             :when (seq (removable node))]
         (kubectl/name-of node))))
