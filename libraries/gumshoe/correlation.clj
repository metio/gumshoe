;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.correlation
  "Ties a finding about a pod to the finding it follows from. A Deployment
   that cannot roll out also has pods that crash, and a NotReady node also has
   pods that do not start; read side by side they look like several problems
   when they are one. A pod finding gets a :parent naming its workload, or
   failing that its node, when that object has a finding at least as severe -
   so folding a child under its parent never hides a worse severity."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.subject :as subject]))

(def ^:private severity-rank {:critical 0 :warning 1 :info 2})

(defn- worst-by-subject
  "The most severe rank reported for each subject."
  [findings]
  (reduce (fn [acc finding]
            (if-let [s (subject/from-finding finding)]
              (update acc s (fnil min 3) (severity-rank (:severity finding) 3))
              acc))
          {}
          findings))

(defn- workload-subject
  [pod]
  (let [[kind name] (str/split (str (kubectl/workload-of pod)) #"/" 2)]
    (when (and name (not= "Pod" kind))
      (subject/subject kind (kubectl/namespace-of pod) name))))

(defn- node-subject
  [pod]
  (some->> pod :spec :nodeName (subject/subject "Node")))

(defn parent-of
  "The subject a pod finding follows from, or nil."
  [finding pods worst]
  (let [s (subject/from-finding finding)
        rank (severity-rank (:severity finding) 3)
        pod (when (= "Pod" (:kind s)) (get pods [(:namespace s) (:name s)]))
        covers? (fn [parent] (when (and parent (<= (get worst parent 4) rank)) parent))]
    (when pod
      (or (covers? (workload-subject pod))
          (covers? (node-subject pod))))))

(defn correlate
  "The findings, each pod finding that follows from another carrying :parent,
   the display name of what it follows from."
  [findings evidence]
  (let [pods (into {} (map (juxt (juxt kubectl/namespace-of kubectl/name-of) identity))
                   (kubectl/items-of (get evidence "pods")))
        worst (worst-by-subject findings)]
    (mapv (fn [finding]
            (if-let [parent (parent-of finding pods worst)]
              (assoc finding :parent (subject/display parent))
              finding))
          findings)))

(defn children
  "The findings folded under a parent, keyed by the parent's display name."
  [findings]
  (group-by :parent (filter :parent findings)))
