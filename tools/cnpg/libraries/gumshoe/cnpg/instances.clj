;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.instances
  "Identifying a CloudNativePG cluster's Postgres pods.

   A Pooler's own pods carry the same `cnpg.io/cluster` label as the instances,
   so that label alone does not separate them. Both forms here exclude any pod
   carrying `cnpg.io/poolerName`."
  (:require [gumshoe.kubectl :as kubectl]))

(def pooler-name-label :cnpg.io/poolerName)
(def cluster-label :cnpg.io/cluster)
(def pod-role-label :cnpg.io/podRole)

(defn labels-of [pod] (-> pod :metadata :labels))

(defn cluster-of [pod] (get (labels-of pod) cluster-label))

(defn- owned-by-cluster?
  "Instance pods are created by the operator with an ownerReference to the
   Cluster; pooler pods are owned by a ReplicaSet. Checked alongside the label
   so neither alone has to be load-bearing."
  [cluster pod]
  (some #(and (= "Cluster" (:kind %)) (= cluster (:name %)))
        (-> pod :metadata :ownerReferences)))

(defn instance-pods
  "The named cluster's Postgres pods in one namespace."
  [pods namespace cluster]
  (filter #(and (= namespace (kubectl/namespace-of %))
                (not (get (labels-of %) pooler-name-label))
                (or (owned-by-cluster? cluster %)
                    (and (= cluster (get (labels-of %) cluster-label))
                         (= "instance" (get (labels-of %) pod-role-label)))))
          pods))

(defn instance-pod?
  "A Postgres instance pod of any cluster, for sweeps that are not scoped to
   one. The `cnpg.io/cluster` label keeps this to CloudNativePG's own pods,
   since an ownerReference of kind Cluster is not unique to this operator."
  [pod]
  (let [labels (labels-of pod)]
    (boolean (and (get labels cluster-label)
                  (not (get labels pooler-name-label))
                  (or (= "instance" (get labels pod-role-label))
                      (some #(= "Cluster" (:kind %)) (-> pod :metadata :ownerReferences)))))))
