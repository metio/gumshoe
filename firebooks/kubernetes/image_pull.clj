;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns firebooks.kubernetes.image-pull
  "Firebook: starts a deployment referencing an image that does not exist,
   producing an ImagePullBackOff for the team to find during a drill."
  (:require [infra.firebook :as firebook]
            [infra.flow :as flow]
            [infra.kubectl :as kubectl]
            [infra.announce :as announce]
            [infra.runbook :as runbook]))

(def options
  {:extinguish {:desc "Put out the fire: delete the fire-drill namespace"
                :alias :e
                :coerce :boolean}})

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-create ["namespaces" "deployments"]
   :kubectl-can-delete ["namespaces"]})

(defn- image-pull
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)]
    (if (:extinguish opts)
      (firebook/extinguish! context)
      (flow/change!
       {:confirmation {:action "start a fire drill: a deployment with an unpullable image"
                       :target cluster
                       :items [(str firebook/drill-namespace "/image-pull")]}
        :announce! #(announce/announce! cluster announcement-data
                                                     "Fire drill started: image-pull")
        :execute! #(firebook/ignite! context
                                     [(firebook/deployment-manifest
                                       {:name "image-pull"
                                        :image "docker.io/library/this-image-does-not-exist:latest"})])
        :post-checks [(firebook/burning-check context "deployments" "image-pull")]}))))

(runbook/execute!
 {:description "Firebook: starts a deployment with an unpullable image for the team to find"
  :options options
  :prerequisites prerequisites
  :action image-pull})
