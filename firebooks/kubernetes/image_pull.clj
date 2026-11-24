;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns firebooks.kubernetes.image-pull
  "Firebook: starts a deployment referencing an image that does not exist,
   producing an ImagePullBackOff for the team to find during a drill."
  (:require [gumshoe.detectives.pods :as pods]
            [gumshoe.firebook :as firebook]))

(firebook/book
 {:description "Firebook: starts a deployment with an unpullable image for the team to find"
  :drill "image-pull"
  :fire "a deployment with an unpullable image"
  :manifests [(firebook/deployment-manifest
               {:name "image-pull"
                :image "docker.io/library/this-image-does-not-exist:latest"})]
  :prerequisites {:kubectl-can-create ["deployments"]}
  :expect {:detectives pods/detectives
           :fire? (fn [{:keys [summary]} _]
                    (boolean (re-find #"ImagePullBackOff|ErrImagePull" summary)))
           :timeout 180
           :interval 10}})
