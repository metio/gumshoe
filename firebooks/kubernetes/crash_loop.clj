;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns firebooks.kubernetes.crash-loop
  "Firebook: starts a deployment whose container exits immediately, producing
   a CrashLoopBackOff for the team to find during a drill."
  (:require [clojure.string :as str]
            [gumshoe.detectives.pods :as pods]
            [gumshoe.firebook :as firebook]))

(firebook/book
 {:description "Firebook: starts a crash-looping deployment for the team to find"
  :drill "crash-loop"
  :fire "a crash-looping deployment"
  :manifests [(firebook/deployment-manifest
               {:name "crash-loop"
                :image "docker.io/library/busybox:stable"
                :command ["sh" "-c" "echo 'I am the fire 🔥' && exit 1"]})]
  :prerequisites {:kubectl-can-create ["deployments"]}
  :expect {:detectives pods/detectives
           :fire? (fn [{:keys [summary]} _]
                    (or (str/includes? summary "CrashLoopBackOff")
                        (str/includes? summary "keeps restarting")))
           :timeout 180
           :interval 10}})
