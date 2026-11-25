;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.pods.list-image-pull-back-off
  "Lists every pod that cannot pull its container image."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]})

(defn- list-image-pull-back-off
  [_opts _ctx]
  (let [{:keys [doc unread]} (kubectl/read-all (kubectl/current-context) "pods")]
    (if unread
      (do (stdout/error (str "could not read the pods of the cluster - " unread)) false)
      (let [pods (kubectl/pods-with-image-pull-back-off doc)]
        (if (empty? pods)
          (stdout/ok "no pod is in ImagePullBackOff")
          (doseq [pod pods]
            (println pod)))
        true))))

(runbook/execute!
 {:description "Lists every pod that cannot pull its container image"
  :prerequisites prerequisites
  :announce? false
  :action list-image-pull-back-off})
