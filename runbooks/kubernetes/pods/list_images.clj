;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.pods.list-images
  "Lists every container image in use, with how many containers run it."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]})

(defn- list-images
  [_opts _ctx]
  (let [{:keys [doc unread]} (kubectl/read-all (kubectl/current-context) "pods")]
    (if unread
      (do (stdout/error (str "could not read the pods of the cluster - " unread)) false)
      (do (doseq [[image count] (sort-by (comp - second) (kubectl/image-counts doc))]
            (println (format "%6d %s" count image)))
          true))))

(runbook/execute!
 {:description "Lists every container image in use, with how many containers run it"
  :prerequisites prerequisites
  :announce? false
  :action list-images})
