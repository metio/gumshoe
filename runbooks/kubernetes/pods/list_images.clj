;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.pods.list-images
  "Lists every container image in use, with how many containers run it.
   Images of init, native sidecar and ephemeral containers are marked with
   their role."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
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
      (do (doseq [{:keys [image count roles]} (kubectl/image-usage doc)
                  :let [other (disj roles :container)]]
            (println (str (format "%6d %s" count image)
                          (when (seq other) (str " (" (str/join ", " (map name other)) ")")))))
          true))))

(runbook/execute!
 {:description "Lists every container image in use, with how many containers run it"
  :prerequisites prerequisites
  :announce? false
  :action list-images})
