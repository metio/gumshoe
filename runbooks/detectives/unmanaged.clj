;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.unmanaged
  "Audits one namespace for objects nobody manages: every object of every
   namespaced type the API serves, each judged by who owns it."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.unmanaged :as unmanaged-detectives]
            [gumshoe.unmanaged :as unmanaged]))

(detective/book
 {:description "Audits one namespace for objects no controller, Helm, Flux, Argo CD or StageSet manages"
  :when-to-run (str "Reach for this at the end of a migration or before deleting a namespace's GitOps source - "
                    "it reads every namespaced type the API serves, not the dozen kinds of kubectl get all, "
                    "and names each object nobody manages, why every other one passed, and every type it could not read.")
  :options unmanaged/options
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["namespaces"]}
  :detectives unmanaged-detectives/detectives
  :collect unmanaged/collect})
