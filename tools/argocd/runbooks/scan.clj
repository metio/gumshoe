;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.argocd
  "Investigates Argo CD: Applications and ApplicationSets."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.argocd :as argocd]
            [gumshoe.tools.argocd]))

(detective/book
 {:description "Investigates Argo CD: failing Applications, syncs and ApplicationSets"
  :when-to-run "Reach for this when an Argo CD Application will not sync or stays unhealthy - error conditions, failed or stuck syncs, auto-sync that does not converge, broken ApplicationSets."
  :detectives argocd/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [argocd/application-type]}})
