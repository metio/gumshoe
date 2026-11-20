;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.traefik
  "Investigates Traefik: routes and middlewares whose references do not resolve."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.traefik :as traefik]
            [gumshoe.tools.traefik]))

(detective/book
 {:description "Investigates Traefik: IngressRoutes, Middlewares and TraefikServices whose references do not resolve"
  :when-to-run "Reach for this when a Traefik route returns 404 or a middleware does not apply - Traefik accepts references to missing Services, ports and Middlewares and reports them only in its log and dashboard."
  :detectives traefik/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [traefik/ingressroute-type]}})
