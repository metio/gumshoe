;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.traefik
  "The Traefik tool package: its detectives join the :traffic scan scope, and
   it teaches the setup wizard the :traefik capability - both through one
   plugin/provide!."
  (:require [gumshoe.detectives.traefik :as traefik]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:traffic traefik/detectives}
  :capabilities {:traefik #(kubectl/serves-crd? traefik/ingressroute-type)}})
