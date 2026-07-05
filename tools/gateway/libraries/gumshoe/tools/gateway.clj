;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.gateway
  "The Gateway API tool package: its detectives fill the :traffic scan scope (and
   so the cluster-wide scan), and it teaches the setup wizard to recognise a
   Gateway API cluster - both through one plugin/provide!."
  (:require [gumshoe.detectives.gateway :as gateway]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:traffic gateway/detectives}
  :capabilities {:gateway-api #(kubectl/serves-crd? "gateways.gateway.networking.k8s.io")}})
