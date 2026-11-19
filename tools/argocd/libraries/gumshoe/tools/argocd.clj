;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.argocd
  "The Argo CD tool package: its detectives join the :gitops scan scope
   (alongside flux), and it teaches the setup wizard the :argocd capability -
   both through one plugin/provide!."
  (:require [gumshoe.detectives.argocd :as argocd]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:gitops argocd/detectives}
  :capabilities {:argocd #(kubectl/serves-crd? argocd/application-type)}})
