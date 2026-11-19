;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.crossplane
  "The Crossplane tool package: its detectives join the :platform scan scope,
   and it teaches the setup wizard the :crossplane capability - both through
   one plugin/provide!."
  (:require [gumshoe.detectives.crossplane :as crossplane]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:platform crossplane/detectives}
  :capabilities {:crossplane #(kubectl/serves-crd? crossplane/provider-type)}})
