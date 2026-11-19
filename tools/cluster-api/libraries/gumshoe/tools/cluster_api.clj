;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.cluster-api
  "The Cluster API tool package: its detectives join the :platform scan scope,
   and it teaches the setup wizard the :cluster-api capability - both through
   one plugin/provide!."
  (:require [gumshoe.detectives.cluster-api :as cluster-api]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:platform cluster-api/detectives}
  :capabilities {:cluster-api #(kubectl/serves-crd? cluster-api/cluster-type)}})
