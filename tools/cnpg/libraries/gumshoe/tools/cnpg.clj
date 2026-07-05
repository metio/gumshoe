;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.cnpg
  "The CloudNativePG tool package: its detectives join the :databases scan scope
   (alongside any other database operator's), and it teaches the setup wizard the
   :cnpg capability - both through one plugin/provide!."
  (:require [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:databases cnpg/detectives}
  :capabilities {:cnpg #(kubectl/resource-exists? "customresourcedefinition" "clusters.postgresql.cnpg.io")}})
