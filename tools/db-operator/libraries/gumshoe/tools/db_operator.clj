;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.db-operator
  "The db-operator tool package: its detectives join the :databases scan scope
   (alongside any other database operator), and it teaches the setup wizard the
   :db-operator capability - both through one plugin/provide!."
  (:require [gumshoe.detectives.db-operator :as db-operator]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:databases db-operator/detectives}
  :capabilities {:db-operator #(kubectl/resource-exists? "customresourcedefinition" "databases.kinda.rocks")}})
