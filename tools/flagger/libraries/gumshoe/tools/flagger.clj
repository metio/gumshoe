;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flagger
  "The Flagger tool package: its detectives join the :delivery scan scope
   (beside StageSet), and it teaches the setup wizard the :flagger capability -
   both through one plugin/provide!."
  (:require [gumshoe.detectives.flagger :as flagger]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:delivery flagger/detectives}
  :capabilities {:flagger #(kubectl/serves-crd? flagger/canary-type)}})
