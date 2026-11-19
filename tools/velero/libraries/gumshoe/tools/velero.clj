;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.velero
  "The Velero tool package: its detectives fill the :backups scan scope (and so
   the cluster-wide scan), and it teaches the setup wizard the :velero
   capability - both through one plugin/provide!."
  (:require [gumshoe.detectives.velero :as velero]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:backups velero/detectives}
  :capabilities {:velero #(kubectl/serves-crd? velero/backup-type)}})
