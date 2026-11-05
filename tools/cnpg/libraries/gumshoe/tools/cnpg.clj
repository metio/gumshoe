;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.cnpg
  "The CloudNativePG tool package: its detectives join the :databases scan scope
   (alongside any other database operator's), and it teaches the setup wizard the
   :cnpg capability - both through one plugin/provide!."
  (:require [gumshoe.cnpg.pooler-logs :as pooler-logs]
            [gumshoe.cnpg.volumes :as volumes]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.cnpg-volumes :as cnpg-volumes]
            [gumshoe.detectives.cnpg-retention :as cnpg-retention]
            [gumshoe.detectives.cnpg-storage :as cnpg-storage]
            [gumshoe.detectives.cnpg-recoverability :as cnpg-recoverability]
            [gumshoe.detectives.cnpg-pooler-logs :as cnpg-pooler-logs]
            [gumshoe.detectives.pooler :as pooler]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:databases (concat cnpg/detectives pooler/detectives cnpg-volumes/detectives
                              cnpg-retention/detectives cnpg-storage/detectives
                              cnpg-recoverability/detectives cnpg-pooler-logs/detectives)}
  ;; PVC usage is not in the resource API, so the volume detective brings its
  ;; own collector; it reads kubelet stats for the nodes hosting instance pods.
  :collectors {volumes/evidence-key volumes/collector
               ;; a rejected pooled login leaves no trace in the resource API,
               ;; so the login detective brings the pooler logs with it
               pooler-logs/evidence-key pooler-logs/collector}
  :capabilities {:cnpg #(kubectl/serves-crd? "clusters.postgresql.cnpg.io")}})
