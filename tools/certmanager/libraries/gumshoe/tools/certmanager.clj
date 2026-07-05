;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.certmanager
  "The cert-manager tool package: its detectives fill the :tls scan scope (and so
   the cluster-wide scan), and it teaches the setup wizard to recognise a
   cert-manager cluster. Requiring this namespace (a casebook via env.edn :plugins,
   or the scan book at its top) registers both through one plugin/provide!."
  (:require [gumshoe.detectives.certificates :as certificates]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:tls certificates/detectives}
  :capabilities {:cert-manager #(kubectl/serves-crd? "certificates.cert-manager.io")}})
