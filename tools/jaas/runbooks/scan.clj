;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.jaas
  "Investigates JaaS: JsonnetSnippets that fail to render, publish, or import."
  (:require [gumshoe.detective :as detective]
            [gumshoe.tools.jaas :as jaas]))

;; A fixed detective set rather than the :gitops scope: the package's detectives
;; already join the gitops scan and the cluster-wide one, so this book is the
;; narrow view - "is JaaS itself healthy" - without every flux source alongside.
(detective/book
 {:description "Investigates JaaS: snippets that fail to render, publish, or import"
  :when-to-run "Reach for this when rendered manifests went stale - JsonnetSnippets that fail to evaluate, are suspended, or import a library that is not there."
  :detectives jaas/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities [:jaas]
                  :kubectl-can-get [jaas/jsonnetsnippet-type jaas/jsonnetlibrary-type]}})
