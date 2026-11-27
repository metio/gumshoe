;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.gateway.rename-listeners
  "Renames a ListenerSet's live listeners to the section names its routes
   attach to, so routes that report NoMatchingParent attach again. Only the
   names are patched; a rename it cannot decide is refused."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.listener-renames :as steps]))

(mutation/book (merge steps/rename-picked {:description "Renames a ListenerSet's listeners to the section names its routes attach to"}))
