;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.flux.retire
  "Retires a HelmRelease without a helm uninstall, so the objects its release
   deployed survive because another writer now manages them.

   It suspends the HelmRelease, strips finalizers.fluxcd.io from it and
   deletes it, in that order, and leaves the release's Helm storage for the
   operator to drop once the post-checks have shown every object survived.
   It refuses while an object the release claims has no writer other than
   Helm (a server-side apply by another field manager, or a manager --writer
   names), while a claimed object or the storage cannot be read, while a
   Helm action is running, and while a Kustomization or ResourceSet that is
   not suspended still renders the HelmRelease. `flux/clear_helm_storage` is
   the opposite operation: it keeps the HelmRelease and drops its storage so
   the next action is a fresh install."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.tools.flux-retire :as retire]))

(mutation/book (merge retire/retire-named {:description "Retires a HelmRelease without a helm uninstall, keeping the objects another writer took over"}))
