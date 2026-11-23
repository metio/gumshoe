;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.flux.clear-helm-storage
  "Clears a HelmRelease's Helm storage so helm-controller's next action is an
   install - for a release whose storage holds a revision that blocks every
   upgrade, such as one parked in pending-install or pending-upgrade after its
   timeout.

   It suspends the Kustomization or ResourceSet that renders the HelmRelease
   before anything is deleted, then the HelmRelease, and refuses while a Helm
   action is running. It deletes the release's workloads before its volumes,
   then the storage, confirms the storage is empty, resumes the HelmRelease and
   waits for the fresh install, and resumes the owner last. The volumes are
   deleted with their data, so their step asks again. Every wait derives from
   the HelmRelease's spec.timeout and spec.interval and the owner's reconcile
   interval. `flux/reset` is the lighter remedy when only the failure counters
   are exhausted."
  (:require [gumshoe.playbook :as playbook]
            [gumshoe.tools.flux-storage :as storage]))

(playbook/book
 (merge storage/playbook
        {:description "Clears a HelmRelease's Helm storage so the next action is an install"}))
