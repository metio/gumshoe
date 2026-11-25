;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dsl.library
  "gumshoe's own functions a gumshoe.dsl change may call, registered under
   their var's name. Each is a library function the books already run and
   its tests already cover; registering it here is what admits it to the
   DSL, so this file is the list a reviewer reads to know what a DSL change
   can do beyond the built-in vocabulary."
  (:require [gumshoe.deletion :as deletion]
            [gumshoe.dsl.registry :as registry]
            [gumshoe.workloads :as workloads]))

(def ^:private functions
  {:gumshoe.workloads/parse-pick
   {:fn #'workloads/parse-pick :kind :parse :args ["pick"]
    :doc "\"<Kind> <namespace>/<name>\" of a Deployment, StatefulSet or DaemonSet as a ref with its :kind"}

   :gumshoe.workloads/scalable-picks
   {:fn #'workloads/scalable-picks :kind :candidates :args ["context"]
    :doc "every Deployment and StatefulSet, as \"<Kind> <namespace>/<name>\""}

   :gumshoe.workloads/scale-refusal-given
   {:fn #'workloads/scale-refusal-given :kind :reason :args ["kind" "workload" "hpas" "replicas"]
    :doc "why scaling the workload to replicas is wrong or pointless, over a {:ok? :items} read of its namespace's HorizontalPodAutoscalers"}

   :gumshoe.workloads/scale-items
   {:fn #'workloads/scale-items :kind :items :args ["kind" "workload" "replicas"]
    :doc "the confirmation lines of a scale"}

   :gumshoe.workloads/scaled?
   {:fn #'workloads/scaled? :kind :predicate :args ["workload" "replicas"]
    :doc "whether the workload observed its spec and runs exactly replicas ready pods"}

   :gumshoe.workloads/suspend-items
   {:fn #'workloads/suspend-items :kind :items :args ["cronjob" "wanted"]
    :doc "the confirmation lines of suspending (wanted true) or resuming (false) a CronJob"}

   :gumshoe.deletion/parse-pick
   {:fn #'deletion/parse-pick :kind :parse :args ["pick"]
    :doc "\"<type> <namespace>/<name>\" of a type the delete book deletes, as a ref"}

   :gumshoe.deletion/candidates
   {:fn #'deletion/candidates :kind :candidates :args ["context"]
    :doc "every object of a type the delete book deletes that is not terminating, as \"<type> <namespace>/<name>\""}

   :gumshoe.deletion/dry-run
   {:fn #'deletion/dry-run :kind :read :args ["context" "ref"]
    :doc "a server-side dry run of deleting the object, as {:ok? :message}"}

   :gumshoe.deletion/read-dependents
   {:fn #'deletion/read-dependents :kind :read :args ["context" "ref"]
    :doc "what is deleted or breaks with the object, as confirmation lines"}

   :gumshoe.deletion/deletion-items
   {:fn #'deletion/deletion-items :kind :items :args ["target" "dry-run" "dependents"]
    :doc "the confirmation lines of a delete: the change, the dry run's verdict and the dependents"}})

(doseq [[name entry] functions]
  (registry/register! name entry))
