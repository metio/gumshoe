;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.namespaces
  "Detectives for namespace lifecycle: a namespace that cannot finish
   terminating. The namespace controller records why in the namespace's own
   conditions, which is the whole diagnosis - enumerating every API type in the
   namespace costs one request per type and says less."
  (:require [gumshoe.kubectl :as kubectl]))

(def ^:private blocking
  "The conditions that mean a deletion is refused rather than in flight. Graded
   by how little can be done about them from outside: a refusal names the
   refuser, a finalizer may still return. `NamespaceContentRemaining` alone is
   normal while a deletion runs, so it is not here."
  {"NamespaceDeletionContentFailure"
   [:critical "something refuses to be deleted"]
   "NamespaceDeletionDiscoveryFailure"
   [:critical "an API cannot be reached, so the controller cannot enumerate content"]
   "NamespaceDeletionGroupVersionParsingFailure"
   [:critical "a CRD version cannot be parsed"]
   "NamespaceFinalizersRemaining"
   [:warning "a finalizer has not returned"]})

(defn detect-stuck-namespaces
  [evidence]
  (for [namespace (kubectl/items-of (get evidence "namespaces"))
        :when (= "Terminating" (-> namespace :status :phase))
        condition (-> namespace :status :conditions)
        :let [[severity why] (get blocking (:type condition))]
        :when (and severity (= "True" (:status condition)))]
    {:severity severity
     :component (kubectl/name-of namespace)
     :summary (format "namespace cannot finish terminating: %s" why)
     :hint (format "%s%s"
                   (or (:message condition) (:type condition))
                   (if-let [since (-> namespace :metadata :deletionTimestamp)]
                     (str " -- deleting since " since)
                     ""))}))

(def detectives
  [{:name "terminating-namespaces"
    :description "Namespaces that cannot finish terminating, and what refuses"
    :requires ["namespaces"]
    :detect detect-stuck-namespaces}])
