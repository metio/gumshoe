;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.kube-context
  "The kube context a change acts on, resolved once per run and pinned.

   A change spec, a playbook or a dispatcher may declare
     :kube-context (fn [opts] -> a kube context name, or nil)
   which receives the parsed flags, so a book can offer a flag such as
   --context and have it reach everything the change does. nil means the
   current kube context of the kubeconfig.

   `resolve` turns the name into a connection
     {:context <the kube context> :cluster <its cluster> :announce-as <the cluster>}
   from the kubeconfig alone, without contacting any API server: the
   :cluster is the cluster the kubeconfig's entry for that context names,
   which is what env.edn matches environments against, and the name the
   confirmation and the changelog show. A name the kubeconfig does not hold
   is refused, so a mistyped context fails before anything is read.

   A run resolves its connection once and binds it as *pinned*; every
   later resolution in the same run - the prerequisites, the candidate
   query, each step of a playbook, the announcement - takes the pinned one
   instead of reading the kubeconfig again. A kubeconfig the operator
   switches while a run is in flight therefore cannot split the run across
   two clusters. A spec inside a pinned run that names a different kube
   context than the run pinned is refused rather than followed: the pick
   and the reads before it were made on the pinned one.

   The seam is apart from a change's :connect, which names something that
   is not a kube cluster (an SSH host, a storage API); change/normalize
   refuses a spec naming both."
  (:refer-clojure :exclude [resolve])
  (:require [gumshoe.kubectl :as kubectl]))

(def ^:dynamic *pinned*
  "nil, or the connection {:context :cluster :announce-as} a run pinned:
   every resolution while it is bound answers with it (see connection)."
  nil)

(defn- as-connection
  [{:keys [context cluster]}]
  {:context context :cluster cluster :announce-as cluster})

(defn resolve
  "{:connection {:context :cluster :announce-as}} for the kube context
   named `context-name`, nil for the current one, or {:refusal <why>} when
   the kubeconfig holds no such context or the name is not a non-blank
   string. Reads the kubeconfig once and contacts no API server."
  [context-name]
  (let [{:keys [refusal] :as found} (kubectl/kube-connection context-name)]
    (if refusal
      {:refusal refusal}
      {:connection (as-connection found)})))

(defn named
  "The name `kube-context` returns for opts, nil without one. A function
   that throws is not caught: its failure ends the run before anything is
   read."
  [kube-context opts]
  (when kube-context (kube-context opts)))

(defn mismatch
  "Pure: why a spec naming `context-name` must not run inside a run pinned
   to `pinned`, or nil when it may: it names nothing, or the context the
   run pinned."
  [context-name pinned]
  (when (and (some? context-name) (not= context-name (:context pinned)))
    (format "this change names the kube context %s, but the run it is part of is pinned to %s, where its subject was picked - name the same context for the whole run"
            (pr-str context-name) (pr-str (:context pinned)))))

(defn connection
  "{:connection ..} or {:refusal ..} for a spec whose :kube-context is
   `kube-context` (nil when it declares none), given the parsed opts.
   Inside a pinned run it is the pinned connection, refused when the spec
   names another context (see mismatch); outside one it is resolved from
   the kubeconfig (see resolve)."
  [kube-context opts]
  (let [context-name (named kube-context opts)]
    (if-let [pinned *pinned*]
      (if-let [why (mismatch context-name pinned)]
        {:refusal why}
        {:connection pinned})
      (resolve context-name))))

(defn pinning
  "Calls (f connection) with the connection for kube-context and opts (see
   connection) bound as *pinned*, so every resolution inside f takes it.
   Returns (refused why) without calling f when it is refused."
  [kube-context opts refused f]
  (let [{:keys [connection refusal]} (connection kube-context opts)]
    (if refusal
      (refused refusal)
      (binding [*pinned* connection]
        (f connection)))))
