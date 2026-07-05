;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.matrix
  "Investigates a Matrix homeserver: the client and federation APIs, signing
   key expiry, .well-known delegation, and - with an admin token - which
   servers federation is failing to reach."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.matrix :as matrix-detectives]
            [gumshoe.inputs :as inputs]
            [gumshoe.runbook :as runbook]
            [gumshoe.secrets :as secrets]
            [gumshoe.synapse :as synapse]))

(def options
  ;; No hard-coded defaults: the server_name and homeserver host come from the
  ;; registry - the flag when given, else env.edn for the active environment,
  ;; else the registry default. That is how a book stays free of one operator's
  ;; hostnames while still working out of the box.
  (merge {:domain {:desc "The Matrix server_name (where .well-known lives) - overrides env.edn"
                   :alias :d
                   :coerce :string}
          :host {:desc "The delegated homeserver host - overrides env.edn"
                 :alias :m
                 :coerce :string}
          :admin-secret {:desc "gopass path to a Synapse admin token - unlocks the federation destinations check"
                         :alias :k
                         :coerce :string}}
         detective/output-option))

(def prerequisites
  {:can-ping-using-ipv4 [:host]})

(defn- investigate
  [opts _ctx]
  (detective/when-to-run! "Reach for this when federation or logins break - the client and federation APIs, signing-key expiry, and .well-known delegation.")
  (let [admin-token (when-let [secret (:admin-secret opts)]
                      (secrets/password secret))]
    (detective/report!
     matrix-detectives/detectives
     (detective/run-detectives matrix-detectives/detectives
                               (synapse/collect-evidence! {:domain (inputs/value :matrix-domain opts)
                                                           :host (inputs/value :matrix-host opts)
                                                           :admin-token admin-token}))
     (:output opts "text"))))

(runbook/execute!
 {:description "Investigates a Matrix homeserver: client/federation APIs, signing keys, delegation, destinations"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action investigate})
