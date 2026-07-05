;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.synapse
  "Evidence collection for a Matrix homeserver (Synapse). Probes the endpoints
   that decide whether federation and clients work: .well-known delegation on
   the server_name domain, the federation API and signing keys on the delegated
   host, the client API, health, and - when an admin token is given - the
   federation destinations Synapse is failing to reach."
  (:require [infra.http :as http]
            [infra.stdout :as stdout]))

(defn collect-evidence!
  "domain is the Matrix server_name; host is the delegated homeserver host.
   admin-token is optional and unlocks the federation destinations check."
  [{:keys [domain host admin-token]}]
  (stdout/print-section "🔍 Evidence (Matrix HTTP API)")
  (stdout/err-println (format "  %s server_name %s, homeserver %s" (stdout/blue "▸") domain host))
  (let [auth (when admin-token {"Authorization" (str "Bearer " admin-token)})
        wk-server (future (http/fetch (str "https://" domain "/.well-known/matrix/server")))
        wk-client (future (http/fetch (str "https://" domain "/.well-known/matrix/client")))
        fed-version (future (http/fetch (str "https://" host "/_matrix/federation/v1/version")))
        server-keys (future (http/fetch (str "https://" host "/_matrix/key/v2/server")))
        client-versions (future (http/fetch (str "https://" host "/_matrix/client/versions")))
        health (future (http/fetch (str "https://" host "/health")))
        destinations (future (when admin-token
                               (http/fetch (str "https://" host "/_synapse/admin/v1/federation/destinations?limit=1000")
                                           auth)))]
    {:now (java.time.Instant/now)
     "domain" domain
     "host" host
     "admin?" (boolean admin-token)
     "wellknown-server" @wk-server
     "wellknown-client" @wk-client
     "federation-version" @fed-version
     "server-keys" @server-keys
     "client-versions" @client-versions
     "health" @health
     "destinations" @destinations}))
