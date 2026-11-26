;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.synapse
  "Evidence collection for a Matrix homeserver (Synapse). Probes the endpoints
   that decide whether federation and clients work: .well-known delegation on
   the server_name domain, the federation API and signing keys on the delegated
   host, the client API, health, and - when an admin token is given - the
   federation destinations Synapse is failing to reach."
  (:require [gumshoe.concurrent :as concurrent]
            [gumshoe.http :as http]
            [gumshoe.stdout :as stdout]))

(defn collect-evidence!
  "domain is the Matrix server_name; host is the delegated homeserver host.
   admin-token is optional and unlocks the federation destinations check."
  [{:keys [domain host admin-token]}]
  (stdout/print-section "🔍 Evidence (Matrix HTTP API)")
  (stdout/item :info (format "server_name %s, homeserver %s" domain host))
  (let [auth (when admin-token {"Authorization" (str "Bearer " admin-token)})
        [wk-server wk-client fed-version server-keys client-versions health destinations]
        (concurrent/all
         [#(http/fetch (str "https://" domain "/.well-known/matrix/server"))
          #(http/fetch (str "https://" domain "/.well-known/matrix/client"))
          #(http/fetch (str "https://" host "/_matrix/federation/v1/version"))
          #(http/fetch (str "https://" host "/_matrix/key/v2/server"))
          #(http/fetch (str "https://" host "/_matrix/client/versions"))
          #(http/fetch (str "https://" host "/health"))
          #(when admin-token
             (http/fetch (str "https://" host "/_synapse/admin/v1/federation/destinations?limit=1000")
                         auth))])]
    {:now (java.time.Instant/now)
     "domain" domain
     "host" host
     "admin?" (boolean admin-token)
     "wellknown-server" wk-server
     "wellknown-client" wk-client
     "federation-version" fed-version
     "server-keys" server-keys
     "client-versions" client-versions
     "health" health
     "destinations" destinations}))
