;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.thanos
  "Evidence collection for Thanos Query: readiness, the connected store
   endpoints, and rule evaluation health, all pulled from its HTTP API."
  (:require [infra.http :as http]
            [infra.stdout :as stdout]))

(defn collect-evidence!
  [base-url]
  (stdout/print-section "🔍 Evidence (Thanos HTTP API)")
  (stdout/err-println (format "  %s %s" (stdout/blue "▸") base-url))
  (let [ready (future (http/fetch (str base-url "/-/ready")))
        stores (future (http/fetch (str base-url "/api/v1/stores")))
        rules (future (http/fetch (str base-url "/api/v1/rules")))]
    {:now (java.time.Instant/now)
     "url" base-url
     "ready" @ready
     "stores" @stores
     "rules" @rules}))
