;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.thanos
  "Evidence collection for Thanos Query: readiness, the connected store
   endpoints, and rule evaluation health, all pulled from its HTTP API."
  (:require [gumshoe.http :as http]
            [gumshoe.stdout :as stdout]))

(defn answered
  "The response when its endpoint answered, nil when it did not. An endpoint
   that did not answer was not read, and nil is how the evidence says so: a
   check joining that read is then unread, where a map would let it pass
   having judged nothing."
  [response]
  (when (:reachable response) response))

(defn evidence
  "The evidence map of one collection from the three responses. The query
   layer's own readiness keeps its unreachable response, since the readiness
   check judges exactly that; the stores and rules reads are nil when their
   endpoint did not answer, even when /-/ready did."
  [base-url ready stores rules]
  {:now (java.time.Instant/now)
   "url" base-url
   "ready" ready
   "stores" (answered stores)
   "rules" (answered rules)})

(defn collect-evidence!
  [base-url]
  (stdout/print-section "🔍 Evidence (Thanos HTTP API)")
  (stdout/err-println (format "  %s %s" (stdout/blue "▸") base-url))
  (let [ready (future (http/fetch (str base-url "/-/ready")))
        stores (future (http/fetch (str base-url "/api/v1/stores")))
        rules (future (http/fetch (str base-url "/api/v1/rules")))]
    (doseq [[path response] [["/api/v1/stores" @stores] ["/api/v1/rules" @rules]]
            :when (not (:reachable response))]
      (stdout/warn (format "%s did not answer: %s" path (:error response))))
    (evidence base-url @ready @stores @rules)))
