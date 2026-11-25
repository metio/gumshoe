;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.matrix
  "Detectives for a Matrix homeserver, judging the evidence from gumshoe.synapse:
   the client and federation APIs must answer, the signing keys must not be
   expiring, .well-known delegation must be coherent, and - with an admin
   token - federation to other servers must not be silently failing."
  (:require [gumshoe.http :as http]))

(def ^:private key-warning-days 7)

(defn detect-client-api
  [evidence]
  (let [host (get evidence "host")
        versions (get evidence "client-versions")]
    (cond
      (not (:reachable versions))
      [{:severity :critical
        :component host
        :summary "the Matrix client API is unreachable"
        :hint (:error versions)}]

      (not (http/ok? versions))
      [{:severity :critical
        :component host
        :summary (format "the client API returned HTTP %s" (:status versions))
        :hint "clients can not talk to the homeserver"}]

      :else [])))

(defn detect-health
  [evidence]
  (let [host (get evidence "host")
        health (get evidence "health")]
    ;; /health only exists behind some reverse proxies; a 404 means the route is
    ;; simply not there (a valid setup), so only an actually-unhealthy status
    ;; (e.g. 5xx) is a finding.
    (when (and (:reachable health) (not (http/ok? health)) (not= 404 (:status health)))
      [{:severity :critical
        :component host
        :summary (format "the health endpoint returned HTTP %s" (:status health))
        :hint "the homeserver or its reverse proxy is unhealthy"}])))

(defn detect-federation-api
  [evidence]
  (let [host (get evidence "host")
        version (get evidence "federation-version")]
    (cond
      (not (:reachable version))
      [{:severity :critical
        :component host
        :summary "the federation API is unreachable"
        :hint "no other Matrix server can reach this homeserver - federation is down"}]

      (not (http/ok? version))
      [{:severity :critical
        :component host
        :summary (format "the federation API returned HTTP %s" (:status version))
        :hint "federation is broken - check the delegated port (usually 8448) and the reverse proxy"}]

      :else [])))

(defn detect-signing-keys
  [evidence]
  (let [now (:now evidence)
        host (get evidence "host")
        keys (get evidence "server-keys")
        valid-until (-> keys :json :valid_until_ts)]
    (cond
      (not (:reachable keys))
      [{:severity :critical
        :component host
        :summary "the signing keys endpoint is unreachable"
        :hint "other servers can not verify this homeserver's events"}]

      ;; Reachable but non-2xx (or non-JSON) leaves valid-until nil; without this
      ;; branch that reads as all-green, hiding a keys endpoint other servers
      ;; cannot use to verify this homeserver's events.
      (not (http/ok? keys))
      [{:severity :critical
        :component host
        :summary (format "the signing keys endpoint returned HTTP %s" (:status keys))
        :hint "other servers can not fetch this homeserver's keys - check the reverse proxy for /_matrix/key/"}]

      ;; A 2xx without valid_until_ts is no key document: a proxy or the web
      ;; client answering its index.html for the key path publishes no key.
      (not (number? valid-until))
      [{:severity :critical
        :component host
        :summary "the signing keys endpoint answered without valid_until_ts"
        :hint "the answer is not Synapse's key document - check that the reverse proxy routes /_matrix/key/ to Synapse"}]

      :else
      (let [expiry (java.time.Instant/ofEpochMilli valid-until)
            days (.toDays (java.time.Duration/between now expiry))]
        (cond
          (neg? days)
          [{:severity :critical
            :component host
            :summary "the published signing keys have expired"
            :hint "federated servers reject events signed with expired keys - restart Synapse to republish"}]

          (< days key-warning-days)
          [{:severity :warning
            :component host
            :summary (format "the signing keys are valid for only %d more days" days)
            :hint "Synapse republishes them automatically, but a stuck server will let them lapse"}]

          :else [])))))

(defn detect-wellknown
  [evidence]
  (let [domain (get evidence "domain")
        wk (get evidence "wellknown-server")]
    (cond
      ;; no delegation at all is a valid setup (SRV or A record on :8448)
      (not (:reachable wk)) []
      (not (http/ok? wk)) []

      (nil? (-> wk :json :m.server))
      [{:severity :warning
        :component (str domain "/.well-known/matrix/server")
        :summary ".well-known/matrix/server is served but has no m.server field"
        :hint "delegation is advertised but broken - federating servers can not find the homeserver"}]

      :else [])))

(defn- federation-failures
  [response]
  (for [destination (-> response :json :destinations)
        :let [failure-ts (:failure_ts destination)]
        :when (and (number? failure-ts) (pos? failure-ts))]
    {:severity :warning
     :component (:destination destination)
     :summary "federation to this server is currently failing"
     :hint (format "Synapse is backing off (retry interval %s ms) - the remote may be down or unreachable"
                   (:retry_interval destination))}))

(defn detect-federation-destinations
  [evidence]
  (when (get evidence "admin?")
    (let [response (get evidence "destinations")]
      (if-not (http/ok? response)
        ;; a rejected token or an admin API the proxy does not expose gives no
        ;; destinations, which would read as federation working everywhere
        [{:severity :warning
          :component (get evidence "host")
          :summary (format "could not read the federation destinations (%s)"
                           (if (:reachable response) (str "HTTP " (:status response)) "unreachable"))
          :hint "check the admin token and that /_synapse/admin is reachable through the proxy"}]
        (federation-failures response)))))

(def detectives
  [{:name "matrix-client-api"
    :description "The Matrix client API answers"
    :requires ["client-versions" "host"]
    :detect detect-client-api}
   {:name "matrix-health"
    :description "The homeserver health endpoint is green"
    :requires ["health" "host"]
    :detect detect-health}
   {:name "matrix-federation-api"
    :description "The federation API is reachable"
    :requires ["federation-version" "host"]
    :detect detect-federation-api}
   {:name "matrix-signing-keys"
    :description "Signing keys are published and not expiring"
    :requires ["server-keys" "host"]
    :detect detect-signing-keys}
   {:name "matrix-wellknown"
    :description ".well-known federation delegation is coherent"
    :requires ["wellknown-server" "domain"]
    :detect detect-wellknown}
   {:name "matrix-federation-destinations"
    :description "Federation to other servers is not silently failing (needs admin token)"
    :requires ["destinations" "admin?" "host"]
    ;; without an admin token nothing is read, and "admin?" and "host" alone
    ;; would count as a read
    :joins ["destinations"]
    :detect detect-federation-destinations}])
