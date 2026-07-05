;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.discovery
  "Public SRE-config discovery. A domain can publish the values that are safe to
   know publicly - its DNS server, its Matrix homeserver, its zones - at
   /.well-known/sre.json, so pointing the setup wizard at a domain pre-fills
   env.edn instead of asking for everything cold. The document mirrors the
   env.edn structure, so a discovered value slots straight in at the same path:

     GET https://example.org/.well-known/sre.json
     {\"dns\":   {\"server\": \"dns.example.org\"},
      \"matrix\":{\"domain\": \"example.org\", \"homeserver\": \"https://synapse.example.org\"},
      \"loki\":  {\"namespace\": \"loki\"}}

   Only publish what is public. Secrets, room IDs, and per-operator identity are
   never discovered - they stay in the operator's own env.edn."
  (:require [clojure.string :as str]
            [infra.http :as http]))

(defn url
  "The well-known SRE-config URL for a domain. A value that already looks like a
   URL is passed through, so a non-standard location can be given directly."
  [domain]
  (let [domain (str/trim (str domain))]
    (cond
      (str/blank? domain) nil
      (str/starts-with? domain "http") domain
      :else (format "https://%s/.well-known/sre.json" domain))))

(defn fetch
  "The SRE config a domain publishes, as an env.edn-shaped map, or nil when it
   publishes none or is unreachable. Never throws - discovery is a convenience,
   never a reason the wizard fails."
  [domain]
  (when-let [u (url domain)]
    (let [response (http/fetch u)]
      (when (and (http/ok? response) (map? (:json response)))
        (:json response)))))

(defn value
  "A discovered value at an env.edn path (the same path an input declares), or
   nil when the domain did not publish it."
  [discovered path]
  (get-in discovered path))
