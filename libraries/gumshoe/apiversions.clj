;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.apiversions
  "Evidence for the api-version-drift detective: what the API server's discovery
   advertises for each resource, next to what the CRDs actually serve. The two
   should agree; when they don't, discovery has gone stale (typically after a
   controller upgrade changed a CRD's served versions, and the aggregated
   discovery cache did not refresh). Every client that builds a RESTMapper from
   discovery - kubectl, and Flux's health-poll - then resolves the kind to a
   version the CRD no longer serves and gets 404 NotFound, while server-side
   apply keeps working because it uses the explicit version from the manifest.

   The detect fns stay pure over this evidence."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]))

(defn parse-api-resources
  "Rows of `kubectl api-resources --no-headers` as {[group plural] version} -
   the served version discovery advertises for each resource. Robust to the
   optional SHORTNAMES column: the last three whitespace fields are always
   APIVERSION NAMESPACED KIND and the first is NAME, so anything in between is
   ignored. Core-group rows (APIVERSION without a '/') get group \"\"."
  [text]
  (into {}
        (for [line (str/split-lines (str/trim (str text)))
              :when (not (str/blank? line))
              :let [f (str/split (str/trim line) #"\s+")]
              :when (>= (count f) 4)
              :let [plural (first f)
                    apiversion (nth f (- (count f) 3))
                    [group version] (if (str/includes? apiversion "/")
                                      (str/split apiversion #"/" 2)
                                      ["" apiversion])]]
          [[group plural] version])))

(defn collect-evidence!
  "The CRDs (their real served versions) and the discovery view (the version the
   API server advertises per resource), keyed for the detect fns."
  [context]
  {:now (java.time.Instant/now)
   "crds" (kubectl/get-all context "customresourcedefinitions")
   "advertised" (parse-api-resources
                 (shell/stdout-of "kubectl" (str "--context=" context)
                                  "api-resources" "--no-headers"))})
