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

(defn advertised-versions
  "The discovery view: {[group plural] version} the API server advertises per
   resource. Registered as the custom evidence source for the \"api-resources\"
   key, so the standard collector can supply it to a composed scan."
  [context]
  (parse-api-resources
   (shell/stdout-of "kubectl" (str "--context=" context)
                    "api-resources" "--no-headers")))
