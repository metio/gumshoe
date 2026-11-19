;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.crossplane
  "Detectives for Crossplane packages (pkg.crossplane.io): Providers,
   Functions and Configurations that do not install, whose package revision
   or runtime is unhealthy, or whose current revision is inactive. A broken
   Provider stops every managed resource it reconciles and a broken Function
   every composition that calls it, so these come before the managed
   resources the generic condition fallback reports one by one."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def provider-type "providers.pkg.crossplane.io")
(def function-type "functions.pkg.crossplane.io")
(def configuration-type "configurations.pkg.crossplane.io")

(def package-types
  [[provider-type "Provider" "every managed resource it reconciles stops syncing"]
   [function-type "Function" "every composition pipeline that calls it fails"]
   [configuration-type "Configuration" "its XRDs and Compositions are not installed or not updated"]])

(def ^:private unpacking-minutes
  "How long unpacking a package may take before the image pull or the
   package's dependencies count as failing."
  10)

(def health-types
  "The health conditions read, in order; the first that is False is reported."
  ["Healthy" "RevisionHealthy" "RuntimeHealthy"])

(defn- conditions
  [object]
  (into {} (map (juxt :type identity)) (-> object :status :conditions)))

(defn- minutes-since
  [value now]
  (when-let [at (try (some-> value java.time.Instant/parse) (catch Exception _ nil))]
    (when now (.toMinutes (java.time.Duration/between at ^java.time.Instant now)))))

(defn- first-line
  [text]
  (some-> text str str/split-lines first str/trim not-empty))

(defn package-findings
  [kind consequence package now]
  (let [by-type (conditions package)
        installed (get by-type "Installed")
        unhealthy (first (keep #(let [c (get by-type %)] (when (= "False" (:status c)) c)) health-types))
        component (kubectl/name-of package)
        image (-> package :spec :package)
        detail (fn [condition] (some-> (first-line (:message condition)) (str " - ")))]
    (remove nil?
            [(when (= "False" (:status installed))
               (case (:reason installed)
                 "InactivePackageRevision"
                 (onset/since
                  {:severity :warning
                   :component component
                   :summary (format "%s's current revision is inactive" kind)
                   :hint (str (detail installed)
                              "with revisionActivationPolicy Manual it waits for its revision to be activated; otherwise a newer revision failed to take over")}
                  (:lastTransitionTime installed))

                 "UnpackingPackage"
                 (when (some-> (minutes-since (:lastTransitionTime installed) now) (>= unpacking-minutes))
                   (onset/since
                    {:severity :critical
                     :component component
                     :summary (format "%s is still unpacking" kind)
                     :hint (str (detail installed) "pulling " (or image "the package") " or resolving its dependencies fails - " consequence)}
                    (:lastTransitionTime installed)))

                 (onset/since
                  {:severity :critical
                   :component component
                   :summary (format "%s is not installed (%s)" kind (or (:reason installed) "no reason"))
                   :hint (str (detail installed) consequence)}
                  (:lastTransitionTime installed))))
             (when unhealthy
               (onset/since
                {:severity :critical
                 :component component
                 :summary (format "%s is not healthy (%s)" kind (:type unhealthy))
                 :hint (str (detail unhealthy)
                            (when (= "RuntimeHealthy" (:type unhealthy)) "its runtime Deployment does not become available - ")
                            consequence)}
                (:lastTransitionTime unhealthy)))])))

(defn detect-packages
  [evidence]
  (let [now (:now evidence)]
    (for [[type kind consequence] package-types
          package (kubectl/items-of (get evidence type))
          finding (package-findings kind consequence package now)]
      finding)))

(def detectives
  [{:name "crossplane-packages"
    :description "Crossplane Providers, Functions and Configurations that do not install or are unhealthy"
    :requires (mapv first package-types)
    :detect detect-packages}])
