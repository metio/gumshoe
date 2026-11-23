;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux
  "The flux tool package: everything gumshoe knows about Flux, in one place and
   registered through a single plugin/provide!. Requiring this namespace (which a
   casebook does via env.edn :plugins, or a flux book does at its top) contributes
   flux's detectives, its :flux cluster capability, the flux CLI tool profile, and
   the drill-down (its CRD kinds plus a reconcile-status probe). Nothing about
   flux lives in the engine any more."
  (:require [clojure.string]
            [gumshoe.effect :as effect]
            [gumshoe.investigation :as investigation]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]
            [gumshoe.plugin :as plugin]
            [gumshoe.subject :as subject]))

(def helmrelease-type "helmreleases.helm.toolkit.fluxcd.io")
(def kustomization-type "kustomizations.kustomize.toolkit.fluxcd.io")
(def gitrepository-type "gitrepositories.source.toolkit.fluxcd.io")
(def ocirepository-type "ocirepositories.source.toolkit.fluxcd.io")
(def helmchart-type "helmcharts.source.toolkit.fluxcd.io")
(def externalartifact-type "externalartifacts.source.toolkit.fluxcd.io")

(defn- ready-condition
  [resource]
  (first (filter #(= "Ready" (:type %)) (-> resource :status :conditions))))

(def ^:private reconcile-annotations
  ["kustomize.toolkit.fluxcd.io/reconcile"
   "fluxcd.controlplane.io/reconcile"])

(defn- annotation
  [resource key]
  (get-in resource [:metadata :annotations (keyword key)]))

(defn inert?
  "An object told not to reconcile. It carries every label and annotation of a
   managed object and is not managed - a gate left on after a migration reads as
   healthy from every angle except the age of what it serves."
  [resource]
  (some #(= "disabled" (annotation resource %)) reconcile-annotations))

(defn- shorten-release-name
  "helm-controller's ShortenName: a name over 53 characters keeps its first 40
   and gains a dash and the first 12 hex digits of its SHA-256."
  [release-name]
  (if (<= (count release-name) 53)
    release-name
    (let [digest (.digest (java.security.MessageDigest/getInstance "SHA-256")
                          (.getBytes ^String release-name "UTF-8"))
          hex (apply str (map #(format "%02x" (bit-and % 0xff)) digest))]
      (str (subs release-name 0 40) "-" (subs hex 0 12)))))

(defn- last-snapshot
  [release]
  (first (-> release :status :history)))

(defn release-name
  "The Helm release a HelmRelease manages: the name its status recorded, else
   helm-controller's rule - spec.releaseName, else <targetNamespace>-<name>
   when the release targets another namespace, else the object's name -
   shortened past 53 characters."
  [release]
  (or (:name (last-snapshot release))
      (shorten-release-name
       (or (not-empty (-> release :spec :releaseName))
           (when-let [target (not-empty (-> release :spec :targetNamespace))]
             (str target "-" (-> release :metadata :name)))
           (-> release :metadata :name)))))

(defn release-namespace
  "Where the release's objects live: the namespace its status recorded, else
   spec.targetNamespace, else the HelmRelease's own."
  [release]
  (or (:namespace (last-snapshot release))
      (not-empty (-> release :spec :targetNamespace))
      (-> release :metadata :namespace)))

(defn storage-namespace
  "Where Helm keeps the release's records, which `helm get` reads: the status's
   storageNamespace, else the spec's, else the HelmRelease's own namespace."
  [release]
  (or (not-empty (-> release :status :storageNamespace))
      (not-empty (-> release :spec :storageNamespace))
      (-> release :metadata :namespace)))

(defn- retry-on-failure?
  [release phase]
  (= "RetryOnFailure" (get-in release [:spec phase :strategy :name])))

(defn- failures
  [release key]
  (or (get-in release [:status key]) 0))

(defn- retries
  [release phase]
  (or (get-in release [:spec phase :remediation :retries]) 0))

(defn- spent?
  "Whether the failures used up the phase's retries. A negative retries is
   Flux's unlimited, which is never spent."
  [release phase counter]
  (let [budget (retries release phase)]
    (and (not (neg? budget)) (> (failures release counter) budget))))

(defn parked?
  "A failed release that will not try again. Flux defaults both install and
   upgrade to `retries: 0`, and once they are spent the release is left failed
   until its spec or chart changes - so `status.conditions` keeps describing a
   cause that may have cleared long ago. `RetryOnFailure` keeps retrying on its
   own interval, and so does a negative `retries`, Flux's unlimited, so a
   release using either is never parked."
  [release]
  (boolean
   (or (and (not (retry-on-failure? release :install))
            (spent? release :install :installFailures))
       (and (not (retry-on-failure? release :upgrade))
            (spent? release :upgrade :upgradeFailures)))))

(def ^:private settle-minutes
  "How long an object may stay not Ready while it is still reconciling before
   that stops looking like progress."
  10)

(defn- named-condition
  [resource type]
  (first (filter #(= type (:type %)) (-> resource :status :conditions))))

(defn in-progress?
  "Whether the Ready condition describes work under way rather than a result.
   ProgressingWithRetry is a failure being retried, so it does not count; a
   generation the controller has not observed yet means Ready describes an
   older spec."
  [resource]
  (let [ready (ready-condition resource)
        reconciling (named-condition resource "Reconciling")
        generation (-> resource :metadata :generation)
        observed (-> resource :status :observedGeneration)]
    (boolean
     (or (= "Progressing" (:reason ready))
         (and (= "True" (:status reconciling)) (not= "ProgressingWithRetry" (:reason reconciling)))
         (and (number? generation) (number? observed) (< observed generation))))))

(defn- minutes-since
  [now timestamp]
  (when (and now timestamp)
    (.toMinutes (java.time.Duration/between (java.time.Instant/parse timestamp) now))))

(defn- not-ready-finding
  [resource kind now]
  (let [ready (ready-condition resource)
        stalled (named-condition resource "Stalled")
        base (onset/since {:component (kubectl/namespace-name-of resource) :hint (:message ready)}
                          (:lastTransitionTime ready))
        age (minutes-since now (:lastTransitionTime ready))]
    (cond
      (= "True" (:status stalled))
      (assoc base :severity :critical
             :summary (format "%s is stalled (%s)" kind (or (:reason stalled) (:reason ready) "unknown"))
             :hint (or (:message stalled) (:message ready)))

      (= "DependencyNotReady" (:reason ready))
      (assoc base :severity :info
             :summary (format "%s waits for a dependency" kind)
             :hint (str (or (:message ready) "a dependency is not Ready")
                        " - the dependency is reported on its own"))

      (and (in-progress? resource) age (< age settle-minutes))
      nil

      (in-progress? resource)
      (assoc base :severity :warning
             :summary (if age
                        (format "%s has been reconciling for %d minutes" kind age)
                        (format "%s is reconciling" kind)))

      :else
      (assoc base :severity :critical
             :summary (format "%s is not Ready (%s)" kind (or (:reason ready) "unknown"))))))

(defn flux-findings
  ([resources kind] (flux-findings resources kind nil))
  ([resources kind now]
   (concat
    (for [resource resources
          :when (= "False" (:status (ready-condition resource)))
          :let [finding (not-ready-finding resource kind now)]
          :when finding]
      finding)
    (for [resource resources
          :when (true? (-> resource :spec :suspend))]
      {:severity :info
       :component (kubectl/namespace-name-of resource)
       :summary (format "%s is suspended" kind)
       :hint "resume it once the reason for the suspension is resolved"}))))

(defn detect-helmrelease-problems
  [evidence]
  (let [releases (kubectl/items-of (get evidence helmrelease-type))
        stuck (filter parked? releases)]
    (concat
     ;; a parked release is reported here instead, with the remedy
     (flux-findings (remove parked? releases) "HelmRelease" (:now evidence))
     (for [release stuck
           :let [ready (ready-condition release)]]
       {:severity :critical
        :component (kubectl/namespace-name-of release)
        :summary (format "HelmRelease is parked after %s and will not retry"
                         (or (:reason ready) "a failure"))
        :hint (format "%s -- the message describes the last attempt (revision %s), which may no longer be true; verify the cause is still present, then clear the counters with `kubectl annotate helmrelease %s reconcile.fluxcd.io/resetAt=$(date -u +%%FT%%TZ) reconcile.fluxcd.io/requestedAt=$(date -u +%%FT%%TZ)`"
                      (or (:message ready) "no message")
                      (or (-> release :status :lastAttemptedRevision) "unknown")
                      (kubectl/name-of release))}))))

(defn detect-inert-deliveries
  [evidence]
  (for [resource (concat (kubectl/items-of (get evidence helmrelease-type))
                         (kubectl/items-of (get evidence kustomization-type)))
        :when (inert? resource)]
    {:severity :warning
     :component (kubectl/namespace-name-of resource)
     :summary "delivery is annotated to skip reconciliation"
     :hint "it will not change whatever it already applied; remove the reconcile annotation once the migration or gate it belongs to is finished"}))

(defn detect-drifted-releases
  "A release whose live objects no longer match what it applied. `Ready` stays
   True -- the Helm operation did succeed -- so nothing else reports this. With
   `driftDetection.mode: warn` the drift is named and never corrected, and a
   field whose desired value has not changed between releases is never in an
   upgrade's patch either, so it persists indefinitely."
  [evidence]
  (for [release (kubectl/items-of (get evidence helmrelease-type))
        :let [drifted (named-condition release "Drifted")
              mode (-> release :spec :driftDetection :mode)]
        :when (= "True" (:status drifted))]
    {:severity (if (= "enabled" mode) :warning :critical)
     :component (kubectl/namespace-name-of release)
     :summary (format "HelmRelease has drifted from what it applied (mode: %s)"
                      (or mode "warn"))
     :hint (format "%s%s"
                   (or (:message drifted) "no detail")
                   (if (= "enabled" mode)
                     " -- correction is on, so this should clear on the next reconcile"
                     " -- correction is off: nothing will fix this, and an upgrade patches only fields whose desired value changed. Diff it with `flux/drift`, then force a re-apply with reconcile.fluxcd.io/forceAt"))}))

(defn- crash-looping
  "Containers a kubelet keeps restarting. A pod can be Running and still be in
   this state, so a replica count does not show it."
  [pod]
  (for [status (concat (-> pod :status :containerStatuses)
                       (-> pod :status :initContainerStatuses))
        :let [reason (-> status :state :waiting :reason)]
        :when (contains? #{"CrashLoopBackOff" "CreateContainerConfigError" "RunContainerError"} reason)]
    {:container (:name status) :reason reason :restarts (or (:restartCount status) 0)}))

(defn detect-succeeded-over-broken
  "The contradiction worth surfacing: a release whose last Helm operation
   succeeded, over a workload that cannot run. `Ready` on a HelmRelease is a
   statement about Helm, not about the software - so a green delivery and a
   crash-looping pod coexist happily, and every dashboard reads fine."
  [evidence]
  (let [pods (kubectl/items-of (get evidence "pods"))
        broken (group-by kubectl/namespace-of
                         (filter #(seq (crash-looping %)) pods))]
    (for [release (kubectl/items-of (get evidence helmrelease-type))
          :let [ready (ready-condition release)
                ;; the workload runs where the release installed it
                casualties (get broken (release-namespace release))]
          :when (and (= "True" (:status ready)) (seq casualties))]
      {:severity :critical
       :component (kubectl/namespace-name-of release)
       :summary (format "HelmRelease reports success while %d pod(s) in its namespace cannot start"
                        (count casualties))
       :hint (format "%s -- Ready on a HelmRelease means the Helm operation succeeded, nothing more; look at %s"
                     (->> casualties
                          (mapcat crash-looping)
                          (map #(format "%s (%s, %d restarts)" (:container %) (:reason %) (:restarts %)))
                          distinct
                          (clojure.string/join ", "))
                     (clojure.string/join ", " (map kubectl/name-of casualties)))})))

(defn detect-kustomization-problems
  [evidence]
  (flux-findings (kubectl/items-of (get evidence kustomization-type)) "Kustomization" (:now evidence)))

(defn detect-gitrepository-problems
  [evidence]
  (flux-findings (kubectl/items-of (get evidence gitrepository-type)) "GitRepository" (:now evidence)))

(defn detect-ocirepository-problems
  [evidence]
  (flux-findings (kubectl/items-of (get evidence ocirepository-type)) "OCIRepository" (:now evidence)))

(defn detect-helmchart-problems
  [evidence]
  (flux-findings (kubectl/items-of (get evidence helmchart-type)) "HelmChart" (:now evidence)))

(def detectives
  [{:name "helmreleases"
    :description "HelmReleases that fail to reconcile or are suspended"
    :requires [helmrelease-type]
    :detect detect-helmrelease-problems}
   {:name "kustomizations"
    :description "Kustomizations that fail to reconcile or are suspended"
    :requires [kustomization-type]
    :detect detect-kustomization-problems}
   {:name "gitrepositories"
    :description "GitRepositories that fail to sync or are suspended"
    :requires [gitrepository-type]
    :detect detect-gitrepository-problems}
   {:name "ocirepositories"
    :description "OCIRepositories that fail to sync or are suspended"
    :requires [ocirepository-type]
    :detect detect-ocirepository-problems}
   {:name "helmcharts"
    :description "HelmCharts that fail to build or are suspended"
    :requires [helmchart-type]
    :detect detect-helmchart-problems}
   {:name "inert-deliveries"
    :description "HelmReleases and Kustomizations annotated to skip reconciliation"
    :requires [helmrelease-type kustomization-type]
    :detect detect-inert-deliveries}
   {:name "succeeded-over-broken"
    :description "HelmReleases reporting success over pods that cannot start"
    :requires [helmrelease-type "pods"]
    :detect detect-succeeded-over-broken}
   {:name "drifted-releases"
    :description "HelmReleases whose live objects no longer match what they applied"
    :requires [helmrelease-type]
    :detect detect-drifted-releases}])

(defn parked-first
  "Release names with the parked ones ahead of the rest, for a picker whose whole
   purpose is the parked ones."
  [releases]
  (mapv kubectl/namespace-name-of
        (concat (filter parked? releases) (remove parked? releases))))

(defn reset-effect
  "Clears the failure counters and asks for a reconcile. `requestedAt` alone
   triggers the reconcile and leaves the exhausted counters in place, so the
   release fails again immediately; `resetAt` is the one that clears them. One
   timestamp for both, so they agree."
  [context namespace name timestamp]
  (effect/plan
   (effect/note (format "clearing the failure counters of %s/%s" namespace name))
   (effect/kubectl context "-n" namespace "annotate" helmrelease-type name "--overwrite"
                   (str "reconcile.fluxcd.io/resetAt=" timestamp)
                   (str "reconcile.fluxcd.io/requestedAt=" timestamp))))

;; --- Helm storage ------------------------------------------------------------------

(defn pending-release?
  "Whether a Helm release revision, as kubectl/helm-releases reads it from its
   Secret's labels, is parked in a pending state: Helm writes pending-install,
   pending-upgrade or pending-rollback before it acts and replaces it with the
   result after. Read from the status label alone, so the payload is never
   decoded."
  [row]
  (boolean (some-> (:status row) (clojure.string/starts-with? "pending-"))))

(def ^:private duration-units
  {"h" 3600000 "m" 60000 "s" 1000 "ms" 1 "us" 1/1000 "µs" 1/1000 "ns" 1/1000000})

(defn duration-seconds
  "A Go duration such as \"5m0s\" or \"1h30m\" in whole seconds, rounded up so
   a wait is never zero, or nil when it is not one."
  [text]
  (when (and (string? text) (re-matches #"(?:\d+(?:\.\d+)?(?:h|ms|us|µs|ns|m|s))+" text))
    (let [millis (reduce + (for [[_ n unit] (re-seq #"(\d+(?:\.\d+)?)(h|ms|us|µs|ns|m|s)" text)]
                             (* (bigdec n) (duration-units unit))))]
      (long (Math/ceil (/ (double millis) 1000))))))

(def ^:private default-helm-timeout
  "helm-controller's timeout for a Helm action when spec.timeout is unset."
  "5m0s")

(defn helm-timeout-seconds
  "How long one Helm action of this release may take: spec.timeout, else
   helm-controller's default of five minutes."
  [release]
  (duration-seconds (or (not-empty (-> release :spec :timeout)) default-helm-timeout)))

(defn interval-seconds
  "The object's spec.interval in seconds, or nil when it has none."
  [resource]
  (duration-seconds (-> resource :spec :interval)))

(defn action-in-flight?
  "Whether a Helm action on this release may still be running: a pending
   revision whose last change is younger than spec.timeout. An older one is
   parked - helm-controller gave up on it - and is exactly what clearing the
   storage removes. `now` is epoch seconds, as the revision labels are."
  [rows release now]
  (let [name (release-name release)
        namespace (storage-namespace release)
        timeout (helm-timeout-seconds release)]
    (boolean
     (some (fn [row]
             (let [changed (or (:modified-at row) (:created-at row))]
               (and (= name (:release row)) (= namespace (:namespace row))
                    (pending-release? row)
                    (or (nil? changed) (<= (- now changed) timeout)))))
           rows))))

;; --- suspend / resume --------------------------------------------------------------

(def suspendable
  "The Flux kinds the suspend and resume books act on, by kind."
  {"HelmRelease" helmrelease-type
   "Kustomization" kustomization-type
   "GitRepository" gitrepository-type
   "OCIRepository" ocirepository-type})

(defn parse-delivery-pick
  "\"<Kind> <namespace>/<name>\" of a suspendable kind as {:kind :type
   :namespace :name}, or nil."
  [value]
  (when-let [[_ kind namespace name] (re-matches #"\s*(\w+)\s+([^/\s]+)/(\S+)\s*" (str value))]
    (when-let [type (suspendable kind)]
      {:kind kind :type type :namespace namespace :name name})))

(defn delivery-picks
  "Every object of the suspendable kinds whose suspension differs from wanted."
  [lists-by-kind wanted]
  (vec (for [[kind list] (sort-by key lists-by-kind)
             resource (sort-by kubectl/namespace-name-of (kubectl/items-of list))
             :when (not= wanted (true? (-> resource :spec :suspend)))]
         (str kind " " (kubectl/namespace-name-of resource)))))

(defn suspend-refusal
  [resource wanted]
  (cond
    (nil? resource) "the object no longer exists"
    (= wanted (true? (-> resource :spec :suspend))) (if wanted "it is suspended already" "it is not suspended")
    :else nil))

(defn suspend-items
  [kind resource wanted]
  (let [target (format "%s %s" kind (kubectl/namespace-name-of resource))]
    (if wanted
      [(format "suspend %s - Flux stops reconciling it, and what it applied stays as it is" target)
       "changes pushed to its source are not applied until it is resumed"]
      [(format "resume %s and request a reconcile - changes pushed to its source meanwhile are applied now" target)])))

(defn suspend-effect
  "Sets spec.suspend; a resume also asks for a reconcile at timestamp, so the
   object catches up at once instead of at its next interval."
  [context {:keys [type namespace name]} wanted timestamp]
  (effect/plan
   (effect/kubectl context "-n" namespace "patch" type name "--type=merge"
                   (str "--patch={\"spec\":{\"suspend\":" wanted "}}"))
   (when-not wanted
     (effect/kubectl context "-n" namespace "annotate" type name "--overwrite"
                     (str "reconcile.fluxcd.io/requestedAt=" timestamp)))))

(defn externalartifact-edges
  "The RFC-0012 back-pointer: spec.sourceRef names the object that produced this
   artifact - a renderer, a builder, anything that publishes one. The reference is
   {apiVersion, kind, name} with no namespace, because a producer publishes into
   its own namespace, so the edge stays in the artifact's. This is the hop that
   turns 'the manifests are stale' into 'this object failed to render them'."
  [artifact]
  (let [ref (-> artifact :spec :sourceRef)]
    (when (and (:kind ref) (:name ref))
      [{:relation "produced by"
        :subject (subject/subject (:kind ref) (kubectl/namespace-of artifact) (:name ref))}])))

(defn externalartifact-facts
  [artifact]
  (let [published (-> artifact :status :artifact)
        ref (-> artifact :spec :sourceRef)]
    [["revision" (:revision published)]
     ["digest" (:digest published)]
     ["url" (:url published)]
     ["last update" (:lastUpdateTime published)]
     ["produced by" (when (:name ref) (format "%s/%s" (:kind ref) (:name ref)))]]))

;; The flux CLI subcommand for a kind, so a drill-down can ask flux for its status.
(def ^:private flux-get-kind
  {"HelmRelease" ["helmrelease"]
   "Kustomization" ["kustomization"]
   "GitRepository" ["source" "git"]
   "OCIRepository" ["source" "oci"]})

(plugin/provide!
 {;; The gitops scan is flux's - it fills the :gitops scope.
  :detectives {:gitops detectives}

  ;; A cluster runs flux when it serves the flux CRDs.
  :capabilities {:flux #(kubectl/serves-crd? kustomization-type)}

  ;; The flux CLI, so any book that lists it inherits the version check.
  :tools {"flux" {:version-command ["version" "--client"] :min-version "2.0"}}

  ;; The flux CRDs become drill-down subjects (describe/yaml probes work on them);
  ;; edges default to ownerReferences.
  :kinds {"HelmRelease"      {:type helmrelease-type}
          "Kustomization"    {:type kustomization-type}
          "GitRepository"    {:type gitrepository-type}
          "OCIRepository"    {:type ocirepository-type}
          "HelmChart"        {:type helmchart-type}
          "ExternalArtifact" {:type externalartifact-type :edges externalartifact-edges}}

  ;; An ExternalArtifact has no phase or replicas, so the generic panel would
  ;; show only its creation timestamp - what matters is the revision it serves
  ;; and who produced it.
  :facts {"ExternalArtifact" externalartifact-facts}

  ;; A flux-native probe: reconcile status via the flux CLI, offered only where
  ;; flux is installed. The flux2 `get` syntax is stable across its majors, so
  ;; one form serves all; were a future major to change it, wrapping :args in
  ;; command/dispatch-by-version keeps this one package agnostic.
  :probes [{:key :flux-status :label "🔁 flux reconcile status"
            :kinds (set (keys flux-get-kind)) :tools ["flux"]
            :args (fn [context {:keys [kind namespace name]}]
                    (into ["flux" (str "--context=" context) (str "--namespace=" namespace) "get"]
                          (conj (flux-get-kind kind) name)))}]})
