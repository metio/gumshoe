;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.jaas
  "The JaaS tool package. JaaS evaluates Jsonnet in the cluster: a JsonnetSnippet
   renders against the JsonnetLibraries it enumerates and publishes the result as
   a Flux ExternalArtifact that anything downstream consumes. This plugin fills
   the :gitops scan scope with snippets that are not Ready and with dangling
   library references, teaches the setup wizard the :jaas capability, and makes
   the JaaS CRDs drill-down subjects - all through one plugin/provide!.

   The edges are the point. A snippet walks to the libraries it imports, to the
   Flux source its Jsonnet comes from, and to the artifact it publishes; from
   there the flux package's back-pointer edge walks on to whoever consumes that
   artifact. A stalled delivery and the snippet that failed to render for it sit
   on one chain you can follow object by object."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]
            [gumshoe.subject :as subject]))

(def jsonnetsnippet-type "jsonnetsnippets.jaas.metio.wtf")
(def jsonnetlibrary-type "jsonnetlibraries.jaas.metio.wtf")

;; A Ready=False reason says how loud the finding should be. Evaluation and
;; identity failures are critical - the snippet rendered before and stopped, so
;; every consumer is now pinned to a stale artifact. Authoring and reference
;; errors warn: they block a snippet that never published in the first place, and
;; the fix is an edit rather than an intervention. States that only mean "the
;; operator has not got there yet" are info.
(def reason-severity
  {"EvaluationFailed" :critical
   "EvaluationTimeout" :critical
   "ArtifactTooLarge" :critical
   "DependencyCycle" :critical
   "RBACDenied" :critical
   "ServiceAccountMissing" :critical
   "InvalidSpec" :warning
   "LibraryNotFound" :warning
   "ExternalVariableConflict" :warning
   "CrossNamespaceRefRejected" :warning
   "SourceRefNotYetSupported" :warning
   "SourceFetchFailed" :warning
   "SourceNotReady" :warning
   "Pending" :info
   "Suspended" :info})

(defn- ready-condition
  [resource]
  (first (filter #(= "Ready" (:type %)) (-> resource :status :conditions))))

(defn ready-reason
  "The Ready condition's reason on a live snippet - what the suspend and resume
   books post-check against, so the two books share the read instead of one
   reaching into the other."
  [context namespace name]
  (:reason (ready-condition
            (kubectl/get-namespaced-resource context namespace jsonnetsnippet-type name))))

(defn- snippet-subject
  [snippet]
  (subject/subject "JsonnetSnippet" (kubectl/namespace-of snippet) (kubectl/name-of snippet)))

(defn declared-libraries
  "Every library a snippet enumerates, resolved to where it would be looked up: a
   ref without a namespace resolves in the snippet's own. The import path defaults
   to the library's name, which is the alias the Jsonnet source imports it by."
  [snippet]
  (let [namespace (kubectl/namespace-of snippet)]
    (for [ref (-> snippet :spec :libraries)
          :when (:name ref)]
      {:import-path (or (:importPath ref) (:name ref))
       :namespace (or (:namespace ref) namespace)
       :name (:name ref)})))

(defn detect-snippet-problems
  [evidence]
  (for [snippet (kubectl/items-of (get evidence jsonnetsnippet-type))
        :let [ready (ready-condition snippet)]
        :when (= "False" (:status ready))]
    {:severity (get reason-severity (:reason ready) :warning)
     :component (kubectl/namespace-name-of snippet)
     ;; Naming the subject explicitly lets the operator drill from the finding
     ;; straight into the snippet, instead of the scan ending at a line of text.
     :subject (snippet-subject snippet)
     :summary (format "JsonnetSnippet is not Ready (%s)" (or (:reason ready) "unknown"))
     ;; The operator appends "(runbook: <url>)" to the condition message, so the
     ;; hint already carries a direct route to the remediation page.
     :hint (:message ready)}))

(defn- library-key
  [namespace name]
  (str namespace "/" name))

(defn detect-missing-libraries
  "A snippet enumerating a JsonnetLibrary that is not in the cluster. The operator
   reports LibraryNotFound once it next evaluates, but holding the two lists side
   by side says it now - and names which ref is dangling and under which import
   alias, which the condition message does not."
  [evidence]
  (let [present (set (map #(library-key (kubectl/namespace-of %) (kubectl/name-of %))
                          (kubectl/items-of (get evidence jsonnetlibrary-type))))]
    (for [snippet (kubectl/items-of (get evidence jsonnetsnippet-type))
          library (declared-libraries snippet)
          :when (not (contains? present (library-key (:namespace library) (:name library))))]
      {:severity :warning
       :component (kubectl/namespace-name-of snippet)
       :subject (snippet-subject snippet)
       :summary (format "JsonnetLibrary %s is missing"
                        (library-key (:namespace library) (:name library)))
       :hint (format "imported as '%s'; evaluation fails with LibraryNotFound"
                     (:import-path library))})))

(def detectives
  [{:name "jsonnetsnippets"
    :description "JsonnetSnippets that fail to evaluate, publish, or are suspended"
    :requires [jsonnetsnippet-type]
    :detect detect-snippet-problems}
   {:name "jsonnetlibrary-refs"
    :description "JsonnetSnippets importing a JsonnetLibrary that is not present"
    ;; Both types are required: with no library list to compare against, every
    ;; ref would read as dangling.
    :requires [jsonnetsnippet-type jsonnetlibrary-type]
    :detect detect-missing-libraries}])

(defn snippet-edges
  [snippet]
  (let [namespace (kubectl/namespace-of snippet)
        name (kubectl/name-of snippet)
        source (-> snippet :spec :sourceRef)]
    (vec
     (concat
      (for [library (declared-libraries snippet)]
        {:relation "imports"
         :subject (subject/subject "JsonnetLibrary" (:namespace library) (:name library))})
      (when (and (:kind source) (:name source))
        [{:relation "renders"
          :subject (subject/subject (:kind source) (or (:namespace source) namespace) (:name source))}])
      ;; The published ExternalArtifact carries the snippet's own name in the
      ;; snippet's own namespace - part of JaaS's wire contract with downstream
      ;; consumers - so the artifact is reachable without a lookup.
      [{:relation "publishes"
        :subject (subject/subject "ExternalArtifact" namespace name)}]))))

(defn library-consumer-edges
  "The snippets that import this library. A snippet enumerates what it imports,
   so the forward direction reads off the object; nothing on the library points
   back, and the answer only exists in the whole snippet list - which is why this
   asks the cluster. It answers the question worth asking before editing a shared
   library: who breaks if I change this?

   Snippets from every namespace are considered, because a library reference may
   cross namespaces when the operator allows it, and the alias each snippet
   imports under rides in the relation - the same library is often `g` in one
   snippet and `grafonnet` in another."
  [context {:keys [namespace name]} _library]
  (->> (kubectl/items-of (kubectl/get-all context jsonnetsnippet-type))
       (sort-by (juxt kubectl/namespace-of kubectl/name-of))
       (mapcat (fn [snippet]
                 (for [library (declared-libraries snippet)
                       :when (and (= namespace (:namespace library))
                                  (= name (:name library)))]
                   {:relation (format "imported as '%s' by" (:import-path library))
                    :subject (subject/subject "JsonnetSnippet"
                                              (kubectl/namespace-of snippet)
                                              (kubectl/name-of snippet))})))
       (distinct)))

(defn snippet-facts
  [snippet]
  (let [spec (:spec snippet)
        status (:status snippet)
        source (:sourceRef spec)]
    [["entry file" (:entryFile spec)]
     ["source" (if (:name source)
                 (format "%s/%s" (:kind source) (:name source))
                 (when (seq (:files spec)) (format "%d inline file(s)" (count (:files spec)))))]
     ["libraries" (some-> (seq (:libraries spec)) count)]
     ["suspended" (when (:suspend spec) "yes")]
     ["revision" (:revision status)]
     ["artifact" (:artifactURL status)]
     ["last sync" (:lastSyncTime status)]]))

(defn library-facts
  [library]
  (let [spec (:spec library)]
    [["files" (some-> (seq (:files spec)) count)]
     ["source" (some-> spec :sourceRef :name)]
     ["revision" (-> library :status :revision)]]))

(plugin/provide!
 {;; Rendering is the step of the gitops pipeline that produces a source, so
  ;; JaaS joins the gitops scan rather than inventing a scope of its own.
  :detectives {:gitops detectives}

  ;; A cluster runs JaaS when it serves the snippet CRD.
  :capabilities {:jaas #(kubectl/serves-crd? jsonnetsnippet-type)}

  :kinds {"JsonnetSnippet" {:type jsonnetsnippet-type :edges snippet-edges}
          "JsonnetLibrary" {:type jsonnetlibrary-type}}

  ;; The import graph walks both ways: a snippet's own spec names what it
  ;; imports, and asking the cluster turns that around into who imports this.
  :fetched-edges {"JsonnetLibrary" library-consumer-edges}

  ;; Neither kind has a phase or replica count, so the generic panel would show
  ;; little beyond the creation timestamp. What an operator wants at a glance is
  ;; what it renders from, what it published, and when.
  :facts {"JsonnetSnippet" snippet-facts
          "JsonnetLibrary" library-facts}})
