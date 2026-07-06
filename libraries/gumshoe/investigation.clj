;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.investigation
  "The interactive drill-down engine. You focus on one subject, the engine shows
   its state, auto-surfaces the smoking gun for what looks wrong, and offers the
   related objects to pivot to and the live probes to run. Selecting a related
   object pivots focus and pushes a breadcrumb; selecting a probe runs it and
   stays. The loop continues until you end it, and the whole path - plus the
   exact commands, via the reproducer - is yours to keep.

   The point is to save clicks, not add them: for a crash-looping pod it already
   shows the crash logs; for a pending pod, the scheduling events; and the most
   likely next step leads the menu, marked. The pure decisions (which probe a
   situation calls for, how the menu is ordered, whether a hostname matches) live
   in testable helpers; only the fetching, the fzf prompt, and the loop are
   impure."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [gumshoe.command :as command]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.subject :as subject]))

;; ---------------------------------------------------------------------------
;; Situations -> plain words and the probe that answers them (pure).

(def situation-text
  {:crash-looping "container is crash-looping"
   :image-pull-error "image can not be pulled"
   :oom-killed "container was OOM-killed"
   :pending "pod can not be scheduled"
   :failed "pod failed"
   :not-ready "not all containers are ready"
   :disk-pressure "node is under disk pressure"
   :memory-pressure "node is under memory pressure"
   :pid-pressure "node is under PID pressure"
   :cordoned "node is cordoned (unschedulable)"
   :unbound "PersistentVolumeClaim is not bound"
   :released "PersistentVolume is Released - its claim is gone but the data (and the ceph image) remains"
   :volume-failed "PersistentVolume is Failed"
   :none-ready "no replicas are ready"
   :degraded "some replicas are not ready"
   :at-max "HorizontalPodAutoscaler is pegged at its maximum replicas"
   :job-failed "Job has failed"})

(def ^:private situation->probe
  "The probe most likely to explain each situation - surfaced automatically and
   marked as recommended in the menu."
  {:crash-looping :logs-previous
   :image-pull-error :describe
   :oom-killed :describe
   :pending :describe
   :failed :logs
   :not-ready :describe
   :none-ready :describe
   :degraded :describe})

(defn recommended-probe
  [situation]
  (get situation->probe situation))

;; ---------------------------------------------------------------------------
;; Probes: live, foreground kubectl views. Snapshots (--tail, describe, top)
;; print and return to the loop; --follow streams until Ctrl-C (unbounded, by
;; design). Every one runs through the streaming path, so nothing is cut off.

(defn with-context
  "A kubectl argv pinned to the investigation's context and (when given) namespace
   - the building block for a probe's :args. Public so a plugin probe builds its
   command the same way the built-ins do."
  [context namespace & args]
  (into ["kubectl" (str "--context=" context)]
        (concat (when namespace [(str "--namespace=" namespace)]) args)))

(def built-in-probes
  [{:key :logs-previous :label "📜 previous logs (the crash)" :kinds #{"Pod"}
    :args (fn [ctx {:keys [namespace name]}]
            (with-context ctx namespace "logs" name "--previous" "--all-containers" "--tail=200"))}
   {:key :logs :label "📄 recent logs" :kinds #{"Pod"}
    :args (fn [ctx {:keys [namespace name]}]
            (with-context ctx namespace "logs" name "--all-containers" "--tail=200"))}
   {:key :logs-follow :label "📡 follow logs live (Ctrl-C to return)" :kinds #{"Pod"}
    :args (fn [ctx {:keys [namespace name]}]
            (with-context ctx namespace "logs" name "--all-containers" "--tail=50" "--follow"))}
   {:key :describe :label "🔬 describe (events, conditions, details)" :kinds :any
    :args (fn [ctx {:keys [kind namespace name]}]
            (with-context ctx namespace "describe" (subject/kind->type kind) name))}
   {:key :top :label "📊 resource usage (top)" :kinds #{"Pod" "Node"}
    :args (fn [ctx {:keys [kind namespace name]}]
            (if (= "Node" kind)
              (with-context ctx nil "top" "node" name)
              (with-context ctx namespace "top" "pod" name "--containers")))}
   {:key :yaml :label "📃 full YAML" :kinds :any
    :args (fn [ctx {:keys [kind namespace name]}]
            (with-context ctx namespace "get" (subject/kind->type kind) name "--output=yaml"))}])

(defonce ^:private extra-probes (atom []))

(defn register-probe!
  "Registers a drill-down probe: {:key :label :kinds :args :tools}. :kinds is :any
   or a set of subject kinds it applies to; :args is (fn [context subject] -> argv)
   (use `with-context` to build a kubectl command, or return any argv - a probe
   may combine tools). :tools is an optional list of binaries the probe needs; the
   probe is only offered when they are all installed, so a tool package brings its
   probes and the menu never lists an action that can't run. A plugin adds actions
   like a HelmRelease's flux reconcile status or a pod's /healthz."
  [probe]
  (swap! extra-probes conj probe))

(defn probes
  "The built-in probes plus every plugin-registered one."
  []
  (concat built-in-probes @extra-probes))

(defn applicable-probes
  "The probes that make sense for a kind, recommended one first: the kind matches
   and every tool the probe needs is installed."
  [kind recommended]
  (let [applicable (filter #(and (or (= :any (:kinds %)) (contains? (:kinds %) kind))
                                 (every? command/installed? (:tools %)))
                           (probes))]
    (sort-by #(if (= recommended (:key %)) 0 1) applicable)))

;; ---------------------------------------------------------------------------
;; The menu (pure): what you can do from a focus, ordered so the smart move is
;; on top - recommended probe, then the rest, then related objects to pivot to
;; (already unhealthy-first), then navigation.

(def ^:private critical-situations
  "The situations that read as an outage rather than a warning - shown in red so
   the worst related objects jump out of the menu."
  #{:crash-looping :oom-killed :image-pull-error :failed :none-ready :unbound :volume-failed})

(defn- situation-severity
  [situation]
  (cond
    (or (nil? situation) (= :ok situation)) nil
    (contains? critical-situations situation) :critical
    :else :warning))

(defn- situation-color
  [situation text]
  (case (situation-severity situation)
    :critical (stdout/red text)
    :warning (stdout/yellow text)
    text))

(defn- situation-marker
  "A colored icon leading a menu item, so an operator scans for the problem
   instead of reading every line. Blank for a healthy or unknown object."
  [situation]
  (case (situation-severity situation)
    :critical (str (stdout/red "🔴") " ")
    :warning (str (stdout/yellow "🟡") " ")
    ""))

(defn- situation-badge
  [situation]
  (if (situation-severity situation)
    (str "  " (situation-color situation (str "· " (get situation-text situation (name situation)))))
    ""))

(defn menu-items
  [{:keys [subject situation edges]} trail]
  (let [recommended (recommended-probe situation)
        probe-items (for [probe (applicable-probes (:kind subject) recommended)]
                      {:type :probe :probe probe
                       :label (str (:label probe)
                                   (when (= (:key probe) recommended) "   ⭐ recommended"))})
        edge-items (for [edge edges]
                     {:type :pivot :subject (:subject edge)
                      :label (str (situation-marker (:situation edge))
                                  "➜ " (:relation edge) ": " (subject/display (:subject edge))
                                  (situation-badge (:situation edge)))})]
    (vec (concat probe-items
                 edge-items
                 (when (seq trail) [{:type :back :label "⬆ back to the previous subject"}])
                 (when (> (count trail) 1) [{:type :jump :label "⏮ jump back to an earlier subject"}])
                 [{:type :done :label "✓ done - end the investigation"}]))))

;; ---------------------------------------------------------------------------
;; Hostname matching (pure): resolve a customer-reported host to the routes that
;; serve it, wildcards included.

(defn host-matches?
  [pattern host]
  (let [pattern (str pattern)]
    (or (= pattern host)
        (and (str/starts-with? pattern "*.")
             (str/ends-with? host (subs pattern 1))
             (not= host (subs pattern 1))))))

(defn routes-for-host
  "Pure: the HTTPRoute items whose hostnames serve the given host."
  [route-items host]
  (filter (fn [route]
            (some #(host-matches? % host) (-> route :spec :hostnames)))
          route-items))

(defn ingresses-for-host
  "Pure: the Ingress items whose rules serve the given host - the classic path
   for a customer-reported URL that is not fronted by Gateway API."
  [ingress-items host]
  (filter (fn [ingress]
            (some #(host-matches? % host) (keep :host (-> ingress :spec :rules))))
          ingress-items))

(defn parse-top-usage
  "Pure: a compact usage string from a `kubectl top --no-headers` line. A pod
   line is NAME CPU MEM; a node line is NAME CPU CPU% MEM MEM%. nil when the line
   is empty or malformed (no metrics-server, a dead pod), so usage is shown only
   when it is real."
  [kind line]
  (let [fields (remove str/blank? (str/split (str/trim (str line)) #"\s+"))]
    (case kind
      "Pod" (when (>= (count fields) 3)
              (format "cpu %s · mem %s" (nth fields 1) (nth fields 2)))
      "Node" (when (>= (count fields) 5)
               (format "cpu %s (%s) · mem %s (%s)"
                       (nth fields 1) (nth fields 2) (nth fields 3) (nth fields 4)))
      nil)))

;; ---------------------------------------------------------------------------
;; Session state: the trail is saved on exit so an investigation can be resumed
;; where it left off - keeping context across a closed terminal, not just within
;; one run. Subjects are plain {:kind :namespace :name} maps, so this is just
;; EDN. Best-effort: a failure to save or load never breaks an investigation.

(def ^:private state-file "recordings/last-investigation.edn")

(defn save-trail!
  ([trail] (save-trail! state-file trail))
  ([path trail]
   (try
     (fs/create-dirs (fs/parent path))
     (spit path (pr-str {:trail (vec trail)}))
     (catch Exception _ nil))))

(defn load-trail
  ([] (load-trail state-file))
  ([path]
   (try
     (when (fs/exists? path)
       (:trail (edn/read-string (slurp path))))
     (catch Exception _ nil))))

;; ---------------------------------------------------------------------------
;; Live fetching and the loop (impure).

(defn- fetch-object
  [context {:keys [kind namespace name]}]
  (let [type (subject/kind->type kind)]
    (if (str/blank? (str namespace))
      (kubectl/get-cluster-resource context type name)
      (kubectl/get-namespaced-resource context namespace type name))))

(defn- pod-subject
  [pod]
  (subject/subject "Pod" (kubectl/namespace-of pod) (kubectl/name-of pod)))

(defn- unhealthy-first
  [pods]
  (sort-by #(if (= :ok (subject/situation "Pod" %)) 1 0) pods))

(defn- label-selector
  [labels]
  (str/join "," (map (fn [[k v]] (str (name k) "=" v)) labels)))

(defn- selected-pods
  [context namespace labels]
  (when (seq labels)
    (->> (kubectl/items-of (kubectl/get-selected context "pods" (label-selector labels)))
         (filter #(= namespace (kubectl/namespace-of %)))
         (unhealthy-first))))

(def ^:private related-cap 12)

(defn- pod-edges-from
  [relation pods]
  (for [pod (take related-cap pods)]
    {:relation relation :subject (pod-subject pod) :situation (subject/situation "Pod" pod)}))

(defn- exposed-by!
  "The routes and ingresses that front a service - the traffic coming in - found
   by filtering all of them through the pure exposes-service? test."
  [context namespace name]
  (let [service (subject/subject "Service" namespace name)]
    (concat
     (for [route (kubectl/items-of (kubectl/get-all context (subject/kind->type "HTTPRoute")))
           :when (subject/exposes-service? "HTTPRoute" route service)]
       {:relation "exposed by"
        :subject (subject/subject "HTTPRoute" (kubectl/namespace-of route) (kubectl/name-of route))})
     (for [ingress (kubectl/items-of (kubectl/get-all context "ingresses"))
           :when (subject/exposes-service? "Ingress" ingress service)]
       {:relation "exposed by"
        :subject (subject/subject "Ingress" (kubectl/namespace-of ingress) (kubectl/name-of ingress))}))))

(defn- fetched-edges!
  "Related subjects that need a cluster query: a node's pods, a workload's pods,
   a service's backends and what fronts it. Unhealthy-first and capped, so a busy
   node does not bury the trail."
  [context {:keys [kind namespace name]} object]
  (case kind
    "Pod"
    (for [service (kubectl/items-of (kubectl/get-namespaced context namespace "services"))
          :when (subject/service-selects-pod? service object)]
      {:relation "member of" :subject (subject/subject "Service" namespace (kubectl/name-of service))})
    "Node"
    (pod-edges-from "hosts pod" (unhealthy-first (kubectl/items-of (kubectl/pods-on-node context name))))
    ("Deployment" "StatefulSet" "DaemonSet" "ReplicaSet" "Job")
    (pod-edges-from "pod" (selected-pods context namespace (-> object :spec :selector :matchLabels)))
    "Service"
    (concat (exposed-by! context namespace name)
            (pod-edges-from "backend pod" (selected-pods context namespace (-> object :spec :selector))))
    []))

(defn- collapse-replicaset!
  "Rewrites an 'owned by ReplicaSet' edge to its Deployment, so the operator
   pivots straight to the workload that matters instead of the intermediate
   ReplicaSet."
  [context edge]
  (if (= "ReplicaSet" (-> edge :subject :kind))
    (if-let [owner (first (subject/owner-edges (fetch-object context (:subject edge))))]
      (assoc edge :subject (:subject owner))
      edge)
    edge))

(defn- focus!
  [context subject]
  (let [object (fetch-object context subject)]
    (if (nil? object)
      {:subject subject :missing? true}
      (let [object-edges (map #(collapse-replicaset! context %)
                              (subject/object-edges (:kind subject) object))]
        {:subject subject
         :object object
         :facts (subject/facts (:kind subject) object)
         :situation (subject/situation (:kind subject) object)
         :edges (concat object-edges (fetched-edges! context subject object))}))))

(defn- recent-warnings
  "The recent Warning events about this object - the concise smoking gun,
   captured (so it is time-bounded) rather than the whole describe output. A
   cluster-scoped object's events (a node's) live in the default namespace, so
   they are searched across all namespaces rather than the current one."
  [context {:keys [namespace name]}]
  (let [scope (if (str/blank? (str namespace)) ["--all-namespaces"] [(str "--namespace=" namespace)])
        args (into ["kubectl" (str "--context=" context)]
                   (concat scope
                           ["get" "events"
                            (str "--field-selector=involvedObject.name=" name ",type=Warning")
                            "--output=custom-columns=REASON:.reason,MESSAGE:.message" "--no-headers"]))]
    (->> (apply shell/stdout-of args)
         str/split-lines
         (remove str/blank?)
         (take-last 5))))

(defn- auto-surface!
  "For a subject that looks wrong, show the evidence a good SRE would reach for
   first, without being asked: the recent Warning events, plus the crash logs
   when it is crash-looping."
  [context focus]
  (let [subject (:subject focus)
        warnings (recent-warnings context subject)]
    (when (seq warnings)
      (stdout/err-println (str "  " (stdout/bold "recent warnings:")))
      (doseq [line warnings] (stdout/err-println (str "    " line))))
    (when (= :crash-looping (:situation focus))
      (stdout/err-println (str "  " (stdout/bold "last crash logs:")))
      (apply shell/run-with-output
             (with-context context (:namespace subject) "logs" (:name subject)
                           "--previous" "--tail=20" "--all-containers")))))

(defn- usage-line
  "Live resource usage for a pod or node, from `kubectl top` - so the facts panel
   shows what it is actually using next to its limits (a memory limit means
   little without the number beside it). Captured, so it is time-bounded; nil
   when metrics are unavailable, never an error."
  [context {:keys [kind namespace name]}]
  (case kind
    "Pod" (parse-top-usage "Pod" (apply shell/stdout-of
                                        (with-context context namespace "top" "pod" name "--no-headers")))
    "Node" (parse-top-usage "Node" (apply shell/stdout-of
                                          (with-context context nil "top" "node" name "--no-headers")))
    nil))

(defn- render!
  [context focus]
  (let [{:keys [subject facts situation]} focus]
    (stdout/print-section (str "🔬 " (subject/display subject)))
    (doseq [[label value] facts]
      (stdout/err-println (format "  %-14s %s" (str label ":") value)))
    (when-let [usage (usage-line context subject)]
      (stdout/err-println (format "  %-14s %s" "usage:" usage)))
    (when (and situation (not= :ok situation))
      (stdout/warn (get situation-text situation (name situation)))
      (auto-surface! context focus))))

(defn- choose!
  [focus trail]
  (let [items (menu-items focus trail)
        ;; fzf --ansi returns the selected line without its color codes, so items
        ;; are keyed by the stripped label to be found again.
        by-label (into {} (map (fn [item] [(stdout/strip-colors (:label item)) item])) items)]
    (get by-label (fuzzy/select-single "next" (map :label items)))))

(defn- run-probe!
  [context subject probe]
  (stdout/print-section (str "▶ " (:label probe)))
  (apply shell/run-with-output ((:args probe) context subject)))

(defn trail-jump
  "The loop state to resume at breadcrumb `i`: that subject becomes the focus and
   the trail rewinds to everything before it - so landing there is exactly the
   state the operator had when they first stood on it."
  [trail i]
  {:subject (nth trail i)
   :trail (subvec (vec trail) 0 i)})

(defn- choose-jump!
  "Lets the operator pick any earlier subject in the trail (numbered as in the
   summary) and returns the state to resume there, or nil when they back out - so
   a deep drill-down can rewind many levels at once, not just one."
  [trail]
  (let [labelled (map-indexed (fn [i s] [(format "%d. %s" (inc i) (subject/display s)) i]) trail)
        by-label (into {} labelled)]
    (when-let [i (get by-label (fuzzy/select-single "jump back to" (map first labelled)))]
      (trail-jump trail i))))

(defn- summarise!
  [trail]
  ;; persist the trail so ./investigate --resume can pick up here next time
  (save-trail! trail)
  (stdout/print-section "🧭 Investigation trail")
  (doseq [[i s] (map-indexed vector trail)]
    (stdout/err-println (format "  %d. %s" (inc i) (subject/display s))))
  (stdout/err-println "")
  (stdout/err-println "resume this exactly where you left off with ./investigate --resume")
  (stdout/err-println (stdout/wrap "the exact kubectl calls are offered as a reproducer below - keep them with the incident notes"))
  true)

(defn investigate!
  "Runs the drill-down loop from a starting subject, optionally seeded with a
   breadcrumb trail (for --resume). Returns true - a read-only investigation
   never fails - so the harness closes on a green banner and offers the
   reproducer of everything it ran."
  ([context start] (investigate! context start []))
  ([context start initial-trail]
   (stdout/print-section "🔦 Drill-down investigation")
   (stdout/err-println "  pivot to a related object to follow the thread, run a probe to look closer,")
   (stdout/err-println "  or end the investigation - nothing here changes anything.")
   (loop [subject start
          trail (vec initial-trail)]
    (let [focus (stdout/with-spinner (str "gathering " (subject/display subject))
                                     #(focus! context subject))]
      (if (:missing? focus)
        (do (stdout/warn (format "%s not found - it may have just been deleted" (subject/display subject)))
            (if (seq trail)
              (recur (peek trail) (pop trail))
              (summarise! [subject])))
        (do
          (render! context focus)
          (let [item (choose! focus trail)]
            (case (:type item)
              (nil :done) (summarise! (conj trail subject))
              :back (recur (peek trail) (pop trail))
              :jump (if-let [dest (choose-jump! trail)]
                      (recur (:subject dest) (:trail dest))
                      (recur subject trail))
              :probe (do (run-probe! context subject (:probe item))
                         (recur subject trail))
              :pivot (recur (:subject item) (conj trail subject))))))))))
