;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detective
  "Detectives extract data from a cluster and point at possible issues.

   A detective is plain data:
     {:name        \"pods\"
      :description \"what it looks for\"
      :requires    [\"pods\"]                  ; resource types it needs
      :detect      (fn [evidence] [finding])} ; pure - no cluster access

   Evidence is collected once per investigation and shared by all detectives,
   keyed by resource type, plus :now for time-based checks. Because :detect is
   pure over that map, detectives compose freely into bigger investigations
   and are trivially unit-testable.

   A finding:
     {:severity :critical | :warning | :info
      :component \"namespace/name\"
      :summary   \"one line of what is wrong\"
      :hint      \"optional: what to do about it\"}"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [infra.config :as config]
            [infra.detectives.registry :as registry]
            [infra.fuzzy-finder :as fuzzy]
            [infra.investigation :as investigation]
            [infra.kubectl :as kubectl]
            [infra.summary :as summary]
            [infra.progress :as progress]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]
            [infra.subject :as subject]))

(def severity-order {:critical 0 :warning 1 :info 2})

(def output-option
  "Standard CLI option shared by every detective runbook."
  {:output {:desc "Report format: text, json, or edn"
            :alias :o
            :default "text"
            :validate #{"text" "json" "edn"}
            :coerce :string}})

(defn healthy?
  "True when nothing critical was found."
  [findings]
  (not-any? #(= :critical (:severity %)) findings))

(defn summary-of
  [findings]
  (merge {:critical 0 :warning 0 :info 0}
         (frequencies (map :severity findings))))

(defn run-detectives
  "Applies every detective to already-collected evidence. A detective that
   throws on unexpected data does not take down the whole investigation: it
   yields one finding reporting its own failure, so every other detective's
   results still reach the operator."
  [detectives evidence]
  (vec (mapcat (fn [{:keys [name detect]}]
                 (try
                   ;; detectives are lazy for-comprehensions - realize them HERE,
                   ;; inside the try, or a throw during realization would escape
                   (doall (map #(assoc % :detective name) (detect evidence)))
                   (catch Exception e
                     [{:severity :warning
                       :detective name
                       :component name
                       :summary "detective failed to run - investigate its input"
                       :hint (ex-message e)}])))
               detectives)))

(defn collect-evidence!
  "Fetches every resource type required by the given detectives exactly once,
   all types in parallel. A type the cluster does not know (missing CRD)
   yields no evidence, so the corresponding detectives simply report nothing."
  [context detectives]
  (let [types (vec (into (sorted-set) (mapcat :requires detectives)))
        cluster (try (kubectl/current-cluster) (catch Exception _ nil))]
    (stdout/print-section "🔍 Evidence")
    ;; every resource type is fetched in parallel and shown on a live task list
    ;; that ticks each one off as it lands - so a slow fetch reads as progress,
    ;; not a stall.
    (let [labelled (mapv (fn [type]
                           [type (future (try (kubectl/get-all context type)
                                              (catch Exception _ nil)))])
                         types)
          results (progress/watch-futures! labelled)]
      (into {:now (java.time.Instant/now)
             ;; operator expectations travel with the evidence, so a detective
             ;; can tell an intended state from a surprising one (e.g. which
             ;; cluster-admins are known) while staying pure over the map.
             "cluster" cluster
             "config" (config/active-config {:kubernetes-cluster cluster})}
            (map vector types results)))))

(defn- severity-tag
  [severity]
  (case severity
    :critical (stdout/red "🔥 CRITICAL")
    :warning (stdout/yellow "🔶 WARNING ")
    :info (stdout/blue "💡 INFO    ")))

(defn- worst-severity
  "The most severe severity-order among findings (critical wins), for ordering
   areas so the story opens on what is most broken."
  [findings]
  (apply min (map (comp severity-order :severity) findings)))

(defn areas
  "Findings grouped by the detective that raised them - one area of the story -
   ordered most-severe-area first, then by name for a stable read."
  [findings]
  (->> (group-by :detective findings)
       (sort-by (fn [[detective fs]] [(worst-severity fs) detective]))))

(defn roll-up
  "Collapses repeated identical (severity, summary) findings within an area into
   one entry carrying the count, every component it covers, and the shared hint.
   A wall of 150 near-identical warnings becomes a single fact with a number,
   which is what an SRE needs to see the shape of the problem."
  [findings]
  (->> (group-by (juxt :severity :summary) findings)
       (sort-by (fn [[[severity _] _]] (severity-order severity)))
       (mapv (fn [[[severity summary] fs]]
               {:severity severity
                :summary summary
                :count (count fs)
                :components (mapv :component fs)
                :hint (:hint (first fs))}))))

(def ^:private sample-size
  "How many affected components to name before summing the rest - enough to
   recognise the pattern without scrolling past it."
  4)

(defn- print-area!
  "Prints one area: a header naming the detective and its finding count, then
   each rolled-up entry with a sample of the components it affects."
  [detective findings]
  (let [n (count findings)]
    (println (str "  " (stdout/bold detective)
                  (format " (%d finding%s)" n (if (= 1 n) "" "s")))))
  (doseq [{:keys [severity summary components hint] total :count} (roll-up findings)]
    (println (str "    " (severity-tag severity) " " summary
                  (when (> total 1) (stdout/bold (format "  ×%d" total)))))
    (let [shown (take sample-size components)
          more (- total (count shown))]
      (println (str "        " (str/join ", " shown)
                    (when (pos? more) (format " (+%d more)" more)))))
    (when hint
      (println (str "        hint: " hint)))))

(defn- print-clean!
  "Names the detectives that came back clean, so a run with problems still shows
   the breadth of what was ruled out."
  [detectives findings]
  (let [noisy (set (map :detective findings))
        clean (remove #(noisy (:name %)) detectives)]
    (when (seq clean)
      (println (str (stdout/green "✓")
                    (format " %d other check%s came back clean: "
                            (count clean) (if (= 1 (count clean)) "" "s"))
                    (str/join ", " (map :name clean)))))))

(defn print-report!
  "Tells the story of the investigation: findings grouped by area with the most
   severe area first and near-identical findings rolled up, then the counts,
   then the checks that came back clean. A clean run instead lists every check
   that passed. Returns true when nothing critical was found, so runbooks can
   turn health into an exit code."
  [detectives findings]
  (stdout/print-section "🔬 Findings")
  (if (empty? findings)
    (do
      (doseq [{:keys [description]} detectives]
        (println (str "  " (stdout/green "✓") " " description)))
      (stdout/print-section-marker)
      (stdout/ok (format "no issues found - %d check%s passed"
                         (count detectives) (if (= 1 (count detectives)) "" "s"))))
    (do
      (doseq [[detective fs] (areas findings)]
        (print-area! detective fs))
      (stdout/print-section-marker)
      (let [counts (summary-of findings)]
        (println (str (stdout/red (str (:critical counts) " critical")) ", "
                      (stdout/yellow (str (:warning counts) " warning")) ", "
                      (stdout/blue (str (:info counts) " info")))))
      (print-clean! detectives findings)))
  (healthy? findings))

(defn- checked
  "The compact record of what ran, for machine-readable output: every
   detective's name and what it looks for, so a clean json/edn report still
   proves its coverage."
  [detectives]
  (mapv #(select-keys % [:name :description]) detectives))

(defn report!
  "Renders findings in the requested format (text, json, edn) to stdout, always
   alongside the list of detectives that ran. Returns true when nothing critical
   was found."
  [detectives findings output]
  (case output
    "json" (do (println (json/generate-string {:findings findings
                                               :checked (checked detectives)
                                               :summary (summary-of findings)}
                                              {:pretty true}))
               (healthy? findings))
    "edn" (do (prn {:findings findings
                    :checked (checked detectives)
                    :summary (summary-of findings)})
              (healthy? findings))
    (print-report! detectives findings)))

(defn when-to-run!
  "Prints a short orientation before an investigation: what this book is for and
   when it is the right tool. An operator reaching for it half-blind (or reading
   a recording later) should not have to open the source to know why it helps.
   Goes to stderr, so it never pollutes json/edn results."
  [text]
  (when-not (str/blank? (str text))
    (stdout/print-section "🔎 When this helps")
    (doseq [line (str/split-lines text)]
      (stdout/err-println (str "  " line)))))

(defn- severity-marker
  "A colored icon for a finding's severity, leading its drill-down entry so the
   critical ones stand out in the fzf list."
  [severity]
  (case severity
    :critical (stdout/red "🔴")
    :warning (stdout/yellow "🟡")
    :info (stdout/blue "🔵")
    "•"))

(defn drillable-subjects
  "Pure: one entry per distinct object a scan's findings point at, in severity
   order, for the drill-down offer. A finding whose kind can not be pinned to an
   object is dropped; several findings about the same object collapse to one. The
   label leads with a colored severity marker so warnings and errors are easy to
   spot in the picker."
  [findings]
  (->> findings
       (keep (fn [finding]
               (when-let [subject (subject/from-finding finding)]
                 {:subject subject
                  :label (format "%s %s - %s"
                                 (severity-marker (:severity finding))
                                 (:summary finding)
                                 (subject/display subject))})))
       (reduce (fn [acc entry]
                 (if (some #(= (:subject %) (:subject entry)) acc)
                   acc
                   (conj acc entry)))
               [])))

(defn- interactive?
  "True only on a real terminal - the drill-down is never offered in a pipe, a
   cron job, or a json/edn run."
  []
  (some? (System/console)))

(defn- offer-drill-down!
  "After a scan, offers to drill into one or more findings - TAB picks several,
   and each is investigated in turn, each handed the same process and all its
   context. A leading 'stop here' entry keeps the list from auto-selecting a
   lone finding, so drilling is always the operator's choice, never a surprise."
  [context findings]
  (when (interactive?)
    (let [drillable (drillable-subjects findings)]
      (when (seq drillable)
        (let [stop "✓ no - stop here"
              ;; fzf --ansi returns the selected line without color codes, so the
              ;; lookup is keyed by the stripped label. The stop entry maps to no
              ;; subject, so choosing it (or nothing) drills nowhere.
              by-label (into {} (map (fn [d] [(stdout/strip-colors (:label d)) (:subject d)])) drillable)
              chosen (fuzzy/select-multi "🔦 drill into findings? (TAB to pick several)"
                                         (cons stop (map :label drillable)))]
          (doseq [label chosen
                  :let [subject (get by-label label)]
                  :when subject]
            (investigation/investigate! context subject)))))))

(defn report-and-offer!
  "Reports findings and, on a real terminal in text mode, offers to drill into
   them. The one call a detective runbook makes after gathering evidence, so
   every scan - the generic kubectl ones and the custom flows alike - ends with
   the same drill-down. Returns true when nothing critical was found."
  [context detectives findings output]
  (let [healthy (report! detectives findings output)]
    ;; keep the findings so the harness can offer to share them as a pad
    (summary/capture! findings)
    (when (= "text" output)
      (offer-drill-down! context findings))
    healthy))

(defn investigate!
  "Collects evidence, runs the detectives, reports, and - on a real terminal -
   offers to drill straight into a finding. Returns true when nothing critical
   was found."
  ([context detectives]
   (investigate! context detectives {}))
  ([context detectives opts]
   (report-and-offer! context
                      detectives
                      (run-detectives detectives (collect-evidence! context detectives))
                      (:output opts "text"))))

(defn book
  "Builds a read-only detective runbook that investigates a fixed set of
   detectives over the current cluster. The whole book is this one call: the
   --output flag, the read-only harness wiring, and the investigate flow are
   all provided.

   Spec: {:description \"...\" :prerequisites {..}
          :detectives <vector> | :scope <keyword>   ; a fixed set, or a registry
          :when-to-run \"one-line orientation shown before the run\" (optional)}

   With :scope the detectives are resolved from the registry when the book runs -
   so a plugin that registered detectives into that scope joins this scan."
  [{:keys [description detectives scope prerequisites when-to-run]}]
  (runbook/execute!
   {:description description
    :options output-option
    :prerequisites prerequisites
    :announce? false
    :action (fn [opts _ctx]
              (when-to-run! when-to-run)
              (investigate! (kubectl/current-context)
                            (or detectives (registry/for-scope scope))
                            opts))}))
