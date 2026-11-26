;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detective
  "Detectives extract data from a cluster and point at possible issues.

   A detective is plain data:
     {:name        \"pods\"
      :description \"what it looks for\"
      :requires    [\"pods\"]                  ; resource types it needs
      :detect      (fn [evidence] [finding])} ; pure - no cluster access

   Evidence is collected once per investigation and shared by all detectives,
   keyed by resource type, plus :now for time-based checks and \"options\" for
   the book's own options beyond --output. Because :detect is
   pure over that map, detectives compose freely into bigger investigations
   and are trivially unit-testable.

   A finding:
     {:severity :critical | :warning | :info
      :component \"namespace/name\"
      :summary   \"one line of what is wrong\"
      :hint      \"optional: what to do about it\"}"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [gumshoe.config :as config]
            [gumshoe.correlation :as correlation]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.flags :as flags]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.investigation :as investigation]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]
            [gumshoe.report :as report]
            [gumshoe.summary :as summary]
            [gumshoe.progress :as progress]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]
            [gumshoe.subject :as subject]
            [gumshoe.theme :as theme]))

(def severity-order {:critical 0 :warning 1 :info 2})

(def output-option
  "Standard CLI option shared by every detective runbook."
  {:output {:desc "Report format: text, json, edn, or a plugin-registered format"
            :alias :o
            :default "text"
            ;; Validate against the live registry, not a frozen literal, so a
            ;; plugin format (sarif, junit, ...) registered via report/register-format!
            ;; is accepted at parse time instead of rejected before it can run.
            :validate {:pred (fn [value] (contains? (set (report/registered)) value))
                       :ex-msg (fn [{:keys [value]}]
                                 (format "invalid report format %s - registered formats: %s"
                                         (pr-str value) (apply str (interpose ", " (report/registered)))))}
            :coerce :string}})

(defn healthy?
  "True when nothing critical was found."
  [findings]
  (not-any? #(= :critical (:severity %)) findings))

(defn summary-of
  [findings]
  (merge {:critical 0 :warning 0 :info 0}
         (frequencies (map :severity findings))))

(def crashed-summary
  "The summary of the finding a detective that threw yields in place of its own."
  "detective failed to run - investigate its input")

(defn normalize-finding
  "A finding whose :severity the report can rank. A string severity reads as
   its keyword (\"critical\" from JSON or EDN data), and any other value - a
   typo such as :error, nil, a number - counts as :critical with a hint naming
   what the detective returned, so a finding the report cannot rank fails the
   run instead of passing it."
  [finding]
  (let [severity (:severity finding)
        severity (if (string? severity) (keyword (str/lower-case (str/trim severity))) severity)]
    (if (contains? severity-order severity)
      (assoc finding :severity severity)
      (assoc finding
             :severity :critical
             :hint (str (format "the detective returned the severity %s, which is not :critical, :warning or :info"
                                (pr-str (:severity finding)))
                        (when-let [hint (:hint finding)] (str " - " hint)))))))

(defn run-detectives
  "Applies every detective to already-collected evidence. A detective that
   throws on unexpected data does not take down the whole investigation: it
   yields one critical finding marked :crashed reporting its own failure, so
   every other detective's results still reach the operator and the run fails,
   since a check that could not judge its evidence has not passed. A pod finding
   that follows from a finding about its workload or node carries that one as
   :parent."
  [detectives evidence]
  (correlation/correlate
   (mapcat (fn [{:keys [name detect]}]
             (try
               ;; detectives are lazy for-comprehensions - realize them HERE,
               ;; inside the try, or a throw during realization would escape
               (doall (map #(normalize-finding (assoc % :detective name)) (detect evidence)))
               ;; Throwable, not Exception: an assert, a :pre condition or a
               ;; StackOverflowError in one detective must not discard every
               ;; other detective's findings.
               (catch Throwable e
                 [{:severity :critical
                   :crashed true
                   :detective name
                   :component name
                   :summary crashed-summary
                   :hint (or (ex-message e) (str e))}])))
           detectives)
   evidence))

(defn crashed-detectives
  "The names of the detectives whose :detect threw, in the order they ran."
  [findings]
  (vec (distinct (keep #(when (:crashed %) (:detective %)) findings))))

(defn crashed-message
  "Why a run with a crashed detective fails, naming each one."
  [names]
  (format "%d check%s could not run, so %s judged nothing - %s"
          (count names) (if (= 1 (count names)) "" "s")
          (if (= 1 (count names)) "it" "they")
          (str/join ", " names)))

(defn empty-subjects?
  "Whether one evidence value holds nothing to judge: nil, an empty list,
   or a kubectl list document whose :items are empty."
  [v]
  (or (nil? v)
      (and (map? v) (contains? v :items) (empty? (:items v)))
      (and (coll? v) (not (map? v)) (empty? v))))

(defn unread?
  "Whether a detective read nothing. It requires evidence and every key of it
   came back nil: a resource type the cluster does not serve, a read RBAC
   refuses and a failed read all look the same here, so the detective's silence
   says nothing about health. An empty list is a read, so a served type with no
   objects does not count.

   A detective that judges a chosen set of subjects - the hosts a scope
   selected, the instances a filter kept - names the evidence keys holding
   them as :judges. When every one of them is nil or empty, it had nothing to
   judge, and its silence is no more a pass than a refused read.

   A detective that joins several reads - a Pooler to its Service, the
   Service to its EndpointSlices - names the evidence keys it joins as
   :joins. When any one of them is nil, the join cannot be made: a missing
   side would read as no endpoints, or no Service, where nobody looked. Such
   a check is unread even though the other keys answered.

   A detective that judges its subjects against several reads of which any
   one is enough - each judged only when it answered - names them as
   :consults. When every one of them is nil, it has nothing to judge against,
   and its silence is no more a pass than a refused read. An empty list
   answers, so a consulted read that came back empty still counts."
  [{:keys [requires judges joins consults]} evidence]
  (boolean (or (and (seq requires) (every? #(nil? (get evidence %)) requires))
               (and (seq joins) (some #(nil? (get evidence %)) joins))
               (and (seq consults) (every? #(nil? (get evidence %)) consults))
               (and (seq judges) (every? #(empty-subjects? (get evidence %)) judges)))))

(defn mark-unread
  "The detectives, each that read nothing carrying :unread true, so a report
   can tell a check that passed from one that had nothing to look at."
  [detectives evidence]
  (mapv #(cond-> % (unread? % evidence) (assoc :unread true)) detectives))

(defn unread-line
  "The line naming the checks that read nothing, grouped by the capability
   their tool package inspects, or nil when every check read something."
  [detectives]
  (let [unread (filter :unread detectives)]
    (when (seq unread)
      (format "%d check%s read nothing on this cluster - %s; what they read is not served, not readable, or not gathered by this scan"
              (count unread) (if (= 1 (count unread)) "" "s")
              (str/join ", "
                        (for [[caps ds] (sort-by (fn [[caps _]] [(empty? caps) (str caps)])
                                                 (group-by #(vec (:capabilities %)) unread))]
                          (format "%s (%s)"
                                  (if (seq caps) (str/join "/" (map name caps)) "other")
                                  (str/join ", " (map :name ds)))))))))

(defn- evidence-base
  "The evidence every investigation carries besides what its detectives
   require: the time it was read, the cluster, and the operator's expectations,
   so a detective can tell an intended state from a surprising one (e.g. which
   cluster-admins are known) while staying pure over the map."
  [cluster]
  {:now (java.time.Instant/now)
   "cluster" cluster
   "config" (config/active-config {:kubernetes-cluster cluster})})

(defn- required-keys
  [detectives]
  (vec (into (sorted-set) (mapcat :requires detectives))))

(def ^:dynamic *collector-timeout-ms*
  "How long one evidence collector may run before its key counts as not read.
   A kubectl read carries its own request timeout; a registered collector that
   talks SSH, HTTP or a port-forward may not, and one that blocks must not hang
   the whole scan."
  300000)

(defn- bounded-collect
  "The value collector returns for key within timeout-ms, or ::timed-out after
   cancelling it. Exceptions from the collector reach the caller. A caller
   interrupted while it waits - a post-check attempt abandoned at its deadline
   - cancels the collector too: an interrupt reaches only the thread it was
   sent to, so the collector's own thread would otherwise run on, command
   after command, after the attempt was given up."
  [collect timeout-ms]
  (let [work (future (collect))]
    (try
      (deref work timeout-ms ::timed-out)
      (finally
        (when-not (realized? work) (future-cancel work))))))

(defn timed-out-message
  "The warning naming the evidence keys whose collector did not return in time."
  [keys timeout-ms]
  (format "%s did not answer within %ds and count%s as not read: %s"
          (if (= 1 (count keys)) "one evidence read" (str (count keys) " evidence reads"))
          (quot timeout-ms 1000)
          (if (= 1 (count keys)) "s" "")
          (str/join ", " keys)))

(defn read-evidence
  "The evidence the detectives require, read one key after another without
   printing anything, for a check that reads it again and again. Unlike
   collect-evidence! it lets a collector's exception through, so a strict read
   that did not answer reaches the caller instead of reading as no evidence. A
   collector that runs past *collector-timeout-ms* throws an ex-info naming its
   key."
  [context cluster detectives]
  (let [timeout-ms *collector-timeout-ms*]
    (into (evidence-base cluster)
          (for [key (required-keys detectives)]
            (let [value (try (bounded-collect #((registry/collector-for key) context key) timeout-ms)
                             (catch java.util.concurrent.ExecutionException e
                               (throw (or (ex-cause e) e))))]
              (when (= ::timed-out value)
                (throw (ex-info (timed-out-message [key] timeout-ms) {::timed-out key})))
              [key value])))))

(defn collect-evidence!
  "Fetches every resource type required by the given detectives exactly once,
   all types in parallel. A type the cluster does not know (missing CRD)
   yields no evidence, so the corresponding detectives simply report nothing.
   A collector that throws or runs past *collector-timeout-ms* yields nil for
   its key, which marks the detectives requiring it unread; the keys that timed
   out are named once collection ends."
  [context detectives]
  (let [types (required-keys detectives)
        cluster (try (kubectl/current-cluster) (catch Exception _ nil))
        timeout-ms *collector-timeout-ms*
        timed-out (atom [])]
    (stdout/print-section "🔍 Evidence")
    ;; every resource type is fetched in parallel and shown on a live task list
    ;; that ticks each one off as it lands - so a slow fetch reads as progress,
    ;; not a stall.
    (let [labelled (mapv (fn [type]
                           ;; every evidence key resolves to a collector through
                           ;; the registry; a plain resource type gets the get-all
                           ;; default, a key like "api-resources" gets its
                           ;; registered collector - one uniform seam, no special
                           ;; case, so a custom-evidence detective composes into
                           ;; any scan.
                           [type (future
                                   (let [value (try (bounded-collect #((registry/collector-for type) context type)
                                                                     timeout-ms)
                                                    (catch Throwable _ nil))]
                                     (if (= ::timed-out value)
                                       (do (swap! timed-out conj type) nil)
                                       value)))])
                         types)
          results (progress/watch-futures! labelled)]
      (when (seq @timed-out)
        (stdout/warn (timed-out-message (sort @timed-out) timeout-ms)))
      (into (evidence-base cluster) (map vector types results)))))

(defn- severity-tag
  [severity]
  (case severity
    :critical (stdout/red (str (theme/severity :critical) " CRITICAL"))
    :warning (stdout/yellow (str (theme/severity :warning) " WARNING "))
    :info (stdout/blue (str (theme/severity :info) " INFO    "))
    (stdout/red (str (theme/severity :critical) " " (pr-str severity)))))

(defn- worst-severity
  "The most severe severity-order among findings (critical wins), for ordering
   areas so the story opens on what is most broken."
  [findings]
  (apply min (map #(severity-order (:severity %) 0) findings)))

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
   which is what an SRE needs to see the shape of the problem. Entries are
   ordered by severity, then by where each first appears among findings."
  [findings]
  (let [position (zipmap (reverse (map (juxt :severity :summary) findings))
                         (range (dec (count findings)) -1 -1))]
    (->> (group-by (juxt :severity :summary) findings)
         ;; group-by keeps no order past eight keys, so entries of one
         ;; severity follow the order the detective returned its findings in
         (sort-by (fn [[[severity _ :as k] _]] [(severity-order severity) (position k)]))
         (mapv (fn [[[severity summary] fs]]
                 {:severity severity
                  :summary summary
                  :count (count fs)
                  :components (mapv :component fs)
                  :subjects (vec (distinct (keep subject/from-finding fs)))
                  :since (onset/earliest fs)
                  :hint (:hint (first fs))})))))

(def ^:private sample-size
  "How many affected components to name before summing the rest - enough to
   recognise the pattern without scrolling past it."
  4)

(defn- folded-under
  "The findings folded under these subjects, and under theirs in turn: a
   folded finding is no root of its own, so whatever follows from it is shown
   with the root it leads to."
  [subjects children]
  (loop [pending (map subject/display subjects) seen #{} folded []]
    (if-let [parent (first pending)]
      (if (seen parent)
        (recur (rest pending) seen folded)
        (let [found (get children parent)]
          (recur (concat (rest pending) (keep #(some-> (subject/from-finding %) subject/display) found))
                 (conj seen parent)
                 (into folded found))))
      folded)))

(defn folded-line
  "The line naming the findings that follow from an entry's objects, or nil
   when none do: their count and each distinct summary with its count."
  [subjects children]
  (let [folded (folded-under subjects children)]
    (when (seq folded)
      (format "follows from it: %d finding%s - %s"
              (count folded) (if (= 1 (count folded)) "" "s")
              (str/join ", " (for [[summary fs] (sort-by (comp - count val) (group-by :summary folded))]
                               (if (> (count fs) 1) (format "%s ×%d" summary (count fs)) summary)))))))

(defn- print-area!
  "Prints one area: a header naming the detective and its finding count, then
   each rolled-up entry with a sample of the components it affects and the
   findings folded under it."
  [detective findings children now]
  (let [n (count findings)]
    (println (str "  " (stdout/bold detective)
                  (format " (%d finding%s)" n (if (= 1 n) "" "s")))))
  (doseq [{:keys [severity summary components subjects since hint] total :count} (roll-up findings)]
    (println (str "    " (severity-tag severity) " " summary
                  (when (> total 1) (stdout/bold (format "  ×%d" total)))
                  (when-let [held (onset/label since now)] (str "  " held))))
    (let [shown (take sample-size components)
          more (- total (count shown))]
      (println (str "        " (str/join ", " shown)
                    (when (pos? more) (format " (+%d more)" more)))))
    (when hint
      (println (stdout/hanging "        " "hint: " hint (stdout/results-wrap-width))))
    (when-let [line (folded-line subjects children)]
      (println (str "        " line)))))

(defn- print-clean!
  "Names the detectives that came back clean, so a run with problems still shows
   the breadth of what was ruled out."
  [detectives findings]
  (let [noisy (set (map :detective findings))
        quiet (remove #(noisy (:name %)) detectives)
        clean (remove :unread quiet)]
    (when (seq clean)
      (println (str (stdout/green "✓")
                    (format " %d other check%s came back clean: "
                            (count clean) (if (= 1 (count clean)) "" "s"))
                    (str/join ", " (map :name clean)))))
    (when-let [line (unread-line quiet)]
      (println (str (theme/token :bullet) " " line)))))

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
      (doseq [{:keys [description]} (remove :unread detectives)]
        (println (str "  " (stdout/green "✓") " " description)))
      (stdout/print-section-marker)
      (let [passed (count (remove :unread detectives))]
        (stdout/ok (format "no issues found - %d check%s passed" passed (if (= 1 passed) "" "s"))))
      (when-let [line (unread-line detectives)]
        (println (str (theme/token :bullet) " " line))))
    (do
      (let [children (correlation/children findings)]
        (doseq [[detective fs] (areas (remove :parent findings))]
          (print-area! detective fs children (java.time.Instant/now))))
      (stdout/print-section-marker)
      (let [counts (summary-of findings)
            folded (count (filter :parent findings))]
        (println (str (stdout/red (str (:critical counts) " critical")) ", "
                      (stdout/yellow (str (:warning counts) " warning")) ", "
                      (stdout/blue (str (:info counts) " info"))
                      (when (pos? folded)
                        (format " (%d shown under the finding they follow from)" folded)))))
      (print-clean! detectives findings)))
  (healthy? findings))

(defn- checked
  "The compact record of what ran, for machine-readable output: every
   detective's name and what it looks for, so a clean json/edn report still
   proves its coverage, with the capabilities it inspects and whether it read
   nothing."
  [detectives]
  (mapv #(select-keys % [:name :description :capabilities :unread]) detectives))

(defn nothing-checked?
  "Whether a run judged nothing: it produced no findings and no detective read
   anything. Such a run has not passed, it has not run, so a report treats it
   as a failure rather than a clean result."
  [detectives findings]
  (and (empty? findings) (every? :unread detectives)))

(defn nothing-checked-message
  "Why a run with no findings is still not a pass."
  [detectives]
  (if (empty? detectives)
    "no check ran, so nothing was checked"
    (format "every check read nothing, so nothing was checked - %s"
            (str/join ", " (map :name detectives)))))

(defn- json-safe
  "data with every value cheshire has no encoder for replaced by its string
   form, so one exotic field in a finding or a book's :extra does not cost the
   whole report. cheshire encodes java.util.Date and java.time.Instant (both
   inst?) and UUIDs; every other java.time value - LocalDate, ZonedDateTime,
   OffsetDateTime and the rest - has no encoder and is written as its string."
  [data]
  (walk/postwalk (fn [v]
                   (if (or (nil? v) (string? v) (number? v) (boolean? v) (keyword? v) (symbol? v)
                           (map? v) (coll? v) (map-entry? v) (inst? v) (uuid? v))
                     v
                     (str v)))
                 data))

(defn- json-plain
  "data with every value that is not a string, number, boolean, keyword,
   symbol, nil or collection replaced by its string form: the last resort
   when even json-safe's output does not encode."
  [data]
  (walk/postwalk (fn [v]
                   (if (or (nil? v) (string? v) (number? v) (boolean? v) (keyword? v) (symbol? v)
                           (map? v) (coll? v) (map-entry? v))
                     v
                     (str v)))
                 data))

(defn json-report
  "The machine-readable report as pretty JSON. A value cheshire cannot encode
   is written as its string form rather than failing the report."
  [data]
  (try (json/generate-string data {:pretty true})
       (catch Exception _
         (try (json/generate-string (json-safe data) {:pretty true})
              (catch Exception _
                (json/generate-string (json-plain data) {:pretty true}))))))

(defn- written?
  "Flushes *out* and whether everything printed to it arrived. A PrintWriter
   swallows the IOException of a full disk or a closed stdout and only
   remembers it for checkError, so without this a report that was never
   written would end like one that was.

   Under babashka a script's *out* is an OutputStreamWriter over System/out,
   a PrintStream that swallows the same IOException: flushing the writer
   never throws, and only System/out's own checkError (which flushes it)
   tells. It is consulted whatever *out* is, because the bytes of the real
   stdout end there either way."
  []
  (let [out-ok (if (instance? java.io.PrintWriter *out*)
                 (not (.checkError ^java.io.PrintWriter *out*))
                 (try (flush) true
                      (catch java.io.IOException _ false)))]
    (and out-ok (not (.checkError System/out)))))

(defn- delivered
  "healthy when the report reached stdout; otherwise names the failure on
   stderr and returns false, so a scan whose report is missing never exits 0."
  [healthy]
  (if (written?)
    healthy
    (do (stdout/error "the report could not be written to stdout (a full disk or a closed pipe?) - nothing was reported")
        false)))

(defn report!
  "Renders findings in the requested format (text, json, edn) to stdout, always
   alongside the list of detectives that ran. Returns true when nothing critical
   was found, no detective crashed and at least one detective read something; a
   run that judged nothing reports :nothing-checked and returns false, and a run
   with a detective that threw names it under :crashed and returns false.

   `extra` is a map a book adds to the machine-readable formats, such as the
   rows it collected; the report's own keys win over it, and the text report
   leaves it out."
  ([detectives findings output] (report! detectives findings output nil))
  ([detectives findings output extra]
   (let [findings (mapv normalize-finding findings)
         blind (nothing-checked? detectives findings)
         crashed (crashed-detectives findings)
         data (cond-> (merge extra {:findings findings
                                    :checked (checked detectives)
                                    :summary (summary-of findings)})
                blind (assoc :nothing-checked true)
                (seq crashed) (assoc :crashed crashed))
         healthy (and (not blind) (empty? crashed) (healthy? findings))]
     ;; keep the findings so the harness banners them and offers to share
     ;; them - every scan renders through here, the custom flows included
     (summary/capture! findings)
     (delivered
      (case output
        "json" (do (println (json-report data))
                   healthy)
        "edn" (do (prn data)
                  healthy)
        ;; a plugin's format (sarif, junit, …), else the rich terminal report
        (if-let [render (report/format-fn output)]
          (do (render data) healthy)
          (if blind
            (do (stdout/print-section "🔬 Findings")
                (stdout/error (nothing-checked-message detectives))
                false)
            (let [shown (print-report! detectives findings)]
              (when (seq crashed)
                (stdout/error (crashed-message crashed)))
              (and shown (empty? crashed))))))))))

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
    :critical (stdout/red (theme/marker :critical))
    :warning (stdout/yellow (theme/marker :warning))
    :info (stdout/blue (theme/marker :info))
    (theme/token :bullet)))

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
            (investigation/investigate! (force context) subject)))))))

(defn report-and-offer!
  "Reports findings and, on a real terminal in text mode, offers to drill into
   them. The one call a detective runbook makes after gathering evidence, so
   every scan - the generic kubectl ones and the custom flows alike - ends with
   the same drill-down. Returns true when nothing critical was found.

   context may be a delay, forced only when the operator drills into a
   finding, so a scan that never touched a cluster does not read the
   kubeconfig. `extra` is handed to report!."
  ([context detectives findings output]
   (report-and-offer! context detectives findings output nil))
  ([context detectives findings output extra]
   (let [healthy (report! detectives findings output extra)]
     (when (= "text" output)
       (offer-drill-down! context findings))
     healthy)))

(defn scan!
  "Collects evidence, marks the detectives that read nothing, runs them,
   reports and offers the drill-down. The one flow every detective book runs,
   whatever its evidence comes from.

   :collect  (fn [opts]) -> the evidence map, or nil when it can not collect;
             it prints why, and the scan fails without reporting
   :around   (fn [opts run]) optional; wraps the collection, for a
             port-forward or a connection check, and calls (run opts) with the
             opts :collect receives - or returns false without calling it
   :extra    (fn [evidence]) optional; a map the machine-readable report
             carries beside the findings, such as the rows collected
   :context  the kube context drill-down investigates in, or a delay of it

   The parsed opts reach every detective as the \"options\" evidence. Returns
   true when something was checked and nothing critical was found."
  [{:keys [collect around extra context]} detectives opts]
  (let [around (or around (fn [opts run] (run opts)))]
    (around opts
            (fn [collect-opts]
              (if-let [evidence (collect collect-opts)]
                (let [evidence (assoc evidence "options" (dissoc opts :output))
                      checked (mark-unread detectives evidence)
                      findings (run-detectives checked evidence)
                      output (:output opts "text")]
                  (if extra
                    (report-and-offer! context checked findings output (extra evidence))
                    (report-and-offer! context checked findings output)))
                false)))))

(defn investigate!
  "Collects evidence from the cluster, runs the detectives, reports, and - on a
   real terminal - offers to drill straight into a finding. Returns true when
   nothing critical was found."
  ([context detectives]
   (investigate! context detectives {}))
  ([context detectives opts]
   (scan! {:collect (fn [_] (collect-evidence! context detectives)) :context context}
          detectives opts)))

(defn check-step
  "A playbook :check step that reads the evidence its detectives need from the
   cluster and reports what they found. It stops the playbook when something
   critical was found or nothing was checked, unless :blocking? is false, so
   an advisory check before the confirmation only informs.

   Spec: {:title \"...\" :prerequisites {..} :detectives [..] :blocking? true}"
  [{:keys [title prerequisites detectives blocking?] :or {blocking? true}}]
  {:title title
   :prerequisites prerequisites
   :check (fn [{:keys [context]}]
            (let [evidence (collect-evidence! context detectives)
                  checked (mark-unread detectives evidence)
                  healthy (report! checked (run-detectives checked evidence) "text")]
              (or (not blocking?) healthy)))})

(defn no-detectives-message
  "Why a book found nothing to run. A scan with no detectives examines no
   evidence and can report no findings, which renders as a clean cluster - so it
   is an error, not a pass. The usual cause is a scope whose detectives come
   from a plugin that env.edn does not list."
  [scope]
  (if scope
    (format (str "the %s scope has no detectives registered, so this scan checked nothing - "
                 "a scope's detectives come from plugins, so add the one that provides them "
                 "to :plugins in env.edn")
            scope)
    "this book lists no detectives, so the scan checked nothing"))

(defn book-detectives
  "The detectives a book runs: its scope's, resolved now, then the listed ones
   the scope does not already hold."
  [scope detectives]
  (let [scoped (when scope (registry/for-scope scope))
        names (set (map :name scoped))]
    (into (vec scoped) (remove #(names (:name %))) detectives)))

(defn book
  "Builds a read-only detective runbook. The whole book is this one call: the
   --output flag, the read-only harness wiring, and the investigate flow are
   all provided.

   Spec: {:description \"...\" :prerequisites {..}
          :detectives <vector> | :scope <keyword>   ; a fixed set, or a registry
          :options {..}                             ; babashka.cli spec (optional)
          :when-to-run \"one-line orientation shown before the run\" (optional)
          :collect (fn [opts]) -> evidence | nil    ; optional, see scan!
          :around (fn [opts run])                   ; optional, see scan!
          :extra (fn [evidence]) -> map}            ; optional, see scan!

   Without :collect the evidence is read from the current cluster, one read per
   resource type the detectives require. With :collect the book gathers it
   from anywhere - DNS, SSH, an HTTP API, a port-forward - and still marks the
   checks that read nothing, so a collection that came back empty fails with
   \"nothing was checked\" rather than passing.

   The parsed :options reach every detective as the \"options\" evidence, so
   a threshold a check reads is set on the command line while :detect stays
   pure over the evidence map.

   With :scope the detectives are resolved from the registry when the book runs -
   so a plugin that registered detectives into that scope joins this scan. A
   scope that resolves to nothing fails rather than reporting a clean cluster;
   see no-detectives-message. With both, the book runs the scope and the listed
   detectives besides, which is how a book opts into a check its scope leaves
   out, such as one registered into an opt-in scope."
  [{:keys [description detectives scope prerequisites when-to-run options collect around extra]}]
  (runbook/execute!
   {:description description
    :options (flags/merge-ordered options output-option)
    :prerequisites prerequisites
    :announce? false
    :action (fn [opts _ctx]
              (let [resolved (registry/with-capabilities (book-detectives scope detectives))]
                (if (empty? resolved)
                  (do (stdout/error (no-detectives-message scope)) false)
                  (do (when-to-run! when-to-run)
                      (if collect
                        (scan! {:collect collect :around around :extra extra
                                :context (delay (kubectl/current-context))}
                               resolved opts)
                        (investigate! (kubectl/current-context) resolved opts))))))}))
