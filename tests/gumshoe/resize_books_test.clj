;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.resize-books-test
  "Characterises the two books that grow volumes, statefulsets/enlarge_volumes
   and prometheus/set_capacity. Each runs its whole :action for real against
   a fake cluster that every gumshoe.shell call answers: kubectl reads are
   answered from the recorded objects, and a patch, a delete or an apply
   changes them as the API server would, so a later read and a post-check see
   what the change left. A scenario can make one mutation fail, stop
   answering reads, change the subject while the operator confirms, decline
   the confirmation or rehearse with --dry-run.

   A transcript keeps the result, every line printed, the confirmation, the
   announcement, the mutations in the order they ran (the manifest an apply
   fed kubectl included), the post-checks with their declared timeouts, the
   banner and the recovery lines under it. Post-checks never sleep: each runs
   once with a timeout of 0. The transcripts are kept in
   tests/golden/resize_books.edn.

   Regenerate the file after an intended change with
   (gumshoe.resize-books-test/write-golden!) and review its diff."
  (:require [babashka.cli :as cli]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [gumshoe.announce :as announce]
            [gumshoe.flow :as flow]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.storage :as storage]
            [gumshoe.verify :as verify]
            [gumshoe.watch :as watch]))

(def golden-path "tests/golden/resize_books.edn")

;; --- the fake cluster ----------------------------------------------------------------

(def ^:private plural
  {"statefulset" "statefulsets"
   "persistentvolumeclaim" "persistentvolumeclaims"})

(defn- parse-argv
  "A kubectl argv as {:flags {name value} :words [positional..]}."
  [argv]
  (reduce (fn [acc arg]
            (if-let [[_ k v] (re-matches #"--([^=]+)(?:=(.*))?" arg)]
              (assoc-in acc [:flags k] (or v true))
              (update acc :words conj arg)))
          {:flags {} :words []}
          (rest argv)))

(defn- labels-match?
  [selector object]
  (every? (fn [term]
            (let [[k v] (str/split term #"=" 2)]
              (= v (get-in object [:metadata :labels (keyword k)]))))
          (str/split selector #",")))

(defn- in-scope?
  "Whether object is one a kubectl call with these flags addresses."
  [{:strs [namespace selector all-namespaces]} object]
  (and (or all-namespaces (nil? namespace) (= namespace (get-in object [:metadata :namespace])))
       (or (nil? selector) (labels-match? selector object))))

(defn- named?
  [name object]
  (= name (get-in object [:metadata :name])))

(defn- deep-merge
  [a b]
  (if (and (map? a) (map? b)) (merge-with deep-merge a b) b))

(def ^:private unanswered
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"})

(defn- denied
  [what]
  {:exit 1 :out "" :err (str "Error from server (Forbidden): " what " is denied")})

(defn- answer-get
  [{:keys [objects answering? unanswered-types]} {:keys [flags words]}]
  (let [[_ type name] words
        found (filter #(in-scope? flags %) (get objects type []))]
    (cond
      (or (not answering?) (contains? unanswered-types type)) unanswered
      name (if-let [object (first (filter #(named? name %) found))]
             {:exit 0 :out (json/generate-string object) :err ""}
             {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" type name)})
      :else {:exit 0 :out (json/generate-string {:items found}) :err ""})))

(defn- fails?
  "Whether the scenario fails this mutation: :fail names [verb type name]
   triples, nil matching anything."
  [{:keys [fail]} verb type name]
  (some (fn [[v t n]] (and (= v verb) (or (nil? t) (= t type)) (or (nil? n) (= n name)))) fail))

(defn- patched
  [{:keys [resize?]} type object patch]
  (let [object (cond-> (deep-merge object patch)
                 (:spec patch) (update-in [:metadata :generation] (fnil inc 0)))]
    (if (and resize? (= "persistentvolumeclaims" type))
      (assoc-in object [:status :capacity :storage] (get-in object [:spec :resources :requests :storage]))
      object)))

(defn- mutate
  "The state after a mutation and the process map kubectl answers with."
  [state {:keys [flags words]} stdin]
  (let [[verb raw-type name] words
        type (get plural raw-type raw-type)]
    (case verb
      "patch"
      (if (fails? state "patch" type name)
        [state (denied (str "patching " type " " name))]
        [(update-in state [:objects type]
                    (fn [objects]
                      (mapv #(if (and (named? name %) (in-scope? flags %))
                               (patched state type % (json/parse-string (get flags "patch") true))
                               %)
                            objects)))
         {:exit 0 :out (str type "/" name " patched") :err ""}])

      "delete"
      (cond
        (fails? state "delete" type name)
        [state (denied (str "deleting " type))]

        (and name (not-any? #(and (in-scope? flags %) (named? name %)) (get-in state [:objects type])))
        [state {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" type name)}]

        :else
        [(update-in state [:objects type]
                    (fn [objects]
                      (vec (remove #(and (in-scope? flags %) (or (nil? name) (named? name %))) objects))))
         {:exit 0 :out (str type " deleted") :err ""}])

      "apply"
      (let [manifest (json/parse-string stdin true)
            name (get-in manifest [:metadata :name])]
        (if (fails? state "apply" "statefulsets" name)
          [state (denied (str "applying statefulsets " name))]
          [(update-in state [:objects "statefulsets"]
                      (fnil conj [])
                      (-> manifest
                          (assoc-in [:metadata :uid] (str "uid-" name "-recreated"))
                          (assoc-in [:metadata :generation] 1)
                          (assoc :status {:readyReplicas (if (:ready-after? state)
                                                           (get-in manifest [:spec :replicas])
                                                           0)})))
           {:exit 0 :out (str "statefulset.apps/" name " created") :err ""}]))

      [state {:exit 0 :out "" :err ""}])))

(defn- respond!
  "kubectl's answer to argv against the fake cluster in state, recording a
   mutation in log."
  [state log stdin argv]
  (let [parsed (parse-argv argv)
        verb (first (:words parsed))]
    (if (= "get" verb)
      (answer-get @state parsed)
      (let [[next-state result] (mutate @state parsed stdin)]
        (swap! log conj (cond-> {:argv (vec argv) :exit (:exit result)}
                          stdin (assoc :stdin (json/parse-string stdin true))))
        (reset! state (if (zero? (:exit result))
                        ((or (:settle next-state) identity) next-state)
                        next-state))
        result))))

;; --- running a book ------------------------------------------------------------------

(defn- load-config
  [path]
  (binding [runbook/*loading* true]
    (load-file path)))

(defn- attempt
  [f]
  (try (f) (catch Exception e (str "threw: " (ex-message e)))))

(defn- printable
  [x]
  (walk/postwalk #(if (fn? %) :fn %) x))

(defn transcript
  "What the book does in one scenario. state is the fake cluster ({:objects
   {type [objects]}} plus the scenario's switches), between is merged into
   it while the operator confirms, and argv is parsed by the book's options."
  [config {:keys [state argv between confirm? dry-run? preflight]
           :or {confirm? true}}]
  (let [opts (cli/parse-opts argv {:spec (:options config)})
        state (atom (merge {:answering? true :resize? true :ready-after? true} state))
        log (atom [])
        confirmations (atom [])
        announced (atom [])
        checks (atom [])
        record (outcome/fresh)
        eventually verify/eventually
        shell! (fn [stdin argv] (respond! state log stdin (vec argv)))
        out (java.io.StringWriter.)]
    (with-redefs-fn
      {#'storage/resize-preflights (atom (if preflight [preflight] []))
       #'watch/resize-watcher-builders (atom [])
       #'shell/execute (fn [& argv] (shell! nil argv))
       #'shell/execute-with-stdin (fn [stdin & argv] (shell! stdin argv))
       ;; run-with-output streams what the command prints, where execute
       ;; captures it
       #'shell/run-with-output (fn [& argv]
                                 (let [{:keys [exit out err]} (shell! nil argv)]
                                   (doseq [text [out err] :when (not (str/blank? text))]
                                     (println text))
                                   exit))
       #'kubectl/current-context (constantly "ctx")
       #'kubectl/current-cluster (constantly "cluster")
       #'fuzzy/select-single (constantly nil)
       #'interact/confirm! (fn [request]
                             (swap! confirmations conj (printable request))
                             (swap! state merge between)
                             confirm?)
       #'announce/announce! (fn [system _ message] (swap! announced conj [system message]) nil)
       #'verify/eventually (fn [check]
                             (swap! checks conj (-> check
                                                    (select-keys [:description :timeout :interval :soft?])
                                                    (assoc :watch? (some? (:watch check)))))
                             (eventually (assoc check :timeout 0)))}
      (fn []
        (let [result (binding [*out* out *err* out
                               outcome/*record* record
                               flow/*dry-run* (boolean dry-run?)]
                       (attempt (fn [] ((:action config) opts {:announcement-data nil}))))
              {:keys [tone text]} (outcome/banner {:changes? true
                                                   :dry-run? (boolean dry-run?)
                                                   :outcome (if (true? result) :ok :failed)}
                                                  @record)]
          (cond-> {:result result
                   :out (vec (remove str/blank? (str/split-lines (stdout/strip-colors (str out)))))
                   :banner text
                   :recovery (vec (outcome/recovery-lines tone @record))}
            (seq @confirmations) (assoc :confirmations @confirmations)
            (seq @announced) (assoc :announced @announced)
            (seq @log) (assoc :mutations @log)
            (seq @checks) (assoc :checks @checks)))))))

(defn surface
  [config]
  (printable
   {:description (:description config)
    :options (:options config)
    :prerequisites (:prerequisites config)
    :announce? (get config :announce? true)
    :details (vec (:details config))}))

;; --- the recorded clusters -----------------------------------------------------------

(defn- claim
  [namespace name class size]
  {:metadata {:namespace namespace :name name :uid (str "uid-" name)}
   :spec {:storageClassName class :resources {:requests {:storage size}}}
   :status {:capacity {:storage size}}})

(def ^:private storage-classes
  [{:metadata {:name "fast"} :allowVolumeExpansion true}
   {:metadata {:name "slow"} :allowVolumeExpansion false}])

(defn- template
  [name class size]
  {:metadata {:name name}
   :spec {:storageClassName class :accessModes ["ReadWriteOnce"] :resources {:requests {:storage size}}}})

(def ^:private web
  {:apiVersion "apps/v1"
   :kind "StatefulSet"
   :metadata {:namespace "shop" :name "web" :uid "uid-web" :generation 3 :resourceVersion "812"
              :creationTimestamp "2026-01-01T00:00:00Z" :managedFields [{:manager "kubectl"}]
              :labels {:app "web"}
              :annotations {(keyword "kubectl.kubernetes.io/last-applied-configuration") "{}" :team "shop"}}
   :spec {:replicas 2
          :selector {:matchLabels {:app "web"}}
          :volumeClaimTemplates [(template "data" "fast" "10Gi") (template "logs" "slow" "1Gi")]}
   :status {:replicas 2 :readyReplicas 2}})

(def ^:private cache
  {:metadata {:namespace "shop" :name "cache" :uid "uid-cache" :generation 1}
   :spec {:replicas 1}
   :status {:readyReplicas 1}})

(def ^:private shop
  {:objects {"statefulsets" [web cache]
             "persistentvolumeclaims" [(claim "shop" "data-web-0" "fast" "10Gi")
                                       (claim "shop" "data-web-1" "fast" "10Gi")
                                       (claim "shop" "logs-web-0" "slow" "1Gi")
                                       (claim "shop" "logs-web-1" "slow" "1Gi")
                                       (claim "other" "data-web-0" "fast" "10Gi")]
             "storageclasses" storage-classes
             "events" []}})

(defn- without-claim
  [cluster name]
  (update-in cluster [:objects "persistentvolumeclaims"]
             (fn [claims] (vec (remove #(and (= name (get-in % [:metadata :name]))
                                             (= "shop" (get-in % [:metadata :namespace])))
                                       claims)))))

(defn- bump-web
  [objects]
  (update objects "statefulsets"
          (fn [sets] (mapv #(if (= "web" (get-in % [:metadata :name])) (update-in % [:metadata :generation] inc) %) sets))))

(def ^:private enlarge-argv ["--namespace" "shop" "--statefulset" "web" "--template" "data" "--capacity" "20"])

(def ^:private prometheus-type "prometheuses.monitoring.coreos.com")

(def ^:private k8s
  {:apiVersion "monitoring.coreos.com/v1"
   :kind "Prometheus"
   :metadata {:namespace "monitoring" :name "k8s" :uid "uid-k8s" :generation 7}
   :spec {:replicas 2
          :storage {:volumeClaimTemplate {:spec {:storageClassName "fast"
                                                 :resources {:requests {:storage "10Gi"}}}}}}})

(defn- prometheus-pod
  [ordinal phase]
  {:metadata {:namespace "monitoring" :name (str "prometheus-k8s-" ordinal)
              :labels {(keyword "app.kubernetes.io/name") "prometheus"
                       (keyword "app.kubernetes.io/instance") "k8s"}}
   :spec {:volumes [{:name "config"}
                    {:name "prometheus-k8s-db"
                     :persistentVolumeClaim {:claimName (str "prometheus-k8s-db-prometheus-k8s-" ordinal)}}]}
   :status {:phase phase}})

(def ^:private monitoring
  {:objects {prometheus-type [k8s]
             "pods" [(prometheus-pod 1 "Running") (prometheus-pod 0 "Running")]
             "persistentvolumeclaims" [(claim "monitoring" "prometheus-k8s-db-prometheus-k8s-0" "fast" "10Gi")
                                       (claim "monitoring" "prometheus-k8s-db-prometheus-k8s-1" "fast" "10Gi")]
             "statefulsets" [{:metadata {:namespace "monitoring" :name "prometheus-k8s" :uid "uid-prometheus-k8s"
                                         :labels {(keyword "operator.prometheus.io/name") "k8s"}}
                              :spec {:replicas 2}}]
             "storageclasses" storage-classes
             "events" [{:type "Warning" :reason "VolumeResizeFailed"
                        :involvedObject {:kind "PersistentVolumeClaim" :name "prometheus-k8s-db-prometheus-k8s-0"}
                        :message "an old failure"}]}})

(def ^:private set-capacity-argv ["--namespace" "monitoring" "--prometheus" "k8s" "--capacity" "20Gi"])

(defn- pods-phase
  [phase]
  (fn [state]
    (update-in state [:objects "pods"] (fn [pods] (mapv #(assoc-in % [:status :phase] phase) pods)))))

;; --- the books and their scenarios ---------------------------------------------------

(def books
  {"runbooks/kubernetes/statefulsets/enlarge_volumes.clj"
   {:enlarged {:state shop :argv enlarge-argv}
    :resize-pending {:state (assoc shop :resize? false) :argv enlarge-argv}
    :pods-not-ready {:state (assoc shop :ready-after? false) :argv enlarge-argv}
    :first-claim-fails {:state (assoc shop :fail [["patch" "persistentvolumeclaims" "data-web-0"]]) :argv enlarge-argv}
    :second-claim-fails {:state (assoc shop :fail [["patch" "persistentvolumeclaims" "data-web-1"]]) :argv enlarge-argv}
    :delete-fails {:state (assoc shop :fail [["delete" "statefulsets" nil]]) :argv enlarge-argv}
    :recreate-fails {:state (assoc shop :fail [["apply" nil nil]]) :argv enlarge-argv}
    :dry-run {:state shop :argv enlarge-argv :dry-run? true}
    :declined {:state shop :argv enlarge-argv :confirm? false}
    :changed-while-confirming {:state shop :argv enlarge-argv
                               :between {:objects (bump-web (:objects shop))}}
    :deleted-while-confirming {:state shop :argv enlarge-argv
                               :between {:objects (assoc (:objects shop) "statefulsets" [cache])}}
    :unanswered-while-confirming {:state shop :argv enlarge-argv :between {:answering? false}}
    :not-expandable {:state shop :argv ["--namespace" "shop" "--statefulset" "web" "--template" "logs" "--capacity" "2Gi"]}
    :shrinking {:state shop :argv ["--namespace" "shop" "--statefulset" "web" "--template" "data" "--capacity" "5"]}
    :missing-claim {:state (without-claim shop "data-web-1") :argv enlarge-argv}
    :preflight-refuses {:state shop :argv enlarge-argv :preflight (fn [_] ["pool replicapool has 3Gi free, needs 20Gi"])}
    :no-templates {:state shop :argv ["--namespace" "shop" "--statefulset" "cache" "--capacity" "20"]}
    :unpicked-template {:state shop :argv ["--namespace" "shop" "--statefulset" "web" "--capacity" "20"]}
    :unpicked {:state shop :argv ["--namespace" "shop" "--statefulset" "nope" "--capacity" "20"]}
    :no-statefulsets {:state (assoc-in shop [:objects "statefulsets"] []) :argv enlarge-argv}
    :unanswered {:state (assoc shop :answering? false) :argv enlarge-argv}
    :claims-unanswered {:state (assoc shop :unanswered-types #{"persistentvolumeclaims"}) :argv enlarge-argv}}

   "tools/prometheus/runbooks/set_capacity.clj"
   {:resized {:state monitoring :argv set-capacity-argv}
    :resize-pending {:state (assoc monitoring :resize? false) :argv set-capacity-argv}
    :pods-not-running {:state (assoc monitoring :settle (pods-phase "Pending")) :argv set-capacity-argv}
    :pause-fails {:state (assoc monitoring :fail [["patch" prometheus-type nil]]) :argv set-capacity-argv}
    :second-claim-fails {:state (assoc monitoring :fail [["patch" "persistentvolumeclaims" "prometheus-k8s-db-prometheus-k8s-1"]])
                         :argv set-capacity-argv}
    :delete-fails {:state (assoc monitoring :fail [["delete" "statefulsets" nil]]) :argv set-capacity-argv}
    :unpause-fails {:state (assoc monitoring
                                  ;; the pause patch goes through; once the StatefulSet
                                  ;; is gone, a patch of the Prometheus is refused
                                  :settle (fn [state]
                                            (if (empty? (get-in state [:objects "statefulsets"]))
                                              (assoc state :fail [["patch" prometheus-type nil]])
                                              state)))
                    :argv set-capacity-argv}
    :dry-run {:state monitoring :argv set-capacity-argv :dry-run? true}
    :declined {:state monitoring :argv set-capacity-argv :confirm? false}
    :changed-while-confirming {:state monitoring :argv set-capacity-argv
                               :between {:objects (update (:objects monitoring) prometheus-type
                                                          (fn [ps] (mapv #(update-in % [:metadata :generation] inc) ps)))}}
    :not-expandable {:state (update-in monitoring [:objects "storageclasses"]
                                       (fn [classes] (mapv #(assoc % :allowVolumeExpansion false) classes)))
                     :argv set-capacity-argv}
    :shrinking {:state monitoring :argv ["--namespace" "monitoring" "--prometheus" "k8s" "--capacity" "5Gi"]}
    :preflight-refuses {:state monitoring :argv set-capacity-argv :preflight (fn [_] ["pool replicapool has 3Gi free, needs 20Gi"])}
    :no-pods {:state (assoc-in monitoring [:objects "pods"] []) :argv set-capacity-argv}
    :unpicked {:state monitoring :argv ["--namespace" "monitoring" "--prometheus" "nope" "--capacity" "20Gi"]}
    :no-prometheus {:state (assoc-in monitoring [:objects prometheus-type] []) :argv set-capacity-argv}
    :unanswered {:state (assoc monitoring :answering? false) :argv set-capacity-argv}}})

(defn transcripts
  []
  (into (sorted-map)
        (for [[path scenarios] books]
          (let [config (load-config path)]
            [path (assoc (into (sorted-map) (for [[k scenario] scenarios] [k (transcript config scenario)]))
                         :surface (surface config))]))))

(defn write-golden!
  []
  (spit golden-path
        ;; split, so a REUSE scan does not read these strings as this file's header
        (str ";; SPDX" "-FileCopyrightText: The gumshoe Authors\n"
             ";; SPDX" "-License-Identifier: 0BSD\n"
             ";; Generated by (gumshoe.resize-books-test/write-golden!); review its diff.\n"
             (with-out-str (pprint/pprint (transcripts))))))

(deftest every-book-keeps-its-transcripts
  (let [golden (edn/read-string (slurp golden-path))
        actual (transcripts)]
    (is (= (set (keys golden)) (set (keys actual))) "the same books")
    (doseq [[path scenarios] actual
            [scenario transcript] scenarios]
      (testing (str path " " scenario)
        (is (= (get-in golden [path scenario]) transcript))))))
