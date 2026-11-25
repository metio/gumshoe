;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ceph
  "Talking to a ceph cluster over SSH. By default it runs the ceph CLI
   directly, which is how it is used on a mgr or admin host - the keyring and
   tooling are already there, no sudo needed. For a host that only carries the
   cephadm bootstrap binary, set :cephadm-shell? on the connection and every
   command is wrapped in 'sudo cephadm shell -- ...' instead."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.effect :as effect]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]))

(defn ceph-args
  "Pure assembly of the remote ceph invocation for the connection's mode."
  [connection command-args]
  (if (:cephadm-shell? connection)
    (vec (concat ["sudo" "cephadm" "shell" "--"] command-args))
    (vec command-args)))

(defn ceph-stdout
  [connection & args]
  (apply ssh/stdout-of connection (ceph-args connection (cons "ceph" args))))

(defn ceph-json
  [connection & args]
  (let [output (apply ceph-stdout connection (concat args ["--format" "json"]))]
    (when-not (str/blank? output)
      (try (json/parse-string output true) (catch Exception _ nil)))))

(defn ceph-json-answer
  "{:ok? <the command answered> :doc <its parsed JSON> :exit .. :err ..} of
   a ceph command. :ok? is true only when it exited cleanly and its output
   parsed; :exit and :err say why a command that is not :ok? did not
   answer."
  [connection & args]
  (let [{:keys [exit out err]} (apply shell/execute (ssh/ssh-args connection (ceph-args connection (concat ["ceph"] args ["--format" "json"]))))
        doc (when-not (str/blank? out)
              (try (json/parse-string out true) (catch Exception _ nil)))]
    {:ok? (and (zero? exit) (some? doc)) :doc doc :exit exit :err err}))

(defn ceph-json-result
  "{:ok? <the command answered> :doc <its parsed JSON>} of a ceph command.
   :ok? is true only when it exited cleanly and its output parsed, so a check
   on an absence - no crash left - can tell an empty answer from none, which
   ceph-json returns as the same nil."
  [connection & args]
  (select-keys (apply ceph-json-answer connection args) [:ok? :doc]))

(defn ceph-stream!
  "Runs a ceph command streaming its output. Returns true on a clean exit."
  [connection & args]
  (apply ssh/stream! connection (ceph-args connection (cons "ceph" args))))

(defn effect
  "An effect that runs a ceph command over SSH, respecting the connection's
   mode - so ceph mutations dry-run and are tested like any other."
  [connection & args]
  (apply effect/ssh connection (ceph-args connection (cons "ceph" args))))

(def ssh-options
  "The CLI options every ceph book shares, so the invocation stays uniform."
  {:host {:desc "A ceph mgr or admin host to run ceph on"
          :alias :s
          :require true
          :coerce :string}
   :user {:desc "The SSH user - your ssh config decides when omitted"
          :alias :u
          :coerce :string}
   :cephadm-shell {:desc "Wrap ceph in 'sudo cephadm shell' - for hosts that only have the cephadm binary"
                   :coerce :boolean}})

(defn connection
  "Builds the ssh connection map from parsed options. cephadm-shell mode is
   the only one that needs sudo on the far end."
  [opts]
  {:host (:host opts)
   :user (:user opts)
   :cephadm-shell? (boolean (:cephadm-shell opts))
   :needs-sudo? (boolean (:cephadm-shell opts))})

(defn connect
  "The :connect of a ceph mutation spec (see gumshoe.mutation): the
   connection the flags name, checked before anything is listed. The
   confirmation names the host and the changelog names \"ceph (<host>)\".
   nil when the host cannot be reached, or sudo is needed and refused."
  [opts]
  (let [connection (connection opts)]
    (stdout/print-section "🔌 Connection")
    (when (ssh/check-connection? connection)
      {:context connection
       :cluster (:host opts)
       :announce-as (format "ceph (%s)" (:host opts))})))

;; ---------------------------------------------------------------------------
;; pure helpers over fetched ceph data

(defn daemon-named
  [daemons daemon-name]
  (first (filter #(= daemon-name (:daemon_name %)) daemons)))

(defn daemon-running?
  [daemon]
  (= "running" (:status_desc daemon)))

(defn osd-id
  "Accepts both '3' and 'osd.3'; nil for anything else."
  [value]
  (parse-long (str/replace (str value) #"^osd\." "")))

(defn osd-name
  [osd]
  (str "osd." (:osd osd)))

(defn osd-pick
  "An --osd value as the name the OSD candidates carry: 3 and osd.3 both
   become osd.3, anything else stays as given so the picker can show it."
  [value]
  (or (some->> value osd-id (str "osd.")) value))

(defn osd-numbered
  [osds id]
  (first (filter #(= id (:osd %)) osds)))

(defn osd-in? [osd] (= 1 (:in osd)))
(defn osd-up? [osd] (= 1 (:up osd)))

;; ---------------------------------------------------------------------------
;; fetching ceph state

(defn health-status
  [connection]
  (-> (ceph-json connection "status") :health :status))

(defn health-of
  "Pure: {:ok? <ceph status answered with a health status> :status <it>} of a
   ceph-json-result of `ceph status`. A command that did not answer, or a
   document without a health status, is not :ok?, so a check never reads an
   unread status as \"not HEALTH_ERR\"."
  [{:keys [ok? doc]}]
  (let [status (-> doc :health :status)]
    {:ok? (boolean (and ok? (string? status) (not (str/blank? status))))
     :status status}))

(defn health-result
  "The cluster's health status read over SSH, as health-of returns it."
  [connection]
  (health-of (ceph-json-result connection "status")))

(defn healthy-enough?
  "Pure: whether a health-result answered HEALTH_OK or HEALTH_WARN. An
   unread status and any other answer are not, so a post-check on it fails
   closed."
  [{:keys [ok? status]}]
  (boolean (and ok? (#{"HEALTH_OK" "HEALTH_WARN"} status))))

(defn upgrade-refusal
  "Pure: why an upgrade must not start on a health-result, or nil. An
   upgrade needs a health status that was read and is not HEALTH_ERR."
  [{:keys [ok? status]}]
  (cond
    (not ok?) "could not read the cluster's health - refusing to upgrade a cluster whose health is unknown"
    (= "HEALTH_ERR" status) "cluster is HEALTH_ERR - resolve that before upgrading"))

(defn change-refusal
  "Pure: why a change to a single daemon or OSD must not start on a
   health-result, or nil. It needs HEALTH_OK, or --force (forced?) for a
   status that was read and is anything else. An unread status refuses even
   under --force: forcing accepts a known degraded cluster, and a status
   nobody read is not known to be merely degraded."
  [{:keys [ok? status]} forced?]
  (cond
    (not ok?) "could not read the cluster's health - refusing to change a cluster whose health is unknown"
    (and (not= "HEALTH_OK" status) (not forced?)) (format "cluster is %s - resolve that first, or rerun with --force" status)))

(defn daemons
  [connection]
  (ceph-json connection "orch" "ps"))

(defn osds
  [connection]
  (:osds (ceph-json connection "osd" "dump")))

;; ---------------------------------------------------------------------------
;; crash reports

(defn unarchived-crashes
  "The crash reports `ceph crash ls-new` lists, read strictly: {:crashes
   [..]} when ceph answered, an empty vector when it answered that there are
   none, and {:unread <why>} when it did not answer or answered something
   that is no list of crash reports. A caller never reads an unanswered
   command as \"no crash left\"."
  [connection]
  (let [{:keys [ok? doc exit err]} (ceph-json-answer connection "crash" "ls-new")]
    (cond
      (and (some? exit) (not (zero? exit)))
      {:unread (str "ceph crash ls-new failed: " (or (not-empty (str/trim (str err))) (str "exit " exit)))}

      (not ok?)
      {:unread "ceph crash ls-new answered nothing a crash list parses from"}

      (not (sequential? doc))
      {:unread "ceph crash ls-new answered a document that is no list of crash reports"}

      :else {:crashes (vec doc)})))

(defn crashes-refusal
  "Why a change resting on an unarchived-crashes result must not run, or
   nil when ceph answered."
  [{:keys [unread]}]
  (when unread
    (str "could not read the unarchived crash reports - " unread)))

(defn archive-effects
  "One effect per crash report, archiving that report alone, so a report
   ceph lists after the operator confirmed is never archived unseen."
  [connection crashes]
  (mapv #(effect connection "crash" "archive" (:crash_id %)) crashes))

(defn archived?
  "Whether none of the confirmed crashes is listed unarchived in `listed`,
   an unarchived-crashes result. A read that did not answer proves nothing,
   and a report listed later than the confirmation is none of these."
  [confirmed {:keys [crashes unread]}]
  (and (nil? unread)
       (some? crashes)
       (let [ids (set (map :crash_id crashes))]
         (not-any? #(contains? ids (:crash_id %)) confirmed))))

;; ---------------------------------------------------------------------------
;; upgrades

(defn version-strings
  "The distinct ceph version strings running across the whole cluster."
  [versions-json]
  (vec (map name (keys (:overall versions-json)))))

(defn version-number
  "Pure: the version number of a ceph version string - 17.2.9 of
   \"ceph version 17.2.9 (abc) quincy (stable)\" - or nil when it names none."
  [version-string]
  (second (re-find #"^\s*ceph version (\S+)" (str version-string))))

(defn all-on-version?
  "True when every daemon reports exactly the one target version. The version
   number is compared whole, so 17.2.1 does not match a cluster on 17.2.10 and
   a short target such as 17.2 matches no release."
  [versions-json target]
  (let [running (version-strings versions-json)]
    (and (= 1 (count running))
         (some? (version-number (first running)))
         (= (version-number (first running)) (str/trim (str target))))))

(defn upgrade-in-progress?
  [upgrade-status-json]
  (true? (:in_progress upgrade-status-json)))

(defn upgrade-finished?
  "Whether an upgrade-status result reports a completed upgrade. A nil result
   means the status could not be read (a blank or unparseable response - common
   while `orch upgrade` restarts the very mgr daemons answering the query), not
   that the upgrade finished, so only a present, not-in-progress status counts."
  [upgrade-status-json]
  (and (some? upgrade-status-json)
       (not (upgrade-in-progress? upgrade-status-json))))

(defn versions
  [connection]
  (ceph-json connection "versions"))

(defn upgrade-status
  [connection]
  (ceph-json connection "orch" "upgrade" "status"))

(def evidence-commands
  "Everything the ceph detectives look at, fetched once per investigation."
  {"status" ["status"]
   "df" ["df"]
   "osd-df" ["osd" "df"]
   "orch-ls" ["orch" "ls"]
   "crash-ls-new" ["crash" "ls-new"]})

(defn collect-evidence!
  "Fetches all ceph evidence in parallel over SSH."
  [connection]
  (stdout/print-section "🔍 Evidence (ceph via SSH)")
  (doseq [[key command] (sort-by first evidence-commands)]
    (stdout/err-println (str "  " (stdout/blue "▸") " ceph " (str/join " " command) " (" key ")")))
  (into {:now (java.time.Instant/now)}
        (concurrent/map-all (fn [[key command]] [key (apply ceph-json connection command)])
                            evidence-commands)))
