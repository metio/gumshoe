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
            [gumshoe.effect :as effect]
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

(defn daemons
  [connection]
  (ceph-json connection "orch" "ps"))

(defn osds
  [connection]
  (:osds (ceph-json connection "osd" "dump")))

;; ---------------------------------------------------------------------------
;; upgrades

(defn version-strings
  "The distinct ceph version strings running across the whole cluster."
  [versions-json]
  (vec (map name (keys (:overall versions-json)))))

(defn all-on-version?
  "True when every daemon reports exactly the one target version."
  [versions-json target]
  (let [running (version-strings versions-json)]
    (and (= 1 (count running))
         (str/includes? (first running) (str target)))))

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
  (let [fetches (mapv (fn [[key command]]
                        (future [key (apply ceph-json connection command)]))
                      evidence-commands)]
    (into {:now (java.time.Instant/now)}
          (map deref)
          fetches)))
