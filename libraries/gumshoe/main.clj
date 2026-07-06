;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.main
  "The front doors, as functions. The repo-root scripts (./gumshoe, ./detect,
   ./run, ./investigate) are thin wrappers over these, and a casebook that has
   gumshoe as a dependency exposes the same doors through bb.edn :tasks:

     :tasks {gumshoe     {:task (exec 'gumshoe.main/front-door)}
             detect      {:task (exec 'gumshoe.main/detect)}
             run         {:task (exec 'gumshoe.main/run)}
             investigate {:task (exec 'gumshoe.main/investigate)}}

   Every book is named by a book-dir-relative suffix and resolved through the
   catalog, so a door launches the same book whether it lives under the working
   directory (the monorepo) or under ~/.gitlibs (a casebook's dependency). Each
   function returns an exit code; the caller decides whether to System/exit."
  (:require [babashka.cli :as cli]
            [clojure.string :as str]
            [gumshoe.catalog :as catalog]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(defn- launch
  "Runs a book by its catalog-resolved path, forwarding extra flags."
  [suffix & extra]
  (apply shell/run-with-output "bb" (catalog/resolve-path suffix) extra))

(defn- books-under
  "Every catalog book whose path sits under a book-root dir (playbooks, firebooks)
   - matched on the path segment so it works for absolute gitlibs paths too."
  [root]
  (filter #(str/includes? (:path %) (str "/" root "/")) (catalog/books)))

(defn- display
  [{:keys [name description]}]
  (if description (format "%s  —  %s" description name) name))

(def ^:private back-item "⬅ Back")

(defn- menu-choice
  "Selects one label from a guided menu. With `back?`, a '⬅ Back' row is offered
   and both choosing it and pressing ESC resolve to :back, so a submenu returns to
   the level above instead of exiting the whole flow. At the top level (`back?`
   false) ESC resolves to nil, which the caller treats as quit. An optional query
   prefills the fuzzy filter, so search terms still seed a launch."
  ([prompt labels back?] (menu-choice prompt labels back? nil))
  ([prompt labels back? query]
   (let [choice (fuzzy/select-single prompt (cond-> (vec labels) back? (conj back-item)) query)]
     (cond
       (= choice back-item) :back
       (and back? (nil? choice)) :back
       :else choice))))

;; ---------------------------------------------------------------------------
;; ./investigate - drill down from a lead

(defn investigate
  "Follows the thread from a symptom to a cause. All flags pass through to the
   investigate book (--host/--pod/--node/--namespace, or bare to be asked)."
  [args]
  (apply launch "runbooks/investigate.clj" args))

;; ---------------------------------------------------------------------------
;; ./run - fuzzy-launch any single book

(def ^:private run-spec
  {:spec {:dry-run {:desc "Pass --dry-run through to the chosen book" :coerce :boolean}
          :no-color {:desc "Disable colored output (also honors NO_COLOR)" :coerce :boolean}
          :help {:desc "Show this help message" :alias :h :coerce :boolean}}
   :restrict true
   :error-fn (fn [{:keys [msg]}]
               (stdout/error msg)
               (stdout/err-println "run with --help to see the available flags")
               (System/exit 1))})

(defn- passthrough
  [opts]
  (cond-> []
    (:dry-run opts) (conj "--dry-run")
    (:no-color opts) (conj "--no-color")))

(defn run
  "Launches any single book by fuzzy search. Search terms prefill the filter.
   `back?` (set when reached from the guided menu) offers a Back row and returns
   :back so the caller can step up a level."
  ([args] (run args false))
  ([args back?]
   (let [{:keys [opts args]} (cli/parse-args args run-spec)]
     (when (:no-color opts) (stdout/disable-colors!))
     (if (:help opts)
       (do (stdout/err-println (stdout/bold "run - launch any single book by fuzzy search"))
           (stdout/err-println "  [search terms] [--dry-run] [--no-color]")
           0)
       (let [books (catalog/books)
             choice (menu-choice "Which book?" (mapv display books) back? (str/join " " args))]
         (cond
           (= :back choice) :back
           :else
           (let [book (first (filter #(= choice (display %)) books))]
             (if (nil? book)
               (do (stdout/error "nothing selected - try again when you know the book you want") 1)
               (apply shell/run-with-output "bb" (:path book) (passthrough opts))))))))))

;; ---------------------------------------------------------------------------
;; ./detect - guided "what hurts?" scan of an area

(def investigations
  [{:label "🔦 Drill down - follow the thread from a hostname, namespace, node, or pod"
    :book "runbooks/investigate.clj"}
   {:label "🌍 Everything - the full cluster investigation"
    :book "runbooks/detectives/cluster.clj"}
   {:label "🧨 Workloads - crash loops, missing replicas, failed jobs, storage"
    :book "runbooks/detectives/workloads.clj"}
   {:label "🧱 Platform - nodes, calico network, CSI storage layer"
    :book "runbooks/detectives/platform.clj"}
   {:label "🔐 Security - RBAC, pod security, network policies"
    :book "runbooks/detectives/security.clj"}
   {:label "🌐 DNS - nameservers, replication, dual stack"
    :book "runbooks/detectives/dns.clj"}
   {:label "📡 external-dns - do all declared hostnames resolve?"
    :book "tools/external-dns/runbooks/scan.clj"}
   {:label "🔀 Traffic - Gateway API gateways and routes"
    :book "tools/gateway/runbooks/scan.clj"}
   {:label "📧 Mail - deliverability (SPF/DKIM/DMARC/rDNS) and SMTP/POP3/IMAP services"
    :book "runbooks/detectives/mail.clj"}
   {:label "📤 Upload limits - the nginx + php-fpm chain for a Moodle/Nextcloud app"
    :book "tools/upload/runbooks/scan.clj"}
   {:label "🐘 Databases - CloudNativePG and db-operator"
    :book "runbooks/detectives/databases.clj"}
   {:label "🔒 TLS - cert-manager certificates and ACME orders"
    :book "tools/certmanager/runbooks/scan.clj"}
   {:label "🔁 GitOps - flux sources and reconciliations"
    :book "tools/flux/runbooks/gitops.clj"}
   {:label "🎬 Delivery - StageSet staged rollouts"
    :book "tools/stageset/runbooks/scan.clj"}
   {:label "📈 Observability - the prometheus-operator stack"
    :book "tools/prometheus/runbooks/scan.clj"}
   {:label "⚡ Events - Warning events from the last hour"
    :book "runbooks/detectives/events.clj"}
   {:label "📊 Thanos - the query layer: readiness, stores, rules (port-forward)"
    :book "tools/thanos/runbooks/scan.clj"}
   {:label "📜 Loki - readiness and component ring health (port-forward)"
    :book "tools/loki/runbooks/scan.clj"}
   {:label "💬 Matrix - client/federation APIs, signing keys, delegation"
    :book "tools/matrix/runbooks/scan.clj"}
   {:label "💾 Restic - are backups actually landing? (asks for repositories)"
    :book "tools/restic/runbooks/scan.clj" :needs-repositories? true}
   {:label "🐙 Ceph - the storage cluster itself (asks for a host)"
    :book "tools/ceph/runbooks/scan.clj" :needs-host? true}
   {:label "🌌 OpenNebula - hosts, VMs, datastores (asks for the frontend)"
    :book "tools/opennebula/runbooks/scan.clj" :needs-frontend? true}])

(def ^:private detect-spec
  {:spec {:dry-run {:desc "Pass --dry-run through to the chosen book" :coerce :boolean}
          :no-color {:desc "Disable colored output (also honors NO_COLOR)" :coerce :boolean}
          :help {:desc "Show this help message" :alias :h :coerce :boolean}}
   :restrict true
   :error-fn (fn [{:keys [msg]}]
               (stdout/error msg)
               (stdout/err-println "run with --help to see the available flags")
               (System/exit 1))})

(defn- ask
  [prompt]
  (binding [*out* *err*] (print prompt) (flush))
  (some-> (read-line) str/trim not-empty))

(defn detect
  "Starts an investigation when something hurts: pick an area, launch its scan.
   `back?` (set when reached from the guided menu) offers a Back row and returns
   :back so the caller can step up a level."
  ([args] (detect args false))
  ([args back?]
   (let [opts (cli/parse-opts args detect-spec)]
     (when (:no-color opts) (stdout/disable-colors!))
     (if (:help opts)
       (do (stdout/err-println (stdout/bold "detect - start an investigation when something hurts"))
           (doseq [{:keys [label]} investigations] (stdout/err-println (str "  " label)))
           0)
       (let [extra (passthrough opts)
             choice (menu-choice "What hurts?" (mapv :label investigations) back?)]
         (if (= :back choice)
           :back
           (let [{:keys [book needs-host? needs-frontend? needs-repositories?]}
                 (first (filter #(= choice (:label %)) investigations))]
             (cond
               (nil? book)
               (do (stdout/error "nothing selected - run again when it hurts") 1)

               needs-host?
               (if-let [host (ask "Which ceph host should I SSH into? ")]
                 (apply launch book "--host" host extra)
                 (do (stdout/error "no host given") 1))

               needs-frontend?
               (if-let [frontend (ask "Which OpenNebula frontend should I SSH into? ")]
                 (apply launch book "--frontend" frontend extra)
                 (do (stdout/error "no frontend given") 1))

               needs-repositories?
               (if-let [repository (ask "Which restic repository should I check? ")]
                 (apply launch book "--repository" repository extra)
                 (do (stdout/error "no repository given") 1))

               :else
               (apply launch book extra)))))))))

;; ---------------------------------------------------------------------------
;; ./gumshoe - the one front door

(def ^:private gumshoe-spec
  {:spec {:host {:desc "Drill down from a hostname a user reported" :coerce :string}
          :pod {:desc "Drill down from a pod (namespace/name)" :coerce :string}
          :node {:desc "Drill down from a node" :coerce :string}
          :namespace {:desc "Drill down from a namespace" :alias :n :coerce :string}
          :help {:desc "Show this help message" :alias :h :coerce :boolean}}
   :restrict true
   :error-fn (fn [{:keys [msg]}]
               (stdout/error msg)
               (stdout/err-println "run --help for the shortcuts, or with no arguments for the guided menu")
               (System/exit 1))})

(defn- lead-args
  [opts]
  (cond-> []
    (:host opts) (conj "--host" (:host opts))
    (:pod opts) (conj "--pod" (:pod opts))
    (:node opts) (conj "--node" (:node opts))
    (:namespace opts) (conj "--namespace" (:namespace opts))))

(def ^:private follow-lead "🔦 Follow a lead - a hostname, pod, node, or namespace you suspect")
(def ^:private scan-area "🩺 Scan for symptoms - what hurts, across an area of the cluster")
(def ^:private run-book "📖 Run a book - a single action, you already know which")
(def ^:private run-playbook "📋 Run a playbook - a multi-step procedure, start to finish")
(def ^:private fire-drill "🔥 Practice a fire drill - break something on purpose to train the team")

(defn- pick-and-run
  "Picks and launches one book under a root (playbooks/firebooks). `back?` (set
   when reached from the guided menu) offers a Back row and returns :back."
  ([prompt root] (pick-and-run prompt root false))
  ([prompt root back?]
   (let [books (books-under root)]
     (if (empty? books)
       (do (stdout/error (format "no %s found" root)) 1)
       (let [choice (menu-choice prompt (mapv display books) back?)]
         (if (= :back choice)
           :back
           (let [book (first (filter #(= choice (display %)) books))]
             (if book (shell/run-with-output "bb" (:path book)) 1))))))))

(defn- guided
  []
  ;; The top menu loops: a submenu that returns :back (its Back row or ESC) re-
  ;; shows this menu instead of exiting, so a wrong turn costs one keypress. ESC
  ;; here, at the top, quits. condp/= compares against the def'd label strings;
  ;; `case` would match the literal symbols (it does not evaluate its test
  ;; constants) and never hit, throwing "No matching clause" on every selection.
  (loop []
    (let [result (condp = (menu-choice "what are you doing?"
                                       [follow-lead scan-area run-book run-playbook fire-drill]
                                       false)
                   nil 1
                   follow-lead (investigate [])
                   scan-area (detect [] true)
                   run-book (run [] true)
                   run-playbook (pick-and-run "which playbook?" "playbooks" true)
                   fire-drill (pick-and-run "which fire drill?" "firebooks" true))]
      (if (= :back result) (recur) result))))

(defn front-door
  "The guided menu, a lead shortcut, or a fuzzy book search - the one door."
  [args]
  (let [{:keys [opts args]} (cli/parse-args args gumshoe-spec)]
    (cond
      (:help opts)
      (do (stdout/err-println (stdout/bold "gumshoe - the SRE detective"))
          (stdout/err-println "  gumshoe                     the guided menu")
          (stdout/err-println "  gumshoe --host <hostname>   drill down from a lead (also --pod/--node/--namespace)")
          (stdout/err-println "  gumshoe <search terms>      fuzzy-launch a book by name")
          0)
      (some opts [:host :pod :node :namespace]) (investigate (lead-args opts))
      (seq args) (run args)
      :else (guided))))
