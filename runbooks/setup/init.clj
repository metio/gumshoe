;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.setup.init
  "Creates env.edn interactively, auto-detecting what it can. Every question is
   optional - skip any of them and the matching default behavior applies.

   It detects the Kubernetes cluster you are on and offers to make this an
   environment tied to it, so the ceph host and VPN you set here are selected
   automatically whenever you switch to that cluster. Run it once per cluster
   and the environments accumulate - staging keeps production's settings and
   vice versa."
  (:require [clojure.string :as str]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.config :as config]
            [gumshoe.discovery :as discovery]
            [gumshoe.inputs :as inputs]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.net :as net]
            [gumshoe.runbook :as runbook]
            [gumshoe.spec :as spec]
            [gumshoe.stdout :as stdout]))

(def prerequisites {})

(def ^:private skip-label "⏭  skip - leave this unset")

(defn- detect-cluster
  "The current kubectl cluster, or nil when there is none / kubectl is absent."
  []
  (try
    (let [cluster (kubectl/current-cluster)]
      (when-not (str/blank? (str cluster)) cluster))
    (catch Exception _ nil)))

(defn- ask-environment
  "When a cluster is detected, offers to name an environment tied to it.
   Returns the name, or nil to keep a flat single-environment config."
  [cluster]
  (when cluster
    (stdout/err-println (format "  detected Kubernetes cluster: %s" (stdout/bold cluster)))
    (stdout/err-println "  naming an environment ties this config to that cluster, so its ceph host")
    (stdout/err-println "  and VPN are selected automatically whenever you are on that context.")
    (interact/ask-text "Environment name"
                       "Environment name for this cluster, e.g. production or staging (Enter to skip):")))

(defn- ask-vpn-interface
  "Lets the operator pick a detected VPN interface, or skip. Returns nil when
   skipped or when nothing was detected."
  []
  (let [candidates (net/vpn-candidates)]
    (if (empty? candidates)
      (do (stdout/err-println "  no VPN interface detected - skipping (add one to env.edn by hand if needed)")
          nil)
      (let [choice (interact/choose-one "VPN interface" (conj (vec candidates) skip-label) nil)]
        (when (and choice (not= skip-label choice)) choice)))))

(defn- discover
  "Offers to pre-fill from a domain's public /.well-known/sre.json. Returns the
   discovered env.edn-shaped map, or nil when skipped or nothing is published."
  []
  (when-let [domain (interact/ask-text
                     "Discover"
                     (str "Pre-fill from a domain's public config? It reads\n"
                          "https://<domain>/.well-known/sre.json for the values that are public\n"
                          "(DNS server, Matrix homeserver, ...). Domain (e.g. example.org), or skip:"))]
    (if-let [discovered (discovery/fetch domain)]
      (do (stdout/ok (format "discovered public config from %s - values below are pre-filled, press Enter to accept" domain))
          discovered)
      (do (stdout/warn (format "%s publishes no /.well-known/sre.json - asking for everything" domain))
          nil))))

(defn- ask-inputs
  "Asks for each host input the config does not already set for this cluster.
   Only the missing ones are asked, so a first run offers them all (each
   skippable) while re-running after the registry grows asks only for what is
   new - env.edn migration by simply running the wizard again. A value the domain
   published is offered as the default, so Enter accepts it. Returns a seq of
   [env-path value] pairs for the ones the operator filled in."
  [existing active discovered]
  (let [missing (inputs/missing (or existing {}) active)]
    (when (seq missing)
      (stdout/print-section "🔌 Service inputs")
      (stdout/err-println (format "  %d input%s not set for this environment - fill any that apply, skip the rest:"
                                  (count missing) (if (= 1 (count missing)) "" "s"))))
    (->> missing
         (keep (fn [{:keys [prompt env-path]}]
                 (let [suggested (discovery/value discovered env-path)
                       answer (interact/ask-text prompt
                                                 (if suggested
                                                   (format "%s [%s] (Enter to accept):" prompt suggested)
                                                   (format "%s (Enter to skip):" prompt)))]
                   (when-let [v (or answer suggested)]
                     [env-path v]))))
         (vec))))

(defn- detect-capabilities
  "Probes the current cluster with every registered capability detector and
   reports what it found, so the environment is labelled with what it can do -
   the labels a book's :cluster-capabilities is checked against. Read-only."
  [cluster]
  (when cluster
    (stdout/err-println (format "  probing %s for capabilities (flux, cert-manager, cnpg, ...)" cluster))
    (let [found (capabilities/detect-present)]
      (if (seq found)
        (stdout/ok (format "detected: %s" (str/join ", " (map name found))))
        (stdout/err-println "  none detected - you can add :capabilities to env.edn by hand"))
      found)))

(defn- init
  [_opts _ctx]
  (let [cluster (detect-cluster)
        environment (ask-environment cluster)
        capabilities (detect-capabilities cluster)
        discovered (discover)
        existing (config/load-file-safely config/primary-path)
        active (config/active-environment {:kubernetes-cluster cluster})
        new-config (config/from-answers
                    {:environment environment
                     :kubernetes-cluster (when environment cluster)
                     :capabilities (when environment capabilities)
                     :vpn-interface (ask-vpn-interface)
                     :ceph-mgr-host (interact/ask-text "Ceph mgr host"
                                                       "Ceph mgr/admin host (Enter to skip):")
                     :inputs (ask-inputs existing active discovered)})
        merged (config/merge-answers (or existing {}) new-config)]
    (stdout/print-section "📝 env.edn")
    (if (empty? new-config)
      (do (stdout/ok "everything was skipped - nothing to write, all defaults apply")
          true)
      (do
        (print (config/render merged))
        ;; a validation pass over the result, so a wrong shape is flagged before
        ;; it is written and silently ignored at run time
        (when-let [issues (spec/env-config-problems merged)]
          (stdout/print-section "🔶 Heads up")
          (doseq [issue issues] (stdout/warn issue)))
        (if (interact/confirm! {:action (if existing "update this env.edn" "write this env.edn")
                                :target "this machine"
                                :items [config/primary-path]})
          (do (stdout/ok (format "wrote %s" (config/write! merged)))
              true)
          false)))))

(runbook/execute!
 {:description "Creates env.edn interactively - every question is optional"
  :prerequisites prerequisites
  :announce? false
  :action init})
