;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.pooler-console
  "Reads a Pooler's PgBouncer admin console and recent log, and says which of
   the two look-alike faults it is."
  (:require [gumshoe.detectives.pooler :as pooler]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.pooler-console :as console]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]))

(def pooler-select
  "The pooler to open. Its candidates come from the run's one read of the
   poolers, so the action adds them."
  {:mode :namespaced :label "Pooler" :namespace-flag :namespace :name-flag :pooler
   :empty-message "no CloudNativePG Pooler exists"
   :on-empty :fail})

(def options
  (select/options pooler-select
                  {:namespace {:desc "Namespace of the pooler - interactive selection when omitted"}
                   :pooler {:desc "Name of the pooler - interactive selection when omitted"}
                   :lines {:desc "How many log lines to read"
                           :alias :l
                           :default 400
                           :coerce :long}}))

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [pooler/pooler-type "pods"]})

(defn- admin-query
  "The answer of one admin console command, nil unless the exec exited 0."
  [context namespace pod sql]
  (kubectl/exec-answer context namespace pod "pgbouncer"
                       ["psql" "-p" "5432" "-U" "pgbouncer" "-d" "pgbouncer" "-c" sql]))

(defn- pod-of
  [context namespace pooler]
  (->> (kubectl/items-of (kubectl/get-selected context "pods"
                                               (str "cnpg.io/poolerName=" pooler)))
       (filter #(= namespace (kubectl/namespace-of %)))
       first
       kubectl/name-of))

(defn- console
  [opts _ctx]
  (let [context (kubectl/current-context)
        poolers (delay (kubectl/items-of (kubectl/get-all context pooler/pooler-type)))
        chosen (select/pick (assoc pooler-select :candidates (fn [_] (mapv kubectl/namespace-name-of @poolers)))
                            opts context)]
    (if (nil? chosen)
      false
      (let [{:keys [namespace name]} (kubectl/namespace-name-map chosen)
            pooler (kubectl/find-item @poolers chosen)
            pod (pod-of context namespace name)]
        (if (nil? pod)
          (do (stdout/error (format "no pod found for pooler %s" chosen)) false)
          (do
            (stdout/print-data-table {:pooler chosen
                                      :cluster (-> pooler :spec :cluster :name)
                                      :pod pod})
            (doseq [sql ["SHOW DATABASES" "SHOW POOLS" "SHOW SERVERS"]]
              (stdout/print-section sql)
              (if-let [answer (admin-query context namespace pod sql)]
                (println answer)
                (stdout/warn (format "could not run %s on the admin console of %s" sql pod))))
            (let [{:keys [read? log]} (kubectl/container-logs context namespace pod "pgbouncer" (:lines opts))]
              (if-not read?
                (do (stdout/error (format "could not read the pgbouncer log of %s - no verdict without it" pod))
                    false)
                (let [verdict (console/classify log)]
                  (stdout/print-section "Recent log, health checks removed")
                  (doseq [line (take-last 20 (console/interesting-lines log))] (println line))
                  (stdout/print-section "Verdict")
                  (stdout/print-data-table (select-keys verdict [:verdict :authentication
                                                                 :unreachable :lines]))
                  (doseq [line (console/guidance verdict pooler)] (stdout/warn line))
                  true)))))))))

(runbook/execute!
 {:description "Reads a Pooler's PgBouncer admin console and says which fault the log shows"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action console})
