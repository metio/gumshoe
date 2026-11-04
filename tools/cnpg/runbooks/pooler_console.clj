;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.pooler-console
  "Reads a Pooler's PgBouncer admin console and recent log, and says which of
   the two look-alike faults it is."
  (:require [gumshoe.detectives.pooler :as pooler]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.pooler-console :as console]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(def options
  {:namespace {:desc "Namespace of the pooler - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :pooler {:desc "Name of the pooler - interactive selection when omitted"
            :alias :p
            :coerce :string}
   :lines {:desc "How many log lines to read"
           :alias :l
           :default 400
           :coerce :long}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [pooler/pooler-type "pods"]})

(defn- admin-query
  [context namespace pod sql]
  (shell/stdout-of "kubectl" (str "--context=" context) (str "--namespace=" namespace)
                   "exec" pod "-c" "pgbouncer" "--"
                   "psql" "-p" "5432" "-U" "pgbouncer" "-d" "pgbouncer" "-c" sql))

(defn- recent-log
  [context namespace pod lines]
  (shell/stdout-of "kubectl" (str "--context=" context) (str "--namespace=" namespace)
                   "logs" pod "-c" "pgbouncer" (str "--tail=" lines)))

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
        poolers (kubectl/items-of (kubectl/get-all context pooler/pooler-type))
        candidates (mapv kubectl/namespace-name-of poolers)
        chosen (interact/choose-namespaced "Pooler" candidates
                                           (:namespace opts) (:pooler opts))]
    (if (nil? chosen)
      (do (stdout/error "no pooler selected") false)
      (let [{:keys [namespace name]} (kubectl/split-namespace-name chosen)
            pooler (kubectl/find-item poolers chosen)
            pod (pod-of context namespace name)]
        (if (nil? pod)
          (do (stdout/error (format "no pod found for pooler %s" chosen)) false)
          (do
            (stdout/print-data-table {:pooler chosen
                                      :cluster (-> pooler :spec :cluster :name)
                                      :pod pod})
            (doseq [sql ["SHOW DATABASES" "SHOW POOLS" "SHOW SERVERS"]]
              (stdout/print-section sql)
              (println (admin-query context namespace pod sql)))
            (let [log (recent-log context namespace pod (:lines opts))
                  verdict (console/classify log)]
              (stdout/print-section "Recent log, health checks removed")
              (doseq [line (take-last 20 (console/interesting-lines log))] (println line))
              (stdout/print-section "Verdict")
              (stdout/print-data-table (select-keys verdict [:verdict :authentication
                                                             :unreachable :lines]))
              (doseq [line (console/guidance verdict pooler)] (stdout/warn line))
              true)))))))

(runbook/execute!
 {:description "Reads a Pooler's PgBouncer admin console and says which fault the log shows"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action console})
