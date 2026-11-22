;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.node-exporter.facts
  "Reads every host's facts from node_exporter through PromQL - CPU count,
   memory, kernel, OS, boot time, whether its exporter is up - and compares
   the reporting hosts with a declared list when one is given. Read-only."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.hosts :as host-detectives]
            [gumshoe.hosts :as hosts]
            [gumshoe.promql :as promql]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def options
  (merge {:url {:desc "The query API base URL (e.g. https://prometheus.example.org); defaults to env.edn :prometheus :url"
                :default (config/env-value {} [:prometheus :url])
                :coerce :string}
          :flavor {:desc "prometheus, thanos (deduplicates, refuses partial answers) or mimir (/prometheus prefix, tenant header)"
                   :default "prometheus"
                   :coerce :string}
          :tenant {:desc "The Mimir tenant, sent as X-Scope-OrgID"
                   :coerce :string}
          :username {:desc "Basic auth username - resolved from the secret store when omitted"
                     :alias :u
                     :coerce :string}
          :password {:desc "Basic auth password - resolved from the secret store when omitted"
                     :alias :w
                     :coerce :string}
          :host-label {:desc "The label naming a host: instance (port stripped), node, hostname or nodename"
                       :default "instance"
                       :coerce :string}
          :selector {:desc "A label selector body narrowing every metric, e.g. cluster=\"staging\""
                     :alias :l
                     :coerce :string}
          :job {:desc "A regular expression naming the node_exporter scrape jobs, for the up check"
                :default "node|.*node-exporter.*"
                :coerce :string}
          :declared {:desc "A file of declared host names, one per line; # starts a comment"
                     :coerce :string}}
         detective/output-option))

(def prerequisites {})

(defn- facts
  [{:keys [url flavor tenant host-label selector job declared output] :as opts} _ctx]
  (detective/when-to-run! "Reach for this to count and size hosts from their metrics, or to find hosts that are declared but not scraped, and scraped but not declared.")
  (when-not url
    (throw (ex-info "no query URL - pass --url or set :prometheus :url in env.edn" {})))
  (when (and declared (not (fs/exists? declared)))
    (throw (ex-info (str "no declaration file " declared) {})))
  (let [ep (promql/endpoint (merge {:url url :flavor (keyword flavor) :tenant tenant}
                                   (promql/credentials url opts)))
        answers (hosts/read! ep {:host-label host-label :selector selector :job job})
        rows (hosts/rows host-label answers)
        evidence (cond-> {:now (java.time.Instant/now)
                          "source" url
                          "hosts" rows
                          "failed" (hosts/failures answers)}
                   declared (assoc "declared" (hosts/declared-hosts (slurp declared))))
        checked (detective/mark-unread host-detectives/detectives evidence)]
    (when (= "text" (or output "text"))
      (stdout/print-section (format "🖥️  Hosts (%d)" (count rows)))
      (stdout/print-table [["HOST" :host] ["CORES" :cores]
                           ["MEMORY GiB" #(some-> (:memory-bytes %) (/ 1073741824.0) (->> (format "%.1f")))]
                           ["KERNEL" :kernel] ["OS" #(str/trim (str (:os %) " " (:os-version %)))]
                           ["UP" #(case (:up %) true "yes" false "no" "?")]]
                          rows))
    (detective/report! checked (detective/run-detectives checked evidence) (or output "text")
                       {:source url :hosts rows})))

(runbook/execute!
 {:description "Reads host facts from node_exporter through PromQL and compares them with declared hosts"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action facts})
