;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.collect-books-test
  "The scans whose evidence comes from somewhere other than the cluster's
   resource reads: DNS, mail, SSH, HTTP APIs, PromQL, port-forwards. Each book
   is loaded for checking, so its action runs against stubbed collectors and
   never contacts anything.

   The golden file pins what each book reports for a fixed collection: its exit
   value, its stdout and stderr, the collector calls it made and the evidence
   its detectives were handed. Set GUMSHOE_RECORD_GOLDEN=1 to rewrite it."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.calico.flows :as flows]
            [gumshoe.ceph :as ceph]
            [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.dns :as dns]
            [gumshoe.evpn :as evpn]
            [gumshoe.external-dns :as external-dns]
            [gumshoe.hosts :as hosts]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.loki :as loki]
            [gumshoe.mail :as mail]
            [gumshoe.opennebula :as opennebula]
            [gumshoe.phpstack :as phpstack]
            [gumshoe.promql :as promql]
            [gumshoe.restic :as restic]
            [gumshoe.runbook :as runbook]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.secrets :as secrets]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]
            [gumshoe.synapse :as synapse]
            [gumshoe.thanos :as thanos]))

(def ^:private golden-path "tests/gumshoe/collect_books_golden.edn")

(def ^:private calls (atom []))

(defn- recording
  "A stub that records its name and arguments and answers with value."
  [label value]
  (fn [& args]
    (swap! calls conj (into [label] args))
    value))

(defn- load-book
  [path]
  (with-redefs [kubectl/current-cluster (constantly nil)
                config/env-value (fn [& _] nil)]
    (binding [runbook/*loading* true]
      (load-file path))))

(defn- finding-of
  [detective]
  {:severity :warning :component "ns/subject" :summary "something to look at" :detective detective})

(defn- run-book
  "Runs a book's action with opts under stubs, the detectives answering with
   findings (nil runs the real ones). Returns what it printed and returned."
  [path opts stubs findings]
  (reset! calls [])
  (let [{:keys [action]} (load-book path)
        err (java.io.StringWriter.)
        result (atom nil)
        detect (fn [detectives evidence]
                 (swap! calls conj [:detectives (mapv :name detectives)
                                    (into (sorted-map) (dissoc evidence "options" :now))])
                 (or findings []))
        out (with-redefs-fn (merge {#'detective/interactive? (constantly false)
                                    #'kubectl/current-cluster (constantly nil)
                                    #'kubectl/current-context (constantly "ctx")
                                    #'config/env-value (fn [& _] nil)
                                    #'config/active-config (constantly {:config :active})}
                                   (when findings {#'detective/run-detectives detect})
                                   stubs)
              (fn []
                (binding [*err* err]
                  (with-out-str (reset! result (action opts nil))))))
        output (:output opts "text")]
    {:result @result
     :stdout (if (= "edn" output) (edn/read-string out) (stdout/strip-colors out))
     :stderr (stdout/strip-colors (str err))
     :calls @calls}))

(def ^:private node-exporter-rows [{:host "w1" :cores 4 :memory-bytes 8589934592 :kernel "6.1" :os "Debian" :os-version "12" :up true}])

(def ^:private loki-service
  {:metadata {:name "loki-distributor" :namespace "loki"
              :labels {(keyword "app.kubernetes.io/component") "distributor"}}})

(defn- candidate-reads
  "Stubs that put the real candidate read and pick back in place over a
   cluster answering kubectl with answer, the operator picking nothing."
  [answer readers]
  (merge (into {} (for [v readers] [v @v]))
         {#'shell/execute (fn [& _] answer)
          #'fuzzy/select-single (constantly nil)}))

(def ^:private kubectl-unanswered
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"})

(def ^:private kubectl-empty
  {:exit 0 :out "{\"items\": []}" :err ""})

(def scenarios
  "Every scan with its stubs. :evidence is what collection hands the
   detectives, shaped as the real collector returns it."
  {"dns" {:path "runbooks/detectives/dns.clj"
          :opts {:server "192.0.2.53" :zone "example.org" :probes ["www.example.org"]}
          :stubs (fn [evidence] {#'dns/collect-evidence! (recording :collect evidence)})
          :evidence {"server" "192.0.2.53" "zone" "example.org" "transports" [] "nameservers" [] "addresses" {} "serials" [] "probes" []}
          :detective "dns-transports"}
   "mail" {:path "runbooks/detectives/mail.clj"
           :opts {:domain "example.org" :host "mx.example.org" :server "192.0.2.53" :dkim-selectors ["default"]}
           :stubs (fn [evidence] {#'mail/collect-evidence! (recording :collect evidence)})
           :evidence {"domain" "example.org" "host" "mx.example.org" "mx" [] "spf" [] "dmarc" [] "dkim" {} "ptr" [] "probes" []}
           :detective "mail-mx"}
   "fabric" {:path "runbooks/detectives/fabric.clj"
             :opts {:hosts "h1,h2" :vni "100"}
             :stubs (fn [evidence] {#'evpn/connections (recording :connections [:conns])
                                    #'evpn/collect-evidence! (recording :collect evidence)})
             :evidence {"evpn-macs" [] "evpn-fdb" [] "evpn-hosts" []}
             :detective "evpn-hosts"
             :refusals {"no hosts" {:opts {:vni "100"}}
                        "no vnis" {:opts {:hosts "h1"}}}}
   "flows" {:path "tools/calico/runbooks/flows.clj"
            :opts {:whisker "https://whisker.example.org" :window 600}
            :stubs (fn [evidence] {#'secrets/find-path (constantly nil)
                                   #'flows/collect-evidence! (recording :collect evidence)})
            :evidence {:url "https://whisker.example.org" :mode :list :window 600 :reachable true :status 200 :flows []}
            :detective "calico-denied-flows"}
   "ceph" {:path "tools/ceph/runbooks/scan.clj"
           :opts {:host "mgr1"}
           :stubs (fn [evidence] {#'ssh/check-connection? (recording :check-connection true)
                                  #'ceph/collect-evidence! (recording :collect evidence)})
           :evidence {"status" {} "df" {} "osd-df" {} "orch-ls" [] "crash-ls-new" []}
           :detective "ceph-health"
           :refusals {"no connection" {:stubs {#'ssh/check-connection? (recording :check-connection false)}}}}
   "external-dns" {:path "tools/external-dns/runbooks/scan.clj"
                   :opts {:server "192.0.2.53"}
                   :stubs (fn [evidence] {#'external-dns/collect-evidence! (recording :collect evidence)})
                   :evidence {"sources" [] "resolved" {} "server" "192.0.2.53" "ingresses" []}
                   :detective "external-dns"}
   "loki" {:path "tools/loki/runbooks/scan.clj"
           :opts {:namespace "loki" :selector "app.kubernetes.io/name=loki" :port-name "http-metrics" :port 13100}
           :stubs (fn [evidence] {#'kubectl/get-selected (recording :get-selected {:items [loki-service]})
                                  #'kubectl/with-port-forward (fn [spec f] (swap! calls conj [:port-forward spec]) (f))
                                  #'kubectl/service-port-or-first (constantly 3100)
                                  #'loki/collect-workloads! (recording :collect-workloads (dissoc evidence "rings" "url"))
                                  #'loki/collect-rings! (recording :collect-rings (select-keys evidence ["rings" "url"]))})
           :evidence {"namespace" "loki" "workloads" [] "rings" {} "url" "http://localhost:13100"}
           :detective "loki-components"
           :refusals {"no namespace" {:opts {:selector "x"}
                                      :stubs {#'kubectl/get-selected (recording :get-selected {:items []})
                                              #'interact/choose-one (recording :choose-one nil)}}
                      "no distributor" {:stubs {#'kubectl/get-selected (recording :get-selected {:items []})}}
                      "candidates unanswered" {:opts {:selector "x"}
                                               :stubs (candidate-reads kubectl-unanswered [#'kubectl/get-selected #'interact/choose-one])}
                      "no candidates" {:opts {:selector "x"}
                                       :stubs (candidate-reads kubectl-empty [#'kubectl/get-selected #'interact/choose-one])}}}
   "matrix" {:path "tools/matrix/runbooks/scan.clj"
             :opts {:domain "example.org" :host "matrix.example.org" :admin-secret "matrix/admin"}
             :stubs (fn [evidence] {#'secrets/password (recording :password "token")
                                    #'synapse/collect-evidence! (recording :collect evidence)})
             :evidence {"domain" "example.org" "host" "matrix.example.org" "admin?" true "client-versions" {} "health" "OK"
                        "federation-version" {} "server-keys" {} "wellknown-server" {} "wellknown-client" {} "destinations" []}
             :detective "matrix-health"}
   "node-exporter" {:path "tools/node-exporter/runbooks/facts.clj"
                    :opts {:url "https://prometheus.example.org" :flavor "prometheus" :host-label "instance" :job "node"
                           :username "u" :password "p"}
                    :stubs (fn [evidence] {#'hosts/read! (recording :collect :answers)
                                           #'hosts/rows (constantly (get evidence "hosts"))
                                           #'hosts/failures (constantly (get evidence "failed"))})
                    :evidence {"source" "https://prometheus.example.org" "hosts" node-exporter-rows "failed" []}
                    :detective "host-facts-complete"}
   "opennebula" {:path "tools/opennebula/runbooks/scan.clj"
                 :opts {:frontend "one.example.org"}
                 :stubs (fn [evidence] {#'ssh/check-connection? (recording :check-connection true)
                                        #'opennebula/collect-evidence! (recording :collect evidence)})
                 :evidence {"hosts" [] "vms" [] "datastores" []}
                 :detective "opennebula-vms"
                 :refusals {"no frontend" {:opts {}}
                            "no connection" {:stubs {#'ssh/check-connection? (recording :check-connection false)}}}}
   "restic" {:path "tools/restic/runbooks/scan.clj"
             :opts {:repository ["s3:backup"] :password-secret "restic/pw" :warn-days 2 :critical-days 7}
             :stubs (fn [evidence] {#'secrets/password (recording :password "pw")
                                    #'restic/collect-evidence! (recording :collect evidence)})
             :evidence {"repositories" []}
             :detective "restic-backups"
             :refusals {"warn not below critical" {:opts {:repository ["s3:backup"] :password-secret "restic/pw" :warn-days 7 :critical-days 7}}
                        "no password" {:stubs {#'secrets/password (recording :password nil)}}}}
   "thanos" {:path "tools/thanos/runbooks/scan.clj"
             :opts {:selector "app.kubernetes.io/name=thanos-query" :port-name "http" :port 19090}
             :stubs (fn [evidence] {#'kubectl/get-selected (recording :get-selected {:items [{:metadata {:namespace "monitoring" :name "thanos-query"}}]})
                                    #'interact/choose-namespaced (recording :choose "monitoring/thanos-query")
                                    #'kubectl/find-item (constantly {})
                                    #'kubectl/service-port-or-first (constantly 9090)
                                    #'kubectl/with-port-forward (fn [spec f] (swap! calls conj [:port-forward spec]) (f))
                                    #'thanos/collect-evidence! (recording :collect evidence)})
             :evidence {"url" "http://localhost:19090" "ready" true "stores" [] "rules" []}
             :detective "thanos-stores"
             :refusals {"no service" {:stubs {#'interact/choose-namespaced (recording :choose nil)}}
                        "candidates unanswered" {:stubs (candidate-reads kubectl-unanswered [#'kubectl/get-selected #'interact/choose-namespaced #'kubectl/find-item])}
                        "no candidates" {:stubs (candidate-reads kubectl-empty [#'kubectl/get-selected #'interact/choose-namespaced #'kubectl/find-item])}}}
   "upload" {:path "tools/upload/runbooks/scan.clj"
             :opts {:namespace "moodle"}
             :stubs (fn [evidence] {#'kubectl/get-all (constantly {:items []})
                                    #'kubectl/names-of (constantly ["moodle"])
                                    #'interact/choose-one (recording :choose-one "moodle")
                                    #'phpstack/collect-evidence! (recording :collect evidence)})
             :evidence {:component "moodle" :found-nginx? true :found-php? true :nginx-explicit? true
                        :nginx-body-size 1048576 :upload-max 1048576 :post-max 1048576 :memory-limit 134217728}
             :detective "upload-limits"
             :refusals {"no namespace" {:stubs {#'interact/choose-one (recording :choose-one nil)}}
                        "candidates unanswered" {:stubs (candidate-reads kubectl-unanswered [#'kubectl/get-all #'kubectl/names-of #'interact/choose-one])}
                        "no candidates" {:stubs (candidate-reads kubectl-empty [#'kubectl/get-all #'kubectl/names-of #'interact/choose-one])}
                        "no containers" {:stubs {#'phpstack/collect-evidence! (recording :collect {:component "moodle" :found-nginx? false :found-php? false})}}
                        "nginx only" {:stubs {#'phpstack/collect-evidence! (recording :collect {:component "moodle" :found-nginx? true :found-php? false
                                                                                                :nginx-body-size 1048576})}}}}})

(defn- outcomes
  "Every scenario of every book: a finding and a clean run in text and edn,
   and each refusal in text."
  []
  (into (sorted-map)
        (for [[book {:keys [path opts stubs evidence detective refusals]}] scenarios]
          [book
           (into (sorted-map)
                 (concat
                  (for [[label findings] [["finding" [(finding-of detective)]] ["clean" []]]
                        output ["text" "edn"]]
                    [(str label " " output) (run-book path (assoc opts :output output) (stubs evidence) findings)])
                  (for [[label refusal] refusals]
                    [label (run-book path (assoc (:opts refusal opts) :output "text")
                                     (merge (stubs evidence) (:stubs refusal)) [])])))])))

(deftest every-collect-book-reports-as-pinned
  (let [actual (outcomes)]
    (if (System/getenv "GUMSHOE_RECORD_GOLDEN")
      (spit golden-path (str ";; SPDX-" "FileCopyrightText: The gumshoe Authors\n"
                             ";; SPDX-" "License-Identifier: 0BSD\n\n"
                             (with-out-str (pprint/pprint actual))))
      (let [golden (edn/read-string (slurp golden-path))]
        (doseq [[book scenarios] golden
                [scenario expected] scenarios]
          (testing (str book " / " scenario)
            (is (= expected (get-in actual [book scenario])))))
        (is (= (set (keys golden)) (set (keys actual))) "every book has a golden entry")))))

(deftest a-scan-whose-collection-read-nothing-checked-nothing
  ;; The upload scan refuses before its detectives run when no container
  ;; answered, so its collection never reaches a report empty.
  (doseq [[book {:keys [path opts stubs evidence]}] (dissoc scenarios "upload")
          :let [unread (update-vals evidence (constantly nil))
                {:keys [result stdout]} (run-book path (assoc opts :output "edn")
                                                  (merge (stubs unread)
                                                         {#'config/active-config (constantly nil)})
                                                  [])]]
    (testing book
      (is (false? result) "a scan that judged nothing fails")
      (is (true? (:nothing-checked stdout)))
      (is (every? :unread (:checked stdout)) "every check is reported as having read nothing"))))

(defn- disruption-check-of-node-maintenance
  []
  (load-book "playbooks/kubernetes/node_maintenance.clj")
  @(resolve 'playbooks.kubernetes.node-maintenance/disruption-check))

(deftest node-maintenance-reports-blocking-budgets-before-the-confirmation
  (let [{:keys [check]} (disruption-check-of-node-maintenance)
        budget {:metadata {:name "web" :namespace "shop"}
                :spec {:maxUnavailable 0}
                :status {:disruptionsAllowed 0 :currentHealthy 1 :desiredHealthy 1 :expectedPods 1}}
        out (with-redefs-fn {#'detective/collect-evidence!
                             (fn [_ _] {:now (java.time.Instant/now) "pods" {:items []}
                                        "poddisruptionbudgets" {:items [budget]}})}
              (fn [] (binding [*err* (java.io.StringWriter.)]
                       (with-out-str (is (true? (check {:context "ctx"}))
                                         "an advisory check never stops the playbook")))))]
    (is (str/includes? (stdout/strip-colors out) "shop/web") "the blocking budget is named")))
