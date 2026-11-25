;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-connections-test
  "The check that a CloudNativePG cluster is at its connection ceiling, and who
   holds the connections. Fixtures are synthetic, shaped as CloudNativePG
   writes them: the Cluster names its primary in status.currentPrimary, the
   instance pod of that name carries cnpg.io/instanceRole=primary, and the
   Postgres container is called postgres. Readings have the shape
   gumshoe.postgres/parse-connections returns."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [gumshoe.cnpg.connections :as connections]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.cnpg-connections :as check-ns]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.joins-support :as joins]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.postgres :as postgres]
            [gumshoe.shell :as shell]
            [gumshoe.tools.cnpg]))

;; --- fixtures ---------------------------------------------------------------------

(def ^:private cluster-type "clusters.postgresql.cnpg.io")

(defn- cluster-of
  [namespace name & {:keys [primary] :or {primary :default}}]
  {:apiVersion "postgresql.cnpg.io/v1"
   :kind "Cluster"
   :metadata {:namespace namespace :name name}
   :spec {:instances 3}
   :status (cond-> {:phase "Cluster in healthy state"}
             (= :default primary) (assoc :currentPrimary (str name "-1") :targetPrimary (str name "-1"))
             (string? primary) (assoc :currentPrimary primary :targetPrimary primary))})

(defn- pod-of
  [namespace name ip & {:keys [phase labels] :or {phase "Running"}}]
  {:apiVersion "v1"
   :kind "Pod"
   :metadata {:namespace namespace :name name :labels (or labels {})}
   :status {:phase phase :podIP ip :podIPs [{:ip ip}]}})

(defn- row
  "One pg_stat_activity group: client backends by default."
  ([addr n] (row addr n "active"))
  ([addr n state] {:client-addr addr :user "app" :application "pgbouncer" :state state
                   :backend-type "client backend" :backends n}))

(defn- background [n] {:client-addr nil :user nil :application nil :state nil
                       :backend-type "background writer" :backends n})

(defn- reading
  [max reserved & rows]
  {:max-connections max :superuser-reserved-connections reserved :reserved-connections 0
   :read-only? true :in-recovery? false :rows (vec rows)})

(defn- read-row [namespace cluster r] {:namespace namespace :cluster cluster :pod (str cluster "-1") :reading r})

(defn- evidence
  "The three reads; nil for a read that did not answer. connection-rows go
   through connections/collect's classification, as the collector would."
  [clusters pods connection-rows]
  {cluster-type (some->> clusters (hash-map :items))
   "pods" (some->> pods (hash-map :items))
   connections/evidence-key (some-> connection-rows connections/classify)})

(def ^:private check
  (first (filter #(= "cnpg-connections" (:name %)) check-ns/detectives)))

(defn- detect [evidence] (vec ((:detect check) evidence)))

(def ^:private pg (cluster-of "db" "pg"))

(def ^:private live-pooler (pod-of "db" "pooler-rw-a" "10.0.0.5"))
(def ^:private recycled (pod-of "backup" "restic-x" "10.0.0.9" :phase "Succeeded"))

;; --- the table --------------------------------------------------------------------

(def ^:private scenarios
  "label, evidence, the [severity summary] pairs expected, and whether the check is unread."
  [["below the ceiling: 96 of the 97 ordinary roles may hold"
    (evidence [pg] [live-pooler] [(read-row "db" "pg" (reading 100 3 (row "10.0.0.5" 96)))])
    [] false]
   ["at the ceiling: 97"
    (evidence [pg] [live-pooler] [(read-row "db" "pg" (reading 100 3 (row "10.0.0.5" 97)))])
    [[:critical "97 of max_connections 100 in use, at or above the 97 left for ordinary roles: 97 from 1 running pod"]]
    false]
   ["above the ceiling: superusers fill the reserved slots"
    (evidence [pg] [live-pooler] [(read-row "db" "pg" (reading 100 3 (row "10.0.0.5" 97) (row nil 3)))])
    [[:critical "100 of max_connections 100 in use, at or above the 97 left for ordinary roles: 97 from 1 running pod, 3 without a client address (local socket)"]]
    false]
   ["all holders live: two running pods"
    (evidence [pg] [live-pooler (pod-of "db" "pooler-rw-b" "10.0.0.6")]
              [(read-row "db" "pg" (reading 100 3 (row "10.0.0.5" 50) (row "10.0.0.6" 47 "idle")))])
    [[:critical "97 of max_connections 100 in use, at or above the 97 left for ordinary roles: 97 from 2 running pods"]]
    false]
   ["all holders vanished: no running pod has the address"
    (evidence [pg] [live-pooler]
              [(read-row "db" "pg" (reading 1500 3 (row "10.25.234.87" 498) (row "10.25.198.195" 483) (row "10.25.198.235" 465) (row "10.25.1.1" 60)))])
    [[:critical "1506 of max_connections 1500 in use, at or above the 1497 left for ordinary roles: 1506 from 4 addresses with no running pod"]]
    false]
   ["a mix, with a recycled address held by a pod that is no longer running"
    (evidence [pg] [live-pooler recycled]
              [(read-row "db" "pg" (reading 100 3 (row "10.0.0.9" 80) (row "10.0.0.5" 15) (row nil 2) (background 6)))])
    [[:critical "97 of max_connections 100 in use, at or above the 97 left for ordinary roles: 80 from 1 address with no running pod, 15 from 1 running pod, 2 without a client address (local socket)"]]
    false]
   ["NULL client_addr: local-socket backends count, background processes do not"
    (evidence [pg] [] [(read-row "db" "pg" (reading 100 3 (row nil 97) (background 40)))])
    [[:critical "97 of max_connections 100 in use, at or above the 97 left for ordinary roles: 97 without a client address (local socket)"]]
    false]
   ["background processes alone never reach the ceiling"
    (evidence [pg] [] [(read-row "db" "pg" (reading 100 3 (row nil 1) (background 120)))])
    [] false]
   ["the SQL read unanswered on one cluster while another answered"
    (evidence [pg (cluster-of "db" "other")] [live-pooler]
              [(read-row "db" "pg" (reading 100 3 (row "10.0.0.5" 10)))
               {:namespace "db" :cluster "other" :pod "other-1" :reading nil}])
    [[:warning "connections unread: kubectl exec into pod db/other-1, container postgres, did not answer"]]
    false]
   ["exec denied everywhere: nothing read, so the check is unread"
    (evidence [pg] [live-pooler] [{:namespace "db" :cluster "pg" :pod "pg-1" :reading nil}])
    [[:warning "connections unread: kubectl exec into pod db/pg-1, container postgres, did not answer"]]
    true]
   ["no primary found"
    (evidence [(cluster-of "db" "pg" :primary nil)] [] [{:namespace "db" :cluster "pg" :reason :no-primary}])
    [[:warning "connections unread: the Cluster names no current primary and no pod carries cnpg.io/instanceRole=primary"]]
    true]
   ["the primary-pod read did not answer"
    (evidence [(cluster-of "db" "pg" :primary nil)] [] [{:namespace "db" :cluster "pg" :reason :primary-unread}])
    [[:warning "connections unread: the Cluster names no current primary and the read of pods labelled cnpg.io/instanceRole=primary did not answer"]]
    true]
   ["the pod answered from recovery, so it was not the primary"
    (evidence [pg] [] [(read-row "db" "pg" (assoc (reading 100 3 (row "10.0.0.5" 99)) :in-recovery? true))])
    [[:warning "connections unread: pod db/pg-1 answered from recovery, so it is not the primary"]]
    true]
   ["several clusters: one at the ceiling, one below, in two namespaces"
    (evidence [pg (cluster-of "shop" "orders")] [live-pooler]
              [(read-row "db" "pg" (reading 100 3 (row "10.0.0.5" 12)))
               (read-row "shop" "orders" (reading 200 3 (row "10.1.0.1" 197)))])
    [[:critical "197 of max_connections 200 in use, at or above the 197 left for ordinary roles: 197 from 1 address with no running pod"]]
    false]
   ["a reading for a cluster the Cluster read does not hold is not judged"
    (evidence [] [live-pooler] [(read-row "db" "gone" (reading 100 3 (row "10.0.0.5" 99)))])
    [] false]
   ["no clusters: nothing to judge"
    (evidence [] [] [])
    [] true]
   ["unanswered read: the connections"
    (evidence [pg] [live-pooler] nil)
    [] true]
   ["unanswered read: the pods"
    (evidence [pg] nil [(read-row "db" "pg" (reading 100 3 (row "10.0.0.5" 99)))])
    [] true]
   ["unanswered read: the Clusters"
    (evidence nil [live-pooler] [(read-row "db" "pg" (reading 100 3 (row "10.0.0.5" 99)))])
    [] true]])

(deftest every-scenario-reports-as-the-table-says
  (doseq [[label evidence expected unread?] scenarios]
    (testing label
      (is (= expected (mapv (juxt :severity :summary) (detect evidence))))
      (is (= unread? (detective/unread? check evidence))))))

(deftest the-finding-names-the-cluster-and-the-holders
  (let [[finding] (detect (evidence [pg] [live-pooler recycled]
                                    [(read-row "db" "pg" (reading 100 3 (row "10.0.0.9" 80) (row "10.0.0.5" 10 "idle")
                                                                  (row "10.0.0.5" 5) (row nil 2) (background 6)))]))
        hint (:hint finding)]
    (is (= "db/pg" (:component finding)))
    (is (str/includes? hint "10.0.0.9 (no running pod) holds 80: 80 active; user app; application pgbouncer"))
    (is (str/includes? hint "10.0.0.5 (pod db/pooler-rw-a) holds 15: 5 active, 10 idle; user app; application pgbouncer"))
    (is (str/includes? hint "local socket holds 2"))
    (is (str/includes? hint "6 background processes hold no connection slot"))
    (is (str/includes? hint "pg_terminate_backend"))
    (is (str/includes? hint "client_connection_check_interval"))))

(deftest many-holders-are-cut-to-the-largest
  (let [rows (for [i (range 1 13)] (row (str "10.9.0." i) (+ 8 i)))
        [finding] (detect (evidence [pg] [] [(read-row "db" "pg" (apply reading 100 3 rows))]))]
    (is (str/includes? (:hint finding) "10.9.0.12 (no running pod) holds 20"))
    (is (not (str/includes? (:hint finding) "10.9.0.1 (no running pod)")))
    (is (str/includes? (:hint finding) "and 7 more addresses"))))

(deftest an-ipv4-mapped-address-matches-the-pod
  (let [[finding] (detect (evidence [pg] [live-pooler] [(read-row "db" "pg" (reading 100 3 (row "::ffff:10.0.0.5" 97)))]))]
    (is (str/ends-with? (:summary finding) "97 from 1 running pod"))))

(deftest the-check-declares-what-it-joins-and-judges
  (is (= [cluster-type "pods" connections/evidence-key] (:requires check)))
  (is (= [cluster-type "pods" connections/evidence-key] (:joins check)))
  (is (= [connections/evidence-key] (:judges check)))
  (is (not (str/blank? (:description check)))))

(deftest the-check-joins-the-databases-scope
  (is (some #(= "cnpg-connections" (:name %)) (registry/for-scope :databases)))
  (is (= connections/collector (registry/collector-for connections/evidence-key))))

;; --- the collector ------------------------------------------------------------------

(deftest the-primary-is-the-current-primary-or-the-labelled-pod
  (is (= "pg-1" (connections/primary-pod pg nil)))
  (let [labelled [(pod-of "db" "pg-2" "10.0.0.2" :labels {:cnpg.io/cluster "pg" :cnpg.io/instanceRole "primary"})
                  (pod-of "db" "x-1" "10.0.0.3" :labels {:cnpg.io/cluster "x" :cnpg.io/instanceRole "primary"})
                  (pod-of "other" "pg-3" "10.0.0.4" :labels {:cnpg.io/cluster "pg" :cnpg.io/instanceRole "primary"})]]
    (is (= "pg-2" (connections/primary-pod (cluster-of "db" "pg" :primary nil) labelled)))
    (is (nil? (connections/primary-pod (cluster-of "db" "pg" :primary nil) [])))))

(defn- collect-with
  "connections/collect over these clusters, with a labelled-pod read and an
   exec answering from the maps given; records the calls."
  [clusters labelled answers]
  (let [calls (atom {:labelled 0 :execs []})
        result (connections/collect clusters
                                    (fn [] (swap! calls update :labelled inc) labelled)
                                    (fn [namespace pod] (swap! calls update :execs conj [namespace pod])
                                      (get answers [namespace pod])))]
    [result @calls]))

(deftest the-collector-reads-each-primary-once
  (let [r (reading 100 3 (row "10.0.0.5" 1))
        [result calls] (collect-with {:items [pg (cluster-of "shop" "orders")]} {:items []}
                                     {["db" "pg-1"] r ["shop" "orders-1"] r})]
    (is (= #{["db" "pg-1"] ["shop" "orders-1"]} (set (:execs calls))))
    (is (zero? (:labelled calls)) "every Cluster named its primary, so no pod read was needed")
    (is (= 2 (count (:items result))))
    (is (empty? (:unread result)))))

(deftest exec-denied-leaves-every-cluster-unread
  (let [[result] (collect-with {:items [pg]} {:items []} {})]
    (is (empty? (:items result)))
    (is (= [{:namespace "db" :cluster "pg" :pod "pg-1" :reason :no-answer}] (:unread result)))
    (is (detective/unread? check {cluster-type {:items [pg]} "pods" {:items []} connections/evidence-key result}))))

(deftest no-primary-execs-nothing
  (let [[result calls] (collect-with {:items [(cluster-of "db" "pg" :primary nil)]} {:items []} {})]
    (is (empty? (:execs calls)))
    (is (= [{:namespace "db" :cluster "pg" :reason :no-primary}] (:unread result)))))

(deftest a-failed-primary-pod-read-is-not-an-absent-primary
  (let [[result] (collect-with {:items [(cluster-of "db" "pg" :primary nil)]} nil {})]
    (is (= [{:namespace "db" :cluster "pg" :reason :primary-unread}] (:unread result)))))

(deftest a-failed-cluster-read-is-no-evidence
  (is (nil? (first (collect-with nil {:items []} {})))))

(deftest the-collector-execs-psql-in-the-postgres-container
  (let [argvs (atom [])]
    (with-redefs [shell/execute (fn [& argv]
                                  (swap! argvs conj (vec argv))
                                  (cond
                                    (some #{"get"} argv)
                                    {:exit 0 :out (json/generate-string {:items [pg]}) :err ""}
                                    :else {:exit 1 :out "" :err "Forbidden"}))]
      (binding [kubectl/*read-cache* nil]
        (let [result (connections/collector "ctx" connections/evidence-key)]
          (is (= [{:namespace "db" :cluster "pg" :pod "pg-1" :reason :no-answer}] (:unread result))))))
    (let [exec (first (filter #(some #{"exec"} %) @argvs))]
      (is (= ["kubectl" "--context=ctx" "--namespace=db"] (take 3 exec)))
      (is (= ["exec" "pg-1" "-c" "postgres" "--"] (take 5 (drop 4 exec))))
      (is (= postgres/connection-command (vec (drop 9 exec)))))))

;; --- the JSON report --------------------------------------------------------------

(def ^:private golden-path "tools/cnpg/tests/gumshoe/cnpg_connections_golden.json")

(def ^:private golden-scenarios
  [["a cluster at its ceiling held mostly by addresses with no running pod, beside one below it and one unread"
    (evidence [pg (cluster-of "shop" "orders") (cluster-of "shop" "carts")] [live-pooler recycled]
              [(read-row "db" "pg" (reading 1500 3 (row "10.25.234.87" 498) (row "10.25.198.195" 483)
                                            (row "10.0.0.9" 465) (row "10.0.0.5" 50 "idle") (row nil 4) (background 9)))
               (read-row "shop" "orders" (reading 200 3 (row "10.0.0.5" 20)))
               {:namespace "shop" :cluster "carts" :pod "carts-1" :reading nil}])]
   ["exec denied on the only cluster"
    (evidence [pg] [live-pooler] [{:namespace "db" :cluster "pg" :pod "pg-1" :reading nil}])]
   ["the connection read did not answer"
    (evidence [pg] [live-pooler] nil)]])

(defn- json-report
  [evidence]
  (let [checked (detective/mark-unread [check] evidence)
        findings (detective/run-detectives checked evidence)]
    (json/parse-string
     (with-out-str (binding [*err* (java.io.StringWriter.)]
                     (detective/report! checked findings "json"))))))

(defn- reports []
  (into (sorted-map) (for [[label evidence] golden-scenarios] [label (json-report evidence)])))

(deftest the-json-findings-are-pinned
  (let [actual (reports)]
    (if (System/getenv "GUMSHOE_RECORD_GOLDEN")
      (spit golden-path (str (json/generate-string actual {:pretty true}) "\n"))
      (is (= (json/parse-string (slurp golden-path)) actual)))))

;; --- no unanswered read yields zero or a pass -----------------------------------------

(def ^:private seed 20261125)

(def ^:private cases
  [{:check check
    :joins [cluster-type "pods" connections/evidence-key]
    :answered (evidence [pg (cluster-of "shop" "orders")] [live-pooler recycled]
                        [(read-row "db" "pg" (reading 100 3 (row "10.0.0.9" 80) (row "10.0.0.5" 17)))
                         (read-row "shop" "orders" (reading 200 3 (row "10.0.0.5" 199)))])}])

(deftest each-missing-joined-read-judges-nothing
  (doseq [[label evidence] (joins/missing-read-cases (:joins (first cases)) (:answered (first cases)))]
    (testing label
      (is (joins/judges-nothing? check evidence)))))

(deftest no-unanswered-read-yields-zero-or-a-pass
  (let [result (tc/quick-check 300 (joins/no-missing-joined-read-judges cases) :seed seed)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :fail])))))

(deftest the-property-catches-a-check-that-forgets-its-join
  ;; the positive control: without :joins and with a detect function that
  ;; reads a missing side as empty, the property must find a counterexample
  (let [careless (-> check
                     (dissoc :joins)
                     (assoc :detect (fn [e] ((:detect check) (merge {"pods" {:items []}} (into {} (remove (comp nil? val)) e))))))]
    (is (not (:pass? (tc/quick-check 300 (joins/no-missing-joined-read-judges
                                          (mapv #(assoc % :check careless) cases))
                                         :seed seed))))))

(deftest answered-evidence-does-produce-findings
  (is (seq (detect (:answered (first cases))))
      "the property's evidence reaches a finding when every read answered"))

(deftest an-unread-cluster-is-never-zero-connections
  ;; whatever answered elsewhere, a cluster whose reading is missing is
  ;; reported as unread and never silently passes
  (doseq [n (range 0 5)]
    (let [rows (concat (for [i (range n)] (read-row "db" (str "c" i) (reading 100 3 (row "10.0.0.5" 1))))
                       [{:namespace "db" :cluster "lost" :pod "lost-1" :reading nil}])
          clusters (cons (cluster-of "db" "lost") (for [i (range n)] (cluster-of "db" (str "c" i))))
          findings (detect (evidence clusters [live-pooler] rows))]
      (is (some #(and (= "db/lost" (:component %)) (= :warning (:severity %))
                      (str/starts-with? (:summary %) "connections unread"))
                findings)))))
