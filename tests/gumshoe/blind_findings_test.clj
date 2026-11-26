;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.blind-findings-test
  "A scan keeps no finding of a check that read nothing unless the finding is
   marked :reports-unread (detective/run-detectives). A check that joins
   several reads is unread as soon as one of them is nil, so any finding its
   detect function still returns then is lost to the scan, however true it is.
   This guard runs every check that declares :joins through run-detectives on
   its own fixture with each joined read unanswered in turn, and with all of
   them unanswered, and fails on a finding the detect function returned that
   the scan dropped, unless it is listed as a deliberate drop below.

   The guard covers the checks that declare :joins, discovered from the
   source, rather than all registered checks: only :joins makes a check
   unread while some of its reads answered, which is when a detect function
   can still conclude something. A check unread through :requires, :judges or
   :consults had nothing answered to conclude from. Every discovered check
   needs a fixture here or in one of the joins case lists, so a new check
   declaring :joins fails this test until it has one."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cnpg-connections-test]
            [gumshoe.detective :as detective]
            [gumshoe.detective-joins-test]
            [gumshoe.detective-joins-unjudged-test]
            [gumshoe.detectives.autoscaler :as autoscaler]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.joins-support :refer [items meta-of]]
            [gumshoe.unanswered-reads-test]))

(def ^:private now (java.time.Instant/parse "2026-11-20T12:00:00Z"))

;; --- discovery ----------------------------------------------------------------------

(defn- source-files
  []
  (->> (cons (io/file "libraries")
             (keep #(let [f (io/file % "libraries")] (when (.isDirectory f) f))
                   (.listFiles (io/file "tools"))))
       (mapcat file-seq)
       (filter #(str/ends-with? (.getName ^java.io.File %) ".clj"))))

(defn- namespaces-declaring-joins
  "The namespaces of library sources that mention :joins."
  []
  (->> (source-files)
       (keep (fn [f]
               (let [text (slurp f)]
                 (when (str/includes? text ":joins")
                   (some->> (re-find #"\(ns\s+([^\s()]+)" text) second symbol)))))
       distinct
       sort))

(defn- detective-vector?
  [x]
  (and (sequential? x) (seq x) (every? #(and (map? %) (:name %) (fn? (:detect %))) x)))

(def ^:private joining-checks
  "Every check declaring :joins in a public detective vector of those
   namespaces, by name."
  (delay
    (let [namespaces (namespaces-declaring-joins)]
      (run! require namespaces)
      (into (sorted-map)
            (for [n namespaces
                  [_ v] (ns-publics n)
                  :let [x @v]
                  :when (detective-vector? x)
                  check x
                  :when (seq (:joins check))]
              [(:name check) check])))))

;; --- fixtures -----------------------------------------------------------------------

(defn- case-fixtures
  "name -> answered evidence from a joins case list."
  [cases]
  (into {} (map (juxt (comp :name :check) :answered)) cases))

(def ^:private text-status
  "Cluster-autoscaler status at 2026-11-20 09:00:00 +0000 UTC:
Cluster-wide:
  Health:      Healthy (ready=3 unready=0 notStarted=0 longNotStarted=0 registered=3 longUnregistered=0)
")

(def ^:private own-fixtures
  "Answered evidence for the joining checks no case list covers. A joined read
   the guard sets to nil may hold anything. Where a check joins only part of
   what it requires, the other reads hold what would make it report, so a
   finding concluded from them while a joined read is missing shows up; a
   check joining its only list read has scalars (a domain, a host, a URL)
   beside it."
  {"cluster-autoscaler"
   {"cluster-autoscaler-status" {:metadata {:namespace "kube-system" :name "cluster-autoscaler-status"
                                            :annotations {(keyword autoscaler/last-updated-annotation)
                                                          "2026-11-20 09:00:00 +0000 UTC"}}
                                 :data {:status text-status}}
    autoscaler/parsed-status-key {:autoscalerStatus "Running"
                                  :clusterWide {:health {:status "Unhealthy"}}}}

   "cnpg-instance-crashloop"
   {"clusters.postgresql.cnpg.io" (items {:metadata (meta-of "db" "pg") :status {:currentPrimary "pg-1"}})
    "pods" (items {:metadata (assoc (meta-of "db" "pg-1") :labels {:cnpg.io/cluster "pg" :cnpg.io/podRole "instance"})
                   :status {:containerStatuses [{:name "postgres" :restartCount 9
                                                 :state {:waiting {:reason "CrashLoopBackOff"}}}]}})}

   "cnpg-pooler-endpoints"
   {"poolers.postgresql.cnpg.io" (items {:metadata (meta-of "db" "pool")
                                         :spec {:instances 2 :cluster {:name "pg"} :type "rw"}})
    "services" (items)
    "endpointslices" (items)}

   "dns-replication" {"serials" {"ns1.example.org" 1 "ns2.example.org" 2} "zone" "example.org"}

   "expected-capabilities"
   {"config" {:capabilities [:calico :flux]}
    "capabilities" {:absent [:calico] :unanswered [:flux]}
    "customresourcedefinitions" (items)}

   "external-dns"
   {"sources" {"app.example.org" ["Ingress shop/app"]}
    "resolved" {"app.example.org" {:a [] :aaaa []}}
    "server" "192.0.2.53"}

   "host-exporters-up" {"hosts" [{:host "h1" :up false}] "up-answered" true}

   "host-facts-complete" {"hosts" [{:host "h1" :up true}] "cores-answered" true "memory-answered" true}

   "loki-mode" {"workloads" (items) "namespace" "loki"}

   "mail-mx" {"mx" [] "domain" "example.org"}
   "mail-spf" {"spf" [] "domain" "example.org"}
   "mail-dmarc" {"dmarc" [] "domain" "example.org"}
   "mail-dkim" {"dkim" {} "domain" "example.org"}
   "mail-rdns" {"ptr" [] "host" "mx.example.org"}
   "mail-services" {"probes" {} "host" "mx.example.org"}

   "matrix-federation-destinations" {"destinations" [] "admin?" true "host" "matrix.example.org"}

   "thanos-stores" {"stores" [] "url" "http://thanos.example.org"}
   "thanos-rules" {"rules" [] "url" "http://thanos.example.org"}

   "webhook-backends"
   {"validatingwebhookconfigurations"
    (items {:metadata (meta-of "policy")
            :webhooks [{:name "validate.example.org"
                        :clientConfig {:service {:namespace "policy" :name "webhook"}}}]})
    "mutatingwebhookconfigurations" (items)
    "services" (items {:metadata (meta-of "policy" "webhook") :spec {:type "ClusterIP"}})
    "endpointslices" (items)}})

(def ^:private fixtures
  (delay
    (merge (case-fixtures @#'gumshoe.detective-joins-test/cases)
           (case-fixtures @#'gumshoe.detective-joins-unjudged-test/cases)
           (case-fixtures @#'gumshoe.unanswered-reads-test/joining-cases)
           (case-fixtures @#'gumshoe.cnpg-connections-test/cases)
           own-fixtures)))

(def ^:private deliberate-drops
  "[check summary] of findings a check returns with a joined read missing
   that the scan is meant to drop. None today: every check either reports
   nothing then or marks what it reports :reports-unread."
  #{})

;; --- the guard ----------------------------------------------------------------------

(defn- blind-cases
  "[label evidence] with each joined read unanswered in turn, and all of them."
  [joins answered]
  (let [base (merge {:now now} answered)]
    (conj (mapv (fn [read] [(str read " unanswered") (assoc base read nil)]) joins)
          ["no joined read answered" (reduce #(assoc %1 %2 nil) base joins)])))

(defn- finding-key [f] (select-keys f [:severity :component :summary]))

(defn lost-findings
  "The findings the check's detect function returns on this evidence that a
   scan does not report, less the deliberate drops."
  [check evidence]
  (let [direct (try (doall ((:detect check) evidence)) (catch Throwable _ []))
        reported (set (map finding-key (detective/run-detectives (detective/mark-unread [check] evidence) evidence)))]
    (vec (for [f direct
               :when (not (contains? deliberate-drops [(:name check) (:summary f)]))
               :when (not (contains? reported (finding-key f)))]
           (:summary f)))))

(deftest every-joining-check-is-discovered
  (testing "every registered check declaring :joins is among the discovered ones"
    (let [registered (set (map :name (filter :joins (registry/all))))]
      (is (empty? (remove (set (keys @joining-checks)) registered)))))
  (testing "the discovery is not vacuous"
    (is (< 50 (count @joining-checks)))
    (is (contains? @joining-checks "cluster-autoscaler"))))

(deftest every-joining-check-has-a-fixture
  (is (empty? (remove (set (keys @fixtures)) (keys @joining-checks)))
      "add answered evidence for the check to own-fixtures or a joins case list"))

(deftest no-check-loses-a-finding-it-reports-while-a-joined-read-is-missing
  (doseq [[name check] @joining-checks
          :let [answered (get @fixtures name)]
          :when answered
          [label evidence] (blind-cases (:joins check) answered)]
    (testing (str name ": " label)
      (is (true? (detective/unread? check evidence)) "a missing joined read leaves the check unread")
      (is (= [] (lost-findings check evidence))
          "mark the finding :reports-unread when it rests only on reads that answered, or list it in deliberate-drops"))))

(deftest the-guard-sees-a-finding-reported-while-blind
  ;; the autoscaler's staleness is the one finding a joining check reports
  ;; with a joined read missing today, so the guard is not vacuous
  (let [check (get @joining-checks "cluster-autoscaler")
        evidence (assoc (merge {:now now} (get @fixtures "cluster-autoscaler")) autoscaler/parsed-status-key nil)]
    (is (= ["the cluster autoscaler has stopped updating its status"]
           (mapv :summary ((:detect check) evidence))))
    (is (= [] (lost-findings check evidence)))))

(deftest the-guard-catches-an-unmarked-finding-from-a-blind-check
  (let [check {:name "probe" :requires ["a" "b"] :joins ["b"]
               :detect (fn [e] (when (get e "a") [{:severity :critical :component "x" :summary "from a alone"}]))}]
    (is (= ["from a alone"] (lost-findings check {"a" (items) "b" nil})))
    (is (= [] (lost-findings (update check :detect (fn [d] #(map (fn [f] (assoc f :reports-unread true)) (d %))))
                             {"a" (items) "b" nil})))))
