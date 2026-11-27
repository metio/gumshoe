;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.scrape-targets-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.detectives.scrape-targets :as scrape]
            [gumshoe.prometheus.targets :as targets]
            [gumshoe.shell :as shell]
            [gumshoe.tools.prometheus]))

;; --- fixtures: Prometheus objects, operator pods, /api/v1/targets answers ---

(defn- prometheus
  ([name spec] (prometheus "monitoring" name spec))
  ([namespace name spec]
   {:apiVersion "monitoring.coreos.com/v1" :kind "Prometheus"
    :metadata {:namespace namespace :name name}
    :spec spec}))

(defn- pod
  [{:keys [namespace name prometheus shard ready? port-name port]
    :or {namespace "monitoring" ready? true port-name "web" port 9090}}]
  {:metadata {:namespace namespace :name name
              :labels {:app.kubernetes.io/name "prometheus"
                       :operator.prometheus.io/name prometheus
                       :operator.prometheus.io/shard (str shard)}}
   :spec {:containers [{:name "prometheus" :ports [{:name port-name :containerPort port}]}
                       {:name "config-reloader" :ports [{:name "reloader-web" :containerPort 8080}]}]}
   :status {:phase "Running"
            :conditions [{:type "Ready" :status (if ready? "True" "False")}]}})

(defn- target
  [{:keys [pool url health error job]}]
  {:discoveredLabels {:__address__ "10.0.3.7:9100" :__meta_kubernetes_pod_annotation_example "kept out"}
   :labels {:job (or job "node-exporter") :instance (str/replace (str url) #"^https?://([^/]+)/.*$" "$1")}
   :scrapePool pool
   :scrapeUrl url
   :globalUrl url
   :lastError (or error "")
   :lastScrape "2026-11-27T10:00:00.000Z"
   :lastScrapeDuration 0.01
   :health health
   :scrapeInterval "30s"
   :scrapeTimeout "10s"})

(defn- answer
  "The raw result of a kubectl get --raw of /api/v1/targets answering ts."
  [ts]
  {:exit 0 :err ""
   :out (json/generate-string {:status "success" :data {:activeTargets ts :droppedTargets []}})})

(def unavailable
  {:exit 1 :out "" :err "Error from server (ServiceUnavailable): the server is currently unable to handle the request"})

(def errors
  "lastError texts as Prometheus publishes them, each with the class it
   must get."
  [["Get \"http://10.0.3.7:9100/metrics\": dial tcp 10.0.3.7:9100: i/o timeout" :dropped]
   ["Get \"http://[fd00::7]:9100/metrics\": dial tcp [fd00::7]:9100: i/o timeout" :dropped]
   ["Get \"http://10.0.3.8:9100/metrics\": dial tcp 10.0.3.8:9100: connect: no route to host" :dropped]
   ["Get \"http://10.0.3.8:9100/metrics\": dial tcp 10.0.3.8:9100: connect: network is unreachable" :dropped]
   ["Get \"http://10.0.3.9:8080/metrics\": dial tcp 10.0.3.9:8080: connect: connection refused" :refused]
   ["server returned HTTP status 401 Unauthorized" :above-transport]
   ["server returned HTTP status 404 Not Found" :above-transport]
   ["server returned HTTP status 503 Service Unavailable" :above-transport]
   ["Get \"https://10.0.3.10:10250/metrics\": tls: failed to verify certificate: x509: certificate signed by unknown authority" :above-transport]
   ["Get \"https://10.0.3.10:10250/metrics\": x509: certificate has expired or is not yet valid: current time 2026-11-27T10:00:00Z is after 2026-11-01T00:00:00Z" :above-transport]
   ["Get \"https://10.0.3.10:9100/metrics\": http: server gave HTTP response to HTTPS client" :above-transport]
   ["Get \"https://10.0.3.10:9100/metrics\": remote error: tls: bad certificate" :above-transport]
   ["Get \"http://10.0.3.12:9090/metrics\": EOF" :above-transport]
   ["sample limit exceeded" :above-transport]
   ["body size limit exceeded" :above-transport]
   ["label_limit exceeded (metric: up, number of labels: 31, limit: 30)" :above-transport]
   ["Get \"http://10.0.3.11:9090/metrics\": context deadline exceeded" :timed-out]
   ["Get \"https://10.0.3.11:9090/metrics\": net/http: TLS handshake timeout" :timed-out]
   ["Get \"http://foo.bar.svc:9090/metrics\": dial tcp: lookup foo.bar.svc on 10.96.0.10:53: no such host" :lookup]
   ["Get \"http://10.0.3.13:9090/metrics\": read tcp 10.0.1.2:40000->10.0.3.13:9090: read: connection reset by peer" :unknown]
   ["something nobody has seen before" :unknown]
   ["" :unknown]
   [nil :unknown]])

;; --- classification ---------------------------------------------------------

(deftest every-known-error-text-gets-its-class
  (doseq [[error class] errors]
    (is (= class (:class (scrape/classify error))) (pr-str error))))

(deftest the-url-in-the-request-prefix-cannot-decide-the-class
  (is (= :refused (:class (scrape/classify "Get \"http://x509-exporter.tls.svc:9793/metrics\": dial tcp 10.0.3.14:9793: connect: connection refused"))))
  (is (= :unknown (:class (scrape/classify "Get \"https://tls-check.example.org:443/x509: tls: EOF\": read: connection reset by peer")))))

(deftest classification-is-case-sensitive-on-the-texts-go-writes
  (is (= :unknown (:class (scrape/classify "CONNECT: CONNECTION REFUSED")))))

;; --- shard paths ------------------------------------------------------------

(deftest the-proxy-path-follows-scheme-port-and-route-prefix
  (let [p (pod {:name "prometheus-general-0" :prometheus "general" :shard 0})]
    (is (= {:path "/api/v1/namespaces/monitoring/pods/http:prometheus-general-0:9090/proxy/api/v1/targets?state=active"}
           (targets/shard-path (prometheus "general" {}) p)))
    (is (= {:path "/api/v1/namespaces/monitoring/pods/https:prometheus-general-0:9090/proxy/prom/api/v1/targets?state=active"}
           (targets/shard-path (prometheus "general" {:web {:tlsConfig {:cert {}}} :routePrefix "/prom/"}) p)))
    (is (= {:path "/api/v1/namespaces/monitoring/pods/http:prometheus-general-0:9091/proxy/api/v1/targets?state=active"}
           (targets/shard-path (prometheus "general" {:portName "api"})
                               (pod {:name "prometheus-general-0" :prometheus "general" :shard 0 :port-name "api" :port 9091}))))))

(deftest a-route-prefix-that-could-leave-the-pod-api-builds-no-path
  (let [p (pod {:name "prometheus-general-0" :prometheus "general" :shard 0})]
    (doseq [prefix ["/../../secrets" "/prom?x=1" "prom" "/a/./b" "/%2e%2e"]]
      (is (nil? (:path (targets/shard-path (prometheus "general" {:routePrefix prefix}) p))) prefix))
    (is (re-find #"no port named web"
                 (:reason (targets/shard-path (prometheus "general" {})
                                              (pod {:name "prometheus-general-0" :prometheus "general" :shard 0 :port-name "http"})))))))

;; --- answers ----------------------------------------------------------------

(deftest an-answer-is-read-only-when-it-is-a-successful-targets-list
  (is (= {:state :read :targets []} (targets/classify-answer (answer []))))
  (is (= :unread (:state (targets/classify-answer unavailable))))
  (is (= "Error from server (ServiceUnavailable): the server is currently unable to handle the request"
         (:reason (targets/classify-answer unavailable))))
  (is (= :unread (:state (targets/classify-answer {:exit 0 :out "<html>proxy error</html>" :err ""}))))
  (is (= :unread (:state (targets/classify-answer {:exit 0 :out "{\"status\":\"error\",\"error\":\"x\"}" :err ""}))))
  (is (= :unread (:state (targets/classify-answer {:exit 0 :out "{\"status\":\"success\",\"data\":{}}" :err ""})))))

(deftest a-target-row-keeps-no-discovered-labels
  (let [row (first (:targets (targets/classify-answer
                              (answer [(target {:pool "serviceMonitor/monitoring/node-exporter/0"
                                                :url "http://10.0.3.7:9100/metrics" :health "up"})]))))]
    (is (= #{:scrape-pool :scrape-url :health :last-error :labels} (set (keys row))))))

;; --- collection -------------------------------------------------------------

(def general (prometheus "general" {:shards 2 :replicas 2}))

(def general-pods
  [(pod {:name "prometheus-general-0" :prometheus "general" :shard 0})
   (pod {:name "prometheus-general-1" :prometheus "general" :shard 0})
   (pod {:name "prometheus-general-shard-1-0" :prometheus "general" :shard 1})
   (pod {:name "prometheus-general-shard-1-1" :prometheus "general" :shard 1})
   ;; a PrometheusAgent of the same name carries the same operator labels
   (pod {:name "prom-agent-general-0" :prometheus "general" :shard 0})])

(def shard-0-targets
  [(target {:pool "serviceMonitor/monitoring/node-exporter/0" :url "http://10.0.3.7:9100/metrics" :health "down"
            :error "Get \"http://10.0.3.7:9100/metrics\": dial tcp 10.0.3.7:9100: i/o timeout"})
   (target {:pool "serviceMonitor/monitoring/node-exporter/0" :url "http://10.0.3.6:9100/metrics" :health "up"})])

(def shard-1-targets
  [(target {:pool "serviceMonitor/web/api/0" :url "http://10.0.4.9:8080/metrics" :health "down" :job "api"
            :error "Get \"http://10.0.4.9:8080/metrics\": dial tcp 10.0.4.9:8080: connect: connection refused"})
   (target {:pool "serviceMonitor/web/api/0" :url "http://10.0.4.8:8080/metrics" :health "unknown" :job "api"})])

(defn- path-of [pod-name] (str "/api/v1/namespaces/monitoring/pods/http:" pod-name ":9090/proxy/api/v1/targets?state=active"))

(defn- collected
  "collect over the fixtures, with answers a map from pod name to the raw
   result; a pod missing from it fails the read. Returns the evidence and
   the pods asked."
  ([answers] (collected answers [general] general-pods))
  ([answers prometheuses pods]
   (let [asked (atom [])
         evidence (targets/collect
                   {:prometheuses (constantly {:state (if (seq prometheuses) :present :absent) :items (vec prometheuses)})
                    :pods (constantly {:state :present :items (vec pods)})
                    :targets (fn [path]
                               (let [pod-name (second (re-find #"pods/https?:([^:]+):" path))]
                                 (swap! asked conj pod-name)
                                 (get answers pod-name unavailable)))})]
     {:evidence evidence :asked @asked})))

(defn- scanned
  "What a scan reports over evidence, through run-detectives with
   mark-unread, as the engine runs it."
  ([evidence] (scanned scrape/detectives evidence))
  ([detectives evidence]
   (let [ev {targets/evidence-key evidence}]
     (detective/run-detectives (detective/mark-unread detectives ev) ev))))

(deftest every-shard-is-read-from-one-ready-replica
  (let [{:keys [evidence asked]} (collected {"prometheus-general-0" (answer shard-0-targets)
                                              "prometheus-general-shard-1-0" (answer shard-1-targets)})]
    (is (= ["prometheus-general-0" "prometheus-general-shard-1-0"] asked) "one replica per shard, no agent pod")
    (is (= [[0 :read "prometheus-general-0"] [1 :read "prometheus-general-shard-1-0"]]
           (mapv (juxt :shard :state :pod) (:shards evidence))))
    (is (= 4 (count (:targets evidence))))
    (is (= #{["monitoring/general" 0] ["monitoring/general" 1]} (set (map (juxt :prometheus :shard) (:targets evidence)))))))

(deftest down-targets-are-reported-with-their-cause-and-up-ones-are-not
  (let [findings (scanned (:evidence (collected {"prometheus-general-0" (answer shard-0-targets)
                                                  "prometheus-general-shard-1-0" (answer shard-1-targets)})))]
    (is (= 2 (count findings)))
    (is (= #{:dropped :refused} (set (map :class findings))))
    (is (every? #(= :warning (:severity %)) findings))
    (is (some #(and (= :dropped (:class %))
                    (str/includes? (:summary %) "http://10.0.3.7:9100/metrics is down - dropped before anything answered")
                    (str/includes? (:hint %) "network policy"))
              findings))
    (is (some #(and (= :refused (:class %)) (str/includes? (:hint %) "binds this port")) findings))))

(deftest a-replica-that-does-not-answer-falls-back-to-the-next
  (let [{:keys [evidence asked]} (collected {"prometheus-general-0" (answer shard-0-targets)
                                              "prometheus-general-shard-1-1" (answer shard-1-targets)})
        shard-1 (second (:shards evidence))]
    (is (= ["prometheus-general-0" "prometheus-general-shard-1-0" "prometheus-general-shard-1-1"] asked))
    (is (= [:read "prometheus-general-shard-1-1"] ((juxt :state :pod) shard-1)))
    (is (= [{:pod "prometheus-general-shard-1-0"
             :reason "Error from server (ServiceUnavailable): the server is currently unable to handle the request"}]
           (:attempts shard-1)))))

(deftest a-shard-that-does-not-answer-is-reported-unread-and-the-rest-is-still-judged
  (let [{:keys [evidence]} (collected {"prometheus-general-0" (answer shard-0-targets)})
        findings (scanned evidence)
        critical (filter #(= :critical (:severity %)) findings)]
    (is (= :unread (:state (second (:shards evidence)))))
    (is (= 1 (count critical)))
    (is (= "monitoring/general shard 1" (:component (first critical))))
    (is (str/includes? (:hint (first critical)) "no ready pod answered"))
    (is (str/includes? (:hint (first critical)) "the sweep is incomplete"))
    (is (= [:dropped] (keep :class findings)) "shard 0's down target is still judged")
    (testing "negative control: an unread shard treated as an empty one reads as clean"
      (let [as-empty (update evidence :shards
                             (fn [rows] (mapv #(if (= :unread (:state %)) (-> % (assoc :state :read) (dissoc :reason)) %) rows)))]
        (is (empty? (filter #(= :critical (:severity %)) (scanned as-empty)))
            "the critical finding comes from the unread row, so the test above would fail on a collector that hid it")))))

(deftest a-shard-with-no-pod-or-no-ready-pod-is-unread-and-a-pod-not-ready-is-never-read
  (testing "the shard's StatefulSet has no pod at all"
    (let [{:keys [evidence]} (collected {"prometheus-general-0" (answer shard-0-targets)} [general] (take 2 general-pods))]
      (is (= [:read :unread] (mapv :state (:shards evidence))))
      (is (str/includes? (:reason (second (:shards evidence))) "no pod of StatefulSet prometheus-general-shard-1 exists"))))
  (testing "its pods exist and none is ready"
    (let [pods (concat (take 2 general-pods)
                       [(pod {:name "prometheus-general-shard-1-0" :prometheus "general" :shard 1 :ready? false})])
          {:keys [evidence asked]} (collected {"prometheus-general-0" (answer shard-0-targets)
                                               "prometheus-general-shard-1-0" (answer shard-1-targets)}
                                              [general] pods)]
      (is (= ["prometheus-general-0"] asked))
      (is (= :unread (:state (second (:shards evidence)))))
      (is (str/includes? (:reason (second (:shards evidence))) "none of its pods is ready")))))

(deftest unlisted-pods-leave-every-shard-unread
  (let [evidence (targets/collect {:prometheuses (constantly {:state :present :items [general]})
                                   :pods (constantly {:state :unread :reason "pods is forbidden"})
                                   :targets (fn [_] (throw (ex-info "must not read" {})))})]
    (is (= [:unread :unread] (mapv :state (:shards evidence))))
    (is (every? #(str/includes? (:reason %) "pods is forbidden") (:shards evidence)))
    (is (= 2 (count (filter #(= :critical (:severity %)) (scanned evidence)))))))

(deftest unlisted-prometheus-objects-leave-both-checks-unread
  (let [evidence (targets/collect {:prometheuses (constantly {:state :unread :reason "timeout"})
                                   :pods (fn [] (throw (ex-info "must not read" {})))
                                   :targets (fn [_] (throw (ex-info "must not read" {})))})
        ev {targets/evidence-key evidence}]
    (is (nil? evidence))
    (is (every? :unread (detective/mark-unread scrape/detectives ev)))
    (is (empty? (scanned evidence)))))

(deftest no-prometheus-is-an-answer-with-nothing-to-read
  (is (= {:shards [] :targets []} (:evidence (collected {} [] [])))))

(deftest a-prometheus-scaled-to-no-replica-is-named-not-unread
  (let [{:keys [evidence asked]} (collected {} [(prometheus "idle" {:replicas 0})] [])
        findings (scanned evidence)]
    (is (empty? asked))
    (is (= [:scaled-down] (mapv :state (:shards evidence))))
    (is (= [:info] (mapv :severity findings)))))

(deftest replicas-reporting-the-same-target-are-deduplicated
  (let [t (first (:targets (targets/classify-answer (answer shard-0-targets))))
        row (assoc t :prometheus "monitoring/general" :shard 0)]
    (is (= [row] (targets/dedupe-targets [row row])))
    (is (= 2 (count (targets/dedupe-targets [row (assoc row :scrape-url "http://10.0.3.99:9100/metrics")]))))))

(deftest shard-and-replica-counts-follow-the-operator-defaults
  (is (= 1 (targets/shard-count (prometheus "p" {}))))
  (is (= 1 (targets/shard-count (prometheus "p" {:shards 0}))))
  (is (= 3 (targets/shard-count (prometheus "p" {:shards 3}))))
  (is (= 1 (targets/replica-count (prometheus "p" {}))))
  (is (= 0 (targets/replica-count (prometheus "p" {:replicas 0}))))
  (is (= ["prometheus-p" "prometheus-p-shard-1" "prometheus-p-shard-2"]
         (mapv :statefulset (targets/expected-shards (prometheus "p" {:shards 3}))))))

;; --- the registered collector against a recorded kubectl -------------------

(deftest the-collector-lists-with-kubectl-and-reads-each-shard-through-the-pod-proxy
  (let [argvs (atom [])
        listing (fn [items] {:exit 0 :err "" :out (json/generate-string {:apiVersion "v1" :kind "List" :items items})})]
    (with-redefs [shell/execute (fn [& argv]
                                  (let [argv (vec argv)]
                                    (swap! argvs conj argv)
                                    (cond
                                      (some #{targets/prometheus-type} argv) (listing [general])
                                      (some #{"pods"} argv) (listing general-pods)
                                      (some #(str/includes? (str %) "prometheus-general-0:") argv) (answer shard-0-targets)
                                      (some #(str/includes? (str %) "prometheus-general-shard-1-0:") argv) (answer shard-1-targets)
                                      :else unavailable)))]
      (let [evidence (targets/collector "kind-test" targets/evidence-key)
            raw (filter #(some #{"--raw"} %) @argvs)]
        (is (= [:read :read] (mapv :state (:shards evidence))))
        (is (= #{"kind-test"} (set (map #(subs (second %) (count "--context=")) @argvs))))
        (is (every? #(some #{"get"} %) @argvs) "every call is a kubectl get")
        (is (= [(path-of "prometheus-general-0") (path-of "prometheus-general-shard-1-0")]
               (mapv last raw)))
        (is (some #(and (some #{"pods"} %) (some #{"--selector=operator.prometheus.io/name"} %)) @argvs))))))

;; --- the hint seam ----------------------------------------------------------

(def node-target
  {:prometheus "monitoring/general" :shard 0 :scrape-pool "serviceMonitor/monitoring/node-exporter/0"
   :scrape-url "http://10.0.3.7:9100/metrics" :health "down" :labels {:job "node-exporter"}
   :last-error "Get \"http://10.0.3.7:9100/metrics\": dial tcp 10.0.3.7:9100: i/o timeout"})

(def pod-target
  (assoc node-target :scrape-url "http://10.0.4.9:8080/metrics" :labels {:job "api"}
         :last-error "Get \"http://10.0.4.9:8080/metrics\": dial tcp 10.0.4.9:8080: i/o timeout"))

(deftest a-casebook-hint-replaces-the-default-for-its-class-only
  (let [d (scrape/detective {:hints {:dropped (fn [t] (when (= "node-exporter" (get-in t [:labels :job]))
                                                        "the host policy decides a hostNetwork target"))
                                     :refused "estate refused hint"}})
        findings ((:detect d) {targets/evidence-key {:shards [] :targets [node-target pod-target]}})
        by-url (into {} (map (juxt #(re-find #"http://\S+" (:summary %)) :hint)) findings)]
    (is (str/includes? (by-url "http://10.0.3.7:9100/metrics") "the host policy decides a hostNetwork target"))
    (is (str/includes? (by-url "http://10.0.4.9:8080/metrics") (:dropped scrape/default-hints))
        "a hint function answering nil falls back to the default")
    (is (= "estate refused hint" (scrape/hint-for {:refused "estate refused hint"} :refused node-target)))
    (is (= (:above-transport scrape/default-hints) (scrape/hint-for {:refused "x"} :above-transport node-target)))))

(deftest extra-rules-add-a-class-without-reclassifying-a-known-error
  (let [d (scrape/detective {:extra-rules [{:class :mesh :pattern #"upstream connect error" :why "the mesh proxy refused"}
                                           {:class :mesh :pattern #"connection refused"}]
                             :hints {:mesh "look at the sidecar"}})
        findings ((:detect d) {targets/evidence-key
                               {:shards []
                                :targets [(assoc node-target :last-error "server returned HTTP status 503 upstream connect error")
                                          (assoc node-target :last-error "upstream connect error or disconnect")
                                          (assoc node-target :last-error "Get \"http://10.0.3.7:9100/metrics\": dial tcp 10.0.3.7:9100: connect: connection refused")]}})]
    (is (= [:above-transport :mesh :refused] (mapv :class findings)))
    (is (= "the mesh proxy refused; look at the sidecar" (:hint (second findings))))))

;; --- what a finding quotes --------------------------------------------------

(deftest cluster-supplied-text-is-printable-and-cut
  (let [hostile (assoc node-target
                       :scrape-url "http://10.0.3.7:9100/metrics\u001b[2J"
                       :last-error (str "Get \"x\": \u001b[31mred\u001b[0m\nforged line‮ " (apply str (repeat 2000 "a"))))
        [finding] ((:detect (scrape/detective)) {targets/evidence-key {:shards [] :targets [hostile]}})]
    (is (not (re-find #"[\x00-\x1f\x7f‮]" (:summary finding))))
    (is (< (count (:summary finding)) 600))
    (is (str/ends-with? (:summary finding) "…"))))

(deftest a-down-target-without-an-error-says-so
  (let [[finding] ((:detect (scrape/detective)) {targets/evidence-key {:shards [] :targets [(assoc node-target :last-error "")]}})]
    (is (= :unknown (:class finding)))
    (is (str/includes? (:summary finding) "no lastError reported"))))

;; --- registration -----------------------------------------------------------

(deftest the-detectives-join-the-opt-in-proxy-scope-with-their-collector
  (is (= #{"scrape-targets" "scrape-target-shards"} (set (map :name (registry/for-scope :proxy)))))
  (is (not-any? #{"scrape-targets" "scrape-target-shards"} (map :name (registry/for-scope :all)))
      "a whole-cluster scan does not ask for pods/proxy")
  (is (identical? targets/collector (registry/collector-for targets/evidence-key))))
