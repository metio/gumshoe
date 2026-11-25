;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.engine-resilience-test
  "The engine keeps judging, and keeps failing closed, when a detective, a
   collector, a post-check, a plugin, a prerequisite builder, a book file or
   env.edn is not what it should be."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.catalog :as catalog]
            [gumshoe.command]
            [gumshoe.config :as config]
            [gumshoe.conformance :as conformance]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.dsl]
            [gumshoe.edn-book :as edn-book]
            [gumshoe.edn-reader :as reader]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugins :as plugins]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.runbook]
            [gumshoe.spec :as spec]
            [gumshoe.verify :as verify]))

(defn- quietly
  "The value of (f) with stdout and stderr captured, as [value out err]."
  [f]
  (let [out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        value (binding [*out* out *err* err] (f))]
    [value (str out) (str err)]))

(def ^:private fine
  {:name "fine" :description "a check that judges" :requires []
   :detect (fn [_] [{:severity :info :component "x" :summary "ok"}])})

;; --- crashed detectives -------------------------------------------------------

(deftest an-error-from-a-detective-keeps-every-sibling-finding
  (doseq [[what thrown] [["an assert" #(assert false "never")]
                         ["a :pre condition" #((fn [x] {:pre [(pos? x)]} x) -1)]
                         ["a stack overflow" #(letfn [(f [n] (inc (f n)))] (f 0))]]]
    (testing what
      (let [findings (detective/run-detectives [{:name "boom" :requires [] :detect (fn [_] (thrown))} fine]
                                               {})]
        (is (some #(= "ok" (:summary %)) findings) "the sibling's finding survives")
        (is (= ["boom"] (detective/crashed-detectives findings)))))))

(deftest a-run-whose-only-detective-crashed-fails-in-every-format
  (let [boom {:name "boom" :description "throws" :requires ["pods"]
              :detect (fn [_] (throw (ex-info "kaboom" {})))}
        evidence {"pods" {:items [{:metadata {:name "p"}}]}}
        checked (detective/mark-unread [boom] evidence)
        findings (detective/run-detectives checked evidence)]
    (testing "the crash is a critical finding naming the detective"
      (is (= [:critical] (map :severity findings)))
      (is (= "kaboom" (:hint (first findings)))))
    (testing "json"
      (let [[healthy out] (quietly #(detective/report! checked findings "json"))
            data (json/parse-string out true)]
        (is (false? healthy))
        (is (= ["boom"] (:crashed data)))
        (is (= 1 (get-in data [:summary :critical])))))
    (testing "edn"
      (let [[healthy out] (quietly #(detective/report! checked findings "edn"))]
        (is (false? healthy))
        (is (= ["boom"] (:crashed (read-string out))))))
    (testing "text names the check that could not run"
      (let [[healthy _ err] (quietly #(detective/report! checked findings "text"))]
        (is (false? healthy))
        (is (str/includes? err "1 check could not run, so it judged nothing - boom"))))))

(deftest a-crash-blocks-a-playbook-check-step
  (let [boom {:name "boom" :description "throws" :requires [] :detect (fn [_] (throw (Error. "bad")))}
        step (detective/check-step {:title "t" :detectives [boom fine]})]
    (with-redefs [kubectl/current-cluster (constantly "c")]
      (is (false? (first (quietly #((:check step) {:context "ctx"}))))))))

;; --- severities -------------------------------------------------------------

(deftest a-finding-severity-is-normalised-and-an-unknown-one-is-critical
  (is (= :critical (:severity (detective/normalize-finding {:severity "critical"}))))
  (is (= :warning (:severity (detective/normalize-finding {:severity " Warning "}))))
  (is (= :info (:severity (detective/normalize-finding {:severity :info}))))
  (doseq [odd [:error nil 3 "fatal"]]
    (testing (pr-str odd)
      (let [f (detective/normalize-finding {:severity odd :hint "h"})]
        (is (= :critical (:severity f)))
        (is (str/includes? (:hint f) (pr-str odd)))
        (is (str/ends-with? (:hint f) " - h"))))))

(deftest a-string-or-unknown-severity-fails-the-report-in-every-format
  (let [detectives [{:name "d" :description "d"}]]
    (doseq [severity ["critical" :error nil]]
      (testing (pr-str severity)
        (let [findings [{:severity severity :detective "d" :component "x" :summary "s"}]
              [healthy out] (quietly #(detective/report! detectives findings "json"))
              data (json/parse-string out)]
          (is (false? healthy))
          (is (= 1 (get-in data ["summary" "critical"])))
          (is (= 3 (count (get data "summary"))) "one key per severity, no duplicate"))
        (let [findings [{:severity severity :detective "d" :component "x" :summary "s"}]
              [healthy out] (quietly #(detective/report! detectives findings "text"))]
          (is (false? healthy) "the text report renders instead of throwing")
          (is (str/includes? out "CRITICAL")))))))

;; --- evidence collection ----------------------------------------------------

(deftest a-blocking-collector-counts-as-unread-instead-of-hanging-the-scan
  (let [never (promise)
        detectives [{:name "slow" :requires ["slow-key"] :detect (fn [_] [])}
                    {:name "quick" :requires ["quick-key"] :detect (fn [_] [])}]
        start (System/currentTimeMillis)]
    (with-redefs [kubectl/current-cluster (constantly "c")
                  registry/collector-for (fn [key]
                                           (if (= "slow-key" key)
                                             (fn [_ _] @never)
                                             (fn [_ _] {:items [{:a 1}]})))]
      (let [[evidence _ err] (binding [detective/*collector-timeout-ms* 300]
                               (quietly #(detective/collect-evidence! "ctx" detectives)))]
        (is (< (- (System/currentTimeMillis) start) 5000))
        (is (nil? (get evidence "slow-key")))
        (is (= {:items [{:a 1}]} (get evidence "quick-key")))
        (is (str/includes? err "slow-key"))
        (is (= [true nil] (map :unread (detective/mark-unread detectives evidence))))))))

(deftest a-collector-that-throws-an-error-counts-as-unread
  (with-redefs [kubectl/current-cluster (constantly "c")
                registry/collector-for (fn [_] (fn [_ _] (throw (StackOverflowError.))))]
    (let [[evidence] (quietly #(detective/collect-evidence! "ctx" [{:name "d" :requires ["k"] :detect (fn [_] [])}]))]
      (is (contains? evidence "k"))
      (is (nil? (get evidence "k"))))))

(deftest read-evidence-names-a-collector-that-did-not-return-and-passes-exceptions-through
  (let [never (promise)]
    (with-redefs [registry/collector-for (fn [_] (fn [_ _] @never))]
      (let [e (try (binding [detective/*collector-timeout-ms* 200]
                     (detective/read-evidence "ctx" "c" [{:name "d" :requires ["k"]}]))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (some? e))
        (is (= "k" (::detective/timed-out (ex-data e))))
        (is (str/includes? (ex-message e) "k")))))
  (with-redefs [registry/collector-for (fn [_] (fn [_ _] (throw (ex-info "unread" {::marker true}))))]
    (let [e (try (detective/read-evidence "ctx" "c" [{:name "d" :requires ["k"]}]) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (true? (::marker (ex-data e))) "the collector's own exception reaches the caller")))
  (with-redefs [registry/collector-for (fn [_] (fn [_ _] {:items []}))]
    (is (= {:items []} (get (detective/read-evidence "ctx" "c" [{:name "d" :requires ["k"]}]) "k")))))

;; --- json --------------------------------------------------------------------

(deftest an-unencodable-value-is-written-as-its-string-form
  (let [odd (Object.)
        findings [{:severity :warning :detective "d" :component "x" :summary "s" :odd odd}]
        [healthy out] (quietly #(detective/report! [{:name "d"}] findings "json" {:rows [odd (fn [])]}))
        data (json/parse-string out true)]
    (is (true? healthy))
    (is (= (str odd) (:odd (first (:findings data)))))
    (is (= 2 (count (:rows data))))
    (is (every? string? (:rows data)))))

(deftest an-encodable-report-is-written-unchanged
  (let [data {:findings [{:since (java.time.Instant/parse "2026-11-25T00:00:00Z")}] :n 1}]
    (is (= (json/generate-string data {:pretty true}) (detective/json-report data)))))

;; --- book discovery ----------------------------------------------------------

(deftest a-deeply-nested-book-file-never-breaks-discovery
  (fs/with-temp-dir [dir {}]
    (let [file (str (fs/path dir "deep.clj"))]
      (spit file (str (apply str (repeat 200000 "(")) (apply str (repeat 200000 ")"))))
      (let [book (catalog/book-at file)]
        (is (= file (:path book)))
        (is (nil? (:description book))))
      (let [parsed (conformance/parse file)]
        (is (string? (:error parsed)))
        (is (nil? (:forms parsed)))))))

(deftest a-front-door-suffix-names-the-book-of-that-id-before-a-longer-path
  (let [catalog [{:id "products/shop/runbooks/investigate.clj" :path "/cb/products/shop/runbooks/investigate.clj"}
                 {:id "runbooks/investigate.clj" :path "/gitlibs/gumshoe/runbooks/investigate.clj"}
                 {:id "mytools/ceph/runbooks/scan.clj" :path "/cb/mytools/ceph/runbooks/scan.clj"}
                 {:id "tools/ceph/runbooks/scan.clj" :path "/gitlibs/gumshoe/tools/ceph/runbooks/scan.clj"}
                 {:id "xruns/a.clj" :path "/cb/xruns/a.clj"}]]
    (is (= "/gitlibs/gumshoe/runbooks/investigate.clj" (catalog/resolve-path "runbooks/investigate.clj" catalog)))
    (is (= "/gitlibs/gumshoe/tools/ceph/runbooks/scan.clj" (catalog/resolve-path "tools/ceph/runbooks/scan.clj" catalog)))
    (testing "a suffix matches at a directory boundary only"
      (is (= "runs/a.clj" (catalog/resolve-path "runs/a.clj" catalog))))
    (testing "without an exact id the shortest boundary match wins"
      (is (= "/gitlibs/gumshoe/runbooks/investigate.clj"
             (catalog/resolve-path "investigate.clj" catalog))))))

;; --- EDN books ----------------------------------------------------------------

(def ^:private header
  (str ";; SPDX-" "FileCopyrightText: The gumshoe Authors\n;; SPDX-" "License-Identifier: 0BSD\n"))

(deftest an-edn-book-whose-values-is-no-map-is-refused-naming-the-path
  (let [source (reader/read-book "tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn")]
    (fs/with-temp-dir [dir {}]
      (doseq [values [5 :x true "s" [1 2]]]
        (testing (pr-str values)
          (let [file (str (fs/path dir "book.edn"))]
            (spit file (str header (pr-str (assoc source :values values))))
            (let [e (try (edn-book/load-book file) nil (catch clojure.lang.ExceptionInfo e e))]
              (is (reader/refused? e))
              (is (str/includes? (ex-message e) "[:values]")))))))))

(deftest an-edn-book-compile-trips-over-is-refused-not-thrown
  (fs/with-temp-dir [dir {}]
    (let [file (str (fs/path dir "book.edn"))]
      (spit file (str header (pr-str (reader/read-book "tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn"))))
      (with-redefs [gumshoe.dsl/compile (fn [_] (throw (IllegalArgumentException. "odd")))]
        (let [e (try (edn-book/load-book file) nil (catch clojure.lang.ExceptionInfo e e))]
          (is (reader/refused? e))
          (is (= "does not compile: odd" (:reason (ex-data e))))))
      (with-redefs [gumshoe.dsl/compile (fn [_] (throw (ex-info "not invalid" {})))]
        (let [[code _ err] (quietly #(edn-book/run file []))]
          (is (= 1 code))
          (is (str/includes? err "refused")))))))

;; --- plugins --------------------------------------------------------------------

(deftest a-plugins-value-of-any-shape-never-stops-the-core
  (is (= {:names ['acme.plugin]} (plugins/plugin-names 'acme.plugin)))
  (is (= {:names ["acme.plugin"]} (plugins/plugin-names "acme.plugin")))
  (is (= {:names []} (plugins/plugin-names nil)))
  (is (= {:names ['a 'b]} (plugins/plugin-names '[a b])))
  (doseq [odd [42 {:a 1} :kw]]
    (testing (pr-str odd)
      (let [{:keys [names warnings]} (plugins/plugin-names odd)]
        (is (= [] names))
        (is (= 1 (count warnings))))))
  (testing "a single symbol loads as one plugin and warns once when it cannot"
    (let [[_ out err] (quietly #(plugins/load! 'gumshoe.no-such-plugin-xyz))
          printed (str out err)]
      (is (= 1 (count (re-seq #"could not load plugin" printed))))
      (is (str/includes? printed "gumshoe.no-such-plugin-xyz"))))
  (testing "a string loads as one plugin, not one per character"
    (let [[_ out err] (quietly #(plugins/load! "gumshoe.no-such-plugin-xyz"))]
      (is (= 1 (count (re-seq #"could not load plugin" (str out err))))))))

;; --- prerequisite builders --------------------------------------------------------

(deftest a-prerequisite-builder-that-throws-fails-its-own-item
  (let [items #(#'gumshoe.runbook/prerequisite-items % {})]
    (prerequisites/register-check! :engine-resilience-test-throws (fn [_ _] (throw (AssertionError. "no config"))))
    (prerequisites/register-check! :engine-resilience-test-fine (fn [_ _] [["fine" (constantly true)]]))
    (try
      (let [built (items {:engine-resilience-test-throws true :engine-resilience-test-fine true})
            results (into {} (map (fn [[label thunk]] [label (thunk)])) built)]
        (is (true? (results "fine")) "the other registered check still runs")
        (let [[label result] (first (filter #(str/includes? (first %) "engine-resilience-test-throws") results))]
          (is (some? label))
          (is (false? (:ok? result)))
          (is (str/includes? (:label result) "no config"))))
      (finally
        (swap! @#'prerequisites/checks dissoc :engine-resilience-test-throws :engine-resilience-test-fine)))))

(deftest a-tool-prerequisite-builder-that-throws-fails-its-own-item
  (with-redefs [gumshoe.command/tool-prerequisites (fn [t] (when (= "acme" t) (fn [_] (throw (Exception. "boom")))))
                gumshoe.command/tool-min-version (constantly nil)]
    (let [built (#'gumshoe.runbook/prerequisite-items {:installed-tools ["acme"]} {})
          failing (filter #(str/includes? (first %) "the prerequisites of acme") built)]
      (is (= 1 (count failing)))
      (is (false? (:ok? ((second (first failing)))))))))

;; --- env.edn shapes ----------------------------------------------------------------

(deftest normalize-drops-every-part-of-the-wrong-shape-and-names-it
  (testing "a well-shaped config is unchanged"
    (let [c {:environments {:p {:select {:kubernetes-cluster "k"} :vpn {:interface "wg0"}}} :defaults {:x 1}}]
      (is (= {:config c :warnings []} (config/normalize c)))))
  (testing "a string :select is dropped from its environment"
    (let [{c :config w :warnings} (config/normalize {:environments {:p {:select "kube.example.org" :x 1}}})]
      (is (= {:environments {:p {:x 1}}} c))
      (is (= 1 (count w)))
      (is (str/includes? (first w) ":select"))))
  (testing "a vector :environments is dropped"
    (let [{c :config w :warnings} (config/normalize {:environments [{:select {}}] :vpn 1})]
      (is (= {:vpn 1} c))
      (is (= 1 (count w)))))
  (testing "a non-map environment and a non-map :defaults are dropped"
    (let [{c :config w :warnings} (config/normalize {:environments {:a "x" :b {:y 1}} :defaults [1]})]
      (is (= {:environments {:b {:y 1}}} c))
      (is (= 2 (count w))))))

(deftest a-malformed-env-file-degrades-instead-of-crashing-every-reader
  (fs/with-temp-dir [dir {}]
    (doseq [[what content] [["a string :select" "{:environments {:prod {:select \"kube.example.org\"}} :dns {:server \"d\"}}"]
                            ["a vector :environments" "{:environments [{:select {:kubernetes-cluster \"k\"}}] :dns {:server \"d\"}}"]
                            ["a string :environments" "{:environments \"prod\" :dns {:server \"d\"}}"]
                            ["a vector :defaults" "{:defaults [1] :dns {:server \"d\"}}"]]]
      (testing what
        (let [file (str (fs/path dir "env.edn"))
              _ (spit file content)
              [loaded _ err] (quietly #(config/load-file-safely file))]
          (is (map? loaded))
          (is (str/includes? err "warning"))
          (with-redefs [config/all (constantly loaded)]
            (is (= "d" (config/env-value {:kubernetes-cluster "kube.example.org"} [:dns :server])))
            (is (seq? (config/known-clusters)))
            (is (map? (config/active-config {:kubernetes-cluster "k"})))))))))

(deftest the-pure-readers-tolerate-a-raw-malformed-config
  (is (nil? (config/select-environment [{:select {}}] {:kubernetes-cluster "k"})))
  (is (nil? (config/select-environment {:p {:select "k"}} {:kubernetes-cluster "k"})))
  (is (nil? (config/select-environment {:p "x"} {:kubernetes-cluster "k"})))
  (is (= [] (vec (config/known-clusters {:environments [{:select {:kubernetes-cluster "k"}}]}))))
  (is (= ["a"] (vec (config/known-clusters {:environments {:p "x"} :clusters ["a"]}))))
  (is (= [] (vec (config/known-clusters {:clusters 5}))))
  (is (= {:x 1} (config/active-config {:x 1 :defaults [1] :environments [2]} :p))))

(deftest env-config-problems-reports-instead-of-throwing-on-any-environments-shape
  (doseq [c [{:environments [{:select {:kubernetes-cluster "k"}}]}
             {:environments "should-be-a-map"}
             {:environments {:p "x"}}
             {:environments {:p {:select 5}}}
             {:environments {:p {:select "kube.example.org"}}}]]
    (testing (pr-str c)
      (let [problems (spec/env-config-problems c)]
        (is (seq problems))
        (is (every? string? problems)))))
  (is (= [":environments -> :p has no :select, so it is only usable via GUMSHOE_ENVIRONMENT"]
         (vec (spec/env-config-problems {:environments {:p {:vpn {:interface "wg0"}}}})))))

;; --- post-checks ------------------------------------------------------------------

(deftest a-slow-check-is-bounded-by-the-wall-clock-timeout
  (let [start (System/currentTimeMillis)
        [held] (quietly #(verify/eventually {:description "slow" :timeout 2 :interval 1
                                              :check (fn [] (Thread/sleep 1500) false)}))
        took (- (System/currentTimeMillis) start)]
    (is (false? held))
    (is (< took 5000) (str "took " took " ms"))))

(deftest a-hanging-check-ends-the-wait-at-most-one-interval-late
  (let [never (promise)
        start (System/currentTimeMillis)
        [held _ err] (quietly #(verify/eventually {:description "hangs" :timeout 1 :interval 1
                                                    :check (fn [] @never)}))
        took (- (System/currentTimeMillis) start)]
    (is (false? held))
    (is (< took 4000) (str "took " took " ms"))
    (is (str/includes? err "did not return within"))))

(deftest a-zero-timeout-still-runs-the-check-once
  (let [calls (atom 0)
        [held] (quietly #(verify/eventually {:description "once" :timeout 0
                                              :check (fn [] (swap! calls inc) true)}))]
    (is (true? held))
    (is (= 1 @calls))))

(deftest an-abort-is-read-once-per-attempt
  (let [calls (atom 0)
        abort (fn [] (if (= 1 (swap! calls inc)) "the job failed" (throw (ex-info "gone" {}))))
        [held _ err] (quietly #(verify/eventually {:description "d" :timeout 10 :interval 1
                                                    :check (constantly false) :abort abort}))]
    (is (false? held))
    (is (= 1 @calls))
    (is (str/includes? err "NOT verified: d - the job failed"))))
