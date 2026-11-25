;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.rightsizing-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.http :as http]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.promql :as promql]
            [gumshoe.rightsizing :as rs]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]))

(def ^:private mi 1048576.0)

;; --- queries ---------------------------------------------------------------------

(deftest queries-read-cadvisor-metrics-per-container
  (let [cpu (rs/cpu-query "7d" nil)
        memory (rs/memory-query "3d" "shop")]
    (is (= "quantile_over_time(0.95, sum by (namespace, pod, container) (rate(container_cpu_usage_seconds_total{container!=\"\",container!=\"POD\"}[5m]))[7d:1m])" cpu))
    (is (= "max_over_time(sum by (namespace, pod, container) (container_memory_working_set_bytes{container!=\"\",container!=\"POD\",namespace=\"shop\"})[3d:1m])" memory))))

(defn- vector-response [& rows]
  {:status "success"
   :data {:resultType "vector"
          :result (vec (for [[namespace pod container value] rows]
                         {:metric {:namespace namespace :pod pod :container container} :value [1763553600.0 value]}))}})

(deftest samples-parse-an-instant-vector
  (is (= {["shop" "web-1" "app"] 0.25 ["shop" "web-2" "app"] 0.5}
         (rs/samples (vector-response ["shop" "web-1" "app" "0.25"] ["shop" "web-2" "app" "0.5"]
                                      ["shop" "web-3" "app" "NaN"] ["shop" "web-4" "app" "garbage"]))))
  (testing "series without a pod or container are dropped"
    (is (empty? (rs/samples {:status "success" :data {:resultType "vector" :result [{:metric {:namespace "x"} :value [1 "1"]}]}}))))
  (testing "errors and other result types yield nothing"
    (is (empty? (rs/samples {:status "error" :error "bad query"})))
    (is (empty? (rs/samples {:status "success" :data {:resultType "matrix" :result []}})))
    (is (empty? (rs/samples nil)))))

;; --- usage rows ------------------------------------------------------------------

(defn- pod [name & {:keys [owner hash phase containers] :or {phase "Running"}}]
  {:metadata (cond-> {:namespace "shop" :name name}
               owner (assoc :ownerReferences [{:kind (first owner) :name (second owner) :controller true}])
               hash (assoc :labels {:pod-template-hash hash}))
   :spec {:containers (vec (for [[cname cpu memory] containers]
                             {:name cname :resources {:requests (cond-> {} cpu (assoc :cpu cpu) memory (assoc :memory memory))}}))
          :initContainers [{:name "init" :resources {:requests {:cpu "4"}}}]}
   :status {:phase phase}})

(deftest usage-rows-group-replicas-by-workload
  (let [pods {:items [(pod "web-7d9f-a" :owner ["ReplicaSet" "web-7d9f"] :hash "7d9f" :containers [["app" "500m" "512Mi"] ["sidecar" "50m" nil]])
                      (pod "web-7d9f-b" :owner ["ReplicaSet" "web-7d9f"] :hash "7d9f" :containers [["app" "500m" "512Mi"] ["sidecar" "50m" nil]])
                      (pod "done" :phase "Succeeded" :containers [["app" "1" "1Gi"]])
                      (pod "no-usage" :containers [["app" "1" "1Gi"]])]}
        cpu {["shop" "web-7d9f-a" "app"] 0.1 ["shop" "web-7d9f-b" "app"] 0.3 ["shop" "web-7d9f-a" "sidecar"] 0.02
             ["shop" "done" "app"] 0.9 ["shop" "web-7d9f-a" "init"] 3.0}
        memory {["shop" "web-7d9f-a" "app"] (* 100 mi) ["shop" "web-7d9f-b" "app"] (* 300 mi)}
        rows (rs/usage-rows pods cpu memory)]
    (is (= [{:namespace "shop" :workload "Deployment/web" :container "app" :pods 2
             :cpu-request 0.5 :memory-request (* 512 mi) :cpu 0.3 :memory (* 300 mi)}
            {:namespace "shop" :workload "Deployment/web" :container "sidecar" :pods 1
             :cpu-request 0.05 :memory-request nil :cpu 0.02 :memory nil}]
           rows)
        "the busiest replica wins; :pods counts replicas with usage; finished pods, init containers and pods without usage are left out")))

;; --- recommendations -------------------------------------------------------------

(deftest recommendations-add-headroom-and-round-up
  (is (= 0.345 (rs/recommend-cpu 0.3)) "0.3 × 1.15 = 0.345, already a 5m step")
  (is (= 0.35 (rs/recommend-cpu 0.301)) "0.346 rounds up to 350m")
  (is (= 0.01 (rs/recommend-cpu 0.001)) "at least 10m")
  (is (nil? (rs/recommend-cpu nil)))
  (is (= (* 345 mi) (rs/recommend-memory (* 300 mi))))
  (is (= (* 16 mi) (rs/recommend-memory (* 2 mi))) "at least 16Mi")
  (is (= (* 116 mi) (rs/recommend-memory (* 100.5 mi))) "115.6Mi rounds up to a whole mebibyte")
  (is (nil? (rs/recommend-memory nil))))

(deftest verdicts
  (testing "cpu"
    (is (= :missing (rs/verdict :cpu nil 0.2 0.25)))
    (is (= :under (rs/verdict :cpu 0.1 0.2 0.25)))
    (is (= :over (rs/verdict :cpu 1.0 0.2 0.25)))
    (is (= :fits (rs/verdict :cpu 0.3 0.2 0.25)))
    (is (= :fits (rs/verdict :cpu 0.05 0.01 0.02)) "twice the recommendation, but under 100m of waste"))
  (testing "memory"
    (is (= :over (rs/verdict :memory (* 1024 mi) (* 100 mi) (* 115 mi))))
    (is (= :fits (rs/verdict :memory (* 64 mi) (* 10 mi) (* 16 mi))) "under 128Mi of waste")
    (is (= :under (rs/verdict :memory (* 256 mi) (* 300 mi) (* 345 mi)))))
  (testing "no usage, no verdict"
    (is (nil? (rs/verdict :cpu 0.5 nil nil)))))

(deftest judging-a-row
  (let [row (rs/judge {:namespace "shop" :workload "Deployment/web" :container "app" :pods 2
                       :cpu-request 2.0 :memory-request (* 256 mi) :cpu 0.3 :memory (* 300 mi)})]
    (is (= [0.345 (* 345 mi) :over :under]
           ((juxt :cpu-recommendation :memory-recommendation :cpu-verdict :memory-verdict) row)))
    (is (rs/actionable? row)))
  (is (not (rs/actionable? (rs/judge {:cpu-request 0.4 :memory-request (* 400 mi) :cpu 0.3 :memory (* 300 mi)}))))
  (is (rs/actionable? (rs/judge {:cpu-request nil :memory-request (* 400 mi) :cpu 0.3 :memory (* 300 mi)})) "an unset request"))

;; --- formatting ------------------------------------------------------------------

(deftest formatting
  (is (= "350m" (rs/format-cpu 0.35)))
  (is (= "2000m" (rs/format-cpu 2.0)))
  (is (= "-" (rs/format-cpu nil)))
  (is (= "345Mi" (rs/format-memory (* 345 mi))))
  (is (= "1Mi" (rs/format-memory 1000.0)))
  (is (= "-" (rs/format-memory nil))))

(deftest patch-lines
  (is (= "kubectl -n shop set resources deployment/web -c app --requests=cpu=350m,memory=345Mi"
         (rs/patch-line {:namespace "shop" :workload "Deployment/web" :container "app"
                         :cpu-recommendation 0.35 :memory-recommendation (* 345 mi)})))
  (is (= "kubectl -n shop set resources statefulset/db -c pg --requests=memory=1024Mi"
         (rs/patch-line {:namespace "shop" :workload "StatefulSet/db" :container "pg" :memory-recommendation (* 1024 mi)})))
  (is (nil? (rs/patch-line {:namespace "shop" :workload "Pod/debug" :container "x" :cpu-recommendation 0.1}))
      "a bare pod has no template")
  (is (str/includes? (rs/patch-line {:namespace "jobs" :workload "Job/backup" :container "x" :cpu-recommendation 0.1})
                     "job/backup")))

;; --- the book --------------------------------------------------------------------

(def ^:private right-size-book
  (binding [runbook/*loading* true]
    (load-file "tools/prometheus/runbooks/right_size.clj")))

(defn- run-right-size
  "Runs the book against a Prometheus answering an empty vector and a cluster
   answering the pods read with pods-result. Returns the result, or the
   message of what it threw, and what it printed."
  [opts pods-result]
  (let [printed (java.io.StringWriter.)
        reads (atom [])]
    (with-redefs [http/fetch (fn [& _] {:reachable true :status 200
                                        :json {:status "success" :data {:resultType "vector" :result []}}})
                  promql/credentials (fn [& _] {})
                  kubectl/current-context (constantly "ctx")
                  shell/execute (fn [& argv] (swap! reads conj (vec argv)) pods-result)]
      (let [result (binding [*out* printed *err* printed]
                     (try ((:action right-size-book) (merge {:prometheus "https://prometheus.example.org" :window "7d"} opts) {})
                          (catch clojure.lang.ExceptionInfo e (str "threw: " (ex-message e)))))]
        {:result result :out (str printed) :reads @reads}))))

(deftest an-unanswered-pods-read-is-not-an-empty-cluster
  (let [{:keys [result out]} (run-right-size {} {:exit 1 :out "" :err "Unable to connect to the server"})]
    (is (= "threw: could not list the pods - the cluster did not answer" result))
    (is (not (str/includes? out "no container usage")))))

(deftest an-answered-empty-pods-read-reports-no-usage
  (let [{:keys [result out reads]} (run-right-size {:namespace "shop"}
                                                   {:exit 0 :out (json/generate-string {:items []}) :err ""})]
    (is (true? result))
    (is (str/includes? out "no container usage over 7d that matches a running pod"))
    (is (some #(some #{"--namespace=shop"} %) reads) "the namespace narrows the read")))

;; --- unparseable requests --------------------------------------------------------

(deftest an-unparseable-request-is-not-an-unset-one
  (let [pods {:items [(pod "web-a" :owner ["ReplicaSet" "web-1"] :hash "1" :containers [["app" "lots" "512Mi"]])
                      (pod "web-b" :owner ["ReplicaSet" "web-1"] :hash "1" :containers [["app" "500m" "512Mi"]])
                      (pod "db-0" :containers [["pg" nil "1 Gi"]])]}
        usage {["shop" "web-a" "app"] 0.3 ["shop" "web-b" "app"] 0.2 ["shop" "db-0" "pg"] 0.1}
        memory {["shop" "web-a" "app"] (* 400 mi) ["shop" "db-0" "pg"] (* 100 mi)}
        rows (rs/usage-rows pods usage memory)
        by-container (into {} (map (juxt :container identity)) rows)]
    (testing "a replica whose request is not a quantity marks that resource unreadable for the workload"
      (is (= #{:cpu} (:unreadable-requests (by-container "app"))))
      (is (= #{:memory} (:unreadable-requests (by-container "pg")))))
    (testing "the verdict names it unreadable, never unset"
      (let [app (rs/judge (by-container "app"))
            pg (rs/judge (by-container "pg"))]
        (is (= :unreadable (:cpu-verdict app)))
        (is (= :fits (:memory-verdict app)))
        (is (= :missing (:cpu-verdict pg)) "an absent request stays unset")
        (is (= :unreadable (:memory-verdict pg)))
        (is (rs/actionable? app))))
    (testing "the table shows ? for an unreadable request and - for an unset one"
      (is (= "?" (rs/format-request (by-container "app") :cpu)))
      (is (= "512Mi" (rs/format-request (by-container "app") :memory)))
      (is (= "-" (rs/format-request (by-container "pg") :cpu)))
      (is (= "?" (rs/format-request (by-container "pg") :memory)))))
  (testing "a row without usage has no verdict, readable or not"
    (is (nil? (:cpu-verdict (rs/judge {:unreadable-requests #{:cpu} :cpu nil}))))))

(deftest the-book-reports-an-unparseable-request-as-unreadable
  (let [printed (java.io.StringWriter.)
        answer {:status "success" :data {:resultType "vector"
                                         :result [{:metric {:namespace "shop" :pod "web-a" :container "app"} :value [1 "0.3"]}]}}
        pods {:items [(pod "web-a" :owner ["ReplicaSet" "web-1"] :hash "1" :containers [["app" "lots" "512Mi"]])]}]
    (with-redefs [http/fetch (fn [& _] {:reachable true :status 200 :json answer})
                  promql/credentials (fn [& _] {})
                  kubectl/current-context (constantly "ctx")
                  shell/execute (fn [& _] {:exit 0 :out (json/generate-string pods) :err ""})]
      (let [result (binding [*out* printed *err* printed]
                     ((:action right-size-book) {:prometheus "https://prometheus.example.org" :window "7d"} {}))
            out (str printed)]
        (is (true? result))
        (is (str/includes? out "unreadable"))
        (is (not (str/includes? out "unset")))))))
