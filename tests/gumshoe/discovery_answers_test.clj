;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.discovery-answers-test
  "Reads of the API's discovery and extensions that did not answer stay
   apart from answers: `kubectl api-resources` that failed is nil and kept
   only for a short window, a custom resource type whose list failed is named as unread, and
   the capability detectors count only for the context they probe and next
   to an answered CRD read."
  (:require [cheshire.core :as cheshire]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.apiversions :as apiversions]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.apiversions :as apiversions-detectives]
            [gumshoe.detectives.custom-resources :as custom-resources]
            [gumshoe.detectives.expectations :as expectations]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]))

(def ^:private discovery
  (str "pods          po    v1                       true    Pod\n"
       "widgets             example.org/v1beta1      true    Widget\n"))

(defn- judged [check evidence]
  (let [checks (detective/mark-unread [check] evidence)]
    {:unread? (boolean (:unread (first checks)))
     :findings (mapv (juxt :severity :component :summary) (detective/run-detectives checks evidence))}))

;; --- api-resources ----------------------------------------------------------------

(deftest a-failed-discovery-is-no-answer
  (doseq [result [{:exit 1 :out "" :err "Unable to connect to the server: dial tcp: i/o timeout"}
                  {:exit 1 :out "" :err "Error from server (Forbidden)"}
                  {:exit 1 :out discovery
                   :err "error: unable to retrieve the complete list of server APIs: metrics.k8s.io/v1beta1: the server is currently unable to handle the request"}]]
    (testing (:err result)
      (is (nil? (apiversions/discovery-answer result)))
      (with-redefs [shell/execute (fn [& _] result)]
        (is (nil? (apiversions/advertised-versions "ctx")))))))

(deftest an-answered-discovery-parses
  (is (= {["" "pods"] "v1" ["example.org" "widgets"] "v1beta1"}
         (apiversions/discovery-answer {:exit 0 :out discovery}))))

(deftest discovery-is-bounded-and-names-the-context
  (let [argv (atom nil)]
    (with-redefs [shell/execute (fn [& args] (reset! argv args) {:exit 0 :out discovery})]
      (apiversions/advertised-versions "staging"))
    (is (some #{"--context=staging"} @argv))
    (is (some #(str/starts-with? % "--request-timeout=") @argv))))

(deftest version-drift-judges-nothing-on-a-failed-discovery
  (let [check (first (filter #(= "version-drift" (:name %)) apiversions-detectives/detectives))
        crds {:items [{:spec {:group "example.org" :names {:plural "widgets"}
                              :versions [{:name "v1" :served true}]}}]}]
    (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "timeout"})]
      (is (= {:unread? true :findings []}
             (judged check {"customresourcedefinitions" crds
                            "api-resources" (apiversions/advertised-versions "ctx")}))))
    (testing "the same CRD against an answered discovery is judged"
      (with-redefs [shell/execute (fn [& _] {:exit 0 :out discovery})]
        (is (= {:unread? false
                :findings [[:critical "example.org/widgets" "discovery advertises v1beta1 but the CRD serves only v1"]]}
               (judged check {"customresourcedefinitions" crds
                              "api-resources" (apiversions/advertised-versions "ctx")})))))))

;; --- resource kinds ---------------------------------------------------------------

(defn- with-empty-kind-cache [f]
  (let [cache @#'kubectl/resource-kinds-cache
        before @cache]
    (reset! cache nil)
    (try (f) (finally (reset! cache before)))))

(defn- answering
  "A shell/execute that counts its calls and answers each from results in
   turn, the last one again once they run out."
  [calls results]
  (let [left (atom results)]
    (fn [& _]
      (swap! calls inc)
      (let [r (first @left)]
        (when (next @left) (swap! left rest))
        r))))

(deftest a-failed-discovery-stands-for-a-while-then-is-asked-again
  (with-empty-kind-cache
    (fn []
      (let [calls (atom 0)
            clock (atom 0)]
        (with-redefs-fn {#'shell/execute (answering calls [{:exit 1 :out "" :err "timeout"}
                                                           {:exit 0 :out discovery}])
                         #'kubectl/now-ms (fn [] @clock)}
          (fn []
            (is (nil? (kubectl/resource-kind "pods")) "the failed discovery names no Kind")
            (is (nil? (kubectl/resource-kind "nodes")))
            (is (nil? (kubectl/resource-kind "widgets.example.org")))
            (is (= 1 @calls) "labels built together trigger one call against an unreachable API")
            (swap! clock + 60001)
            (is (= "Pod" (kubectl/resource-kind "pods")) "a later call asks again and gets the answer")
            (is (= "Widget" (kubectl/resource-kind "widgets.example.org")))
            (swap! clock + 600000)
            (is (= "Pod" (kubectl/resource-kind "pods")))
            (is (= 2 @calls) "a complete discovery is cached for the run")))))))

(deftest a-discovery-that-exits-0-with-nothing-is-asked-again-after-a-while
  (with-empty-kind-cache
    (fn []
      (let [calls (atom 0)
            clock (atom 0)]
        (with-redefs-fn {#'shell/execute (answering calls [{:exit 0 :out ""}])
                         #'kubectl/now-ms (fn [] @clock)}
          (fn []
            (is (= {} (kubectl/resource-kinds)))
            (is (= {} (kubectl/resource-kinds)))
            (is (= 1 @calls))
            (swap! clock + 60001)
            (is (= {} (kubectl/resource-kinds)))
            (is (= 2 @calls))))))))

(deftest a-partial-discovery-names-the-kinds-it-read
  (with-empty-kind-cache
    (fn []
      (let [calls (atom 0)
            clock (atom 0)
            half {:exit 1 :out discovery
                     :err "error: unable to retrieve the complete list of server APIs: metrics.k8s.io/v1beta1: the server is currently unable to handle the request"}]
        (with-redefs-fn {#'shell/execute (answering calls [half])
                         #'kubectl/now-ms (fn [] @clock)}
          (fn []
            (is (= "Pod" (kubectl/resource-kind "pods")) "one unavailable aggregated API does not hide the rest")
            (is (= "Widget" (kubectl/resource-kind "widgets.example.org")))
            (is (nil? (kubectl/resource-kind "nodes.metrics.k8s.io")))
            (is (= 1 @calls) "every label of one prerequisite list shares the call")
            (swap! clock + 60001)
            (kubectl/resource-kind "pods")
            (is (= 2 @calls) "an incomplete answer is not kept for the whole run")))))))

;; --- custom resources -------------------------------------------------------------

(def ^:private crds
  {:items (for [type ["widgets.example.org" "gadgets.example.org" "gizmos.example.org"]]
            {:metadata {:name type}
             :spec {:versions [{:name "v1" :served true :subresources {:status {}}}]}})})

(defn- kubectl-answering
  "A shell/execute that answers kubectl gets by type: an object list, an empty
   list or a failure."
  [by-type]
  (fn [& args]
    (let [type (some #(when (contains? by-type %) %) args)
          answer (get by-type type)]
      (if (= :fail answer)
        {:exit 1 :out "" :err "conversion webhook for example.org/v1 failed: Post \"https://conv.svc:443\": connection refused"}
        {:exit 0 :out (cheshire/generate-string answer)}))))

(def ^:private failing-widget
  {:kind "Widget" :apiVersion "example.org/v1" :metadata {:namespace "a" :name "w"}
   :status {:conditions [{:type "Ready" :status "False" :reason "Broken" :lastTransitionTime "2026-11-25T10:00:00Z"}]}})

(deftest a-custom-resource-type-whose-read-failed-is-named-unread
  (with-redefs [shell/execute (kubectl-answering {"customresourcedefinitions" crds
                                                  "widgets.example.org" {:items [failing-widget]}
                                                  "gadgets.example.org" {:items []}
                                                  "gizmos.example.org" :fail})]
    (let [read (kubectl/custom-resources "ctx" #{})]
      (is (= ["gizmos.example.org"] (:unread read)))
      (is (= ["w"] (map kubectl/name-of (:items read))))
      (testing "and the check reports it beside the findings of the types it read"
        (is (= [[:warning "a/w" "Widget is not Ready (Broken)"]
                [:warning "gizmos.example.org" "custom resources of this type could not be read"]]
               (map (juxt :severity :component :summary)
                    (custom-resources/detect-custom-resource-conditions
                     {:now (java.time.Instant/parse "2026-11-25T12:00:00Z") "custom-resources" read}))))))))

(deftest every-custom-resource-type-answering-names-none-unread
  (with-redefs [shell/execute (kubectl-answering {"customresourcedefinitions" crds
                                                  "widgets.example.org" {:items []}
                                                  "gadgets.example.org" {:items []}
                                                  "gizmos.example.org" {:items []}})]
    (is (= {:items [] :unread []} (kubectl/custom-resources "ctx" #{})))
    (is (empty? (custom-resources/detect-custom-resource-conditions
                 {:now (java.time.Instant/now) "custom-resources" (kubectl/custom-resources "ctx" #{})})))))

(deftest unlistable-crds-leave-the-custom-resources-unanswered
  (with-redefs [shell/execute (kubectl-answering {"customresourcedefinitions" :fail})]
    (is (nil? (kubectl/custom-resources "ctx" #{})))))

(deftest a-plain-list-of-custom-resources-still-reads
  (is (= ["Widget is not Ready (Broken)"]
         (map :summary (custom-resources/detect-custom-resource-conditions
                        {:now (java.time.Instant/parse "2026-11-25T12:00:00Z") "custom-resources" [failing-widget]})))))

;; --- capabilities -----------------------------------------------------------------

(defn- with-detectors [detectors f]
  (let [registry @#'capabilities/detectors
        before @registry]
    (reset! registry detectors)
    (try (f) (finally (reset! registry before)))))

(deftest a-detector-that-throws-is-unanswered-not-absent
  (with-detectors {:calico (fn [] (throw (ex-info "timeout" {})))
                   :flux (constantly false)
                   :ceph (constantly true)
                   :asserts (fn [] (assert false))}
    (fn []
      (is (= {:present [:ceph] :absent [:flux] :unanswered [:asserts :calico]} (capabilities/detect-answers)))
      (testing "detect-present, which the setup wizard reads, keeps counting a throw as absent"
        (is (= [:ceph] (capabilities/detect-present)))))))

(deftest the-detectors-answer-only-for-the-current-context
  (with-detectors {:ceph (constantly true)}
    (fn []
      (is (= {:present [:ceph] :absent [] :unanswered []} (registry/capability-answers "prod" "prod")))
      (is (nil? (registry/capability-answers "staging" "prod")))
      (testing "the registered collector compares with the current context"
        (with-redefs [kubectl/current-context (constantly "prod")]
          (is (nil? ((registry/collector-for "capabilities") "staging" "capabilities")))
          (is (= [:ceph] (:present ((registry/collector-for "capabilities") "prod" "capabilities")))))))))

(def ^:private expected-capabilities (first expectations/detectives))

(def ^:private answered
  {"config" {:capabilities [:calico :flux :ceph :gateway-api]}
   "capabilities" {:present [:ceph] :absent [:flux :velero] :unanswered [:calico]}
   "customresourcedefinitions" {:items []}})

(deftest only-an-answered-absence-is-an-outage
  (is (= {:unread? false
          :findings [[:critical "flux" "declared cluster capability is not present"]
                     [:warning "calico" "could not tell whether the declared cluster capability is present"]]}
         (judged expected-capabilities answered))))

(deftest without-the-crd-read-or-the-answers-nothing-is-judged
  (doseq [read ["customresourcedefinitions" "capabilities"]]
    (testing (str read " did not answer")
      (is (= {:unread? true :findings []} (judged expected-capabilities (assoc answered read nil))))
      (is (empty? ((:detect expected-capabilities) (assoc answered read nil)))))))

(deftest an-unreachable-api-server-is-no-outage-of-every-capability
  ;; every detector answers absent against an API server that does not
  ;; answer, and the CRD read fails with it
  (is (= {:unread? true :findings []}
         (judged expected-capabilities (assoc answered
                                              "capabilities" {:present [] :absent [:calico :flux :ceph] :unanswered []}
                                              "customresourcedefinitions" nil)))))

(deftest nothing-declared-is-nothing-checked
  (is (= {:unread? false :findings []}
         (judged expected-capabilities (assoc answered "config" {})))))
