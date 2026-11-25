;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.read-all-books-test
  "The hand-written books that report on a list of every object of a type,
   run whole against a kubectl that answers, answers with nothing, does not
   answer, or answers that the resource type does not exist. A read that did
   not answer is reported as such and fails the run; only an answered empty
   list is an all-clear."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(defn- answering
  [items]
  (fn [& _] {:exit 0 :out (json/generate-string {:items items}) :err ""}))

(defn- unanswered
  [& _]
  {:exit 1 :out "" :err "Error from server (ServiceUnavailable): the server is currently unable to handle the request"})

(defn- no-such-type
  [& _]
  {:exit 1 :out "" :err "error: the server doesn't have a resource type \"horizontalpodautoscalers\""})

(defn- book
  [path]
  (binding [runbook/*loading* true] (load-file path)))

(defn- run-action
  "{:result <what the action returned> :out <stdout and stderr>} of book's
   action with opts, against kubectl answering through execute."
  [config execute opts]
  (let [out (java.io.StringWriter.)]
    (with-redefs [shell/execute execute
                  kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")]
      (let [result (binding [*out* out *err* out]
                     ((:action config) opts {}))]
        {:result result :out (stdout/strip-colors (str out))}))))

(def ^:private pod-pulling
  {:metadata {:namespace "shop" :name "web-1"}
   :spec {:containers [{:name "web" :image "registry.example.org/web:1"}]}
   :status {:containerStatuses [{:name "web" :state {:waiting {:reason "ImagePullBackOff"}}}]}})

(def ^:private hpa
  {:metadata {:namespace "shop" :name "web"}
   :spec {:metrics [{:type "Pods" :pods {:metric {:name "http_requests_per_second"}}}]}})

(def ^:private tainted-node
  {:metadata {:name "n1"} :spec {:taints [{:key "a" :effect "NoSchedule"}]}})

(def ^:private books
  "path -> {:opts .. :items <what an answering cluster holds> :type <the type read>}"
  {"runbooks/kubernetes/pods/list_image_pull_back_off.clj" {:items [pod-pulling] :type "pods"}
   "runbooks/kubernetes/pods/list_images.clj" {:items [pod-pulling] :type "pods"}
   "tools/prometheus/runbooks/list_scaling_metrics.clj" {:items [hpa] :type "horizontalpodautoscalers"}
   "runbooks/kubernetes/nodes/remove_taint.clj" {:items [] :type "nodes"
                                                 :opts {:node "n1" :taint "a:NoSchedule"}}})

(deftest a-read-that-did-not-answer-fails-the-run
  (doseq [[path {:keys [opts type]}] books]
    (testing path
      (let [{:keys [result out]} (run-action (book path) unanswered opts)]
        (is (false? result) "an unanswered read is never an all-clear")
        (is (str/includes? out (str "could not read the " type " of the cluster")))
        (is (str/includes? out "unable to handle the request") "the reason kubectl gave is named")
        (is (not (str/includes? out "✅")))))))

(deftest a-resource-type-the-server-lacks-fails-the-run
  (doseq [[path {:keys [opts type]}] books]
    (testing path
      (let [{:keys [result out]} (run-action (book path) no-such-type opts)]
        (is (false? result))
        (is (str/includes? out (format "the server has no %s resource" type)))))))

(deftest an-answered-empty-list-is-an-all-clear
  (testing "list_image_pull_back_off"
    (let [{:keys [result out]} (run-action (book "runbooks/kubernetes/pods/list_image_pull_back_off.clj") (answering []) nil)]
      (is (true? result))
      (is (str/includes? out "no pod is in ImagePullBackOff"))))
  (testing "list_images"
    (is (true? (:result (run-action (book "runbooks/kubernetes/pods/list_images.clj") (answering []) nil)))))
  (testing "list_scaling_metrics"
    (let [{:keys [result out]} (run-action (book "tools/prometheus/runbooks/list_scaling_metrics.clj") (answering []) nil)]
      (is (true? result))
      (is (str/includes? out "no HorizontalPodAutoscaler uses any metrics"))))
  (testing "remove_taint"
    (let [{:keys [result out]} (run-action (book "runbooks/kubernetes/nodes/remove_taint.clj") (answering [])
                                           {:node "n1" :taint "a:NoSchedule"})]
      (is (true? result))
      (is (str/includes? out "no node has any taints")))))

(deftest an-answered-list-is-reported
  (testing "list_image_pull_back_off"
    (let [{:keys [result out]} (run-action (book "runbooks/kubernetes/pods/list_image_pull_back_off.clj") (answering [pod-pulling]) nil)]
      (is (true? result))
      (is (str/includes? out "shop/web-1"))))
  (testing "list_images"
    (let [{:keys [result out]} (run-action (book "runbooks/kubernetes/pods/list_images.clj") (answering [pod-pulling]) nil)]
      (is (true? result))
      (is (str/includes? out "registry.example.org/web:1"))))
  (testing "list_scaling_metrics"
    (let [{:keys [result out]} (run-action (book "tools/prometheus/runbooks/list_scaling_metrics.clj") (answering [hpa]) nil)]
      (is (true? result))
      (is (str/includes? out "http_requests_per_second"))))
  (testing "remove_taint reaches the change for a tainted node"
    (let [seen (atom nil)]
      (with-redefs [gumshoe.flow/change! (fn [config] (reset! seen config) true)]
        (run-action (book "runbooks/kubernetes/nodes/remove_taint.clj") (answering [tainted-node])
                    {:node "n1" :taint "a:NoSchedule"}))
      (is (= ["a:NoSchedule"] (get-in @seen [:confirmation :items]))))))

(deftest read-all-test
  (testing "an answered list"
    (with-redefs [shell/execute (answering [{:metadata {:name "n1"}}])]
      (is (= {:doc {:items [{:metadata {:name "n1"}}]}} (kubectl/read-all "ctx" "nodes")))))
  (testing "a read that did not answer names kubectl's reason"
    (with-redefs [shell/execute unanswered]
      (is (str/includes? (:unread (kubectl/read-all "ctx" "nodes")) "unable to handle the request"))))
  (testing "a resource type the server lacks"
    (with-redefs [shell/execute no-such-type]
      (is (= {:unread "the server has no horizontalpodautoscalers resource"}
             (kubectl/read-all "ctx" "horizontalpodautoscalers")))))
  (testing "output that does not parse did not answer"
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out "not json" :err ""})]
      (is (some? (:unread (kubectl/read-all "ctx" "nodes")))))))
