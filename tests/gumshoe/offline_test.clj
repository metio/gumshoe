;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.offline-test
  "Inside gumshoe.offline/forbidding nothing reaches outside the process:
   every launch of gumshoe.shell and every request of gumshoe.http refuses
   before it starts, and a lazy result is realized inside the binding."
  (:require [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.config :as config]
            [gumshoe.http :as http]
            [gumshoe.offline :as offline]
            [gumshoe.pad :as pad]
            [gumshoe.shell :as shell]))

(defn- launches
  "[result launched] of thunk with babashka.process stubbed: every process
   it would have started is recorded and none starts."
  [thunk]
  (let [launched (atom [])
        record (fn [& args] (swap! launched conj (vec args)) (throw (ex-info "stubbed" {})))]
    (with-redefs [process/process record
                  process/shell record]
      [(try (thunk) (catch clojure.lang.ExceptionInfo e e)) @launched])))

(deftest check-does-nothing-outside-forbidding
  (is (nil? offline/*forbidden*))
  (is (nil? (offline/check! ["kubectl" "get" "nodes"]))))

(deftest check-names-what-it-refused-and-why
  (let [e (try (binding [offline/*forbidden* "the plan reads nothing"]
                 (offline/check! ["kubectl" "get" ["nodes" "n1"]]))
               (catch clojure.lang.ExceptionInfo e e))]
    (is (offline/forbidden? e))
    (is (= "not running 'kubectl get nodes n1': the plan reads nothing" (ex-message e)))
    (is (= "kubectl get nodes n1" (:what (ex-data e))))))

(deftest forbidden-is-only-the-exception-check-throws
  (is (not (offline/forbidden? (ex-info "other" {}))))
  (is (not (offline/forbidden? (RuntimeException. "x"))))
  (is (not (offline/forbidden? nil))))

(deftest forbidding-returns-the-result-of-a-thunk-that-reaches-nothing
  (is (= [1 2 3] (offline/forbidding "why" #(map inc [0 1 2]))))
  (is (= {:a [1]} (offline/forbidding "why" (constantly {:a [1]})))))

(deftest forbidding-realizes-a-lazy-result-inside-the-binding
  (testing "a lazy seq whose elements read is refused, not returned to read later"
    (is (offline/forbidden?
         (try (offline/forbidding "why" #(map (fn [x] (offline/check! ["read" x]) x) [1 2]))
              (catch clojure.lang.ExceptionInfo e e)))))
  (testing "nested in a map and a vector"
    (is (offline/forbidden?
         (try (offline/forbidding "why" (fn [] {:items [(lazy-seq (offline/check! ["read"]))]}))
              (catch clojure.lang.ExceptionInfo e e))))))

(deftest every-shell-launch-refuses-before-it-starts-anything
  (doseq [[label thunk] {"execute" #(shell/execute "kubectl" "get" "nodes")
                         "execute-with-stdin" #(shell/execute-with-stdin "x" "kubectl" "apply" "-f" "-")
                         "execute-env" #(shell/execute-env {"A" "b"} "env")
                         "stdout-of" #(shell/stdout-of "kubectl" "get" "nodes")
                         "run-with-output" #(shell/run-with-output "ssh" "host")
                         "run-attributed" #(shell/run-attributed "kubectl" "delete" "pod" "p")
                         "capture-line" #(shell/capture-line "gum" "input")
                         "pipe-to!" #(shell/pipe-to! "x" "wl-copy")
                         "start" #(shell/start {} "kubectl" "port-forward" "pod/p" "80")}]
    (testing label
      (let [[result launched] (launches #(offline/forbidding "the plan reads nothing" thunk))]
        (is (offline/forbidden? result))
        (is (empty? launched))))))

(deftest the-shell-launches-as-before-outside-forbidding
  (testing "positive control for the stub: outside the binding the launch is attempted"
    (let [[_ launched] (launches #(shell/execute "kubectl" "get" "nodes"))]
      (is (= 1 (count launched))))))

(deftest http-requests-refuse-before-they-connect
  (doseq [[label thunk] {"request" #(http/request :get "http://127.0.0.1:9/never" {})
                         "fetch" #(http/fetch "http://127.0.0.1:9/never")
                         "fetch-lines!" #(http/fetch-lines! "http://127.0.0.1:9/never")}]
    (testing label
      (let [e (try (offline/forbidding "why" thunk) (catch clojure.lang.ExceptionInfo e e))]
        (is (offline/forbidden? e))
        (is (str/includes? (ex-message e) "http://127.0.0.1:9/never"))))))

(deftest announcing-and-creating-a-pad-refuse-before-they-post
  (testing "a webhook announcer: the fan-out does not turn the refusal into a warning"
    (with-redefs [config/env-value (constantly {:type :webhook :url "http://127.0.0.1:9/hook"})]
      (is (offline/forbidden?
           (try (offline/forbidding "why" #(announce/announce! "sys" {:actor "a"} "msg"))
                (catch clojure.lang.ExceptionInfo e e))))))
  (testing "a HedgeDoc pad"
    (is (offline/forbidden?
         (try (offline/forbidding "why" #(pad/create-note! "http://127.0.0.1:9" "# x"))
              (catch clojure.lang.ExceptionInfo e e))))))
