;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.local-port-test
  "A local port outside 1-65535 is refused as such, by the --port option of
   every port-forwarding book and by with-port-forward itself, and only a
   port something really holds is reported as in use."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook])
  (:import [java.net InetAddress ServerSocket]))

(def ^:private forwarding-books
  "Each book that forwards a port, with the arguments it requires besides."
  {"tools/db-operator/runbooks/connect.clj" []
   "tools/db-operator/runbooks/admin_connect.clj" []
   "tools/db-operator/runbooks/dump.clj" ["--dump" "out.sql"]
   "tools/db-operator/runbooks/restore.clj" ["--dump" "in.sql"]
   "tools/loki/runbooks/scan.clj" []
   "tools/thanos/runbooks/scan.clj" []
   "tools/prometheus/runbooks/delete_metric.clj" ["--metric" "up"]})

(defn- refusal
  "What with-port-forward prints for local-port, and whether its body ran."
  [local-port]
  (let [ran (atom false)
        err (java.io.StringWriter.)
        result (binding [*err* err]
                 (kubectl/with-port-forward {:context "c" :namespace "n" :service "s"
                                             :local-port local-port :remote-port 80}
                   (fn [] (reset! ran true) :ran)))]
    {:result result :ran @ran :printed (str err)}))

(deftest valid-local-port-is-one-to-65535
  (is (every? kubectl/valid-local-port? [1 80 5432 65535]))
  (is (not-any? kubectl/valid-local-port? [0 -1 65536 70000 543200 nil "5432"])))

(deftest with-port-forward-refuses-a-port-outside-the-range-as-such
  (doseq [port [0 -1 65536 70000]]
    (testing port
      (let [{:keys [result ran printed]} (refusal port)]
        (is (false? result))
        (is (false? ran))
        (is (str/includes? printed (str "--port must be between 1 and 65535, got " port)) printed)
        (is (not (str/includes? printed "already in use")) printed)))))

(deftest with-port-forward-still-refuses-a-port-something-holds
  (with-open [held (ServerSocket. 0 50 (InetAddress/getByName "127.0.0.1"))]
    (let [port (.getLocalPort held)
          {:keys [result ran printed]} (refusal port)]
      (is (false? result))
      (is (false? ran))
      (is (str/includes? printed (format "local port %d is already in use" port)) printed))))

(deftest a-check-that-fails-otherwise-is-reported-as-itself
  (with-redefs-fn {#'kubectl/local-port-taken? (fn [_] (throw (SecurityException. "binding is not permitted here")))}
    (fn []
      (let [{:keys [result ran printed]} (refusal 4242)]
        (is (false? result))
        (is (false? ran))
        (is (str/includes? printed "local port 4242 could not be checked: binding is not permitted here") printed)
        (is (not (str/includes? printed "already in use")) printed)))))

(deftest only-a-bind-failure-reads-as-taken
  (is (thrown? IllegalArgumentException (#'kubectl/port-bindable? 70000))
      "an out-of-range port is not reported as held")
  (with-open [held (ServerSocket. 0 50 (InetAddress/getByName "127.0.0.1"))]
    (is (false? (#'kubectl/port-bindable? (.getLocalPort held))))))

(deftest every-forwarding-book-refuses-an-out-of-range-port-while-parsing
  (doseq [[path required] forwarding-books]
    (testing path
      (let [{:keys [options]} (binding [runbook/*loading* true] (load-file path))]
        (is (= 5432 (:port (:opts (runbook/arguments options (into required ["--port" "5432"])))))
            "a port in range is taken as given")
        (doseq [port ["0" "-1" "70000"]]
          (let [message (try (runbook/arguments options (into required ["--port" port])) nil
                             (catch Exception e (ex-message e)))]
            (is (some? message) (str "--port " port " was accepted"))
            (is (str/includes? (str message) "--port must be between 1 and 65535") (str message))))))))
