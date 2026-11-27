;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ceph-mgr-hosts-test
  "The ceph helpers that read through [:ceph :mgr-hosts] try the configured
   hosts in order and use the first that answers; a list where none answers
   fails closed. Every ssh call is answered by a stub keyed on the host, so
   nothing reaches a network."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.ceph :as ceph]
            [gumshoe.config :as config]
            [gumshoe.net :as net]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.tools.ceph :as tools-ceph]))

(def ^:private gi (* 1024 1024 1024))

(defn- df-json
  "A `ceph df --format json` document whose tightest pool has max-avail bytes."
  [max-avail]
  (format "{\"stats\":{\"total_avail_bytes\":%d},\"pools\":[{\"name\":\"a\",\"stats\":{\"max_avail\":%d}},{\"name\":\"b\",\"stats\":{\"max_avail\":%d}}]}"
          (* 100 max-avail) (* 2 max-avail) max-avail))

(def ^:private unreachable {:exit 255 :out "" :err "ssh: connect to host: No route to host"})

(defn- target-host
  "The ssh destination in an ssh argv: the token after the option terminator."
  [args]
  (second (drop-while #(not= "--" %) args)))

(defn- with-fake-ceph
  "Runs f with every ssh command answered from answers, a map of host to
   {:exit :out :err} (a host missing from it is unreachable), the VPN up unless
   vpn-up? is false, and env.edn naming hosts as the mgr hosts. Returns
   {:result <f's value> :hosts <the hosts ssh was pointed at, in order>}."
  [{:keys [hosts answers vpn-up?] :or {vpn-up? true}} f]
  (let [called (atom [])]
    (binding [ssh/*user-config* nil]
      (with-redefs [config/all (constantly {:vpn {:interface "wg9"} :ceph {:mgr-hosts hosts}})
                    net/interface-up? (fn [_] vpn-up?)
                    shell/execute (fn [& args]
                                    (let [host (target-host args)]
                                      (swap! called conj host)
                                      (merge {:exit 0 :out "" :err ""} (get answers host unreachable))))]
        {:result (f) :hosts @called}))))

(defn- preflight
  [plan]
  (tools-ceph/capacity-preflight {:cluster "kube.example.org" :plan plan}))

(def ^:private grow-20 [{:current "10Gi" :target "30Gi"}])

;; --- first-answering ---------------------------------------------------------

(defn- recording-read
  "A read answering from a map of host to answer, recording each host read."
  [answers]
  (let [calls (atom [])]
    [calls (fn [host] (swap! calls conj host) (get answers host {:ok? false}))]))

(deftest first-answering-uses-the-second-host-when-the-first-is-unreachable
  (let [[calls read] (recording-read {"m2" {:ok? true :out "x"}})
        result (ceph/first-answering ["m1" "m2" "m3"] read)]
    (is (= "m2" (:host result)))
    (is (= {:ok? true :out "x"} (:answer result)))
    (is (= ["m1" "m2"] @calls))
    (is (= ["m1" "m2"] (map :host (:tried result))))))

(deftest first-answering-reads-no-host-after-the-first-that-answers
  (let [[calls read] (recording-read {"m1" {:ok? true} "m2" {:ok? true} "m3" {:ok? true}})]
    (is (= "m1" (:host (ceph/first-answering ["m1" "m2" "m3"] read))))
    (is (= ["m1"] @calls))))

(deftest first-answering-keeps-the-configured-order
  (let [[calls read] (recording-read {})]
    (ceph/first-answering ["c" "a" "b"] read)
    (is (= ["c" "a" "b"] @calls))))

(deftest first-answering-skips-a-host-that-answers-with-an-error
  (let [[calls read] (recording-read {"m1" {:ok? false :exit 1 :err "Error EACCES"}
                                      "m2" {:ok? true :out "fine"}})
        result (ceph/first-answering ["m1" "m2"] read)]
    (is (= "m2" (:host result)))
    (is (= ["m1" "m2"] @calls))
    (is (= {:ok? false :exit 1 :err "Error EACCES"} (:answer (first (:tried result)))))))

(deftest first-answering-counts-only-a-true-ok-as-an-answer
  (let [[_ read] (recording-read {"m1" {:ok? "yes"} "m2" {} "m3" {:ok? true}})]
    (is (= "m3" (:host (ceph/first-answering ["m1" "m2" "m3"] read))))))

(deftest first-answering-treats-a-throwing-read-as-unanswered
  (let [read (fn [host] (if (= "m1" host) (throw (ex-info "boom" {})) {:ok? true}))
        result (ceph/first-answering ["m1" "m2"] read)]
    (is (= "m2" (:host result)))
    (is (= {:ok? false :error "boom"} (:answer (first (:tried result)))))))

(deftest first-answering-reports-no-host-when-none-answers
  (let [[calls read] (recording-read {})
        result (ceph/first-answering ["m1" "m2" "m3"] read)]
    (is (nil? (:host result)))
    (is (nil? (:answer result)))
    (is (= ["m1" "m2" "m3"] @calls (map :host (:tried result))))))

(deftest first-answering-an-empty-list-reads-nothing
  (doseq [hosts [[] nil ()]]
    (let [[calls read] (recording-read {"m1" {:ok? true}})]
      (is (= {:host nil :answer nil :tried []} (ceph/first-answering hosts read)))
      (is (empty? @calls)))))

;; --- mgr-hosts ----------------------------------------------------------------

(deftest mgr-hosts-reads-the-configured-list-in-order
  (doseq [[configured expected] [[["m1" "m2" "m3"] ["m1" "m2" "m3"]]
                                 [["m3" "m1"] ["m3" "m1"]]
                                 ["m1" ["m1"]]
                                 [[" m1 " "" "  " nil :m2 "m1" "m2"] ["m1" "m2"]]
                                 [[] []]
                                 [nil []]]]
    (with-redefs [config/all (constantly {:ceph {:mgr-hosts configured}})]
      (is (= expected (tools-ceph/mgr-hosts {:kubernetes-cluster "c"})) (pr-str configured)))))

;; --- capacity preflight -------------------------------------------------------

(deftest capacity-uses-the-second-host-when-the-first-is-unreachable
  (testing "the second host's real reading decides: room for the growth"
    (let [{:keys [result hosts]} (with-fake-ceph {:hosts ["m1" "m2" "m3"]
                                                  :answers {"m2" {:out (df-json (* 50 gi))}}}
                                   #(preflight grow-20))]
      (is (= [] result))
      (is (= ["m1" "m2"] hosts) "m3 is never read once m2 answered")))
  (testing "the second host's real reading decides: no room for the growth"
    (let [{:keys [result hosts]} (with-fake-ceph {:hosts ["m1" "m2" "m3"]
                                                  :answers {"m2" {:out (df-json (* 5 gi))}}}
                                   #(preflight grow-20))]
      (is (= ["ceph has 5.0 GiB usable free but the resize needs 20.0 GiB more - refusing to avoid filling the cluster"]
             result))
      (is (= ["m1" "m2"] hosts)))))

(deftest capacity-reads-only-the-first-host-when-it-answers
  (let [{:keys [result hosts]} (with-fake-ceph {:hosts ["m1" "m2"]
                                                :answers {"m1" {:out (df-json (* 5 gi))}
                                                          "m2" {:out (df-json (* 500 gi))}}}
                                 #(preflight grow-20))]
    (is (= ["ceph has 5.0 GiB usable free but the resize needs 20.0 GiB more - refusing to avoid filling the cluster"]
           result))
    (is (= ["m1"] hosts))))

(deftest capacity-refuses-when-every-host-is-unreachable
  (testing "the refusal keeps its wording and names every host tried, in order"
    (let [{:keys [result hosts]} (with-fake-ceph {:hosts ["m1" "m2" "m3"]} #(preflight grow-20))]
      (is (= ["could not read ceph free capacity from m1, m2, m3 - refusing the resize (fail closed)"] result))
      (is (= ["m1" "m2" "m3"] hosts))))
  (testing "a single unreachable host gives exactly the refusal it always gave"
    (let [{:keys [result]} (with-fake-ceph {:hosts ["mgr-1.example.org"]} #(preflight grow-20))]
      (is (= ["could not read ceph free capacity from mgr-1.example.org - refusing the resize (fail closed)"] result))))
  (testing "a plan the check cannot measure is still refused when ceph was not read"
    (let [{:keys [result]} (with-fake-ceph {:hosts ["m1" "m2"]} #(preflight [{:current "10Gi" :target "500m"}]))]
      (is (= ["could not read ceph free capacity from m1, m2 - refusing the resize (fail closed)"] result)))))

(deftest capacity-skips-a-host-that-answers-with-an-error
  (doseq [[label bad] [["ceph exits non-zero" {:exit 1 :out "" :err "Error EACCES: access denied"}]
                       ["ceph exits non-zero with a document" {:exit 13 :out (df-json (* 500 gi))}]
                       ["ssh exits cleanly with no output" {:exit 0 :out ""}]
                       ["the output is not JSON" {:exit 0 :out "Error initializing cluster client"}]
                       ["the document names no pool capacity" {:exit 0 :out "{\"stats\":{\"total_avail_bytes\":999999999999},\"pools\":[]}"}]]]
    (testing label
      (let [{:keys [result hosts]} (with-fake-ceph {:hosts ["m1" "m2"]
                                                    :answers {"m1" bad "m2" {:out (df-json (* 5 gi))}}}
                                     #(preflight grow-20))]
        (is (= ["ceph has 5.0 GiB usable free but the resize needs 20.0 GiB more - refusing to avoid filling the cluster"]
               result))
        (is (= ["m1" "m2"] hosts)))
      (testing "and refuses when that error is the only answer"
        (let [{:keys [result]} (with-fake-ceph {:hosts ["m1"] :answers {"m1" bad}} #(preflight grow-20))]
          (is (= ["could not read ceph free capacity from m1 - refusing the resize (fail closed)"] result)))))))

(deftest capacity-keeps-the-vpn-refusal-and-reads-nothing-off-network
  (let [{:keys [result hosts]} (with-fake-ceph {:hosts ["m1" "m2"]
                                                :answers {"m1" {:out (df-json (* 500 gi))}}
                                                :vpn-up? false}
                                 #(preflight grow-20))]
    (is (= ["ceph mgr host m1 is configured but the VPN is down - cannot verify ceph capacity, refusing the resize"]
           result))
    (is (empty? hosts))))

(deftest capacity-is-not-checked-without-mgr-hosts
  (doseq [configured [[] nil ["" "  "]]]
    (let [{:keys [result hosts]} (with-fake-ceph {:hosts configured} #(preflight grow-20))]
      (is (= [] result) (pr-str configured))
      (is (empty? hosts) (pr-str configured)))))

(deftest capacity-refuses-when-the-read-throws
  (with-redefs [config/all (constantly {:ceph {:mgr-hosts ["m1" "m2"]}})
                net/interface-up? (constantly true)]
    (is (= ["could not read ceph free capacity from m1, m2 - refusing the resize (fail closed)"]
           (tools-ceph/capacity-preflight {:cluster "c" :plan grow-20}
                                          (fn [_] (throw (ex-info "ssh vanished" {}))))))))

(deftest capacity-problems-fails-closed-on-every-unread-shape
  (testing "an answer claiming ok without a usable figure is refused"
    (is (= ["could not read ceph free capacity from m1 - refusing the resize (fail closed)"]
           (tools-ceph/capacity-problems ["m1"] true {:host "m1" :answer {:ok? true} :tried [{:host "m1"}]} grow-20))))
  (testing "no attempt recorded still names the configured hosts"
    (is (= ["could not read ceph free capacity from m1, m2 - refusing the resize (fail closed)"]
           (tools-ceph/capacity-problems ["m1" "m2"] true nil grow-20))))
  (testing "no hosts is not a ceph-checkable resize"
    (is (= [] (tools-ceph/capacity-problems [] false nil grow-20)))))

;; --- cluster-log watcher ------------------------------------------------------

(defn- watch-once
  [hosts]
  ((tools-ceph/ceph-cluster-log-errors hosts "wg9")))

(deftest cluster-log-reads-the-second-host-when-the-first-is-unreachable
  (let [{:keys [result hosts]} (with-fake-ceph {:hosts ["m1" "m2" "m3"]
                                                :answers {"m2" {:out "2026-11-27 osd.3 failed\n\n  2026-11-27 pg 1.2 degraded  \n"}}}
                                 #(doall (watch-once ["m1" "m2" "m3"])))]
    (is (= ["ceph cluster log: 2026-11-27 osd.3 failed" "ceph cluster log: 2026-11-27 pg 1.2 degraded"] result))
    (is (= ["m1" "m2"] hosts))))

(deftest cluster-log-says-so-when-no-host-answers
  (testing "every host unreachable is a signal naming them, never an empty log"
    (let [{:keys [result hosts]} (with-fake-ceph {:hosts ["m1" "m2"]} #(doall (watch-once ["m1" "m2"])))]
      (is (= ["ceph cluster log: not read - no ceph mgr host answered (tried m1, m2), so ceph errors during this resize are not shown"]
             result))
      (is (= ["m1" "m2"] hosts))))
  (testing "a single unreachable host is reported too"
    (let [{:keys [result]} (with-fake-ceph {:hosts ["m1"]} #(doall (watch-once ["m1"])))]
      (is (= 1 (count result)))
      (is (str/includes? (first result) "not read")))))

(deftest cluster-log-skips-a-host-that-answers-with-an-error
  (let [{:keys [result hosts]} (with-fake-ceph {:hosts ["m1" "m2"]
                                                :answers {"m1" {:exit 1 :out "" :err "Error EACCES"}
                                                          "m2" {:out "osd.1 down"}}}
                                 #(doall (watch-once ["m1" "m2"])))]
    (is (= ["ceph cluster log: osd.1 down"] result))
    (is (= ["m1" "m2"] hosts))))

(deftest cluster-log-a-quiet-log-is-quiet
  (let [{:keys [result hosts]} (with-fake-ceph {:hosts ["m1" "m2"] :answers {"m1" {:out ""}}}
                                 #(doall (watch-once ["m1" "m2"])))]
    (is (empty? result))
    (is (= ["m1"] hosts))))

(deftest cluster-log-reads-nothing-while-the-vpn-is-down
  (let [{:keys [result hosts]} (with-fake-ceph {:hosts ["m1"] :answers {"m1" {:out "osd.1 down"}} :vpn-up? false}
                                 #(doall (watch-once ["m1"])))]
    (is (= ["ceph cluster log: not read - the VPN interface wg9 is down"] result))
    (is (empty? hosts))))

(deftest cluster-log-tries-the-hosts-again-on-every-sample
  (let [answers (atom {"m1" {:out "first"}})
        called (atom [])]
    (binding [ssh/*user-config* nil]
      (with-redefs [net/interface-up? (constantly true)
                    shell/execute (fn [& args]
                                    (let [host (target-host args)]
                                      (swap! called conj host)
                                      (merge {:exit 0 :out "" :err ""} (get @answers host unreachable))))]
        (let [watcher (tools-ceph/ceph-cluster-log-errors ["m1" "m2"] "wg9")]
          (is (= ["ceph cluster log: first"] (doall (watcher))))
          (reset! answers {"m2" {:out "second"}})
          (is (= ["ceph cluster log: second"] (doall (watcher))))
          (is (= ["m1" "m1" "m2"] @called)))))))

(deftest cluster-log-signals-is-pure-over-the-selection
  (is (= ["ceph cluster log: a"] (tools-ceph/cluster-log-signals {:host "m1" :answer {:ok? true :out "a\n"} :tried [{:host "m1"}]})))
  (is (= ["ceph cluster log: not read - no ceph mgr host is configured"]
         (tools-ceph/cluster-log-signals {:host nil :tried []}))))
