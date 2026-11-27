;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.report-printable-test
  "The report makes every string a finding carries printable and bounded
   before any format renders it, so a detective that quotes cluster text
   without cleaning it still cannot write escape sequences, bidirectional or
   zero-width characters, or unbounded text to the operator or to an agent
   reading the json."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.walk :as walk]
            [gumshoe.detective :as detective]
            [gumshoe.render :as render]
            [gumshoe.report :as report]
            [gumshoe.summary]))

(defn- quietly
  "The value of (f) with stdout and stderr captured, as [value out err]."
  [f]
  (let [out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        value (binding [*out* out *err* err] (f))]
    [value (str out) (str err)]))

(def ^:private hostile-parts
  "Text a cluster object can carry: a screen clear and cursor home (CSI), an
   OSC 8 hyperlink terminated by BEL and by ST, a bare BEL, the 8-bit CSI, a
   right-to-left override and isolate, zero-width space and joiner, a byte
   order mark, CR/LF, a line and a paragraph separator, DEL and a tab."
  ["\u001b[2J\u001b[H"
   "\u001b]8;;https://evil.example/\u0007click\u001b]8;;\u0007"
   "\u001b]8;;https://evil.example/\u001b\\st\u001b]8;;\u001b\\"
   "\u0007" "\u009b31m" "‮" "⁦" "​" "‍" "﻿"
   "\r\n" " " " " "\u007f" "\t"])

(def ^:private hostile
  (str "evil " (str/join "x" hostile-parts) " end"))

(def ^:private forbidden
  "Characters no rendered finding may carry."
  #"[\u0000-\u001f\u007f-\u009f​-‏‪-‮  ⁠-⁩﻿]")

(defn- clean-string? [s]
  (not (re-find forbidden s)))

(def ^:private flood (apply str (repeat 10000 "A")))

(def ^:private careless
  "A detective that quotes the cluster's text as it found it."
  {:name "careless" :description "quotes cluster text raw" :requires []
   :detect (fn [_]
             [{:severity :critical
               :component (str "shop/web" "‮" "\u001b[2J")
               :summary (str "Deployment unhealthy: " hostile)
               :hint (str "the controller said: " hostile " " flood)
               :message flood
               :condition {:reason hostile :messages [hostile {:deep hostile}]}}
              {:severity :critical
               :component "shop/api"
               :summary (str "Deployment unhealthy: " hostile)
               :hint "same"}])})

(defn- findings []
  (detective/run-detectives [careless] {}))

(defn- strings-in [data]
  (let [acc (atom [])]
    (walk/postwalk #(do (when (string? %) (swap! acc conj %)) %) data)
    @acc))

(deftest the-text-report-carries-no-escape-or-format-character
  (let [[healthy out] (quietly #(detective/report! [careless] (findings) "text"))
        plain (render/strip-colors out)]
    (is (false? healthy))
    (is (str/includes? plain "careless"))
    (is (str/includes? plain "×2") "the two findings with one summary still roll up")
    (is (not (str/includes? plain "\u001b")) "no escape survives besides the report's own colour")
    (is (not (re-find #"[\u0007\u009b‮⁦​‍﻿  \u007f\r]" plain)))
    (is (not (str/includes? plain flood)) "the 10k-character text is cut")))

(deftest the-json-report-carries-only-printable-bounded-strings
  (let [[_ out] (quietly #(detective/report! [careless] (findings) "json"))
        data (json/parse-string out true)
        strings (strings-in (:findings data))]
    (is (= 2 (count (:findings data))))
    (is (every? clean-string? strings))
    (is (not (str/includes? out "\\u001B")) "the encoder had no control character to escape")
    (is (every? #(<= (render/columns %) (apply max (vals detective/finding-text-budgets))) strings))
    (let [[first-finding] (:findings data)]
      (is (<= (render/columns (:summary first-finding)) 500))
      (is (<= (render/columns (:hint first-finding)) 4000))
      (is (str/ends-with? (:hint first-finding) "…"))
      (is (= 1 (count (re-seq #"…" (:message first-finding)))))
      (is (= "critical" (:severity first-finding)) "the severity is still ranked"))))

(deftest the-edn-report-carries-only-printable-bounded-strings
  (let [[_ out] (quietly #(detective/report! [careless] (findings) "edn"))
        data (edn/read-string {:default (fn [_ v] v)} out)]
    (is (not (str/includes? out "\u001b")))
    (is (every? clean-string? (strings-in (:findings data))))
    (is (= :critical (:severity (first (:findings data)))))))

(deftest a-plugin-format-and-the-shared-summary-receive-printable-findings
  (let [seen (atom nil)]
    (report/register-format! "report-printable-test" #(reset! seen %))
    (quietly #(detective/report! [careless] (findings) "report-printable-test"))
    (is (= 2 (count (:findings @seen))))
    (is (every? clean-string? (strings-in (:findings @seen))))
    (is (every? clean-string? (strings-in @@#'gumshoe.summary/captured)))))

(deftest the-drill-down-is-offered-the-findings-as-the-report-shows-them
  (let [offered (atom nil)]
    (with-redefs-fn {#'detective/offer-drill-down! (fn [_ fs] (reset! offered fs))}
      #(quietly (fn [] (detective/report-and-offer! nil [careless] (findings) "text"))))
    (is (= 2 (count @offered)))
    (is (every? clean-string? (strings-in @offered)))
    (is (every? clean-string? (map (comp render/strip-colors :label) (detective/drillable-subjects @offered))))))

(deftest without-the-boundary-the-same-checks-fail
  (with-redefs [detective/printable-finding identity]
    (let [[_ text] (quietly #(detective/report! [careless] (findings) "text"))
          [_ out] (quietly #(detective/report! [careless] (findings) "json"))]
      (is (str/includes? (render/strip-colors text) "\u001b[2J"))
      (is (not (every? clean-string? (strings-in (:findings (json/parse-string out true)))))))))

(deftest a-clean-finding-is-unchanged
  (let [finding {:severity :warning
                 :component "shop/web"
                 :detective "deployments"
                 :summary "Deployment has 0/3 ready replicas"
                 :hint "kubectl -n shop describe deployment web"
                 :since (java.time.Instant/parse "2026-11-28T10:00:00Z")
                 :count 3
                 :subject {:kind "Deployment" :namespace "shop" :name "web"}
                 :labels {"app.kubernetes.io/name" "web"}
                 :containers ["app" "sidecar"]
                 :tags #{:a :b}
                 :reports-unread false}]
    (is (= finding (detective/printable-finding finding)))))

(deftest a-text-a-detective-already-cut-gets-no-second-ellipsis
  (let [message (render/cut 600 (render/printable flood))
        finding {:severity :critical :component "x" :summary "s" :message message :hint message}]
    (is (= finding (detective/printable-finding finding)))))

(deftest every-bound-holds-and-cuts-once
  (let [finding (detective/printable-finding
                 {:severity :info :component flood :summary flood :hint flood :message flood
                  :other {:nested flood}})]
    (is (= 500 (render/columns (:summary finding))))
    (is (= 4000 (render/columns (:hint finding))))
    (is (= 2000 (render/columns (:message finding))))
    (is (= 2000 (render/columns (:component finding))))
    (is (= 2000 (render/columns (get-in finding [:other :nested]))))
    (is (every? #(= 1 (count (re-seq #"…" %))) (strings-in finding)))))

(defrecord Quoted [text])

(deftest string-keys-and-records-are-cleaned-too
  (let [finding (detective/printable-finding
                 {:severity :info :labels {(str "k" "‮") "v\u001b[2J"}})]
    (is (= {"k" "v"} (:labels finding))))
  (let [r (render/printable-data (constantly 50) 8
                                 (assoc (->Quoted "x\u001b[2Jy") "k\u200b" "v\u0007"))]
    (is (instance? Quoted r))
    (is (= "xy" (:text r)))
    (is (= "v" (get r "k")))
    (is (not (contains? r "k\u200b")))))

(deftest data-deeper-than-the-bound-becomes-one-bounded-string
  (let [deep (reduce (fn [v _] [v]) (str "bottom" hostile) (range 10))
        endless {:x (reduce (fn [v _] [v]) (range) (range 8))}
        cleaned (detective/printable-finding {:severity :info :deep deep})
        strings (strings-in cleaned)]
    (is (seq strings))
    (is (every? clean-string? strings))
    (is (str/includes? (str/join strings) "bottom"))
    (testing "data nested past the print level is elided, never printed raw"
      (let [deeper (detective/printable-finding
                    {:severity :info :deep (reduce (fn [v _] [v]) hostile (range 40))})]
        (is (every? clean-string? (strings-in deeper)))))
    (testing "an endless sequence below the depth bound is printed bounded"
      (let [out (detective/printable-finding endless)]
        (is (every? #(<= (render/columns %) detective/finding-text-default) (strings-in out)))))))

;; --- idempotence ---------------------------------------------------------------

(def ^:private gen-text
  "Strings mixing printable text with every hostile part."
  (gen/fmap str/join
            (gen/vector (gen/one-of [gen/string
                                     gen/string-ascii
                                     (gen/elements hostile-parts)
                                     (gen/elements [" " "  " " " " " "…" "漢字" "😀"])]))))

(deftest printable-and-cut-are-idempotent
  (let [result (tc/quick-check
                300
                (prop/for-all [s gen-text
                               budget (gen/choose 1 80)]
                  (let [p (render/printable s)
                        c (render/cut budget p)
                        pc (render/printable-cut budget s)]
                    (and (= p (render/printable p))
                         (= c (render/cut budget c))
                         (= pc (render/printable-cut budget pc))
                         (clean-string? pc)
                         (<= (render/columns pc) budget))))
                :seed 6084)]
    (is (:pass? result) (pr-str (:shrunk result)))))

(deftest printable-finding-is-idempotent
  (let [gen-finding (gen/let [summary gen-text hint gen-text message gen-text
                              nested (gen/vector gen-text 0 3)
                              k gen-text]
                      {:severity :warning :component "a/b" :summary summary :hint hint
                       :message message :nested {:texts nested k summary}})
        result (tc/quick-check
                200
                (prop/for-all [f gen-finding]
                  (let [once (detective/printable-finding f)]
                    (and (= once (detective/printable-finding once))
                         (every? clean-string? (strings-in once)))))
                :seed 6084)]
    (is (:pass? result) (pr-str (:shrunk result)))))
