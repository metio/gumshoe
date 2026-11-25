;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-pin-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn- object [& {:keys [uid generation rv annotation] :or {uid "u1"}}]
  {:metadata (cond-> {:uid uid :resourceVersion (or rv "100")}
               generation (assoc :generation generation)
               annotation (assoc :annotations {:x annotation}))})

(deftest a-fingerprint-prefers-generation
  (is (= {:uid "u1" :generation 3} (mutation/fingerprint (object :generation 3))))
  (is (= {:uid "u1" :resource-version "7"} (mutation/fingerprint (object :rv "7"))) "no generation: resourceVersion")
  (is (= {:uid "u1" :generation 3 :facts "a"} (mutation/fingerprint (object :generation 3 :annotation "a") #(-> % :metadata :annotations :x))))
  (is (nil? (mutation/fingerprint nil)))
  (is (nil? (mutation/fingerprint nil :anything))))

(deftest drift-names-what-moved
  (let [before (mutation/fingerprint (object :generation 3))]
    (is (nil? (mutation/drift before (mutation/fingerprint (object :generation 3 :rv "999"))))
        "a status update changes resourceVersion but not generation")
    (is (= "it was deleted after the confirmation" (mutation/drift before nil)))
    (is (= "it was deleted and created again after the confirmation"
           (mutation/drift before (mutation/fingerprint (object :uid "u2" :generation 1)))))
    (is (= "its spec changed after the confirmation (generation 3, now 4)"
           (mutation/drift before (mutation/fingerprint (object :generation 4))))))
  (testing "without a generation any change counts"
    (is (= "it changed after the confirmation"
           (mutation/drift (mutation/fingerprint (object :rv "1")) (mutation/fingerprint (object :rv "2")))))
    (is (nil? (mutation/drift (mutation/fingerprint (object :rv "1")) (mutation/fingerprint (object :rv "1"))))))
  (testing "facts outside the spec"
    (let [facts #(-> % :metadata :annotations :x)]
      (is (= "what the change rests on moved after the confirmation (\"a\", now \"b\")"
             (mutation/drift (mutation/fingerprint (object :generation 3 :annotation "a") facts)
                             (mutation/fingerprint (object :generation 3 :annotation "b") facts))))))
  (is (nil? (mutation/drift nil nil)) "nothing confirmed, nothing to compare"))

(defmacro ^:private err-str
  "What body prints on stderr."
  [& body]
  `(let [w# (java.io.StringWriter.)]
     (binding [*err* w#] ~@body)
     (str w#)))

(deftest the-pinned-precondition
  (let [ref {:type "deployments" :namespace "shop" :name "web"}
        before (mutation/fingerprint (object :generation 3))]
    (is (nil? (mutation/pinned-precondition nil nil before (constantly nil))) "no pin keeps no precondition")
    (is (= :mine (mutation/pinned-precondition :mine nil before (constantly nil))) "no pin keeps the book's")
    (is (true? ((mutation/pinned-precondition nil ref before (constantly (object :generation 3))))))
    (is (false? ((mutation/pinned-precondition nil ref before (constantly (object :generation 4))))))
    (is (false? ((mutation/pinned-precondition (constantly true) ref before (constantly nil)))))
    (testing "an object not read at confirmation is refused, since nothing confirms what is acted on"
      (is (false? ((mutation/pinned-precondition nil ref nil (constantly (object :generation 3))))))
      (is (re-find #"refusing: it could not be read when the change was confirmed"
                   (err-str ((mutation/pinned-precondition nil ref nil (constantly (object :generation 3))))))))
    (testing "a failing book precondition skips the fetch"
      (let [fetched (atom 0)]
        (is (false? ((mutation/pinned-precondition (constantly false) ref before (fn [_] (swap! fetched inc) nil)))))
        (is (zero? @fetched))))
    (testing "the refusal is printed on stderr, beside the banner that ends the run"
      (is (re-find #"refusing: its spec changed"
                   (err-str ((mutation/pinned-precondition nil ref before (constantly (object :generation 9)))))))
      (is (= "" (with-out-str (binding [*err* (java.io.StringWriter.)]
                                ((mutation/pinned-precondition nil ref before (constantly (object :generation 9)))))))
          "nothing of the refusal reaches stdout"))))

(deftest fetch-pinned-picks-the-scope
  (with-redefs [kubectl/get-namespaced-resource (fn [_ ns type name] [:ns ns type name])
                kubectl/get-cluster-resource (fn [_ type name] [:cluster type name])]
    (is (= [:ns "shop" "deployments" "web"] (mutation/fetch-pinned "ctx" {:type "deployments" :namespace "shop" :name "web"})))
    (is (= [:cluster "nodes" "n1"] (mutation/fetch-pinned "ctx" {:type "nodes" :name "n1"})))))

(defn- run-with [fetches spec]
  (let [calls (atom fetches) executed (atom [])]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (constantly true)
                  effect/run! (fn [e] (swap! executed conj e) true)
                  mutation/fetch-pinned (fn [_ _] (let [[o & more] @calls] (reset! calls more) o))]
      (let [err (java.io.StringWriter.)
            out (with-out-str
                  (binding [*err* err]
                    (mutation/run-on! (merge {:select {:mode :one :label "Deployment"}
                                              :confirm {:action "x"}
                                              :pin (constantly {:type "deployments" :namespace "shop" :name "web"})
                                              :effect (fn [_] (effect/plan (effect/note "go")))}
                                             spec)
                                      "Deployment shop/web" {} nil)))]
        {:executed @executed :out out :err (str err) :left (count @calls)}))))

(deftest run-on-refuses-an-object-that-changed-while-confirming
  (testing "unchanged: the effect runs"
    (let [{:keys [executed left]} (run-with [(object :generation 3) (object :generation 3 :rv "200")] {})]
      (is (zero? left) "fingerprinted at confirmation and again before the effect")
      (is (= [[[:note "go"]]] executed) "the plan ran once")))
  (testing "spec changed: refused before the effect"
    (let [{:keys [executed out err]} (run-with [(object :generation 3) (object :generation 4)] {})]
      (is (re-find #"refusing: its spec changed after the confirmation" err))
      (is (not (str/includes? out "refusing")) "the refusal stays off stdout")
      (is (empty? executed))))
  (testing "replaced: refused"
    (is (empty? (:executed (run-with [(object :generation 3) (object :uid "u2" :generation 1)] {})))))
  (testing "unread before the confirmation: refused before asking, with nothing read again"
    (let [{:keys [executed err left]} (run-with [nil (object :uid "u2" :generation 1)] {})]
      (is (empty? executed))
      (is (= 1 left) "the object was not read again")
      (is (re-find #"refusing: it does not exist or could not be read" err))))
  (testing "unread when a covered change is shown: refused, whatever exists before the effect"
    (let [{:keys [executed err]} (run-with [nil (object :uid "u2" :generation 1)] {:confirmed? true})]
      (is (empty? executed))
      (is (re-find #"refusing: it could not be read when the change was confirmed" err))))
  (testing "a pin returning nil checks nothing"
    (let [{:keys [executed left]} (run-with [] {:pin (constantly nil)})]
      (is (zero? left))
      (is (= [[[:note "go"]]] executed) "the plan ran once"))))
