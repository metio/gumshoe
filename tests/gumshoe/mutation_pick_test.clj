;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-pick-test
  "mutation/parse-pick turns a pick into a ref or a refusal, and run!,
   run-on! and playbook steps refuse a pick it refuses before they read,
   derive, connect or ask for a confirmation."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.playbook :as playbook]
            [gumshoe.runbook :as runbook]))

(def ^:private namespaced {:mode :namespaced :label "CronJob" :namespace-flag :namespace :name-flag :job})
(def ^:private one {:mode :one :label "Node" :flag :node})
(def ^:private many {:mode :many :label "Certificate" :flag :certificates})

;; --- the pure parse ------------------------------------------------------------

(deftest a-subject-pick-parses-into-its-ref
  (is (= {:ref {:type "cronjobs" :namespace "batch" :name "nightly"}}
         (mutation/parse-pick {:select namespaced :subject {:type "cronjobs"}} "batch/nightly")))
  (is (= {:ref {:type "nodes" :name "n1"}}
         (mutation/parse-pick {:select one :subject {:type "nodes"}} "n1")))
  (is (= {:ref {:kind "Deployment" :type "deployments" :namespace "shop" :name "web"}}
         (mutation/parse-pick {:select {:mode :one :label "Workload"}
                               :subject {:parse (fn [_] {:kind "Deployment" :type "deployments"
                                                         :namespace "shop" :name "web"})}}
                              "Deployment shop/web"))))

(deftest a-subject-pick-that-does-not-parse-is-refused
  (testing "with the default words, naming the pick and the label"
    (doseq [target ["nightly" "" "  " nil "a/b/c" ["batch/nightly"]]]
      (is (= {:refusal (format "the pick %s does not name a CronJob" (pr-str target))}
             (mutation/parse-pick {:select namespaced :subject {:type "cronjobs"}} target))
          (pr-str target))))
  (testing "with the subject's :invalid"
    (is (= {:refusal "the pick is not \"<namespace>/<name>\""}
           (mutation/parse-pick {:select namespaced
                                 :subject {:type "cronjobs" :invalid "the pick is not \"<namespace>/<name>\""}}
                                "nightly"))))
  (testing "a parser naming no type leaves no ref"
    (is (contains? (mutation/parse-pick {:select one :subject {:parse (constantly {:name "n1"})}} "n1")
                   :refusal))))

(deftest a-subject-on-a-many-select-is-a-mistake-in-the-spec
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"a :subject needs a select that picks one object"
                        (mutation/parse-pick {:select many :subject {:type "certificates"}} ["shop/web"]))))

(deftest a-namespaced-pick-without-a-subject-must-name-a-namespace-and-a-name
  (is (= {:ref nil} (mutation/parse-pick {:select namespaced} "batch/nightly")))
  (is (= {:ref nil} (mutation/parse-pick {:select namespaced} " batch/nightly ")))
  (doseq [target ["nightly" "batch/" "/nightly" "a/b/c" "batch/night ly" "" "   " nil 42 ["batch/nightly"]]]
    (is (= {:refusal (format "the pick %s does not name a CronJob" (pr-str target))}
           (mutation/parse-pick {:select namespaced} target))
        (pr-str target))))

(deftest a-single-pick-without-a-subject-must-be-a-non-blank-string
  (doseq [select [one nil]]
    (testing (pr-str select)
      (is (= {:ref nil} (mutation/parse-pick {:select select} "n1")))
      (testing "a free-form pick is the book's to read"
        (is (= {:ref nil} (mutation/parse-pick {:select select} "Deployment shop/web")))
        (is (= {:ref nil} (mutation/parse-pick {:select select} "3 instance(s)"))))
      (doseq [target [nil "" "  \t" :n1 42 {:name "n1"} ["n1"]]]
        (is (contains? (mutation/parse-pick {:select select} target) :refusal) (pr-str target)))))
  (is (= {:refusal "the pick nil does not name a Node"} (mutation/parse-pick {:select one} nil)))
  (is (= {:refusal "the pick nil does not name a subject"} (mutation/parse-pick {} nil))
      "a step without a select is named by the generic word"))

(deftest a-many-pick-must-be-a-non-empty-collection-of-non-blank-strings
  (is (= {:ref nil} (mutation/parse-pick {:select many} ["shop/web-tls"])))
  (is (= {:ref nil} (mutation/parse-pick {:select many} '("a" "b"))))
  (is (= {:ref nil} (mutation/parse-pick {:select many} #{"a"})))
  (doseq [target [nil [] '() [nil] [""] ["a" " "] ["a" nil] "shop/web-tls" {:a "b"} [:a]]]
    (is (= {:refusal (format "the pick %s does not name a Certificate" (pr-str target))}
           (mutation/parse-pick {:select many} target))
        (pr-str target))))

;; --- the three entry points ---------------------------------------------------

(defn- unexpected
  [what]
  (fn [& _] (throw (ex-info (str what " must not run for a refused pick") {}))))

(defmacro ^:private refused-run
  "Runs body with every read, connection and confirmation failing the test
   should it run; {:result :out}."
  [& body]
  `(with-redefs [kubectl/current-context (constantly "ctx")
                 kubectl/current-cluster (constantly "cluster")
                 mutation/fetch-pinned (unexpected "a read")
                 flow/change! (unexpected "the confirmation")]
     (let [out# (java.io.StringWriter.)
           result# (binding [*out* out# *err* out#] (try ~@body (catch Exception e# (str "threw: " (ex-message e#)))))]
       {:result result# :out (str out#)})))

(defn- spec
  [& {:as overrides}]
  (merge {:select (assoc namespaced :candidates (constantly ["batch/nightly"]))
          :confirm {:action "start a CronJob" :destructive? false}
          :derive (unexpected ":derive")
          :items (unexpected ":items")
          :pin (unexpected ":pin")
          :subjects (unexpected ":subjects")
          :refusal (unexpected ":refusal")
          :precondition (unexpected ":precondition")
          :announce (unexpected ":announce")
          :effect (unexpected ":effect")
          :verify (unexpected ":verify")}
         overrides))

(deftest run-on-refuses-before-anything-runs
  (doseq [[label s] [["without a subject" (spec)]
                     ["with a subject" (spec :subject {:type "cronjobs"})]]
          target ["nightly" nil "" "  "]]
    (testing (str label ", " (pr-str target))
      (is (= {:result false :out (format "refusing: the pick %s does not name a CronJob\n" (pr-str target))}
             (refused-run (mutation/run-on! s target {} nil)))))))

(deftest run-on-refuses-before-it-connects
  (is (= {:result false :out "refusing: the pick nil does not name a OSD\n"}
         (refused-run (mutation/run-on! (spec :select {:mode :one :label "OSD"}
                                              :pin nil :subjects nil
                                              :connect (unexpected ":connect"))
                                        nil {} nil)))))

(deftest run-refuses-a-pick-that-does-not-parse-the-same-way
  (doseq [[label s] [["without a subject" (spec)]
                     ["with a subject" (spec :subject {:type "cronjobs"})]]]
    (testing label
      (with-redefs [fuzzy/select-single (constantly "nightly")]
        (let [{:keys [result out]} (refused-run (mutation/run! s {} nil))]
          (is (false? result))
          (is (re-find #"refusing: the pick \"nightly\" does not name a CronJob\n$" out)))))))

(deftest a-playbook-step-refuses-a-pick-that-does-not-parse
  ;; The playbook judges every step's pick before its own one confirmation,
  ;; so the operator is never asked to approve a step that will be refused.
  (let [step (spec :title "start it")
        asked (atom 0)
        book (binding [runbook/*loading* true]
               (playbook/book {:description "starts a CronJob"
                               :select (:select step)
                               :confirm {:action "start a CronJob" :destructive? false}
                               :steps [step]}))]
    (with-redefs [fuzzy/select-single (constantly "nightly")
                  interact/confirm! (fn [_] (swap! asked inc) true)]
      (let [{:keys [result out]} (refused-run ((:action book) {} {}))]
        (is (false? result))
        (is (zero? @asked) "the playbook's confirmation is not asked")
        (is (re-find #"refusing: the pick \"nightly\" does not name a CronJob" out))))))

(deftest an-accepted-pick-still-runs
  (let [seen (atom nil)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  flow/change! (fn [config] (reset! seen config) true)]
      (is (true? (mutation/run-on! {:select namespaced
                                    :confirm {:action "start a CronJob" :destructive? false}
                                    :effect (fn [{:keys [target]}] (effect/plan (effect/note target)))}
                                   "batch/nightly" {} nil)))
      (is (= ["batch/nightly"] (get-in @seen [:confirmation :items]))))))
