;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-named-target-test
  "A change book whose flags name a target never reports that target as
   needing no change when its select offered no candidate, unless a strict
   read found the object. The Clojure suspend book and its EDN form run
   through mutation/run! against a cluster holding no candidate, while the
   named CronJob is absent, present or unanswered."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.edn-book :as edn-book]
            [gumshoe.mutation :as mutation]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.shell :as shell]))

(def ^:private clj-book "runbooks/kubernetes/cronjobs/suspend.clj")
(def ^:private edn-book "tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn")

(defn- clj-spec
  []
  (with-redefs [mutation/book identity]
    (binding [runbook/*loading* true]
      (load-file clj-book))))

(defn- edn-spec
  []
  (edn-book/load-book edn-book))

(def ^:private suspended-cronjob
  "{\"apiVersion\":\"batch/v1\",\"kind\":\"CronJob\",\"metadata\":{\"name\":\"nightly\",\"namespace\":\"shop\",\"uid\":\"u1\",\"resourceVersion\":\"1\"},\"spec\":{\"suspend\":true}}")

(defn- answer
  "A cluster whose CronJob list offers no candidate: empty, or holding only
   the suspended nightly when it is :present. The strict presence read of
   shop/nightly answers as `presence` says."
  [presence argv]
  (let [presence-read? (some #{"--ignore-not-found"} argv)]
    (cond
      (some #{"config"} argv)
      {:exit 0 :out "{\"current-context\":\"ctx\",\"contexts\":[{\"name\":\"ctx\",\"context\":{\"cluster\":\"c\"}}]}" :err ""}

      (and presence-read? (= :unread presence))
      {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"}

      presence-read?
      {:exit 0 :out (if (= :present presence) "cronjob.batch/nightly\n" "") :err ""}

      (some #{"get"} argv)
      {:exit 0
       :out (if (= :present presence)
              (str "{\"items\":[" suspended-cronjob "]}")
              "{\"items\":[]}")
       :err ""}

      :else (throw (ex-info "the test cluster does not answer this" {:argv argv})))))

(defn- run
  [spec presence argv]
  (let [commands (atom [])
        out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        opts (:opts (runbook/arguments (:options (binding [runbook/*loading* true]
                                                    (mutation/book spec)))
                                       argv))
        result (binding [*out* out *err* err]
                 (with-redefs [shell/execute (fn [& argv]
                                               (swap! commands conj (vec argv))
                                               (answer presence (vec argv)))]
                   (mutation/run! spec opts nil)))]
    {:result result :printed (str out err) :commands @commands}))

(deftest named-target-reads-the-flags-each-mode-takes
  (is (= "shop/nightly" (select/named-target {:mode :namespaced :namespace-flag :namespace :name-flag :job}
                                             {:namespace "shop" :job "nightly"})))
  (is (= "nightly" (select/named-target {:mode :namespaced :namespace-flag :namespace :name-flag :job}
                                        {:job "nightly"})))
  (is (nil? (select/named-target {:mode :namespaced :namespace-flag :namespace :name-flag :job}
                                 {:namespace "shop"}))
      "a namespace alone names no object")
  (is (= "n1" (select/named-target {:mode :one :flag :node} {:node "n1"})))
  (is (nil? (select/named-target {:mode :one :flag :node} {:node " "})))
  (is (= ["a" "b"] (select/named-target {:mode :many :flag :orders} {:orders ["a" "b"]})))
  (is (nil? (select/named-target {:mode :many :flag :orders} {:orders []}))))

(deftest named-but-not-offered-never-calls-a-missing-target-done
  (let [select {:label "CronJob" :empty-message "every CronJob is suspended already"}]
    (is (= {:ok "every CronJob is suspended already"} (mutation/named-but-not-offered select "shop/nightly" :present)))
    (is (= {:error "no CronJob shop/nightly - not found"} (mutation/named-but-not-offered select "shop/nightly" :absent)))
    (is (str/starts-with? (:error (mutation/named-but-not-offered select "shop/nightly" :unread)) "could not read CronJob shop/nightly"))
    (is (str/includes? (:error (mutation/named-but-not-offered select ["a" "b"] nil)) "CronJob a, b not found among the candidates"))))

(deftest a-named-target-that-does-not-exist-fails
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [result printed]} (run spec :absent ["--namespace" "shop" "--job" "nightly"])]
        (is (false? result))
        (is (str/includes? printed "no CronJob shop/nightly - not found") printed)
        (is (not (str/includes? printed "suspended already")) printed)))))

(deftest a-named-target-that-exists-outside-the-candidates-needs-no-change
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [result printed]} (run spec :present ["--namespace" "shop" "--job" "nightly"])]
        (is (true? result))
        (is (str/includes? printed "every CronJob is suspended already") printed)))))

(deftest a-named-target-nothing-could-read-fails
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [result printed]} (run spec :unread ["--namespace" "shop" "--job" "nightly"])]
        (is (false? result))
        (is (str/includes? printed "could not read CronJob shop/nightly") printed)))))

(deftest a-name-without-its-namespace-cannot-be-read-and-fails
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [result printed commands]} (run spec :absent ["--job" "nightly"])]
        (is (false? result))
        (is (str/includes? printed "CronJob nightly not found among the candidates") printed)
        (is (not-any? #(some #{"--ignore-not-found"} %) commands))))))

(deftest no-named-target-keeps-the-idempotent-message
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [result printed commands]} (run spec :absent [])]
        (is (true? result))
        (is (str/includes? printed "every CronJob is suspended already") printed)
        (is (not-any? #(some #{"--ignore-not-found"} %) commands) "nothing named, nothing read")))))
