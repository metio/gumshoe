;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-named-target-test
  "A change book whose flags name a target never reports that target as
   needing no change when its select offered no candidate, unless a strict
   read found the object, and never asks for a different target when its
   select offered others. The Clojure suspend book and its EDN form run
   through mutation/run! against a cluster holding no candidate, or other
   CronJobs only, while the named CronJob is absent, present or unanswered;
   and against one offering the named CronJob, which is taken without
   asking."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.edn-book :as edn-book]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.interact :as interact]
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

      (and (some #{"get"} argv) (some #{"nightly"} argv))
      {:exit 0 :out suspended-cronjob :err ""}

      (some #{"get"} argv)
      {:exit 0
       :out (if (= :present presence)
              (str "{\"items\":[" suspended-cronjob "]}")
              "{\"items\":[]}")
       :err ""}

      :else (throw (ex-info "the test cluster does not answer this" {:argv argv})))))

(defn- cronjob
  [namespace name suspended?]
  (format "{\"apiVersion\":\"batch/v1\",\"kind\":\"CronJob\",\"metadata\":{\"name\":\"%s\",\"namespace\":\"%s\",\"uid\":\"u-%s\",\"resourceVersion\":\"1\"},\"spec\":{\"suspend\":%s}}"
          name namespace name suspended?))

(defn- answer-with-others
  "A cluster offering other CronJobs that are not suspended (shop/hourly,
   batch/nightly), beside shop/nightly as `nightly` says: :absent, :present
   suspended (so outside the candidates), :unread, or :offered not
   suspended (so among them)."
  [nightly argv]
  (let [presence-read? (some #{"--ignore-not-found"} argv)
        others [(cronjob "shop" "hourly" false) (cronjob "batch" "nightly" false)]
        listed (case nightly
                 :present (conj others (cronjob "shop" "nightly" true))
                 :offered (conj others (cronjob "shop" "nightly" false))
                 others)]
    (cond
      (some #{"config"} argv)
      {:exit 0 :out "{\"current-context\":\"ctx\",\"contexts\":[{\"name\":\"ctx\",\"context\":{\"cluster\":\"c\"}}]}" :err ""}

      (and presence-read? (= :unread nightly))
      {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"}

      presence-read?
      {:exit 0 :out (if (#{:present :offered} nightly) "cronjob.batch/nightly\n" "") :err ""}

      (and (some #{"get"} argv) (some #{"nightly"} argv))
      {:exit 0 :out (cronjob "shop" "nightly" (= :present nightly)) :err ""}

      (some #{"get"} argv)
      {:exit 0 :out (str "{\"items\":[" (str/join "," listed) "]}") :err ""}

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

(defn- run-among-others
  "Runs the spec against answer-with-others. The picker and the
   confirmation are recorded and answer nothing, so a run that asks for a
   pick shows it, and one that reaches the confirmation stops there."
  [spec nightly argv]
  (let [commands (atom [])
        pickers (atom [])
        confirmations (atom [])
        out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        opts (:opts (runbook/arguments (:options (binding [runbook/*loading* true]
                                                    (mutation/book spec)))
                                       argv))
        result (binding [*out* out *err* err]
                 (with-redefs [shell/execute (fn [& argv]
                                               (swap! commands conj (vec argv))
                                               (answer-with-others nightly (vec argv)))
                               fuzzy/select-single (fn [& args] (swap! pickers conj (vec args)) nil)
                               fuzzy/select-multi (fn [& args] (swap! pickers conj (vec args)) nil)
                               interact/confirm! (fn [request] (swap! confirmations conj request) false)]
                   (mutation/run! spec opts nil)))]
    {:result result :printed (str out err) :commands @commands
     :pickers @pickers :confirmations @confirmations}))

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

(deftest named-but-not-offered-never-calls-a-named-target-done
  (let [select {:label "CronJob" :empty-message "every CronJob is suspended already"}]
    (is (= "no CronJob shop/nightly - not found" (mutation/named-but-not-offered select "shop/nightly" :absent)))
    (is (str/starts-with? (mutation/named-but-not-offered select "shop/nightly" :unread) "could not read CronJob shop/nightly"))
    (is (str/starts-with? (mutation/named-but-not-offered select "shop/nightly" :not-eligible)
                          "CronJob shop/nightly is present but not eligible for this change"))
    (is (str/includes? (mutation/named-but-not-offered select ["a" "b"] nil) "CronJob a, b not found among the candidates"))))

(deftest a-named-target-that-does-not-exist-fails
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [result printed]} (run spec :absent ["--namespace" "shop" "--job" "nightly"])]
        (is (false? result))
        (is (str/includes? printed "no CronJob shop/nightly - not found") printed)
        (is (not (str/includes? printed "suspended already")) printed)))))

(deftest a-named-target-already-done-outside-the-candidates-is-skipped
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [result printed]} (run spec :present ["--namespace" "shop" "--job" "nightly"])]
        (is (true? result))
        (is (str/includes? printed "already holds: CronJob shop/nightly is suspended") printed)
        (is (str/includes? printed "skipped, already done: suspend a CronJob") printed)))))

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


(deftest a-named-target-missing-among-other-candidates-fails-without-asking
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [result printed pickers confirmations]} (run-among-others spec :absent ["--namespace" "shop" "--job" "nightly"])]
        (is (false? result))
        (is (str/includes? printed "no CronJob shop/nightly - not found") printed)
        (is (not (str/includes? printed "please select")) printed)
        (is (= [] pickers) "an operator who named a target is never asked to pick another")
        (is (= [] confirmations))))))

(deftest a-named-target-already-done-outside-other-candidates-is-skipped
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [result printed pickers confirmations]} (run-among-others spec :present ["--namespace" "shop" "--job" "nightly"])]
        (is (true? result) printed)
        (is (str/includes? printed "skipped, already done: suspend a CronJob") printed)
        (is (= [] pickers))
        (is (= [] confirmations))))))

(deftest a-named-target-nothing-could-read-among-other-candidates-fails
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [result printed pickers]} (run-among-others spec :unread ["--namespace" "shop" "--job" "nightly"])]
        (is (false? result))
        (is (str/includes? printed "could not read CronJob shop/nightly") printed)
        (is (= [] pickers))))))

(deftest a-name-matching-no-candidate-in-any-namespace-fails-closed
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [result printed pickers commands]} (run-among-others spec :absent ["--job" "weekly"])]
        (is (false? result))
        (is (str/includes? printed "CronJob weekly not found among the candidates") printed)
        (is (= [] pickers))
        (is (not-any? #(some #{"--ignore-not-found"} %) commands)
            "a name without its namespace names no object to read")))))

(deftest a-named-target-among-the-candidates-is-taken-without-asking
  (doseq [[form spec] [[:clj (clj-spec)] [:edn (edn-spec)]]]
    (testing form
      (let [{:keys [pickers confirmations printed]} (run-among-others spec :offered ["--namespace" "shop" "--job" "nightly"])]
        (is (= [] pickers) printed)
        (is (= 1 (count confirmations)) printed)
        (is (= "shop/nightly" (some->> confirmations first :items (some #(when (str/includes? (str %) "nightly") %)) str
                                       (re-find #"shop/nightly")))
            (pr-str confirmations))))))

(deftest a-namespace-alone-still-narrows-the-picker
  (let [{:keys [pickers result]} (run-among-others (clj-spec) :absent ["--namespace" "shop"])]
    (is (false? result) "the stub picker picks nothing")
    (is (= 1 (count pickers)) "naming no object leaves the choice to the operator")))

(deftest pick-refuses-a-named-target-the-candidates-do-not-offer-in-every-mode
  (let [picked (fn [select opts]
                 (let [pickers (atom [])
                       out (java.io.StringWriter.)]
                   (binding [*out* out *err* out]
                     (with-redefs [fuzzy/select-single (fn [_ values & _] (swap! pickers conj values) (first values))
                                   fuzzy/select-multi (fn [_ values] (swap! pickers conj values) [(first values)])]
                       (assoc (mutation/pick! {:select select} opts "ctx") :pickers @pickers :printed (str out))))))]
    (testing ":many with a lone other candidate, which fzf --select-1 would take without a terminal"
      (let [{:keys [result pickers printed]} (picked {:mode :many :label "Order" :flag :orders
                                                      :candidates (constantly ["shop/a"])}
                                                     {:orders ["shop/b"]})]
        (is (false? result))
        (is (= [] pickers))
        (is (str/includes? printed "Order shop/b not found among the candidates") printed)))
    (testing ":many with every named target offered"
      (let [{:keys [target pickers]} (picked {:mode :many :label "Order" :flag :orders :candidates (constantly ["shop/a" "shop/b"])}
                                             {:orders ["shop/a"]})]
        (is (= ["shop/a"] target))
        (is (= [] pickers))))
    (testing ":one"
      (is (false? (:result (picked {:mode :one :label "Node" :flag :node :candidates (constantly ["n1"])}
                                   {:node "n2"}))))
      (is (= "n1" (:target (picked {:mode :one :label "Node" :flag :node :candidates (constantly ["n1" "n2"])}
                                   {:node "n1"})))))
    (testing "nothing named still asks"
      (let [{:keys [target pickers]} (picked {:mode :one :label "Node" :flag :node :candidates (constantly ["n1" "n2"])} {})]
        (is (= "n1" target))
        (is (= 1 (count pickers)))))))

(deftest offered-reads-each-mode
  (is (select/offered? {:mode :one} "a" ["a" "b"]))
  (is (not (select/offered? {:mode :one} "c" ["a" "b"])))
  (is (select/offered? {:mode :many} ["a" "b"] ["a" "b" "c"]))
  (is (not (select/offered? {:mode :many} ["a" "x"] ["a" "b"])))
  (is (select/offered? {:mode :namespaced} "shop/web" ["shop/web"]))
  (is (not (select/offered? {:mode :namespaced} "shop/web" ["other/web"])))
  (is (select/offered? {:mode :namespaced} "web" ["other/web"]) "a bare name matches in any namespace")
  (is (not (select/offered? {:mode :namespaced} "web" ["shop/webapp" "shop/xweb"])))
  (is (not (select/offered? {:mode :one} "a" []))))

(deftest a-named-target-neither-done-nor-refused-outside-the-candidates-is-not-eligible
  (let [spec (-> (clj-spec) change/normalize (dissoc :done :refusal))]
    (doseq [[scenario run-fn] [[:empty #(run spec :present %)] [:among-others #(run-among-others spec :present %)]]]
      (testing scenario
        (let [{:keys [result printed confirmations]} (run-fn ["--namespace" "shop" "--job" "nightly"])]
          (is (false? result) "the filter that left it out may be one the change relies on")
          (is (str/includes? printed "CronJob shop/nightly is present but not eligible for this change") printed)
          (is (empty? confirmations)))))))
