;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.playbook-named-target-test
  "A playbook or a dispatcher whose flags name a target its select does not
   offer judges it by its :subject and :done, as a change book does: absent,
   present but not eligible, or already done. Without a :subject of its own,
   a playbook reads the target by the :subject of its first change step; one
   that declares neither fails closed with the generic message."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.dispatch :as dispatch]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.interact :as interact]
            [gumshoe.playbook :as playbook]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]))

(def ^:private namespaces
  "The cluster: shop is the one candidate, legacy exists without the label
   adoption sets, adopted carries it, gone does not exist."
  {"shop" {:apiVersion "v1" :kind "Namespace" :metadata {:name "shop" :uid "u-shop" :resourceVersion "1"}}
   "legacy" {:apiVersion "v1" :kind "Namespace" :metadata {:name "legacy" :uid "u-legacy" :resourceVersion "1"}}
   "adopted" {:apiVersion "v1" :kind "Namespace"
              :metadata {:name "adopted" :uid "u-adopted" :resourceVersion "1"
                         :labels {:adopted "true"}}}})

(defn- answer
  [commands]
  (fn [& argv]
    (let [argv (vec (filter string? argv))
          named (some #(get namespaces %) argv)]
      (swap! commands conj argv)
      (cond
        (some #{"config"} argv)
        {:exit 0 :out "{\"current-context\":\"ctx\",\"contexts\":[{\"name\":\"ctx\",\"context\":{\"cluster\":\"c\"}}]}" :err ""}

        (and (some #{"get"} argv) (some #{"--ignore-not-found"} argv))
        {:exit 0 :out (if named (str "namespace/" (-> named :metadata :name) "\n") "") :err ""}

        (and (some #{"get"} argv) (some #{"namespaces"} argv) (some #{"gone"} argv))
        {:exit 1 :out "" :err "Error from server (NotFound): namespaces \"gone\" not found"}

        (and (some #{"get"} argv) named)
        {:exit 0 :out (json/generate-string named) :err ""}

        :else {:exit 1 :out "" :err (str "the test cluster does not answer " argv)}))))

(def ^:private select
  {:mode :one :label "Namespace" :flag :namespace
   :candidates (fn [_] ["shop"])})

(def ^:private stamp
  {:title "stamp the namespace"
   :confirm {:action "stamp the namespace" :destructive? false}
   :prerequisites {}
   :effect (fn [{:keys [target]}] [[:kubectl "label" "namespace" target "adopted=true"]])
   :no-verify "the test never runs it"})

(def ^:private adopt
  {:description "adopt a namespace"
   :select select
   :subject {:type "namespaces"}
   :done (fn [{:keys [object]}] (= "true" (-> object :metadata :labels :adopted)))
   :confirm {:action "adopt the namespace" :destructive? false}
   :steps [stamp]})

(defn- run
  "Runs driver (playbook/run! or dispatch/run!) on spec with --namespace
   named. The picker and the confirmation answer nothing and are recorded."
  [driver spec named]
  (let [commands (atom [])
        asked (atom [])
        out (java.io.StringWriter.)
        result (binding [*out* out *err* out]
                 (with-redefs [shell/execute (answer commands)
                               fuzzy/select-single (fn [& args] (swap! asked conj [:pick args]) nil)
                               interact/confirm! (fn [request] (swap! asked conj [:confirm request]) false)]
                   (driver spec {:namespace named} nil)))]
    {:result result :printed (str out) :asked @asked :commands @commands}))

(deftest pick-spec-carries-the-subject-and-the-done-of-the-playbook
  (let [spec (playbook/pick-spec adopt)]
    (is (= select (:select spec)))
    (is (= {:type "namespaces"} (:subject spec)))
    (is (fn? (:done spec)))
    (is (= "adopt the namespace" (:title spec)))
    (is (map? (change/normalize spec))))
  (testing "a playbook without them picks by its select alone"
    (is (= {:select select :title "adopt the namespace"}
           (playbook/pick-spec (dissoc adopt :subject :done)))))
  (testing "the first change step's subject reads the target when the playbook declares none"
    (is (= [{:from :pick :type "namespaces"}]
           (mapv #(select-keys % [:from :type])
                 (:subjects (playbook/pick-spec (-> adopt
                                                    (dissoc :subject :done)
                                                    (assoc :steps [{:check (constantly true)}
                                                                   (assoc stamp :subject {:type "namespaces"})])))))))
    (is (nil? (:subjects (playbook/pick-spec (-> adopt
                                                 (dissoc :subject :done)
                                                 (assoc :steps [(assoc stamp :subject {:type "namespaces"}
                                                                       :select {:mode :one :label "Other" :flag :other})])))))
        "a step that picks through a select of its own reads another kind of target")))

(deftest a-playbook-names-a-target-that-does-not-exist
  (let [{:keys [result printed asked]} (run playbook/run! adopt "gone")]
    (is (false? result))
    (is (str/includes? printed "no Namespace gone - not found") printed)
    (is (= [] asked) "nothing is picked or confirmed")))

(deftest a-playbook-names-a-target-present-but-not-eligible
  (let [{:keys [result printed asked]} (run playbook/run! adopt "legacy")]
    (is (false? result))
    (is (str/includes? printed "Namespace legacy is present but not eligible for this change") printed)
    (is (= [] asked))))

(deftest a-playbook-names-a-target-already-done
  (let [{:keys [result printed asked commands]} (run playbook/run! adopt "adopted")]
    (is (true? result) printed)
    (is (str/includes? printed "skipped, already done: adopt the namespace") printed)
    (is (= [] asked))
    (is (not-any? #(some #{"label"} %) commands) "nothing was changed")))

(deftest a-playbook-reads-a-named-target-by-its-first-change-steps-subject
  (let [spec (-> adopt (dissoc :subject :done) (assoc :steps [(assoc stamp :subject {:type "namespaces"})]))]
    (is (str/includes? (:printed (run playbook/run! spec "gone")) "no Namespace gone - not found"))
    (is (str/includes? (:printed (run playbook/run! spec "adopted")) "Namespace adopted is present but not eligible")
        "a step's subject reads the target; only the playbook's :done can call it done")))

(deftest a-playbook-declaring-neither-fails-closed-as-before
  (let [{:keys [result printed commands]} (run playbook/run! (dissoc adopt :subject :done) "adopted")]
    (is (false? result))
    (is (str/includes? printed "Namespace adopted not found among the candidates - this book has no typed subject") printed)
    (is (not-any? #(some #{"--ignore-not-found"} %) commands) "nothing names an object to read")))

(deftest a-playbook-runs-an-offered-target-as-before
  (let [{:keys [asked printed]} (run playbook/run! adopt "shop")]
    (is (= [:confirm] (mapv first asked)) printed)))

(def ^:private dispatcher
  {:description "adopt by product"
   :select select
   :subject {:type "namespaces"}
   :done (fn [{:keys [object]}] (= "true" (-> object :metadata :labels :adopted)))
   :classify (constantly :web)
   :routes {:web {:playbook (dissoc adopt :subject :done :description)}}})

(deftest a-dispatcher-judges-a-named-target-by-its-subject-and-done
  (testing "absent"
    (let [{:keys [result printed]} (run dispatch/run! dispatcher "gone")]
      (is (false? result))
      (is (str/includes? printed "no Namespace gone - not found") printed)))
  (testing "present but not eligible"
    (let [{:keys [result printed]} (run dispatch/run! dispatcher "legacy")]
      (is (false? result))
      (is (str/includes? printed "Namespace legacy is present but not eligible") printed)))
  (testing "done"
    (let [{:keys [result printed asked]} (run dispatch/run! dispatcher "adopted")]
      (is (true? result) printed)
      (is (str/includes? printed "skipped, already done: adopt by product") printed)
      (is (= [] asked))))
  (testing "neither declared"
    (let [{:keys [result printed]} (run dispatch/run! (dissoc dispatcher :subject :done) "adopted")]
      (is (false? result))
      (is (str/includes? printed "not found among the candidates") printed))))

(deftest a-playbook-done-that-normalize-refuses-fails-as-the-book-loads
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":done true"
                        (binding [runbook/*loading* true]
                          (playbook/book (assoc adopt :done true))))))
