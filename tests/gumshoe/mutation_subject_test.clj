;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-subject-test
  "The :subject and :refusal keys of a mutation spec, against stubbed reads."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn- cronjob
  [& {:keys [suspend generation uid] :or {suspend false generation 1 uid "u1"}}]
  {:metadata {:namespace "batch" :name "nightly" :uid uid :generation generation}
   :spec {:suspend suspend}})

(defn- unread!
  []
  (throw (ex-info "kubectl get did not answer: i/o timeout" {::kubectl/unread true})))

(defn- run
  "Runs spec on target with reads answered by (read ref call-number) and
   flow/change! recorded. Returns what the run printed and returned, the
   config change! received, and the reads it made, each with whether it ran
   under strict reads."
  [spec target read & {:keys [between]}]
  (let [reads (atom []) seen (atom nil)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  mutation/fetch-pinned (fn [_ ref]
                                          (swap! reads conj [ref kubectl/*strict-reads*])
                                          (read ref (count @reads)))
                  flow/change! (fn [config]
                                 (reset! seen config)
                                 (when between (between))
                                 (if-let [p (:precondition config)] (p) true))]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out]
                     (mutation/run-on! (merge {:select {:mode :namespaced :label "CronJob"}
                                               :confirm {:action "suspend a CronJob"}
                                               :effect (fn [_] (effect/plan (effect/note "go")))}
                                              spec)
                                       target {} nil))]
        {:result result :out (str out) :config @seen :reads @reads}))))

(deftest the-subject-is-parsed-read-strictly-and-handed-to-every-callback
  (let [seen (atom [])
        record (fn [k] (fn [ctx] (swap! seen conj [k mutation/*phase* (select-keys ctx [:ref :object])]) nil))
        seen-in (fn [k phase] (some (fn [[k' phase' ctx]] (when (and (= k k') (= phase phase')) ctx)) @seen))
        {:keys [result reads]} (run {:subject {:type "cronjobs"}
                                     :derive (fn [ctx] ((record :derive) ctx) {})
                                     :items (fn [ctx] ((record :items) ctx) ["x"])
                                     :refusal (record :refusal)
                                     :precondition (fn [ctx] ((record :precondition) ctx) true)}
                                    "batch/nightly"
                                    (fn [_ n] (cronjob :suspend (> n 1))))]
    (is (true? result))
    (is (= {:type "cronjobs" :namespace "batch" :name "nightly"} (ffirst reads)))
    (is (every? second reads) "every read of the subject is strict")
    (testing ":derive and :items see the object read before the confirmation"
      (is (= false (get-in (seen-in :derive :derive) [:object :spec :suspend])))
      (is (= false (get-in (seen-in :items nil) [:object :spec :suspend]))))
    (testing ":refusal and :precondition judge the object read before the confirmation first"
      (is (= false (get-in (seen-in :refusal :derive) [:object :spec :suspend])))
      (is (= false (get-in (seen-in :precondition :derive) [:object :spec :suspend]))))
    (testing ":refusal and :precondition see the object read again after it"
      (is (= true (get-in (seen-in :refusal :judge) [:object :spec :suspend])))
      (is (= true (get-in (seen-in :precondition :judge) [:object :spec :suspend]))))))

(deftest a-refusal-is-printed-once-and-stops-the-change
  (let [precondition-ran (atom false)
        {:keys [result out]} (run {:subject {:type "cronjobs"}
                                   :refusal (fn [{:keys [object]}] (when (-> object :spec :suspend) "it is suspended already"))
                                   :precondition (fn [_] (reset! precondition-ran true) true)}
                                  "batch/nightly" (fn [_ _] (cronjob :suspend true)))]
    (is (false? result))
    (is (= "refusing: it is suspended already\n" out))
    (is (false? @precondition-ran) "the precondition is not consulted after a refusal")))

(deftest a-refusal-works-without-a-subject
  (let [{:keys [result out]} (run {:refusal (fn [{:keys [target]}] (str target " is busy"))}
                                  "batch/nightly" (fn [_ _] nil))]
    (is (false? result))
    (is (= "refusing: batch/nightly is busy\n" out))))

(deftest a-pick-that-does-not-parse-is-refused-with-the-subject-s-words
  (testing "the default names the pick and the label"
    (let [{:keys [result out reads config]} (run {:subject {:type "cronjobs"}} "nightly" (fn [_ _] (cronjob)))]
      (is (false? result))
      (is (= "refusing: the pick \"nightly\" does not name a CronJob\n" out))
      (is (empty? reads) "nothing is read for a pick that names nothing")
      (is (nil? config) "no confirmation is shown for a pick that names nothing")))
  (testing ":invalid replaces it"
    (is (= "refusing: the pick is not \"<namespace>/<name>\"\n"
           (:out (run {:subject {:type "cronjobs" :invalid "the pick is not \"<namespace>/<name>\""}}
                      "nightly" (fn [_ _] (cronjob))))))))

(deftest the-parser-may-name-the-type
  (let [{:keys [reads]} (run {:select {:mode :one :label "Workload"}
                              :subject {:parse (fn [t] (when (= t "Deployment shop/web")
                                                         {:kind "Deployment" :type "deployments" :namespace "shop" :name "web"}))}}
                             "Deployment shop/web" (fn [_ _] (cronjob)))]
    (is (= {:kind "Deployment" :type "deployments" :namespace "shop" :name "web"} (ffirst reads)))))

(deftest a-one-select-defaults-to-a-cluster-scoped-name
  (let [{:keys [reads]} (run {:select {:mode :one :label "Node"} :subject {:type "nodes"}}
                             "worker-1" (fn [_ _] (cronjob)))]
    (is (= {:type "nodes" :name "worker-1"} (ffirst reads)))))

(deftest the-subject-is-pinned
  (testing "a spec change between confirmation and precondition refuses"
    (let [{:keys [result out]} (run {:subject {:type "cronjobs"}} "batch/nightly"
                                    (fn [_ n] (cronjob :generation n)))]
      (is (false? result))
      (is (re-find #"refusing: its spec changed after the confirmation \(generation 1, now \d\)" out))))
  (testing "the object read before the confirmation is the fingerprint, so it is read once there"
    (let [{:keys [result reads]} (run {:subject {:type "cronjobs"}} "batch/nightly" (fn [_ _] (cronjob)))]
      (is (true? result))
      (is (= 2 (count reads)) "once before the confirmation, once for the drift check"))
    (let [{:keys [reads]} (run {:subject {:type "cronjobs"} :refusal (constantly nil)} "batch/nightly" (fn [_ _] (cronjob)))]
      (is (= 3 (count reads)) "a refusal reads the object again for itself")))
  (testing ":facts joins the fingerprint"
    (let [{:keys [result out]} (run {:subject {:type "cronjobs" :facts #(-> % :spec :suspend)}} "batch/nightly"
                                    (fn [_ n] (cronjob :suspend (> n 1))))]
      (is (false? result))
      (is (re-find #"refusing: what the change rests on moved after the confirmation \(false, now true\)" out))))
  (testing "an explicit :pin takes precedence"
    (let [{:keys [reads]} (run {:subject {:type "cronjobs"}
                                :pin (constantly {:type "configmaps" :namespace "batch" :name "settings"})}
                               "batch/nightly" (fn [_ _] (cronjob)))]
      (is (some #(= {:type "configmaps" :namespace "batch" :name "settings"} (first %)) reads)))))

(deftest an-unread-subject-stops-before-the-confirmation
  (let [{:keys [result out config]} (run {:subject {:type "cronjobs"}} "batch/nightly" (fn [_ _] (unread!)))]
    (is (false? result))
    (is (nil? config) "no confirmation is shown")
    (is (re-find #"could not read what the change rests on - kubectl get did not answer" out))))

(deftest an-unread-subject-after-the-confirmation-refuses
  (let [refused (atom [])
        {:keys [result out]} (run {:subject {:type "cronjobs"}
                                   :refusal (fn [_] (swap! refused conj mutation/*phase*) nil)}
                                  "batch/nightly"
                                  (fn [_ n] (if (= n 1) (cronjob) (unread!))))]
    (is (false? result))
    (is (= [:derive] @refused) "the refusal judged the answered read before the confirmation, and never a read that did not answer")
    (is (re-find #"refusing: could not read what the precondition rests on - kubectl get did not answer" out))))

(deftest a-subject-needs-a-select-that-picks-one
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"picks one object"
                        (run {:select {:mode :many :label "Node"} :subject {:type "nodes"}} ["a" "b"] (fn [_ _] nil)))))

(deftest read-subject-reads-the-ref-of-the-context
  (with-redefs [kubectl/get-namespaced-resource (fn [ctx ns type name] [ctx ns type name])]
    (is (= ["ctx" "batch" "cronjobs" "nightly"]
           (mutation/read-subject {:context "ctx" :ref {:type "cronjobs" :namespace "batch" :name "nightly"}})))
    (is (nil? (mutation/read-subject {:context "ctx" :ref nil})))))

(deftest help-mentions-the-check-for-a-subject-or-refusal
  (is (some #{"  checks again right before acting that what was confirmed still holds"}
            (mutation/details {:select {:mode :one :label "Node"} :confirm {:action "x"} :subject {:type "nodes"}})))
  (is (some #{"  checks again right before acting that what was confirmed still holds"}
            (mutation/details {:select {:mode :one :label "Node"} :confirm {:action "x"} :refusal (constantly nil)}))))
