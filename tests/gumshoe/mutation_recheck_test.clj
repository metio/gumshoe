;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-recheck-test
  "The :recheck key of a mutation spec, the live state :refusal and
   :precondition judge read again after the confirmation, against a stubbed
   shell/execute."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]))

(defn- configmap
  [name secret]
  {:metadata {:namespace "hedgedoc" :name name :uid (str name "-uid") :generation 1}
   :data {"secretName" secret}})

(defn- secret-of
  "The secretName the ConfigMap `name` in hedgedoc holds, read with kubectl."
  [context name]
  (get-in (kubectl/get-namespaced-resource context "hedgedoc" "configmaps" name) [:data :secretName]))

(defn- run
  "Runs spec on the namespace hedgedoc with every kubectl get answered by
   (answer name phase), where phase is :before until flow/change! is called
   and :after from then on; an answer of :unread is a read that did not
   answer, nil one that found nothing. Returns what the run printed and
   returned, the config change! received, and every read as [name phase
   mutation/*phase*]."
  [spec answer]
  (let [reads (atom []) seen (atom nil) phase (atom :before)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  shell/execute (fn [& argv]
                                  (let [name (nth argv (+ 2 (.indexOf ^java.util.List (vec argv) "get")))
                                        _ (swap! reads conj [name @phase mutation/*phase*])
                                        answer (answer name @phase)]
                                    (cond
                                      (= :unread answer) {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"}
                                      (nil? answer) {:exit 1 :out "" :err (str "Error from server (NotFound): configmaps \"" name "\" not found")}
                                      :else {:exit 0 :out (json/generate-string answer) :err ""})))
                  flow/change! (fn [config]
                                 (reset! seen config)
                                 (reset! phase :after)
                                 (if-let [p (:precondition config)] (p) true))]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out]
                     (mutation/run-on! (merge {:select {:mode :one :label "Namespace"}
                                               :confirm {:action "stamp hedgedoc"}
                                               :effect (fn [_] (effect/plan (effect/note "go")))}
                                              spec)
                                       "hedgedoc" {} nil))]
        {:result result :out (str out) :config @seen :reads @reads}))))

(def ^:private moving
  "The secretName of the ConfigMap database moves from old to new between
   the confirmation and the effect, with its uid and generation unchanged."
  (fn [name phase] (configmap name (if (= :after phase) "new" "old"))))

(def ^:private derive-secret
  (fn [{:keys [context]}] {:secret (secret-of context "database")}))

(def ^:private keeps-old-secret
  (fn [{:keys [secret]}] (= "old" secret)))

(deftest the-precondition-judges-what-recheck-reads-after-the-confirmation
  (testing "without :recheck the precondition judges the value :derive read before the confirmation"
    (let [{:keys [result]} (run {:derive derive-secret :precondition keeps-old-secret} moving)]
      (is (true? result))))
  (testing "with :recheck the precondition sees the value read again after the confirmation"
    (let [{:keys [result reads]} (run {:derive derive-secret
                                       :recheck derive-secret
                                       :precondition keeps-old-secret}
                                      moving)]
      (is (false? result))
      (is (= [["database" :before :derive] ["database" :after :judge]] reads)
          "derive reads once before the confirmation, recheck once after it"))))

(deftest recheck-values-reach-only-the-judgement
  (let [seen (atom {})
        record (fn [k] (fn [ctx] (swap! seen assoc k (select-keys ctx [:secret :before-secret]))))
        {:keys [result config]} (run {:derive derive-secret
                                      :recheck (fn [{:keys [context secret]}]
                                                 {:secret (secret-of context "database") :before-secret secret})
                                      :refusal (fn [ctx] ((record :refusal) ctx) nil)
                                      :precondition (fn [ctx] ((record :precondition) ctx) true)
                                      :effect (fn [ctx] ((record :effect) ctx) (effect/plan (effect/note "go")))
                                      :verify (fn [ctx] ((record :verify) ctx) [])}
                                     moving)]
    (is (true? result))
    (is (some? config))
    (testing ":recheck receives the derived values, so it can compare them with what it reads"
      (is (= {:secret "new" :before-secret "old"} (:refusal @seen)))
      (is (= {:secret "new" :before-secret "old"} (:precondition @seen))))
    (testing ":effect and :verify keep what :derive read, which the confirmation showed"
      (is (= {:secret "old"} (:effect @seen)))
      (is (= {:secret "old"} (:verify @seen))))))

(deftest recheck-sees-the-subject-read-again
  (let [seen (atom nil)
        {:keys [result]} (run {:select {:mode :one :label "ConfigMap"}
                               :subject {:type "configmaps"}
                               :recheck (fn [{:keys [object]}] (reset! seen (get-in object [:data :secretName])) {})
                               :precondition (constantly true)}
                              moving)]
    (is (true? result) "the uid and generation held, so the pin lets the change run")
    (is (= "new" @seen))))

(deftest an-unanswered-recheck-read-refuses-before-the-judgement
  (let [judged (atom false)
        {:keys [result out]} (run {:derive derive-secret
                                   :recheck derive-secret
                                   :refusal (fn [_] (reset! judged true) nil)}
                                  (fn [name phase] (if (= :after phase) :unread (configmap name "old"))))]
    (is (false? result))
    (is (false? @judged) "the refusal never judges a read that did not answer")
    (is (str/includes? out "refusing: could not read what the precondition rests on - "))))

(deftest a-recheck-read-that-finds-nothing-is-judged
  (let [{:keys [result out]} (run {:derive derive-secret
                                   :recheck derive-secret
                                   :refusal (fn [{:keys [secret]}] (when-not secret "the database ConfigMap is gone"))}
                                  (fn [name phase] (when (= :before phase) (configmap name "old"))))]
    (is (false? result))
    (is (= "refusing: the database ConfigMap is gone\n" out))))

(deftest recheck-without-a-judgement-is-a-spec-error
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":recheck is judged by :refusal or :precondition"
                        (run {:derive derive-secret :recheck derive-secret} moving))))

(deftest help-mentions-the-check-for-recheck
  (is (some #{"  checks again right before acting that what was confirmed still holds"}
            (mutation/details {:select {:mode :one :label "Namespace"} :confirm {:action "x"}
                               :recheck derive-secret :precondition keeps-old-secret}))))

(deftest every-read-runs-in-the-phase-of-its-side-of-the-confirmation
  (let [ref (fn [name] {:type "configmaps" :namespace "hedgedoc" :name name})
        {:keys [result reads]} (run {:subjects (fn [_] [(ref "volume") (ref "database")])
                                     :pin (fn [_] (ref "settings"))
                                     :derive derive-secret
                                     :recheck derive-secret
                                     :precondition (constantly true)}
                                    (fn [name _] (configmap name "same")))]
    (is (true? result))
    (is (= #{[:before :derive]} (set (map rest (filter #(= :before (second %)) reads))))
        "the subjects, :derive and the pin's fingerprint before the confirmation")
    (is (= #{[:after :judge]} (set (map rest (filter #(= :after (second %)) reads))))
        "the subjects, :recheck and every drift check after it")
    (is (= ["volume" "database" "database" "settings" "volume" "database"]
           (map first (filter #(= :after (second %)) reads)))
        "the subjects read again, then :recheck, then the pin's and the subjects' drift checks")))
