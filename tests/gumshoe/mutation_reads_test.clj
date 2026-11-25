;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-reads-test
  "What a mutation spec's reads cost: each object fetched once per phase,
   independent reads issued at once, a read after the confirmation never
   served from a read before it, and every poll of a post-check sent to the
   cluster. A run goes through the real flow/change! against a recorded
   cluster whose kubectl counts every get with the phase it ran in."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

;; --- the recorded cluster -------------------------------------------------------------

(defn- object
  [type name & {:keys [generation data] :or {generation 1}}]
  [[type "batch" name]
   {:metadata {:namespace "batch" :name name :uid (str name "-uid") :generation generation}
    :data (or data {})}])

(defn- arg [argv prefix]
  (some #(when (and (string? %) (str/starts-with? % prefix)) (subs % (count prefix))) argv))

(defn- key-of
  "[type namespace name] of a kubectl get of one object."
  [argv]
  (let [[_ type name] (drop-while #(not= "get" %) argv)]
    [type (arg argv "--namespace=") name]))

(defn- answer
  "What kubectl prints for the object k of objects: the object, NotFound
   for one that is not there, and a timeout for one held as :unanswered."
  [objects k]
  (let [o (get objects k)]
    (cond
      (= :unanswered o) {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"}
      o {:exit 0 :out (json/generate-string o) :err ""}
      :else {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" (first k) (last k))})))

(defn- run
  "Runs spec on target through mutation/run-on! and the real flow/change!,
   against a cluster holding objects. The operator confirms; `confirming`,
   when given, is (fn [objects] -> objects) and edits the cluster while the
   confirmation is shown. `meet` is (fn [phase]) called inside every get,
   for a test about concurrency. Returns what the run returned and printed
   and every get as {:object [type namespace name] :phase *phase* :strict?}."
  [spec target objects & {:keys [confirming meet dry-run?]}]
  (let [cluster (atom (into {} objects))
        reads (atom [])]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  shell/execute (fn [& argv]
                                  (let [k (key-of argv)
                                        phase mutation/*phase*]
                                    (swap! reads conj {:object k :phase phase :strict? kubectl/*strict-reads*})
                                    (when meet (meet phase))
                                    (answer @cluster k)))
                  interact/confirm! (fn [_] (when confirming (swap! cluster confirming)) true)
                  effect/run! (fn [_] true)]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out flow/*dry-run* (boolean dry-run?)]
                     (mutation/run-on! (merge {:select {:mode :namespaced :label "CronJob"}
                                               :confirm {:action "suspend a CronJob"}
                                               :effect (fn [_] (effect/plan (effect/note "go")))}
                                              spec)
                                       target {} nil))]
        {:result result :out (str out) :reads @reads}))))

(defn- fetches
  "How often each object was fetched in phase."
  [reads phase]
  (frequencies (map :object (filter #(= phase (:phase %)) reads))))

(def ^:private cronjob-key ["cronjobs" "batch" "nightly"])
(def ^:private settings-key ["configmaps" "batch" "settings"])
(def ^:private creds-key ["secrets" "batch" "creds"])

(def ^:private cluster
  [(object "cronjobs" "nightly" :data {:suspend "false"})
   (object "configmaps" "settings" :data {:mode "on"})
   (object "secrets" "creds")])

(defn- settings-mode
  [context]
  (get-in (kubectl/get-namespaced-resource context "batch" "configmaps" "settings") [:data :mode]))

(def ^:private reading-spec
  "A spec that reads the same objects from every callback: the subject,
   two subjects, a :derive and a :recheck that read the subject and a
   subject again, a :refusal and the default pin of the subject."
  {:subject {:type "cronjobs"}
   :subjects (fn [_] [{:type "configmaps" :namespace "batch" :name "settings"}
                      {:type "secrets" :namespace "batch" :name "creds"}])
   :derive (fn [{:keys [context] :as ctx}]
             {:mode (settings-mode context) :suspend (get-in (mutation/read-subject ctx) [:data :suspend])})
   :recheck (fn [{:keys [context] :as ctx}]
              {:mode (settings-mode context) :suspend (get-in (mutation/read-subject ctx) [:data :suspend])})
   :refusal (fn [{:keys [suspend object]}]
              (cond
                (= "true" suspend) "it is suspended already"
                (not= suspend (get-in object [:data :suspend])) "the subject and the recheck disagree"))})

;; --- each object once per phase -------------------------------------------------------

(deftest each-object-is-fetched-once-per-phase
  (let [{:keys [result reads]} (run reading-spec "batch/nightly" cluster)]
    (is (true? result))
    (is (= {cronjob-key 1 settings-key 1 creds-key 1} (fetches reads :derive))
        "the subject, the subjects, :derive and the pin's fingerprint share one read of each")
    (is (= {cronjob-key 1 settings-key 1 creds-key 1} (fetches reads :judge))
        "the subject and subjects again, :recheck, :refusal and every drift check share one read of each")
    (is (every? :strict? reads) "every read of both phases is strict")
    (is (= #{:derive :judge} (set (map :phase reads))) "no read runs outside a phase")))

(deftest a-dry-run-reads-each-object-once-per-phase
  (let [{:keys [result reads]} (run reading-spec "batch/nightly" cluster :dry-run? true)]
    (is (true? result))
    (is (= {cronjob-key 1 settings-key 1 creds-key 1} (fetches reads :derive)))
    (is (= {cronjob-key 1 settings-key 1 creds-key 1} (fetches reads :judge))
        "the rehearsal judges against its own reads, never the ones before it")))

;; --- the confirmation divides the caches ---------------------------------------------

(deftest a-read-after-the-confirmation-sees-an-edit-made-during-it
  (testing "a :refusal judging the subject's data sees the edit"
    (let [{:keys [result out reads]} (run reading-spec "batch/nightly" cluster
                                          :confirming #(assoc-in % [cronjob-key :data :suspend] "true"))]
      (is (false? result))
      (is (str/includes? out "refusing: it is suspended already"))
      (is (= 1 (get (fetches reads :judge) cronjob-key)) "the subject was fetched again after the confirmation")))
  (testing "the pin sees a spec change made during it"
    (let [{:keys [result out]} (run (dissoc reading-spec :refusal :recheck) "batch/nightly" cluster
                                    :confirming #(assoc-in % [cronjob-key :metadata :generation] 2))]
      (is (false? result))
      (is (str/includes? out "refusing: its spec changed after the confirmation (generation 1, now 2)"))))
  (testing "a subject's drift check sees a replacement made during it"
    (let [{:keys [result out]} (run (dissoc reading-spec :refusal :recheck) "batch/nightly" cluster
                                    :confirming #(assoc-in % [creds-key :metadata :uid] "other"))]
      (is (false? result))
      (is (str/includes? out "refusing: secrets batch/creds: it was deleted and created again after the confirmation"))))
  (testing ":recheck sees what moved during it"
    (let [{:keys [result out]} (run (assoc reading-spec :refusal (fn [{:keys [mode]}] (when (= "off" mode) "settings turned off")))
                                    "batch/nightly" cluster
                                    :confirming #(assoc-in % [settings-key :data :mode] "off"))]
      (is (false? result))
      (is (str/includes? out "refusing: settings turned off")))))

;; --- post-checks poll the cluster ------------------------------------------------------

(deftest every-poll-of-a-post-check-reads-the-cluster
  (let [polls (atom 0)
        spec (assoc reading-spec
                    :verify (fn [ctx]
                              [{:description "suspended"
                                :timeout 10
                                :interval 0
                                :check (fn []
                                         (swap! polls inc)
                                         (and (mutation/read-subject ctx)
                                              (<= 3 @polls)))}]))
        {:keys [result reads]} (run spec "batch/nightly" cluster)]
    (is (true? result))
    (is (= 3 @polls))
    (is (= 3 (count (filter #(nil? (:phase %)) reads)))
        "the check read the subject on each of its three polls, outside both phases")
    (is (= {cronjob-key 3} (fetches reads nil)))))

;; --- independent reads run at once ---------------------------------------------------

(defn- meeting
  "(fn [phase]) for run's :meet: each get of a phase waits until n gets of
   that phase are running, and records whether they all met. Reads issued
   one after another never meet: the first waits alone and times out."
  [n met]
  (let [latches (atom {})]
    (fn [phase]
      (let [latch (get (swap! latches #(if (contains? % phase) % (assoc % phase (CountDownLatch. n)))) phase)]
        (.countDown latch)
        (swap! met conj [phase (.await latch 5 TimeUnit/SECONDS)])))))

(def ^:private three-subjects
  {:select {:mode :one :label "Namespace"}
   :subjects (fn [_] [{:type "cronjobs" :namespace "batch" :name "nightly"}
                      {:type "configmaps" :namespace "batch" :name "settings"}
                      {:type "secrets" :namespace "batch" :name "creds"}])})

(deftest the-subjects-are-read-at-once-in-each-phase
  (let [met (atom [])
        {:keys [result]} (run three-subjects "batch" cluster :meet (meeting 3 met))]
    (is (true? result))
    (is (= [[:derive true] [:derive true] [:derive true] [:judge true] [:judge true] [:judge true]]
           (sort-by (comp str first) @met))
        "three reads were in flight together before the confirmation, and three after it")))

(deftest a-dry-run-reads-the-subjects-at-once
  (let [met (atom [])
        {:keys [result]} (run three-subjects "batch" cluster :meet (meeting 3 met) :dry-run? true)]
    (is (true? result))
    (is (every? second @met))
    (is (= 6 (count @met)))))

(deftest a-derive-issues-its-own-reads-at-once
  (let [met (atom [])
        read (fn [context type name] #(kubectl/get-namespaced-resource context "batch" type name))
        {:keys [result]} (run {:select {:mode :one :label "Namespace"}
                               :derive (fn [{:keys [context]}]
                                         (concurrent/values {:job (read context "cronjobs" "nightly")
                                                             :settings (read context "configmaps" "settings")
                                                             :creds (read context "secrets" "creds")}))}
                              "batch" cluster :meet (meeting 3 met))]
    (is (true? result))
    (is (= [[:derive true] [:derive true] [:derive true]] @met)
        "each read ran in the :derive phase, inside its future, while the other two did")))

(deftest a-subject-that-did-not-answer-after-the-confirmation-still-refuses
  (let [{:keys [result out reads]} (run (dissoc reading-spec :refusal :recheck) "batch/nightly" cluster
                                        :confirming #(assoc % creds-key :unanswered))]
    (is (false? result))
    (is (str/includes? out "refusing: could not read what the precondition rests on - kubectl get did not answer"))
    (is (= 2 (get (fetches reads :judge) creds-key))
        "the prefetch did not keep the failed read, so the drift check asked again and refused on its answer")))

;; --- pure parts ------------------------------------------------------------------------

(deftest judged-refs-names-each-object-the-judge-reads-once
  (let [subject {:type "cronjobs" :namespace "batch" :name "nightly"}
        pinned (assoc subject :facts :suspend)
        settings {:type "configmaps" :namespace "batch" :name "settings"}]
    (is (= [subject settings] (mutation/judged-refs {:refusal identity} {:ref subject :refs [settings]} pinned))
        "the pin of the subject is the subject")
    (is (= [pinned settings] (mutation/judged-refs {} {:ref subject :refs [settings]} pinned))
        "without a refusal the subject is read only by its pin")
    (is (= [settings] (mutation/judged-refs {} {:refs [settings (assoc settings :optional? true)]} nil)))
    (is (= [] (mutation/judged-refs {} {} nil)))))
