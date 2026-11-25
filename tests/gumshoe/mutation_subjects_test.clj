;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-subjects-test
  "The :subjects key of a mutation spec, a change acting on several objects,
   against a stubbed kubectl."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]))

(def ^:private claim {:type "persistentvolumeclaims" :namespace "moodle" :name "moodle-data"})
(def ^:private database {:type "databases.kinda.rocks" :namespace "moodle" :name "moodle"})
(def ^:private route {:type "httproutes" :namespace "moodle" :name "moodle" :optional? true})

(defn- object
  [{:keys [name]} & {:keys [generation uid seen] :or {generation 1}}]
  {:metadata (cond-> {:namespace "moodle" :name name :uid (or uid (str name "-uid")) :generation generation}
               seen (assoc :annotations {:seen seen}))})

(defn- unread!
  []
  (throw (ex-info "kubectl get did not answer: i/o timeout" {::kubectl/unread true})))

(defn- ref-of
  "The ref a kubectl get of one namespaced object reads."
  [argv]
  (let [[_ type name] (drop-while #(not= "get" %) argv)]
    {:type type
     :namespace (some #(when (str/starts-with? % "--namespace=") (subs % (count "--namespace="))) argv)
     :name name}))

(defn- answer
  "What kubectl prints for the object (read ref phase) returns: the object,
   NotFound for nil, and a timeout for a read that throws unread."
  [read {:keys [type name] :as ref} phase]
  (try
    (if-let [object (read ref phase)]
      {:exit 0 :out (json/generate-string object) :err ""}
      {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" type name)})
    (catch clojure.lang.ExceptionInfo e
      (if (::kubectl/unread (ex-data e))
        {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"}
        (throw e)))))

(defn- run
  "Runs spec on the namespace moodle with every kubectl get answered by
   (read ref phase), where phase is :before until flow/change! is called
   and :after from then on, and flow/change! recorded. Returns what the run
   printed and returned, the config change! received, and every kubectl
   read it ran, each with its phase and whether it ran under strict reads."
  [spec read & {:keys [target] :or {target "moodle"}}]
  (let [reads (atom []) seen (atom nil) phase (atom :before)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  shell/execute (fn [& argv]
                                  (let [ref (ref-of argv) at @phase]
                                    (swap! reads conj [ref at kubectl/*strict-reads*])
                                    (answer read ref at)))
                  flow/change! (fn [config]
                                 (reset! seen config)
                                 (reset! phase :after)
                                 (if-let [p (:precondition config)] (p) true))]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out]
                     (mutation/run-on! (merge {:select {:mode :one :label "Namespace"}
                                               :confirm {:action "adopt moodle"}
                                               :effect (fn [_] (effect/plan (effect/note "go")))}
                                              spec)
                                       target {} nil))]
        {:result result :out (str out) :config @seen :reads @reads}))))

(defn- adoptees
  [& refs]
  (fn [{:keys [target]}]
    (map #(assoc % :namespace target) (map #(dissoc % :namespace) refs))))

(deftest every-subject-is-read-strictly-and-handed-to-every-callback
  (let [seen (atom [])
        record (fn [k] (fn [ctx] (swap! seen conj [k (select-keys ctx [:refs :objects])]) nil))
        {:keys [result reads]} (run {:subjects (adoptees claim database)
                                     :derive (fn [ctx] ((record :derive) ctx) {})
                                     :items (fn [ctx] ((record :items) ctx) ["x"])
                                     :refusal (record :refusal)
                                     :precondition (fn [ctx] ((record :precondition) ctx) true)
                                     :effect (fn [ctx] ((record :effect) ctx) (effect/plan (effect/note "go")))}
                                    (fn [ref phase] (object ref :seen (str (:name ref) "-" (name phase)))))
        by (into {} @seen)]
    (is (true? result) "an annotation leaves the fingerprint alone")
    (is (= #{claim database} (set (map first (filter #(= :before (second %)) reads))))
        "every subject is read before :derive")
    (is (= 4 (count reads)) "each subject once in each phase")
    (is (every? #(nth % 2) reads) "every read of a subject is strict")
    (testing ":derive, :items and :effect see the objects read before the confirmation"
      (is (= [claim database] (get-in by [:derive :refs])))
      (is (= ["moodle-data-before" "moodle-before"] (map #(get-in % [:metadata :annotations :seen]) (get-in by [:derive :objects]))))
      (is (= ["moodle-data-before" "moodle-before"] (map #(get-in % [:metadata :annotations :seen]) (get-in by [:items :objects])))))
    (testing ":refusal and :precondition see every object read again after it"
      (is (= ["moodle-data-after" "moodle-after"] (map #(get-in % [:metadata :annotations :seen]) (get-in by [:refusal :objects]))))
      (is (= ["moodle-data-after" "moodle-after"] (map #(get-in % [:metadata :annotations :seen]) (get-in by [:precondition :objects])))))))

(deftest a-second-subject-edited-after-the-confirmation-is-refused-by-name
  (let [{:keys [result out]} (run {:subjects (adoptees claim database)}
                                  (fn [ref phase]
                                    (object ref :generation (if (and (= "moodle" (:name ref)) (= :after phase)) 2 1))))]
    (is (false? result))
    (is (= "refusing: databases.kinda.rocks moodle/moodle: its spec changed after the confirmation (generation 1, now 2)\n"
           out))))

(deftest a-subject-replaced-after-the-confirmation-is-refused-by-name
  (let [{:keys [result out]} (run {:subjects (adoptees claim database)}
                                  (fn [ref phase]
                                    (object ref :uid (when (and (= "moodle-data" (:name ref)) (= :after phase)) "other"))))]
    (is (false? result))
    (is (re-find #"refusing: persistentvolumeclaims moodle/moodle-data: it was deleted and created again" out))))

(deftest unchanged-subjects-let-the-change-run
  (let [{:keys [result reads]} (run {:subjects (adoptees claim database)} (fn [ref _] (object ref)))]
    (is (true? result))
    (is (= 4 (count reads)) "each subject once before the confirmation and once for the drift check")
    (is (= {:before 2 :after 2} (frequencies (map second reads))))))

(deftest facts-of-a-subject-join-its-fingerprint
  (let [{:keys [result out]} (run {:subjects (fn [_] [(assoc database :facts #(get-in % [:spec :secretName]))])}
                                  (fn [ref phase] (assoc (object ref) :spec {:secretName (if (= :after phase) "new" "old")})))]
    (is (false? result))
    (is (= "refusing: databases.kinda.rocks moodle/moodle: what the change rests on moved after the confirmation (\"old\", now \"new\")\n"
           out))))

(deftest a-missing-subject-is-refused-unless-optional
  (testing "a required subject that did not exist when the change was confirmed refuses by name"
    (let [{:keys [result out]} (run {:subjects (adoptees claim database)}
                                    (fn [ref _] (when-not (= "moodle-data" (:name ref)) (object ref))))]
      (is (false? result))
      (is (= "refusing: persistentvolumeclaims moodle/moodle-data: it did not exist when the change was confirmed\n" out))))
  (testing "an optional subject may be absent and is handed over as nil"
    (let [objects (atom nil)
          {:keys [result]} (run {:subjects (adoptees claim route)
                                 :derive (fn [ctx] (reset! objects (:objects ctx)) {})}
                                (fn [ref _] (when-not (= "httproutes" (:type ref)) (object ref))))]
      (is (true? result))
      (is (= [true false] (map some? @objects)))))
  (testing "an optional subject created after the confirmation refuses by name"
    (let [{:keys [result out]} (run {:subjects (adoptees claim route)}
                                    (fn [ref phase] (when (or (= :after phase) (not= "httproutes" (:type ref))) (object ref))))]
      (is (false? result))
      (is (= "refusing: httproutes moodle/moodle: it was created after the confirmation\n" out)))))

(deftest an-unread-subject-stops-before-the-confirmation
  (let [{:keys [result out config]} (run {:subjects (adoptees claim database)}
                                         (fn [ref _] (if (= "moodle" (:name ref)) (unread!) (object ref))))]
    (is (false? result))
    (is (nil? config) "no confirmation is shown")
    (is (re-find #"could not read what the change rests on - kubectl get did not answer" out))))

(deftest an-unread-subject-after-the-confirmation-refuses
  (let [refused (atom false)
        {:keys [result out]} (run {:subjects (adoptees claim database)
                                   :refusal (fn [_] (reset! refused true) nil)}
                                  (fn [ref phase] (if (= :after phase) (unread!) (object ref))))]
    (is (false? result))
    (is (false? @refused) "the refusal never judges a read that did not answer")
    (is (re-find #"refusing: could not read what the precondition rests on - kubectl get did not answer" out))))

(deftest subjects-compose-with-a-subject-and-a-pin
  (testing "a :subject keeps its :ref and :object next to the :refs and :objects"
    (let [seen (atom nil)
          {:keys [result]} (run {:select {:mode :namespaced :label "Database"}
                                 :subject {:type "databases.kinda.rocks"}
                                 :subjects (fn [{:keys [ref]}] [(assoc claim :namespace (:namespace ref))])
                                 :derive (fn [ctx] (reset! seen (select-keys ctx [:ref :refs])) {})}
                                (fn [ref _] (object ref))
                                :target "moodle/moodle")]
      (is (true? result))
      (is (= {:ref {:type "databases.kinda.rocks" :namespace "moodle" :name "moodle"} :refs [claim]} @seen))))
  (testing "a :pin is still checked alongside every subject"
    (let [settings {:type "configmaps" :namespace "moodle" :name "settings"}
          {:keys [result out]} (run {:subjects (adoptees claim) :pin (constantly settings)}
                                    (fn [ref phase] (object ref :generation (if (and (= "settings" (:name ref)) (= :after phase)) 2 1))))]
      (is (false? result))
      (is (re-find #"refusing: its spec changed after the confirmation \(generation 1, now 2\)" out)))))

(deftest a-many-select-may-declare-subjects
  (let [{:keys [result reads]} (run {:select {:mode :many :label "Claim"}
                                     :subjects (fn [{:keys [target]}]
                                                 (map #(assoc claim :name %) target))}
                                    (fn [ref _] (object ref))
                                    :target ["moodle-data" "moodle-files"])]
    (is (true? result))
    (is (= #{"moodle-data" "moodle-files"} (set (map (comp :name first) (take 2 reads)))) "the target of a :many select is its names")))

(deftest a-subject-without-a-type-or-name-is-a-spec-error
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":subjects ref needs a :type and a :name"
                        (run {:subjects (constantly [{:type "configmaps"}])} (fn [_ _] nil)))))

(deftest subjects-and-connect-do-not-mix
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"takes neither"
                        (run {:subjects (adoptees claim) :connect (constantly {:context "x" :cluster "x"})}
                             (fn [_ _] nil)))))

(deftest subjects-drift-names-the-first-object-that-moved
  (is (nil? (mutation/subjects-drift [claim database]
                                     [{:uid "a" :generation 1} {:uid "b" :generation 1}]
                                     [{:uid "a" :generation 1} {:uid "b" :generation 1}])))
  (is (= "databases.kinda.rocks moodle/moodle: it was deleted after the confirmation"
         (mutation/subjects-drift [claim database]
                                  [{:uid "a" :generation 1} {:uid "b" :generation 1}]
                                  [{:uid "a" :generation 1} nil])))
  (is (= "nodes worker-1: it did not exist when the change was confirmed"
         (mutation/subjects-drift [{:type "nodes" :name "worker-1"}] [nil] [nil]))))

(deftest help-mentions-the-check-for-subjects
  (is (some #{"  checks again right before acting that what was confirmed still holds"}
            (mutation/details {:select {:mode :one :label "Namespace"} :confirm {:action "x"} :subjects (constantly [])}))))
