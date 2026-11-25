;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-reads-values-test
  "The :reads and :values of a mutation spec: a value computed once per run,
   a read read before the confirmation and again after it for the refusal,
   the plan keeping what the confirmation showed, a post-check reading a
   read again, and the :derive and :recheck aliases running as their
   canonical entries do. A run goes through the real flow/change! against a
   recorded cluster whose kubectl records every get with its phase."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]
            [gumshoe.verify :as verify]
            [gumshoe.velero.action-books :as velero-books]
            [gumshoe.velero.actions :as velero-actions])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

;; --- the recorded cluster -------------------------------------------------------------

(defn- arg [argv prefix]
  (some #(when (and (string? %) (str/starts-with? % prefix)) (subs % (count prefix))) argv))

(defn- key-of
  "[type namespace name] of a kubectl get, name nil for a list."
  [argv]
  (let [[_ type name] (drop-while #(not= "get" %) argv)]
    [type (or (arg argv "--namespace=") (arg argv "-n=")) (when-not (and name (str/starts-with? name "-")) name)]))

(defn- answer
  [objects k]
  (let [o (get objects k)]
    (cond
      (= :unanswered o) {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"}
      o {:exit 0 :out (json/generate-string o) :err ""}
      :else {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" (first k) (last k))})))

(defn- run
  "Runs spec on target through mutation/run-on! and the real flow/change!
   against a cluster holding objects ({[type namespace name] object}). The
   operator confirms; `confirming` (fn [objects] -> objects) edits the
   cluster while the confirmation is shown. A post-check is tried once.
   Returns what the run returned
   and printed, every get as {:object :phase} and the plan the effect ran."
  [spec target objects & {:keys [confirming dry-run? meet]}]
  (let [cluster (atom objects)
        reads (atom [])
        ran (atom nil)
        eventually verify/eventually]
    (with-redefs [verify/eventually (fn [check] (eventually (assoc check :timeout 0)))
                  kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  shell/execute (fn [& argv]
                                  (let [k (key-of argv)]
                                    (swap! reads conj {:object k :phase mutation/*phase*})
                                    (when meet (meet k))
                                    (answer @cluster k)))
                  interact/confirm! (fn [_]
                                      (when confirming (swap! cluster confirming))
                                      true)
                  effect/run! (fn [plan] (reset! ran plan) true)]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out flow/*dry-run* (boolean dry-run?)]
                     (mutation/run-on! (merge {:select {:mode :one :label "Namespace"}
                                               :confirm {:action "stamp hedgedoc" :destructive? false}
                                               :effect (fn [_] (effect/plan (effect/note "go")))}
                                              spec)
                                       target {} nil))]
        {:result result :out (str out) :reads @reads :plan @ran}))))

(defn- configmap
  [name mode]
  {:metadata {:namespace "hedgedoc" :name name :uid (str name "-uid") :generation 1}
   :data {:mode mode}})

(def ^:private settings-key ["configmaps" "hedgedoc" "settings"])

(defn- mode-of
  [context]
  (get-in (kubectl/get-namespaced-resource context "hedgedoc" "configmaps" "settings") [:data :mode]))

(def ^:private flips-to-off
  "The settings say on until the operator confirms, and off from then on."
  #(assoc % settings-key (configmap "settings" "off")))

(def ^:private on-cluster {settings-key (configmap "settings" "on")})

(defn- counting
  "[calls f], f returning the next of a counter each call, recording the
   phase it ran in."
  []
  (let [calls (atom [])]
    [calls (fn [_] (swap! calls conj mutation/*phase*) (str "token-" (count @calls)))]))

;; --- a value once, a read again ---------------------------------------------------------

(deftest a-value-is-computed-once-and-a-read-is-read-again-after-the-confirmation
  (let [[value-calls token] (counting)
        seen (atom {})
        record (fn [k] (fn [ctx] (swap! seen assoc k (select-keys ctx [:mode :token]))))
        {:keys [result reads plan]}
        (run {:reads {:mode (fn [{:keys [context]}] (mode-of context))}
              :values {:token token}
              :refusal (fn [ctx] ((record :refusal) ctx) nil)
              :items (fn [ctx] ((record :items) ctx) ["stamp"])
              :effect (fn [{:keys [token] :as ctx}] ((record :effect) ctx) (effect/plan (effect/note token)))
              :verify (fn [ctx] ((record :verify) ctx) [])}
             "hedgedoc" on-cluster :confirming flips-to-off)]
    (is (true? result))
    (testing "the value is computed once, before the confirmation"
      (is (= [:derive] @value-calls)))
    (testing "the read runs before the confirmation and again after it"
      (is (= [[settings-key :derive] [settings-key :judge]]
             (map (juxt :object :phase) (filter #(= settings-key (:object %)) reads)))))
    (testing "the refusal judges the read made after the confirmation, beside the one value"
      (is (= {:mode "off" :token "token-1"} (:refusal @seen))))
    (testing ":items, :effect and :verify keep what the confirmation showed"
      (is (= {:mode "on" :token "token-1"} (:items @seen)))
      (is (= {:mode "on" :token "token-1"} (:effect @seen)))
      (is (= {:mode "on" :token "token-1"} (:verify @seen))))
    (is (= [[:note "token-1"]] plan) "the effect stamps the value computed before the confirmation")))

(deftest a-refusal-over-a-read-refuses-a-state-that-moved-during-the-confirmation
  (let [{:keys [result out plan]}
        (run {:reads {:mode (fn [{:keys [context]}] (mode-of context))}
              :refusal (fn [{:keys [mode]}] (when (= "off" mode) "the settings are off"))}
             "hedgedoc" on-cluster :confirming flips-to-off)]
    (is (false? result))
    (is (nil? plan) "nothing ran")
    (is (str/includes? out "refusing: the settings are off"))))

(deftest a-refusal-over-a-read-refuses-before-the-confirmation-too
  (let [asked (atom false)
        {:keys [result out]}
        (with-redefs [interact/confirm! (fn [_] (reset! asked true) true)]
          (run {:reads {:mode (fn [{:keys [context]}] (mode-of context))}
                :refusal (fn [{:keys [mode]}] (when (= "off" mode) "the settings are off"))}
               "hedgedoc" {settings-key (configmap "settings" "off")}))]
    (is (false? result))
    (is (= "refusing: the settings are off\n" out))
    (is (false? @asked) "the operator was never asked")))

(deftest without-a-refusal-a-read-is-not-read-again
  (let [{:keys [result reads]}
        (run {:reads {:mode (fn [{:keys [context]}] (mode-of context))}}
             "hedgedoc" on-cluster :confirming flips-to-off)]
    (is (true? result))
    (is (= [:derive] (map :phase reads)) "nothing judges a read after the confirmation, so nothing reads it")))

(deftest a-rehearsal-computes-the-value-once-and-reads-twice
  (let [[value-calls token] (counting)
        {:keys [result reads]}
        (run {:reads {:mode (fn [{:keys [context]}] (mode-of context))}
              :values {:token token}
              :refusal (constantly nil)}
             "hedgedoc" on-cluster :dry-run? true)]
    (is (true? result))
    (is (= [:derive] @value-calls))
    (is (= [:derive :judge] (map :phase reads)))))

(deftest a-read-that-does-not-answer
  (testing "before the confirmation stops the change before anything is asked"
    (let [{:keys [result out reads]}
          (run {:reads {:mode (fn [{:keys [context]}] (mode-of context))}
                :refusal (constantly nil)}
               "hedgedoc" {settings-key :unanswered})]
      (is (false? result))
      (is (str/includes? out "could not read what the change rests on"))
      (is (= [:derive] (map :phase reads)))))
  (testing "after the confirmation refuses with the reason, and no judgement runs"
    (let [judged (atom [])
          {:keys [result out]}
          (run {:reads {:mode (fn [{:keys [context]}] (mode-of context))}
                :refusal (fn [_] (swap! judged conj mutation/*phase*) nil)}
               "hedgedoc" on-cluster :confirming #(assoc % settings-key :unanswered))]
      (is (false? result))
      (is (= [:derive] @judged))
      (is (str/includes? out "refusing: could not read what the precondition rests on - ")))))

(deftest a-read-that-finds-nothing-is-judged
  (let [{:keys [result out]}
        (run {:reads {:mode (fn [{:keys [context]}] (mode-of context))}
              :refusal (fn [{:keys [mode]}] (when-not mode "the settings are gone"))}
             "hedgedoc" on-cluster :confirming #(dissoc % settings-key))]
    (is (false? result))
    (is (str/includes? out "refusing: the settings are gone"))))

(deftest reads-are-read-at-once
  (let [latch (CountDownLatch. 2)
        meet (fn [_] (.countDown latch) (.await latch 5 TimeUnit/SECONDS))
        read-of (fn [name] (fn [{:keys [context]}]
                             (kubectl/get-namespaced-resource context "hedgedoc" "configmaps" name)))
        {:keys [result reads]}
        (run {:reads {:a (read-of "a") :b (read-of "b")}}
             "hedgedoc" {["configmaps" "hedgedoc" "a"] (configmap "a" "x")
                         ["configmaps" "hedgedoc" "b"] (configmap "b" "y")}
             :meet meet)]
    (is (true? result) "both reads met inside the latch, so they ran at the same time")
    (is (zero? (.getCount latch)))
    (is (= #{:derive} (set (map :phase reads))))))

(deftest a-value-sees-the-reads-and-not-another-value
  (let [seen (atom {})
        {:keys [result]}
        (run {:reads {:mode (fn [{:keys [context]}] (mode-of context))}
              :values {:first (fn [ctx] (swap! seen assoc :first (select-keys ctx [:mode :second])) 1)
                       :second (fn [ctx] (swap! seen assoc :second (select-keys ctx [:mode :first])) 2)}}
             "hedgedoc" on-cluster)]
    (is (true? result))
    (is (= {:first {:mode "on"} :second {:mode "on"}} @seen))))

(deftest a-value-that-asks-the-operator-asks-once
  (let [asked (atom 0)
        {:keys [result plan]}
        (run {:values {:stage (fn [_] (swap! asked inc) "canary")}
              :refusal (fn [{:keys [stage]}] (when-not stage "no stage was chosen"))
              :effect (fn [{:keys [stage]}] (effect/plan (effect/note stage)))}
             "hedgedoc" on-cluster)]
    (is (true? result))
    (is (= 1 @asked) "judged before and after the confirmation, chosen once")
    (is (= [[:note "canary"]] plan))))

;; --- a post-check reads a read again -------------------------------------------------

(deftest read-again-reads-the-named-read-now
  (let [checked (atom nil)
        {:keys [result]}
        (run {:reads {:mode (fn [{:keys [context]}] (mode-of context))}
              :verify (fn [ctx]
                        [{:description "the settings are off"
                          :check #(reset! checked [(:mode ctx) (mutation/read-again ctx :mode)])}])}
             "hedgedoc" on-cluster
             :confirming flips-to-off)]
    (is (some? result))
    (is (= ["on" "off"] @checked) "the confirmed value stays in the context, read-again sees the cluster now")))

(deftest read-again-of-a-name-the-change-does-not-read-throws
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"the change reads nothing named :mood"
                        (mutation/read-again {::mutation/reads {:mode (constantly 1)}} :mood)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reads nothing named :mode"
                        (mutation/read-again {} :mode))))

;; --- the aliases are their canonical entries -------------------------------------------

(deftest the-aliases-run-as-their-canonical-entries-do
  (let [derive-fn (fn [{:keys [context]}] {:mode (mode-of context)})
        recheck-fn (fn [{:keys [context mode]}] {:mode (mode-of context) :confirmed mode})
        refusal (fn [{:keys [mode confirmed]}] (when (not= mode confirmed) (str "moved from " confirmed " to " mode)))]
    (doseq [[label alias canonical] [[":derive" {:derive derive-fn}
                                      {:values {change/derive-entry derive-fn}}]
                                     [":derive and :recheck" {:derive derive-fn :recheck recheck-fn :refusal refusal}
                                      {:values {change/derive-entry derive-fn}
                                       :reads {change/recheck-entry recheck-fn}
                                       :refusal refusal}]]]
      (testing label
        (let [a (run alias "hedgedoc" on-cluster :confirming flips-to-off)
              c (run canonical "hedgedoc" on-cluster :confirming flips-to-off)]
          (is (= a c)))))
    (is (str/starts-with? (:out (run {:derive derive-fn :recheck recheck-fn :refusal refusal} "hedgedoc" on-cluster :confirming flips-to-off))
                          "refusing: moved from on to off\n")
        "the alias pair still compares the confirmed value with the current one")))

;; --- a migrated book: velero backup ----------------------------------------------------

(def ^:private schedule-key ["schedules.velero.io" "velero" "daily"])
(def ^:private locations-key ["backupstoragelocations.velero.io" "velero" nil])

(defn- schedule []
  {:metadata {:namespace "velero" :name "daily" :uid "daily-uid" :generation 1}
   :spec {:schedule "0 1 * * *" :template {:includedNamespaces ["shop"] :ttl "720h0m0s"}}
   :status {:phase "Enabled"}})

(defn- locations [phase]
  {:items [{:metadata {:namespace "velero" :name "default"} :spec {:default true} :status {:phase phase}}]})

(defn- run-velero
  [confirming]
  (let [names (atom 0)]
    (with-redefs [velero-actions/backup-name (let [real velero-actions/backup-name]
                                               (fn [s now] (swap! names inc) (real s now)))]
      (assoc (run velero-books/backup "velero/daily"
                  {schedule-key (schedule) locations-key (locations "Available")}
                  :confirming confirming)
             :names @names))))

(deftest velero-backup-names-its-backup-once-and-reads-its-locations-again
  (testing "a location that stays available: one name, the locations read on both sides"
    (let [{:keys [out reads plan names]} (run-velero nil)]
      (is (not (str/includes? out "refusing")) "the backup ran; its post-check, tried once, waits for a Backup nobody completes")
      (is (= 1 names) "the backup name is a value, minted once per run")
      (is (= [:derive :judge] (map :phase (filter #(= locations-key (:object %)) reads))))
      (is (= 1 (count plan)))
      (is (str/includes? (pr-str plan) "daily-") "the plan creates the backup under the minted name")))
  (testing "a location that became unavailable during the confirmation refuses the backup"
    (let [{:keys [result out plan names]} (run-velero #(assoc % locations-key (locations "Unavailable")))]
      (is (false? result))
      (is (nil? plan))
      (is (= 1 names))
      (is (str/includes? out "refusing: BackupStorageLocation default is Unavailable, so the backup cannot be written")))))
