;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dsl-procedure-test
  "gumshoe.dsl's procedures, data declaring :kind :procedure: what
   compile-procedure refuses, what it compiles to, and whole runs through
   gumshoe.playbook/run-on! against a fake cluster that applies patches and
   deletes, shaped like an adoption: a stamp of the objects a chart adopts,
   a registered effect writing the release file, and a purge of the
   leftovers, under one confirmation."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.change :as change]
            [gumshoe.conformance :as conformance]
            [gumshoe.deletes :as deletes]
            [gumshoe.dsl :as dsl]
            [gumshoe.dsl.registry :as registry]
            [gumshoe.edn-book :as edn-book]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.offline :as offline]
            [gumshoe.outcome :as outcome]
            [gumshoe.playbook :as playbook]
            [gumshoe.reads :as reads]
            [gumshoe.reads.result :as result]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.testing.plan :as plan]
            [gumshoe.testing.refusal :as refusal]
            [gumshoe.verify :as verify]))

;; --- registered functions and effect types -----------------------------------------
;;
;; Other tests reset gumshoe.effect's registry wholesale, so every test that
;; needs the effect type registers it itself and restores the registry after.

(defn- file-says?
  [dir file text]
  (let [f (fs/file dir file)]
    (and (fs/exists? f) (= text (slurp f)))))

(def ^:private types
  {:dsl-procedure-test/write-file
   {:describe (fn [[dir file _]] (str "write " file " in " dir))
    :perform (fn [[dir file text]] (spit (fs/file dir file) text) true)
    :args ["dir" "file" "text"]
    :destructive? false
    :touches (fn [_] [])
    :doc "writes a file"}})

(defn- with-types
  [thunk]
  (let [saved @@#'effect/effect-types]
    (try
      (doseq [[op handlers] types] (effect/register-effect-type! op handlers))
      (thunk)
      (finally (reset! @#'effect/effect-types saved)))))

(registry/register! :gumshoe.dsl-procedure-test/namespaces
                    {:fn (fn [_context] ["shop"]) :kind :candidates :args ["context"]
                     :doc "the namespaces a test offers"})

(registry/register! :gumshoe.dsl-procedure-test/file-says?
                    {:fn file-says? :kind :predicate :args ["dir" "file" "text"]
                     :doc "whether a file in a test directory holds the text"})

;; --- the procedure -------------------------------------------------------------------

(def ^:private routes "httproutes.gateway.networking.k8s.io")

(def ^:private target
  {:pick :one :label "namespace" :type "namespaces" :flag :namespace
   :candidates [:call :gumshoe.dsl-procedure-test/namespaces [:context]]})

(def ^:private stamped?
  [:and
   [:field [:metadata :labels :app.kubernetes.io/managed-by] := "Helm"]
   [:field [:metadata :annotations :meta.helm.sh/release-name] := "moodle"]])

(def ^:private stamp-step
  {:title "stamp"
   :confirm {:action "stamp Helm ownership onto the adoptees" :destructive? false}
   :announce false
   :reads {:adoptees [:present [{:type "persistentvolumeclaims" :name "data"}
                                {:type routes :name "moodle"}]
                      {:namespace [:target]}]}
   :effect [[:each :adoptees
             {:state {:holds stamped? :says "{each.metadata.name} carries Helm's ownership"}}
             [:kubectl :patch {:merge {:metadata {:labels {"app.kubernetes.io/managed-by" "Helm"}
                                                  :annotations {"meta.helm.sh/release-name" "moodle"}}}}]]]})

(defn- write-step
  [dir]
  {:title "write"
   :confirm {:action "write the release file" :destructive? false}
   :announce false
   :values {:dir dir}
   :reads {:holds [:present [{:type "configmaps" :name "hold"}] {:namespace [:target]}]}
   :refuse [{:when [:some? [:read :holds 0]] :says "a hold ConfigMap stops the install"}]
   :state {:holds [:call :gumshoe.dsl-procedure-test/file-says? [:value :dir] "shop.txt" "release moodle"]
           :says "the release file is written"}
   :effect [[:effect :dsl-procedure-test/write-file [:value :dir] "shop.txt" "release moodle"]]})

(def ^:private purge-step
  {:title "purge"
   :confirm {:action "delete the ansible leftovers"}
   :announce false
   :recovery "a deleted leftover is gone; the ansible role recreates it"
   :reads {:leftovers [:present [{:type "services" :name "moodle-tideways"}
                                 {:type "configmaps" :name "mdl-settings"}]
                       {:namespace [:target]}]}
   :effect [[:each :leftovers [:kubectl :delete]]]})

(defn- procedure
  [dir]
  {:gumshoe/dsl :v0-experimental
   :kind :procedure
   :description "Adopts a namespace's objects into a Helm release"
   :target target
   :prerequisites {}
   :confirm {:action "adopt the namespace into Helm"}
   :recovery "the namespace {target} may hold stamped objects and no release file"
   :steps [stamp-step (write-step dir) purge-step]})

(defn- phased
  "The adoption in phases, with an unattended pause that waits for the
   release and a check that no hold remains before the purge."
  [dir]
  (-> (procedure dir)
      (dissoc :steps)
      (assoc :phases [{:phase :prepare :title "prepare" :steps [stamp-step (write-step dir)]}
                      {:phase :install :title "install"
                       :steps [[:pause {:says "merge the release into the GitOps repository"
                                        :unattended :continue
                                        :until [{:read [:get "configmaps" "release" {:namespace [:target]}]
                                                 :holds [:field [:data :state] := "installed"]
                                                 :says "the release in {target} is installed"
                                                 :timeout 900 :interval 15}]}]
                               [:check {:title "no hold remains"
                                        :reads {:hold [:present [{:type "configmaps" :name "hold"}] {:namespace [:target]}]}
                                        :checks [{:name "no-hold" :description "no hold ConfigMap stops the purge"
                                                  :severity :critical :expect [:absent :hold]
                                                  :says "a hold ConfigMap remains in {target}"}]}]]}
                      {:phase :cleanup :title "clean up" :steps [purge-step]}])))

(defn- standalone
  "A step as a change of its own."
  [step]
  (assoc step :gumshoe/dsl :v0-experimental :target target :prerequisites {} :description (:title step)))

;; --- the fake cluster ----------------------------------------------------------------

(defn- object
  [type namespace name & {:as extra}]
  (merge-with merge
              {:kind type
               :metadata (cond-> {:name name :uid (str "uid-" name) :resourceVersion "1"}
                           namespace (assoc :namespace namespace))}
              extra))

(defn- shop
  "The namespace with its adoptees and two leftovers. extra are more
   objects."
  [& extra]
  (into {}
        (map (fn [o] [[(:kind o) (get-in o [:metadata :namespace]) (get-in o [:metadata :name])] o]))
        (concat [(object "namespaces" nil "shop")
                 (object "persistentvolumeclaims" "shop" "data")
                 (object routes "shop" "moodle")
                 (object "services" "shop" "moodle-tideways")
                 (object "configmaps" "shop" "mdl-settings")]
                extra)))

(def ^:private hold (object "configmaps" "shop" "hold"))
(def ^:private installed (object "configmaps" "shop" "release" :data {:state "installed"}))

(defn- parse-argv
  [argv]
  (reduce (fn [acc arg]
            (if-let [[_ k v] (re-matches #"--([^=]+)(?:=(.*))?" arg)]
              (assoc-in acc [:flags k] (or v true))
              (update acc :words conj arg)))
          {:flags {} :words []}
          (rest argv)))

(defn- deep-merge
  [a b]
  (if (and (map? a) (map? b)) (merge-with deep-merge a b) b))

(defn- bump
  [o]
  (update-in o [:metadata :resourceVersion] #(str (inc (parse-long %)))))

(defn- respond!
  [state log argv]
  (let [{:keys [flags words]} (parse-argv argv)
        [verb type name] words
        ns (get flags "namespace")
        k [type ns name]
        {:keys [objects]} @state]
    (case verb
      "get" (cond
              name (if-let [o (get objects k)]
                     {:exit 0 :out (json/generate-string o) :err ""}
                     (if (get flags "ignore-not-found")
                       {:exit 0 :out "" :err ""}
                       (refusal/not-found type name)))
              :else {:exit 0
                     :out (json/generate-string {:items (vec (for [[[t n _] o] objects
                                                                    :when (and (= t type) (= n ns))]
                                                                o))})
                     :err ""})
      "patch" (do (swap! log conj [:patch type ns name])
                  (if (contains? objects k)
                    (do (swap! state update-in [:objects k]
                               #(bump (deep-merge % (json/parse-string (get flags "patch") true))))
                        {:exit 0 :out "patched" :err ""})
                    (refusal/not-found type name)))
      "delete" (do (swap! log conj [:delete type ns name])
                   (if (or (contains? objects k) (get flags "ignore-not-found"))
                     (do (swap! state update :objects dissoc k)
                         {:exit 0 :out "deleted" :err ""})
                     (refusal/not-found type name)))
      {:exit 1 :out "" :err (str "the fake cluster does not answer " (pr-str argv))})))

(defn- run-procedure
  "Runs spec, a compiled procedure or a hand-built playbook spec, against
   the namespace shop. scenario: :objects the cluster, :opts the parsed
   flags, :dry-run? a rehearsal, :answer what the operator answers. Returns
   the result, what was printed, the confirmations asked, the mutations,
   the objects left and the recovery guidance recorded."
  [spec {:keys [objects opts dry-run? answer] :or {opts {} answer true}}]
  (let [state (atom {:objects objects})
        log (atom [])
        confirmations (atom [])
        eventually verify/eventually
        out (java.io.StringWriter.)
        ;; the stub stands in for gumshoe.shell's launch, so it judges the
        ;; offline guard that launch judges
        shell! (fn [& argv] (offline/check! (vec argv)) (respond! state log (vec argv)))
        streamed (fn [& argv] (:exit (apply shell! argv)))]
    (with-redefs-fn
      {#'shell/execute shell!
       #'shell/run-attributed streamed
       #'shell/run-with-output streamed
       #'interact/confirm! (fn [request] (swap! confirmations conj request) answer)
       #'announce/announce! (constantly nil)
       #'playbook/attended? (constantly false)
       #'verify/eventually (fn [check] (eventually (assoc check :timeout 0)))}
      (fn []
        (binding [*out* out *err* out
                  outcome/*record* (outcome/fresh)
                  kube-context/*pinned* {:context "ctx" :cluster "cluster"}
                  flow/*dry-run* (boolean dry-run?)]
          (let [result (playbook/run-on! spec "shop" opts nil)]
            {:result result
             :printed (stdout/strip-colors (str out))
             :confirmations @confirmations
             :mutations @log
             :objects (:objects @state)
             :recovery (:text (:recovery (outcome/recorded-recovery)))}))))))

(defn- with-dir
  [f]
  (let [dir (str (fs/create-temp-dir {:prefix "dsl-procedure-test"}))]
    (try (with-types #(f dir))
         (finally (fs/delete-tree dir)))))

(defn- labelled?
  [objects k]
  (= "Helm" (get-in objects [k :metadata :labels :app.kubernetes.io/managed-by])))

;; --- what it compiles to -------------------------------------------------------------

(deftest a-procedure-compiles-to-a-playbook-spec-of-compiled-changes
  (with-dir
    (fn [dir]
      (let [p (dsl/compile-procedure (procedure dir))
            [stamp write purge] (:steps p)]
        (is (dsl/procedure? p))
        (is (= :change (playbook/kind stamp) (playbook/kind write) (playbook/kind purge)))
        (is (true? (:report-progress p)))
        (is (= {:type "namespaces"} (:subject p)) "the target's type is the playbook's subject")
        (is (true? (get-in p [:confirm :destructive?])) "a destructive step makes the one confirmation destructive")
        (is (= ["stamp" "write" "purge"] (mapv :title (:steps p))))
        (is (= ["patch each object of :adoptees (at most 25)"] (:outline stamp)))
        (is (= [":dsl-procedure-test/write-file"] (:outline write)))
        (is (= ["delete each object of :leftovers (at most 25), destructive"] (:outline purge)))
        (is (true? (get-in purge [:confirm :destructive?])))
        (is (change/destructive-effect? (:effect purge)))
        (doseq [step (:steps p)]
          (is (= step (change/normalize step)) "every step is already canonical")
          (is (= [] (conformance/contract-problems step)) (:title step))
          (is (= [] (conformance/guard-problems step)) (:title step)))
        (is (= {:one-confirmation :on :destructive-confirmation :on :each-step-judged-when-reached :on
                :progress-report :on :shared-data :none}
               (dissoc (dsl/safeguards-of p) :steps)))
        (is (= [[:steps 0] [:steps 1] [:steps 2]] (mapv :path (:steps (dsl/safeguards-of p)))))
        (is (= (procedure dir) (dsl/source-of p)))
        (is (nil? (dsl/phases-of p)))))))

(deftest phases-name-their-steps-and-title-them
  (with-dir
    (fn [dir]
      (let [p (dsl/compile-procedure (phased dir))]
        (is (= [{:phase :prepare :title "prepare" :steps [0 1]}
                {:phase :install :title "install" :steps [2 3]}
                {:phase :cleanup :title "clean up" :steps [4]}]
               (dsl/phases-of p)))
        (is (= ["prepare: stamp" "prepare: write" "install: merge the release into the GitOps repository"
                "install: no hold remains" "clean up: purge"]
               (mapv :title (:steps p))))
        (is (= [:change :change :pause :check :change] (mapv playbook/kind (:steps p))))
        (is (= :continue (:unattended (nth (:steps p) 2))))))))

;; --- the snapshot key of an :each ----------------------------------------------------

(deftest the-snapshots-of-two-steps-acting-through-each-at-index-0-compose
  (with-dir
    (fn [dir]
      (let [alone-stamp (dsl/compile (standalone stamp-step))
            alone-purge (dsl/compile (standalone purge-step))
            p (dsl/compile-procedure (procedure dir))
            [stamp _ purge] (:steps p)]
        (testing "a change compiled alone keeps its name, which two such changes share"
          (is (contains? (:values alone-stamp) :gumshoe.dsl/each-0))
          (is (contains? (:values alone-purge) :gumshoe.dsl/each-0))
          (is (thrown? clojure.lang.ExceptionInfo
                       (change/compose {:values (:values alone-stamp)} {:values (:values alone-purge)}))))
        (testing "a step's snapshot is named for the step, so the two compose"
          (is (= #{:gumshoe.dsl/step-0-each-0} (set (keys (:values stamp)))))
          (is (= #{:gumshoe.dsl/step-2-each-0} (set (keys (:values purge)))))
          (is (= #{:gumshoe.dsl/step-0-each-0 :gumshoe.dsl/step-2-each-0}
                 (set (keys (:values (change/compose {:values (:values stamp)} {:values (:values purge)}))))))))
      (testing "control: a snapshot key that ignores the step clashes again"
        (with-redefs-fn {#'dsl/snapshot-key (fn [_ i] (keyword "gumshoe.dsl" (str "each-" i)))}
          (fn []
            (let [[stamp _ purge] (:steps (dsl/compile-procedure (procedure dir)))]
              (is (thrown? clojure.lang.ExceptionInfo
                           (change/compose {:values (:values stamp)} {:values (:values purge)}))))))))))

;; --- what compile-procedure refuses --------------------------------------------------

(defn- refusal-of
  [data]
  (try (dsl/compile-procedure data) nil
       (catch clojure.lang.ExceptionInfo e
         (when (dsl/invalid? e) (select-keys (ex-data e) [:path :reason])))))

(deftest compile-procedure-refuses-what-it-cannot-run-safely
  (with-dir
    (fn [dir]
      (let [base (procedure dir)
            cases [[(assoc base :kind :detective) [:kind] ":procedure"]
                   [(dissoc base :gumshoe/dsl) [:gumshoe/dsl] "declares no version"]
                   [(assoc base :reads {:x [:list "pods" {:namespace [:target]}]}) [:reads] "not a key of a procedure"]
                   [(assoc base :phases []) [:phases] "not both"]
                   [(assoc base :steps []) [:steps] "one or more steps"]
                   [(dissoc base :steps) [:steps] "declares its :steps"]
                   [(assoc-in base [:steps 1 :target] target) [:steps 1 :target] "procedure's target"]
                   [(assoc-in base [:steps 1 :gumshoe/dsl] :v0-experimental) [:steps 1 :gumshoe/dsl] "declares the version"]
                   [(assoc-in base [:steps 1 :kind] :detective) [:steps 1 :kind] "declares no :kind"]
                   [(assoc-in base [:steps 1] [:wait 5]) [:steps 1] "a step is a change map"]
                   [(assoc-in base [:steps 1] "write") [:steps 1] "a step is a change map"]
                   [(assoc base :steps [[:pause {:says "look"}]]) [:steps] "only reads is a detective"]
                   [(assoc base :confirm {:action "adopt" :destructive? false}) [:confirm :destructive?] "[:steps 2] is destructive"]
                   [(assoc base :confirm {:action "adopt" :separately? true}) [:confirm :separately?] "not a key"]
                   [(assoc-in base [:target :candidates] {:unless :state}) [:target :candidates] "no :state of its own"]
                   [(assoc base :target (dissoc target :type)) [:target] "neither the :type"]
                   [(assoc-in base [:steps 1 :refuse] [{:when [:some? [:read :adoptees]] :says "x"}])
                    [:steps 1 :refuse 0 :when 1 1] "names no declared :reads entry"]
                   [(assoc-in base [:steps 2 :effect 0 1] :holds) [:steps 2 :effect 0 1] "names no declared :reads entry"]
                   [(update-in base [:steps 2] dissoc :recovery) [:steps 2 :recovery] "destructive"]
                   [(assoc-in base [:steps 0 :options] {:namespace {:desc "other" :coerce :long}}) [:options] "--namespace"]
                   [(assoc base :recovery "{each.metadata.name}") [:recovery] "{each.metadata.name}"]
                   [(assoc-in base [:steps 1] [:pause]) [:steps 1] "a pause is"]
                   [(assoc-in base [:steps 1] [:pause {:says "act" :wait 3}]) [:steps 1 1 :wait] "not a key of a pause"]
                   [(assoc-in base [:steps 1] [:pause {:says "act on {target}"}]) [:steps 1 1 :says] "no placeholder"]
                   [(assoc-in base [:steps 1] [:pause {:says "act" :unattended :continue}]) [:steps 1 1 :unattended] "declare :until"]
                   [(assoc-in base [:steps 1] [:pause {:says "act" :unattended :abort :until []}]) [:steps 1 1 :unattended] ":continue"]
                   [(assoc-in base [:steps 1] [:pause {:says "act" :until []}]) [:steps 1 1 :until] "one or more"]
                   [(assoc-in base [:steps 1] [:pause {:says "act" :until [{:read [:list "configmaps" {:namespace [:target]}]
                                                                             :holds [:some? [:each]] :says "x"}]}])
                    [:steps 1 1 :until 0 :read] "one object"]
                   [(assoc-in base [:steps 1] [:pause {:says "act" :until [{:read [:get "configmaps" "x" {:namespace [:target]}]
                                                                             :holds [:some? [:each]] :says "x" :timeout 7200}]}])
                    [:steps 1 1 :until 0 :timeout] "at most"]
                   [(assoc-in base [:steps 1] [:pause {:says "act" :until [{:read [:get "configmaps" "x" {:namespace [:target]}]
                                                                             :holds [:some? [:each]] :says "x"}
                                                                            {:read [:get "configmaps" "y" {:namespace [:target]}]
                                                                             :holds [:some? [:each]] :says "x"}]}])
                    [:steps 1 1 :until 1 :says] "earlier check"]
                   [(assoc-in base [:steps 1] [:check {:reads {:x [:list "pods" {:namespace [:target]}]}
                                                       :checks [{:name "a" :description "b" :severity :info :expect [:present :x] :says "c"}]}])
                    [:steps 1 1 :title] "non-blank"]
                   [(assoc-in base [:steps 1] [:check {:title "t" :effect [] :reads {} :checks []}]) [:steps 1 1 :effect] "not a key of a check step"]
                   [(assoc-in base [:steps 1] [:check {:title "t" :reads {:x [:list "pods" {:namespace [:target]}]}
                                                       :checks [{:name "a" :description "b" :severity :info :expect [:present :y] :says "c"}]}])
                    [:steps 1 1 :checks 0 :expect 1] "names no declared :reads entry"]
                   [(-> base (dissoc :steps) (assoc :phases [{:phase :a :title "a" :steps [stamp-step]}
                                                             {:phase :a :title "b" :steps [purge-step]}]))
                    [:phases 1 :phase] "earlier phase"]
                   [(-> base (dissoc :steps) (assoc :phases [{:phase :a :steps [stamp-step]}])) [:phases 0 :title] "non-blank"]
                   [(-> base (dissoc :steps) (assoc :phases [{:phase "a" :title "a" :steps [stamp-step]}])) [:phases 0 :phase] "unqualified keyword"]
                   [(-> base (dissoc :steps) (assoc :phases [{:phase :a :title "a" :steps []}])) [:phases 0 :steps] "one or more steps"]
                   [(-> base (dissoc :steps) (assoc :phases [{:phase :a :title "a" :steps [(assoc stamp-step :target target)]}]))
                    [:phases 0 :steps 0 :target] "procedure's target"]]]
        (doseq [[data path words] cases]
          (let [refused (refusal-of data)]
            (is (= path (:path refused)) (pr-str [path refused]))
            (is (some-> (:reason refused) (str/includes? words)) (pr-str [words refused]))))
        (testing "the step's refusal names the step's path in its message"
          (is (str/starts-with? (ex-message (try (dsl/compile-procedure (assoc-in base [:steps 2 :effect 0 1] :holds))
                                                 (catch clojure.lang.ExceptionInfo e e)))
                                "[:steps 2 :effect 0 1]: ")))
        (testing "compile and compile-detective point a procedure at compile-procedure"
          (is (str/includes? (:reason (try (dsl/compile base) (catch clojure.lang.ExceptionInfo e (ex-data e))))
                             "compile-procedure"))
          (is (str/includes? (:reason (try (dsl/compile-detective base) (catch clojure.lang.ExceptionInfo e (ex-data e))))
                             "compile-procedure")))
        (testing "control: the base compiles"
          (is (dsl/procedure? (dsl/compile-procedure base))))))))

;; --- whole runs ----------------------------------------------------------------------

(deftest three-steps-run-under-one-confirmation
  (with-dir
    (fn [dir]
      (let [{:keys [result confirmations mutations objects printed]}
            (run-procedure (dsl/compile-procedure (procedure dir)) {:objects (shop)})]
        (is (true? result) printed)
        (is (= 1 (count confirmations)) "the operator is asked once")
        (let [{:keys [action items destructive?]} (first confirmations)]
          (is (= "adopt the namespace into Helm" action))
          (is (true? destructive?) "a destructive step asks for the typed yes")
          (is (= ["shop"
                  "1/3 stamp Helm ownership onto the adoptees"
                  "    patch each object of :adoptees (at most 25)"
                  "      patch persistentvolumeclaims shop/data"
                  (str "      patch " routes " shop/moodle")
                  "2/3 write the release file"
                  "    :dsl-procedure-test/write-file"
                  "3/3 delete the ansible leftovers"
                  "    delete each object of :leftovers (at most 25), destructive"]
                 (vec items))
              "every step, what it does, and the objects of the step judged before the confirmation"))
        (is (= [[:patch "persistentvolumeclaims" "shop" "data"]
                [:patch routes "shop" "moodle"]
                [:delete "services" "shop" "moodle-tideways"]
                [:delete "configmaps" "shop" "mdl-settings"]]
               mutations))
        (is (labelled? objects ["persistentvolumeclaims" "shop" "data"]))
        (is (file-says? dir "shop.txt" "release moodle"))
        (is (not (contains? objects ["services" "shop" "moodle-tideways"])))
        (is (not (contains? objects ["configmaps" "shop" "mdl-settings"])))))))

(deftest the-objects-shown-come-from-the-judgement-before-the-confirmation
  (with-dir
    (fn [dir]
      (testing "control: a step without :outline-now shows only what its data says"
        (let [spec (update-in (dsl/compile-procedure (procedure dir)) [:steps 0] dissoc :outline-now)
              {:keys [confirmations]} (run-procedure spec {:objects (shop)})]
          (is (not-any? #(str/includes? % "persistentvolumeclaims shop/data") (:items (first confirmations))))))
      (testing ":outline-now reads nothing itself"
        (let [spec (assoc-in (dsl/compile-procedure (procedure dir)) [:steps 0 :outline-now]
                             (fn [_] [(:out (shell/execute "git" "config" "user.name"))]))
              thrown (try (run-procedure spec {:objects (shop)}) nil
                          (catch clojure.lang.ExceptionInfo e e))]
          (is (offline/forbidden? thrown) "the confirmation is never built from a read made for it"))))))

(deftest a-declined-confirmation-changes-nothing
  (with-dir
    (fn [dir]
      (let [{:keys [result mutations]} (run-procedure (dsl/compile-procedure (procedure dir))
                                                      {:objects (shop) :answer false})]
        (is (false? result))
        (is (= [] mutations))
        (is (not (fs/exists? (fs/file dir "shop.txt"))))))))

(deftest a-refused-second-step-stops-the-procedure-and-says-where
  (with-dir
    (fn [dir]
      (let [{:keys [result mutations objects printed recovery]}
            (run-procedure (dsl/compile-procedure (procedure dir)) {:objects (shop hold)})]
        (is (false? result))
        (is (str/includes? printed "refusing: a hold ConfigMap stops the install") printed)
        (is (= [[:patch "persistentvolumeclaims" "shop" "data"] [:patch routes "shop" "moodle"]] mutations)
            "the first step ran, nothing after the refused one did")
        (is (not (fs/exists? (fs/file dir "shop.txt"))))
        (is (contains? objects ["services" "shop" "moodle-tideways"]) "the third step never deleted anything")
        (is (str/includes? recovery "the namespace shop may hold stamped objects and no release file") recovery)
        (is (str/includes? recovery "finished: 1/3 stamp") recovery)
        (is (str/includes? recovery "stopped at: 2/3 write") recovery)
        (is (str/includes? recovery "not run: 3/3 purge") recovery)
        (testing "control: without the progress report the guidance names no step"
          (let [spec (dissoc (dsl/compile-procedure (procedure dir)) :report-progress)
                {:keys [recovery]} (run-procedure spec {:objects (shop hold)})]
            (is (not (str/includes? (str recovery) "not run:")))))))))

(deftest a-failed-post-check-in-step-2-stops-before-step-3
  (with-dir
    (fn [dir]
      ;; a write that writes the wrong text: its post-check does not hold
      (effect/register-effect-type! :dsl-procedure-test/write-file
                                    (assoc (types :dsl-procedure-test/write-file)
                                           :perform (fn [[d file _]] (spit (fs/file d file) "garbage") true)))
      (let [{:keys [result mutations objects recovery]}
            (run-procedure (dsl/compile-procedure (procedure dir)) {:objects (shop)})]
        (is (false? result))
        (is (= 2 (count mutations)) "only the stamp's patches")
        (is (contains? objects ["configmaps" "shop" "mdl-settings"]))
        (is (str/includes? recovery "stopped at: 2/3 write") recovery)))))

(deftest a-procedure-started-again-skips-what-took-effect
  (with-dir
    (fn [dir]
      (let [spec (dsl/compile-procedure (procedure dir))
            first-run (run-procedure spec {:objects (shop hold)})
            {:keys [result confirmations mutations]}
            (run-procedure spec {:objects (dissoc (:objects first-run) ["configmaps" "shop" "hold"])})]
        (is (false? (:result first-run)))
        (is (true? result))
        (is (= "1/3 stamp Helm ownership onto the adoptees (already done)"
               (second (:items (first confirmations)))))
        (is (= [[:delete "services" "shop" "moodle-tideways"] [:delete "configmaps" "shop" "mdl-settings"]] mutations)
            "the stamp is not applied again")
        (is (file-says? dir "shop.txt" "release moodle"))))))

(deftest a-dry-run-shows-every-step-and-changes-nothing
  (with-dir
    (fn [dir]
      (let [{:keys [result confirmations mutations printed]}
            (run-procedure (dsl/compile-procedure (procedure dir)) {:objects (shop) :dry-run? true})]
        (is (true? result) printed)
        (is (= [] confirmations))
        (is (= [] mutations))
        (is (not (fs/exists? (fs/file dir "shop.txt"))))
        (doseq [words ["1/3 stamp" "2/3 write" "3/3 purge"
                       "patch persistentvolumeclaims data" (str "write shop.txt in " dir)
                       "delete services moodle-tideways" "delete configmaps mdl-settings"]]
          (is (str/includes? printed words) words))))))

(deftest a-pause-waits-for-its-checks-and-a-check-step-blocks
  (with-dir
    (fn [dir]
      (let [spec (dsl/compile-procedure (phased dir))]
        (testing "the release installed and no hold: every step runs"
          (let [{:keys [result mutations confirmations printed]} (run-procedure spec {:objects (shop installed)})]
            (is (true? result) printed)
            (is (= 1 (count confirmations)))
            (is (some #{"⏸  install: merge the release into the GitOps repository"} (:items (first confirmations))))
            (is (= 4 (count mutations)))))
        (testing "the release not installed: the pause's check fails and the purge never runs"
          (let [{:keys [result mutations recovery printed]} (run-procedure spec {:objects (shop)})]
            (is (false? result))
            (is (str/includes? printed "the release in shop is installed") printed)
            (is (= 2 (count mutations)))
            (is (str/includes? recovery "not run: 3/4 install: no hold remains; 4/4 clean up: purge")
                recovery)))))))

(deftest a-check-step-with-a-critical-finding-stops-the-procedure
  (with-dir
    (fn [dir]
      (let [spec (dsl/compile-procedure
                  (assoc (procedure dir) :steps
                         [stamp-step
                          [:check {:title "no hold remains"
                                   :reads {:hold [:present [{:type "configmaps" :name "hold"}] {:namespace [:target]}]}
                                   :checks [{:name "no-hold" :description "no hold ConfigMap stops the purge"
                                             :severity :critical :expect [:absent :hold]
                                             :says "a hold ConfigMap remains in {target}"}]}]
                          purge-step]))]
        (let [{:keys [result mutations printed recovery]} (run-procedure spec {:objects (shop hold)})]
          (is (false? result))
          (is (str/includes? printed "a hold ConfigMap remains in shop") printed)
          (is (= 2 (count mutations)) "the stamp ran, the purge did not")
          (is (str/includes? recovery "stopped at: 2/3 no hold remains") recovery))
        (testing "control: without the hold the purge runs"
          (let [{:keys [result mutations]} (run-procedure spec {:objects (shop)})]
            (is (true? result))
            (is (= 4 (count mutations)))))
        (testing "an advisory check informs and the procedure goes on"
          (let [spec (dsl/compile-procedure
                      (assoc-in (procedure dir) [:steps 1]
                                [:check {:title "no hold remains" :blocking? false
                                         :reads {:hold [:present [{:type "configmaps" :name "hold"}] {:namespace [:target]}]}
                                         :checks [{:name "no-hold" :description "d" :severity :critical
                                                   :expect [:absent :hold] :says "a hold remains"}]}]))
                {:keys [result mutations]} (run-procedure spec {:objects (shop hold)})]
            (is (true? result))
            (is (= 4 (count mutations)))))))))

(deftest a-run-resumed-after-the-pause-starts-there
  (with-dir
    (fn [dir]
      (let [spec (dsl/compile-procedure (phased dir))
            {:keys [result mutations confirmations]}
            (run-procedure spec {:objects (shop installed) :opts {:from-segment 2}})]
        (is (true? result))
        (is (= [[:delete "services" "shop" "moodle-tideways"] [:delete "configmaps" "shop" "mdl-settings"]] mutations)
            "the steps before the pause are not run again")
        (is (= ["shop" "⏸  install: merge the release into the GitOps repository" "3/4 install: no hold remains"
                "4/4 delete the ansible leftovers" "    delete each object of :leftovers (at most 25), destructive"
                "      delete services shop/moodle-tideways" "      delete configmaps shop/mdl-settings"]
               (vec (:items (first confirmations))))
            "the first change of the resumed segment is judged before the confirmation, so its objects show")
        (is (not (fs/exists? (fs/file dir "shop.txt"))) "the write before the pause did not run")))))

(deftest help-lists-every-step-with-what-it-does
  (with-dir
    (fn [dir]
      (let [details (vec (playbook/details (dsl/compile-procedure (procedure dir))))]
        (is (some #{"      patch each object of :adoptees (at most 25)"} details) details)
        (is (some #{"      delete each object of :leftovers (at most 25), destructive"} details) details)
        (is (some #(str/includes? % "asks once to confirm every step below (destructive - type yes)") details))))))

(deftest every-change-step-lists-what-it-touches
  (with-dir
    (fn [dir]
      (doseq [step (:steps (dsl/compile-procedure (procedure dir)))]
        (let [state (atom {:objects (shop)})]
          (with-redefs-fn {#'shell/execute (fn [& argv] (respond! state (atom []) (vec argv)))}
            #(binding [kube-context/*pinned* {:context "ctx" :cluster "cluster"}]
               (plan/covers! step {:target "shop"}))))))))

;; --- equivalence with a playbook built by hand ---------------------------------------

(defn- present
  [context namespace refs]
  (vec (for [ref refs
             :let [o (result/object! (reads/object context (assoc ref :namespace namespace)))]
             :when o]
         {:ref (assoc ref :namespace namespace) :object o})))

(defn- helm-stamped?
  [o]
  (and (= "Helm" (get-in o [:metadata :labels :app.kubernetes.io/managed-by]))
       (= "moodle" (get-in o [:metadata :annotations :meta.helm.sh/release-name]))))

(defn- hand-built
  "The adoption as a Clojure playbook spec, the way a casebook writes one
   today."
  [dir]
  (let [adoptees [{:type "persistentvolumeclaims" :name "data"} {:type routes :name "moodle"}]
        leftovers [{:type "services" :name "moodle-tideways"} {:type "configmaps" :name "mdl-settings"}]
        patch (json/generate-string {:metadata {:labels {"app.kubernetes.io/managed-by" "Helm"}
                                                :annotations {"meta.helm.sh/release-name" "moodle"}}})]
    {:description "Adopts a namespace's objects into a Helm release"
     :select {:mode :one :label "namespace" :flag :namespace :candidates (fn [_] ["shop"])}
     :subject {:type "namespaces"}
     :confirm {:action "adopt the namespace into Helm" :destructive? true}
     :steps [{:title "stamp" :subject {:type "namespaces"} :prerequisites {}
              :confirm {:action "stamp Helm ownership onto the adoptees" :destructive? false}
              :values {:members (fn [{:keys [context target]}] (present context target adoptees))}
              :effect (fn [{:keys [context members]}]
                        (apply effect/plan
                               (for [{{:keys [type namespace name]} :ref o :object} members :when (not (helm-stamped? o))]
                                 (effect/kubectl context (str "--namespace=" namespace) "patch" type name "--type=merge"
                                                 (str "--patch=" patch)))))
              :verify (fn [{:keys [context members]}]
                        (for [{{:keys [type namespace name]} :ref} members]
                          {:description (str name " carries Helm's ownership")
                           :check #(verify/object-holds? (kubectl/get-namespaced-resource context namespace type name) helm-stamped?)}))
              :done true}
             {:title "write" :subject {:type "namespaces"} :prerequisites {}
              :confirm {:action "write the release file" :destructive? false}
              :refusal (fn [{:keys [context target]}]
                         (when (seq (present context target [{:type "configmaps" :name "hold"}]))
                           "a hold ConfigMap stops the install"))
              :effect (fn [_] (effect/plan [:dsl-procedure-test/write-file dir "shop.txt" "release moodle"]))
              :verify (fn [_] [{:description "the release file is written" :check #(file-says? dir "shop.txt" "release moodle")}])
              :done true}
             {:title "purge" :subject {:type "namespaces"} :prerequisites {}
              :confirm {:action "delete the ansible leftovers" :destructive? true}
              :recovery "a deleted leftover is gone; the ansible role recreates it"
              :values {:members (fn [{:keys [context target]}] (present context target leftovers))}
              :effect (fn [{:keys [context members]}]
                        (apply effect/plan (for [{:keys [ref]} members] (deletes/effect context ref {:wait false}))))
              :verify (fn [{:keys [context members]}]
                        (for [{{:keys [type namespace name]} :ref} members]
                          {:description (str name " is gone") :verifies-absence? true
                           :check #(kubectl/confirmed-gone? context namespace type name)}))}]}))

(deftest a-procedure-acts-as-the-playbook-a-casebook-builds-by-hand
  (doseq [[scenario objects] [["a clean namespace" (shop)] ["a hold stopping the write" (shop hold)]]]
    (with-dir
      (fn [dsl-dir]
        (with-dir
          (fn [hand-dir]
            (let [from-data (run-procedure (dsl/compile-procedure (procedure dsl-dir)) {:objects objects})
                  by-hand (run-procedure (hand-built hand-dir) {:objects objects})
                  strip (fn [objects] (update-vals objects #(update % :metadata dissoc :resourceVersion)))]
              (is (= (:result by-hand) (:result from-data)) scenario)
              (is (= (:mutations by-hand) (:mutations from-data)) scenario)
              (is (= (strip (:objects by-hand)) (strip (:objects from-data))) scenario)
              (is (= (fs/exists? (fs/file hand-dir "shop.txt")) (fs/exists? (fs/file dsl-dir "shop.txt"))) scenario)
              (is (= (count (:confirmations by-hand)) (count (:confirmations from-data))) scenario)
              (is (= (:destructive? (first (:confirmations by-hand))) (:destructive? (first (:confirmations from-data)))) scenario)
              (testing "control: the comparison sees a difference"
                (let [other (run-procedure (update (hand-built hand-dir) :steps pop) {:objects objects})]
                  (when (= "a clean namespace" scenario)
                    (is (not= (:mutations other) (:mutations from-data)))))))))))))

;; --- the book file -------------------------------------------------------------------

(def ^:private book-file "tests/edn-books/playbooks/kubernetes/workloads/cold-restart.edn")

(deftest the-book-file-loads-as-a-playbook
  (let [compiled (edn-book/load-book book-file)]
    (is (dsl/procedure? compiled))
    (is (= [:change :pause :change] (mapv playbook/kind (:steps compiled))))
    (let [config (binding [runbook/*loading* true] (edn-book/book book-file))]
      (is (= "Restarts a Deployment or StatefulSet cold: scales it to zero, waits for the operator, scales it back"
             (:description config)))
      (is (contains? (:options config) :replicas))
      (is (contains? (:options config) :from-segment) "a procedure with a pause resumes with --from-segment"))))
