;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.conformance-library-test
  "gumshoe.conformance run against a casebook it does not know, built in a
   temporary directory."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.conformance :as conformance]
            [gumshoe.flags]))

(def ^:private header
  (str ";; SPDX-FileCopyrightText: The casebook Authors\n"
       ";; SPDX-License-" "Identifier: Apache-2.0\n\n"))

(def ^:private good-book
  (str header
       "(ns casebook.hello.ok\n"
       "  \"A book that follows every rule. For a change, use runbook/execute! or\n"
       "   gumshoe.mutation/book instead; neither mention is a call.\"\n"
       "  (:require [gumshoe.detective]))\n\n"
       ";; (mutation/book {...}) in a comment is not a call either\n"
       "(gumshoe.detective/book\n"
       " {:description \"Always clean\"\n"
       "  :options {:namespace {:desc \"The namespace\" :alias :n}}\n"
       "  :detectives [{:name \"ok\" :detect (fn [_] [])}]})\n"))

(def ^:private spec-library
  (str header
       "(ns casebook.spec\n"
       "  \"Reads with aliased keywords, a var quote, a syntax quote and a reader conditional.\"\n"
       "  (:require [clojure.spec.alpha :as s]))\n\n"
       "(s/def ::s/name string?)\n"
       "(def kw ::local)\n"
       "(def v #'kw)\n"
       "(defmacro twice [x] (list 'do x x))\n"
       "(defmacro quoted [x] `(do ~x ~@[x]))\n"
       "(def platform #?(:bb :bb :clj :clj))\n"))

(def ^:private unbalanced-library
  (str header
       "(ns casebook.moodle.database\n"
       "  \"Has an unbalanced form.\"\n"
       "  (:require [clojure.string :as str]))\n\n"
       "(defn database-name [release]\n"
       "  (str/replace release \"-\" \"_\")\n"))

(def ^:private unbalanced-book
  (str header
       "(ns casebook.moodle.adopt.status\n"
       "  \"Has an unbalanced form.\"\n"
       "  (:require [gumshoe.detective :as detective]))\n\n"
       "(detective/book\n"
       " {:description \"Status\"\n"
       "  :detectives [{:name \"status\" :detect (fn [_] [])}]}))\n"))

(def ^:private sloppy-book
  (str ";; no license here\n"
       "(ns casebook.sloppy)\n\n"
       "(runbook/execute!\n"
       " {:options {:name {:alias :n}\n"
       "            :node {:desc \"The node\" :alias :n :colour :red}\n"
       "            :host {:desc \"The host\" :alias :h}}\n"
       "  :action (fn [_ _] true)})\n"))

(def ^:private twice-book
  (str header
       "(ns casebook.twice\n"
       "  \"Enters the harness twice, once from a helper.\"\n"
       "  (:require [gumshoe.runbook :as runbook]))\n\n"
       "(defn- again [] (runbook/execute! {:description \"two\" :action (fn [_ _] true)}))\n\n"
       "(runbook/execute! {:description \"one\" :action (fn [_ _] true)})\n"))

(defn- casebook!
  "A casebook in a temporary directory: {relative path -> content}."
  [files]
  (let [root (fs/create-temp-dir {:prefix "casebook"})]
    (doseq [[path content] files
            :let [file (fs/path root path)]]
      (fs/create-dirs (fs/parent file))
      (spit (str file) content))
    (str root)))

(defn- roots
  [root]
  {:books [(str (fs/path root "runbooks")) (str (fs/path root "playbooks"))]
   :libraries [(str (fs/path root "libraries"))]})

(defn- by-file
  "{file name -> sorted checks} of findings."
  [findings]
  (update-vals (group-by #(str (fs/file-name (:file %))) findings)
               #(vec (sort (map :check %)))))

(def ^:private reset-library
  (str header
       "(ns casebook.reset\n"
       "  \"What the casebook still defines after gumshoe took over action-in-flight?.\")\n\n"
       "(defn release-name [target] (second (re-find #\"/(.*)\" target)))\n"))

(def ^:private unresolved-book
  (str header
       "(ns casebook.moodle.reset\n"
       "  \"Calls a function its library no longer defines, inside the action.\"\n"
       "  (:require [casebook.reset :as reset]\n"
       "            [gumshoe.runbook :as runbook]))\n\n"
       "(runbook/execute!\n"
       " {:description \"Reset\"\n"
       "  :action (fn [_ _] (reset/action-in-flight? (reset/release-name \"apps/moodle\")))})\n"))

(defn- marker-book
  "A book whose action writes `marker`, so a run of it leaves a trace."
  [marker]
  (str header
       "(ns casebook.marker\n"
       "  \"Writes a marker file when it runs.\"\n"
       "  (:require [casebook.reset :as reset]\n"
       "            [gumshoe.runbook :as runbook]))\n\n"
       "(runbook/execute!\n"
       " {:description \"Marker\"\n"
       "  :action (fn [_ _] (spit " (pr-str marker) " (reset/release-name \"apps/ran\")) true)})\n"))

(deftest a-book-that-parses-but-does-not-load-is-named-with-its-symbol
  (let [root (casebook! {"libraries/casebook/reset.clj" reset-library
                         "runbooks/moodle/reset.clj" unresolved-book
                         "runbooks/hello/ok.clj" good-book})]
    (try
      (let [findings (conformance/findings-in (roots root))
            [finding] findings]
        (is (= {"reset.clj" [:load]} (by-file findings))
            "only the book that cannot load, and nothing for the library it requires")
        (is (str/ends-with? (:file finding) "runbooks/moodle/reset.clj"))
        (is (str/includes? (:message finding) "reset/action-in-flight?") (:message finding)))
      (testing "verify! fails and names the book"
        (let [err (java.io.StringWriter.)
              e (binding [*out* (java.io.StringWriter.) *err* err]
                  (try (conformance/verify! (roots root)) nil
                       (catch clojure.lang.ExceptionInfo e e)))]
          (is (= 1 (:babashka/exit (ex-data e))))
          (is (str/includes? (str err) "runbooks/moodle/reset.clj"))
          (is (str/includes? (str err) "reset/action-in-flight?"))))
      (testing ":load? false leaves loading to the caller"
        (is (= [] (conformance/findings-in (assoc (roots root) :load? false)))))
      (finally (fs/delete-tree root)))))

(deftest loading-a-book-does-not-run-it
  (let [marker (str (fs/path (fs/create-temp-dir {:prefix "marker"}) "ran"))
        root (casebook! {"libraries/casebook/reset.clj" reset-library
                         "runbooks/marker.clj" (marker-book marker)})]
    (try
      (is (= [] (conformance/findings-in (roots root))))
      (is (not (fs/exists? marker)) "the book's action never ran")
      (finally (fs/delete-tree root) (fs/delete-tree (fs/parent marker))))))

(deftest a-casebook-that-follows-every-rule-has-no-findings
  (let [root (casebook! {"runbooks/hello/ok.clj" good-book
                         "libraries/casebook/spec.clj" spec-library})]
    (try
      (is (= [] (conformance/findings-in (roots root))))
      (testing "verify! reports the files it checked"
        (is (= 2 (binding [*out* (java.io.StringWriter.)] (conformance/verify! (roots root))))))
      (finally (fs/delete-tree root)))))

(deftest a-casebook-file-with-an-unbalanced-form-is-named
  (let [root (casebook! {"runbooks/hello/ok.clj" good-book
                         "runbooks/moodle/adopt/status.clj" unbalanced-book
                         "libraries/casebook/moodle/database.clj" unbalanced-library})]
    (try
      (let [findings (conformance/findings-in (roots root))]
        (is (= {"status.clj" [:unparseable] "database.clj" [:unparseable]} (by-file findings))
            "an unparseable file is reported once and skipped by the structural checks")
        (is (every? #(str/includes? (:message %) "does not parse") findings)))
      (testing "verify! fails with exit code 1 and names each file"
        (let [err (java.io.StringWriter.)
              e (binding [*out* (java.io.StringWriter.) *err* err]
                  (try (conformance/verify! (roots root)) nil
                       (catch clojure.lang.ExceptionInfo e e)))]
          (is (= 1 (:babashka/exit (ex-data e))))
          (is (str/includes? (str err) "runbooks/moodle/adopt/status.clj"))
          (is (str/includes? (str err) "libraries/casebook/moodle/database.clj"))
          (is (not (str/includes? (str err) "ok.clj")))))
      (finally (fs/delete-tree root)))))

(deftest a-casebook-book-that-breaks-the-structural-rules-is-named
  (let [root (casebook! {"runbooks/sloppy.clj" sloppy-book
                         "playbooks/twice.clj" twice-book})]
    (try
      (let [findings (conformance/findings-in (roots root))
            messages (map :message (filter #(= "sloppy.clj" (str (fs/file-name (:file %)))) findings))]
        (is (= {"sloppy.clj" [:description :license :load :namespace-docstring :options :options :options]
                "twice.clj" [:harness]}
               (by-file findings)))
        (is (some #(str/includes? % "option :name has no :desc") messages))
        (is (some #(str/includes? % "unknown option keys [:colour]") messages))
        (is (some #(str/includes? % "colliding option aliases") messages))
        (is (some #(str/includes? % "needs a :description") messages)))
      (testing "a license the caller names must be the one each file carries"
        (is (= #{"sloppy.clj" "twice.clj"}
               (set (for [f (conformance/findings-in (assoc (roots root) :license "0BSD"))
                          :when (= :license (:check f))]
                      (str (fs/file-name (:file f))))))))
      (finally (fs/delete-tree root)))))

(deftest roots-a-casebook-does-not-have-hold-no-files
  (is (= [] (conformance/clj-files ["does-not-exist"]))))

(def ^:private pair-book
  (str header
       "(ns casebook.moodle.pair\n"
       "  \"Destructures namespace-name-map as a pair inside an effect.\"\n"
       "  (:require [gumshoe.kubectl :as kubectl]\n"
       "            [gumshoe.mutation :as mutation]))\n\n"
       "(mutation/book\n"
       " {:description \"Pair\"\n"
       "  :select {:mode :namespaced}\n"
       "  :effect (fn [{:keys [target]}]\n"
       "            (let [[ns name] (kubectl/split-namespace-name target)]\n"
       "              [ns name]))})\n"))

(def ^:private pair-library
  (str header
       "(ns casebook.targets\n"
       "  \"Destructures namespace-name-map as a pair in a for :let, and by key.\"\n"
       "  (:require [gumshoe.kubectl :as kubectl]))\n\n"
       "(defn names [targets]\n"
       "  (for [t targets :let [[_ n] (kubectl/namespace-name-map t)]] n))\n\n"
       "(defn namespaces [targets]\n"
       "  (for [t targets :let [{:keys [namespace]} (kubectl/namespace-name-map t)]] namespace))\n"))

(deftest a-vector-binding-of-namespace-name-map-is-named
  (let [root (casebook! {"runbooks/moodle/pair.clj" pair-book
                         "libraries/casebook/targets.clj" pair-library})]
    (try
      ;; the pair book's spec is no more than its effect, so its contract
      ;; judgements are notes here
      (let [findings (conformance/findings-in (assoc (roots root) :spec-contract :advise))]
        (is (= {"pair.clj" [:destructuring] "targets.clj" [:destructuring]} (by-file findings)))
        (is (some #(str/includes? (:message %) "binds kubectl/split-namespace-name to the vector [ns name]") findings))
        (is (some #(str/includes? (:message %) "{:namespace .. :name ..}") findings)))
      (finally (fs/delete-tree root)))))

(defn- without-header
  "content with its SPDX header removed, as in a casebook that carries none."
  [content]
  (str/replace content header ""))

(deftest a-casebook-without-headers-skips-only-the-license-check-when-asked
  (let [root (casebook! {"runbooks/hello/ok.clj" (without-header good-book)
                         "libraries/casebook/reset.clj" (without-header reset-library)
                         "runbooks/moodle/reset.clj" (without-header unresolved-book)
                         "runbooks/moodle/adopt/status.clj" (without-header unbalanced-book)
                         "playbooks/twice.clj" (without-header twice-book)})]
    (try
      (testing "by default every file must carry a header"
        (is (= #{"ok.clj" "reset.clj" "status.clj" "twice.clj"}
               (set (for [f (conformance/findings-in (roots root))
                          :when (= :license (:check f))]
                      (str (fs/file-name (:file f))))))))
      (testing ":license false omits the header check and keeps every other one"
        (let [findings (conformance/findings-in (assoc (roots root) :license false))]
          (is (not-any? #(= :license (:check %)) findings))
          (is (= {"reset.clj" [:load] "status.clj" [:unparseable] "twice.clj" [:harness]}
                 (by-file findings)))))
      (testing "verify! accepts :license false"
        (let [clean (casebook! {"runbooks/hello/ok.clj" (without-header good-book)})]
          (try
            (is (= 1 (binding [*out* (java.io.StringWriter.)]
                       (conformance/verify! (assoc (roots clean) :license false)))))
            (finally (fs/delete-tree clean)))))
      (finally (fs/delete-tree root)))))

(defn- mutation-book
  [ns spec]
  (str header
       "(ns " ns "\n"
       "  \"A change.\"\n"
       "  (:require [gumshoe.mutation :as mutation]))\n\n"
       "(mutation/book " spec ")\n"))

(deftest a-mutation-book-without-recovery-is-an-advisory-and-passes
  (let [root (casebook! {"runbooks/bare.clj"
                         (mutation-book "casebook.bare" "{:description \"Cordons\" :select {:mode :one} :confirm {:action \"cordon\"}}")
                         "runbooks/guided.clj"
                         (mutation-book "casebook.guided" "{:description \"Cordons\" :select {:mode :one} :confirm {:action \"cordon\"} :recovery \"uncordon it\"}")
                         "runbooks/shared.clj"
                         (mutation-book "casebook.shared" "(merge shared {:description \"Cordons\"})")})
        opts (assoc (roots root) :load? false)]
    (try
      (is (= {"bare.clj" [:recovery]} (by-file (conformance/advisories (conformance/files-in opts))))
          "a literal spec without :recovery is named; one with it, and a merged one, are not")
      (is (empty? (filter #(= :recovery (:check %)) (conformance/findings-in opts))) "the gate does not fail on it")
      (let [err (java.io.StringWriter.)]
        (is (= 3 (binding [*out* (java.io.StringWriter.) *err* err] (conformance/verify! opts))))
        (is (str/includes? (str err) "note: ") "verify! prints it as a note")
        (is (str/includes? (str err) "bare.clj") (str err)))
      (finally (fs/delete-tree root)))))

;; --- the contract of a mutation spec ----------------------------------------

(def ^:private complete-spec
  {:confirm {:action "cordon a node" :destructive? false}
   :prerequisites {:kubectl-can-patch ["nodes"]}
   :effect (fn [_] [])
   :verify (fn [_] [])})

(deftest a-complete-spec-has-no-contract-problems
  (is (empty? (conformance/contract-problems complete-spec))))

(deftest every-omission-of-the-contract-is-a-problem
  (doseq [[omission spec expected]
          [["no confirmation action" (assoc complete-spec :confirm {:destructive? false}) "no confirmation action"]
           ["a blank confirmation action" (assoc-in complete-spec [:confirm :action] " ") "no confirmation action"]
           ["no statement of destructiveness" (update complete-spec :confirm dissoc :destructive?) "whether it is destructive"]
           ["no prerequisites" (dissoc complete-spec :prerequisites) "no :prerequisites"]
           ["no effect" (dissoc complete-spec :effect) "no :effect"]
           ["no post-check and no reason" (dissoc complete-spec :verify) "no post-check (:verify) and states no reason"]
           ["a blank reason" (-> complete-spec (dissoc :verify) (assoc :no-verify "")) ":no-verify must be a string"]
           ["a post-check and a reason" (assoc complete-spec :no-verify "none needed") "drop the reason"]]]
    (testing omission
      (let [problems (conformance/contract-problems spec)]
        (is (= 1 (count problems)) (pr-str problems))
        (is (str/includes? (first problems) expected) (pr-str problems))))))

(deftest a-reason-stands-in-for-a-post-check
  (is (empty? (conformance/contract-problems
               (-> complete-spec (dissoc :verify) (assoc :no-verify "the session is the result"))))))

(def ^:private destructive-spec
  (assoc-in complete-spec [:confirm :destructive?] true))

(deftest a-destructive-spec-without-a-guard-is-a-guard-problem
  (let [[problem & more] (conformance/guard-problems destructive-spec)]
    (is (nil? more))
    (is (str/includes? problem "is destructive and judges nothing again after the confirmation") problem)))

(deftest every-guard-satisfies-the-rule
  (doseq [[guard spec] [[":refusal" (assoc destructive-spec :refusal (fn [_] nil))]
                        [":precondition" (assoc destructive-spec :precondition (fn [_] true))]
                        [":subject" (assoc destructive-spec :subject {:type "nodes"} :select {:mode :one})]
                        [":subjects" (assoc destructive-spec :subjects (fn [_] []))]
                        [":pin" (assoc destructive-spec :pin (fn [_] nil))]
                        [":no-guard" (assoc destructive-spec :no-guard "the effect refuses a target that moved")]]]
    (testing guard
      (is (empty? (conformance/guard-problems (change/normalize spec)))))))

(deftest a-spec-that-is-not-destructive-needs-no-guard
  (is (empty? (conformance/guard-problems complete-spec))))

(deftest a-no-guard-reason-is-judged-like-a-no-verify-reason
  (is (str/includes? (first (conformance/guard-problems (assoc destructive-spec :no-guard " ")))
                     ":no-guard must be a string"))
  (is (str/includes? (first (conformance/guard-problems
                             (change/normalize (assoc destructive-spec :no-guard "why" :refusal (fn [_] nil)))))
                     "drop the reason")))

(deftest a-declared-pin-is-called-with-a-pick-in-a-namespace-of-its-own
  (let [ns conformance/probe-namespace
        problems #(conformance/pin-problems (assoc complete-spec :pin %1) %2)
        one {:mode :one :label "Namespace" :flag :namespace}
        namespaced {:mode :namespaced :label "Database" :namespace-flag :namespace :name-flag :name}]
    (testing "a pin on the picked namespace, as a namespace or as <namespace>/<name>"
      (is (empty? (problems (fn [{:keys [target]}] {:type "databases" :namespace target :name "moodle"}) one)))
      (is (empty? (problems (fn [{:keys [opts]}] {:type "databases" :namespace (:namespace opts) :name (:name opts)})
                            namespaced))))
    (testing "a pin on the subject's ref"
      (is (empty? (conformance/pin-problems
                   (assoc complete-spec :subject {:type "cronjobs"} :pin (fn [{:keys [ref]}] ref))
                   namespaced))))
    (testing "a cluster-scoped ref has no namespace to ignore"
      (is (empty? (problems (fn [{:keys [target]}] {:type "nodes" :name target}) one))))
    (testing "a pin that pins nothing before its derived values exist is not judged"
      (is (empty? (problems (fn [{:keys [observed]}] (when observed {:type "x" :name "y"})) namespaced))))
    (testing "a pin that ignores the picked namespace"
      (let [[problem :as all] (problems (fn [_] {:type "databases" :namespace "default" :name "moodle"}) one)]
        (is (= 1 (count all)))
        (is (str/includes? problem "\"default\""))
        (is (str/includes? problem ns))))
    (testing "a pin that returns no ref with a type and a name"
      (is (str/includes? (first (problems (fn [{:keys [target]}] {:namespace target :name "moodle"}) one))
                         "no ref with a :type and a :name"))
      (is (str/includes? (first (problems (fn [_] "moodle") one)) "no ref")))
    (testing "a pin that throws, and one that is no function"
      (is (str/includes? (first (problems (fn [_] (throw (ex-info "boom" {}))) one)) "throws"))
      (is (= ["declares a :pin that is not a function"] (problems {:type "x"} one))))))

(deftest an-alias-and-its-canonical-key-get-the-same-findings
  (let [one {:mode :one :label "Namespace" :flag :namespace}
        stray (fn [_] {:type "databases" :namespace "default" :name "moodle"})
        judged #(conformance/spec-problems {:spec % :select one})]
    (doseq [[label alias canonical]
            [["a pin that ignores the picked namespace"
              (assoc complete-spec :pin stray)
              (assoc complete-spec :subjects [{:from :reads :ref stray}])]
             ["a pin that is no function"
              (assoc complete-spec :pin {:type "x"})
              (assoc complete-spec :subjects [{:from :reads :ref {:type "x"}}])]
             ["a pin on the subject's ref"
              (assoc complete-spec :subject {:type "cronjobs"} :pin (fn [{:keys [ref]}] ref))
              (assoc complete-spec :subjects [{:from :pick :type "cronjobs"} {:from :reads :ref (fn [{:keys [ref]}] ref)}])]
             ["a precondition and a missing post-check"
              (-> complete-spec (dissoc :verify) (assoc :precondition (constantly true)))
              (-> complete-spec (dissoc :verify) (assoc :refusal (constantly nil)))]]]
      (testing label
        (is (= (judged alias) (judged canonical)))))
    (is (seq (judged (assoc complete-spec :pin stray))) "the pin is judged at all")))

(deftest a-spec-normalize-refuses-is-one-finding
  (is (= ["is refused: the spec has two empty messages, \"a\" at the top and \"b\" in :select - keep the one in :select"]
         (conformance/spec-problems {:spec (assoc complete-spec :empty-message "a" :select {:mode :one :empty-message "b"})
                                     :select nil})))
  (is (= ["is refused: a :subject, :subjects or :pin reads its objects with kubectl, so a spec with :connect takes neither"]
         (conformance/spec-problems {:spec (assoc complete-spec :connect identity :subject {:type "x"}) :select nil}))))

(def ^:private contract-library
  (str header
       "(ns casebook.contract.steps\n"
       "  \"A shared spec without a post-check, and a toggle pair.\"\n"
       "  (:require [gumshoe.toggle :as toggle]))\n\n"
       "(def unverified\n"
       "  {:prerequisites {:kubectl-can-patch [\"nodes\"]}\n"
       "   :select {:mode :one :label \"Node\" :flag :node :candidates (fn [_] [])}\n"
       "   :confirm {:action \"cordon a node\" :destructive? false}\n"
       "   :effect (fn [_] [])})\n\n"
       "(def pausing\n"
       "  (toggle/pair\n"
       "   {:spec {:prerequisites {:kubectl-can-patch [\"cronjobs\"]}\n"
       "           :select {:mode :namespaced :label \"CronJob\"}\n"
       "           :subject {:type \"cronjobs\"}}\n"
       "    :refusal (fn [_ _] nil) :items (fn [_ _] []) :patch (fn [_] \"{}\")\n"
       "    :applied? (fn [_ _] true) :verify {:timeout 1 :interval 1}\n"
       "    :on {:verb \"suspend\" :confirm {:action \"suspend a CronJob\"} :announce \"%s\" :verified \"%s\"}\n"
       "    :off {:verb \"resume\" :confirm {:action \"resume a CronJob\" :destructive? false} :announce \"%s\" :verified \"%s\"}}))\n"))

(def ^:private contract-merged-book
  (str header
       "(ns casebook.contract.cordon\n"
       "  \"Runs the shared spec.\"\n"
       "  (:require [casebook.contract.steps :as steps]\n"
       "            [gumshoe.mutation :as mutation]))\n\n"
       "(mutation/book (merge steps/unverified {:description \"Cordons a node\"}))\n"))

(def ^:private contract-playbook
  (str header
       "(ns casebook.contract.maintenance\n"
       "  \"A playbook whose second step does not say whether it is destructive.\"\n"
       "  (:require [gumshoe.playbook :as playbook]))\n\n"
       "(playbook/book\n"
       " {:description \"Maintenance\"\n"
       "  :select {:mode :one :label \"Node\" :flag :node :candidates (fn [_] [])}\n"
       "  :confirm {:action \"maintain a node\" :destructive? true}\n"
       "  :steps [{:title \"Check\" :check (fn [_] true)}\n"
       "          {:title \"Drain\" :prerequisites {} :confirm {:action \"drain a node\"}\n"
       "           :effect (fn [_] []) :no-verify \"the operator watches the drain\"}]})\n"))

(def ^:private contract-pin-book
  (str header
       "(ns casebook.contract.stamp\n"
       "  \"Pins an object in a namespace it does not pick.\"\n"
       "  (:require [gumshoe.mutation :as mutation]))\n\n"
       "(mutation/book\n"
       " {:description \"Stamps\"\n"
       "  :prerequisites {}\n"
       "  :select {:mode :one :label \"Namespace\" :flag :namespace :candidates (fn [_] [])}\n"
       "  :confirm {:action \"stamp a database\" :destructive? false}\n"
       "  :pin (fn [_] {:type \"databases\" :namespace \"default\" :name \"moodle\"})\n"
       "  :effect (fn [_] [])\n"
       "  :verify (fn [_] [])})\n"))

(deftest every-mutation-spec-a-casebook-defines-is-judged-as-it-evaluates
  (let [root (casebook! {"libraries/casebook/contract/steps.clj" contract-library
                         "runbooks/cordon.clj" contract-merged-book
                         "runbooks/stamp.clj" contract-pin-book
                         "playbooks/maintenance.clj" contract-playbook})]
    (try
      (let [findings (filter #(= :spec-contract (:check %)) (conformance/findings-in (roots root)))
            messages (set (map :message findings))]
        (is (= {"steps.clj" [:spec-contract :spec-contract] "stamp.clj" [:spec-contract]
                "maintenance.clj" [:spec-contract]}
               (by-file findings))
            "the merged book inherits the library's problem, which is named once, against the library")
        (is (contains? messages "mutation spec casebook.contract.steps/unverified has no post-check (:verify) and states no reason for having none (:no-verify \"why\")"))
        (is (some #(str/starts-with? % "mutation spec casebook.contract.steps/pausing :on does not state whether it is destructive") messages)
            "a toggle pair is judged per side, as toggle/pair built it")
        (is (some #(str/starts-with? % "mutation spec casebook.contract.maintenance step 2 \"Drain\" does not state whether it is destructive") messages))
        (is (some #(str/starts-with? % "mutation spec casebook.contract.stamp has a :pin that returns namespace \"default\"") messages)))
      (testing ":spec-contract :advise turns the judgements into notes"
        (is (not-any? #(= :spec-contract (:check %))
                      (conformance/findings-in (assoc (roots root) :spec-contract :advise))))
        (is (= 4 (count (filter #(= :spec-contract (:check %))
                                (:advisories (conformance/report-in (assoc (roots root) :spec-contract :advise))))))))
      (finally (fs/delete-tree root)))))

(def ^:private unguarded-book
  (str header
       "(ns casebook.guard.purge\n"
       "  \"Deletes without judging what it deletes again.\"\n"
       "  (:require [gumshoe.mutation :as mutation]))\n\n"
       "(mutation/book\n"
       " {:description \"Purges\"\n"
       "  :prerequisites {}\n"
       "  :select {:mode :one :label \"Namespace\" :flag :namespace :candidates (fn [_] [])}\n"
       "  :confirm {:action \"purge a namespace\" :destructive? true}\n"
       "  :effect (fn [_] [])\n"
       "  :verify (fn [_] [])})\n"))

(deftest a-destructive-spec-without-a-guard-is-a-note-unless-guards-are-required
  (let [root (casebook! {"runbooks/purge.clj" unguarded-book})
        guard? #(= :guard (:check %))]
    (try
      (let [{:keys [findings advisories]} (conformance/report-in (roots root))]
        (is (not-any? guard? findings) "a casebook keeps passing")
        (is (= ["mutation spec casebook.guard.purge is destructive and judges nothing again after the confirmation"]
               (map #(first (str/split (:message %) #",")) (filter guard? advisories)))))
      (testing ":guards :require fails on it"
        (is (= {"purge.clj" [:guard]}
               (by-file (filter guard? (conformance/findings-in (assoc (roots root) :guards :require))))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
                       (conformance/verify! (assoc (roots root) :guards :require))))))
      (finally (fs/delete-tree root)))))

(def ^:private verified-library
  (str header
       "(ns casebook.verified.steps\n"
       "  \"A shared spec with its post-check.\")\n\n"
       "(def cordon\n"
       "  {:prerequisites {:kubectl-can-patch [\"nodes\"]}\n"
       "   :select {:mode :one :label \"Node\" :flag :node :candidates (fn [_] [])}\n"
       "   :confirm {:action \"cordon a node\" :destructive? false}\n"
       "   :effect (fn [_] [])\n"
       "   :verify (fn [_] [])})\n"))

(defn- verified-book
  [ns spec]
  (str header
       "(ns " ns "\n"
       "  \"Cordons a node.\"\n"
       "  (:require [casebook.verified.steps :as steps]\n"
       "            [gumshoe.mutation :as mutation]))\n\n"
       "(mutation/book " spec ")\n"))

(deftest a-spec-with-its-verify-removed-makes-verify-fail-naming-the-spec
  (let [root (casebook! {"libraries/casebook/verified/steps.clj" verified-library
                         "runbooks/cordon.clj" (verified-book "casebook.verified.cordon"
                                                              "(merge steps/cordon {:description \"Cordons\"})")
                         "runbooks/unverified.clj" (verified-book "casebook.verified.unverified"
                                                                  (str "{:description \"Cordons\" :prerequisites (:prerequisites steps/cordon)"
                                                                       " :select (:select steps/cordon) :confirm (:confirm steps/cordon)"
                                                                       " :effect (:effect steps/cordon)}"))})
        run (fn [opts]
              (let [err (java.io.StringWriter.)
                    result (binding [*out* (java.io.StringWriter.) *err* err]
                             (try (conformance/verify! opts) (catch clojure.lang.ExceptionInfo e e)))]
                [result (str err)]))]
    (try
      (let [[e err] (run (roots root))]
        (is (= 1 (:babashka/exit (ex-data e))))
        (is (str/includes? err "runbooks/unverified.clj: mutation spec casebook.verified.unverified has no post-check") err)
        (is (not (str/includes? err "cordon.clj")) err))
      (testing "under :spec-contract :advise it is a note and verify! passes"
        (let [[checked err] (run (assoc (roots root) :spec-contract :advise))]
          (is (= 3 checked))
          (is (str/includes? err "note: ") err)
          (is (str/includes? err "casebook.verified.unverified has no post-check") err)))
      (finally (fs/delete-tree root)))))

(deftest the-explicit-options-of-a-derived-option-map-are-checked
  (let [file (str (fs/create-temp-file {:suffix ".clj"}))]
    (try
      (spit file (str header
                      "(ns casebook.derived \"Derives its select flags.\"\n"
                      "  (:require [gumshoe.mutation :as mutation] [gumshoe.select :as select]))\n"
                      "(def selection {:mode :one :label \"Thing\" :flag :pick})\n"
                      "(mutation/book\n"
                      " {:description \"Derives\"\n"
                      "  :options (select/options selection {:force {:alias :f :colour true}})\n"
                      "  :select selection})\n"))
      (let [messages (map :message (conformance/option-problems [(conformance/parse file)]))]
        (is (some #(str/includes? % "option :force has no :desc") messages))
        (is (some #(str/includes? % "unknown option keys [:colour]") messages))
        (is (not-any? #(str/includes? % ":pick") messages)
            "the derived option is complete and not read statically"))
      (finally (fs/delete file)))))

(defn- options-book
  "A detective book whose options are written by `options-form`."
  [ns-name options-form]
  (str header
       "(ns " ns-name " \"Reads many flags.\"\n"
       "  (:require [gumshoe.detective :as detective] [gumshoe.flags :as flags]))\n"
       "(detective/book\n"
       " {:description \"Many flags\"\n"
       "  :options " options-form "\n"
       "  :detectives [{:name \"ok\" :detect (fn [_] [])}]})\n"))

(def ^:private nine-options
  (str/join " " (for [i (range 1 10)] (format ":o%d {:desc \"Option %d\"}" i i))))

(deftest a-book-whose-options-lost-their-order-is-a-note
  (let [root (str (fs/create-temp-dir))
        books (str (fs/path root "runbooks"))]
    (try
      (fs/create-dirs books)
      (spit (str (fs/path books "literal.clj")) (options-book "casebook.order.literal" (str "{" nine-options "}")))
      (spit (str (fs/path books "ordered.clj")) (options-book "casebook.order.ordered" (str "(flags/ordered " nine-options ")")))
      (spit (str (fs/path books "eight.clj"))
            (options-book "casebook.order.eight" (str "{" (str/join " " (for [i (range 1 9)] (format ":o%d {:desc \"Option %d\"}" i i))) "}")))
      (let [{:keys [findings advisories]} (conformance/report {:books (:books (conformance/files-in {:books [books]}))
                                                               :libraries [] :license "Apache-2.0"})]
        (is (empty? findings) (pr-str findings))
        (is (= [[(str (fs/path books "literal.clj")) :option-order]]
               (vec (for [{:keys [file check]} advisories :when (= :option-order check)] [file check]))))
        (is (some #(str/includes? (:message %) "flags/ordered") advisories)))
      (finally (fs/delete-tree root)))))

(deftest the-options-of-flags-ordered-are-checked
  (let [file (str (fs/create-temp-file {:suffix ".clj"}))]
    (try
      (spit file (options-book "casebook.order.checked"
                               "(flags/merge-ordered (flags/ordered :a {:alias :x} :b {:desc \"b\" :alias :x}) detective/output-option)"))
      (let [messages (map :message (conformance/option-problems [(conformance/parse file)]))]
        (is (some #(str/includes? % "option :a has no :desc") messages))
        (is (some #(str/includes? % "colliding option aliases") messages)))
      (finally (fs/delete file)))))

;; --- the option-order advisory, once per book ---------------------------------------

(defn- option-library
  "A step library whose step carries nine options written as a map literal,
   which reads as a hash map and so has lost their order."
  [ns-name]
  (str header
       "(ns " ns-name " \"A check step carrying many flags.\")\n\n"
       "(def options {" nine-options "})\n\n"
       "(def step {:title \"Checks\" :options options :check (fn [_] true)})\n\n"
       "(def playbook {:steps [step]})\n"))

(defn- option-playbook
  "A playbook in `ns-name` that merges `library`'s playbook and its step once
   per title, and declares `own` as its options when given."
  [ns-name library titles own]
  (str header
       "(ns " ns-name " \"Merges a step carrying options.\"\n"
       "  (:require [" library " :as steps] [gumshoe.playbook :as playbook]))\n\n"
       "(playbook/book\n"
       " (merge steps/playbook\n"
       "        {:description \"Merges\"\n"
       (when own (str "         :options " own "\n"))
       "         :steps [" (str/join " " (for [t titles] (format "(merge steps/step {:title %s})" (pr-str t)))) "]}))\n"))

(defn- option-order-advisories
  "{file name -> number of :option-order advisories} for the casebook at root."
  [root]
  (let [{:keys [advisories]} (conformance/report (merge (roots root) (conformance/files-in (roots root))
                                                        {:license "Apache-2.0"
                                                         :classpath [(str (fs/path root "libraries"))]}))]
    (frequencies (for [{:keys [file check]} advisories :when (= :option-order check)]
                   (str (fs/file-name file))))))

(deftest a-playbook-merging-one-two-or-many-specs-with-options-is-advised-once
  (doseq [[n lib] [[1 "casebook.order.one.steps"] [2 "casebook.order.two.steps"] [7 "casebook.order.many.steps"]]]
    (testing (str n " merged specs")
      (let [root (casebook! {(str "libraries/" (str/replace lib "." "/") ".clj") (option-library lib)
                             "playbooks/adopt.clj" (option-playbook (str lib "-book") lib
                                                                    (map #(str "step " %) (range n)) nil)})]
        (try
          (is (= {"adopt.clj" 1} (option-order-advisories root)))
          (finally (fs/delete-tree root)))))))

(deftest two-books-sharing-one-library-spec-are-each-advised-once
  (let [lib "casebook.order.shared.steps"
        root (casebook! {"libraries/casebook/order/shared/steps.clj" (option-library lib)
                         "playbooks/first.clj" (option-playbook "casebook.order.shared.first" lib ["a" "b"] nil)
                         "playbooks/second.clj" (option-playbook "casebook.order.shared.second" lib ["a" "b" "c"] nil)})]
    (try
      (is (= {"first.clj" 1 "second.clj" 1} (option-order-advisories root)))
      (finally (fs/delete-tree root)))))

(deftest a-book-whose-own-and-merged-option-maps-lost-order-is-advised-once
  (let [lib "casebook.order.both.steps"
        own (str "{" (str/join " " (for [i (range 1 11)] (format ":own%d {:desc \"Own %d\"}" i i))) "}")
        root (casebook! {"libraries/casebook/order/both/steps.clj" (option-library lib)
                         "playbooks/both.clj" (option-playbook "casebook.order.both.book" lib ["a"] own)})]
    (try
      (is (= {"both.clj" 1} (option-order-advisories root)))
      (let [{:keys [advisories]} (conformance/report (merge (roots root) (conformance/files-in (roots root))
                                                            {:license "Apache-2.0"}))
            [{:keys [message]}] (filter #(= :option-order (:check %)) advisories)]
        (is (str/includes? message "2 of its option maps (10 and 9 options)") message)
        (is (str/includes? message "flags/ordered") message))
      (finally (fs/delete-tree root)))))

(deftest option-order-findings-name-each-book-once
  (let [lost (into {} (for [i (range 9)] [(keyword (str "o" i)) {:desc "o"}]))
        lost-ten (assoc lost :o9 {:desc "o"})
        ordered (apply array-map (mapcat (fn [[k v]] [k v]) lost))]
    (is (gumshoe.flags/order-lost? lost))
    (is (= [] (vec (conformance/option-order-findings [{:file "a.clj" :option-maps [ordered ordered]}]))))
    (is (= ["a.clj"] (mapv :file (conformance/option-order-findings [{:file "a.clj" :option-maps [lost]}]))))
    (is (= ["a.clj"] (mapv :file (conformance/option-order-findings [{:file "a.clj" :option-maps [lost ordered lost]}]))))
    (is (= ["a.clj"] (mapv :file (conformance/option-order-findings [{:file "a.clj" :option-maps (repeat 12 lost)}]))))
    (is (= ["a.clj" "b.clj"] (mapv :file (conformance/option-order-findings [{:file "a.clj" :option-maps [lost lost]}
                                                                            {:file "b.clj" :option-maps [lost]}
                                                                            {:file "a.clj" :option-maps [lost-ten]}]))))
    (testing "the message names the options of a single map, and the sizes of several distinct maps"
      (is (str/includes? (:message (first (conformance/option-order-findings [{:file "a.clj" :option-maps [lost lost]}])))
                         "lists its 9 options out of order"))
      (is (str/includes? (:message (first (conformance/option-order-findings [{:file "a.clj" :option-maps [lost lost-ten]}])))
                         "2 of its option maps (9 and 10 options)"))
      (is (str/includes? (:message (first (conformance/option-order-findings
                                           [{:file "a.clj" :option-maps [lost lost-ten lost (assoc lost-ten :o10 {:desc "o"})]}])))
                         "3 of its option maps (9, 10 and 11 options)")))
    (is (= [] (vec (conformance/option-order-findings [{:file "a.edn" :option-maps [lost]}]))))))

(deftest verify-prints-the-option-order-count-as-the-number-of-books-advised
  (let [lib "casebook.order.count.steps"
        root (casebook! {"libraries/casebook/order/count/steps.clj" (option-library lib)
                         "playbooks/first.clj" (option-playbook "casebook.order.count.first" lib ["a" "b" "c"] nil)
                         "playbooks/second.clj" (option-playbook "casebook.order.count.second" lib ["a" "b"] nil)})
        err (java.io.StringWriter.)]
    (try
      (binding [*out* (java.io.StringWriter.) *err* err]
        (conformance/verify! (assoc (roots root) :license "Apache-2.0")))
      (let [err (str err)
            notes (filter #(str/includes? % "(option-order)") (str/split-lines err))]
        (is (= 2 (count notes)) err)
        (is (str/includes? err "note: 2 option-order advisories in 2 file(s), not gating") err))
      (finally (fs/delete-tree root)))))
