;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.runbook-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gumshoe.config :as config]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.runbook :as runbook]))

;; The prerequisites registry is a process-wide atom; reset it around each test.
(use-fixtures :each (fn [t]
                      (reset! @#'prerequisites/checks {})
                      (t)
                      (reset! @#'prerequisites/checks {})))

(def ^:private unknown-items #'runbook/unknown-prerequisite-items)
(def ^:private book-heading #'runbook/book-heading)
(def ^:private can-i-item #'runbook/can-i-item)

(deftest book-heading-test
  (testing "the kind comes from the book's path segment, not a fixed 'Runbook'"
    (is (= "📋 Playbook · node maintenance"
           (book-heading "/x/playbooks/kubernetes/node_maintenance.clj" "node maintenance")))
    (is (= "🔥 Firebook · simulate an ImagePullBackOff"
           (book-heading "/x/firebooks/kubernetes/image_pull.clj" "simulate an ImagePullBackOff")))
    (is (= "🚀 Runbook · cordon a node"
           (book-heading "/x/runbooks/kubernetes/nodes/cordon.clj" "cordon a node")))
    (is (= "📋 Playbook · ceph upgrade"
           (book-heading "/x/tools/ceph/playbooks/upgrade.clj" "ceph upgrade"))
        "a tool package's playbook is still a Playbook"))
  (testing "a blank or missing description leaves just the kind; a missing path is a runbook"
    (is (= "🔥 Firebook" (book-heading "/x/firebooks/a.clj" nil)))
    (is (= "🚀 Runbook" (book-heading nil "")))))
(def ^:private cluster-item #'runbook/cluster-item)

(defn- run-thunk [[_heading thunk]] (thunk))

(deftest kubectl-permission-label-test
  (testing "a resource the cluster knows renders as its Kind, verb upper-cased and Kubernetes-scoped"
    (with-redefs [kubectl/resource-kind {"pods" "Pod"}
                  kubectl/can-i? (constantly true)]
      (let [[heading _] (can-i-item "get" "pods")]
        (is (= "Kubernetes: can GET Pod resources" heading))
        (is (= {:ok? true :label "Kubernetes: can GET Pod resources"}
               (run-thunk (can-i-item "get" "pods"))))))
    (with-redefs [kubectl/resource-kind {"nodes" "Node"}
                  kubectl/can-i? (constantly false)]
      (is (= {:ok? false :label "Kubernetes: cannot PATCH Node resources"}
             (run-thunk (can-i-item "patch" "nodes"))))))
  (testing "an unknown or unreachable resource falls back to the verbatim name, never a fabricated Kind"
    (with-redefs [kubectl/resource-kind (constantly nil)
                  kubectl/can-i? (constantly true)]
      (is (= "Kubernetes: can GET helmreleases.helm.toolkit.fluxcd.io"
             (first (can-i-item "get" "helmreleases.helm.toolkit.fluxcd.io")))))))

(deftest cluster-label-test
  (testing "no context: names Kubernetes explicitly"
    (with-redefs [kubectl/current-cluster (constantly "")]
      (is (= {:ok? false :label "not connected to any Kubernetes cluster"}
             (run-thunk (cluster-item []))))))
  (testing "a suitable cluster: 'Kubernetes cluster: <name>'"
    (with-redefs [kubectl/current-cluster (constantly "kube.example.org")
                  config/known-clusters (constantly [])
                  config/env-value (constantly nil)]
      (is (= "connected to a suitable Kubernetes cluster" (first (cluster-item []))))
      (is (= {:ok? true :label "Kubernetes cluster: kube.example.org"}
             (run-thunk (cluster-item [])))))))

(deftest unknown-prerequisite-fails-closed-test
  (testing "a built-in prerequisite key is not flagged as unknown"
    (is (empty? (unknown-items {:installed-tools ["kubectl"]
                                :kubectl-can-get ["pods"]
                                :can-ping-using-ipv4 [:host]
                                :existing-paths [:checkout {:option :out :writable true}]}))))
  (testing "a plugin-registered key is not flagged as unknown"
    (prerequisites/register-check! :change-window (fn [_ _] []))
    (is (empty? (unknown-items {:change-window "prod"}))))
  (testing "a declared key nothing handles becomes a fail-closed checklist item"
    (let [items (unknown-items {:no-such-prerequisite "x"})]
      (is (= 1 (count items)) "no built-in or plugin handles this key, so it is unknown")
      (is (false? (:ok? ((second (first items)))))
          "the unknown key blocks the book instead of being silently dropped"))))

(deftest a-book-loaded-for-checking-is-not-run
  (let [ran (atom false)
        config {:description "Would run" :action (fn [_ _] (reset! ran true) true)}]
    (is (= config (binding [runbook/*loading* true] (runbook/execute! config)))
        "execute! hands back the config it was given")
    (is (false? @ran))))

(defn- absolute-classpath
  "This run's classpath with every entry absolute, so a bb started in another
   directory loads the same libraries."
  []
  (->> (str/split (System/getProperty "java.class.path") #":")
       (map #(str (fs/absolutize %)))
       (str/join ":")))

(defn- run-book
  "Runs a book declaring an :existing-paths prerequisite in a fresh bb, from a
   scratch directory with no env.edn, with the given arguments."
  [dir & args]
  (let [script (str "(require '[gumshoe.runbook :as runbook])"
                    "(binding [*command-line-args* " (pr-str (vec args)) "]"
                    "  (runbook/execute! {:description \"needs a checkout\" :announce? false"
                    "                     :options {:checkout {:desc \"the checkout\"}}"
                    "                     :prerequisites {:existing-paths [:checkout]}"
                    "                     :action (fn [_ _] (println \"ACTION RAN\") true)}))")]
    @(process/process {:dir (str dir) :out :string :err :string :continue true
                       :extra-env {"GUMSHOE_ENV" (str (fs/path dir "env.edn")) "NO_COLOR" "1"}}
                      "bb" "-cp" (absolute-classpath) "-e" script)))

(deftest a-missing-directory-stops-the-book-in-the-prerequisites-test
  (fs/with-temp-dir [dir {}]
    (let [missing (str (fs/path dir "nonexistent"))
          {:keys [exit out err]} (run-book dir "--checkout" missing)
          output (str out err)]
      (is (= 1 exit))
      (is (str/includes? output "Prerequisites"))
      (is (str/includes? output (str "directory --checkout: " missing " does not exist")) output)
      (is (str/includes? output "PREREQUISITES NOT MET"))
      (is (not (str/includes? output "ACTION RAN")) "the action never runs")
      (is (not (str/includes? output "unexpected error"))))))

(deftest an-existing-directory-lets-the-book-run-test
  (fs/with-temp-dir [dir {}]
    (let [{:keys [exit out err]} (run-book dir "--checkout" (str dir))]
      (is (zero? exit) (str out err))
      (is (str/includes? out "ACTION RAN")))))

(defn- run-named-book
  "Runs a book with a --name option in a fresh bb, from a scratch directory
   with no env.edn, with the given arguments."
  [dir & args]
  (let [script (str "(require '[gumshoe.runbook :as runbook])"
                    "(binding [*command-line-args* " (pr-str (vec args)) "]"
                    "  (runbook/execute! {:description \"names a release\" :announce? false"
                    "                     :options {:name {:desc \"the release\" :alias :N :coerce :string}}"
                    "                     :action (fn [opts _] (println (str \"NAME=\" (:name opts))) true)}))")]
    @(process/process {:dir (str dir) :out :string :err :string :continue true
                       :extra-env {"GUMSHOE_ENV" (str (fs/path dir "env.edn")) "NO_COLOR" "1"}}
                      "bb" "-cp" (absolute-classpath) "-e" script)))

(deftest a-deprecated-alias-runs-the-book-with-a-note-on-stderr-test
  (fs/with-temp-dir [dir {}]
    (let [{:keys [exit out err]} (run-named-book dir "-s" "web")]
      (is (zero? exit) (str out err))
      (is (str/includes? out "NAME=web") "the deprecated alias still reaches --name")
      (is (str/includes? err "-s is a deprecated alias of --name; use -N") err)
      (is (not (str/includes? out "deprecated")) "the note stays off stdout"))))

(deftest the-current-alias-runs-the-book-without-a-note-test
  (fs/with-temp-dir [dir {}]
    (let [{:keys [exit out err]} (run-named-book dir "-N" "web")]
      (is (zero? exit) (str out err))
      (is (str/includes? out "NAME=web"))
      (is (not (str/includes? (str out err) "deprecated"))))))

(deftest arguments-test
  (let [options {:name {:desc "the release" :alias :N :coerce :string}}]
    (testing "the current alias and the long flag parse without a note"
      (is (= {:opts {:name "web"} :notes []} (runbook/arguments options ["-N" "web"])))
      (is (= {:opts {:name "web"} :notes []} (runbook/arguments options ["--name" "web"]))))
    (testing "a deprecated alias parses to its option and names its successor"
      (is (= {:opts {:name "web"} :notes ["-r is a deprecated alias of --name; use -N"]}
             (runbook/arguments options ["-r" "web"]))))
    (testing "an option that claims a deprecated letter keeps it"
      (is (= {:opts {:name "web" :release "r1"} :notes []}
             (runbook/arguments (assoc options :release {:desc "r" :alias :r :coerce :string})
                                ["--name" "web" "-r" "r1"]))))
    (testing "a book without the option refuses the letter"
      (is (thrown-with-msg? Exception #"Unknown option: -s"
                            (runbook/arguments {:host {:desc "h" :coerce :string}} ["-s" "web"]))))))

(deftest help-lists-the-deprecated-aliases-test
  (let [text (runbook/help {:description "names a release"
                            :options {:name {:desc "The release" :alias :N :coerce :string}}})]
    (is (str/includes? text "-N, --name"))
    (is (str/includes? text "The release (deprecated alias: -r, -s)"))))
