;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.select-flags-test
  "The select flag convention: the aliases gumshoe.flags decides, the
   options gumshoe.select derives from a select, and that every book gumshoe
   ships follows both."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.book-surfaces-test :as surfaces]
            [gumshoe.conformance :as conformance]
            [gumshoe.flags :as flags]
            [gumshoe.mutation :as mutation]
            [gumshoe.playbook :as playbook]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]))

;; --- gumshoe.flags ------------------------------------------------------------------

(deftest deprecated-aliases-test
  (testing "an option takes its deprecated aliases"
    (is (= {:r :name :s :name} (flags/deprecated-aliases {:name {:alias :N}})))
    (is (= {:m :service} (flags/deprecated-aliases {:service {:alias :s}}))))
  (testing "a letter another option declares stays with that option"
    (is (= {:s :name} (flags/deprecated-aliases {:name {:alias :N} :release {:alias :r}})))
    (is (= {} (flags/deprecated-aliases {:service {:alias :s} :metric {:alias :m}}))))
  (testing "a book without the option takes none"
    (is (= {} (flags/deprecated-aliases {:host {:alias :H}})))
    (is (= {} (flags/deprecated-aliases {})))))

(deftest documented-test
  (testing "the option names the deprecated aliases it takes"
    (is (= "The name (deprecated alias: -r, -s)"
           (get-in (flags/documented {:name {:desc "The name" :alias :N}}) [:name :desc])))
    (is (= "The name (deprecated alias: -s)"
           (get-in (flags/documented {:name {:desc "The name"} :release {:alias :r}}) [:name :desc]))))
  (testing "other options are left alone"
    (is (= {:host {:desc "h"}} (flags/documented {:host {:desc "h"}})))))

(deftest notes-test
  (let [options {:name {:desc "n" :alias :N} :namespace {:desc "ns" :alias :n}}]
    (testing "one note per deprecated alias used, naming the current alias"
      (is (= ["-s is a deprecated alias of --name; use -N"] (flags/notes options ["-n" "shop" "-s" "web"])))
      (is (= ["-s is a deprecated alias of --name; use -N"] (flags/notes options ["-s" "web" "-s" "db"]))
          "a letter used twice is noted once")
      (is (= ["-r is a deprecated alias of --name; use -N"] (flags/notes options ["-rweb"]))
          "the value can follow the letter directly"))
    (testing "no note for the current alias, the long flag or values after --"
      (is (= [] (flags/notes options ["-N" "web" "--name" "web"])))
      (is (= [] (flags/notes options ["--" "-s"]))))
    (testing "an option without an alias is named by its long flag"
      (is (= ["-s is a deprecated alias of --name; use --name"] (flags/notes {:name {:desc "n"}} ["-s" "web"]))))
    (testing "a letter another option claims is that option's, not a deprecated alias"
      (is (= [] (flags/notes (assoc options :server {:alias :s}) ["-s" "web"]))))))

;; --- gumshoe.select/options ---------------------------------------------------------

(deftest derived-options-test
  (testing "a :namespaced select reads two flags, described from its label"
    (is (= {:namespace {:desc "The namespace of the StageSet - interactive selection when omitted"
                        :alias :n :coerce :string}
            :name {:desc "The name of the StageSet - interactive selection when omitted"
                   :alias :N :coerce :string}}
           (select/options {:mode :namespaced :label "StageSet" :namespace-flag :namespace :name-flag :name}))))
  (testing "a :one select reads one string flag"
    (is (= {:node {:desc "The Node - interactive selection when omitted" :alias :n :coerce :string}}
           (select/options {:mode :one :label "Node" :flag :node}))))
  (testing "a :many select reads a repeatable flag"
    (is (= {:volumes {:desc "PersistentVolume to pick, repeatable - interactive selection when omitted"
                      :alias :v :coerce [:string]}}
           (select/options {:mode :many :label "PersistentVolume" :flag :volumes}))))
  (testing "a flag the convention does not name gets no alias"
    (is (= {:widget {:desc "The Widget - interactive selection when omitted" :coerce :string}}
           (select/options {:mode :one :label "Widget" :flag :widget})))))

(deftest explicit-options-test
  (let [selection {:mode :one :label "Workload" :flag :pick}]
    (testing "an explicit option is merged key by key, so a reworded flag keeps its alias"
      (is (= {:pick {:desc "\"<Kind> <namespace>/<name>\"" :alias :p :coerce :string}}
             (select/options selection {:pick {:desc "\"<Kind> <namespace>/<name>\""}}))))
    (testing "the book's own options join the derived ones"
      (is (= {:desc "The replica count" :alias :r :coerce :long}
             (:replicas (select/options selection {:replicas {:desc "The replica count" :alias :r :coerce :long}})))))
    (testing "two options with one alias are refused"
      (is (thrown-with-msg? Exception #"options share an alias: -p for --pick and --port"
                            (select/options selection {:port {:desc "The port" :alias :p}}))))
    (testing "deriving twice changes nothing"
      (let [once (select/options selection {:pick {:desc "x"}})]
        (is (= once (select/options selection once)))))))

;; --- every book gumshoe ships -------------------------------------------------------

(def ^:private modes #{:one :many :namespaced})

(defn- declared-selects
  "Every literal select map in a file: a map with a :mode the selects take."
  [file]
  (let [{:keys [forms]} (conformance/parse file)]
    (filter #(and (map? %) (modes (:mode %))) (tree-seq coll? seq forms))))

(defn- select-flags
  [selection]
  (keep selection [:flag :namespace-flag :name-flag]))

(def ^:private library-files
  (conformance/clj-files (cons "libraries"
                               (for [tool (.listFiles (java.io.File. "tools"))]
                                 (str tool "/libraries")))))

(deftest every-select-flag-has-a-conventional-alias
  (doseq [file (concat (surfaces/book-files) library-files)
          :when (not (re-find #"^tools/ui-charm/" file))
          selection (declared-selects file)
          flag (select-flags selection)]
    (testing (str file " " flag)
      (is (contains? flags/aliases flag)
          "a new select flag gets its alias in gumshoe.flags/aliases"))))

(defn- load-book
  "The book's config and the specs it handed mutation/book and playbook/book."
  [file]
  (let [specs (atom [])
        capture (fn [original] (fn [spec] (swap! specs conj spec) (original spec)))
        config (with-redefs [mutation/book (capture mutation/book)
                             playbook/book (capture playbook/book)]
                 (binding [runbook/*loading* true] (load-file file)))]
    {:config config :specs @specs}))

(defn- spec-selects
  [spec]
  (keep :select (cons spec (:steps spec))))

(deftest every-book-gives-its-select-flags-the-conventional-alias
  (doseq [file (surfaces/book-files)
          :let [{:keys [config specs]} (load-book file)
                options (:options config)]
          selection (concat (declared-selects file) (mapcat spec-selects specs))
          flag (select-flags selection)]
    (testing (str file " " flag)
      (is (= (flags/aliases flag) (get-in options [flag :alias]))))))

(deftest no-book-gives-one-alias-to-two-options
  (doseq [file (surfaces/book-files)
          :let [options (:options (:config (load-book file)))
                aliases (keep :alias (vals (merge options runbook/standard-options)))]]
    (testing file
      (is (= (count aliases) (count (set aliases)))
          (str "aliases " (vec (sort aliases)))))))

(deftest no-book-declares-a-deprecated-alias-as-its-own
  (doseq [file (surfaces/book-files)
          :let [options (:options (:config (load-book file)))]
          [option letters] flags/deprecated]
    (testing (str file " " option)
      (is (not (contains? (set letters) (get-in options [option :alias])))))))

;; --- composition across tools -------------------------------------------------------

(defn- spec-of
  [file]
  (with-redefs [mutation/book identity]
    (binding [runbook/*loading* true] (load-file file))))

(deftest steps-from-different-tools-join-in-one-playbook
  (let [steps (map spec-of ["tools/jaas/runbooks/jaas/suspend.clj"
                            "tools/flux/runbooks/flux/reconcile.clj"
                            "tools/stageset/runbooks/stageset/reconcile.clj"])
        options (playbook/all-options {:steps steps})]
    (is (= :N (get-in options [:name :alias])) "every step reads --name as -N")
    (is (= :n (get-in options [:namespace :alias])))
    (is (empty? (set/intersection #{:r :s} (set (keep :alias (vals options)))))
        "the deprecated letters stay free for the playbook's own options")))
