;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.config-docs-test
  "env.edn.example and the README name the environment variables and files
   gumshoe.config reads. These tests read both documents and the code, so a
   rename on either side fails the gate instead of leaving the operator a
   file nobody reads or an override that is ignored."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private documents ["env.edn.example" "README.md"])

(def ^:private config-source "libraries/gumshoe/config.clj")

(def ^:private briko-reads
  "The XDG variables a briko.paths function reads on gumshoe's behalf, by the
   call that names it."
  {"paths/config-home" "XDG_CONFIG_HOME"
   "paths/state-home" "XDG_STATE_HOME"})

(defn- read-by
  "The environment variables a source file reads with System/getenv, or
   through a briko.paths function."
  [file]
  (let [text (slurp (str file))]
    (into (set (map second (re-seq #"System/getenv\s+\"([A-Za-z0-9_]+)\"" text)))
          (keep (fn [[call variable]] (when (str/includes? text call) variable)))
          briko-reads)))

(defn- read-by-libraries
  []
  (apply set/union (map read-by (fs/glob "." "{libraries,tools/*/libraries}/**.clj"))))

(defn- env-var-names
  "Every token of text shaped like an environment variable name: upper case,
   with at least one underscore."
  [text]
  (set (re-seq #"\b[A-Z][A-Z0-9]*(?:_[A-Z0-9]+)+\b" text)))

(defn- env-edn-paths
  "Every path to an env.edn file named in text: one starting with ./, ~ or a
   $VARIABLE."
  [text]
  (set (re-seq #"(?<![A-Za-z0-9_/.-])[~$.][A-Za-z0-9_./-]*/env\.edn" text)))

(defn- search-paths-with
  "The search paths gumshoe.config computes in a fresh bb process with
   GUMSHOE_ENV, XDG_CONFIG_HOME, HOME and user.home set to known markers, so a
   documented path can be compared with the one the code builds."
  []
  (let [classpath (str/join ":" (map #(str (fs/absolutize %))
                                     (str/split (System/getProperty "java.class.path") #":")))
        {:keys [out]} (process/shell {:out :string :err :string
                                      :extra-env {"GUMSHOE_ENV" "/marker-env/env.edn"
                                                  "XDG_CONFIG_HOME" "/marker-xdg"
                                                  "HOME" "/marker-home"}}
                                     "bb" "-Duser.home=/marker-home" "-cp" classpath "-e"
                                     "(require 'gumshoe.config) (prn (#'gumshoe.config/search-paths))")]
    (edn/read-string out)))

(defn- as-searched
  "A documented path written the way the marker run computes it."
  [path]
  (-> path
      (str/replace #"^\$XDG_CONFIG_HOME" "/marker-xdg")
      (str/replace #"^~" "/marker-home")
      (str/replace #"^\./" "")))

(deftest every-documented-gumshoe-variable-is-read-by-the-config-test
  (let [config (read-by config-source)]
    (is (contains? config "GUMSHOE_ENV"))
    (is (contains? config "GUMSHOE_ENVIRONMENT"))
    (doseq [document documents
            name (env-var-names (slurp document))
            :when (str/starts-with? name "GUMSHOE_ENV")]
      (is (contains? config name)
          (str document " names " name ", which " config-source " never reads")))))

(deftest every-variable-env-edn-example-names-is-read-test
  (let [libraries (read-by-libraries)]
    (doseq [name (env-var-names (slurp "env.edn.example"))]
      (is (contains? libraries name)
          (str "env.edn.example names " name ", which no library reads")))))

(deftest every-variable-the-config-reads-is-documented-test
  (doseq [document documents
          :let [named (env-var-names (slurp document))]
          name (read-by config-source)]
    (is (contains? named name) (str config-source " reads " name ", which " document " does not name"))))

(deftest documented-env-edn-paths-are-the-searched-ones-test
  (let [searched (search-paths-with)]
    (testing "the marker run computed the search order"
      (is (= "/marker-env/env.edn" (first searched)) (pr-str searched)))
    (doseq [document documents
            :let [documented (env-edn-paths (slurp document))]]
      (testing document
        (doseq [path documented]
          (is (some #{(as-searched path)} searched)
              (str document " names " path ", which gumshoe.config does not search")))
        (doseq [path (rest searched)]
          (is (some #(= path (as-searched %)) documented)
              (str "gumshoe.config searches " path ", which " document " does not name")))))))
