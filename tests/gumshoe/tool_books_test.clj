;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tool-books-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn- required-namespaces
  [source]
  (let [ns-form (read-string source)]
    (set (for [clause (rest ns-form)
               :when (and (seq? clause) (= :require (first clause)))
               spec (rest clause)]
           (if (vector? spec) (first spec) spec)))))

(defn- source-of
  "The file of a tool namespace, gumshoe.tools.gateway ->
   tools/*/libraries/gumshoe/tools/gateway.clj."
  [ns-sym]
  (let [file (str (str/replace (munge (last (str/split (str ns-sym) #"\."))) "." "/") ".clj")]
    (first (fs/glob "tools" (str "*/libraries/gumshoe/tools/" file)))))

(deftest a-scope-book-loads-the-plugin-that-fills-its-scope
  ;; the registry is global, so a book that leaves its plugin out still works
  ;; whenever env.edn or an earlier test loaded it - read the sources instead
  (doseq [book (fs/glob "tools" "*/runbooks/**.clj")
          :let [source (slurp (str book))
                scope (second (re-find #":scope (:[a-z-]+)" source))]
          :when scope]
    (is (some (fn [ns-sym]
                (when-let [f (source-of ns-sym)]
                  (str/includes? (slurp (str f)) (str ":detectives {" scope))))
              (filter #(str/starts-with? (str %) "gumshoe.tools.") (required-namespaces source)))
        (str book " runs scope " scope " without requiring the tool namespace that fills it"))))
