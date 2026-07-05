;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.recording
  "Every run of a book leaves a timestamped record under recordings/ (which is
   gitignored): the input variables it was given, the selections the operator
   made, the exact CLI calls, and the outcome. This is the audit trail for
   'what happened, with what inputs' - especially for changes.

   Secret-looking option values never touch disk: they are redacted here."
  (:require [babashka.fs :as fs]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [infra.interact :as interact]
            [infra.shell :as shell]))

(def base-directory "recordings")

(defn- timestamp
  []
  (.format (java.time.LocalDateTime/now)
           (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss")))

(defn book-slug
  "A recording sub-path from a book's file path, robust to absolute paths:
   .../runbooks/x/y.clj -> x/y."
  [book-file]
  (if-let [m (re-find #"(?:runbooks|playbooks|firebooks)/(.+?)\.clj$" (str book-file))]
    (second m)
    (-> (str book-file) (str/replace #"\.clj$" "") (str/replace #".*/" ""))))

(defn secret-key?
  [k]
  (boolean (re-find #"token|password|secret" (str/lower-case (name k)))))

(defn redact
  "Replaces secret-looking option values so nothing sensitive is written."
  [opts]
  (reduce-kv (fn [m k v] (assoc m k (if (secret-key? k) "<redacted>" v)))
             {}
             opts))

(defn save!
  "Writes the recording directory for one run and returns its path. Never
   throws - a failed recording must not fail the book."
  [{:keys [book-file description opts outcome meta]}]
  (try
    (let [dir (fs/path base-directory (book-slug book-file) (timestamp))]
      (fs/create-dirs dir)
      (spit (str (fs/path dir "meta.edn"))
            (with-out-str (pprint/pprint (merge {:book (book-slug book-file)
                                                 :description description
                                                 :outcome outcome}
                                                meta))))
      (spit (str (fs/path dir "input.edn"))
            (with-out-str (pprint/pprint (redact opts))))
      (spit (str (fs/path dir "selections.edn"))
            (with-out-str (pprint/pprint (interact/recorded-selections))))
      (spit (str (fs/path dir "commands.sh"))
            (str "#!/usr/bin/env sh\n"
                 "# CLI calls made by this run - review before replaying.\n\n"
                 (str/join "\n" (shell/recording))
                 "\n"))
      (str dir))
    (catch Exception _ nil)))
