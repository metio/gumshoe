;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.conformance-glyphs-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.conformance :as conformance]))

(def ^:private header
  (str ";; SPDX-" "FileCopyrightText: The casebook Authors\n"
       ";; SPDX-" "License-Identifier: 0BSD\n\n"))

(defn- casebook!
  "A casebook in a temporary directory: {relative path -> content}."
  [files]
  (let [root (fs/create-temp-dir {:prefix "glyphs"})]
    (doseq [[path content] files
            :let [file (fs/path root path)]]
      (fs/create-dirs (fs/parent file))
      (spit (str file) content))
    (str root)))

(def ^:private hand-written
  (str header
       "(ns casebook.status\n"
       "  \"Reports adoption progress with ✓ and ✗ by hand.\"\n"
       "  (:require [gumshoe.stdout :as stdout]))\n\n"
       "(defn report [ok? label]\n"
       "  (if ok?\n"
       "    (println (str \"✓ \" label))\n"
       "    (stdout/err-println (str \"  \" (stdout/red \"✗\") \" \" label))))\n\n"
       "(defn heading [] (stdout/print-section \"⏳ Terminating namespaces\"))\n"))

(def ^:private through-the-theme
  (str header
       "(ns casebook.themed\n"
       "  \"Reports through the surface; ✓ in a docstring is prose.\"\n"
       "  (:require [gumshoe.stdout :as stdout]\n"
       "            [gumshoe.theme :as theme]))\n\n"
       "(def menu-label \"✓ done - stop here\")\n\n"
       "(defn report [ok? label]\n"
       "  \"Prints ✓ or ⬜ through the theme.\"\n"
       "  (stdout/line (if ok? :done :not-yet) label)\n"
       "  (stdout/print-section (str (theme/glyph :in-progress) \" Waiting\"))\n"
       "  (println \"🔥 a firebook's fire is prose, not an outcome\"))\n"))

(def ^:private theme-plugin
  (str header
       "(ns gumshoe.theme\n"
       "  \"Stands in for the theme namespace, the one place glyphs belong.\")\n\n"
       "(defn shown [] (println \"✅ ❌ 🔶\"))\n"))

(defn- parsed [root & paths]
  (mapv #(conformance/parse (str (fs/path root %))) paths))

(deftest a-glyph-printed-by-hand-is-named-once-per-call-with-its-line
  (let [root (casebook! {"libraries/casebook/status.clj" hand-written})]
    (try
      (let [found (conformance/hand-written-glyphs (parsed root "libraries/casebook/status.clj"))]
        (is (= 3 (count found)) (str/join "\n" (map conformance/format-finding found)))
        (is (every? #(= :glyph (:check %)) found))
        (is (= ["line 10 prints ✓ by hand in (println ...)"
                "line 11 prints ✗ by hand in (stdout/err-println ...)"
                "line 13 prints ⏳ by hand in (stdout/print-section ...)"]
               (map #(first (str/split (:message %) #";")) found))
            "the outermost printing call is named, the nested red is not counted again"))
      (finally (fs/delete-tree root)))))

(deftest output-through-the-theme-and-unprinted-glyphs-pass
  (let [root (casebook! {"libraries/casebook/themed.clj" through-the-theme
                         "libraries/gumshoe/theme.clj" theme-plugin
                         "libraries/casebook/tests/status_test.clj" hand-written})]
    (try
      (is (empty? (conformance/hand-written-glyphs
                   (parsed root "libraries/casebook/themed.clj" "libraries/gumshoe/theme.clj"
                           "libraries/casebook/tests/status_test.clj")))
          "the surface, a docstring, a menu label, a severity glyph, the theme namespace and a test file are all fine")
      (finally (fs/delete-tree root)))))

(deftest the-glyph-set-holds-the-built-in-outcome-marks-and-no-ascii
  (let [glyphs (conformance/default-glyphs)]
    (is (every? glyphs ["✅" "❌" "🔶" "▸" "⬜" "⏳" "⏭" "✓" "✗" "✔" "✘" "⚠"]))
    (is (not-any? #(re-matches #"[\x00-\x7F]*" %) glyphs) "[OK] and + are ordinary text")
    (is (not-any? glyphs ["🔥" "💡" "🔴"]) "severities and markers are not outcomes")))

(deftest a-hand-written-glyph-is-an-advisory-unless-the-caller-requires-the-theme
  (let [root (casebook! {"libraries/casebook/status.clj" hand-written})
        opts {:books [(str (fs/path root "runbooks"))]
              :libraries [(str (fs/path root "libraries"))]
              :license "0BSD" :load? false}]
    (try
      (testing "by default it is a note and the gate passes"
        (let [{:keys [findings advisories]} (conformance/report-in opts)]
          (is (empty? (filter #(= :glyph (:check %)) findings)))
          (is (= 3 (count (filter #(= :glyph (:check %)) advisories)))))
        (let [err (java.io.StringWriter.)]
          (is (= 1 (binding [*out* (java.io.StringWriter.) *err* err] (conformance/verify! opts))))
          (is (str/includes? (str err) "note: 3 glyph advisories in 1 file(s), not gating") (str err))))
      (testing "with :glyphs :require it fails the gate"
        (let [{:keys [findings advisories]} (conformance/report-in (assoc opts :glyphs :require))]
          (is (= 3 (count (filter #(= :glyph (:check %)) findings))))
          (is (empty? (filter #(= :glyph (:check %)) advisories))))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"3 conformance finding"
                              (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
                                (conformance/verify! (assoc opts :glyphs :require))))))
      (finally (fs/delete-tree root)))))
