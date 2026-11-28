;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.report-escapes-test
  "json and edn reports write every hidden code point (control, format,
   separator, lone surrogate, unassigned, default-ignorable) as \\uXXXX, in a
   book's :extra as much as anywhere, so evidence reaches a reader whole and
   nothing in it reaches a terminal or an agent raw."
  (:require [briko.json :as briko-json]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.detective :as detective]
            [gumshoe.render :as render]))

(defn- quietly
  "The value of (f) with stdout and stderr captured, as [value out err]."
  [f]
  (let [out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        value (binding [*out* out *err* err] (f))]
    [value (str out) (str err)]))

(defn- code-point-string [& cps]
  (let [sb (StringBuilder.)]
    (doseq [cp cps] (.appendCodePoint sb (int cp)))
    (str sb)))

(def ^:private hostile-parts
  "One sample of every class hidden? names: C0 (ESC with a screen clear, BEL,
   NUL), DEL, C1 (the 8-bit CSI, NEL), the bidirectional embeddings, overrides,
   isolates and marks, the Arabic letter mark, zero-width space, joiners and
   word joiner, the byte order mark, the soft hyphen, line and paragraph
   separators, a variation selector, a Hangul filler, a tag character, a lone
   high and a lone low surrogate, an unassigned code point and a noncharacter."
  ["\u001b[2J" "\u0007" "\u0000" "\u007f" "\u009b" "\u0085"
   "\u202a" "\u202b" "\u202c" "\u202d" "\u202e"
   "\u2066" "\u2067" "\u2068" "\u2069" "\u200e" "\u200f" "\u061c"
   "\u200b" "\u200c" "\u200d" "\u2060" "\ufeff" "\u00ad"
   "\u2028" "\u2029" "\ufe0f" "\u3164"
   (code-point-string 0xE0041)
   "\ud800" "\udc00" "\u0378" "\uffff"])

(def ^:private hostile
  (str "evil " (str/join "x" hostile-parts) " end"))

(def ^:private readable
  "Non-ASCII text that is not hidden and stays readable on the wire."
  "Größe 日本 café ✓")

(def ^:private extra
  "A book's :extra with hidden code points in values, nested values, string
   keys, keyword and symbol keys and a character."
  {:rows [{:name hostile :labels {hostile hostile}}
          {:name readable :note (str readable hostile)}]
   :nested {:deeper [#{hostile} (list hostile)]}
   hostile [hostile]
   (keyword (str "key " hostile)) "keyword key"
   :symbol (symbol hostile)
   :char (char 0x9b)})

(defn- raw-hidden
  "The hidden code points in printed outside the whitespace that separates
   JSON or EDN values, as hex strings."
  [^String printed]
  (->> (.toArray (.codePoints printed))
       (filter render/hidden?)
       (remove #{9 10 13})
       (map #(format "U+%04X" %))
       distinct
       vec))

(defn- report-output [format]
  (second (quietly #(detective/report! [] [] format extra))))

(deftest every-sample-is-hidden
  (doseq [part hostile-parts
          :let [cps (.toArray (.codePoints ^String part))]]
    (is (some render/hidden? cps) (pr-str part)))
  (testing "readable non-ASCII text is not hidden"
    (is (not-any? render/hidden? (.toArray (.codePoints ^String readable))))))

(deftest json-report-writes-no-hidden-code-point-and-reads-back-whole
  (let [out (report-output "json")
        read (json/parse-string out)]
    (is (= [] (raw-hidden out)))
    (testing "the bytes on the wire hold none either: a lone surrogate is escaped, not replaced"
      (is (= out (String. (.getBytes ^String out "UTF-8") "UTF-8"))))
    (testing "a JSON reader gets back exactly what cheshire would have encoded"
      (is (= (json/parse-string (json/generate-string extra))
             (select-keys read (keys (json/parse-string (json/generate-string extra))))))
      (is (= hostile (get-in read ["rows" 0 "name"])))
      (is (= hostile (get-in read ["rows" 0 "labels" hostile])))
      (is (= [hostile] (get read hostile)))
      (is (= "\u009b" (get read "char"))))
    (testing "readable non-ASCII text stays as it is"
      (is (str/includes? out readable)))))

(deftest edn-report-writes-no-hidden-code-point-and-reads-back-whole
  (let [out (report-output "edn")
        read (edn/read-string {:readers render/edn-readers} out)]
    (is (= [] (raw-hidden out)))
    (is (= out (String. (.getBytes ^String out "UTF-8") "UTF-8")))
    (testing "clojure.edn gets back the exact strings, keys, keywords, symbols and characters"
      (is (= extra (select-keys read (keys extra)))))
    (testing "a keyword or symbol holding a hidden code point is tagged, as EDN cannot escape one"
      (is (str/includes? out "#gumshoe/keyword \"key evil "))
      (is (str/includes? out "#gumshoe/symbol \"evil ")))
    (testing "a character is written as a \\u literal"
      (is (str/includes? out ":char \\u009B")))
    (testing "readable non-ASCII text stays as it is"
      (is (str/includes? out readable)))))

(deftest clean-data-prints-as-before
  (let [data {:findings [{:severity :info :summary readable}]
              :rows [{:name "web" :n 1 :at #inst "2026-11-28T00:00:00Z"}]
              :c \a :q "say \"hi\"\n" :s 'sym}]
    (is (= (pr-str data) (render/edn-text data)))
    (is (= (json/generate-string data {:pretty true}) (detective/json-report data)))))

(deftest a-backslash-or-quote-in-text-does-not-confuse-the-escaper
  (let [data {"a\\" "\\\u202e\"" :b "\"\\u202e\"" :c \" :d \\ :e [\u0085 "x"]}]
    (is (= (json/parse-string (json/generate-string (dissoc data :c :d :e)))
           (json/parse-string (detective/json-report (dissoc data :c :d :e))))
        "json")
    (is (= data (edn/read-string (render/edn-text data))) "edn")
    (is (= [] (raw-hidden (render/edn-text data))))
    (is (= [] (raw-hidden (detective/json-report (dissoc data :c :d :e)))))))

(deftest a-sorted-map-with-a-tagged-key-still-prints
  (let [data {:s (sorted-map :a 1 (keyword "x\u200b") 2)}
        out (render/edn-text data)]
    (is (= [] (raw-hidden out)))
    (is (= data (edn/read-string {:readers render/edn-readers} out)))))

(deftest plain-writers-fail-these-checks
  (testing "cheshire and pr-str alone write hidden code points raw, so the checks above can fail"
    (is (seq (raw-hidden (json/generate-string extra {:pretty true}))))
    (is (seq (raw-hidden (pr-str extra)))))
  (testing "the report without the escapers would fail them"
    (with-redefs [briko-json/escape-hidden identity
                  render/edn-text pr-str]
      (is (seq (raw-hidden (report-output "json"))))
      (is (seq (raw-hidden (report-output "edn")))))))

(def ^:private any-code-point
  "Any code point, surrogates included, so a generated string can hold a lone
   surrogate; weighted towards the hidden classes and the edges of ASCII."
  (gen/frequency [[3 (gen/choose 0 0x7f)]
                  [2 (gen/elements (mapcat #(.toArray (.codePoints ^String %)) hostile-parts))]
                  [1 (gen/elements [(int \") (int \\) (int \u) 0x2028 0xe0001])]
                  [2 (gen/choose 0x80 0xffff)]
                  [1 (gen/choose 0x10000 0x10ffff)]]))

(def ^:private any-string
  (gen/fmap #(apply code-point-string %) (gen/vector any-code-point 0 24)))

(def ^:private round-trips
  (prop/for-all [ks (gen/vector any-string 0 4)
                 vs (gen/vector any-string 0 4)]
    (let [data {:extra (zipmap ks vs) :list vs}
          json-out (detective/json-report data)
          edn-out (render/edn-text data)]
      (and (= [] (raw-hidden json-out))
           (= [] (raw-hidden edn-out))
           (= (json/parse-string (json/generate-string data)) (json/parse-string json-out))
           (= data (edn/read-string {:readers render/edn-readers} edn-out))))))

(deftest any-string-round-trips-through-json-and-edn-without-raw-hidden-code-points
  (let [result (tc/quick-check 300 round-trips :seed 14)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :seed])))))

(defn- differing-from-briko
  "The first code points (up to five) on which pattern, matched against the
   code point alone, disagrees with briko.json/hidden?, as U+ strings."
  [pattern]
  (into [] (comp (remove #(= (boolean (re-matches pattern (String. (Character/toChars (int %)))))
                             (briko-json/hidden? %)))
                 (take 5)
                 (map #(format "U+%04X" %)))
        (range 0 0x110000)))

(deftest the-edn-escaper-hides-exactly-what-briko-json-hides
  (testing "render's EDN class and briko.json/hidden? name the same code points, so the json and edn reports escape alike"
    (is (= [] (differing-from-briko @#'render/hidden-pattern))))
  (testing "negative control: a class without the format characters differs"
    (is (seq (differing-from-briko #"[\p{Cc}\p{Zl}\p{Zp}\p{Cs}\p{Cn}]")))))
