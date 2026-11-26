;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.postgres-script-copy-test
  "A COPY ... FROM stdin statement is recognised whatever the length of its
   column list, so its data rows are never read as SQL. A false refusal here
   blocks every restore of the dump, so a property checks that no COPY data,
   however full of semicolons, quotes and breaker keywords, is ever named."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.postgres-script :as script]))

(defn- found [lines] (mapv :statement (script/transaction-breakers lines)))

(defn- columns [n] (str/join ", " (map #(str "column_number_" %) (range n))))

(deftest a-copy-with-twenty-columns-skips-its-data
  (is (= [] (found ["COPY public.wide (" (columns 20) ") FROM stdin;"]))
      "sanity: a statement split over lines")
  (is (= [] (found [(str "COPY public.wide (" (columns 20) ") FROM stdin;")
                    "1\t<p>Hello&nbsp;End of term</p>"
                    "2\tx;commit"
                    "\\."
                    "ALTER TABLE ONLY public.wide ADD CONSTRAINT p PRIMARY KEY (column_number_0);"]))))

(deftest a-copy-with-three-hundred-columns-skips-its-data
  (let [header (str "COPY public.huge (" (columns 300) ") FROM stdin;")]
    (is (> (count header) 4000))
    (is (= [] (found [header "1\tit's; VACUUM; \"odd" "2\tx;begin" "\\." "SELECT 1;"])))
    (testing "a real breaker after the data is still named, on its own line"
      (is (= [{:line 5 :statement "VACUUM"}]
             (script/transaction-breakers [header "1\ta;end" "\\." "SELECT 1;" "VACUUM;"]))))))

(deftest a-copy-whose-column-list-wraps-skips-its-data
  (is (= [] (found ["COPY public.wrapped ("
                    (columns 150)
                    ","
                    "    other_column"
                    ") FROM"
                    "stdin;"
                    "1\tfoo;rollback"
                    "\\."]))))

(deftest a-copy-to-stdout-or-from-a-file-has-no-data-block
  (is (= ["VACUUM"] (found [(str "COPY public.t (" (columns 60) ") TO stdout;") "VACUUM;"])))
  (is (= ["VACUUM"] (found [(str "COPY public.t (" (columns 60) ") FROM '/tmp/stdin';") "VACUUM;"]))
      "a file named stdin is quoted and starts no data block"))

(deftest a-reindex-concurrently-past-the-head-is-named
  (let [long-name (apply str "t_" (repeat 250 "x"))]
    (is (= ["REINDEX CONCURRENTLY"]
           (found [(str "REINDEX (VERBOSE, TABLESPACE " long-name ") TABLE CONCURRENTLY t;")])))))

(def ^:private breaker-words
  ["COMMIT" "commit" "End" "end" "BEGIN" "begin" "abort" "ROLLBACK" "vacuum"
   "CREATE DATABASE x" "drop database y" "START TRANSACTION" "prepare transaction 'p'"
   "REINDEX DATABASE z" "alter system set a = 1" "cluster"])

(def ^:private data-noise
  ["'" "\"" "''" "$$" "$x$" "/*" "*/" "--" "\\\\" "&nbsp;" "(" ")" "\\N" " " "\t"])

(def ^:private gen-data-cell
  (gen/fmap (fn [[before noise word after]] (str before noise ";" word after))
            (gen/tuple gen/string-alphanumeric (gen/elements data-noise) (gen/elements breaker-words)
                       (gen/elements ["" " of term" ";" "; x" "</p>" "'"]))))

(def ^:private gen-data-row
  (gen/fmap #(str/join "\t" %) (gen/vector gen-data-cell 1 4)))

(def ^:private gen-copy-block
  (gen/let [table gen/nat
            width (gen/choose 1 400)
            wrap? gen/boolean
            rows (gen/vector gen-data-row 0 6)]
    (let [cols (columns width)
          header (if wrap?
                   [(str "COPY public.t" table " (") cols ") FROM stdin;"]
                   [(str "COPY public.t" table " (" cols ") FROM stdin;")])]
      (concat header rows ["\\."]))))

(def ^:private gen-statement
  (gen/elements [["SET statement_timeout = 0;"]
                 ["SELECT pg_catalog.set_config('search_path', '', false);"]
                 ["CREATE TABLE public.t (a integer, b text);"]
                 ["CREATE FUNCTION public.f() RETURNS void" "    LANGUAGE plpgsql" "    AS $$" "BEGIN" "  COMMIT;" "END;" "$$;"]
                 ["-- commit;"]
                 ["/* begin; */"]
                 ["ALTER TABLE ONLY public.t ADD CONSTRAINT p PRIMARY KEY (a);"]]))

(def ^:private gen-dump
  (gen/fmap #(vec (apply concat ["-- PostgreSQL database dump"] (concat % [["-- PostgreSQL database dump complete"]])))
            (gen/vector (gen/one-of [gen-statement gen-copy-block]) 1 12)))

(deftest no-copy-data-is-ever-named-a-breaker
  (let [result (tc/quick-check 300 (prop/for-all [lines gen-dump] (= [] (found lines))) :seed 17)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :seed])))))

(deftest the-property-would-catch-a-scanner-that-reads-copy-data
  (let [result (with-redefs-fn {#'script/copy-from-stdin? (constantly false)}
                 #(tc/quick-check 300 (prop/for-all [lines gen-dump] (= [] (found lines))) :seed 17))]
    (is (not (:pass? result)) "a scanner that never recognises COPY fails the property")))
