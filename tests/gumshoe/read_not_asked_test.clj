;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.read-not-asked-test
  "A read that was never made is :unread with :not-asked, so every gate
   that refuses an unanswered read refuses it too, evidence/joined? is
   answered? over each read, and the detective report tells a missing
   input from a source that did not answer."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.evidence :as evidence]
            [gumshoe.reads.result :as result]))

(def ^:private ref {:source :dns :type "MX" :name "example.org" :server "192.0.2.53"})

(def ^:private present (result/present ref {:items ["10 mx.example.org."]}))
(def ^:private absent (result/absent ref {:items []}))
(def ^:private unread (result/unread ref "SERVFAIL"))
(def ^:private not-asked (result/not-asked {:type "MX records"} "no domain was given"))

(deftest a-read-never-made-stays-unread-test
  (is (result/result? not-asked))
  (is (= :unread (result/state-of not-asked)))
  (is (result/unread? not-asked))
  (is (not (result/answered? not-asked)))
  (is (result/not-asked? not-asked))
  (testing "only a not-asked result is not-asked"
    (is (not-any? result/not-asked? [present absent unread nil [] {:not-asked true}]))))

(deftest every-gate-refuses-a-read-never-made-test
  (is (some? (result/refusal-unless-present not-asked)))
  (is (some? (result/refusal-unless-absent not-asked)))
  (is (some? (result/unread-refusal not-asked)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no domain was given" (result/items! not-asked)))
  (is (thrown? clojure.lang.ExceptionInfo (result/object! not-asked)))
  (is (= not-asked (result/first-unread [present not-asked])))
  (is (= not-asked (result/combined ref [present not-asked] vector)))
  (is (= [[:mx not-asked]] (result/unread-in {:mx not-asked :a present}))))

(deftest the-line-says-whether-the-read-was-asked-test
  (is (= "could not read MX example.org at 192.0.2.53 (SERVFAIL)" (result/unread-line-of unread)))
  (is (= "did not read MX records (no domain was given)" (result/unread-line-of not-asked)))
  (is (nil? (result/unread-line-of present))))

(deftest joined-is-answered-over-every-read-test
  (let [ev {"present" present "absent" absent "unread" unread "not-asked" not-asked
            "nil" nil "empty" [] "list" ["x"]}]
    (testing "answered?"
      (is (= {"present" true "absent" true "unread" false "not-asked" false "nil" false "empty" true "list" true}
             (update-vals ev evidence/answered?)))
      (is (false? (evidence/answered? (get ev "never-collected")))))
    (testing "missing? is its negation"
      (doseq [[k v] ev] (is (= (not (evidence/answered? v)) (evidence/missing? v)) k)))
    (testing "joined? needs every read answered"
      (is (evidence/joined? ev ["present" "absent" "empty" "list"]))
      (doseq [k ["unread" "not-asked" "nil" "never-collected"]]
        (is (not (evidence/joined? ev ["present" k])) k)))
    (testing "consulted? needs one"
      (is (evidence/consulted? ev ["unread" "absent"]))
      (is (not (evidence/consulted? ev ["unread" "not-asked" "nil"]))))
    (testing "not-asked? holds only for a read never made"
      (is (= #{"not-asked"} (set (keep (fn [[k v]] (when (evidence/not-asked? v) k)) ev)))))))

(def ^:private checks
  [{:name "records" :requires ["mx" "domain"] :joins ["mx"]}
   {:name "both" :joins ["mx" "txt"]}
   {:name "fine" :requires ["txt"]}])

(deftest mark-unread-names-what-was-not-given-test
  (testing "a read never made"
    (let [marked (detective/mark-unread checks {"mx" not-asked "txt" present})]
      (is (= [{:unread true :not-asked ["no domain was given"]}
              {:unread true :not-asked ["no domain was given"]}
              {}]
             (map #(select-keys % [:unread :not-asked :reasons]) marked)))))
  (testing "a read that did not answer"
    (let [marked (detective/mark-unread checks {"mx" unread "txt" present "domain" "example.org"})]
      (is (= {:unread true :reasons ["could not read MX example.org at 192.0.2.53 (SERVFAIL)"]}
             (select-keys (first marked) [:unread :not-asked :reasons])))))
  (testing "both at once: still unread, with each named"
    (let [marked (detective/mark-unread checks {"mx" not-asked "txt" unread})]
      (is (= {:unread true :not-asked ["no domain was given"]
              :reasons ["could not read MX example.org at 192.0.2.53 (SERVFAIL)"]}
             (select-keys (second marked) [:unread :not-asked :reasons])))))
  (testing "plain nil evidence keeps the older marks"
    (is (= {:unread true} (select-keys (first (detective/mark-unread checks {})) [:unread :not-asked :reasons]))))
  (testing "a not-asked result on a check that read something else is no mark"
    (is (not (:unread (first (detective/mark-unread [{:name "c" :consults ["mx" "txt"]}]
                                                   {"mx" not-asked "txt" present})))))))

(deftest the-report-lines-split-not-asked-from-unread-test
  (let [marked (detective/mark-unread
                [{:name "a" :requires ["mx"]} {:name "b" :requires ["ptr"]} {:name "c" :requires ["probes"]}
                 {:name "d" :requires ["ns"]}]
                {"mx" not-asked
                 "ptr" (result/not-asked {:source :mail :read :ptr} "no host was given")
                 "probes" (result/not-asked {:source :mail :read :probes} "no host was given")
                 "ns" unread})]
    (is (= "3 checks judged nothing because an input was not given - a (no domain was given); b, c (no host was given)"
           (detective/not-asked-line marked)))
    (is (= "1 check read nothing on this cluster - other (d); what they read is not served, not readable, or not gathered by this scan"
           (detective/unread-line marked)))
    (is (nil? (detective/not-asked-line (detective/mark-unread [{:name "d" :requires ["ns"]}] {"ns" unread}))))
    (testing "a run where every check lacked its input is still nothing checked"
      (is (detective/nothing-checked? (take 3 marked) []))
      (is (= (str "every check read nothing, so nothing was checked - a, b, c; "
                  "3 checks judged nothing because an input was not given - a (no domain was given); b, c (no host was given)")
             (detective/nothing-checked-message (take 3 marked)))))))
