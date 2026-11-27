;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.read-result-test
  "The source-neutral half of the three-state reads, judged over a fake
   source that is neither kubectl nor dig: the states and their predicates,
   on-state, the extractors and the exception they throw, derived, the
   refusal builders, describe, several reads at once, and the claim that
   the namespace depends on nothing else in gumshoe."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.reads :as reads]
            [gumshoe.reads.result :as result]))

;; --- a fake source -------------------------------------------------------------------
;; A thermometer that answers a reading, answers that it has no sensor of that
;; name, or does not answer at all.

(defmethod result/describe :thermometer
  [{:keys [sensor room]}]
  (str "the " sensor " sensor in the " room))

(defn- ref-of [sensor] {:source :thermometer :sensor sensor :room "cellar"})

(defn- classify
  "The fake source's classifier over its raw answer."
  [sensor {:keys [code celsius]}]
  (let [ref (ref-of sensor)]
    (case code
      :ok (result/present ref {:object {:celsius celsius}})
      :no-such-sensor (result/absent ref)
      (result/unread ref (str "the bus answered " (name (or code :nothing)))))))

(def ^:private warm (classify "north" {:code :ok :celsius 21}))
(def ^:private none (classify "south" {:code :no-such-sensor}))
(def ^:private silent (classify "east" {:code :timeout}))

(defn- thrown [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

;; --- states and polarity -------------------------------------------------------------

(deftest each-result-is-in-exactly-one-state
  (doseq [[r expected] [[warm :present] [none :absent] [silent :unread]]]
    (testing (pr-str expected)
      (is (= expected (result/state-of r)))
      (is (= [(= :present expected) (= :absent expected) (= :unread expected)]
             [(result/present? r) (result/absent? r) (result/unread? r)]))
      (is (= (not= :unread expected) (result/answered? r))))))

(deftest anything-that-is-not-a-result-is-unread
  (doseq [v [nil [] {} "present" :present {:state :maybe} {:state "present"} true
             (sorted-map "host" #{"Ingress"}) (sorted-map :state :present)]]
    (testing (pr-str v)
      (is (false? (result/result? v)))
      (is (result/unread? v))
      (is (not (result/present? v)))
      (is (not (result/absent? v))))))

(deftest on-state-takes-the-branch-of-the-state-and-needs-all-three
  (let [branches {:present (constantly :act) :absent (constantly :skip) :unread (constantly :refuse)}]
    (is (= [:act :skip :refuse :refuse]
           (map #(result/on-state % branches) [warm none silent nil]))))
  (is (thrown? IllegalArgumentException
               (result/on-state warm {:present identity :absent identity}))))

;; --- extractors ----------------------------------------------------------------------

(deftest extractors-throw-for-a-read-that-did-not-answer
  (is (= {:celsius 21} (result/object! warm)))
  (is (nil? (result/object! none)))
  (doseq [f [result/object! result/items! result/value!]]
    (let [e (thrown #(f silent))]
      (is (some? e))
      (is (= "could not read the east sensor in the cellar: the bus answered timeout" (ex-message e)))
      (testing "the exception is readable under both keys"
        (is (true? (:gumshoe.reads/unread (ex-data e))))
        (is (true? (:gumshoe.kubectl/unread (ex-data e))))
        (is (result/unread-exception? e))
        (is (= silent (:gumshoe.reads/result (ex-data e))))))))

(deftest derived-never-hands-f-a-read-that-did-not-answer
  (let [called (atom 0)
        f (fn [o] (swap! called inc) (:celsius o))]
    (is (= 21 (result/value! (result/derived warm f))))
    (is (nil? (result/value! (result/derived none f))))
    (is (= silent (result/derived silent f)))
    (is (= 1 @called)))
  (testing "a list result derives over its items, [] when absent"
    (is (= 0 (result/value! (result/derived (result/absent (ref-of "x") {:items []}) count))))
    (is (= 2 (result/value! (result/derived (result/present (ref-of "x") {:items [1 2]}) count))))))

;; --- the exception key ---------------------------------------------------------------

(deftest an-unread-exception-is-recognised-by-either-key
  (is (result/unread-exception? (ex-info "x" {:gumshoe.reads/unread true})))
  (is (result/unread-exception? (ex-info "x" {:gumshoe.kubectl/unread true})))
  (is (not (result/unread-exception? (ex-info "x" {:something-else true}))))
  (is (not (result/unread-exception? (RuntimeException. "x"))))
  (is (not (result/unread-exception? nil)))
  (let [e (result/unread-exception "no answer" {:host "h"})]
    (is (= {:host "h" :gumshoe.reads/unread true :gumshoe.kubectl/unread true} (ex-data e)))))

;; --- refusals and describe -----------------------------------------------------------

(deftest refusals-name-the-read-in-the-sources-words
  (is (= "could not read the east sensor in the cellar (the bus answered timeout) - acting on a wrong answer could do harm, so this refuses rather than act on a read that did not answer"
         (result/unread-refusal silent)))
  (is (nil? (result/unread-refusal warm)))
  (is (nil? (result/unread-refusal none)))
  (is (nil? (result/refusal-unless-present warm)))
  (is (= "the south sensor in the cellar does not exist" (result/refusal-unless-present none)))
  (is (str/starts-with? (result/refusal-unless-present silent) "could not read the east sensor"))
  (is (nil? (result/refusal-unless-absent none)))
  (is (= "the north sensor in the cellar still exists" (result/refusal-unless-absent warm)))
  (is (str/starts-with? (result/refusal-unless-absent silent) "could not read the east sensor"))
  (is (= "could not read the east sensor in the cellar (the bus answered timeout)" (result/unread-line-of silent)))
  (is (nil? (result/unread-line-of warm))))

(deftest a-ref-without-a-source-reads-as-type-namespace-and-name
  (is (= "deployments web/api" (result/describe {:type "deployments" :namespace "web" :name "api"})))
  (is (= "nodes worker-1" (result/describe {:type "nodes" :name "worker-1"})))
  (is (= "pods in every namespace" (result/describe {:type "pods" :namespace :all})))
  (is (= "pods in web matching app=x" (result/describe {:type "pods" :namespace "web" :selector "app=x"}))))

;; --- several reads at once -----------------------------------------------------------

(deftest combined-is-only-as-good-as-its-worst-read
  (let [ref (ref-of "all")]
    (is (= silent (result/combined ref [warm silent none] vector)))
    (is (= [{:celsius 21} nil] (result/value! (result/combined ref [warm none] vector))))
    (is (= 3 (result/value! (result/combined ref [(result/present ref {:items [1 2]})
                                                  (result/derived warm :celsius)]
                                             #(+ (count %1) (if (= 21 %2) 1 0))))))
    (is (nil? (result/first-unread [warm none])))
    (is (= [["east" silent]] (result/unread-in {"north" warm "east" silent "south" none})))))

;; --- the split -----------------------------------------------------------------------

(defn- requires-of
  "The namespaces a library's ns form requires."
  [path]
  (let [form (with-open [r (java.io.PushbackReader. (io/reader path))] (read r))]
    (set (for [clause (drop 2 form)
               :when (and (seq? clause) (= :require (first clause)))
               spec (rest clause)]
           (if (vector? spec) (first spec) spec)))))

(deftest the-source-neutral-half-depends-on-nothing-in-gumshoe
  (is (= #{'clojure.string} (requires-of "libraries/gumshoe/reads/result.clj")))
  (testing "evidence, which every detective requires, reaches only the neutral half"
    (is (= #{'gumshoe.reads.result} (requires-of "libraries/gumshoe/evidence.clj")))))

(deftest gumshoe-reads-forwards-every-neutral-name
  (doseq [[forwarded neutral] [[#'reads/present? #'result/present?] [#'reads/absent? #'result/absent?]
                               [#'reads/unread? #'result/unread?] [#'reads/answered? #'result/answered?]
                               [#'reads/on-state #'result/on-state] [#'reads/describe #'result/describe]
                               [#'reads/object! #'result/object!] [#'reads/items! #'result/items!]
                               [#'reads/derived #'result/derived] [#'reads/value! #'result/value!]
                               [#'reads/unread-refusal #'result/unread-refusal]
                               [#'reads/refusal-unless-present #'result/refusal-unless-present]
                               [#'reads/refusal-unless-absent #'result/refusal-unless-absent]
                               [#'reads/states #'result/states]]]
    (testing (str forwarded)
      (is (identical? @neutral @forwarded))
      (is (= (:arglists (meta neutral)) (:arglists (meta forwarded))))
      (is (= (:doc (meta neutral)) (:doc (meta forwarded))))))
  (testing "a fake source's ref described through the forwarded name"
    (is (= "the north sensor in the cellar" (reads/describe (ref-of "north"))))))
