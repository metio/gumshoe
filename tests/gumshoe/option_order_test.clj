;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.option-order-test
  "--help lists a book's options in the order they are written, then the
   standard flags, whatever the number of options and whichever builder
   joins them: select/options, detective/book, playbook/book,
   dispatch/book, firebook/drill-spec and change/compose."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.book-surfaces-test :as surfaces]
            [gumshoe.change :as change]
            [gumshoe.conformance :as conformance]
            [gumshoe.detective :as detective]
            [gumshoe.dispatch :as dispatch]
            [gumshoe.firebook :as firebook]
            [gumshoe.flags :as flags]
            [gumshoe.playbook :as playbook]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]))

(def ^:private standard [:dry-run :no-color :help])

(defn help-order
  "The long flags --help lists, in the order it lists them."
  [config]
  (->> (str/split-lines (stdout/strip-colors (runbook/help config)))
       (drop-while #(not= "Options:" %))
       rest
       (keep #(second (re-find #"^\s+(?:-\w, )?\s*--([\w-]+)" %)))
       (mapv keyword)))

(defn- option-names
  [n]
  (mapv #(keyword (str "opt-" (char (+ (int \a) (quot % 26))) (char (+ (int \a) (mod % 26))))) (range n)))

(defn- option-map
  "An option map holding `names` in this order, as an array map of any size."
  [names]
  (apply array-map (mapcat (fn [k] [k {:desc (str "The " (name k)) :coerce :string}]) names)))

(defn- config-of
  [options]
  (binding [runbook/*loading* true]
    (runbook/execute! {:description "d" :options options :action (fn [_ _] true)})))

(deftest help-keeps-the-order-of-nine-or-more-options
  (let [names [:url :flavor :tenant :username :password :host-label :selector :job :declared :output]]
    (is (= (into names standard) (help-order (config-of (option-map names)))))))

(deftest help-keeps-the-order-when-the-standard-flags-make-nine
  (let [names [:namespace :service :metric :selector :port-name :port]]
    (is (= (into names standard) (help-order (config-of (option-map names)))))))

(deftest help-order-is-declaration-order-for-any-count
  (let [result (tc/quick-check
                200
                (prop/for-all [names (gen/fmap (comp vec distinct)
                                               (gen/vector (gen/fmap #(nth (option-names 40) %) (gen/choose 0 39)) 1 40))]
                  (= (into names standard) (help-order (config-of (option-map names)))))
                :seed 20261125)]
    (is (:pass? result) (pr-str (:shrunk result)))))

(deftest help-order-covers-every-size-from-one-to-forty
  (doseq [n (range 1 41)
          :let [names (option-names n)]]
    (is (= (into names standard) (help-order (config-of (option-map names)))) (str n " options"))))

(deftest a-book-option-named-like-a-standard-flag-keeps-its-place
  (let [options (option-map [:a :b :c :d :e :f :g :h :i])
        options (apply array-map (concat (mapcat identity options) [:dry-run {:desc "Rehearse" :coerce :boolean}]))]
    (is (= [:a :b :c :d :e :f :g :h :i :dry-run :no-color :help] (help-order (config-of options))))))

;; --- builders -----------------------------------------------------------------

(def ^:private namespaced-select
  {:mode :namespaced :label "release" :namespace-flag :namespace :name-flag :name
   :candidates (constantly [])})

(deftest select-options-lists-derived-flags-first-then-the-books-own
  (let [own (option-map [:k1 :k2 :k3 :k4 :k5 :k6 :k7 :k8])
        merged (select/options namespaced-select
                               (apply array-map (concat (mapcat identity own)
                                                        [:name {:desc "The release to reset"}])))]
    (is (= [:namespace :name :k1 :k2 :k3 :k4 :k5 :k6 :k7 :k8] (vec (keys merged))))
    (is (= {:desc "The release to reset" :coerce :string :alias :N} (:name merged))
        "an overriding key keeps its alias and its first position")
    (is (= [:namespace :name :k1 :k2 :k3 :k4 :k5 :k6 :k7 :k8 :dry-run :no-color :help]
           (help-order (config-of merged))))))

(deftest detective-book-lists-output-after-the-books-options
  (let [names (option-names 9)
        config (binding [runbook/*loading* true]
                 (detective/book {:description "d" :detectives [] :options (option-map names)}))]
    (is (= (into (conj names :output) standard) (help-order config)))))

(deftest playbook-lists-the-steps-options-in-step-order-then-its-own
  (let [steps [{:check identity :options (option-map [:s1 :s2 :s3 :s4 :s5])}
               {:check identity :options (option-map [:s3 :t1 :t2 :t3 :t4])}]
        all (playbook/all-options {:steps steps :options (option-map [:p1 :s1])})]
    (is (= [:s1 :s2 :s3 :s4 :s5 :t1 :t2 :t3 :t4 :p1] (vec (keys all))))))

(deftest dispatch-lists-the-routes-options-then-its-own
  (let [route {:playbook {:steps [{:check identity :options (option-map [:r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8])}]}}
        all (dispatch/all-options {:routes {:only route} :options (option-map [:d1 :d2])})]
    (is (= [:r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :d1 :d2] (vec (keys all))))))

(deftest firebook-lists-extinguish-after-the-books-options
  (let [names (option-names 9)
        spec (firebook/drill-spec {:description "d" :options (option-map names)
                                   :ignite {:effect (constantly nil)}
                                   :extinguish {:effect (constantly nil)}})]
    (is (= (conj names :extinguish) (vec (keys (:options spec)))))))

(deftest compose-lists-options-in-order-of-first-appearance
  (let [a {:options (option-map [:a1 :a2 :a3 :a4 :a5])}
        b {:options (option-map [:a3 :b1 :b2 :b3 :b4])}]
    (is (= [:a1 :a2 :a3 :a4 :a5 :b1 :b2 :b3 :b4] (vec (keys (:options (change/compose a b))))))))

;; --- gumshoe.flags --------------------------------------------------------------

(deftest ordered-keeps-the-written-order-at-any-size
  (doseq [n (range 0 41)
          :let [names (option-names n)
                options (apply flags/ordered (mapcat (fn [k] [k {:desc (name k)}]) names))]]
    (is (= names (vec (keys options))) (str n " options"))
    (is (not (flags/order-lost? options)))))

(deftest ordered-takes-a-repeated-flag-at-its-first-position
  (is (= [[:a {:desc "a2"}] [:b {:desc "b"}]]
         (vec (flags/ordered :a {:desc "a1"} :b {:desc "b"} :a {:desc "a2"})))))

(deftest ordered-refuses-an-odd-argument-list
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"pairs" (flags/ordered :a {:desc "a"} :b))))

(deftest merge-ordered-keeps-first-appearance
  (let [a (option-map (option-names 9))
        b (array-map :x {:desc "x"} :opt-ac {:desc "late"})
        merged (flags/merge-ordered a nil b)]
    (is (= (conj (option-names 9) :x) (vec (keys merged))))
    (is (= {:desc "late"} (:opt-ac merged)))
    (is (nil? (flags/merge-ordered nil nil)))
    (is (= {:a {:desc "a" :alias :x}}
           (flags/merge-ordered-with merge {:a {:desc "a"}} {:a {:alias :x}})))))

(deftest merge-ordered-equals-merge-as-data
  (let [result (tc/quick-check
                200
                (prop/for-all [maps (gen/vector (gen/map gen/keyword gen/small-integer {:max-elements 12}) 0 5)]
                  (= (if (some some? maps) (apply merge maps) nil)
                     (apply flags/merge-ordered maps)))
                :seed 20261125)]
    (is (:pass? result) (pr-str (:shrunk result)))))

(deftest merge-ordered-lists-keys-in-order-of-first-appearance
  (let [result (tc/quick-check
                200
                (prop/for-all [key-lists (gen/vector (gen/fmap (comp vec distinct) (gen/vector gen/keyword 0 15)) 1 4)]
                  (= (vec (distinct (apply concat key-lists)))
                     (vec (keys (apply flags/merge-ordered (map #(apply array-map (interleave % (range))) key-lists))))))
                :seed 20261125)]
    (is (:pass? result) (pr-str (:shrunk result)))))

(deftest order-lost-names-a-hash-map
  (is (flags/order-lost? (into {} (map (fn [k] [k {}]) (option-names 9)))))
  (is (not (flags/order-lost? (option-map (option-names 9)))))
  (is (not (flags/order-lost? (into (sorted-map) (option-map (option-names 9))))))
  (is (not (flags/order-lost? {})))
  (is (not (flags/order-lost? nil))))

;; --- conformance ----------------------------------------------------------------

(deftest every-book-keeps-its-option-order
  (is (empty? (conformance/option-order-findings
               (conformance/load-books (surfaces/book-files) {})))))
