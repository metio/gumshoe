;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.change-compose-test
  "change/compose: each key's rule, each clash it refuses, and the
   properties the rules promise - associative, idempotent on a repeated
   part, and never weaker in its guard, its post-checks or its
   confirmation than any part."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.change :as change]))

(defn- refused-why
  "The message compose refuses parts with, or nil when it accepts them."
  [& parts]
  (try (apply change/compose parts) nil
       (catch clojure.lang.ExceptionInfo e
         (when (change/invalid? e) (ex-message e)))))

(def ^:private seed 20261125)

(defn- check!
  "Runs a test.check property with a fixed seed and reports a failure's
   shrunk case."
  [n property]
  (let [result (tc/quick-check n property :seed seed)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :fail])))))

;; --- each key's rule -------------------------------------------------------------

(deftest reads-and-values-merge-by-name-in-part-order
  (let [a (fn [_] :a) b (fn [_] :b) c (fn [_] :c)
        composed (change/compose {:reads {:x a} :values {:v1 a}}
                                 {:reads {:y b :x a} :values {:v2 b}}
                                 {:values {:v3 c}})]
    (is (= [[:x a] [:y b]] (seq (:reads composed))) "the same function under one name is one entry")
    (is (= [:v1 :v2 :v3] (keys (:values composed))) "values keep their order, which is the order they are computed in"))
  (testing "the order survives past the size a map keeps by itself"
    (let [names (map #(keyword (str "v" %)) (range 20))
          parts (map (fn [n] {:values {n (fn [_] n)}}) names)]
      (is (= names (keys (:values (apply change/compose parts))))))))

(deftest every-refusal-is-judged-and-the-first-reason-refuses
  (let [judged (atom [])
        part (fn [label why] {:refusal (fn [_] (swap! judged conj label) why)})
        refusal (:refusal (change/compose (part :a nil) (part :b "b refuses") (part :c "c refuses")))]
    (is (= "b refuses" (refusal {})))
    (is (= [:a :b] @judged) "a part after the first reason is not judged")
    (testing "unmet is a reason like any other"
      (is (= change/unmet ((:refusal (change/compose {:precondition (constantly false)} (part :d "d")))  {}))))
    (testing "a precondition alias joins the refusals"
      (is (= change/unmet ((:refusal (change/compose (part :e nil) {:precondition (constantly false)})) {}))))))

(deftest a-refusal-or-precondition-two-parts-name-is-judged-once
  (let [judged (atom 0)
        precondition (fn [_] (swap! judged inc) true)
        refusal (fn [_] (swap! judged inc) nil)]
    (is (nil? ((:refusal (change/compose {:precondition precondition :refusal refusal}
                                         {:precondition precondition}
                                         {:refusal refusal}))
               {})))
    (is (= 2 @judged) "each guard once: a precondition may read the cluster")))

(deftest post-checks-and-items-concatenate-in-part-order
  (let [composed (change/compose {:verify (fn [_] [{:description "a"}]) :items (fn [_] ["one"])}
                                 {:verify (fn [_] nil)}
                                 {:verify (fn [_] (list {:description "b"})) :items (fn [_] ["two"])})]
    (is (= [{:description "a"} {:description "b"}] ((:verify composed) {})))
    (is (= ["one" "two"] ((:items composed) {})))))

(deftest done-holds-when-every-part-is-done
  (let [yes (constantly true) no (constantly false) v (fn [_] [])]
    (doseq [[label parts expected] [["true and true" [{:done true :verify v} {:done true}] true]
                                    ["true and a function" [{:done true :verify v} {:done yes}] yes]
                                    ["absent is neutral" [{:verify v} {:done yes}] yes]
                                    ["satisfied? is :done" [{:satisfied? true :verify v} {}] true]]]
      (testing label
        (is (= expected (:done (apply change/compose parts))))))
    (testing "two functions hold together"
      (let [done (:done (change/compose {:done yes} {:done no}))]
        (is (false? (done {})))
        (is (true? ((:done (change/compose {:done yes} {:done (constantly true)})) {})))))))

(deftest subjects-concatenate-and-a-ref-two-parts-name-is-refused
  (let [web {:type "deployments" :namespace "apps" :name "web"}
        db {:type "statefulsets" :namespace "apps" :name "db"}
        pick {:type "cronjobs"}
        pin (fn [_] web)
        composed (change/compose {:subject pick :subjects (fn [_] [web])}
                                 {:subjects (fn [_] [db]) :pin pin})]
    (is (= [:pick :target :reads] (mapv :from (:subjects composed))))
    (is (= [web db] ((:refs (change/subject composed :target)) {})))
    (is (= pin (change/pin composed)))
    (testing "the same ref from two parts is refused when the refs are computed"
      (let [refs (:refs (change/subject (change/compose {:subjects (fn [_] [web])} {:subjects (fn [_] [db web])}) :target))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"two parts of the change name the subject deployments apps/web"
                              (refs {})))))
    (testing "one part's refs are never deduplicated or judged by compose"
      (let [one (fn [_] [web web])]
        (is (= one (:refs (change/subject (change/compose {:subjects one}) :target))))))))

(deftest a-confirmation-is-destructive-when-any-part-says-so
  (doseq [[a b expected] [[{:action "go" :destructive? false} {:destructive? true} {:action "go" :destructive? true}]
                          [{:action "go" :destructive? false} {} {:action "go" :destructive? false}]
                          [{:action "go"} {:separately? true} {:action "go" :separately? true}]
                          [{:action "go" :separately? false :destructive? true} {:action "go" :separately? true}
                           {:action "go" :separately? true :destructive? true}]]]
    (is (= expected (:confirm (change/compose {:confirm a} {:confirm b}))) (pr-str [a b]))))

(deftest prerequisites-and-options-join
  (is (= {:installed-tools ["kubectl" "fzf"] :kubectl-can-get ["pods"] :kubectl-can-delete ["pods"]}
         (:prerequisites (change/compose {:prerequisites {:installed-tools ["kubectl"] :kubectl-can-get ["pods"]}}
                                         {:prerequisites {:installed-tools ["kubectl" "fzf"] :kubectl-can-delete ["pods"]}}))))
  (is (= {:name {:alias :n :desc "first"} :node {:desc "x"}}
         (:options (change/compose {:options {:name {:alias :n :desc "first"}}}
                                   {:options {:name {:alias :n :desc "second"} :node {:desc "x"}}})))))

(deftest reasons-for-having-no-post-check-or-guard-are-kept
  (is (= "a; b" (:no-verify (change/compose {:no-verify "a"} {:no-verify "b"} {:no-verify "a"}))))
  (is (= "a; b" (:no-guard (change/compose {:no-guard "a; b"} {:no-guard "b"})))))

(deftest a-single-valued-key-two-parts-give-alike-is-one
  (let [effect (fn [_] [])]
    (is (= {:effect effect :title "t"} (change/compose {:effect effect :title "t"} {:effect effect :title "t"})))))

(deftest a-composed-whole-spec-is-what-normalize-makes-of-it
  (let [spec {:select {:mode :one} :subject {:type "nodes"} :refusal (constantly nil)
              :derive (fn [_] {}) :confirm {:action "go" :destructive? false} :effect (fn [_] [])
              :verify (fn [_] []) :announce? false :empty-message "none"}]
    (is (= (change/normalize spec) (change/compose spec)))
    (is (= (change/compose spec) (change/normalize (change/compose spec))))
    (testing "a :precondition becomes a new refusal each time, which behaves alike"
      (let [spec (assoc spec :precondition (constantly false))]
        (is (= change/unmet ((:refusal (change/compose spec)) {}) ((:refusal (change/normalize spec)) {})))))))

(deftest compose-leaves-the-whole-change-checks-to-normalize
  (let [fragment (change/compose {:done true})]
    (is (= {:done true} fragment) "a fragment a later part completes")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #":done true .* has no :verify" (change/normalize fragment)))
    (is (= true (:done (change/normalize (change/compose fragment {:verify (fn [_] [])})))))))

(deftest nil-parts-are-skipped
  (is (= {:title "t"} (change/compose nil {:title "t"} nil)))
  (is (= {} (change/compose))))

;; --- each clash ------------------------------------------------------------------

(deftest every-clash-is-refused
  (let [f (fn [_] :f) g (fn [_] :g)]
    (doseq [[label parts message]
            [[":reads name" [{:reads {:x f}} {:reads {:x g}}] #"bind the :reads entry :x to different functions"]
             [":values name" [{:values {:x f}} {:values {:x g}}] #"bind the :values entry :x to different functions"]
             ["a read and a value" [{:reads {:x f}} {:values {:x g}}] #":x is both a read and a value"]
             ["two :derive" [{:derive f} {:derive g}] #"bind the :values entry :gumshoe.change/derive to different functions"]
             [":derive beside a named value" [{:derive f} {:values {:x g}}] #"has :derive beside the named entries \[:x\]"]
             [":recheck beside a named read" [{:recheck f :refusal f} {:reads {:x g}}] #"has :recheck beside the named entries \[:x\]"]
             [":effect" [{:effect f} {:effect g}] #"give :effect differently"]
             [":select" [{:select {:mode :one}} {:select {:mode :many}}] #"give :select differently"]
             [":empty-message" [{:empty-message "a"} {:select {:empty-message "b"}}] #"give :select differently"]
             [":connect" [{:connect f} {:connect g}] #"give :connect differently"]
             [":confirm :action" [{:confirm {:action "a"}} {:confirm {:action "b"}}] #"give \[:confirm :action\] differently"]
             [":title" [{:title "a"} {:title "b"}] #"give :title differently"]
             [":description" [{:description "a"} {:description "b"}] #"give :description differently"]
             [":recovery" [{:recovery "a"} {:recovery f}] #"give :recovery differently"]
             [":announce false beside a message" [{:announce? false} {:announce f}] #"give :announce differently"]
             [":announce" [{:announce f} {:announce g}] #"give :announce differently"]
             ["two picks" [{:subject {:type "a"}} {:subject {:type "b"}}] #"give \[:subjects :pick\] differently"]
             ["two pins" [{:pin f} {:pin g}] #"give \[:subjects :reads\] differently"]
             [":target entries' other keys" [{:subjects [{:from :target :refs f :x 1}]} {:subjects [{:from :target :refs g :x 2}]}]
              #"give \[:subjects :target\] differently"]
             [":options" [{:options {:name {:alias :n}}} {:options {:name {:alias :m}}}] #"declare --name differently"]
             [":prerequisites" [{:prerequisites {:x "a"}} {:prerequisites {:x "b"}}] #"cannot share their prerequisites"]
             [":verify beside :no-verify" [{:verify f} {:no-verify "why"}] #"says it has no post-check \(\"why\"\) and a part declares a :verify"]
             [":no-verify beside :verify" [{:no-verify "why"} {:verify f}] #"says it has no post-check"]
             [":no-guard beside :refusal" [{:no-guard "why"} {:refusal f}] #"says it has no guard \(\"why\"\) and a part declares a :refusal or :subjects"]
             [":no-guard beside :precondition" [{:precondition f} {:no-guard "why"}] #"says it has no guard"]
             [":no-guard beside a subject" [{:no-guard "why"} {:subject {:type "a"}}] #"says it has no guard"]
             [":no-verify that is not a string" [{:no-verify "why"} {:no-verify 1}] #"give :no-verify differently"]
             ["a key compose has no rule for" [{:check f} {:check g}] #"give :check differently"]
             [":confirmed?" [{:confirmed? true} {:confirmed? false}] #"give :confirmed\? differently"]]]
      (testing label
        (let [why (apply refused-why parts)]
          (is (some? why) "refused")
          (when why (is (re-find message why) why)))
        (testing "in either order"
          (is (some? (apply refused-why (reverse parts)))))))))

(deftest a-part-normalize-refuses-is-refused-by-compose
  (is (re-find #"has :done and :satisfied\?" (refused-why {:done true :satisfied? true}))))

(deftest a-single-spec-saying-it-has-no-post-check-beside-one-is-refused
  (is (re-find #"says it has no post-check" (refused-why {:verify (fn [_] []) :no-verify "why"}))))

;; --- properties --------------------------------------------------------------------

(def ^:private flags (range 4))

(def ^:private contexts
  "Every context the generated functions tell apart: a subset of flags."
  (vec (for [bits (range (bit-shift-left 1 (count flags)))]
         {:flags (set (filter #(bit-test bits %) flags))})))

(def ^:private refusal-pool
  (vec (concat [(constantly nil) (constantly change/unmet) (constantly "always")]
               (for [i flags] (fn [{:keys [flags]}] (when (contains? flags i) (str "flag " i)))))))

(def ^:private verify-pool
  (vec (for [i (range 4)] (fn [{:keys [flags]}] (cond-> [{:description (str "check " i)}]
                                                   (contains? flags i) (conj {:description (str "extra " i)}))))))

(def ^:private done-pool
  (vec (concat [(constantly true) (constantly false)]
               (for [i flags] (fn [{:keys [flags]}] (contains? flags i))))))

(def ^:private gen-guard-part
  "A part with any of a refusal, a precondition, post-checks, a :done and a
   confirmation, drawn from shared pools so parts share functions too."
  (gen/let [refusal (gen/one-of [(gen/return nil) (gen/elements refusal-pool)])
            precondition (gen/one-of [(gen/return nil) (gen/elements done-pool)])
            verify (gen/one-of [(gen/return nil) (gen/elements verify-pool)])
            done (gen/one-of [(gen/return nil) (gen/return true) (gen/elements done-pool)])
            destructive (gen/elements [nil true false])
            separately (gen/elements [nil true false])]
    (cond-> {}
      refusal (assoc :refusal refusal)
      precondition (assoc :precondition precondition)
      verify (assoc :verify verify)
      done (assoc :done done)
      (some? destructive) (assoc-in [:confirm :destructive?] destructive)
      (some? separately) (assoc-in [:confirm :separately?] separately))))

(defn- descriptions
  [spec ctx]
  (set (map :description (when-let [v (:verify spec)] (v ctx)))))

(defn- done?
  "Whether a canonical spec's own :done holds for ctx, before the engine
   joins it with the post-checks, which every part's :done shares."
  [{:keys [done]} ctx]
  (and (some? done)
       (or (true? done) (boolean (done ctx)))))

(deftest a-composed-guard-is-never-weaker-than-any-part
  (check! 300
          (prop/for-all [parts (gen/vector gen-guard-part 1 5)]
            (let [composed (apply change/compose parts)
                  canonical (map change/compose parts)]
              (and
               ;; every part that refuses still refuses, with the first reason
               (every? (fn [ctx]
                         (let [whys (keep #(when-let [r (:refusal %)] (r ctx)) canonical)]
                           (= (first whys) (when-let [r (:refusal composed)] (r ctx)))))
                       contexts)
               ;; every part's post-check still runs
               (every? (fn [ctx] (every? #(set/subset? (descriptions % ctx) (descriptions composed ctx)) canonical))
                       contexts)
               (= (some? (:verify composed)) (boolean (some :verify canonical)))
               ;; the confirmation is destructive, and asks again, when any part says so
               (= (boolean (some #(true? (get-in % [:confirm :destructive?])) canonical))
                  (true? (get-in composed [:confirm :destructive?])))
               (= (boolean (some #(true? (get-in % [:confirm :separately?])) canonical))
                  (true? (get-in composed [:confirm :separately?])))
               ;; a composed change is done only when every part declaring :done is
               (every? (fn [ctx]
                         (or (not (done? composed ctx))
                             (every? #(or (nil? (:done %)) (done? % ctx)) canonical)))
                       contexts)
               (= (some? (:done composed)) (boolean (some :done canonical))))))))

;; --- associativity and idempotence ---------------------------------------------------

(def ^:private named-pool (vec (for [i (range 3)] (fn [_] i))))

(def ^:private gen-part
  "A part over every rule: guards and post-checks, named reads and values
   that may clash, a single-valued effect and title, subjects and reasons."
  (gen/let [guard gen-guard-part
            reads (gen/map (gen/elements [:a :b]) (gen/elements named-pool) {:max-elements 2})
            values (gen/map (gen/elements [:c :d :a]) (gen/elements named-pool) {:max-elements 2})
            effect (gen/one-of [(gen/return nil) (gen/elements named-pool)])
            title (gen/elements [nil "t" "u"])
            refs (gen/one-of [(gen/return nil)
                              (gen/elements (vec (for [n ["x" "y" "z"]] (fn [_] [{:type "pods" :name n}]))))])
            items (gen/one-of [(gen/return nil) (gen/elements (vec (for [i (range 2)] (fn [_] [(str "item " i)]))))])
            no-verify (gen/elements [nil "p" "q"])]
    (cond-> guard
      (seq reads) (assoc :reads reads)
      (seq values) (assoc :values values)
      effect (assoc :effect effect)
      title (assoc :title title)
      refs (assoc :subjects refs)
      items (assoc :items items)
      no-verify (assoc :no-verify no-verify))))

(defn- behaviour
  "What a composed spec does, comparable across groupings: the data keys as
   they are, and each function key by what it returns in every context.
   :refused when compose refused the parts."
  [compose-fn]
  (try
    (let [spec (compose-fn)
          run (fn [f] (when f (mapv (fn [ctx] (try (f ctx) (catch clojure.lang.ExceptionInfo e [:threw (ex-message e)])))
                                    contexts)))]
      (-> spec
          (dissoc :refusal :verify :items :done :subjects)
          (assoc :refusal (run (:refusal spec))
                 :verify (run (:verify spec))
                 :items (run (:items spec))
                 :done (if (fn? (:done spec)) (run (:done spec)) (:done spec))
                 :subjects (mapv #(if-let [refs (:refs %)] (assoc % :refs (run refs)) %) (:subjects spec))
                 :reads-order (keys (:reads spec))
                 :values-order (keys (:values spec)))))
    (catch clojure.lang.ExceptionInfo e
      (if (change/invalid? e) :refused (throw e)))))

(deftest compose-is-associative
  (check! 300
          (prop/for-all [a gen-part b gen-part c gen-part]
            (= (behaviour #(change/compose (change/compose a b) c))
               (behaviour #(change/compose a (change/compose b c)))
               (behaviour #(change/compose a b c))))))

(deftest composing-a-part-again-changes-nothing
  (check! 300
          (prop/for-all [base gen-part overlay gen-part]
            (let [once (try (change/compose base overlay) (catch clojure.lang.ExceptionInfo _ :refused))]
              (or (= :refused once)
                  (and (= once (change/compose once overlay))
                       (= (behaviour #(change/compose base)) (behaviour #(change/compose base base)))
                       (= (behaviour #(change/compose base overlay))
                          (behaviour #(change/compose base overlay overlay)))))))))
