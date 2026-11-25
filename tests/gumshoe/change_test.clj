;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.change-test
  "gumshoe.change/normalize over every alias, every combination of them and
   every conflict, and the engine running an alias spec and its canonical
   spelling alike."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.runbook :as runbook]))

;; --- fixtures --------------------------------------------------------------------

(defn- refusal [_] nil)
(defn- precondition [_] true)
(defn- targets [_] [{:type "configmaps" :namespace "batch" :name "settings"}])
(defn- pin [_] {:type "secrets" :namespace "batch" :name "token"})
(defn- derive-fn [_] {:stamp "s"})
(defn- recheck [_] {:fresh true})
(defn- connect [_] {:context "c" :cluster "c"})
(defn- announce [_] "message")
(defn- effect-fn [_] (effect/plan (effect/note "go")))
(defn- verify [_] [])
(defn- items [_] ["x"])
(defn- live [_] "read")
(defn- stamp [_] "now")

(def ^:private subject {:type "cronjobs" :facts :suspend})
(def ^:private one {:mode :one :label "CronJob"})
(def ^:private many {:mode :many :label "CronJob"})

(def ^:private alias-keys #{:subject :pin :precondition :empty-message :announce? :derive :recheck})

(defn- refused-message
  "The message normalize refuses spec with, or nil when it accepts it."
  [spec]
  (try (change/normalize spec) nil
       (catch clojure.lang.ExceptionInfo e
         (when (change/invalid? e) (ex-message e)))))

;; --- aliases ---------------------------------------------------------------------

(deftest every-alias-has-its-canonical-form
  (doseq [[label spec expected]
          [[":subject is the :pick entry of :subjects"
            {:subject subject}
            {:subjects [{:from :pick :type "cronjobs" :facts :suspend}]}]
           [":subjects as a function is the :target entry"
            {:subjects targets}
            {:subjects [{:from :target :refs targets}]}]
           [":pin is the :reads entry"
            {:pin pin}
            {:subjects [{:from :reads :ref pin}]}]
           ["the three compose in the order pick, target, reads"
            {:pin pin :subjects targets :subject subject}
            {:subjects [{:from :pick :type "cronjobs" :facts :suspend}
                        {:from :target :refs targets}
                        {:from :reads :ref pin}]}]
           ["a top-level :empty-message moves into :select"
            {:select one :empty-message "none"}
            {:select (assoc one :empty-message "none")}]
           ["the same :empty-message in both places is one"
            {:select (assoc one :empty-message "none") :empty-message "none"}
            {:select (assoc one :empty-message "none")}]
           ["a top-level :empty-message without a :select makes one"
            {:empty-message "none"}
            {:select {:empty-message "none"}}]
           [":announce? false is :announce false"
            {:announce? false}
            {:announce false}]
           [":announce? nil, which the harness reads as false, is :announce false"
            {:announce? nil}
            {:announce false}]
           [":announce? true is the default and is dropped"
            {:announce? true :announce announce}
            {:announce announce}]
           [":announce? true alone leaves nothing"
            {:announce? true}
            {}]
           [":announce false stays"
            {:announce false}
            {:announce false}]
           ["nil aliases are absent"
            {:subject nil :pin nil :precondition nil :empty-message nil :select one}
            {:select one}]]]
    (testing label
      (is (= expected (change/normalize spec))))))

(deftest every-other-key-is-carried-as-it-is
  (let [spec {:title "t" :description "d" :options {:n {:desc "n"}} :prerequisites {}
              :select one :reads {:live live} :values {:stamp stamp} :refusal refusal
              :confirm {:action "act" :destructive? true :separately? true}
              :items items :announce announce :effect effect-fn :verify verify
              :no-verify "why" :recovery "undo it" :confirmed? true
              ::playbook-key 1 :position :after-x}]
    (is (= spec (change/normalize spec)))
    (is (= {:connect connect :select one :effect effect-fn}
           (change/normalize {:connect connect :select one :effect effect-fn})))))

(deftest reads-and-values-and-their-aliases
  (doseq [[label spec expected]
          [["named :reads are carried as they are"
            {:reads {:live live}}
            {:reads {:live live}}]
           ["named :values are carried as they are"
            {:values {:stamp stamp}}
            {:values {:stamp stamp}}]
           ["both, under different names"
            {:reads {:live live} :values {:stamp stamp}}
            {:reads {:live live} :values {:stamp stamp}}]
           [":derive is the :values entry derive-entry"
            {:derive derive-fn}
            {:values {change/derive-entry derive-fn}}]
           [":recheck is the :reads entry recheck-entry"
            {:recheck recheck :refusal refusal}
            {:reads {change/recheck-entry recheck} :refusal refusal}]
           [":derive and :recheck together"
            {:derive derive-fn :recheck recheck :refusal refusal}
            {:values {change/derive-entry derive-fn} :reads {change/recheck-entry recheck} :refusal refusal}]
           ["the canonical spelling of the aliases stays"
            {:values {change/derive-entry derive-fn} :reads {change/recheck-entry recheck} :refusal refusal}
            {:values {change/derive-entry derive-fn} :reads {change/recheck-entry recheck} :refusal refusal}]
           ["empty maps are absent"
            {:reads {} :values {}}
            {}]
           ["nil maps and nil aliases are absent"
            {:reads nil :values nil :derive nil :recheck nil}
            {}]
           ["a var is a function of the context"
            {:values {:stamp #'stamp}}
            {:values {:stamp #'stamp}}]]]
    (testing label
      (is (= expected (change/normalize spec))))))

(deftest a-canonical-alias-entry-normalizes-to-what-its-alias-does
  (is (= (change/normalize {:derive derive-fn})
         (change/normalize {:values {change/derive-entry derive-fn}})))
  (is (= (change/normalize {:recheck recheck :refusal refusal})
         (change/normalize {:reads {change/recheck-entry recheck} :refusal refusal}))))

(deftest the-accessors-split-named-entries-from-the-aliases
  (let [named (change/normalize {:reads {:live live} :values {:stamp stamp} :refusal refusal})
        aliased (change/normalize {:derive derive-fn :recheck recheck :refusal refusal})]
    (is (= {:live live} (change/reads named)))
    (is (= {:stamp stamp} (change/values named)))
    (is (nil? (change/derive-alias named)))
    (is (nil? (change/recheck-alias named)))
    (is (nil? (change/reads aliased)))
    (is (nil? (change/values aliased)))
    (is (= derive-fn (change/derive-alias aliased)))
    (is (= recheck (change/recheck-alias aliased)))
    (is (nil? (change/reads (change/normalize {}))))))

(deftest a-precondition-is-a-refusal-in-the-generic-words
  (let [{:keys [refusal] :as canonical} (change/normalize {:precondition (fn [{:keys [ok]}] ok)})]
    (is (not (contains? canonical :precondition)))
    (is (nil? (refusal {:ok true})))
    (is (= change/unmet (refusal {:ok false})))
    (is (= change/unmet (refusal {:ok nil})) "a falsy precondition is unmet")))

(deftest the-refusal-is-judged-before-the-precondition
  (let [calls (atom [])
        {:keys [refusal]} (change/normalize
                           {:refusal (fn [{:keys [why]}] (swap! calls conj :refusal) why)
                            :precondition (fn [{:keys [ok]}] (swap! calls conj :precondition) ok)})]
    (testing "a reason stops before the precondition runs"
      (is (= "busy" (refusal {:why "busy" :ok true})))
      (is (= [:refusal] @calls)))
    (reset! calls [])
    (testing "no reason hands over to the precondition"
      (is (= change/unmet (refusal {:ok false})))
      (is (= [:refusal :precondition] @calls)))
    (reset! calls [])
    (testing "both passing let the change run"
      (is (nil? (refusal {:ok true})))
      (is (= [:refusal :precondition] @calls)))))

(deftest a-canonical-spelling-normalizes-to-what-its-alias-does
  (doseq [[label alias canonical]
          [[":subject" {:subject subject} {:subjects [(assoc subject :from :pick)]}]
           [":subjects function" {:subjects targets} {:subjects [{:from :target :refs targets}]}]
           [":pin" {:pin pin} {:subjects [{:from :reads :ref pin}]}]
           [":empty-message" {:select one :empty-message "none"} {:select (assoc one :empty-message "none")}]
           [":announce?" {:announce? false} {:announce false}]]]
    (testing label
      (is (= (change/normalize alias) (change/normalize canonical))))))

(deftest the-accessors-read-the-canonical-subjects
  (let [canonical (change/normalize {:subject subject :subjects targets :pin pin})]
    (is (= (assoc subject :from :pick) (change/pick-subject canonical)))
    (is (= targets (:refs (change/subject canonical :target))))
    (is (= pin (change/pin canonical)))
    (is (nil? (change/pin (change/normalize {:subject subject}))))
    (is (nil? (change/pick-subject (change/normalize {}))))))

;; --- conflicts -------------------------------------------------------------------

(deftest an-ambiguous-or-unrunnable-spec-is-refused
  (doseq [[label spec message]
          [["canonical :subjects beside :subject"
            {:subjects [{:from :target :refs targets}] :subject subject}
            #"names its subjects in :subjects and again as :subject"]
           ["canonical :subjects beside :pin"
            {:subjects [{:from :pick :type "cronjobs"}] :pin pin}
            #"names its subjects in :subjects and again as :pin"]
           ["an entry with an unknown :from"
            {:subjects [{:from :elsewhere :refs targets}]}
            #":from :elsewhere - write :pick, :target or :reads"]
           ["an entry without a :from"
            {:subjects [{:type "cronjobs"}]}
            #":from nil"]
           ["an entry that is not a map"
            {:subjects ["cronjobs"]}
            #":from nil"]
           ["a :from named twice"
            {:subjects [{:from :reads :ref pin} {:from :reads :ref pin}]}
            #"name :from :reads 2 times"]
           ["two different empty messages"
            {:select (assoc one :empty-message "inside") :empty-message "outside"}
            #"two empty messages, \"outside\" at the top and \"inside\" in :select"]
           ["a changelog message beside :announce? false"
            {:announce announce :announce? false}
            #"an :announce message and :announce\? false"]
           [":announce false beside :announce? true"
            {:announce false :announce? true}
            #":announce false and :announce\? true"]
           ["a :recheck with nothing to judge it"
            {:recheck recheck}
            #"a :recheck is judged by :refusal or :precondition, and the spec has neither"]
           ["a :subject on a :many select"
            {:select many :subject subject}
            #"a :subject needs a select that picks one object, not a :many select"]
           [":connect beside :subject"
            {:connect connect :subject subject}
            #"so a spec with :connect takes neither"]
           [":connect beside a :subjects function"
            {:connect connect :subjects targets}
            #"so a spec with :connect takes neither"]
           [":connect beside :pin"
            {:connect connect :pin pin}
            #"so a spec with :connect takes neither"]
           [":connect beside canonical :subjects"
            {:connect connect :subjects [{:from :reads :ref pin}]}
            #"so a spec with :connect takes neither"]
           ["a spec that is not a map"
            [:subject subject]
            #"a change spec is a map"]
           [":derive beside :values"
            {:derive derive-fn :values {:stamp stamp}}
            #"the spec has :derive and :values - write what :derive returns as :values entries"]
           [":recheck beside :reads"
            {:recheck recheck :reads {:live live} :refusal refusal}
            #"the spec has :recheck and :reads - write what :recheck returns as :reads entries"]
           [":derive beside a named read"
            {:derive derive-fn :reads {:live live}}
            #"the spec has :derive beside the named entries \[:live\], which a :derive map could hide"]
           [":recheck beside a named value"
            {:recheck recheck :values {:stamp stamp} :refusal refusal}
            #"the spec has :recheck beside the named entries \[:stamp\], which a :recheck map could hide"]
           ["a canonical :derive entry beside a named value"
            {:values {change/derive-entry derive-fn :stamp stamp}}
            #"the spec has :derive beside the named entries \[:stamp\]"]
           ["a canonical :recheck entry beside a named read"
            {:reads {change/recheck-entry recheck :live live} :refusal refusal}
            #"the spec has :recheck beside the named entries \[:live\]"]
           ["one name as a read and a value"
            {:reads {:live live} :values {:live stamp}}
            #":live is both a read and a value"]
           ["a read that would hide the subject's object"
            {:reads {:object live}}
            #"the :reads entry :object would hide the :object the engine hands every callback"]
           ["a value that would hide the context"
            {:values {:context stamp}}
            #"the :values entry :context would hide the :context"]
           ["a value that would hide the pick"
            {:values {:target stamp}}
            #"the :values entry :target would hide"]
           ["a read that would hide the subjects"
            {:reads {:objects live}}
            #"the :reads entry :objects would hide"]
           ["a name that is not a keyword"
            {:reads {"live" live}}
            #":reads names its entries with keywords, not \"live\""]
           ["an entry that is not a function"
            {:values {:stamp "now"}}
            #"the :values entry :stamp is a function of the context, not \"now\""]
           ["an entry that is a map"
            {:values {:stamp {:a 1}}}
            #"the :values entry :stamp is a function of the context"]
           [":reads that is not a map"
            {:reads [live]}
            #":reads is a map of name to \(fn \[ctx\] -> value\)"]
           [":values as one function"
            {:values stamp}
            #":values is a map of name to"]
           ["the :derive entry among the reads"
            {:reads {change/derive-entry derive-fn}}
            #":gumshoe.change/derive is reserved and is not a :reads entry"]
           ["the :recheck entry among the values"
            {:values {change/recheck-entry recheck}}
            #":gumshoe.change/recheck is reserved and is not a :values entry"]
           ["another reserved name"
            {:values {::change/elsewhere stamp}}
            #":gumshoe.change/elsewhere is reserved"]
           ["a canonical :recheck entry with nothing to judge it"
            {:reads {change/recheck-entry recheck}}
            #"a :recheck is judged by :refusal or :precondition, and the spec has neither"]]]
    (testing label
      (let [refused (refused-message spec)]
        (is (some? refused) "it is refused")
        (when refused (is (re-find message refused)))))))

(deftest a-recheck-judged-by-a-precondition-alone-is-runnable
  (is (fn? (:refusal (change/normalize {:recheck recheck :precondition precondition})))))

(deftest a-refused-spec-fails-when-the-book-loads
  (binding [runbook/*loading* true]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"two empty messages"
                          (mutation/book {:description "d" :select (assoc one :empty-message "a")
                                          :empty-message "b" :confirm {:action "x"} :effect effect-fn})))))

;; --- combinations ----------------------------------------------------------------

(def ^:private fragments
  "One fragment per alias or canonical key normalize reads, each a spelling
   a book may use; every subset of them is a spec normalize is run over."
  [{:subject subject}
   {:subjects targets}
   {:pin pin}
   {:subjects [{:from :reads :ref pin}]}
   {:precondition precondition}
   {:refusal refusal}
   {:recheck recheck}
   {:empty-message "top"}
   {:select (assoc many :empty-message "inside")}
   {:announce? false}
   {:announce announce}
   {:connect connect}
   {:derive derive-fn :no-verify "why" :confirm {:action "x" :destructive? false}}
   {:reads {:live live}}
   {:values {:stamp stamp}}])

(def ^:private combinations
  "Every subset of fragments merged into one spec (fragments naming the
   same key merge left to right), 2^15 of them."
  (for [mask (range (bit-shift-left 1 (count fragments)))]
    (apply merge {} (keep-indexed (fn [i f] (when (bit-test mask i) f)) fragments))))

(deftest normalize-is-idempotent-over-every-combination
  (let [outcomes (for [spec combinations]
                   (if-let [refused (refused-message spec)]
                     {:refused refused}
                     (let [canonical (change/normalize spec)]
                       {:spec spec :canonical canonical :again (change/normalize canonical)})))]
    (is (< 100 (count (remove :refused outcomes))) "many combinations are accepted")
    (is (< 100 (count (filter :refused outcomes))) "many combinations are refused")
    (doseq [{:keys [spec canonical again]} (remove :refused outcomes)]
      (is (= canonical again) (str "normalize is idempotent for " (keys spec)))
      (is (empty? (filter alias-keys (keys canonical))) (str "no alias survives for " (keys spec)))
      (is (not-any? #{:derive :recheck} (keys canonical)) (str ":derive and :recheck become entries for " (keys spec)))
      (is (every? #(or (nil? %) (map? %)) [(:reads canonical) (:values canonical)]))
      (is (or (nil? (:subjects canonical)) (vector? (:subjects canonical))))
      (is (not (contains? canonical :announce?))))))

(deftest a-refused-combination-is-refused-for-a-reason-normalize-names
  (let [refusals (set (keep refused-message combinations))]
    (is (every? string? refusals))
    (is (= #{"a :subject needs a select that picks one object, not a :many select"
             "a :subject, :subjects or :pin reads its objects with kubectl, so a spec with :connect takes neither"
             "a :recheck is judged by :refusal or :precondition, and the spec has neither"
             "the spec has an :announce message and :announce? false, so it is unclear whether it posts to the changelog - drop one"
             "the spec has two empty messages, \"top\" at the top and \"inside\" in :select - keep the one in :select"
             "the spec names its subjects in :subjects and again as :subject - write one or the other"
             "the spec names its subjects in :subjects and again as :pin - write one or the other"
             "the spec has :derive and :values - write what :derive returns as :values entries"
             "the spec has :recheck and :reads - write what :recheck returns as :reads entries"
             "the spec has :derive beside the named entries [:live], which a :derive map could hide - write what :derive returns as named :values and :reads"
             "the spec has :recheck beside the named entries [:stamp], which a :recheck map could hide - write what :recheck returns as named :values and :reads"}
           refusals))))

;; --- the engine reads only the canonical map -------------------------------------

(defn- configmap
  [generation]
  {:metadata {:namespace "batch" :name "settings" :uid "u" :generation generation}})

(defn- run
  "Runs spec on target with each read answered by (read ref call-number) and
   flow/change! applying its precondition. Returns what the run printed and
   returned, the confirmation and the reads it made."
  [spec target read]
  (let [reads (atom []) seen (atom nil)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  mutation/fetch-pinned (fn [_ ref]
                                          (swap! reads conj ref)
                                          (read ref (count @reads)))
                  flow/change! (fn [config]
                                 (reset! seen config)
                                 (if-let [p (:precondition config)]
                                   (or (p) (do (println "flow aborts") false))
                                   true))]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out]
                     (mutation/run-on! (merge {:select {:mode :namespaced :label "CronJob"}
                                               :confirm {:action "suspend a CronJob"}
                                               :effect effect-fn}
                                              spec)
                                       target {} nil))]
        {:result result :out (str out) :reads @reads
         :confirmation (:confirmation @seen)}))))

(def ^:private cronjob-ref {:type "cronjobs" :namespace "batch" :name "nightly"})

(defn- moving
  "Every object answers generation 1 before the confirmation and 2 after it."
  [_ n]
  (configmap (if (<= n 1) 1 2)))

(deftest the-engine-runs-an-alias-spec-and-its-canonical-spelling-alike
  (doseq [[label alias canonical read confirmed?]
          [["a subject that drifted is refused in the single-subject words"
            {:subject {:type "cronjobs"}}
            {:subjects [{:from :pick :type "cronjobs"}]}
            (fn [_ n] (configmap n))]
           ["a pin that drifted is refused in the same words"
            {:pin (constantly cronjob-ref)}
            {:subjects [{:from :reads :ref (constantly cronjob-ref)}]}
            moving]
           ["several subjects name the one that drifted"
            {:subjects (constantly [cronjob-ref])}
            {:subjects [{:from :target :refs (constantly [cronjob-ref])}]}
            moving]
           ["a precondition that fails refuses before the confirmation"
            {:precondition (constantly false)}
            {:refusal (constantly change/unmet)}
            (fn [_ _] nil)
            false]
           ["a refusal before a precondition prints its reason before the confirmation"
            {:refusal (constantly "busy") :precondition (constantly true)}
            {:refusal (constantly "busy")}
            (fn [_ _] nil)
            false]
           ["a covered precondition that fails aborts in flow's words alone"
            {:precondition (constantly false) :confirmed? true}
            {:refusal (constantly change/unmet) :confirmed? true}
            (fn [_ _] nil)]
           ["a covered refusal before a precondition prints its reason"
            {:refusal (constantly "busy") :precondition (constantly true) :confirmed? true}
            {:refusal (constantly "busy") :confirmed? true}
            (fn [_ _] nil)]
           ["a subject that holds runs"
            {:subject {:type "cronjobs"} :precondition (constantly true)}
            {:subjects [{:from :pick :type "cronjobs"}] :refusal (constantly nil)}
            (fn [_ _] (configmap 1))]]]
    (testing label
      (let [a (run alias "batch/nightly" read)
            c (run canonical "batch/nightly" read)]
        (is (= a c))
        (if (false? confirmed?)
          (is (nil? (:confirmation a)) "the change was refused before its confirmation")
          (is (some? (:confirmation a)) "the change reached its confirmation"))))))

(deftest the-words-of-each-refusal-stay-as-they-were
  (testing "a subject: the object is \"it\""
    (is (= "refusing: its spec changed after the confirmation (generation 1, now 2)\nflow aborts\n"
           (:out (run {:subject {:type "cronjobs"}} "batch/nightly" moving)))))
  (testing "several subjects: the refusal names the object"
    (is (= "refusing: cronjobs batch/nightly: its spec changed after the confirmation (generation 1, now 2)\nflow aborts\n"
           (:out (run {:subjects (constantly [cronjob-ref])} "batch/nightly" moving)))))
  (testing "a precondition prints nothing of its own after the confirmation"
    (is (= "flow aborts\n" (:out (run {:precondition (constantly false) :confirmed? true} "batch/nightly" (fn [_ _] nil))))))
  (testing "a precondition that does not hold before the confirmation refuses in the engine's words"
    (is (= (str "refusing: " mutation/unmet-before-confirmation "\n")
           (:out (run {:precondition (constantly false)} "batch/nightly" (fn [_ _] nil)))))))

(deftest a-top-level-or-select-empty-message-is-what-run!-shows
  (doseq [spec [{:select {:mode :one :label "CronJob" :candidates (constantly [])} :empty-message "none here"}
                {:select {:mode :one :label "CronJob" :candidates (constantly []) :empty-message "none here"}}]]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out]
                     (mutation/run! (merge {:confirm {:action "x"} :effect effect-fn} spec) {} nil))]
        (is (true? result))
        (is (re-find #"none here" (str out)))))))

(deftest help-reads-the-canonical-map
  (let [base {:select one :confirm {:action "x"}}
        check "  checks again right before acting that what was confirmed still holds"]
    (doseq [spec [{:subject subject} {:subjects targets} {:pin pin} {:precondition precondition}
                  {:refusal refusal} {:subjects [{:from :reads :ref pin}]}]]
      (is (some #{check} (mutation/details (merge base spec))) (str (keys spec))))
    (is (not-any? #{check} (mutation/details base)))
    (is (some #{check} (mutation/details (merge base {:recheck recheck :refusal refusal}))))
    (is (not-any? #{check} (mutation/details (merge base {:reads {:live live} :values {:stamp stamp}})))
        "a read nothing judges is not checked again")
    (is (some #{check} (mutation/details (merge base {:reads {:live live} :refusal refusal}))))
    (is (not-any? #{"  announces the change to the changelog"}
                  (mutation/details (merge base {:announce? false}))))))
