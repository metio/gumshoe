;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dsl-test
  "gumshoe.dsl/compile: what each word compiles to, the precise refusal of
   every key, word, name and argument it does not know, and the property
   that every change it accepts is pinned and fails closed unless it opts
   out with a reason."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.change :as change]
            [gumshoe.conformance :as conformance]
            [gumshoe.dsl :as dsl]
            [gumshoe.dsl.registry :as registry]
            [gumshoe.shell :as shell]
            [gumshoe.verify :as verify]))

(def ^:private base
  "A whole change, the cronjob suspend book's DSL form."
  {:gumshoe/dsl :v0-experimental
   :description "Suspends a CronJob so no new Job starts"
   :target {:pick :namespaced :label "CronJob" :type "cronjobs"
            :namespace-flag :namespace :name-flag :job
            :candidates {:unless :state :order :listed}
            :empty "every CronJob is suspended already"}
   :prerequisites {:installed-tools ["kubectl"] :kubectl-can-patch ["cronjobs"]}
   :state {:holds [:field [:spec :suspend] := true]
           :says "CronJob {target} is suspended" :timeout 30 :interval 3}
   :refuse [{:when [:state] :says "it is suspended already"}]
   :confirm {:action "suspend a CronJob"}
   :items [:call :gumshoe.workloads/suspend-items [:object] true]
   :announce "Suspend CronJob {target}"
   :effect [[:kubectl :patch {:resource "cronjob" :merge {:spec {:suspend true}}}]]})

(def ^:private deleting
  "A destructive change: base deleting its CronJob."
  (-> base
      (assoc :state {:holds :absent :says "{target} is gone"}
             :effect [[:kubectl :delete]]
             :confirm {:action "delete a CronJob"}
             :recovery "recreate {target} from its source")
      (dissoc :refuse :items)))

(def ^:private cronjob
  {:metadata {:namespace "batch" :name "nightly" :uid "u-1" :generation 1 :resourceVersion "1"}
   :spec {:suspend false :schedule "0 3 * * *"}})

(def ^:private ctx
  {:context "ctx" :cluster "cluster" :target "batch/nightly" :opts {}
   :ref {:type "cronjobs" :namespace "batch" :name "nightly"} :object cronjob})

(defn- refusal
  "The ex-data and message compile refuses dsl with, or nil when it accepts it."
  [dsl]
  (try (dsl/compile dsl) nil
       (catch clojure.lang.ExceptionInfo e
         (when (dsl/invalid? e) {:path (:path (ex-data e)) :message (ex-message e)}))))

;; --- what each word compiles to ---------------------------------------------------

(deftest a-change-compiles-to-a-canonical-change-map
  (let [c (dsl/compile base)]
    (is (= c (change/normalize c)) "normalize finds nothing to change")
    (is (= [{:from :pick :type "cronjobs"}] (:subjects c)) "the picked object is the change's subject, so it is pinned")
    (is (true? (:done c)) "the target state is the skip")
    (is (= {:action "suspend a CronJob" :destructive? false} (:confirm c)))
    (is (= {:mode :namespaced :label "CronJob" :namespace-flag :namespace :name-flag :job
            :empty-message "every CronJob is suspended already"}
           (dissoc (:select c) :candidates)))
    (is (= #{:namespace :job} (set (keys (:options c)))) "the target's flags become options")
    (is (= [[:kubectl "ctx" "--namespace=batch" "patch" "cronjob" "nightly" "--type=merge"
             "--patch={\"spec\":{\"suspend\":true}}"]]
           ((:effect c) ctx)))
    (is (= "Suspend CronJob batch/nightly" ((:announce c) ctx)))
    (is (= ["suspend CronJob batch/nightly - no new Job starts until it is resumed"] ((:items c) ctx)))
    (is (nil? ((:refusal c) ctx)))
    (is (= "it is suspended already" ((:refusal c) (assoc-in ctx [:object :spec :suspend] true))))
    (is (= "the CronJob no longer exists" ((:refusal c) (assoc ctx :object nil))))
    (let [[check & more] ((:verify c) ctx)]
      (is (nil? more))
      (is (= {:description "CronJob batch/nightly is suspended" :timeout 30 :interval 3}
             (select-keys check [:description :timeout :interval :verifies-absence?]))))))

(def ^:private accepted
  "[what, a change, what its compiled map must satisfy]."
  [["a :delete makes the change destructive when :destructive? is not stated"
    deleting
    #(true? (get-in % [:confirm :destructive?]))]
   ["an :absent state verifies an absence"
    deleting
    #(true? (:verifies-absence? (first ((:verify %) ctx))))]
   ["the recovery is a template"
    deleting
    #(= "recreate batch/nightly from its source" ((:recovery %) ctx))]
   ["an opt-out of :done drops the skip and keeps the post-check"
    (assoc base :opt-out {:done "it is cheap to apply again"})
    #(and (nil? (:done %)) (some? (:verify %)))]
   ["an opt-out of :verify without a state is the :no-verify reason"
    (-> base (dissoc :state :refuse) (assoc :target (assoc (:target base) :candidates [:call :gumshoe.deletion/candidates [:context]])
                                            :opt-out {:verify "the controller reports nothing to wait for"}))
    #(and (= "the controller reports nothing to wait for" (:no-verify %)) (nil? (:verify %)) (nil? (:done %)))]
   ["an opt-out of :destructive confirms a delete with a keypress"
    (-> deleting (assoc-in [:confirm :destructive?] false) (assoc :opt-out {:destructive "a scratch namespace"}) (dissoc :recovery))
    #(false? (get-in % [:confirm :destructive?]))]
   ["an opt-out of :recovery leaves the fallback guidance"
    (-> deleting (dissoc :recovery) (assoc :opt-out {:recovery "the object is recreated by its controller"}))
    #(nil? (:recovery %))]
   [":announce false posts nothing"
    (assoc base :announce false)
    #(false? (:announce %))]
   [":items may be templates"
    (assoc base :items ["suspend {target}" "schedule {object.spec.schedule}"])
    #(= ["suspend batch/nightly" "schedule 0 3 * * *"] ((:items %) ctx))]
   ["a :separately? confirmation is kept"
    (assoc-in base [:confirm :separately?] true)
    #(true? (get-in % [:confirm :separately?]))]
   ["a :title is kept"
    (assoc base :title "Suspend")
    #(= "Suspend" (:title %))]
   ["a cluster-scoped patch names no namespace"
    (assoc base :target {:pick :one :label "Node" :type "nodes" :flag :node :candidates {:unless :state}})
    #(= [[:kubectl "ctx" "patch" "cronjob" "n1" "--type=merge" "--patch={\"spec\":{\"suspend\":true}}"]]
        ((:effect %) (assoc ctx :ref {:type "nodes" :name "n1"})))]
   ["a patch without :resource names the subject's type, and its body takes expressions"
    (assoc base :effect [[:kubectl :patch {:merge {:spec {:suspend [:opt :job]}}}]])
    #(= [[:kubectl "ctx" "--namespace=batch" "patch" "cronjobs" "nightly" "--type=merge" "--patch={\"spec\":{\"suspend\":\"x\"}}"]]
        ((:effect %) (assoc ctx :opts {:job "x"})))]
   ["a scale names the singular type and the count"
    (assoc base :effect [[:kubectl :scale {:replicas 3}]])
    #(= [[:kubectl "ctx" "--namespace=batch" "scale" "cronjob/nightly" "--replicas=3"]] ((:effect %) ctx))]
   ["an annotate writes each annotation sorted, removes a nil one and may overwrite"
    (assoc base :effect [[:kubectl :annotate {:annotations {"b" [:target] :a.io/x "1" "gone" nil} :overwrite true}]])
    #(= [[:kubectl "ctx" "--namespace=batch" "annotate" "cronjobs" "nightly" "a.io/x=1" "b=batch/nightly" "gone-" "--overwrite"]]
        ((:effect %) ctx))]
   ["a delete does not wait"
    deleting
    #(= [[:kubectl "ctx" "--namespace=batch" "delete" "cronjobs" "nightly" "--wait=false"]] ((:effect %) ctx))]
   ["several effects run in order as one plan"
    (assoc base :effect [[:kubectl :annotate {:annotations {"a" "1"}}] [:kubectl :scale {:replicas 0}]])
    #(= 2 (count ((:effect %) ctx)))]
   ["a value reads a flag, and a template renders it"
    (assoc base :values {:why [:opt :job]} :announce "Suspend {target}: {value.why}")
    #(= "Suspend batch/nightly: x" ((:announce %) (assoc ctx :why "x")))]
   ["a template renders a missing value as format renders nil"
    (assoc base :values {:why [:opt :job]} :announce "{value.why}")
    #(= "null" ((:announce %) ctx))]
   ["a field compared with a missing number is false, never an error"
    (assoc-in base [:state :holds] [:field [:spec :replicas] :> 0])
    #(nil? ((:refusal %) ctx))]])

(deftest each-word-compiles-to-what-it-says
  (doseq [[what dsl holds?] accepted]
    (testing what
      (let [c (try (dsl/compile dsl) (catch Exception e e))]
        (is (map? c) (str (when (instance? Exception c) (ex-message c))))
        (when (map? c) (is (holds? c)))))))

(deftest predicates-judge-the-object
  (let [judge (fn [holds object]
                (let [c (dsl/compile (assoc base :state {:holds holds :says "x"} :refuse [{:when [:state] :says "holds"}]))]
                  (= "holds" ((:refusal c) (assoc ctx :object object)))))
        o (assoc-in cronjob [:spec :replicas] 2)]
    (is (judge [:field [:spec :replicas] := 2] o))
    (is (judge [:field [:spec :replicas] :not= 3] o))
    (is (judge [:field [:spec :replicas] :< 3] o))
    (is (judge [:field [:spec :replicas] :<= 2] o))
    (is (judge [:field [:spec :replicas] :> 1] o))
    (is (judge [:field [:spec :replicas] :>= 2] o))
    (is (not (judge [:field [:spec :replicas] :> 2] o)))
    (is (judge [:= [:object :metadata :name] "nightly"] o))
    (is (judge [:some? [:object :spec :schedule]] o))
    (is (judge [:nil? [:object :spec :startingDeadlineSeconds]] o))
    (is (judge [:and [:field [:spec :replicas] := 2] [:not [:field [:spec :suspend] := true]]] o))
    (is (judge [:or [:field [:spec :replicas] := 9] [:field [:spec :suspend] := false]] o))
    (is (not (judge [:or [:field [:spec :replicas] := 9]] o)))
    (is (judge [:call :gumshoe.workloads/scaled? [:object] 0] (assoc o :status {:observedGeneration 1})) "a registered predicate")
    (is (not (judge [:field [:spec :suspend] :not= true] nil)) "[:state] never holds for an object that does not exist")))

(deftest refusals-are-judged-in-order-and-the-gone-refusal-last
  (let [c (dsl/compile (assoc base
                              :refuse [{:when [:field [:spec :schedule] := "0 3 * * *"] :says "it runs at {object.spec.schedule}"}
                                       {:when [:absent] :says "first: gone"}]
                              :target (assoc (:target base) :gone "it is gone")))]
    (is (= "it runs at 0 3 * * *" ((:refusal c) ctx)))
    (is (= "first: gone" ((:refusal c) (assoc ctx :object nil))) "a declared refusal comes before the target's gone refusal")
    (is (= "it is gone" ((:refusal (dsl/compile (dissoc (assoc base :target (assoc (:target base) :gone "it is gone")) :refuse)))
                         (assoc ctx :object nil))))))

(deftest a-reason-refusal-calls-a-registered-function-over-a-declared-read
  (let [c (dsl/compile (assoc base
                              :reads {:hpas [:list "horizontalpodautoscalers" {:namespace [:ref :namespace]}]}
                              :refuse [{:reason [:call :gumshoe.workloads/scale-refusal-given "Deployment" [:object] [:read :hpas] 2]}]))]
    (is (= "could not read the HorizontalPodAutoscalers in its namespace, so whether one manages its replicas is unknown"
           ((:refusal c) (assoc ctx :hpas {:ok? false}))))))

(deftest reads-run-kubectl-and-candidates-filter-by-the-state
  (let [listed {:items [(assoc-in cronjob [:metadata :name] "z") (assoc-in cronjob [:spec :suspend] true)
                        (assoc-in cronjob [:metadata :name] "a")]}
        commands (atom [])]
    (with-redefs [shell/execute (fn [& argv]
                                  (swap! commands conj (vec argv))
                                  {:exit 0 :err "" :out (json/generate-string (if (some #{"nightly"} argv) cronjob listed))})]
      (let [c (dsl/compile (assoc base :reads {:pods [:list "pods" {:namespace [:ref :namespace]}]
                                               :one [:get "cronjobs" [:ref :name] {:namespace "batch"}]
                                               :node [:get "nodes" "n1"]}))]
        (is (= ["batch/z" "batch/a"] ((:candidates (:select c)) "ctx")) "listed in the API's order, the suspended one left out")
        (is (= ["batch/a" "batch/z"] ((:candidates (:select (dsl/compile (assoc-in base [:target :candidates :order] :sorted)))) "ctx")))
        (is (= {:ok? true :items (:items listed)} ((get-in c [:reads :pods]) ctx)))
        (is (= cronjob ((get-in c [:reads :one]) ctx)))
        ((get-in c [:reads :node]) ctx)
        (is (some #(and (= ["get" "nodes" "n1" "--output=json"] (take-last 4 %))
                        (not-any? (fn [arg] (str/starts-with? arg "--namespace=")) %))
                  @commands)
            "a read without a namespace reads a cluster-scoped object")))))

;; --- refusals ------------------------------------------------------------------------

(def ^:private refused
  "[a change compile must refuse, the :path it names, a word its message holds]."
  [[(dissoc base :gumshoe/dsl) [:gumshoe/dsl] "declares no version - declare :gumshoe/dsl :v0-experimental"]
   [(assoc base :gumshoe/dsl 0) [:gumshoe/dsl] "declares :gumshoe/dsl 0, and this compiles :v0-experimental"]
   [(assoc base :gumshoe/dsl 1) [:gumshoe/dsl] "declares :gumshoe/dsl 1"]
   [(assoc base :gumshoe/dsl :v0) [:gumshoe/dsl] "declares :gumshoe/dsl :v0,"]
   [(assoc base :gumshoe/dsl "v0-experimental") [:gumshoe/dsl] "declares :gumshoe/dsl \"v0-experimental\""]
   [(assoc base :gumshoe/dsl 'v0-experimental) [:gumshoe/dsl] "declares :gumshoe/dsl v0-experimental,"]
   [(assoc base :gumshoe/dsl :v1-experimental :announce 'format) [:gumshoe/dsl] "may change in any release"]
   [[] [] "a change is a map"]
   [(assoc base :steps []) [:steps] ":steps is not a key of a change"]
   [(assoc base :announce 'format) [:announce] "format is a symbol"]
   [(assoc base :announce str) [:announce] "is code"]
   [(assoc base :items '(1 2)) [:items] "vectors and maps"]
   [(assoc base :items #{1}) [:items] "vectors and maps"]
   [(assoc base :description "") [:description] "non-blank"]
   [(assoc base :title " ") [:title] "non-blank"]
   [(dissoc base :prerequisites) [:prerequisites] "as a map"]
   [(assoc base :target "cronjobs") [:target] "the target is a map"]
   [(assoc-in base [:target :pick] :many) [:target :pick] "write :one or :namespaced"]
   [(assoc-in base [:target :flag] :job) [:target :flag] "not a key of a :namespaced target"]
   [(assoc-in base [:target :label] nil) [:target :label] "non-blank"]
   [(update base :target dissoc :type) [:target] "neither the :type"]
   [(assoc-in base [:target :type] "") [:target :type] "non-blank"]
   [(assoc-in base [:target :invalid] "") [:target :invalid] "non-blank"]
   [(assoc-in base [:target :empty] "") [:target :empty] "non-blank"]
   [(assoc base :target {:pick :one :label "Node" :type "nodes" :candidates {:unless :state}}) [:target :flag] "names the flag"]
   [(update base :target dissoc :name-flag) [:target :name-flag] "as a keyword"]
   [(update base :target dissoc :candidates) [:target :candidates] "names its :candidates"]
   [(assoc-in base [:target :candidates] {:unless :state :sort true}) [:target :candidates :sort] "not a key"]
   [(assoc-in base [:target :candidates] {:unless :done}) [:target :candidates :unless] "not :done"]
   [(assoc-in base [:target :candidates] {:unless :state :order :random}) [:target :candidates :order] "no order"]
   [(-> base (update :target dissoc :type) (assoc-in [:target :parse] :gumshoe.workloads/parse-pick))
    [:target :candidates] "lists the objects of the target's :type"]
   [(assoc-in base [:target :parse] :gumshoe.workloads/parse-pick) [:target :candidates] "which a :parse may not read"]
   [(-> base (dissoc :state :refuse) (assoc :opt-out {:verify "nothing to wait for here"})) [:target :candidates] "needs a :state"]
   [(assoc base :values {:n [:opt :job]} :state {:holds [:field [:spec :suspend] := [:value :n]] :says "x"})
    [:state :holds 3 0] "not available in the target's :candidates {:unless :state}"]
   [(assoc-in base [:target :candidates] "all") [:target :candidates] "no candidates form"]
   [(assoc-in base [:target :candidates] [:list "cronjobs"]) [:target :candidates] "no candidates form"]
   [(assoc-in base [:target :candidates] [:call :gumshoe.deletion/dry-run [:context] [:context]])
    [:target :candidates 1] "is a :read function, and this place takes :candidates"]
   [(-> base (assoc-in [:target :parse] :nope/parse) (assoc-in [:target :candidates] [:call :gumshoe.deletion/candidates [:context]]))
    [:target :parse] "not a registered function"]
   [(-> base (assoc-in [:target :parse] :gumshoe.workloads/scaled?) (assoc-in [:target :candidates] [:call :gumshoe.deletion/candidates [:context]]))
    [:target :parse] "is a :predicate function"]
   [(assoc base :opt-out {:everything "trust me"}) [:opt-out :everything] "not a key of the :opt-out"]
   [(assoc base :opt-out {:done ""}) [:opt-out :done] "non-blank"]
   [(dissoc base :confirm) [:confirm] "the :confirm is a map"]
   [(assoc-in base [:confirm :yes?] true) [:confirm :yes?] "not a key of the :confirm"]
   [(assoc-in base [:confirm :action] "") [:confirm :action] "non-blank"]
   [(assoc-in base [:confirm :destructive?] "no") [:confirm :destructive?] "true or false"]
   [(assoc base :options {:replicas 3}) [:options] "map of flag keyword"]
   [(assoc base :options {:other {:alias :n}}) [:options] "options share an alias"]
   [(assoc base :reads [:list "pods"]) [:reads] "map of name"]
   [(assoc base :reads {:a/b [:get "pods" "x"]}) [:reads :a/b] "unqualified keyword"]
   [(assoc base :values {:target [:opt :job]}) [:values :target] "the engine binds itself"]
   [(assoc base :reads {:x [:get "pods" "x"]} :values {:x [:opt :job]}) [:reads :x] "both a read and a value"]
   [(assoc base :effect []) [:effect] "one or more effects"]
   [(assoc base :effect [[:ssh "reboot"]]) [:effect 0] "no effect"]
   [(assoc base :effect [[:kubectl :cordon]]) [:effect 0 1] "no kubectl verb"]
   [(assoc base :effect [[:kubectl :delete {:force true}]]) [:effect 0 2 :force] "not a key of the options of :delete"]
   [(assoc base :effect [[:kubectl :patch {}]]) [:effect 0 2 :merge] "names its merge patch"]
   [(assoc base :effect [[:kubectl :patch {:merge {} :resource ""}]]) [:effect 0 2 :resource] "non-blank"]
   [(assoc base :effect [[:kubectl :scale {}]]) [:effect 0 2] "names the count"]
   [(assoc base :effect [[:kubectl :annotate {:annotations {}}]]) [:effect 0 2 :annotations] "one or more"]
   [(assoc base :effect [[:kubectl :annotate {:annotations {"a" "1"} :overwrite "yes"}]]) [:effect 0 2 :overwrite] "true or false"]
   [(assoc base :effect [[:kubectl :annotate {:annotations {1 "1"}}]]) [:effect 0 2 :annotations 1] "keyed by"]
   [(assoc base :effect [[:kubectl :delete {} {}]]) [:effect 0] "one map of its options"]
   [(assoc-in base [:state :wait] 3) [:state :wait] "not a key of the :state"]
   [(update base :state dissoc :holds) [:state :holds] "says what holds"]
   [(update base :state dissoc :says) [:state :says] "non-blank"]
   [(assoc-in base [:state :timeout] 0) [:state :timeout] "above zero"]
   [(assoc-in base [:state :holds] "suspended") [:state :holds] "no predicate"]
   [(assoc-in base [:state :holds] [:matches [:spec] "x"]) [:state :holds 0] "no predicate - the predicates are"]
   [(assoc-in base [:state :holds] [:field [:spec :suspend] :=]) [:state :holds] "[:field path op value]"]
   [(assoc-in base [:state :holds] [:field :spec := true]) [:state :holds 1] "a path vector"]
   [(assoc-in base [:state :holds] [:field [:spec :suspend] :is true]) [:state :holds 2] "no comparison"]
   [(assoc-in base [:state :holds] [:= [:object]]) [:state :holds] "[:= a b]"]
   [(assoc-in base [:state :holds] [:some?]) [:state :holds] "[:some? expr]"]
   [(assoc-in base [:state :holds] [:not]) [:state :holds] "[:not predicate]"]
   [(assoc-in base [:state :holds] [:and]) [:state :holds] "one or more predicates"]
   [(assoc-in base [:state :holds] [:absent]) [:state :holds] "[:absent] is not available"]
   [(assoc-in base [:state :holds] [:state]) [:state :holds] "[:state] is not available"]
   [(assoc base :refuse [{:when [:absent :now] :says "x"}]) [:refuse 0 :when] "[:absent]"]
   [(assoc-in base [:state :holds] [:call :gumshoe.workloads/scaled? [:object]]) [:state :holds 1] "takes 2 argument(s) (workload, replicas), not 1"]
   [(assoc-in base [:state :holds] [:call :gumshoe.workloads/scale-items [:object] 1 2]) [:state :holds 1] "is a :items function"]
   [(assoc-in base [:state :holds] [:call 'scaled? [:object]]) [:state :holds 1] "is a symbol"]
   [(assoc-in base [:state :holds] [:call "scaled?" [:object]]) [:state :holds 1] "qualified keyword"]
   [(assoc base :announce "{opt.nope}") [:announce] "names no declared option"]
   [(assoc base :items [:call :gumshoe.workloads/suspend-items [:eval "x"] true]) [:items 2 0] "no expression"]
   [(assoc base :items [:call :gumshoe.workloads/suspend-items [:context] true]) [:items 2 0] "not available in :items"]
   [(assoc base :items [:call :gumshoe.workloads/suspend-items [1 2] true]) [:items 2] "is a form"]
   [(assoc base :items [:call :gumshoe.workloads/suspend-items [:value :x] true]) [:items 2 1] "names no declared :values entry"]
   [(assoc base :items [:call :gumshoe.workloads/suspend-items [:read :x] true]) [:items 2 1] "names no declared :reads entry"]
   [(assoc base :items [:call :gumshoe.workloads/suspend-items [:value] true]) [:items 2] "names what it reads"]
   [(assoc base :items [:call :gumshoe.workloads/suspend-items [:object 1.5] true]) [:items 2 1] "no step of a path"]
   [(assoc base :items [:call :gumshoe.workloads/suspend-items [:target :x] true]) [:items 2] "takes no path"]
   [(assoc base :items [:call :gumshoe.workloads/suspend-items [:object] {1 2}]) [:items 3 1] "keyed by keywords or strings"]
   [(assoc base :reads {:a [:get "pods" "x"] :b [:get "pods" [:read :a]]}) [:reads :b 2 0] "not available in a :reads entry"]
   [(assoc base :values {:a [:opt :job] :b [:value :a]}) [:values :b 0] "not available in a :values entry"]
   [(assoc base :reads {:a [:list "pods"]}) [:reads :a 2] "reads one namespace"]
   [(assoc base :reads {:a [:list "pods" {:in "x"}]}) [:reads :a 2 :in] "not a key of a read's options"]
   [(assoc base :reads {:a [:list]}) [:reads :a] "takes a type"]
   [(assoc base :reads {:a [:get "pods"]}) [:reads :a] "takes a type, a name"]
   [(assoc base :reads {:a [:object]}) [:reads :a] "no read"]
   [(assoc base :reads {:a [:call :gumshoe.workloads/scale-items 1 2 3]}) [:reads :a 1] "is a :items function"]
   [(assoc base :effect [[:kubectl :scale {:replicas [:list "pods" {:namespace "x"}]}]]) [:effect 0 2 :replicas 0] "not available in this place"]
   [(assoc base :announce "Suspend {}") [:announce] "empty placeholder"]
   [(assoc base :announce "Suspend {pick}") [:announce] "{pick} names nothing"]
   [(assoc base :announce "{value.why}") [:announce] "names no declared :values entry"]
   [(assoc base :announce 3) [:announce] "a template or false"]
   [(assoc base :refuse {:when [:state]}) [:refuse] "a vector"]
   [(assoc base :refuse [{:if [:state] :says "x"}]) [:refuse 0 :if] "not a key of a :refuse entry"]
   [(assoc base :refuse [{:when [:state] :says "x" :reason [:call :gumshoe.workloads/scale-refusal-given 1 2 3 4]}]) [:refuse 0] "not both"]
   [(assoc base :refuse [{:reason "no"}]) [:refuse 0 :reason] "[:call <:reason function>"]
   [(assoc base :refuse [{:when [:state]}]) [:refuse 0] "a :refuse entry is"]
   [(assoc base :refuse [{:reason [:call :gumshoe.deletion/dry-run [:context] [:ref]]}]) [:refuse 0 :reason 1] "is a :read function, and this place takes :reason"]
   [(assoc base :items "suspend it") [:items] ":items is"]
   [(assoc base :items []) [:items] ":items is"]
   [(-> base (dissoc :state :refuse) (assoc-in [:target :candidates] [:call :gumshoe.deletion/candidates [:context]]))
    [:state] "no target :state"]
   [(assoc-in deleting [:confirm :destructive?] false) [:confirm :destructive?] "a bare keypress"]
   [(dissoc deleting :recovery) [:recovery] "is destructive and says nothing"]
   [(assoc base :opt-out {:verify "no wait needed here"}) [:opt-out :verify] "declares a :state"]
   [(-> base (dissoc :state :refuse) (assoc-in [:target :candidates] [:call :gumshoe.deletion/candidates [:context]])
        (assoc :opt-out {:verify "nothing to wait for here" :done "nothing to skip"}))
    [:opt-out :done] "declares no :state"]
   [(assoc base :opt-out {:recovery "nothing to undo here"}) [:opt-out :recovery] "is not destructive"]
   [(assoc deleting :opt-out {:recovery "nothing to undo here"}) [:opt-out :recovery] "declares a :recovery"]
   [(assoc base :opt-out {:destructive "nothing deleted here"}) [:opt-out :destructive] "deletes nothing"]])

(deftest every-unknown-key-word-name-or-argument-is-refused-precisely
  (doseq [[dsl path word] refused]
    (testing (pr-str path word)
      (let [r (refusal dsl)]
        (is (some? r) "refused")
        (when r
          (is (= path (:path r)) (:message r))
          (is (str/includes? (:message r) word) (:message r))
          (is (str/starts-with? (:message r) (if (seq path) (pr-str path) "the change"))
              "the message names where"))))))

(deftest a-refused-change-is-refused-with-an-ex-info-the-caller-can-tell
  (let [e (try (dsl/compile (dissoc base :effect)) (catch clojure.lang.ExceptionInfo e e))]
    (is (dsl/invalid? e))
    (is (not (dsl/invalid? (ex-info "other" {}))))))

;; --- the registry --------------------------------------------------------------------

(deftest the-registry-refuses-what-it-cannot-vouch-for
  (let [ok {:fn #'str :kind :value :args ["x"] :doc "joins"}
        why #(try (registry/register! %1 %2) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))]
    (is (re-find #"qualified keyword" (why :str ok)))
    (is (re-find #"registers no :fn" (why :gumshoe.dsl-test/a (dissoc ok :fn))))
    (is (re-find #"has the kind :effect" (why :gumshoe.dsl-test/a (assoc ok :kind :effect))))
    (is (re-find #"vector of strings" (why :gumshoe.dsl-test/a (assoc ok :args [:x]))))
    (is (re-find #"has no :doc" (why :gumshoe.dsl-test/a (dissoc ok :doc))))
    (is (nil? (why :gumshoe.dsl-test/joined ok)))
    (is (nil? (why :gumshoe.dsl-test/joined (assoc ok :doc "reworded"))) "the same entry again is a no-op")
    (is (re-find #"registered already" (why :gumshoe.dsl-test/joined (assoc ok :args ["x" "y"]))))
    (is (re-find #"registered already" (why :gumshoe.dsl-test/joined (assoc ok :fn #'pr-str))))))

(deftest a-registered-function-is-callable-once-registered
  (registry/register! :gumshoe.dsl-test/shout {:fn #'str/upper-case :kind :value :args ["text"] :doc "upper case"})
  (let [c (dsl/compile (assoc base :values {:loud [:call :gumshoe.dsl-test/shout [:target]]}
                              :announce "{value.loud}"))]
    (is (= "BATCH/NIGHTLY" ((get-in c [:values :loud]) ctx)))))

(deftest gumshoes-own-functions-are-registered-with-their-kind
  (is (= {:gumshoe.workloads/parse-pick :parse
          :gumshoe.workloads/scalable-picks :candidates
          :gumshoe.workloads/scale-refusal-given :reason
          :gumshoe.workloads/scale-items :items
          :gumshoe.workloads/scaled? :predicate
          :gumshoe.workloads/suspend-items :items
          :gumshoe.deletion/parse-pick :parse
          :gumshoe.deletion/candidates :candidates
          :gumshoe.deletion/dry-run :read
          :gumshoe.deletion/read-dependents :read
          :gumshoe.deletion/deletion-items :items}
         (into {} (for [[k {:keys [kind]}] (registry/entries) :when (#{"gumshoe.workloads" "gumshoe.deletion"} (namespace k))]
                    [k kind])))))

;; --- round trip --------------------------------------------------------------------

(deftest compiling-is-pure-and-repeatable
  (let [a (dsl/compile base)
        b (dsl/compile (edn/read-string (pr-str base)))]
    (is (= base (dsl/source-of a)))
    (is (= (dsl/safeguards-of a) (dsl/safeguards-of b)))
    (doseq [k [:description :options :prerequisites :confirm :subjects :done]]
      (is (= (get a k) (get b k)) (str k)))
    (is (= (dissoc (:select a) :candidates) (dissoc (:select b) :candidates)))
    (doseq [k [:effect :announce :items :refusal]]
      (is (= ((get a k) ctx) ((get b k) ctx)) (str k)))
    (is (= (map #(dissoc % :check) ((:verify a) ctx)) (map #(dissoc % :check) ((:verify b) ctx))))))

;; --- the safeguards, as a property ---------------------------------------------------

(def ^:private seed 20261125)

(defn- check!
  [n property]
  (let [result (tc/quick-check n property :seed seed)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :fail])))
    result))

(def ^:private gen-target
  (gen/elements [{:pick :namespaced :label "CronJob" :type "cronjobs" :namespace-flag :namespace :name-flag :job}
                 {:pick :one :label "Node" :type "nodes" :flag :node}
                 {:pick :one :label "Workload" :flag :pick :parse :gumshoe.workloads/parse-pick}]))

(def ^:private gen-predicate
  (gen/elements [[:field [:spec :suspend] := true]
                 [:not [:field [:spec :suspend] := true]]
                 [:field [:spec :replicas] :>= 1]
                 [:and [:some? [:object :spec]] [:field [:metadata :name] :not= "x"]]
                 [:or [:nil? [:object :status]] [:field [:status :ready] := true]]
                 [:call :gumshoe.workloads/scaled? [:object] 2]]))

(def ^:private gen-effect
  (gen/elements [[:kubectl :patch {:merge {:spec {:suspend true}}}]
                 [:kubectl :scale {:replicas 2}]
                 [:kubectl :annotate {:annotations {"a" "b"} :overwrite true}]
                 [:kubectl :delete]]))

(def ^:private gen-change
  "Changes compile accepts: every combination of target, state, effects,
   refusals and opt-outs whose opt-outs each opt out of something."
  (gen/let [target gen-target
            state (gen/one-of [(gen/return nil) (gen/return :absent) gen-predicate])
            effects (gen/vector gen-effect 1 3)
            refuse (gen/vector (gen/elements [{:when [:absent] :says "gone"}
                                              {:when [:field [:metadata :name] := "x"] :says "it is x"}])
                               0 2)
            stated (gen/elements [nil true false])
            recovery? gen/boolean
            skip? gen/boolean]
    (let [deletes? (some #(= :delete (second %)) effects)
          destructive? (if (some? stated) stated (boolean deletes?))
          opt-out (cond-> {}
                    (nil? state) (assoc :verify "the change has nothing to wait for")
                    (and state (not skip?)) (assoc :done "it is cheap to apply again")
                    (and deletes? (false? stated)) (assoc :destructive "a scratch namespace only")
                    (and destructive? (not recovery?)) (assoc :recovery "its controller recreates it"))]
      (cond-> {:gumshoe/dsl :v0-experimental
               :description "a generated change"
               :target (assoc target :candidates (if (and state (:type target) (not (:parse target)))
                                                   {:unless :state}
                                                   [:call :gumshoe.deletion/candidates [:context]]))
               :prerequisites {}
               :confirm (cond-> {:action "change it"} (some? stated) (assoc :destructive? stated))
               :effect effects
               :refuse (cond-> refuse state (conj {:when [:state] :says "done already"}))}
        state (assoc :state {:holds state :says "{target} holds"})
        (seq opt-out) (assoc :opt-out opt-out)
        (and destructive? recovery?) (assoc :recovery "recreate {target}")))))

(defn- probe-ctx
  [c]
  (let [{:keys [mode]} (:select c)]
    {:context "ctx" :cluster "cluster" :opts {}
     :target (if (= :namespaced mode) "probe/x" "x")
     :ref {:type "cronjobs" :namespace (when (= :namespaced mode) "probe") :name "x" :kind "Deployment"}
     :object nil}))

(defn- safe?
  "Whether a compiled change holds every safeguard it did not opt out of
   with a reason, and names each opt-out as it was given."
  [dsl]
  (let [c (dsl/compile dsl)
        opt-out (:opt-out dsl)
        checks (when-let [v (:verify c)] (v (probe-ctx c)))
        deletes? (some #(= :delete (second %)) (:effect dsl))]
    (and
     ;; pinned: the picked object is a subject the engine reads again and refuses on drift
     (= :pick (:from (first (:subjects c))))
     (= [] (conformance/guard-problems c))
     (= [] (conformance/contract-problems c))
     ;; a fail-closed post-check, or the reason for none
     (if (:no-verify c)
       (and (nil? (:verify c)) (= (:verify opt-out) (:no-verify c)))
       (and (seq checks) (= [] (verify/fail-closed-findings checks))))
     ;; the skip comes from the same state, or the reason for none
     (= (some? (:done c)) (and (some? (:verify c)) (not (contains? opt-out :done))))
     ;; a delete asks for yes unless a reason says otherwise
     (or (not deletes?) (true? (get-in c [:confirm :destructive?])) (string? (:destructive opt-out)))
     ;; a destructive change says what it leaves behind, or why not
     (or (not (get-in c [:confirm :destructive?])) (some? (:recovery c)) (string? (:recovery opt-out)))
     ;; the safeguards report names every opt-out
     (= (set (keys opt-out))
        (set (for [[k v] {:verify (:post-check (dsl/safeguards-of c))
                          :done (when-not (contains? opt-out :verify) (:done (dsl/safeguards-of c)))
                          :destructive (:destructive-confirmation (dsl/safeguards-of c))
                          :recovery (:recovery (dsl/safeguards-of c))}
                   :when (map? v)]
               k))))))

(deftest every-compiled-change-is-pinned-and-fails-closed-unless-it-opts-out-with-a-reason
  (check! 300 (prop/for-all [dsl gen-change] (safe? dsl))))

(deftest the-property-would-catch-a-post-check-that-does-not-fail-closed
  (testing "a positive control: with a post-check that always holds, the property fails"
    (with-redefs-fn {#'gumshoe.dsl/post-checks-of
                     (fn [_ _ says]
                       (fn [ctx] [{:description (says {:ctx ctx}) :timeout 1 :interval 1 :check (constantly true)}]))}
      #(is (false? (:pass? (tc/quick-check 50 (prop/for-all [dsl gen-change] (safe? dsl)) :seed seed)))))))
