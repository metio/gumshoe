;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dsl-effect-test
  "gumshoe.dsl's [:effect <registered type> arg ..]: what compile refuses,
   what the registration contract of gumshoe.effect/register-effect-type!
   adds, and whole runs through gumshoe.mutation/run-on! of an effect that
   writes a file into a temporary directory, on the picked object and on
   each object of a read, against a fake cluster."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.change :as change]
            [gumshoe.conformance :as conformance]
            [gumshoe.dsl :as dsl]
            [gumshoe.dsl.registry :as registry]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.mutation :as mutation]
            [gumshoe.offline :as offline]
            [gumshoe.outcome :as outcome]
            [gumshoe.reads]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.testing.plan :as plan]
            [gumshoe.verify :as verify]))

;; --- the registered effect types -----------------------------------------------------
;;
;; Other tests reset gumshoe.effect's registry wholesale, so every test that
;; needs these types registers them itself and restores the registry after.

(defn- file-says?
  [dir file text]
  (let [f (fs/file dir file)]
    (and (fs/exists? f) (= text (slurp f)))))

(def ^:private types
  {:dsl-effect-test/write-file
   {:describe (fn [[dir file _]] (str "write " file " in " dir))
    :perform (fn [[dir file text]] (spit (fs/file dir file) text) true)
    :args ["dir" "file" "text"]
    :destructive? false
    :touches (fn [_] [])
    :doc "writes a file"}

   :dsl-effect-test/remove-file
   {:describe (fn [[dir file]] (str "remove " file " from " dir))
    :perform (fn [[dir file]] (fs/delete-if-exists (fs/file dir file)) true)
    :args ["dir" "file"]
    :touches (fn [_] [])}

   :dsl-effect-test/label-file
   {:describe (fn [[dir namespace name]] (str "record " namespace "/" name " in " dir))
    :perform (fn [[dir namespace name]] (spit (fs/file dir (str namespace "-" name)) "seen") true)
    :args ["dir" "namespace" "name"]
    :destructive? false
    :touches (fn [[_ namespace name]] [{:type "configmaps" :namespace namespace :name name}])}

   :dsl-effect-test/sneaky
   {:describe (fn [_] "record something")
    :perform (fn [_] true)
    :args ["namespace" "name"]
    :destructive? false
    :touches (fn [[namespace name]] [{:type "configmaps" :namespace namespace :name name}])}

   :dsl-effect-test/untouched
   {:describe (fn [[file]] (str "write " file))
    :perform (fn [_] true)
    :args ["file"]
    :destructive? false}

   :dsl-effect-test/countless
   {:describe (fn [_] "something") :perform (fn [_] true) :destructive? false}

   :dsl-effect-test/reading-describe
   {:describe (fn [[file]] (str "write " file " for " (:out (shell/execute "git" "config" "user.name"))))
    :perform (fn [_] true)
    :args ["file"]
    :destructive? false}})

(defn- with-types
  [thunk]
  (let [saved @@#'effect/effect-types]
    (try
      (doseq [[op handlers] types] (effect/register-effect-type! op handlers))
      (thunk)
      (finally (reset! @#'effect/effect-types saved)))))

(registry/register! :gumshoe.dsl-effect-test/namespaces
                    {:fn (fn [_context] ["shop"]) :kind :candidates :args ["context"]
                     :doc "the namespaces a test offers"})

(registry/register! :gumshoe.dsl-effect-test/file-says?
                    {:fn file-says? :kind :predicate :args ["dir" "file" "text"]
                     :doc "whether a file in a test directory holds the text"})

(registry/register! :gumshoe.dsl-effect-test/git-user
                    {:fn (fn [] (:out (shell/execute "git" "config" "user.name")))
                     :kind :value :args []
                     :doc "a :value function that reads, which a plan must refuse"})

(registry/register! :gumshoe.dsl-effect-test/configmaps
                    {:fn (fn [context namespace] (gumshoe.reads/listing context {:type "configmaps" :namespace namespace}))
                     :kind :read :args ["context" "namespace"]
                     :doc "a test read"})

;; --- the changes ---------------------------------------------------------------------

(defn- write
  "Writes shop.txt into dir, proven by reading the file back."
  [dir]
  {:gumshoe/dsl :v0-experimental
   :description "Writes the namespace's file"
   :target {:pick :one :label "namespace" :type "namespaces" :flag :namespace
            :candidates [:call :gumshoe.dsl-effect-test/namespaces [:context]]}
   :prerequisites {}
   :values {:dir dir}
   :state {:holds [:call :gumshoe.dsl-effect-test/file-says? [:value :dir] "shop.txt" "hello shop"]
           :says "shop.txt says hello"}
   :confirm {:action "write the namespace's file"}
   :announce false
   :effect [[:effect :dsl-effect-test/write-file [:value :dir] "shop.txt" "hello shop"]]})

(defn- write-each
  "Writes one file per ConfigMap of the namespace, holding its data.text."
  [dir]
  {:gumshoe/dsl :v0-experimental
   :description "Writes a file per ConfigMap"
   :target {:pick :one :label "namespace" :type "namespaces" :flag :namespace
            :candidates [:call :gumshoe.dsl-effect-test/namespaces [:context]]}
   :prerequisites {}
   :values {:dir dir}
   :reads {:maps [:list "configmaps" {:namespace [:target]}]}
   :confirm {:action "write a file per ConfigMap"}
   :announce false
   :effect [[:each :maps
             {:state {:holds [:call :gumshoe.dsl-effect-test/file-says? [:value :dir] [:each :metadata :name] [:each :data :text]]
                      :says "{each.metadata.name} is written"}}
             [:effect :dsl-effect-test/write-file [:value :dir] [:each :metadata :name] [:each :data :text]]]]})

(defn- remove-file
  [dir]
  (-> (write dir)
      (assoc :description "Removes the namespace's file"
             :confirm {:action "remove the namespace's file"}
             :state {:holds [:not [:call :gumshoe.dsl-effect-test/file-says? [:value :dir] "shop.txt" "hello shop"]]
                     :says "shop.txt is gone"}
             :recovery "write shop.txt again"
             :effect [[:effect :dsl-effect-test/remove-file [:value :dir] "shop.txt"]])))

;; --- the fake cluster ----------------------------------------------------------------

(defn- object
  [type namespace name & {:as extra}]
  (merge {:kind type
          :metadata (cond-> {:name name :uid (str "uid-" name) :resourceVersion "1"}
                      namespace (assoc :namespace namespace))}
         extra))

(defn- cluster
  [& extra]
  (into {}
        (map (fn [o] [[(:kind o) (get-in o [:metadata :namespace]) (get-in o [:metadata :name])] o]))
        (cons (object "namespaces" nil "shop") extra)))

(defn- parse-argv
  [argv]
  (reduce (fn [acc arg]
            (if-let [[_ k v] (re-matches #"--([^=]+)(?:=(.*))?" arg)]
              (assoc-in acc [:flags k] (or v true))
              (update acc :words conj arg)))
          {:flags {} :words []}
          (rest argv)))

(defn- respond
  [objects argv]
  (let [{:keys [flags words]} (parse-argv argv)
        [verb type name] words
        ns (get flags "namespace")]
    (if (= "get" verb)
      (if name
        (if-let [o (get objects [type ns name])]
          {:exit 0 :out (json/generate-string o) :err ""}
          {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" type name)})
        {:exit 0
         :out (json/generate-string {:items (vec (for [[[t n] o] (map (fn [[[t n _] o]] [[t n] o]) objects)
                                                       :when (and (= t type) (= n ns))]
                                                   o))})
         :err ""})
      {:exit 1 :out "" :err (str "the fake cluster does not answer " (pr-str argv))})))

(defn- run-change
  "Runs dsl compiled against the namespace shop. Returns the result, what
   was printed, the confirmation requests, the post-check descriptions and
   the kubectl calls that were no read."
  [dsl objects & {:keys [dry-run?]}]
  (let [spec (dsl/compile dsl)
        confirmations (atom [])
        checks (atom [])
        writes (atom [])
        eventually verify/eventually
        out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        shell! (fn [& argv]
                 ;; the real gumshoe.shell checks gumshoe.offline before any
                 ;; command starts, and this stub replaces it
                 (offline/check! argv)
                 (let [argv (vec argv)]
                   (when-not (some #{"get"} argv) (swap! writes conj argv))
                   (respond objects argv)))
        streamed (fn [& argv] (:exit (apply shell! argv)))]
    (with-redefs-fn
      {#'shell/execute shell!
       #'shell/run-attributed streamed
       #'shell/run-with-output streamed
       #'interact/confirm! (fn [request] (swap! confirmations conj request) true)
       #'announce/announce! (constantly nil)
       #'verify/eventually (fn [check]
                             (swap! checks conj (:description check))
                             (eventually (assoc check :timeout 0)))}
      (fn []
        (let [result (binding [*out* out *err* err
                               outcome/*record* (outcome/fresh)
                               flow/*dry-run* (boolean dry-run?)
                               kube-context/*pinned* {:context "ctx" :cluster "cluster"}]
                       (mutation/run-on! spec "shop" {} nil))]
          {:result result
           :printed (stdout/strip-colors (str out err))
           :confirmations (mapv #(select-keys % [:items :destructive?]) @confirmations)
           :checks @checks
           :commands @writes})))))

(defn- temp-dir
  []
  (str (fs/create-temp-dir {:prefix "gumshoe-dsl-effect-"})))

;; --- the registration contract -------------------------------------------------------

(deftest a-registration-declares-its-arguments-destructiveness-and-touches-or-is-refused
  (with-types
    (fn []
      (testing "a type that does not say is destructive"
        (is (true? (:destructive? (effect/effect-type :dsl-effect-test/remove-file))))
        (is (false? (:destructive? (effect/effect-type :dsl-effect-test/write-file)))))
      (testing "the types a change may name are those naming their :args"
        (let [listed (registry/effects)]
          (is (= {:kind :effect :args ["dir" "file" "text"] :destructive? false :doc "writes a file"}
                 (get listed :dsl-effect-test/write-file)))
          (is (contains? listed :dsl-effect-test/remove-file))
          (is (not (contains? listed :dsl-effect-test/countless)))))
      (testing "a malformed registration is refused"
        (doseq [[op handlers words]
                [[:cmd {:describe str :perform str} "is built in"]
                 [:x {:describe str :perform str :args "dir"} "vector of non-blank strings"]
                 [:x {:describe str :perform str :args ["dir" ""]} "vector of non-blank strings"]
                 [:x {:describe str :perform str :destructive? "no"} ":destructive? true or false"]
                 [:x {:describe str :perform str :touches [:a]} "function of its arguments"]
                 [:x {:describe str :perform str :doc ""} "non-blank :doc"]]]
          (let [e (try (effect/register-effect-type! op handlers) nil
                       (catch clojure.lang.ExceptionInfo e e))]
            (is (some? e) (pr-str handlers))
            (is (str/includes? (str (ex-message e)) words) (ex-message e))
            (is (nil? (effect/effect-type op)) "and not registered"))))
      (testing "the DSL registry takes no :effect function: an effect type has one registry"
        (let [e (try (registry/register! :gumshoe.dsl-effect-test/an-effect
                                         {:fn identity :kind :effect :args ["x"] :doc "x"})
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
          (is (str/includes? (str (ex-message e)) "register it with gumshoe.effect/register-effect-type!"))
          (is (nil? (registry/entry :gumshoe.dsl-effect-test/an-effect))))))))

(deftest a-registered-describe-renders-without-reading
  (with-types
    (fn []
      (is (= "write a.txt in /d" (effect/describe [:dsl-effect-test/write-file "/d" "a.txt" "x"])))
      (let [called (atom false)]
        (with-redefs-fn {#'shell/execute (fn [& argv] (offline/check! argv) (reset! called true) {:exit 0 :out "me" :err ""})}
          (fn []
            (let [e (try (effect/describe [:dsl-effect-test/reading-describe "a.txt"]) nil
                         (catch clojure.lang.ExceptionInfo e e))]
              (is (offline/forbidden? e) "a :describe that starts a command throws")
              (is (false? @called) "before the command starts")))))
      (testing "control: the same :describe outside describe reads"
        (with-redefs-fn {#'shell/execute (fn [& argv] (offline/check! argv) {:exit 0 :out "me" :err ""})}
          #(is (= "write a.txt for me" ((:describe (effect/effect-type :dsl-effect-test/reading-describe)) ["a.txt"]))))))))

;; --- what compile refuses ------------------------------------------------------------

(defn- refusal-of
  [dsl]
  (try (dsl/compile dsl) nil
       (catch clojure.lang.ExceptionInfo e
         (when (dsl/invalid? e) {:path (:path (ex-data e)) :message (ex-message e)}))))

(defn- refused
  [dir]
  (let [w (write dir)
        each (write-each dir)]
    [[(assoc w :effect [[:effect :dsl-effect-test/nope "x"]]) [:effect 0 1]
      ":dsl-effect-test/nope is no registered effect type - register it with gumshoe.effect/register-effect-type! before the change compiles, or name one of"]
     [(assoc w :effect [[:effect :dsl-effect-test/nope "x"]]) [:effect 0 1] ":dsl-effect-test/write-file"]
     [(assoc w :effect [[:effect :cmd "rm" "-rf" "/"]]) [:effect 0 1] ":cmd is built into gumshoe.effect"]
     [(assoc w :effect [[:effect :kubectl "ctx" "delete" "ns" "shop"]]) [:effect 0 1] ":kubectl is built into gumshoe.effect"]
     [(assoc w :effect [[:effect "write-file" "x"]]) [:effect 0 1] "named by a keyword"]
     [(assoc w :effect [[:effect]]) [:effect 0 1] "named by a keyword"]
     [(assoc w :effect [[:effect :dsl-effect-test/countless]]) [:effect 0 1] "names no :args"]
     [(assoc w :effect [[:effect :dsl-effect-test/write-file [:value :dir] "shop.txt"]]) [:effect 0 1]
      "takes 3 argument(s) (dir, file, text), not 2"]
     [(assoc w :effect [[:effect :dsl-effect-test/write-file [:value :dir] "shop.txt" "x" "y"]]) [:effect 0 1] "not 4"]
     [(assoc w :effect [[:effect :dsl-effect-test/write-file [:value :dir] "shop.txt" [:list "configmaps" {:namespace "shop"}]]])
      [:effect 0 4 0] "is not available in this place"]
     [(assoc w :effect [[:effect :dsl-effect-test/write-file [:value :dir] "shop.txt" [:get "configmaps" "x" {:namespace "shop"}]]])
      [:effect 0 4 0] "is not available in this place"]
     [(assoc w :effect [[:effect :dsl-effect-test/write-file [:value :dir] "shop.txt" [:context]]]) [:effect 0 4 0] "is not available in this place"]
     [(assoc w :effect [[:effect :dsl-effect-test/write-file [:value :dir] "shop.txt"
                         [:call :gumshoe.dsl-effect-test/configmaps [:target] [:target]]]])
      [:effect 0 4 1] "is a :read function, and this place takes :value"]
     [(assoc w :effect [[:effect :dsl-effect-test/write-file [:value :dir] "shop.txt" [:each :data :text]]]) [:effect 0 4 0] "is not available"]
     [(assoc w :effect [[:effect :dsl-effect-test/write-file [:value :nope] "shop.txt" "x"]]) [:effect 0 2 1] "names no declared :values entry"]
     [(assoc w :effect [[:effect :dsl-effect-test/write-file [:value :dir] "shop.txt" 'x]]) [:effect 0 4] "is a symbol"]
     [(dissoc w :state) [:state] "declares no target :state"]
     [(dissoc (remove-file dir) :state) [:state] "declares no target :state"]
     [(dissoc (remove-file dir) :recovery) [:recovery] "the change is destructive"]
     [(assoc-in (remove-file dir) [:confirm :destructive?] false) [:confirm :destructive?]
      "destroys what it acts on, and the :confirm says :destructive? false"]
     [(assoc w :opt-out {:recovery "nothing to recover"}) [:opt-out :recovery] "it is not destructive"]
     [(update-in each [:effect 0] #(assoc % 2 {})) [:effect 0 2 :state] "an :each that does not delete says what holds for each object"]
     [(assoc-in each [:effect 0 3] [:effect :dsl-effect-test/write-file [:value :dir] [:each :metadata :name]]) [:effect 0 3 1] "not 2"]
     [(assoc-in each [:effect 0 3] [:effect :dsl-effect-test/remove-file [:value :dir] [:each :metadata :name]]) [:recovery] "the change is destructive"]
     ;; accepted
     [(assoc w :effect [[:effect :dsl-effect-test/write-file [:value :dir] "{target}.txt" [:call :gumshoe.dsl-effect-test/git-user]]]) nil nil]
     [(assoc w :effect [[:effect :dsl-effect-test/write-file [:value :dir] "shop.txt" {:who [:target] :n 1}]]) nil nil]
     [(assoc-in each [:effect 0 3] [:effect :dsl-effect-test/label-file [:value :dir] [:each :metadata :namespace] [:each :metadata :name]]) nil nil]
     [(-> each (assoc :recovery "write it again")
          (assoc-in [:effect 0 3] [:effect :dsl-effect-test/remove-file [:value :dir] [:each :metadata :name]])) nil nil]]))

(deftest every-malformed-registered-effect-is-refused-precisely
  (with-types
    (fn []
      (doseq [[dsl path words] (refused "/nonexistent")]
        (let [r (refusal-of dsl)]
          (if (nil? path)
            (is (nil? r) (str "accepted: " (pr-str (:effect dsl)) " - " (:message r)))
            (testing (pr-str path words)
              (is (= path (:path r)) (:message r))
              (is (str/includes? (str (:message r)) words) (:message r)))))))))

(deftest an-unregistered-type-is-refused-when-nothing-is-registered
  (let [saved @@#'effect/effect-types]
    (try
      (reset! @#'effect/effect-types {})
      (let [r (refusal-of (write "/nonexistent"))]
        (is (= [:effect 0 1] (:path r)))
        (is (str/includes? (:message r) "name one of none") (:message r)))
      (finally (reset! @#'effect/effect-types saved)))))

(deftest the-refusal-table-would-catch-a-compile-that-accepts-any-type
  (with-types
    (fn []
      (with-redefs-fn {#'gumshoe.dsl/registered-effect-type! (fn [_ _ _] {})}
        #(is (nil? (refusal-of (assoc (write "/nonexistent") :effect [[:effect :dsl-effect-test/nope "x"]] :recovery "x")))
             "with the check replaced, an unregistered type compiles - so the table tests the check")))))

;; --- what it compiles to -------------------------------------------------------------

(deftest a-registered-effect-compiles-to-its-op-and-the-values-of-its-arguments
  (with-types
    (fn []
      (let [c (dsl/compile (write "/d"))
            ctx {:context "ctx" :target "shop" :ref {:type "namespaces" :name "shop"}
                 :object (object "namespaces" nil "shop") :dir "/d"}]
        (is (= c (change/normalize c)) "normalize finds nothing to change")
        (is (= [[:dsl-effect-test/write-file "/d" "shop.txt" "hello shop"]] ((:effect c) ctx)))
        (is (= ["shop" "write shop.txt in /d"] ((:items c) ctx))
            "the confirmation shows the pick and what the effect will do, as its :describe says")
        (is (false? (get-in c [:confirm :destructive?])))
        (is (not (change/destructive-effect? (:effect c))))
        (is (= [{:type :dsl-effect-test/write-file :destructive? false}] (:effects (dsl/safeguards-of c))))
        (is (= :on (:post-check (dsl/safeguards-of c))))
        (is (= [] (conformance/contract-problems c)))
        (is (= [] (conformance/guard-problems c)))
        (is (= [] (verify/fail-closed-findings ((:verify c) ctx))) "its post-check fails closed"))
      (let [c (dsl/compile (remove-file "/d"))]
        (is (true? (get-in c [:confirm :destructive?])) "a type that does not say asks for the typed yes")
        (is (change/destructive-effect? (:effect c)) "and marks the effect, so no composition drops it")
        (is (= :on (:destructive-confirmation (dsl/safeguards-of c))))
        (is (= [] (conformance/guard-problems c)))))))

(deftest the-destructive-default-is-what-makes-an-unsaid-type-destructive
  (with-types
    (fn []
      (with-redefs-fn {#'effect/effect-type (fn [op] (some-> (get @@#'effect/effect-types op) (update :destructive? boolean)))}
        #(is (false? (get-in (dsl/compile (dissoc (remove-file "/d") :recovery)) [:confirm :destructive?]))
             "with a default of false the remove compiles as a bare keypress - so the tests above test the default")))))

;; --- runs ----------------------------------------------------------------------------

(deftest a-registered-effect-writes-its-file-and-proves-it
  (with-types
    (fn []
      (let [dir (temp-dir)
            {:keys [result confirmations checks commands printed]} (run-change (write dir) (cluster))]
        (is (true? result) printed)
        (is (= "hello shop" (slurp (fs/file dir "shop.txt"))))
        (is (= [{:items ["shop" (str "write shop.txt in " dir)] :destructive? false}] confirmations))
        (is (= ["shop.txt says hello"] checks))
        (is (empty? commands) "no kubectl call changed anything")
        (testing "run again: the file says hello, so the change is done and asks nothing"
          (let [r (run-change (write dir) (cluster))]
            (is (true? (:result r)))
            (is (empty? (:confirmations r)))
            (is (str/includes? (:printed r) "skipped, already done") (:printed r))))))))

(deftest a-destructive-registered-effect-asks-for-the-typed-yes
  (with-types
    (fn []
      (let [dir (temp-dir)
            _ (spit (fs/file dir "shop.txt") "hello shop")
            {:keys [result confirmations checks]} (run-change (remove-file dir) (cluster))]
        (is (true? result))
        (is (= [{:items ["shop" (str "remove shop.txt from " dir)] :destructive? true}] confirmations))
        (is (= ["shop.txt is gone"] checks))
        (is (not (fs/exists? (fs/file dir "shop.txt"))))))))

(deftest a-registered-effect-acts-on-each-object-a-read-returned
  (with-types
    (fn []
      (let [dir (temp-dir)
            _ (spit (fs/file dir "b") "bee")
            objects (cluster (object "configmaps" "shop" "a" :data {:text "ay"})
                             (object "configmaps" "shop" "b" :data {:text "bee"}))
            {:keys [result confirmations checks printed]} (run-change (write-each dir) objects)]
        (is (true? result) printed)
        (is (= "ay" (slurp (fs/file dir "a"))))
        (is (= [{:items ["shop" "write-file configmaps shop/a" (str "write a in " dir)] :destructive? false}] confirmations)
            "b already holds its text, so it is neither listed nor written")
        (is (= ["a is written"] checks) "one post-check per object acted on")
        (testing "every object written: done, nothing asked"
          (let [r (run-change (write-each dir) objects)]
            (is (true? (:result r)))
            (is (empty? (:confirmations r)))))))))

(deftest a-dry-run-shows-what-the-registered-effect-would-do-and-does-nothing
  (with-types
    (fn []
      (let [dir (temp-dir)
            {:keys [result printed]} (run-change (write dir) (cluster) :dry-run? true)]
        (is (true? result) printed)
        (is (str/includes? printed (str "would run: write shop.txt in " dir)) printed)
        (is (not (fs/exists? (fs/file dir "shop.txt"))) "nothing was written")))))

(deftest an-effect-whose-arguments-read-is-refused-before-the-confirmation
  (with-types
    (fn []
      (let [dir (temp-dir)
            dsl (assoc (write dir) :effect [[:effect :dsl-effect-test/write-file [:value :dir] "shop.txt"
                                             [:call :gumshoe.dsl-effect-test/git-user]]])
            {:keys [result confirmations printed commands]} (run-change dsl (cluster))]
        (is (false? result))
        (is (empty? confirmations))
        (is (str/includes? printed "refusing: its :items reached outside the process: not running 'git config user.name'") printed)
        (is (empty? commands) "git never ran")
        (is (not (fs/exists? (fs/file dir "shop.txt")))))
      (testing "an effect type whose :describe reads is refused the same way"
        (let [dir (temp-dir)
              dsl (assoc (write dir) :effect [[:effect :dsl-effect-test/reading-describe "shop.txt"]])
              {:keys [result confirmations printed]} (run-change dsl (cluster))]
          (is (false? result))
          (is (empty? confirmations))
          (is (str/includes? printed "refusing: its :items reached outside the process") printed))))))

;; --- plan coverage -------------------------------------------------------------------

(defn- covered
  [dsl objects]
  (with-redefs-fn {#'shell/execute (fn [& argv] (respond objects (vec argv)))}
    #(binding [kube-context/*pinned* {:context "ctx" :cluster "cluster"}]
       (plan/check (dsl/compile dsl) {:target "shop"}))))

(deftest the-dry-run-plan-is-held-to-what-a-registered-effect-touches
  (with-types
    (fn []
      (let [objects (cluster (object "configmaps" "shop" "a" :data {:text "ay"}))]
        (testing "an effect touching no cluster object"
          (let [{:keys [ok? touched failure]} (covered (write "/nonexistent") objects)]
            (is ok? (pr-str failure))
            (is (= [] touched))))
        (testing "an effect touching each object, listed by the confirmation"
          (let [dsl (assoc-in (write-each "/nonexistent") [:effect 0 3]
                              [:effect :dsl-effect-test/label-file [:value :dir] [:each :metadata :namespace] [:each :metadata :name]])
                {:keys [ok? touched failure]} (covered dsl objects)]
            (is ok? (pr-str failure))
            (is (= [{:type "configmaps" :namespace "shop" :name "a"}] touched))))
        (testing "an effect touching an object the confirmation does not list"
          (let [dsl (assoc (write "/nonexistent") :effect [[:effect :dsl-effect-test/sneaky "shop" "elsewhere"]])
                {:keys [ok? missing]} (covered dsl objects)]
            (is (false? ok?))
            (is (= [{:type "configmaps" :namespace "shop" :name "elsewhere"}] missing))))
        (testing "a type registered without :touches cannot be compared, and fails"
          (let [dsl (assoc (write "/nonexistent") :effect [[:effect :dsl-effect-test/untouched "x"]])
                {:keys [ok? unidentified]} (covered dsl objects)]
            (is (false? ok?))
            (is (= [[:dsl-effect-test/untouched "x"]] unidentified))))))))
