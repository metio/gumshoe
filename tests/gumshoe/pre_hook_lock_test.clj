;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pre-hook-lock-test
  "Pre-hooks as an estate's lock: the context they see (the pinned kube
   context and cluster, and how the book reaches a cluster), fail-closed
   hooks that block unless they explicitly allow, the env.edn policy that
   requires them, and :require-known-cluster, which checks every book that
   reaches a cluster against the known clusters."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gumshoe.config :as config]
            [gumshoe.hooks :as hooks]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]))

(use-fixtures :each (fn [t]
                      (reset! @#'hooks/pre-hooks [])
                      (t)
                      (reset! @#'hooks/pre-hooks [])))

(def ^:private no-policy {:declared nil :unreadable nil})

(def ^:private pinned-context
  {:change? true
   :cluster-access :pinned
   :connection {:context "staging" :cluster "kube.staging.example" :announce-as "kube.staging.example"}})

(defn- run
  ([context] (run context no-policy))
  ([context policy]
   (let [err (java.io.StringWriter.)
         result (binding [*err* err] (hooks/run-pre-hooks! context policy))]
     (assoc result :stderr (str err)))))

;; ---------------------------------------------------------------------------
;; fail-open stays the default

(deftest a-hook-without-a-declaration-fails-open-test
  (testing "a throwing hook registered as a bare function or as a map without :fail-closed? warns and allows"
    (hooks/register-pre-hook! (fn [_] (throw (ex-info "boom" {}))))
    (hooks/register-pre-hook! {:name :estate/audit :hook (fn [_] (throw (ex-info "boom" {})))})
    (let [{:keys [allowed? stderr]} (run pinned-context)]
      (is (true? allowed?))
      (is (str/includes? stderr "pre-hook failed - allowing")))))

(deftest a-fail-open-hook-allows-on-nil-and-on-a-map-without-allow-test
  (doseq [answer [nil {} "yes" {:allow? "true"}]]
    (reset! @#'hooks/pre-hooks [])
    (hooks/register-pre-hook! (constantly answer))
    (is (true? (:allowed? (run pinned-context))) (pr-str answer))))

(deftest a-fail-open-hook-that-times-out-allows-test
  (hooks/register-pre-hook! {:hook (fn [_] (Thread/sleep 2000) false) :timeout-ms 50})
  (let [{:keys [allowed? stderr]} (run pinned-context)]
    (is (true? allowed?))
    (is (str/includes? stderr "pre-hook timed out - allowing"))))

;; ---------------------------------------------------------------------------
;; fail-closed

(deftest a-fail-closed-hook-that-throws-blocks-naming-it-test
  (hooks/register-pre-hook! {:name :estate/cluster-lock :fail-closed? true
                             :hook (fn [_] (throw (ex-info "inventory unreachable" {})))})
  (let [{:keys [allowed? reason hook]} (run pinned-context)]
    (is (false? allowed?))
    (is (= ":estate/cluster-lock" hook))
    (is (= "fail-closed pre-hook :estate/cluster-lock failed: inventory unreachable" reason))))

(deftest a-fail-closed-hook-that-trips-an-assert-blocks-test
  (hooks/register-pre-hook! {:name :estate/cluster-lock :fail-closed? true :hook (fn [_] (assert false "gate bug"))})
  (is (false? (:allowed? (run pinned-context)))))

(deftest a-fail-closed-hook-that-times-out-blocks-test
  (hooks/register-pre-hook! {:name :estate/cluster-lock :fail-closed? true :timeout-ms 50
                             :hook (fn [_] (Thread/sleep 2000) true)})
  (let [{:keys [allowed? reason]} (run pinned-context)]
    (is (false? allowed?))
    (is (= "fail-closed pre-hook :estate/cluster-lock timed out" reason))))

(deftest a-fail-closed-hook-allows-only-an-explicit-allow-test
  (testing "true and {:allow? true} allow"
    (doseq [answer [true {:allow? true} {:allow? true :reason "known cluster"}]]
      (reset! @#'hooks/pre-hooks [])
      (hooks/register-pre-hook! {:name :estate/lock :fail-closed? true :hook (constantly answer)})
      (is (true? (:allowed? (run pinned-context))) (pr-str answer))))
  (testing "anything else blocks and names what it returned"
    (doseq [answer [nil {} "yes" 1 {:allow? "true"} {:allow? nil} [:ok]]]
      (reset! @#'hooks/pre-hooks [])
      (hooks/register-pre-hook! {:name :estate/lock :fail-closed? true :hook (constantly answer)})
      (let [{:keys [allowed? reason]} (run pinned-context)]
        (is (false? allowed?) (pr-str answer))
        (is (= (format "fail-closed pre-hook :estate/lock returned %s, not an explicit allow (true or {:allow? true})"
                       (pr-str answer))
               reason)))))
  (testing "a veto keeps its reason"
    (reset! @#'hooks/pre-hooks [])
    (hooks/register-pre-hook! {:name :estate/lock :fail-closed? true
                               :hook (constantly {:allow? false :reason "kube.prod is locked"})})
    (is (= {:allowed? false :reason "kube.prod is locked" :hook ":estate/lock"}
           (dissoc (run pinned-context) :stderr))))
  (testing "a bare false names the hook"
    (reset! @#'hooks/pre-hooks [])
    (hooks/register-pre-hook! {:name :estate/lock :fail-closed? true :hook (constantly false)})
    (is (= "fail-closed pre-hook :estate/lock vetoed this run" (:reason (run pinned-context))))))

(deftest a-long-answer-is-shortened-in-the-message-test
  (hooks/register-pre-hook! {:fail-closed? true :hook (constantly (apply str (repeat 500 "x")))})
  (let [{:keys [reason]} (run pinned-context)]
    (is (str/ends-with? reason "..., not an explicit allow (true or {:allow? true})"))
    (is (< (count reason) 250))))

(deftest an-unnamed-fail-closed-hook-is-named-by-its-place-test
  (hooks/register-pre-hook! (constantly true))
  (hooks/register-pre-hook! {:fail-closed? true :hook (constantly nil)})
  (let [{:keys [reason hook]} (run pinned-context)]
    (is (= "pre-hook #2" hook))
    (is (str/starts-with? reason "fail-closed pre-hook pre-hook #2 returned nil"))))

(deftest the-first-block-stops-the-run-test
  (let [called (atom [])]
    (hooks/register-pre-hook! {:name :a :hook (fn [_] (swap! called conj :a) {:allow? false :reason "freeze"})})
    (hooks/register-pre-hook! {:name :b :fail-closed? true :hook (fn [_] (swap! called conj :b) true)})
    (is (= "freeze" (:reason (run pinned-context))))
    (is (= [:a] @called))))

;; ---------------------------------------------------------------------------
;; who chooses: the hook, env.edn, never a later registration

(deftest env-edn-makes-a-hook-fail-closed-test
  (hooks/register-pre-hook! {:name :estate/lock :hook (fn [_] (throw (ex-info "boom" {})))})
  (testing "named in :pre-hooks :fail-closed, a hook that declares nothing blocks on a throw"
    (is (false? (:allowed? (run pinned-context {:declared {:fail-closed [:estate/lock]}})))))
  (testing "control: without the policy the same hook fails open"
    (is (true? (:allowed? (run pinned-context no-policy))))))

(deftest a-later-registration-cannot-open-a-fail-closed-hook-test
  (hooks/register-pre-hook! {:name :estate/lock :fail-closed? true :hook (constantly true)})
  (hooks/register-pre-hook! {:name :estate/lock :fail-closed? false :hook (constantly nil)})
  (testing "the replacement function runs, and still fail-closed"
    (is (= 1 (count @@#'hooks/pre-hooks)))
    (is (false? (:allowed? (run pinned-context)))))
  (testing "a later registration can close an open one"
    (reset! @#'hooks/pre-hooks [])
    (hooks/register-pre-hook! {:name :estate/lock :hook (constantly nil)})
    (hooks/register-pre-hook! {:name :estate/lock :fail-closed? true :hook (constantly nil)})
    (is (false? (:allowed? (run pinned-context))))))

(deftest a-required-hook-that-is-not-registered-blocks-every-book-test
  (let [policy {:declared {:fail-closed [:estate/lock :estate/ticket]}}]
    (hooks/register-pre-hook! {:name :estate/ticket :hook (constantly true)})
    (doseq [access [:pinned :unpinned :unresolved :none]]
      (let [{:keys [allowed? reason]} (run (assoc pinned-context :cluster-access access) policy)]
        (is (false? allowed?) (str access))
        (is (= "env.edn requires the fail-closed pre-hook :estate/lock, but no loaded plugin registered it - check :plugins in env.edn"
               reason))))
    (testing "control: with both registered the run is allowed"
      (hooks/register-pre-hook! {:name :estate/lock :hook (constantly true)})
      (is (true? (:allowed? (run pinned-context policy)))))))

(deftest a-malformed-policy-blocks-every-book-test
  (doseq [declared [[:estate/lock] {:fail-closed :estate/lock} {:fail-closed ["estate/lock"]} {:fail-closed {:a 1}}]]
    (let [{:keys [allowed? reason]} (run (assoc pinned-context :cluster-access :none) {:declared declared})]
      (is (false? allowed?) (pr-str declared))
      (is (str/ends-with? reason "- no book runs until it is fixed") reason)))
  (testing "readable policies"
    (is (nil? (hooks/policy-problem nil)))
    (is (nil? (hooks/policy-problem {})))
    (is (nil? (hooks/policy-problem {:fail-closed []})))
    (is (nil? (hooks/policy-problem {:fail-closed #{:a/b}})))))

(deftest an-unreadable-env-edn-blocks-every-book-that-reaches-a-cluster-test
  (let [policy {:declared nil :unreadable "env.edn: the file is not readable EDN: EOF"}]
    (doseq [access [:pinned :unpinned :unresolved]]
      (let [{:keys [allowed? reason]} (run (assoc pinned-context :cluster-access access) policy)]
        (is (false? allowed?) (str access))
        (is (str/includes? reason "env.edn could not be read (env.edn: the file is not readable EDN: EOF)"))))
    (testing "a book that reaches no cluster still runs"
      (is (true? (:allowed? (run (assoc pinned-context :cluster-access :none) policy)))))))

(deftest registration-refuses-what-it-cannot-run-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"needs a function under :hook"
                        (hooks/register-pre-hook! {:name :x/y :fail-closed? true})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"is a keyword"
                        (hooks/register-pre-hook! {:name "x/y" :hook (constantly true)})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"positive number of milliseconds"
                        (hooks/register-pre-hook! {:hook (constantly true) :timeout-ms 0})))
  (is (empty? @@#'hooks/pre-hooks)))

;; ---------------------------------------------------------------------------
;; the context a hook sees

(deftest a-run-whose-context-cannot-be-resolved-test
  (let [seen (atom nil)
        context (runbook/pre-hook-context {:description "d" :kube-context (constantly nil)}
                                          {:book "b.clj" :opts {} :change? true
                                           :connection {:context nil :cluster nil :announce-as nil}})]
    (testing "a fail-open hook runs and is told the context is unresolved, with no connection"
      (hooks/register-pre-hook! (fn [ctx] (reset! seen ctx) true))
      (is (true? (:allowed? (run context))))
      (is (= :unresolved (:cluster-access @seen)))
      (is (nil? (:connection @seen))))
    (testing "a fail-closed hook is not asked and the run is blocked"
      (let [asked (atom false)]
        (hooks/register-pre-hook! {:name :estate/lock :fail-closed? true :hook (fn [_] (reset! asked true) true)})
        (let [{:keys [allowed? reason]} (run context)]
          (is (false? allowed?))
          (is (str/starts-with? reason "fail-closed pre-hook :estate/lock cannot vouch for this run"))
          (is (false? @asked)))))))

(deftest cluster-access-test
  (let [pinned {:context "staging" :cluster "kube.staging.example"}]
    (is (= :pinned (runbook/cluster-access {:kube-context (constantly nil)} pinned)))
    (is (= :unresolved (runbook/cluster-access {:kube-context (constantly nil)} {:context nil :cluster nil})))
    (is (= :unresolved (runbook/cluster-access {:kube-context (constantly nil)} nil)))
    (testing "a book that pins nothing reaches a cluster when its prerequisites say so"
      (doseq [prerequisites [{:installed-tools ["kubectl" "fzf"]}
                             {:installed-tools ["helm"]}
                             {:installed-tools ["kubectl-cnpg"]}
                             {:cluster-capabilities []}
                             {:kubectl-can-get ["pods"]}
                             {:kubectl-can-delete ["pods"]}
                             {:kubectl-can-exec ["pods"]}]]
        (is (= :unpinned (runbook/cluster-access {:prerequisites prerequisites} nil)) (pr-str prerequisites))))
    (testing "and reaches none otherwise"
      (doseq [prerequisites [nil {} {:installed-tools ["ssh"]} {:installed-tools ["dig" "openssl"]}
                             {:can-ping-using-ipv4 [:host]}]]
        (is (= :none (runbook/cluster-access {:prerequisites prerequisites} nil)) (pr-str prerequisites))))
    (testing "a pinned connection wins over the prerequisites"
      (is (= :pinned (runbook/cluster-access {:kube-context (constantly "staging")
                                              :prerequisites {:installed-tools ["ssh"]}}
                                             pinned))))))

(deftest pre-hook-context-test
  (let [connection {:context "staging" :cluster "kube.staging.example" :announce-as "kube.staging.example"}]
    (is (= {:description "drains a node" :book "drain.clj" :opts {:node "n1"} :change? true
            :connection connection :cluster-access :pinned}
           (runbook/pre-hook-context {:description "drains a node" :kube-context (constantly nil)}
                                     {:book "drain.clj" :opts {:node "n1"} :change? true :connection connection})))
    (is (= {:description "lists pods" :book "pods.clj" :opts {} :change? false
            :connection nil :cluster-access :unpinned}
           (runbook/pre-hook-context {:description "lists pods" :prerequisites {:installed-tools ["kubectl"]}}
                                     {:book "pods.clj" :opts {} :change? false :connection nil})))
    (is (= :none (:cluster-access (runbook/pre-hook-context {:description "dns"} {:connection nil}))))))

(deftest blocked-text-test
  (is (= "change freeze" (runbook/blocked-text {:reason "change freeze"})))
  (is (= "change freeze (pre-hook :estate/freeze)" (runbook/blocked-text {:reason "change freeze" :hook ":estate/freeze"})))
  (is (= "fail-closed pre-hook :estate/lock timed out"
         (runbook/blocked-text {:reason "fail-closed pre-hook :estate/lock timed out" :hook ":estate/lock"}))))

;; ---------------------------------------------------------------------------
;; :require-known-cluster

(deftest known-cluster-required?-test
  (is (false? (runbook/known-cluster-required? nil)))
  (is (false? (runbook/known-cluster-required? false)))
  (doseq [v [true "yes" :on 1 {}]]
    (is (true? (runbook/known-cluster-required? v)) (pr-str v))))

(defn- cluster-items
  "The cluster checklist items prerequisite-items builds, run, against a
   stubbed env.edn whose known clusters are `known`."
  [prerequisites access known-required? connection known]
  (with-redefs [config/known-clusters (constantly known)
                config/env-value (fn [& _] nil)
                kubectl/current-cluster (constantly "kube.current.example")]
    (binding [kube-context/*pinned* connection]
      (->> (#'runbook/prerequisite-items prerequisites {} access known-required?)
           (filter #(= "connected to a suitable Kubernetes cluster" (first %)))
           (mapv #((second %)))))))

(deftest require-known-cluster-checks-every-book-that-reaches-a-cluster-test
  (let [staging {:context "staging" :cluster "kube.staging.example"}]
    (testing "a pinned book without :cluster-capabilities is checked"
      (is (= [{:ok? false :label "current Kubernetes cluster kube.staging.example is none you configured: kube.prod.example"}]
             (cluster-items {} :pinned true staging ["kube.prod.example"])))
      (is (= [{:ok? true :label "Kubernetes cluster: kube.staging.example"}]
             (cluster-items {} :pinned true staging ["kube.staging.example"]))))
    (testing "an unpinned book is checked against the current cluster"
      (is (= [{:ok? false :label "current Kubernetes cluster kube.current.example is none you configured: kube.prod.example"}]
             (cluster-items {:installed-tools ["kubectl"]} :unpinned true nil ["kube.prod.example"]))))
    (testing "an unresolved book fails"
      (is (= [{:ok? false :label "not connected to any Kubernetes cluster"}]
             (cluster-items {} :unresolved true {:context nil :cluster nil} ["kube.prod.example"]))))
    (testing "an env.edn naming no cluster fails the check instead of letting every cluster pass"
      (is (false? (:ok? (first (cluster-items {} :pinned true staging [])))))
      (is (str/starts-with? (:label (first (cluster-items {:cluster-capabilities []} :pinned true staging [])))
                            "env.edn sets :require-known-cluster but names no cluster")))
    (testing "a book that reaches no cluster gets no check"
      (is (= [] (cluster-items {} :none true nil ["kube.prod.example"]))))
    (testing "control: without the setting a book that declares nothing gets no check, and an empty list passes"
      (is (= [] (cluster-items {} :pinned false staging ["kube.prod.example"])))
      (is (= [{:ok? true :label "Kubernetes cluster: kube.staging.example"}]
             (cluster-items {:cluster-capabilities []} :pinned false staging []))))
    (testing "one check when the book declares :cluster-capabilities as well"
      (is (= 1 (count (cluster-items {:cluster-capabilities []} :pinned true staging ["kube.staging.example"])))))))

;; ---------------------------------------------------------------------------
;; a real run through runbook/execute!, in a fresh bb

(defn- absolute-classpath
  []
  (->> (str/split (System/getProperty "java.class.path") #":")
       (map #(str (fs/absolutize %)))
       (str/join ":")))

(defn- run-locked-book
  "Runs a change book with :kube-context in a fresh bb from dir, with
   env.edn holding env (nil for none), the kubeconfig answering connection,
   and setup (a string of forms) evaluated before the book runs. The book
   prints ACTION RAN when its action runs."
  [dir {:keys [env connection setup prerequisites]}]
  (when env (spit (str (fs/path dir "env.edn")) env))
  (let [script (str "(require '[gumshoe.runbook :as runbook] '[gumshoe.hooks :as hooks] '[gumshoe.kubectl :as kubectl])"
                    (or setup "")
                    "(with-redefs [kubectl/kube-connection (fn [& _] " (pr-str connection) ")]"
                    "  (runbook/execute! {:description \"locks a cluster\" :announce? false :change? true"
                    "                     :kube-context (constantly nil)"
                    "                     :prerequisites " (pr-str (or prerequisites {}))
                    "                     :action (fn [_ _] (println \"ACTION RAN\") true)}))")]
    @(process/process {:dir (str dir) :out :string :err :string :continue true
                       :extra-env {"GUMSHOE_ENV" (str (fs/path dir "env.edn")) "NO_COLOR" "1" "KUBECONFIG" "/dev/null"}}
                      "bb" "-cp" (absolute-classpath) "-e" script)))

(def ^:private staging {:context "staging" :cluster "kube.staging.example"})

(deftest a-hook-sees-the-pinned-context-and-cluster-in-a-real-run-test
  (fs/with-temp-dir [dir {}]
    (let [{:keys [exit out err]}
          (run-locked-book dir {:connection staging
                                :setup (str "(hooks/register-pre-hook! {:name :estate/lock :fail-closed? true"
                                            "  :hook (fn [ctx] (println (pr-str (select-keys ctx [:connection :cluster-access :change?]))) true)})")})]
      (is (zero? exit) (str out err))
      (is (str/includes? out (pr-str {:connection {:context "staging" :cluster "kube.staging.example" :announce-as "kube.staging.example"}
                                      :cluster-access :pinned :change? true}))
          out)
      (is (str/includes? out "ACTION RAN")))))

(deftest a-fail-closed-hook-blocks-a-real-run-test
  (fs/with-temp-dir [dir {}]
    (let [{:keys [exit out err]}
          (run-locked-book dir {:connection staging
                                :setup "(hooks/register-pre-hook! {:name :estate/lock :fail-closed? true :hook (fn [_] (throw (ex-info \"inventory down\" {})))})"})]
      (is (= 1 exit) (str out err))
      (is (str/includes? (str out err) "BLOCKED - fail-closed pre-hook :estate/lock failed: inventory down"))
      (is (not (str/includes? out "ACTION RAN")))))
  (testing "control: the same hook without :fail-closed? lets the run through"
    (fs/with-temp-dir [dir {}]
      (let [{:keys [exit out err]}
            (run-locked-book dir {:connection staging
                                  :setup "(hooks/register-pre-hook! {:name :estate/lock :hook (fn [_] (throw (ex-info \"inventory down\" {})))})"})]
        (is (zero? exit) (str out err))
        (is (str/includes? out "ACTION RAN"))))))

(deftest a-book-cannot-reopen-the-estate-lock-test
  (testing "env.edn requires the hook; the book registers it again fail-open, and the run is still blocked"
    (fs/with-temp-dir [dir {}]
      (let [{:keys [exit out err]}
            (run-locked-book dir {:connection staging
                                  :env "{:pre-hooks {:fail-closed [:estate/lock]}}"
                                  :setup (str "(hooks/register-pre-hook! {:name :estate/lock :hook (constantly nil)})"
                                              "(hooks/register-pre-hook! {:name :estate/lock :fail-closed? false :hook (constantly nil)})")})]
        (is (= 1 exit) (str out err))
        (is (str/includes? (str out err) "BLOCKED - fail-closed pre-hook :estate/lock returned nil"))
        (is (not (str/includes? out "ACTION RAN"))))))
  (testing "env.edn requires a hook no plugin registered"
    (fs/with-temp-dir [dir {}]
      (let [{:keys [exit out err]} (run-locked-book dir {:connection staging :env "{:pre-hooks {:fail-closed [:estate/lock]}}"})]
        (is (= 1 exit) (str out err))
        (is (str/includes? (str out err) "no loaded plugin registered it"))
        (is (not (str/includes? out "ACTION RAN")))))))

(deftest an-unreadable-env-edn-blocks-a-real-run-test
  (fs/with-temp-dir [dir {}]
    (let [{:keys [exit out err]} (run-locked-book dir {:connection staging :env "{:pre-hooks"})]
      (is (= 1 exit) (str out err))
      (is (str/includes? (str out err) "BLOCKED - env.edn could not be read"))
      (is (not (str/includes? out "ACTION RAN"))))))

(deftest require-known-cluster-in-a-real-run-test
  (testing "a book declaring no :cluster-capabilities is refused on a cluster env.edn does not name"
    (fs/with-temp-dir [dir {}]
      (let [{:keys [exit out err]} (run-locked-book dir {:connection staging
                                                         :env "{:require-known-cluster true :clusters [\"kube.prod.example\"]}"})]
        (is (= 1 exit) (str out err))
        (is (str/includes? (str out err) "current Kubernetes cluster kube.staging.example is none you configured"))
        (is (str/includes? (str out err) "PREREQUISITES NOT MET"))
        (is (not (str/includes? out "ACTION RAN"))))))
  (testing "and runs on one it names"
    (fs/with-temp-dir [dir {}]
      (let [{:keys [exit out err]} (run-locked-book dir {:connection staging
                                                         :env "{:require-known-cluster true :clusters [\"kube.staging.example\"]}"})]
        (is (zero? exit) (str out err))
        (is (str/includes? out "ACTION RAN")))))
  (testing "control: without the setting the same book runs on the unnamed cluster"
    (fs/with-temp-dir [dir {}]
      (let [{:keys [exit out err]} (run-locked-book dir {:connection staging
                                                         :env "{:clusters [\"kube.prod.example\"]}"})]
        (is (zero? exit) (str out err))
        (is (str/includes? out "ACTION RAN"))))))
