;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dsl-detective-test
  "gumshoe.dsl's read-only book shape: what compile-detective refuses, that
   the compiled detective holds no way to write, whole scans against a fake
   cluster in every report format, the three states of a read (a check
   resting on a read that did not answer reads nothing and never passes),
   templating, joining a scope, listing, and the cordoned-node findings of
   gumshoe.detectives.nodes reproduced from data on the same evidence."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.catalog :as catalog]
            [gumshoe.conformance :as conformance]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.nodes :as nodes]
            [gumshoe.detectives.registry :as detectives]
            [gumshoe.dsl :as dsl]
            [gumshoe.dsl.registry :as registry]
            [gumshoe.edn-book :as edn-book]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.offline :as offline]
            [gumshoe.outcome :as outcome]
            [gumshoe.reads.result :as result]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.testing.refusal :as refusal]))

(def ^:private cordoned-edn "tests/edn-books/runbooks/kubernetes/nodes/cordoned.edn")

;; --- registered functions the fixtures call --------------------------------------------

(defn- owner-class
  "Who owns obj, as infra.run's namespace audit classifies it, cut down to
   what the port needs: an ownerReference, Helm's labels, fahrplan's label,
   or nothing."
  [obj]
  (let [labels (get-in obj [:metadata :labels] {})]
    (cond
      (seq (get-in obj [:metadata :ownerReferences])) "controller"
      (= "Helm" (:app.kubernetes.io/managed-by labels)) "helm"
      (= "fahrplan" (:app.kubernetes.io/managed-by labels)) "fahrplan"
      (#{"kube-root-ca.crt" "default"} (get-in obj [:metadata :name])) "kube-apiserver"
      :else nil)))

(def ^:private inventory-answer
  "What the inventory read answers in each test: a gumshoe.reads result, or
   whatever a broken read function returns."
  (atom nil))

(def ^:private shelled (atom []))

(registry/register! :gumshoe.dsl-detective-test/owned?
                    {:fn #(some? (owner-class %)) :kind :predicate :args ["object"]
                     :doc "whether an object has an owner"})
(registry/register! :gumshoe.dsl-detective-test/inventory
                    {:fn (fn [_context namespace] (@inventory-answer namespace)) :kind :read :args ["context" "namespace"]
                     :doc "every object in a namespace, as one gumshoe.reads list result"})
(registry/register! :gumshoe.dsl-detective-test/shells-out?
                    {:fn (fn [_] (shell/execute "kubectl" "delete" "namespace" "shop") true) :kind :predicate :args ["object"]
                     :doc "a predicate that tries to run a command, which a check must never manage"})

;; --- the detectives -------------------------------------------------------------------

(defn- source
  "The fixture file's data."
  []
  (edn/read-string (slurp cordoned-edn)))

(def ^:private audit
  "infra.run's ownership audit (runbooks/hedgedoc/adopt/audit.clj in their
   casebook) written as data: every object of a namespace has an owner."
  {:gumshoe/dsl :v0-experimental
   :kind :detective
   :description "Reports every object in a namespace that nobody owns"
   :options {:namespace {:desc "the namespace to audit" :alias :n :coerce :string}}
   :prerequisites {}
   :reads {:inventory [:call :gumshoe.dsl-detective-test/inventory [:context] [:opt :namespace]]}
   :checks [{:name "unowned-objects"
             :description "every object belongs to fahrplan, Helm, a controller or a named third party"
             :severity :critical
             :each :inventory
             :expect [:call :gumshoe.dsl-detective-test/owned? [:each]]
             :says "{each.kind} {each.metadata.name} belongs to nobody"
             :component "{opt.namespace}/{each.metadata.name}"
             :hint "purge it, or name its owner"}]})

(def ^:private settings
  "A detective over an optional object and a list: the ConfigMap must
   exist, its Deployment must not run without it, and every Deployment
   must ask for at least two replicas."
  {:gumshoe/dsl :v0-experimental
   :kind :detective
   :description "Checks a shop's settings and Deployments"
   :prerequisites {}
   :reads {:settings [:get "configmaps" "settings" {:namespace "shop"}]
           :deployments [:list "deployments" {:namespace "shop"}]}
   :checks [{:name "settings-present"
             :description "the settings ConfigMap exists"
             :severity :critical
             :expect [:present :settings]
             :says "ConfigMap shop/settings is missing"
             :component "shop/settings"}
            {:name "settings-mode"
             :description "the settings say which mode the shop runs in"
             :severity :warning
             :expect [:or [:absent :settings] [:some? [:read :settings :data :mode]]]
             :says "ConfigMap shop/settings names no mode"}
            {:name "replicas"
             :description "every Deployment asks for at least two replicas"
             :severity :warning
             :each :deployments
             :expect [:>= [:each :spec :replicas] 2]
             :says "Deployment {each.metadata.name} asks for {each.spec.replicas} replica(s)"
             :since [:each :metadata :creationTimestamp]}]})

;; --- the fake cluster -----------------------------------------------------------------

(defn- object
  [kind namespace name & {:as extra}]
  (merge-with merge
              {:kind kind :metadata (cond-> {:name name} namespace (assoc :namespace namespace))}
              extra))

(defn- parse-argv
  [argv]
  (reduce (fn [acc arg]
            (if-let [[_ k v] (re-matches #"--([^=]+)(?:=(.*))?" arg)]
              (assoc-in acc [:flags k] (or v true))
              (update acc :words conj arg)))
          {:flags {} :words []}
          (rest argv)))

(defn- cluster
  "A shell/execute answering kubectl get from objects, a map of type ->
   objects. A type in silent does not answer. Every call is recorded."
  [objects silent]
  (fn [& argv]
    ;; the launch gumshoe.shell/execute stands in for refuses inside
    ;; gumshoe.offline before it records or starts anything
    (offline/check! argv)
    (swap! shelled conj (vec argv))
    (let [{:keys [flags words]} (parse-argv argv)
          [verb type name] words]
      (cond
        (not= "get" verb) {:exit 1 :out "" :err (str "the fake cluster answers no " verb)}
        (contains? silent type) refusal/unanswered
        name (if-let [o (first (filter #(and (= name (get-in % [:metadata :name]))
                                             (= (get flags "namespace") (get-in % [:metadata :namespace])))
                                       (get objects type)))]
               {:exit 0 :out (json/generate-string o) :err ""}
               {:exit 0 :out "" :err ""})
        :else {:exit 0
               :out (json/generate-string {:kind "List"
                                           :items (vec (filter #(or (get flags "all-namespaces")
                                                                    (nil? (get flags "namespace"))
                                                                    (= (get flags "namespace") (get-in % [:metadata :namespace])))
                                                               (get objects type)))})
               :err ""}))))

(defn- scan
  "Runs compiled through detective/scan! against the fake cluster, as
   detective/book runs it. Returns the result, stdout, stderr and the
   commands it ran."
  [compiled {:keys [objects silent opts] :or {opts {}}}]
  (reset! shelled [])
  (let [out (java.io.StringWriter.)
        err (java.io.StringWriter.)]
    (with-redefs-fn {#'shell/execute (cluster objects (set silent))}
      (fn []
        (let [result (binding [*out* out *err* err
                               outcome/*record* (outcome/fresh)
                               kube-context/*pinned* {:context "ctx" :cluster "cluster"}]
                       (detective/scan! {:collect (:collect compiled) :extra (:extra compiled) :context (delay "ctx")}
                                        (:detectives compiled) (merge {:output "json"} opts)))]
          {:result result
           :out (stdout/strip-colors (str out))
           :err (stdout/strip-colors (str err))
           :commands @shelled})))))

(defn- report
  [r]
  (json/parse-string (:out r) true))

(defn- shop
  [& {:keys [settings deployments] :or {settings {:mode "live"}}}]
  {"configmaps" (if settings [(object "ConfigMap" "shop" "settings" :data settings)] [])
   "deployments" (or deployments
                     [(object "Deployment" "shop" "web" :spec {:replicas 3})
                      (object "Deployment" "shop" "worker" :spec {:replicas 1}
                              :metadata {:creationTimestamp "2026-11-27T10:00:00Z"})])})

;; --- what compile-detective refuses -------------------------------------------------

(defn- refusal-of
  [data]
  (try (dsl/compile-detective data) nil
       (catch clojure.lang.ExceptionInfo e
         (when (dsl/invalid? e) (ex-message e)))))

(def ^:private refusals
  "[what is wrong, the detective, the start of the reason]."
  (let [d (source)
        check (first (:checks d))
        with-check #(assoc d :checks [(merge check %)])]
    (concat
     (for [k [:target :values :state :refuse :problems :pin :confirm :items :announce :effect :recovery :opt-out :title]]
       [(str "the change key " k) (assoc d k nil) (str "[" k "]: " k " belongs to a change - a detective is read-only")])
     [["no version" (dissoc d :gumshoe/dsl) "[:gumshoe/dsl]: the detective declares no version"]
      ["another version" (assoc d :gumshoe/dsl :v1) "[:gumshoe/dsl]: the detective declares :gumshoe/dsl :v1"]
      ["another kind" (assoc d :kind :scan) "[:kind]: the :kind is :detective, not :scan"]
      ["an unknown key" (assoc d :steps []) "[:steps]: :steps is not a key of a detective"]
      ["a symbol" (assoc d :description 'clojure.core/eval) "[:description]: clojure.core/eval is a symbol"]
      ["no reads" (dissoc d :reads) "[:reads]: a detective declares what it reads"]
      ["no checks" (assoc d :checks []) "[:checks]: a detective declares one or more checks"]
      ["a read nobody judges" (assoc-in d [:reads :pods] [:list "pods" {:namespace :all}]) "[:reads :pods]: no check judges :pods"]
      ["a read named as the engine binds" (assoc-in d [:reads :context] [:list "pods" {:namespace :all}]) "[:reads :context]: :context is a name the engine binds"]
      ["a read named :now" (-> d (assoc-in [:reads :now] [:list "pods" {:namespace :all}])) "[:reads :now]: :now is a name a check binds"]
      ["a kubectl write as a read" (assoc-in d [:reads :nodes] [:kubectl :delete]) "[:reads :nodes]: [:kubectl :delete] is no read"]
      ["an effect as a read" (assoc-in d [:reads :nodes] [:effect :write-file "x"]) "[:reads :nodes]: [:effect :write-file \"x\"] is no read"]
      ["a list naming no scope" (assoc-in d [:reads :nodes] [:list "nodes" {}]) "[:reads :nodes 2 :namespace]: a detective's read names where it reads"]
      ["a get of every namespace" (assoc-in d [:reads :nodes] [:get "nodes" "w1" {:namespace :all}]) "[:reads :nodes 3 :namespace]: :all reads every namespace"]
      ["a value function as a read" (assoc-in d [:reads :nodes] [:call :gumshoe.dsl-detective-test/owned? [:context]])
       "[:reads :nodes 1]: :gumshoe.dsl-detective-test/owned? is a :predicate function"]
      ["an --output option" (assoc d :options {:output {:desc "x"}}) "[:options :output]: --output is the report format"]
      ["an option without a description" (assoc d :options {:node {:coerce :string}}) "[:options :node :desc]: an option's :desc is a non-blank string"]
      ["no prerequisites" (dissoc d :prerequisites) "[:prerequisites]: the detective declares its :prerequisites as a map"]
      ["a check without :expect" (assoc d :checks [(dissoc check :expect)]) "[:checks 0 :expect]: a check says what holds"]
      ["a check with an unknown key" (with-check {:effect [[:kubectl :delete]]}) "[:checks 0 :effect]: :effect is not a key of a check"]
      ["an unknown severity" (with-check {:severity :error}) "[:checks 0 :severity]: :error is no severity"]
      ["two checks of one name" (assoc d :checks [check check]) "[:checks 1 :name]: \"cordoned-nodes\" names an earlier check"]
      ["an :each over no read" (with-check {:each :pods}) "[:checks 0 :each]: [:read :pods] names no declared :reads entry"]
      ["a check reading nothing" (assoc d :checks [(-> check (dissoc :each) (assoc :expect [:= 1 1]))]) "[:checks 0]: the check reads nothing"]
      ["a field outside an :each" (assoc d :checks [(dissoc check :each)]) "[:checks 0 :expect 1]: [:field ..] judges the object an :each check judges"]
      ["the picked object" (with-check {:expect [:some? [:object :spec]]}) "[:checks 0 :expect 1 0]: [:object :spec] is not available in the check"]
      ["the target" (with-check {:says "{target} is cordoned"}) "[:checks 0 :says]: {target}: [:target] is not available"]
      ["a read function in a check" (with-check {:expect [:some? [:call :gumshoe.dsl-detective-test/inventory [:each] "x"]]})
       "[:checks 0 :expect 1 1]: :gumshoe.dsl-detective-test/inventory is a :read function"]
      ["an effect in a check" (with-check {:expect [:some? [:effect :write-file "x"]]}) "[:checks 0 :expect 1 0]: :effect is no expression"]
      ["[:present] of no read" (with-check {:expect [:present :pods]}) "[:checks 0 :expect 1]: [:read :pods] names no declared :reads entry"]
      ["[:present] of no keyword" (with-check {:expect [:present "nodes"]}) "[:checks 0 :expect 1]: [:present ..] names a :reads entry as a keyword"]
      ["an empty reason without :each" (assoc d :checks [(-> check (dissoc :each) (assoc :expect [:present :nodes] :empty-is-clean "x"))])
       "[:checks 0 :empty-is-clean]: only an :each check judges objects"]
      ["a blank empty reason" (with-check {:empty-is-clean ""}) "[:checks 0 :empty-is-clean]: the reason an empty read is clean"]])))

(deftest compile-detective-refuses-what-a-detective-cannot-say
  (doseq [[what data expected] refusals]
    (testing what
      (let [reason (refusal-of data)]
        (is (some? reason) "refused")
        (is (and reason (str/starts-with? reason expected)) (str "reason: " reason))))))

(deftest a-change-refuses-a-kind-and-a-detective-predicate
  (let [change (edn/read-string (slurp "tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn"))]
    (is (nil? (try (dsl/compile change) nil (catch Exception e (ex-message e)))) "the change compiles")
    (is (str/starts-with? (try (dsl/compile (assoc change :kind :detective)) "" (catch Exception e (ex-message e)))
                          "[:kind]: a change declares no :kind"))
    (is (str/starts-with? (try (dsl/compile (assoc change :refuse [{:when [:present :x] :says "x"}])) ""
                               (catch Exception e (ex-message e)))
                          "[:refuse 0 :when]: [:present :read] is a detective's check"))))

(deftest the-fixture-and-the-ports-compile
  (doseq [d [(source) audit settings]]
    (is (dsl/detective? (dsl/compile-detective d)))
    (is (= d (dsl/source-of (dsl/compile-detective d))))))

;; --- read-only ------------------------------------------------------------------------

(deftest the-compiled-detective-holds-no-way-to-write
  (let [compiled (dsl/compile-detective settings)]
    (testing "it is a detective book spec and nothing else"
      (is (= #{:description :prerequisites :options :detectives :collect :extra} (set (keys compiled))))
      (is (every? #(= #{:name :description :requires :joins :detect} (disj (set (keys %)) :judges))
                  (:detectives compiled))))
    (testing "the book it builds announces nothing"
      (let [config (binding [runbook/*loading* true] (detective/book compiled))]
        (is (false? (:announce? config)))))
    (testing "a scan runs only reads, and never an effect, a confirmation or an announcement"
      (let [touched (atom [])]
        (with-redefs [effect/run! (fn [& args] (swap! touched conj [:effect args]) (throw (ex-info "an effect ran" {})))
                      flow/change! (fn [& args] (swap! touched conj [:change args]) (throw (ex-info "a change ran" {})))
                      announce/announce! (fn [& args] (swap! touched conj [:announce args]))]
          (let [r (scan compiled {:objects (shop)})]
            (is (= [] @touched))
            (is (seq (:commands r)))
            (is (every? #(= "get" (first (:words (parse-argv %)))) (:commands r)) (pr-str (:commands r)))))))
    (testing "a registered predicate that runs a command crashes its check, which fails the run, and the command never starts"
      (let [shelling (assoc (source) :checks [(assoc (first (:checks (source))) :expect [:call :gumshoe.dsl-detective-test/shells-out? [:each]])])
            r (scan (dsl/compile-detective shelling) {:objects {"nodes" [(object "Node" nil "w1")]}})
            [finding] (:findings (report r))]
        (is (false? (:result r)))
        (is (= "critical" (:severity finding)))
        (is (str/includes? (:hint finding) "not running 'kubectl delete namespace shop'"))
        (is (not-any? #(some #{"delete"} %) (:commands r)))))))

;; --- whole scans ----------------------------------------------------------------------

(deftest a-scan-reports-what-each-check-found
  (let [r (scan (dsl/compile-detective settings) {:objects (shop)})
        {:keys [findings checked summary reads]} (report r)]
    (is (true? (:result r)) "no critical finding")
    (is (= [{:severity "warning" :component "shop/worker" :summary "Deployment worker asks for 1 replica(s)"
             :since "2026-11-27T10:00:00Z" :detective "replicas"}]
           findings))
    (is (= {:critical 0 :warning 1 :info 0} summary))
    (is (= ["settings-present" "settings-mode" "replicas"] (map :name checked)))
    (is (not-any? :unread checked))
    (is (= [{:name "deployments" :state "present" :count 2} {:name "settings" :state "present" :count 1}] reads))))

(deftest absent-and-present-are-judged-apart
  (testing "an object the cluster answered is not there is absent: the presence check reports, the check that allows its absence does not"
    (let [r (scan (dsl/compile-detective settings) {:objects (shop :settings nil)})
          {:keys [findings checked reads]} (report r)]
      (is (false? (:result r)))
      (is (= #{["settings-present" "critical" "ConfigMap shop/settings is missing"]
               ["replicas" "warning" "Deployment worker asks for 1 replica(s)"]}
             (set (map (juxt :detective :severity :summary) findings))))
      (is (not-any? :unread checked))
      (is (some #(= {:name "settings" :state "absent" :count 0} %) reads))))
  (testing "a present object without the field reports, since a missing field holds no predicate"
    (let [{:keys [findings]} (report (scan (dsl/compile-detective settings) {:objects (shop :settings {:other "x"})}))]
      (is (some #(= "ConfigMap shop/settings names no mode" (:summary %)) findings))))
  (testing "an empty list is absent and an :each over it judged nothing, so the check reads nothing"
    (let [{:keys [checked]} (report (scan (dsl/compile-detective settings) {:objects (shop :deployments [])}))]
      (is (= [false false true] (map #(boolean (:unread %)) checked))))))

(deftest an-unread-read-leaves-its-checks-unread-and-never-passes
  (testing "the Deployments read does not answer: the replicas check reads nothing and reports nothing, the others judge"
    (let [r (scan (dsl/compile-detective settings) {:objects (shop) :silent #{"deployments"}})
          {:keys [findings checked reads nothing-checked]} (report r)]
      (is (= [] findings) "no finding rests on the read that did not answer")
      (is (= {"settings-present" false "settings-mode" false "replicas" true}
             (into {} (map (juxt :name #(boolean (:unread %)))) checked)))
      (is (nil? nothing-checked))
      (is (= "unread" (:state (first (filter #(= "deployments" (:name %)) reads)))))
      (is (str/includes? (:reason (first (filter #(= "deployments" (:name %)) reads))) "i/o timeout"))
      (is (str/includes? (:err r) "could not read :deployments - "))))
  (testing "every read silent: nothing was checked, and the run fails"
    (let [r (scan (dsl/compile-detective settings) {:objects (shop) :silent #{"deployments" "configmaps"}})
          {:keys [findings checked nothing-checked]} (report r)]
      (is (false? (:result r)))
      (is (true? nothing-checked))
      (is (= [] findings))
      (is (every? :unread checked))))
  (testing "control: the same cluster answering judges and reports"
    (is (seq (:findings (report (scan (dsl/compile-detective settings) {:objects (shop)})))))))

(deftest a-missing-flag-reads-nothing-rather-than-the-default-namespace
  (reset! inventory-answer (fn [_] (throw (ex-info "never asked" {}))))
  (let [unflagged (assoc audit :reads {:inventory [:list "configmaps" {:namespace [:opt :namespace]}]})
        r (scan (dsl/compile-detective unflagged) {:objects {"configmaps" [(object "ConfigMap" "default" "loose")]}})]
    (is (= [] (:commands r)) "no kubectl call")
    (is (true? (:nothing-checked (report r))))
    (is (str/includes? (:err r) "its namespace evaluated to nil, so it was not read"))))

(deftest a-read-function-must-return-reads-results
  (doseq [[what answer reason] [["nil" nil "it returned nil, which is no gumshoe.reads result"]
                                ["a bare list" {:ok? true :items []} "it returned {:ok? true, :items []}, which is no gumshoe.reads result"]
                                ["no result at all" [] "it returned no gumshoe.reads result at all"]
                                ["a throw" ::throws "inventory broke"]
                                ["an unread result" (result/unread {:type "everything" :namespace "shop"} "forbidden")
                                 "forbidden"]]]
    (testing what
      (reset! inventory-answer (fn [_] (if (= ::throws answer) (throw (ex-info "inventory broke" {})) answer)))
      (let [r (scan (dsl/compile-detective audit) {:objects {} :opts {:namespace "shop"}})
            {:keys [findings nothing-checked reads]} (report r)]
        (is (= [] findings))
        (is (true? nothing-checked))
        (is (str/includes? (:reason (first reads)) reason) (pr-str reads))))))

(deftest the-ported-audit-reports-every-unowned-object
  (let [objects [(object "ConfigMap" "shop" "kube-root-ca.crt")
                 (object "Deployment" "shop" "moodle" :metadata {:labels {:app.kubernetes.io/managed-by "Helm"}})
                 (object "Pod" "shop" "moodle-1" :metadata {:ownerReferences [{:kind "ReplicaSet"}]})
                 (object "Secret" "shop" "tideways-token")
                 (object "Service" "shop" "mdl-legacy")]]
    (reset! inventory-answer (fn [namespace] {:state :present :ref {:type "everything" :namespace namespace} :items objects}))
    (let [r (scan (dsl/compile-detective audit) {:objects {} :opts {:namespace "shop"}})
          {:keys [findings]} (report r)]
      (is (false? (:result r)))
      (is (= [["shop/tideways-token" "Secret tideways-token belongs to nobody" "purge it, or name its owner"]
              ["shop/mdl-legacy" "Service mdl-legacy belongs to nobody" "purge it, or name its owner"]]
             (map (juxt :component :summary :hint) findings))))
    (testing "an inventory that found nothing judged nothing, as the audit says of an empty namespace"
      (reset! inventory-answer (fn [namespace] {:state :absent :ref {:type "everything" :namespace namespace} :items []}))
      (is (true? (:nothing-checked (report (scan (dsl/compile-detective audit) {:objects {} :opts {:namespace "shop"}}))))))
    (testing "control: :empty-is-clean says why it is clean, and the check counts as read"
      (let [clean (assoc-in audit [:checks 0 :empty-is-clean] "an empty namespace holds nothing unowned")
            {:keys [checked nothing-checked findings]} (report (scan (dsl/compile-detective clean) {:objects {} :opts {:namespace "shop"}}))]
        (is (nil? nothing-checked))
        (is (= [] findings))
        (is (not-any? :unread checked))))))

(deftest every-report-format-carries-the-findings
  (let [compiled (dsl/compile-detective settings)
        json-report (report (scan compiled {:objects (shop)}))
        edn-report (edn/read-string (:out (scan compiled {:objects (shop) :opts {:output "edn"}})))
        text (scan compiled {:objects (shop) :opts {:output "text"}})]
    (is (= (:findings json-report)
           (mapv #(update % :severity name) (:findings edn-report))))
    (is (= [:present :present] (map :state (:reads edn-report))))
    (is (str/includes? (:out text) "Deployment worker asks for 1 replica(s)"))
    (is (str/includes? (:out text) "shop/worker"))
    (is (str/includes? (:out text) "2 other checks came back clean: settings-present, settings-mode"))
    (testing "an unread check is named as reading nothing in text"
      (let [silent (scan compiled {:objects (shop) :silent #{"deployments"} :opts {:output "text"}})]
        (is (str/includes? (:out silent) "1 check read nothing on this cluster"))
        (is (str/includes? (:out silent) "replicas"))))
    (testing "cluster text in a reason is escaped in json and edn as in every report"
      (reset! inventory-answer (fn [_] (result/unread {:type "everything" :namespace "shop"} "denied\u001b[31m")))
      (let [r (scan (dsl/compile-detective audit) {:objects {} :opts {:namespace "shop"}})]
        (is (not (str/includes? (:out r) "\u001b")))
        (is (str/includes? (:reason (first (:reads (report r)))) "denied\u001b[31m"))))))

;; --- equivalence with the Clojure detective ---------------------------------------------

(def ^:private node-evidence
  [(object "Node" nil "w1" :spec {:unschedulable true})
   (object "Node" nil "w2")
   (object "Node" nil "w3" :spec {:unschedulable false})
   (object "Node" nil "w4" :spec {:unschedulable "true"})
   (object "Node" nil "w5" :spec {:unschedulable true}
           :status {:conditions [{:type "Ready" :status "True"}]})])

(defn- cordon-findings
  [findings]
  (filter #(= "node is cordoned (unschedulable)" (:summary %)) findings))

(deftest the-data-detective-finds-what-the-nodes-detective-finds
  (let [compiled (dsl/compile-detective (source))
        ready (mapv #(assoc-in % [:status :conditions] [{:type "Ready" :status "True"}]) node-evidence)
        clojure (cordon-findings (detective/run-detectives nodes/detectives {"nodes" {:items ready}}))
        r (scan compiled {:objects {"nodes" ready}})
        data (:findings (report r))]
    (is (= ["w1" "w5"] (map :component clojure)) "the evidence holds two cordoned nodes")
    (is (= (map #(select-keys % [:severity :component :summary :hint]) clojure)
           (map #(-> % (select-keys [:severity :component :summary :hint]) (update :severity keyword)) data)))
    (testing "the same read: one list of the cluster-scoped nodes"
      (is (= [["kubectl" "--context=ctx" "--request-timeout=30s" "get" "nodes" "--output=json"]] (:commands r))))
    (testing "control: a port that judges another field differs"
      (let [wrong (assoc-in (source) [:checks 0 :expect] [:not [:field [:spec :unschedulable] := "true"]])]
        (is (not= (map :component clojure)
                  (map :component (:findings (report (scan (dsl/compile-detective wrong) {:objects {"nodes" ready}}))))))))))

;; --- negative controls ---------------------------------------------------------------

(deftest controls-each-safeguard-is-what-the-tests-see
  (testing "were an unread read handed over as absent, the replicas check would pass on a read nobody made"
    (with-redefs-fn {#'gumshoe.dsl/read-outcome (fn [_] {:state :absent :items [] :data []})}
      (fn [] (let [{:keys [checked]} (report (scan (dsl/compile-detective
                                              (assoc-in settings [:checks 2 :empty-is-clean] "control"))
                                             {:objects (shop) :silent #{"deployments"}}))]
         (is (not (:unread (last checked))) "the control shows the test would catch it")))))
  (testing "were checks run outside gumshoe.offline, a predicate's command would start"
    (with-redefs-fn {#'gumshoe.offline/forbidding (fn [_ thunk] (thunk))}
      (fn [] (let [shelling (assoc (source) :checks [(assoc (first (:checks (source))) :expect [:call :gumshoe.dsl-detective-test/shells-out? [:each]])])
             r (scan (dsl/compile-detective shelling) {:objects {"nodes" [(object "Node" nil "w1")]}})]
         (is (some #(some #{"delete"} %) (:commands r)) "the control shows the offline binding is what stops it"))))))

;; --- joining a scope, listing and conformance -----------------------------------------

(defn- joins-a-scope
  []
  (let [names (dsl/register-detective! ::scope (dsl/compile-detective (source)))
        registered (detectives/for-scope ::scope)]
    (is (= ["cordoned-nodes"] names (mapv :name registered)))
    (with-redefs-fn {#'shell/execute (cluster {"nodes" node-evidence} #{})}
      (fn [] (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
         (let [evidence (detective/collect-evidence! "ctx" registered)
               checked (detective/mark-unread registered evidence)]
           (is (= ["w1" "w5"] (map :component (detective/run-detectives checked evidence))))))))
    (testing "a read that does not answer leaves the scope's check unread"
      (with-redefs-fn {#'shell/execute (cluster {} #{"nodes"})}
        (fn [] (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
           (let [evidence (detective/collect-evidence! "ctx" registered)]
             (is (every? :unread (detective/mark-unread registered evidence))))))))
    (testing "a detective that reads a flag is refused, since the scanning book's flags are not its own"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reads a flag"
                            (dsl/register-detective! ::scope (dsl/compile-detective audit)))))))

(deftest a-detective-joins-a-scope-as-a-plugin-detective-does
  (try
    (joins-a-scope)
    (finally
      ;; the registry is global: leave no test scope behind for later scans
      (swap! @#'detectives/registry dissoc ::scope))))

(deftest the-catalog-lists-an-edn-detective-and-the-loader-builds-its-book
  (fs/with-temp-dir [dir {}]
    (let [root (fs/path dir "runbooks")]
      (fs/create-dirs (fs/path root "team"))
      (fs/copy cordoned-edn (fs/path root "team" "cordoned.edn"))
      (let [books (catalog/books [(str root)] (str dir))]
        (is (= [["runbooks/team/cordoned.edn" "Reports every cordoned node, which takes no new pods"]]
               (map (juxt :id :description) books))))))
  (let [config (binding [runbook/*loading* true] (edn-book/book cordoned-edn))]
    (is (= "Reports every cordoned node, which takes no new pods" (:description config)))
    (is (contains? (:options config) :output))
    (is (false? (:announce? config)))))

(deftest conformance-accepts-the-edn-detective
  (is (= {:findings [] :advisories []}
         (conformance/report {:books [cordoned-edn] :libraries [] :license "0BSD" :guards :require}))))
