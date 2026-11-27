;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.kube-context-test
  "A change spec's :kube-context takes the kube context from the parsed
   flags, is resolved once from the kubeconfig and reaches every kubectl
   call of the run: the candidate query, the subject, :pin and :subjects
   reads, :reads, :values, :derive, the post-checks and the effects, the
   cluster the confirmation names and the announcement. Every program is
   faked: shell/execute answers from an in-memory kubeconfig and cluster
   and records every argv, and the effects are recorded instead of run."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.change :as change]
            [gumshoe.dispatch :as dispatch]
            [gumshoe.effect :as effect]
            [gumshoe.interact :as interact]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.playbook :as playbook]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.verify :as verify]))

;; --- the fake kubeconfig and cluster ----------------------------------------------

(def ^:private kubeconfig
  {:kind "Config"
   :current-context "laptop"
   :contexts [{:name "laptop" :context {:cluster "kind-laptop" :user "me"}}
              {:name "prod" :context {:cluster "kube.prod.example" :user "ops"}}
              {:name "staging" :context {:cluster "kube.staging.example" :user "ops"}}]
   :clusters [{:name "kind-laptop" :cluster {:server "https://127.0.0.1:6443"}}
              {:name "kube.prod.example" :cluster {:server "https://prod.invalid"}}
              {:name "kube.staging.example" :cluster {:server "https://staging.invalid"}}]
   :users [{:name "me" :user {:token "REDACTED"}} {:name "ops" :user {:token "REDACTED"}}]})

(defn- minified
  "What `kubectl config view --minify` prints for the kubeconfig's current
   context."
  [config]
  (let [current (:current-context config)
        entry (first (filter #(= current (:name %)) (:contexts config)))]
    (assoc config :contexts (if entry [entry] []))))

(defn- namespace-object
  [name]
  {:kind "Namespace" :metadata {:name name :uid (str "uid-" name) :resourceVersion "1"}})

(defn- context-of-argv
  [argv]
  (some #(when (str/starts-with? % "--context=") (subs % (count "--context="))) argv))

(defn- answer
  "The answer of the fake programs to argv, given the kubeconfig in config."
  [config argv]
  (let [args (vec argv)]
    (cond
      (= args ["kubectl" "config" "view" "--minify" "--output" "json"])
      {:exit 0 :out (json/generate-string (minified @config)) :err ""}

      (= args ["kubectl" "config" "view" "--output" "json"])
      {:exit 0 :out (json/generate-string @config) :err ""}

      (and (= "kubectl" (first args)) (some #{"auth"} args))
      {:exit 0 :out "" :err ""}

      (and (= "kubectl" (first args)) (some #{"get"} args))
      (let [after (rest (drop-while #(not= "get" %) args))
            [type name] (remove #(str/starts-with? % "-") after)]
        (cond
          (= "nodes" type) {:exit 0 :out (json/generate-string {:items []}) :err ""}
          (and (= "namespaces" type) name) {:exit 0 :out (json/generate-string (namespace-object name)) :err ""}
          (= "namespaces" type) {:exit 0 :out (json/generate-string {:items [(namespace-object "web")
                                                                             (namespace-object "api")]})
                                 :err ""}
          :else {:exit 1 :out "" :err "error: the server doesn't have a resource type"}))

      :else {:exit 0 :out "" :err ""})))

(defn- recorded-run
  "Runs (f) against the fake programs and returns {:result :calls :effects
   :confirmations :announced :out}: every argv shell/execute received, every
   argv an effect ran, every confirmation asked and every announcement.
   The kubeconfig is `config` (default kubeconfig); `on-call` is called with
   each argv once it is answered, so a test can switch the kubeconfig in
   the middle of a run."
  ([f] (recorded-run {} f))
  ([{:keys [config on-call] :or {config kubeconfig}} f]
   (let [config (atom config)
         calls (atom [])
         effects (atom [])
         confirmations (atom [])
         announced (atom [])
         execute (fn [& argv]
                   (swap! calls conj (vec argv))
                   (let [answered (answer config argv)]
                     (when on-call (on-call (vec argv) config))
                     answered))
         run (fn [& argv] (swap! effects conj (vec argv)) 0)
         out (java.io.StringWriter.)
         eventually verify/eventually
         result (with-redefs [shell/execute execute
                              shell/execute-with-stdin (fn [_stdin & argv] (swap! effects conj (vec argv)) {:exit 0 :out "" :err ""})
                              shell/run-attributed run
                              shell/run-with-output run
                              interact/confirm! (fn [confirmation] (swap! confirmations conj confirmation) true)
                              announce/announce! (fn [system data message] (swap! announced conj [system data message]))
                              verify/eventually #(eventually (assoc % :timeout 0))]
                  (binding [*out* out *err* out]
                    (f)))]
     {:result result
      :calls @calls
      :effects @effects
      :confirmations @confirmations
      :announced @announced
      :out (str out)})))

(defn- config-reads
  [calls]
  (filter #(= ["kubectl" "config" "view"] (take 3 %)) calls))

(defn- kubectl-calls
  "Every kubectl argv of a run that talks to a cluster: every one but the
   kubeconfig reads."
  [{:keys [calls effects]}]
  (remove #(= ["kubectl" "config" "view"] (take 3 %))
          (filter #(= "kubectl" (first %)) (concat calls effects))))

(defn stray-contexts
  "The kubectl argv among calls that do not carry exactly one --context,
   naming `context`. Empty when every call is pinned to it."
  [calls context]
  (remove (fn [argv]
            (let [given (filter #(str/starts-with? % "--context=") argv)]
              (= [(str "--context=" context)] given)))
          calls))

;; --- the specs ---------------------------------------------------------------------

(def ^:private from-flag (fn [opts] (:context opts)))

(defn- namespace-candidates
  [context]
  (map kubectl/name-of (kubectl/items-of (kubectl/get-json context "namespaces"))))

(defn- base-spec
  [& {:as overrides}]
  (merge {:kube-context from-flag
          :select {:mode :one :label "namespace" :flag :namespace :candidates namespace-candidates}
          :confirm {:action "label a namespace" :destructive? false}
          :reads {:live (fn [{:keys [context target]}] (kubectl/get-cluster-resource context "namespaces" target))}
          :values {:nodes (fn [{:keys [context]}] (kubectl/get-json context "nodes"))}
          :refusal (fn [{:keys [live]}] (when-not live "it is gone"))
          :announce (fn [{:keys [target]}] (str "Label " target))
          :effect (fn [{:keys [context target]}]
                    (effect/plan (effect/kubectl context "label" "namespace" target "team=web")
                                 (effect/kubectl-stdin context "{}" "apply" "--filename" "-")))
          :verify (fn [{:keys [context target]}]
                    [{:description "the namespace still exists"
                      :check #(some? (kubectl/get-cluster-resource context "namespaces" target))}])}
         overrides))

(def ^:private subject-kinds
  "One spec per way a change declares what it acts on."
  {:subject (base-spec :subject {:type "namespaces"})
   :pin (base-spec :pin (fn [{:keys [target]}] {:type "namespaces" :name target}))
   :subjects (base-spec :subjects (fn [{:keys [target]}] [{:type "namespaces" :name target}]))
   :derive (-> (base-spec :subject {:type "namespaces"})
               (dissoc :values :reads)
               (assoc :refusal (fn [{:keys [object]}] (when-not object "it is gone")))
               (assoc :derive (fn [{:keys [context]}] {:nodes (kubectl/get-json context "nodes")})))})

(defn- as-playbook
  [spec]
  {:description "label it"
   :kube-context from-flag
   :select (:select spec)
   :confirm {:action "label it" :destructive? false}
   :steps [(dissoc spec :select) (dissoc spec :select)]})

(def ^:private entry-points
  "Every way a change spec runs, each (fn [spec opts]) -> result."
  {:run! (fn [spec opts] (mutation/run! spec opts {:cluster "kind-laptop" :actor "me@laptop"}))
   :run-on! (fn [spec opts] (mutation/run-on! spec "web" opts {:cluster "kind-laptop" :actor "me@laptop"}))
   :playbook (fn [spec opts] (playbook/run! (as-playbook spec) opts {:cluster "kind-laptop" :actor "me@laptop"}))
   :dispatch (fn [spec opts]
               (dispatch/run! {:kube-context from-flag
                               :select (:select spec)
                               :classify (constantly :web)
                               :routes {:web {:playbook (as-playbook spec)}}}
                              opts {:cluster "kind-laptop" :actor "me@laptop"}))})

;; --- (c): one spec under an overridden context ------------------------------------

(deftest a-subject-spec-under-an-overridden-context-plans-its-effects-on-that-context
  (let [run (recorded-run #(mutation/run! (:subject subject-kinds) {:context "prod" :namespace "web"} nil))]
    (is (true? (:result run)) (:out run))
    (testing "the effect plan carries the overridden context, not the current one"
      (is (= [["kubectl" "--context=prod" "label" "namespace" "web" "team=web"]
              ["kubectl" "--context=prod" "apply" "--filename" "-"]]
             (:effects run))))
    (testing "nothing ran on the current context"
      (is (empty? (filter #(some #{"--context=laptop"} %) (kubectl-calls run)))))))

(deftest a-pin-spec-under-an-overridden-context-plans-its-effects-on-that-context
  (let [run (recorded-run #(mutation/run! (:pin subject-kinds) {:context "staging" :namespace "web"} nil))]
    (is (true? (:result run)) (:out run))
    (is (= #{"--context=staging"} (set (map second (:effects run)))))
    (is (empty? (stray-contexts (kubectl-calls run) "staging")))))

;; --- every kubectl call carries the resolved context ------------------------------

(deftest every-kubectl-call-of-every-entry-point-carries-the-resolved-context
  (doseq [[kind spec] subject-kinds
          [entry run!] entry-points
          [flag expected] [["prod" "prod"] ["staging" "staging"] [nil "laptop"]]]
    (testing (format "%s through %s with --context %s" kind entry (pr-str flag))
      (let [run (recorded-run #(run! spec (cond-> {:namespace "web"} flag (assoc :context flag))))
            calls (kubectl-calls run)]
        (is (true? (:result run)) (:out run))
        (is (seq (:effects run)) "the change ran its effects")
        (is (some #(some #{"get"} %) calls) "the run read the cluster")
        (is (empty? (stray-contexts calls expected))
            (str "calls not pinned to " expected ": " (pr-str (stray-contexts calls expected))))))))

(deftest the-stray-check-finds-a-call-without-or-with-another-context
  (is (= [["kubectl" "get" "pods"]]
         (stray-contexts [["kubectl" "get" "pods"] ["kubectl" "--context=prod" "get" "pods"]] "prod")))
  (is (= [["kubectl" "--context=laptop" "delete" "pod" "x"]]
         (stray-contexts [["kubectl" "--context=laptop" "delete" "pod" "x"]] "prod")))
  (is (= [["kubectl" "--context=prod" "--context=laptop" "get" "pods"]]
         (stray-contexts [["kubectl" "--context=prod" "--context=laptop" "get" "pods"]] "prod"))))

(deftest a-reader-dropping-the-context-is-caught-by-the-property
  ;; positive control: the same run with a candidate query that ignores the
  ;; context it is handed leaves a call the property names
  (let [spec (base-spec :subject {:type "namespaces"}
                        :select {:mode :one :label "namespace" :flag :namespace
                                 :candidates (fn [_] (namespace-candidates nil))})
        run (recorded-run #(mutation/run! spec {:context "prod" :namespace "web"} nil))]
    (is (= [["kubectl" "--context=" "--request-timeout=30s" "get" "namespaces" "--output=json"]]
           (stray-contexts (kubectl-calls run) "prod")))))

;; --- the confirmation and the announcement ----------------------------------------

(deftest the-confirmation-and-the-announcement-name-the-overridden-cluster
  (let [run (recorded-run #(mutation/run! (:subject subject-kinds) {:context "prod" :namespace "web"}
                                          {:cluster "kind-laptop" :actor "me@laptop"}))
        [confirmation] (:confirmations run)
        [[system data message]] (:announced run)]
    (is (= "This will label a namespace on 'kube.prod.example':\n  - web"
           (interact/confirmation-message confirmation)))
    (testing "the changelog names the overridden cluster and picks its environment"
      (is (= "kube.prod.example" system))
      (is (= "kube.prod.example" (:cluster data)))
      (is (= "me@laptop" (:actor data)))
      (is (= "Label web" message)))))

(deftest the-callbacks-receive-the-overridden-context-and-cluster
  (let [seen (atom [])
        spy (fn [k] (fn [{:keys [context cluster]}] (swap! seen conj [k context cluster]) nil))
        spec (base-spec :subject {:type "namespaces"}
                        :values {:spied (spy :value)}
                        :items (fn [ctx] ((spy :items) ctx) ["web"])
                        :refusal (spy :refusal))
        run (recorded-run #(mutation/run! spec {:context "prod" :namespace "web"} nil))]
    (is (true? (:result run)) (:out run))
    (is (= #{["prod" "kube.prod.example"]} (set (map rest @seen))))
    (is (= #{:value :items :refusal} (set (map first @seen))))))

;; --- a single resolution per run ---------------------------------------------------

(deftest a-run-reads-the-kubeconfig-once
  (doseq [[entry run!] entry-points
          [flag argv] [["prod" ["kubectl" "config" "view" "--output" "json"]]
                       [nil ["kubectl" "config" "view" "--minify" "--output" "json"]]]]
    (testing (format "%s with --context %s" entry (pr-str flag))
      (let [run (recorded-run #(run! (:subject subject-kinds) (cond-> {:namespace "web"} flag (assoc :context flag))))]
        (is (true? (:result run)) (:out run))
        (is (= [argv] (config-reads (:calls run))))))))

(deftest a-kubeconfig-switched-during-the-run-does-not-move-it
  ;; the operator switches the current context in another terminal right
  ;; after the run resolved it; every later call stays on the first one
  (doseq [[entry run!] entry-points]
    (testing (str entry)
      (let [run (recorded-run {:on-call (fn [argv config]
                                          (when (= "config" (second argv))
                                            (swap! config assoc :current-context "staging")))}
                              #(run! (:subject subject-kinds) {:namespace "web"}))]
        (is (true? (:result run)) (:out run))
        (is (empty? (stray-contexts (kubectl-calls run) "laptop")))
        (is (= "kind-laptop" (:target (first (:confirmations run)))))))))

(deftest a-run-resolving-again-per-step-would-split-across-the-switch
  ;; positive control for the test above: with the pin ignored, a playbook
  ;; resolves the kube context again for each step and lands on the
  ;; context the operator switched to
  (let [resolve-always (fn [kube opts] (kube-context/resolve (kube-context/named kube opts)))
        run (with-redefs [kube-context/connection resolve-always]
              (recorded-run {:on-call (fn [argv config]
                                        (when (= "config" (second argv))
                                          (swap! config assoc :current-context "staging")))}
                            #((:playbook entry-points) (:subject subject-kinds) {:namespace "web"})))]
    (is (seq (stray-contexts (kubectl-calls run) "laptop")))
    (is (seq (filter #(some #{"--context=staging"} %) (:effects run))))))

(deftest the-current-context-and-its-cluster-come-from-one-reading
  (let [calls (atom 0)]
    (with-redefs [shell/execute (fn [& _]
                                  (swap! calls inc)
                                  {:exit 0 :out (json/generate-string (minified kubeconfig)) :err ""})]
      (is (= {:context "laptop" :cluster "kind-laptop"} (kubectl/kube-connection)))
      (is (= 1 @calls)))))

;; --- an unknown context fails closed ----------------------------------------------

(deftest an-unknown-context-fails-before-anything-is-read
  (doseq [[entry run!] entry-points
          flag ["nope" "" "  "]]
    (testing (format "%s with --context %s" entry (pr-str flag))
      (let [run (recorded-run #(run! (:subject subject-kinds) {:context flag :namespace "web"}))]
        (is (false? (:result run)))
        (is (empty? (kubectl-calls run)) "no read and no effect")
        (is (empty? (:confirmations run)))
        (is (empty? (:announced run)))
        (is (str/includes? (:out run) "refusing: "))))))

(deftest resolve-names-why-it-refuses
  (with-redefs [shell/execute (fn [& _] {:exit 0 :out (json/generate-string kubeconfig) :err ""})]
    (is (= {:connection {:context "prod" :cluster "kube.prod.example" :announce-as "kube.prod.example"}}
           (kube-context/resolve "prod")))
    (is (= {:refusal "the kubeconfig has no kube context named nope"} (kube-context/resolve "nope")))
    (is (= {:refusal "the kubeconfig has no kube context named Prod"} (kube-context/resolve "Prod")))
    (is (= {:refusal "the kube context must be a non-blank name, not \"\""} (kube-context/resolve "")))
    (is (= {:refusal "the kube context must be a non-blank name, not :prod"} (kube-context/resolve :prod))))
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "error: open /nope: no such file"})]
    (is (= {:refusal "could not read the kubeconfig to find the kube context prod"}
           (kube-context/resolve "prod")))))

(deftest a-throwing-kube-context-function-stops-the-run-before-any-read
  (let [spec (assoc (:subject subject-kinds) :kube-context (fn [_] (throw (ex-info "boom" {}))))
        calls (atom [])]
    (with-redefs [shell/execute (fn [& argv] (swap! calls conj argv) {:exit 0 :out "" :err ""})]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom"
                            (mutation/run! spec {:namespace "web"} nil))))
    (is (empty? @calls))))

;; --- the refusal matrix ------------------------------------------------------------

(defn- refusal-of
  [spec]
  (try (change/normalize spec) nil
       (catch clojure.lang.ExceptionInfo e
         (when (::change/invalid (ex-data e)) (ex-message e)))))

(def ^:private ssh-connect (fn [_] {:context {:host "mgr-1"} :cluster "mgr-1"}))

(deftest the-refusal-matrix
  (let [minimal {:select {:mode :one :label "namespace" :flag :namespace :candidates (constantly [])}
                 :confirm {:action "x" :destructive? false}
                 :effect (constantly [])
                 :no-verify "test"}
        refused? #(some? (refusal-of (merge minimal %)))]
    (doseq [[label spec expected] [["kube-context alone" {:kube-context from-flag} false]
                                   ["kube-context with :subject" {:kube-context from-flag :subject {:type "namespaces"}} false]
                                   ["kube-context with :pin" {:kube-context from-flag :pin (fn [_] nil)} false]
                                   ["kube-context with :subjects" {:kube-context from-flag :subjects (fn [_] [])} false]
                                   ["a nil kube-context is absent" {:kube-context nil :subject {:type "namespaces"}} false]
                                   ["connect alone" {:connect ssh-connect} false]
                                   ["connect with kube-context" {:connect ssh-connect :kube-context from-flag} true]
                                   ["connect with kube-context and a subject" {:connect ssh-connect :kube-context from-flag :subject {:type "namespaces"}} true]
                                   ["connect with :subject" {:connect ssh-connect :subject {:type "namespaces"}} true]
                                   ["connect with :pin" {:connect ssh-connect :pin (fn [_] nil)} true]
                                   ["connect with :subjects" {:connect ssh-connect :subjects (fn [_] [])} true]
                                   ["a kube-context that is a name" {:kube-context "prod"} true]
                                   ["a kube-context that is a map" {:kube-context {:context "prod"}} true]]]
      (testing label
        (is (= expected (refused? spec)) (refusal-of (merge minimal spec))))))
  (testing "the refusal says why"
    (is (= "a spec takes :connect, for what is not a kube cluster, or :kube-context, for the kube context its kubectl calls use - not both"
           (refusal-of {:connect ssh-connect :kube-context from-flag})))
    (is (= ":kube-context is (fn [opts] -> a kube context name or nil), not \"prod\""
           (refusal-of {:kube-context "prod"})))))

(deftest compose-keeps-one-kube-context
  (is (= from-flag (:kube-context (change/compose {:kube-context from-flag} {:kube-context from-flag}))))
  (is (thrown? clojure.lang.ExceptionInfo
               (change/compose {:kube-context from-flag} {:kube-context (fn [_] "prod")})))
  (is (thrown? clojure.lang.ExceptionInfo
               (change/normalize (change/compose {:kube-context from-flag} {:connect ssh-connect})))))

(deftest a-step-naming-another-context-than-its-run-is-refused
  (testing "a playbook pinned to the current context with a step naming prod"
    (let [spec (:subject subject-kinds)
          book (assoc (as-playbook spec) :kube-context nil)
          run (recorded-run #(playbook/run! book {:context "prod" :namespace "web"} nil))]
      (is (false? (:result run)))
      (is (empty? (:effects run)))
      (is (str/includes? (:out run) "names the kube context \"prod\", but the run it is part of is pinned to \"laptop\""))))
  (testing "a mismatch is pure"
    (is (nil? (kube-context/mismatch nil {:context "laptop"})))
    (is (nil? (kube-context/mismatch "laptop" {:context "laptop"})))
    (is (some? (kube-context/mismatch "prod" {:context "laptop"})))))

(deftest a-derive-moving-the-change-to-another-cluster-is-refused
  (let [spec (-> (:derive subject-kinds)
                 (assoc :derive (fn [_] {:context "laptop" :cluster "kind-laptop"})))
        run (recorded-run #(mutation/run! spec {:context "prod" :namespace "web"} nil))]
    (is (false? (:result run)))
    (is (empty? (:effects run)))
    (is (empty? (:confirmations run)))
    (is (str/includes? (:out run) "refusing: its :derive returned :context \"laptop\", but the change runs on :context \"prod\"")))
  (testing "a :derive repeating the connection is accepted"
    (is (nil? (mutation/moved-connection {:context "prod" :cluster "c"} {:context "prod" :cluster "c" :x 1})))
    (is (nil? (mutation/moved-connection {:context "prod" :cluster "c"} {:x 1})))))

(deftest a-connect-spec-is-never-pinned-to-a-kube-context
  (let [spec {:connect (fn [_] {:context {:host "mgr-1"} :cluster "mgr-1"})
              :select {:mode :one :label "OSD" :flag :osd :candidates (fn [_] ["osd.1"])}
              :confirm {:action "bring an OSD back in" :destructive? false}
              :effect (fn [{:keys [context target]}] (effect/plan (effect/ssh context "ceph" "osd" "in" target)))
              :no-verify "test"}
        run (recorded-run #(mutation/run! spec {:osd "osd.1"} {:cluster "kind-laptop"}))]
    (is (true? (:result run)) (:out run))
    (is (empty? (config-reads (:calls run))) "no kubeconfig read")
    (is (= "mgr-1" (:target (first (:confirmations run)))))))

;; --- the harness: prerequisites and announcement data -------------------------------

(deftest the-prerequisites-ask-about-the-pinned-context
  (let [pinned {:context "prod" :cluster "kube.prod.example" :announce-as "kube.prod.example"}
        calls (atom [])]
    (with-redefs [shell/execute (fn [& argv] (swap! calls conj (vec argv)) {:exit 0 :out "" :err ""})
                  shell/exit-code-of (fn [& argv] (swap! calls conj (vec argv)) 0)]
      (let [items (binding [kube-context/*pinned* pinned]
                    (doall (#'runbook/prerequisite-items {:kubectl-can-get ["pods"]
                                                          :kubectl-can-exec ["pods"]
                                                          :cluster-capabilities []}
                                                         {})))
            results (mapv #((second %)) items)]
        (is (every? :ok? results) (pr-str results))
        (is (some #(= "Kubernetes cluster: kube.prod.example" (:label %)) results))))
    (let [asked (filter #(some #{"auth" "api-resources"} %) @calls)]
      (is (seq asked))
      (is (empty? (stray-contexts asked "prod"))))
    (is (empty? (config-reads @calls)) "the pinned cluster is not read again")))

(deftest the-announcement-data-names-the-pinned-cluster
  (with-redefs [shell/stdout-of (fn [& argv] (if (= ["kubectl"] (take 1 argv)) (throw (ex-info "no" {})) "x"))]
    (binding [kube-context/*pinned* {:context "prod" :cluster "kube.prod.example"}]
      (is (= "kube.prod.example" (:cluster (#'runbook/announcement-data)))))))

(deftest the-books-hand-their-kube-context-to-the-harness
  (binding [runbook/*loading* true]
    (let [spec (base-spec :subject {:type "namespaces"})]
      (is (= from-flag (:kube-context (mutation/book spec))))
      (is (nil? ((:kube-context (mutation/book (dissoc spec :kube-context))) {:context "prod"}))
          "a spec without one pins the current context")
      (is (not (contains? (mutation/book (-> spec (dissoc :kube-context :subject :reads :refusal)
                                             (assoc :connect ssh-connect)))
                          :kube-context))
          "a :connect spec pins no kube context")
      (is (= from-flag (:kube-context (playbook/book (as-playbook spec))))))))
