;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.refusal-polarity-test
  "gumshoe.testing.refusal over real specs, and the negative controls that
   show it fails a gate that always refuses, one that never refuses, one
   that refuses for an unrelated reason, and one whose other read would
   make a real call."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.mutation :as mutation]
            [gumshoe.reads :as reads]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.steps.nodes :as nodes]
            [gumshoe.testing.refusal :as refusal]))

;; --- fixtures ------------------------------------------------------------------------

(def ^:private node
  {:apiVersion "v1" :kind "Node"
   :metadata {:name "node-1" :uid "u-node-1" :resourceVersion "7"}
   :spec {}})

(def ^:private deployment
  {:apiVersion "apps/v1" :kind "Deployment"
   :metadata {:name "web" :namespace "apps" :uid "u-web" :generation 3 :resourceVersion "11"}
   :spec {:replicas 2 :strategy {:type "RollingUpdate"}}
   :status {:observedGeneration 3 :replicas 2 :updatedReplicas 2 :availableReplicas 2}})

(def ^:private secret
  {:apiVersion "v1" :kind "Secret"
   :metadata {:name "web-tls" :namespace "apps" :uid "u-tls" :resourceVersion "5"}})

(def ^:private config-map
  {:apiVersion "v1" :kind "ConfigMap"
   :metadata {:name "web-settings" :namespace "apps" :uid "u-settings" :resourceVersion "9"}
   :data {:mode "live"}})

(def ^:private secret-read
  (refusal/kubectl-get {:type "secrets" :namespace "apps" :name "web-tls"}))

(def ^:private settings-read
  (refusal/kubectl-get {:type "configmaps" :namespace "apps" :name "web-settings"}))

(def ^:private secret-unread
  (str "could not read the Secret apps/web-tls (the read did not answer: "
       (:err refusal/unanswered)
       ") - acting on a wrong answer could do harm, so this refuses rather than act on a read that did not answer"))

(defn- gate
  "A casebook-style change on Deployment apps/web whose :reads and
   :refusal are the given ones."
  [spec]
  (merge {:select {:mode :namespaced :label "Deployment" :candidates (constantly ["apps/web"])}
          :confirm {:action "restart the web Deployment" :destructive? false}
          :effect (fn [{:keys [context]}]
                    (effect/plan (effect/kubectl context "--namespace=apps" "rollout" "restart" "deployment/web")))
          :no-verify "a fixture: nothing is applied"}
         spec))

(defn- read-secret
  [{:keys [context]}]
  (reads/namespaced-object context "apps" "secrets" "web-tls"))

(defn- read-settings
  [{:keys [context]}]
  (reads/namespaced-object context "apps" "configmaps" "web-settings"))

(def ^:private tls-gate
  "Refuses unless the Secret exists, with the read in :reads and the
   refusal written with gumshoe.reads."
  (gate {:reads {:tls read-secret}
         :refusal (fn [{:keys [tls]}]
                    (reads/refusal-unless-present tls {:what "the Secret apps/web-tls"}))}))

(defn- load-book
  [path]
  (binding [runbook/*loading* true]
    (with-redefs [mutation/book identity]
      (load-file path))))

;; --- real specs -------------------------------------------------------------------------

(deftest cordon-refuses-while-the-node-read-does-not-answer
  (let [{:keys [unanswered answered]}
        (refusal/is-polar nodes/cordon
                          {:target "node-1"
                           :read (refusal/kubectl-get {:type "nodes" :name "node-1"})
                           :answer node
                           :refusal (str "could not read what the change rests on - kubectl get did not answer: "
                                         (:err refusal/unanswered))})]
    (testing "both judgements ran in both runs"
      (is (= #{:refused} (set (map :verdict [(:before unanswered) (:after unanswered)]))))
      (is (= #{:proceeds} (set (map :verdict [(:before answered) (:after answered)])))))))

(deftest restart-refuses-while-the-workload-read-does-not-answer
  (refusal/is-polar (load-book "runbooks/kubernetes/workloads/restart.clj")
                    {:target "Deployment apps/web"
                     :read (refusal/kubectl-get {:type "deployments" :namespace "apps" :name "web"})
                     :answer deployment
                     :refusal #"^could not read what the change rests on - kubectl get did not answer: "}))

(deftest a-reads-gate-refuses-with-its-own-reason
  (refusal/is-polar tls-gate {:target "apps/web" :read secret-read :answer secret :refusal secret-unread}))

(deftest a-gate-with-two-reads-is-tested-one-read-at-a-time
  (let [spec (gate {:reads {:tls read-secret :settings read-settings}
                    :refusal (fn [{:keys [tls settings]}]
                               (or (reads/refusal-unless-present tls {:what "the Secret apps/web-tls"})
                                   (reads/unread-refusal settings {:what "the ConfigMap apps/web-settings"})))})]
    (testing "the Secret, with the ConfigMap answering"
      (refusal/is-polar spec {:target "apps/web" :read secret-read :answer secret
                              :cluster [[settings-read config-map]]
                              :refusal secret-unread}))
    (testing "the ConfigMap, with the Secret answering, and an absent ConfigMap is an answer"
      (refusal/is-polar spec {:target "apps/web" :read settings-read :answer refusal/absent
                              :cluster [[secret-read secret]]
                              :refusal #"^could not read the ConfigMap apps/web-settings "}))))

(deftest a-precondition-that-returns-false-refuses-as-unmet
  (refusal/is-polar (gate {:reads {:tls read-secret}
                           :precondition (fn [{:keys [tls]}] (reads/present? tls))})
                    {:target "apps/web" :read secret-read :answer secret :refusal refusal/unmet}))

(deftest the-before-judgement-alone-can-be-asked-for
  (let [{:keys [unanswered answered]}
        (refusal/is-polar tls-gate {:target "apps/web" :read secret-read :answer secret
                                    :refusal secret-unread :phases [:before]})]
    (is (= [:before :calls] (keys unanswered)))
    (is (= {:verdict :proceeds} (:before answered)))))

;; --- negative controls ------------------------------------------------------------------

(defn- failures
  [spec opts]
  (let [{:keys [holds? failures]} (refusal/polarity spec opts)]
    (is (false? holds?))
    failures))

(deftest a-gate-that-never-refuses-fails
  (testing "a refusal that proceeds on a read that did not answer: (absent? r) is false for :unread"
    (is (= ["while kubectl get secrets web-tls --namespace=apps does not answer, the gate must refuse with \"the Secret is missing\", but before the confirmation it proceeded"
            "while kubectl get secrets web-tls --namespace=apps does not answer, the gate must refuse with \"the Secret is missing\", but after the confirmation it proceeded"]
           (failures (gate {:reads {:tls read-secret}
                            :refusal (fn [{:keys [tls]}] (when (reads/absent? tls) "the Secret is missing"))})
                     {:target "apps/web" :read secret-read :answer secret :refusal "the Secret is missing"})))))

(deftest a-gate-that-always-refuses-fails
  (testing "a refusal that never sees the answer: it judges a key the change does not bind"
    (is (= ["once kubectl get secrets web-tls --namespace=apps answers, the gate must proceed, but before the confirmation it refused: \"could not read the Secret apps/web-tls - acting on a wrong answer could do harm, so this refuses rather than act on a read that did not answer\""
            "once kubectl get secrets web-tls --namespace=apps answers, the gate must proceed, but after the confirmation it refused: \"could not read the Secret apps/web-tls - acting on a wrong answer could do harm, so this refuses rather than act on a read that did not answer\""]
           (failures (gate {:reads {:tls read-secret}
                            :refusal (fn [{:keys [secret]}]
                                       (reads/refusal-unless-present secret {:what "the Secret apps/web-tls"}))})
                     {:target "apps/web" :read secret-read :answer secret
                      :refusal #"^could not read the Secret apps/web-tls"})))))

(deftest a-refusal-for-an-unrelated-reason-fails
  (testing "a gate whose first refusal fires whatever the Secret read answers"
    (let [found (failures (gate {:reads {:tls read-secret}
                                 :refusal (fn [{:keys [tls]}]
                                            (or "the change window is closed"
                                                (reads/refusal-unless-present tls {:what "the Secret apps/web-tls"})))})
                          {:target "apps/web" :read secret-read :answer secret :refusal secret-unread})]
      (is (= 4 (count found)))
      (is (str/includes? (first found) "but before the confirmation it refused with another reason: \"the change window is closed\""))
      (is (str/includes? (nth found 2) "once kubectl get secrets web-tls --namespace=apps answers, the gate must proceed, but before the confirmation it refused: \"the change window is closed\"")))))

(deftest another-read-in-the-gate-cannot-make-a-real-call
  (let [sentinel (str (fs/create-temp-dir) "/ran")]
    (testing "through gumshoe.shell: the ConfigMap read is not stubbed, and a command through shell/execute never starts"
      (let [found (failures (gate {:reads {:tls read-secret
                                           :settings read-settings
                                           :probe (fn [_] (shell/execute "touch" sentinel))}
                                   :refusal (fn [{:keys [tls]}]
                                              (reads/refusal-unless-present tls {:what "the Secret apps/web-tls"}))})
                            {:target "apps/web" :read secret-read :answer secret :refusal secret-unread})]
        (is (some #(str/starts-with? % (str "a real command was attempted and refused: kubectl --context=polarity-control --request-timeout=30s "
                                                 "get --namespace=apps configmaps web-settings --ignore-not-found --output=json"))
                  found))
        (is (some #(= (str "a real command was attempted and refused: touch " sentinel
                           " - answer it in :cluster, or it would reach a live system")
                      %)
                  found))
        (is (not (fs/exists? sentinel)))))
    (testing "around gumshoe.shell: a read through babashka.process is refused too"
      (let [found (failures (gate {:reads {:tls read-secret
                                           :probe (fn [_] (process/shell "touch" sentinel))}
                                   :refusal (fn [{:keys [tls]}]
                                              (reads/refusal-unless-present tls {:what "the Secret apps/web-tls"}))})
                            {:target "apps/web" :read secret-read :answer secret :refusal secret-unread})]
        (is (some #(= (str "a real command was attempted and refused: touch " sentinel
                           " - answer it in :cluster, or it would reach a live system")
                      %)
                  found))
        (is (not (fs/exists? sentinel)))))))

(deftest a-read-the-gate-never-makes-proves-nothing
  (is (some #{"kubectl get configmaps web-settings --namespace=apps was never made in the answered run, so this control proves nothing about it"}
            (failures tls-gate {:target "apps/web" :read settings-read :answer config-map
                                :cluster [[secret-read secret]] :refusal secret-unread}))))

(deftest a-matcher-answering-two-reads-fails
  (let [both (with-meta (fn [argv] (boolean (some #{"secrets" "configmaps"} argv))) {:description "both reads"})
        spec (gate {:reads {:tls read-secret :settings read-settings}
                    :refusal (fn [{:keys [tls settings]}]
                               (or (reads/refusal-unless-present tls {:what "the Secret apps/web-tls"})
                                   (reads/unread-refusal settings)))})]
    (is (some #(str/starts-with? % "both reads matched 2 distinct commands, so more than one read was answered: ")
              (failures spec {:target "apps/web" :read both :answer secret :refusal secret-unread})))))

(deftest is-polar-reports-a-failure-naming-what-did-not-hold
  (let [reports (atom [])]
    (binding [clojure.test/report #(swap! reports conj %)]
      (refusal/is-polar (gate {:reads {:tls read-secret}
                               :refusal (fn [{:keys [tls]}] (when (reads/absent? tls) "the Secret is missing"))})
                        {:target "apps/web" :read secret-read :answer secret :refusal "the Secret is missing"
                         :phases [:before]}))
    (is (= [:fail] (map :type @reports)))
    (is (= (str "refusal polarity does not hold:\n  while kubectl get secrets web-tls --namespace=apps does not answer, "
                "the gate must refuse with \"the Secret is missing\", but before the confirmation it proceeded")
           (:message (first @reports))))))

(deftest polarity-refuses-an-open-ended-expectation
  (is (thrown-with-msg? IllegalArgumentException #"needs :refusal"
                        (refusal/polarity tls-gate {:target "apps/web" :read secret-read :answer secret})))
  (is (thrown-with-msg? IllegalArgumentException #"needs :read"
                        (refusal/polarity tls-gate {:target "apps/web" :answer secret :refusal secret-unread})))
  (is (thrown-with-msg? IllegalArgumentException #"needs :answer"
                        (refusal/polarity tls-gate {:target "apps/web" :read secret-read :refusal secret-unread}))))

;; --- matchers ---------------------------------------------------------------------------

(deftest kubectl-get-matches-one-read-exactly
  (let [named (refusal/kubectl-get {:type "secrets" :namespace "apps" :name "web-tls"})
        listing (refusal/kubectl-get {:type "secrets" :namespace :all})
        cluster (refusal/kubectl-get {:type "nodes" :name "node-1"})]
    (is (named ["kubectl" "--context=x" "--namespace=apps" "--request-timeout=30s" "get" "secrets" "web-tls" "--output=json"]))
    (is (named ["kubectl" "get" "-n" "apps" "secrets" "web-tls" "-o" "json" "--ignore-not-found"]))
    (is (not (named ["kubectl" "--namespace=other" "get" "secrets" "web-tls"])))
    (is (not (named ["kubectl" "--namespace=apps" "get" "secrets"])))
    (is (not (named ["kubectl" "--namespace=apps" "get" "secrets" "web-tls" "other"])))
    (is (not (named ["ssh" "host" "kubectl" "get" "secrets" "web-tls"])))
    (is (listing ["kubectl" "get" "secrets" "--all-namespaces" "--output=json"]))
    (is (not (listing ["kubectl" "--namespace=apps" "get" "secrets"])))
    (is (cluster ["kubectl" "get" "nodes" "node-1"]))
    (is (not (cluster ["kubectl" "--namespace=apps" "get" "nodes" "node-1"])))
    (is (not (cluster ["kubectl" "get" "--raw" "/api/v1/nodes/node-1"])))
    (is (= "kubectl get secrets web-tls --namespace=apps" (refusal/describe named)))))

(deftest answers-take-results-strings-and-documents
  (is (= {:exit 1 :out "" :err "x"} (refusal/answer {:exit 1 :err "x"})))
  (is (= {:exit 0 :out "plain" :err ""} (refusal/answer "plain")))
  (is (= {:exit 0 :out "{\"kind\":\"Secret\"}" :err ""} (refusal/answer {:kind "Secret"}))))
