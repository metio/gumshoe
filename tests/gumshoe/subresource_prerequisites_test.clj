;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.subresource-prerequisites-test
  "A permission prerequisite names a subresource as \"pods/eviction\", and
   each change book asks for the verb its effect really uses: drain evicts,
   scale writes the scale subresource, the finalize write updates
   namespaces/finalize, and a Helm rollback writes its release Secrets in the
   release's namespace."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.helm-rollback :as rollback]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.steps.nodes :as nodes]))

(defn- spec-of
  [path]
  (with-redefs [mutation/book identity] (load-file path)))

(defn- asked
  "The argvs kubectl was run with while f ran, every one answered exit."
  [exit f]
  (let [seen (atom [])]
    (with-redefs [shell/execute (fn [& argv] (swap! seen conj (vec argv)) {:exit exit :out "" :err ""})]
      (f))
    @seen))

(deftest a-subresource-goes-to-kubectl-as-a-flag
  (is (= ["pods" "eviction"] (kubectl/resource-and-subresource "pods/eviction")))
  (is (= ["deployments" nil] (kubectl/resource-and-subresource "deployments")))
  (is (= ["auth" "can-i" "create" "pods" "--subresource=eviction" "--all-namespaces" "--quiet"]
         (kubectl/can-i-args "create" "pods/eviction")))
  (is (= ["auth" "can-i" "get" "nodes" "--all-namespaces" "--quiet"]
         (kubectl/can-i-args "get" "nodes")))
  (is (= ["auth" "can-i" "update" "secrets" "--namespace=web" "--quiet"]
         (kubectl/can-i-args "update" "secrets" "web"))))

(deftest can-i-asks-about-the-subresource
  (is (= [["kubectl" "auth" "can-i" "patch" "deployments" "--subresource=scale" "--all-namespaces" "--quiet"]]
         (asked 0 #(is (true? (kubectl/can-i? "patch" "deployments/scale"))))))
  (is (= [["kubectl" "auth" "can-i" "create" "pods" "--subresource=exec" "--all-namespaces" "--quiet"]]
         (asked 0 #(is (true? (kubectl/can-exec? "pods")))))
      "exec is the exec subresource, asked the same way")
  (asked 1 #(is (false? (kubectl/can-i? "create" "pods/eviction")))))

(deftest a-namespaced-check-names-context-and-namespace
  (is (= [["kubectl" "--context=ctx" "--request-timeout=30s" "auth" "can-i" "update" "namespaces"
           "--subresource=finalize" "--namespace=web" "--quiet"]]
         (asked 0 #(is (true? (kubectl/can-i-in? "ctx" "update" "namespaces/finalize" "web")))))))

(deftest missing-rights-lists-what-was-denied-in-order
  (with-redefs [shell/execute (fn [& argv] {:exit (if (some #{"update"} argv) 1 0) :out "" :err ""})]
    (is (= [["update" "secrets"]] (kubectl/missing-rights "ctx" "web" rollback/storage-rights))))
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"})]
    (is (= rollback/storage-rights (kubectl/missing-rights "ctx" "web" rollback/storage-rights))
        "a check that did not answer counts as missing")))

(deftest the-runbook-checks-update-and-subresources
  (let [items (with-redefs [kubectl/resource-kind (constantly nil)]
                (doall (#'runbook/prerequisite-items {:kubectl-can-update ["namespaces/finalize"]
                                                      :kubectl-can-create ["pods/eviction"]}
                                                     {})))
        labels (mapv first items)]
    (is (= ["Kubernetes: can CREATE pods (eviction subresource)"
            "Kubernetes: can UPDATE namespaces (finalize subresource)"]
           labels))
    (is (= [["kubectl" "auth" "can-i" "create" "pods" "--subresource=eviction" "--all-namespaces" "--quiet"]
            ["kubectl" "auth" "can-i" "update" "namespaces" "--subresource=finalize" "--all-namespaces" "--quiet"]]
           (asked 0 #(doseq [[_ thunk] items] (is (:ok? (thunk)))))))
    (is (not-any? #(str/includes? (first %) "unknown prerequisite") items)
        ":kubectl-can-update is a built-in key")))

(deftest permission-checks-are-one-question-per-resource
  (is (= [["get" "nodes"] ["update" "namespaces/finalize"] ["create" "pods/exec"]]
         (runbook/permission-checks {:kubectl-can-get ["nodes"]
                                     :kubectl-can-update ["namespaces/finalize"]
                                     :kubectl-can-exec ["pods"]}))))

(deftest the-summary-names-update
  (is (some #{"kubectl replace (update): namespaces/finalize"}
            (prerequisites/summary {:kubectl-can-update ["namespaces/finalize"]}))))

(deftest drain-asks-for-eviction-and-daemonsets
  (let [checks (set (runbook/permission-checks (:prerequisites nodes/drain)))]
    (is (contains? checks ["create" "pods/eviction"]))
    (is (contains? checks ["get" "daemonsets"]))
    (is (not (contains? checks ["delete" "pods"])) "kubectl drain evicts, it deletes no pod")))

(deftest scale-asks-for-the-scale-subresource
  (let [checks (set (runbook/permission-checks (:prerequisites (spec-of "runbooks/kubernetes/workloads/scale.clj"))))]
    (is (contains? checks ["patch" "deployments/scale"]))
    (is (contains? checks ["patch" "statefulsets/scale"]))
    (is (not (contains? checks ["patch" "deployments"])))))

(deftest remove-finalizers-asks-to-update-finalize
  (is (contains? (set (runbook/permission-checks
                       (:prerequisites (spec-of "runbooks/kubernetes/namespaces/remove_finalizers.clj"))))
                 ["update" "namespaces/finalize"])))

(def ^:private releases
  [{:namespace "web" :release "shop" :version 5 :status "deployed"}
   {:namespace "web" :release "shop" :version 4 :status "superseded"}])

(defn- rollback-refusal
  [exit-of]
  (let [spec (spec-of "runbooks/kubernetes/helm/rollback.clj")]
    (with-redefs [kubectl/helm-releases (fn [& _] releases)
                  shell/execute (fn [& argv] {:exit (exit-of argv) :out "" :err ""})]
      ((:refusal spec) {:context "ctx" :pick {:namespace "web" :release "shop"} :to nil :before 5}))))

(deftest rollback-refuses-without-write-access-to-its-release-secrets
  (is (= "you may not create secrets or update secrets in namespace web, which helm needs to record the rollback"
         (rollback-refusal (constantly 1))))
  (is (= "you may not update secrets in namespace web, which helm needs to record the rollback"
         (rollback-refusal #(if (some #{"update"} %) 1 0))))
  (is (nil? (rollback-refusal (constantly 0))))
  (testing "the rights are asked in the release's namespace"
    (let [seen (atom [])]
      (rollback-refusal #(do (swap! seen conj %) 0))
      (is (= #{"--namespace=web"} (set (keep (fn [argv] (some #{"--namespace=web"} argv)) @seen))))
      (is (= 2 (count @seen))))))

(deftest rights-refusal-is-nil-with-every-right
  (is (nil? (rollback/rights-refusal "web" []))))
