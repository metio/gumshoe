;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.helm-rollback-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.helm-rollback :as rollback]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.select :as select]
            [gumshoe.shell :as shell]))

(defn- rev [version status & {:keys [release namespace] :or {release "shop" namespace "web"}}]
  {:namespace namespace :release release :status status :version version})

(def ^:private releases
  [(rev 3 "superseded") (rev 5 "deployed") (rev 4 "failed") (rev 2 "superseded") (rev 1 "superseded")
   (rev 9 "deployed" :release "other") (rev 7 "deployed" :namespace "elsewhere")])

(def ^:private pick {:namespace "web" :release "shop"})

(deftest picks
  (is (= pick (rollback/parse-pick "web/shop")))
  (is (nil? (rollback/parse-pick "shop"))))

(deftest history-is-one-release-newest-first
  (is (= [5 4 3 2 1] (map :version (rollback/history releases pick))))
  (is (empty? (rollback/history releases {:namespace "web" :release "gone"}))))

(deftest the-target-skips-revisions-that-never-deployed
  (let [history (rollback/history releases pick)]
    (is (= (rev 3 "superseded") (rollback/rollback-target history nil)) "4 failed, so 3")
    (is (= (rev 4 "failed") (rollback/rollback-target history 4)) "a named revision is taken as named")
    (is (nil? (rollback/rollback-target history 8)))))

(deftest refusals
  (let [history (rollback/history releases pick)]
    (is (nil? (rollback/rollback-refusal history nil)))
    (is (nil? (rollback/rollback-refusal history 1)))
    (is (= "the release no longer exists" (rollback/rollback-refusal [] nil)))
    (is (= "revision 5 is the current one" (rollback/rollback-refusal history 5)))
    (is (= "revision 12 is not in the release's history - helm keeps only --history-max revisions"
           (rollback/rollback-refusal history 12)))
    (is (= "no earlier revision of the release was deployed"
           (rollback/rollback-refusal [(rev 2 "deployed") (rev 1 "failed")] nil)))
    (is (= "revision 6 is pending-upgrade - another helm operation is in progress, or was interrupted and needs fixing first"
           (rollback/rollback-refusal (rollback/history (conj releases (rev 6 "pending-upgrade")) pick) nil)))))

(deftest the-confirmation
  (is (= ["roll Helm release web/shop back from revision 5 (deployed) to revision 3 (superseded)"
          "helm records the rollback as revision 6; its status says the manifests were applied, not that the pods are ready"
          "the release's source (Git, a pipeline) still holds the newer chart or values and will upgrade it again"]
         (rollback/rollback-items pick (rollback/history releases pick) nil))))

(deftest the-command-names-the-revision-and-context
  (is (= ["helm" "rollback" "shop" "3" "--namespace=web" "--kube-context=kind-gumshoe"]
         (rollback/rollback-args "kind-gumshoe" pick 3))))

(deftest completion-needs-a-newer-deployed-revision
  (is (rollback/rolled-back? [(rev 6 "deployed") (rev 5 "superseded")] 5))
  (is (not (rollback/rolled-back? [(rev 6 "failed") (rev 5 "superseded")] 5)))
  (is (not (rollback/rolled-back? [(rev 6 "pending-rollback")] 5)))
  (is (not (rollback/rolled-back? [(rev 5 "deployed")] 5)))
  (is (not (rollback/rolled-back? [] 5))))

(deftest candidates-are-releases-with-somewhere-to-go
  (is (= ["web/shop"] (rollback/picks releases)) "other and elsewhere have one revision each"))

(deftest reads-the-listing-kubectl-produces
  (let [listing (kubectl/parse-helm-listing "web\tshop\tdeployed\t2\t\t1763553600\t2026-11-19T12:00:00Z\nweb\tshop\tsuperseded\t1\t1763553600\t1763550000\t2026-11-19T11:00:00Z\n")]
    (is (= ["web/shop"] (rollback/picks listing)))
    (is (= 1 (:version (rollback/rollback-target (rollback/history listing pick) nil))))))

(deftest an-unanswered-release-listing-is-not-an-empty-one
  (let [throttled (fn [& _] {:exit 1 :out "" :err "Error from server (TooManyRequests): the server has received too many requests"})
        candidates (fn [context] (rollback/picks (kubectl/helm-releases context)))]
    (with-redefs [shell/execute throttled]
      (testing "a plain read still returns nil"
        (is (nil? (kubectl/helm-releases "ctx"))))
      (testing "the candidate listing reports the read instead of finding no release"
        (let [{:keys [unread candidates]} (select/resolve-target {:mode :one :label "Release" :candidates candidates} {} "ctx")]
          (is (nil? candidates))
          (is (re-find #"TooManyRequests" (str unread))))))
    (testing "a listing that answered with nothing is still empty under strict reads"
      (with-redefs [shell/execute (fn [& _] {:exit 0 :out "" :err ""})]
        (is (= [] (binding [kubectl/*strict-reads* true] (kubectl/helm-releases "ctx"))))))))

(deftest a-namespaced-release-listing-names-its-namespace-and-release
  (let [argv (atom nil)]
    (with-redefs [shell/execute (fn [& args] (reset! argv (vec args)) {:exit 0 :out "" :err ""})]
      (kubectl/helm-releases "ctx" "web" "shop")
      (is (some #{"--namespace=web"} @argv))
      (is (not-any? #{"--all-namespaces"} @argv))
      (is (some #{"--selector=owner=helm,name=shop"} @argv))
      (kubectl/helm-releases "ctx" "web")
      (is (some #{"--namespace=web"} @argv))
      (is (some #{"--selector=owner=helm"} @argv))
      (kubectl/helm-releases "ctx")
      (is (some #{"--all-namespaces"} @argv))
      (is (not-any? #(str/starts-with? % "--namespace") @argv)))
    (testing "an unanswered namespaced listing throws under strict reads and is nil otherwise"
      (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"})]
        (is (nil? (kubectl/helm-releases "ctx" "web" "shop")))
        (is (thrown? clojure.lang.ExceptionInfo
                     (binding [kubectl/*strict-reads* true] (kubectl/helm-releases "ctx" "web" "shop"))))))))
