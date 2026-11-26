;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.stageset-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.effect :as effect]
            [gumshoe.mutation :as mutation]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.investigation :as investigation]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.subject :as subject]
            [gumshoe.tools.stageset :as stageset]))

(defn- summaries [findings] (set (map :summary findings)))

(deftest stageset-severity-by-reason-test
  (testing "Ready=False maps its reason to a severity; hard failures are critical, gates warn, human-waits inform"
    (let [evidence {stageset/stageset-type
                    {:items [{:metadata {:namespace "apps" :name "web"}
                              :status {:conditions [{:type "Ready" :status "False" :reason "RollbackStoreFailed"
                                                     :message "snapshot missing"}]}}
                             {:metadata {:namespace "apps" :name "api"}
                              :status {:conditions [{:type "Ready" :status "False" :reason "AwaitingPromotion"}]}}
                             {:metadata {:namespace "apps" :name "worker"}
                              :status {:conditions [{:type "Ready" :status "False" :reason "BudgetExhausted"}]}}
                             {:metadata {:namespace "apps" :name "healthy"}
                              :status {:conditions [{:type "Ready" :status "True"}]}}]}}
          findings (stageset/detect-stageset-problems evidence)
          by-summary (into {} (map (juxt :summary :severity) findings))]
      (is (= #{"StageSet is not Ready (RollbackStoreFailed)"
               "StageSet is not Ready (AwaitingPromotion)"
               "StageSet is not Ready (BudgetExhausted)"}
             (summaries findings)) "a healthy StageSet produces nothing")
      (is (= :critical (by-summary "StageSet is not Ready (RollbackStoreFailed)")))
      (is (= :info (by-summary "StageSet is not Ready (AwaitingPromotion)")))
      (is (= :warning (by-summary "StageSet is not Ready (BudgetExhausted)"))))))

(deftest held-update-is-info-test
  (let [findings (stageset/detect-held-updates
                  {stageset/stageset-type
                   {:items [{:metadata {:namespace "apps" :name "web"}
                             :status {:pendingUpdate
                                      {:revisions {"first" "sha256:abc"}
                                       :nextWindowOpens "2026-07-06T02:00:00Z"}}}]}})]
    (is (= #{"a new revision is held by the update window"} (summaries findings)))
    (is (= "next window opens: 2026-07-06T02:00:00Z" (:hint (first findings)))
        "nextWindowOpens is a field of pendingUpdate, so the wait's end is named")))

(deftest package-registers-delivery-scope-and-capability-test
  (is (seq (registry/for-scope :delivery)) "the package fills the :delivery scan scope")
  (is (contains? (set (capabilities/registered)) :stageset)))

(deftest finding-names-its-subject-test
  (testing "a finding points at the StageSet, so a scan drills straight into it"
    (is (= (subject/subject "StageSet" "apps" "web")
           (:subject (first (stageset/detect-stageset-problems
                             {stageset/stageset-type
                              {:items [{:metadata {:namespace "apps" :name "web"}
                                        :status {:conditions [{:type "Ready" :status "False"
                                                               :reason "StageFailed"}]}}]}})))))))

(deftest stageset-edges-follow-every-stage-source-test
  (testing "a bare sourceRef is an ExternalArtifact; a producer kind lands on the producer itself"
    (is (= [{:relation "stage 'first' builds from"
             :subject (subject/subject "ExternalArtifact" "apps" "base")}
            {:relation "stage 'second' builds from"
             :subject (subject/subject "JsonnetSnippet" "apps" "dashboards")}]
           (stageset/stageset-edges
            {:metadata {:namespace "apps" :name "web"}
             :spec {:stages [{:name "first" :sourceRef {:name "base"}}
                             {:name "second" :sourceRef {:apiVersion "jaas.metio.wtf/v1"
                                                         :kind "JsonnetSnippet"
                                                         :name "dashboards"}}]}})))))

(deftest stageset-edges-honour-an-explicit-namespace-test
  (is (= [{:relation "stage 'first' builds from"
           :subject (subject/subject "ExternalArtifact" "platform" "shared")}]
         (stageset/stageset-edges
          {:metadata {:namespace "apps" :name "web"}
           :spec {:stages [{:name "first" :sourceRef {:name "shared" :namespace "platform"}}]}}))))

(deftest stageset-edges-include-the-migration-ladder-test
  (is (= [{:relation "migrations from"
           :subject (subject/subject "ExternalArtifact" "apps" "ladder")}]
         (stageset/stageset-edges
          {:metadata {:namespace "apps" :name "web"}
           :spec {:migrationsSourceRef {:sourceRef {:name "ladder"}}}}))))

(deftest stageset-facts-test
  (let [facts (into {} (stageset/stageset-facts
                        {:metadata {:namespace "apps" :name "web"}
                         :status {:version "2.1.0"
                                  :stages [{:name "first" :phase "Succeeded"}
                                           {:name "second" :phase "Progressing"}]}}))]
    (is (= "first: Succeeded, second: Progressing" (get facts "stages")))
    (is (= "2.1.0" (get facts "version")))
    (is (nil? (get facts "held until")))))

(deftest stageset-is-a-drill-down-subject-test
  (is (= stageset/stageset-type (subject/kind->type "StageSet")))
  (is (= stageset/stageinventory-type (subject/kind->type "StageInventory"))))

(deftest inventory-edges-ask-the-cluster-by-label-test
  (testing "shards are found by the stage-set label, scoped to the namespace, and each names its stage"
    (let [asked (atom nil)]
      (with-redefs [kubectl/get-selected (fn [context type selector]
                                           (reset! asked {:context context :type type :selector selector})
                                           {:items [{:metadata {:namespace "apps" :name "web-second-00-ab12cd34ef"
                                                                :labels {(keyword stageset/stage-label) "second"}}}
                                                    {:metadata {:namespace "apps" :name "web-first-00-9f8e7d6c5b"
                                                                :labels {(keyword stageset/stage-label) "first"}}}
                                                    {:metadata {:namespace "other" :name "web-first-00-ffffffffff"
                                                                :labels {(keyword stageset/stage-label) "first"}}}]})]
        (let [edges (stageset/inventory-edges
                     "ctx" (subject/subject "StageSet" "apps" "web") {})]
          (is (= {:context "ctx"
                  :type stageset/stageinventory-type
                  :selector "stages.metio.wtf/stage-set=web"}
                 @asked))
          (is (= [{:relation "stage 'first' applied via"
                   :subject (subject/subject "StageInventory" "apps" "web-first-00-9f8e7d6c5b")}
                  {:relation "stage 'second' applied via"
                   :subject (subject/subject "StageInventory" "apps" "web-second-00-ab12cd34ef")}]
                 edges)
              "sorted by name, and a same-named StageSet in another namespace is not picked up"))))))

(deftest inventory-edges-without-a-stage-label-test
  (testing "a shard missing its stage label still walks, just without the stage in the relation"
    (with-redefs [kubectl/get-selected (fn [_ _ _]
                                         {:items [{:metadata {:namespace "apps" :name "web-first-00-abc"}}]})]
      (is (= ["applied via"]
             (map :relation (stageset/inventory-edges
                             "ctx" (subject/subject "StageSet" "apps" "web") {})))))))

(deftest package-registers-the-inventory-edges-test
  (with-redefs [kubectl/get-selected (fn [_ _ _]
                                       {:items [{:metadata {:namespace "apps" :name "web-first-00-abc"}}]})]
    (is (= ["applied via"]
           (map :relation (investigation/plugin-fetched-edges
                           "ctx" (subject/subject "StageSet" "apps" "web") {})))
        "provide! wired inventory-edges into the fetched-edges seam")))

(defn- promoted-stageset
  [annotation handled]
  {:metadata {:namespace "apps" :name "web"
              :annotations (when annotation {:stages.metio.wtf/promote annotation})}
   :status {:stages [{:name "canary" :lastHandledPromotion handled}
                     {:name "prod"}]}})

(deftest promotion-handled-test
  (testing "the stage handled the token the promote annotation addresses to it"
    (is (true? (stageset/promotion-handled? (promoted-stageset "canary@tok2" "tok2") "canary"))))
  (testing "the controller has not yet acted on the newest token"
    (is (false? (stageset/promotion-handled? (promoted-stageset "canary@tok2" "tok1") "canary"))))
  (testing "the annotation addresses another stage, or is missing or malformed"
    (is (false? (stageset/promotion-handled? (promoted-stageset "prod@tok2" "tok2") "canary")))
    (is (false? (stageset/promotion-handled? (promoted-stageset nil "tok2") "canary")))
    (is (false? (stageset/promotion-handled? (promoted-stageset "canary@" "") "canary"))))
  (testing "a StageSet that could not be read is not promoted"
    (is (false? (stageset/promotion-handled? nil "canary")))))

(defn- with-stages
  [spec-stages status-stages]
  {:metadata {:namespace "apps" :name "site"}
   :spec {:stages (mapv #(hash-map :name %) spec-stages)}
   :status (when status-stages {:stages (mapv #(hash-map :name %) status-stages)})})

(deftest the-declared-stages-come-from-the-status-else-the-spec
  (is (= ["dev" "prod"] (stageset/declared-stages (with-stages ["dev" "prod"] nil))))
  (is (= ["dev" "qa" "prod"] (stageset/declared-stages (with-stages ["dev" "prod"] ["dev" "qa" "prod"]))))
  (is (= [] (stageset/declared-stages (with-stages [] nil))))
  (is (= [] (stageset/declared-stages nil))))

(deftest a-promotion-is-refused-unless-the-stageset-declares-the-stage
  (let [site (-> (with-stages ["dev" "prod"] nil)
                 (assoc-in [:spec :stages 1 :promotion] {:requireManualPromotion true})
                 (assoc-in [:spec :stages 1 :sourceRef] {:name "site"})
                 (assoc :status {:conditions [{:type "Ready" :status "False" :reason "AwaitingPromotion"}]
                                 :lastAttemptedRevisions {:apps/site "r2"}
                                 :stages [{:name "dev"} {:name "prod" :appliedRevision "r2" :promotionState {:phase "AwaitingManual"}}]}))]
    (is (nil? (stageset/promotion-refusal "apps/site" site "prod" "prod")))
    (is (nil? (stageset/promotion-refusal "apps/site" site "prod" nil)) "a stage picked interactively")
    (is (= "StageSet apps/site no longer exists" (stageset/promotion-refusal "apps/site" nil "prod" "prod")))
    (is (= "StageSet apps/site declares no stages"
           (stageset/promotion-refusal "apps/site" (with-stages [] nil) nil "prod")))
    (is (= "StageSet apps/site declares no stage qa, only dev, prod"
           (stageset/promotion-refusal "apps/site" site nil "qa")))
    (is (= "no stage of StageSet apps/site is held at its promotion gate" (stageset/promotion-refusal "apps/site" site nil nil)))
    (is (= "StageSet apps/site no longer declares stage qa"
           (stageset/promotion-refusal "apps/site" site "qa" nil)))))

(defn- reconciled
  [& {:keys [handled override pending suspend]}]
  {:metadata {:namespace "apps" :name "site"}
   :spec (cond-> {} suspend (assoc :suspend true))
   :status (cond-> {:conditions [{:type "Ready" :status "True"}]}
             handled (assoc :lastHandledReconcileAt handled)
             override (assoc :lastHandledUpdateOverride override)
             pending (assoc :pendingUpdate pending))})

(deftest a-reconcile-is-handled-once-the-controller-wrote-the-token
  (is (true? (stageset/reconcile-handled? (reconciled :handled "T") "T" false)))
  (testing "a StageSet Ready before the request has not handled it"
    (is (false? (stageset/reconcile-handled? (reconciled :handled "earlier") "T" false)))
    (is (false? (stageset/reconcile-handled? (reconciled) "T" false))))
  (testing "a StageSet that could not be read has not handled it"
    (is (false? (stageset/reconcile-handled? nil "T" false)))))

(deftest an-update-now-is-handled-once-the-override-was-honoured-and-nothing-is-held
  (is (true? (stageset/reconcile-handled? (reconciled :handled "T" :override "T") "T" true)))
  (is (false? (stageset/reconcile-handled? (reconciled :handled "T") "T" true))
      "the reconcile was handled, the override not yet")
  (is (false? (stageset/reconcile-handled? (reconciled :handled "T" :override "earlier") "T" true)))
  (is (false? (stageset/reconcile-handled? (reconciled :handled "T" :override "T" :pending {:reason "Window"}) "T" true))
      "an update still held has not rolled"))

(deftest a-reconcile-stamps-the-token-on-every-annotation-it-sets
  (is (= ["reconcile.fluxcd.io/requestedAt=T"] (stageset/reconcile-annotations "T" false)))
  (is (= ["reconcile.fluxcd.io/requestedAt=T" "stages.metio.wtf/update-now=T"]
         (stageset/reconcile-annotations "T" true))))

(deftest a-reconcile-is-refused-for-a-suspended-or-missing-stageset
  (is (nil? (stageset/reconcile-refusal "apps/site" (reconciled))))
  (is (= "StageSet apps/site is suspended; it will not act on a reconcile request"
         (stageset/reconcile-refusal "apps/site" (reconciled :suspend true))))
  (is (= "StageSet apps/site no longer exists" (stageset/reconcile-refusal "apps/site" nil))))

(defn- reconcile-spec []
  (binding [runbook/*loading* true]
    (with-redefs [mutation/book identity]
      (load-file "tools/stageset/runbooks/stageset/reconcile.clj"))))

(deftest the-reconcile-book-waits-for-its-own-token
  (let [spec (reconcile-spec)
        ctx {:context "ctx" :target "apps/site" :ref {:namespace "apps" :name "site"} :token "T"
             :opts {:update-now true}}
        [check] ((:verify spec) ctx)
        serving (fn [object] (fn [& _] {:exit 0 :out (json/generate-string object) :err ""}))]
    (is (= [[:kubectl "ctx" "--namespace=apps" "annotate" stageset/stageset-type "site"
             "reconcile.fluxcd.io/requestedAt=T" "stages.metio.wtf/update-now=T" "--overwrite"]]
           (effect/collect ((:effect spec) ctx))))
    (with-redefs [shell/execute (serving (reconciled :handled "earlier" :override "earlier"))]
      (is (false? ((:check check))) "Ready=True from before the request does not pass"))
    (with-redefs [shell/execute (serving (reconciled :handled "T" :override "T"))]
      (is (true? ((:check check)))))))
