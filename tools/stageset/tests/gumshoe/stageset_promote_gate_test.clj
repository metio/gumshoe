;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.stageset-promote-gate-test
  "stageset/promote offers and accepts only a stage held at its promotion
   gate at the revision being rolled out, and a promotion that was not acted
   on names the promote annotation it left armed with the command that
   removes it."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.tools.stageset :as stageset]))

(defn- site
  "StageSet apps/site rolling out revision r2: dev promoted, prod in `phase`
   at `applied`."
  [phase & {:keys [applied aborted reason suspend generation source]
            :or {applied "r2" reason "AwaitingPromotion" generation 4 source {:name "site"}}}]
  {:metadata {:namespace "apps" :name "site" :generation 4}
   :spec (cond-> {:stages [{:name "dev" :sourceRef source :promotion {:soak {:duration "1h"}}}
                           {:name "prod" :sourceRef source :promotion {:requireManualPromotion true}}]}
           suspend (assoc :suspend true))
   :status {:observedGeneration generation
            :conditions [{:type "Ready" :status "False" :reason reason}]
            :lastAttemptedRevisions {:apps/site "r2"}
            :stages [{:name "dev" :appliedRevision "r2" :promotionState {:phase "Promoted"}}
                     {:name "prod" :appliedRevision applied
                      :promotionState (cond-> {:phase phase} aborted (assoc :abortedRevision aborted))}]}})

(deftest a-stage-held-at-its-gate-at-the-current-revision-may-be-promoted
  (doseq [phase ["AwaitingManual" "Soaking" "Analyzing" "Blocked"]]
    (testing phase
      (is (nil? (stageset/hold (site phase) "prod")))
      (is (nil? (stageset/promotion-refusal "apps/site" (site phase) "prod" "prod")))))
  (is (= ["prod"] (stageset/held-stages (site "AwaitingManual")))))

(deftest a-stage-promoted-at-the-current-revision-is-refused
  (is (= "StageSet apps/site: stage prod is not held at its promotion gate (phase Promoted)"
         (stageset/promotion-refusal "apps/site" (site "Promoted") "prod" "prod")))
  (is (= [] (stageset/held-stages (site "Promoted"))) "the picker offers nothing")
  (is (str/includes? (stageset/promotion-refusal "apps/site" (site "Promoted") nil nil)
                     "no stage of StageSet apps/site is held")))

(deftest a-stage-the-rollout-has-not-reached-is-refused
  (testing "its status still describes the previous revision"
    (is (= "stage prod is held at a revision the rollout is no longer on (applied r1)"
           (stageset/hold (site "AwaitingManual" :applied "r1") "prod"))))
  (testing "it has no status yet"
    (is (str/includes? (stageset/hold (update-in (site "AwaitingManual") [:status :stages] pop) "prod")
                       "phase none"))))

(deftest a-stage-rolled-back-from-the-current-revision-may-be-promoted
  (is (nil? (stageset/hold (site "Blocked" :applied "r1" :aborted "r2") "prod"))
      "the aborted revision is the one the rollout is on")
  (is (some? (stageset/hold (site "Blocked" :applied "r0" :aborted "r1") "prod"))))

(deftest a-producer-source-is-judged-by-every-pinned-revision
  (let [producer {:kind "JsonnetSnippet" :apiVersion "jaas.metio.wtf/v1" :name "site"}]
    (is (nil? (stageset/hold (-> (site "AwaitingManual" :source producer)
                                 (assoc-in [:status :lastAttemptedRevisions] {:apps/site-abc12 "r2"}))
                             "prod")))
    (is (some? (stageset/hold (-> (site "AwaitingManual" :source producer)
                                  (assoc-in [:status :lastAttemptedRevisions] {:apps/site-abc12 "r3"}))
                              "prod")))))

(deftest a-stageset-the-controller-would-not-act-on-is-refused
  (is (str/includes? (stageset/hold (site "AwaitingManual" :suspend true) "prod") "suspended"))
  (is (str/includes? (stageset/hold (site "AwaitingManual" :generation 3) "prod") "not observed"))
  (is (str/includes? (stageset/hold (site "AwaitingManual" :reason "BudgetExhausted") "prod") "no promotion hold"))
  (is (str/includes? (stageset/hold (update-in (site "AwaitingManual") [:spec :stages 1] dissoc :promotion) "prod")
                     "no promotion gate")))

(deftest a-named-stage-that-is-not-held-says-why
  (is (= "StageSet apps/site: stage dev is not held at its promotion gate (phase Promoted)"
         (stageset/promotion-refusal "apps/site" (site "AwaitingManual") nil "dev")))
  (is (= "StageSet apps/site declares no stage qa, only dev, prod"
         (stageset/promotion-refusal "apps/site" (site "AwaitingManual") nil "qa"))))

(defn- armed
  [value]
  (assoc-in (site "AwaitingManual") [:metadata :annotations :stages.metio.wtf/promote] value))

(deftest the-disarm-command-removes-the-promote-annotation
  (is (= "kubectl --context=ctx --namespace=apps annotate stagesets.stages.metio.wtf site stages.metio.wtf/promote-"
         (stageset/disarm-command "ctx" "apps" "site"))))

(deftest a-failed-promotion-names-the-armed-annotation-and-how-to-remove-it
  (let [command "kubectl --context=ctx --namespace=apps annotate stagesets.stages.metio.wtf site stages.metio.wtf/promote-"]
    (testing "the token is still addressed to the stage"
      (let [text (stageset/promotion-recovery "ctx" "apps/site" (armed "prod@t1") "prod")]
        (is (str/includes? text "STILL ARMED for stage prod"))
        (is (str/includes? text command))))
    (testing "the StageSet could not be read"
      (let [text (stageset/promotion-recovery "ctx" "apps/site" nil "prod")]
        (is (str/includes? text "unknown"))
        (is (str/includes? text command))))
    (testing "the annotation names another stage"
      (is (str/includes? (stageset/promotion-recovery "ctx" "apps/site" (armed "dev@t1") "prod") "names stage dev")))
    (testing "nothing is armed"
      (is (str/includes? (stageset/promotion-recovery "ctx" "apps/site" (site "AwaitingManual") "prod")
                         "nothing is left armed")))))
