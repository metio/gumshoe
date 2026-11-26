;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.stageset-promote-gate-test
  "stageset/promote offers and accepts only a stage held at its promotion
   gate at the revision being rolled out, and a promotion that was not acted
   on names the promote annotation it left armed with the command that
   removes it. The promotion is bound to the revision the stage is held at,
   and a token the controller ignored ends the post-check at once."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.runbook :as runbook]
            [gumshoe.tools.stageset :as stageset]
            [gumshoe.verify :as verify]))

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
      (let [text (stageset/promotion-recovery "ctx" "apps/site" (armed "prod@t1") "prod" "t1")]
        (is (str/includes? text "STILL ARMED for stage prod"))
        (is (str/includes? text command))))
    (testing "the StageSet could not be read"
      (let [text (stageset/promotion-recovery "ctx" "apps/site" nil "prod" "t1")]
        (is (str/includes? text "unknown"))
        (is (str/includes? text command))))
    (testing "the annotation names another stage"
      (is (str/includes? (stageset/promotion-recovery "ctx" "apps/site" (armed "dev@t1") "prod" "t2") "names stage dev")))
    (testing "nothing is armed"
      (is (str/includes? (stageset/promotion-recovery "ctx" "apps/site" (site "AwaitingManual") "prod" "t1")
                         "nothing is left armed")))))

;; --- the revision binding and ignored tokens ---------------------------------------

(def ^:private book-path "tools/stageset/runbooks/stageset/promote.clj")

(defn- book-spec
  "The spec the promote book hands mutation/book."
  []
  (binding [runbook/*loading* true]
    (with-redefs [mutation/book identity]
      (load-file book-path))))

(defn- with-status
  "StageSet apps/site held at r2, its prod stage status merged with `extra`."
  [extra]
  (update-in (site "AwaitingManual") [:status :stages 1] merge extra))

(defn- ignored
  [token reason]
  (with-status {:ignoredPromotion {:token token :reason reason :revision "r2"
                                   :message (format "promote token %s ignored: %s" token reason)
                                   :time "2026-11-26T10:00:00Z"}}))

(defn- post-check
  "The book's post-check for stage prod at r2 and token T1, reading `object`."
  [object]
  (with-redefs [kubectl/get-namespaced-resource (fn [_ namespace type name]
                                                  (when (= ["apps" stageset/stageset-type "site"] [namespace type name])
                                                    object))]
    (let [[check] ((:verify (book-spec)) {:context "ctx" :target "apps/site" :stage "prod"
                                          :revisions {"prod" "r2"} :token "T1"})]
      {:held? (verify/attempt (:check check))
       :abort ((:abort check))
       :description (:description check)
       :check check})))

(defn- run-post-check
  "The post-check run as a change runs it, with its declared 300 s timeout:
   its verdict and how long it took."
  [object]
  (with-redefs [kubectl/get-namespaced-resource (fn [& _] object)]
    (let [[check] ((:verify (book-spec)) {:context "ctx" :target "apps/site" :stage "prod"
                                          :revisions {"prod" "r2"} :token "T1"})
          start (System/currentTimeMillis)
          err (java.io.StringWriter.)
          verdict (binding [*err* err] (verify/eventually check))]
      {:verdict verdict :output (str err) :millis (- (System/currentTimeMillis) start)})))

(deftest the-held-revision-is-the-aborted-one-after-a-rollback-else-the-applied-one
  (is (= "r2" (stageset/held-revision {:appliedRevision "r2" :promotionState {:phase "Soaking"}})))
  (is (= "r2" (stageset/held-revision {:appliedRevision "r1" :promotionState {:phase "Blocked" :abortedRevision "r2"}})))
  (is (= "r1" (stageset/held-revision {:appliedRevision "r1" :promotionState {:phase "Blocked" :abortedRevision ""}})))
  (is (nil? (stageset/held-revision {}))))

(deftest a-stage-whose-held-revision-is-not-pinned-is-refused-even-when-its-applied-one-is
  (is (some? (stageset/hold (site "Blocked" :applied "r2" :aborted "r1") "prod"))
      "the controller judges the aborted revision r1, which the rollout of r2 does not pin"))

(deftest the-held-revisions-name-each-held-stage
  (is (= {"prod" "r2"} (stageset/held-revisions (site "AwaitingManual"))))
  (is (= {"prod" "r2"} (stageset/held-revisions (site "Blocked" :applied "r1" :aborted "r2"))))
  (is (= {} (stageset/held-revisions (site "Promoted")))))

(deftest the-patch-stamps-the-token-and-its-revision-together
  (let [plan ((:effect (book-spec)) {:context "ctx" :ref {:namespace "apps" :name "site"}
                                     :stage "prod" :revisions {"prod" "sha256:abc"} :token "T1"})]
    (is (= 1 (count plan)) "one kubectl call, so one patch carries both annotations")
    (is (= [:kubectl "ctx" "--namespace=apps" "annotate" stageset/stageset-type "site"
            "stages.metio.wtf/promote=prod@T1"
            "stages.metio.wtf/promote-revision=sha256:abc"
            "--overwrite"]
           (first plan)))
    (testing "the dry-run plan shows both annotations"
      (let [shown (effect/describe (first plan))]
        (is (str/includes? shown "stages.metio.wtf/promote=prod@T1"))
        (is (str/includes? shown "stages.metio.wtf/promote-revision=sha256:abc"))))))

(deftest the-revision-is-written-exactly-as-the-status-holds-it
  (is (= ["stages.metio.wtf/promote=prod@2026-11-26T10:00:00.123Z"
          "stages.metio.wtf/promote-revision=main@sha1:0123abcd"]
         (stageset/promote-annotations "prod" "main@sha1:0123abcd" "2026-11-26T10:00:00.123Z"))))

(deftest the-confirmation-and-announcement-name-the-revision
  (let [spec (book-spec)
        ctx {:target "apps/site" :stage "prod" :revisions {"prod" "r2"}}]
    (is (= ["stage prod of StageSet apps/site at revision r2"] ((:items spec) ctx)))
    (is (= "Promote stage prod of StageSet apps/site at revision r2" ((:announce spec) ctx)))))

(deftest a-revision-that-moved-after-the-confirmation-is-refused
  (let [refusal (:refusal (book-spec))
        ctx {:target "apps/site" :stage "prod" :opts {:stage "prod"} :revisions {"prod" "r1"}}]
    (is (nil? (refusal (assoc ctx :object (-> (site "AwaitingManual" :applied "r1")
                                              (assoc-in [:status :lastAttemptedRevisions] {:apps/site "r1"}))
                              :revisions {"prod" "r1"})))
        "held at the confirmed revision")
    (let [moved (refusal (assoc ctx :object (site "AwaitingManual")))]
      (is (str/includes? moved "held at revision r2 now, not at r1 as confirmed")))))

(deftest a-handled-token-verifies-the-promotion
  (let [{:keys [held? abort description]} (post-check (with-status {:lastHandledPromotion "T1"}))]
    (is (true? held?))
    (is (nil? abort))
    (is (str/includes? description "revision r2"))))

(deftest a-controller-without-the-ignored-record-still-verifies-the-promotion
  (testing "a controller older than stageset-controller 799771f writes no ignoredPromotion"
    (let [object (-> (with-status {:lastHandledPromotion "T1"})
                     (update-in [:status :stages 1] dissoc :ignoredPromotion))]
      (is (not (contains? (get-in object [:status :stages 1]) :ignoredPromotion)))
      (is (true? (:held? (post-check object))))
      (is (nil? (:abort (post-check object))))))
  (testing "while it has not acted yet, the check waits without aborting"
    (let [{:keys [held? abort]} (post-check (site "AwaitingManual"))]
      (is (false? held?))
      (is (nil? abort)))))

(deftest a-token-the-controller-ignored-ends-the-post-check-at-once
  (doseq [reason ["NoPromotionGate" "AlreadyPromoted" "NotHeld" "RevisionMismatch"]]
    (testing reason
      (let [{:keys [held? abort]} (post-check (ignored "T1" reason))]
        (is (false? held?))
        (is (= (format "the controller ignored the promotion (%s): promote token T1 ignored: %s" reason reason)
               abort)))
      (let [{:keys [verdict millis output]} (run-post-check (ignored "T1" reason))]
        (is (false? verdict))
        (is (str/includes? output (str "NOT verified: stage prod of StageSet apps/site acted on the promotion of revision r2 - the controller ignored the promotion (" reason ")")))
        (is (< millis 5000) "the wait ends on the first poll, not after 300 s")))))

(deftest another-token-in-the-ignored-record-does-not-fail-this-run
  (let [{:keys [held? abort]} (post-check (ignored "T0" "NotHeld"))]
    (is (false? held?))
    (is (nil? abort)))
  (testing "and this run's token, handled after an older one was ignored, verifies"
    (let [object (update-in (ignored "T0" "NotHeld") [:status :stages 1] assoc :lastHandledPromotion "T1")]
      (is (true? (:held? (post-check object))))
      (is (nil? (:abort (post-check object)))))))

(deftest a-token-ignored-for-another-stage-does-not-fail-this-run
  (let [object (update-in (site "AwaitingManual") [:status :stages 0] assoc
                          :ignoredPromotion {:token "T1" :reason "NotHeld"})]
    (is (nil? (:abort (post-check object))))))

(deftest the-recovery-names-the-reason-and-what-to-do
  (doseq [[reason advice] {"NoPromotionGate" "no promotion gate"
                           "AlreadyPromoted" "already cleared its gate"
                           "NotHeld" "Run the book again once it is"
                           "RevisionMismatch" "Inspect the revision it is held at now"}]
    (testing reason
      (let [object (assoc-in (ignored "T1" reason) [:metadata :annotations :stages.metio.wtf/promote] "prod@T1")
            text (stageset/promotion-recovery "ctx" "apps/site" object "prod" "T1")]
        (is (str/includes? text (format "ignored the promotion of stage prod of StageSet apps/site (%s)" reason)))
        (is (str/includes? text advice))
        (is (str/includes? text "never judges that token again"))
        (is (not (str/includes? text "STILL ARMED"))))))
  (testing "the book's recovery reads the StageSet and passes this run's token"
    (with-redefs [kubectl/get-namespaced-resource (fn [& _] (ignored "T1" "RevisionMismatch"))]
      (is (str/includes? ((:recovery (book-spec)) {:context "ctx" :target "apps/site" :stage "prod" :token "T1"})
                         "(RevisionMismatch)")))))

(deftest an-armed-token-bound-to-a-revision-says-which-controllers-honour-the-binding
  (let [object (-> (armed "prod@t1")
                   (assoc-in [:metadata :annotations :stages.metio.wtf/promote-revision] "r2"))
        text (stageset/promotion-recovery "ctx" "apps/site" object "prod" "t1")]
    (is (str/includes? text "STILL ARMED"))
    (is (str/includes? text "bound to revision r2"))
    (is (str/includes? text "an older one ignores the binding"))))
