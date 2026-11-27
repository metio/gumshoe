;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.namespace-deletion-conditions-test
  "The terminating-namespaces check over each of the five deletion conditions
   the namespace controller writes: the three that never resolve are critical
   at once, the two transient ones warn only past terminating-grace, a
   condition whose status is not True never fires, and every finding quotes
   the condition's message stripped of control characters and bounded. A
   namespace on which none of the five holds warns past the grace, naming the
   namespace controller, or the metadata finalizers once the controller is done."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cleanup :as cleanup]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.namespaces :as namespaces]))

(def ^:private now (java.time.Instant/parse "2026-11-26T12:00:00Z"))
(def ^:private fresh "2026-11-26T11:59:00Z")
(def ^:private old "2026-11-26T11:00:00Z")

(def ^:private failures
  ["NamespaceDeletionDiscoveryFailure" "NamespaceDeletionGroupVersionParsingFailure" "NamespaceDeletionContentFailure"])

(def ^:private transient-pair
  ["NamespaceFinalizersRemaining" "NamespaceContentRemaining"])

(defn- condition
  ([type] (condition type "True"))
  ([type status] (condition type status (str type " message")))
  ([type status message]
   (cond-> {:type type :status status :reason "Reason" :lastTransitionTime "2026-11-01T00:00:00Z"}
     message (assoc :message message))))

(defn- terminating [deleted & conditions]
  {:metadata (cond-> {:name "gone"} deleted (assoc :deletionTimestamp deleted))
   :status {:phase "Terminating" :conditions (vec conditions)}})

(defn- stuck [now & namespaces]
  (vec (namespaces/detect-stuck-namespaces {:now now "namespaces" {:items (vec namespaces)}})))

(deftest the-check-covers-every-condition-the-controller-writes
  (is (= (set cleanup/deletion-condition-types) (set (concat failures transient-pair)))))

(deftest each-failure-is-critical-at-once
  (doseq [type failures]
    (testing type
      (let [[finding & more] (stuck now (terminating fresh (condition type)))]
        (is (nil? more))
        (is (= :critical (:severity finding)))
        (is (= type (:condition finding)))
        (is (= "gone" (:component finding)))
        (is (= fresh (:since finding)))))))

(deftest a-failure-is-reported-without-an-age-too
  (doseq [type failures]
    (testing type
      (is (= [:critical] (map :severity (stuck nil (terminating nil (condition type)))))))))

(deftest each-transient-condition-waits-for-the-grace
  (doseq [type transient-pair]
    (testing type
      (is (empty? (stuck now (terminating fresh (condition type)))) "within the grace")
      (is (= [:warning] (map :severity (stuck now (terminating old (condition type))))) "past the grace"))))

(deftest the-grace-starts-at-the-deletion-not-at-the-condition
  (testing "a condition that turned True long ago in a fresh deletion is still in flight"
    (is (empty? (stuck now (terminating fresh (assoc (condition "NamespaceFinalizersRemaining")
                                                     :lastTransitionTime "2026-01-01T00:00:00Z")))))))

(deftest no-condition-fires-unless-its-status-is-true
  (doseq [type (concat failures transient-pair)
          status ["False" "Unknown" nil]]
    (testing (str type " " status)
      (is (empty? (stuck now (terminating fresh (condition type status)))))
      (is (= [nil] (map :condition (stuck now (terminating old (condition type status)))))
          "past the grace it explains nothing, so the namespace reads as silent"))))

(deftest a-recovered-failure-left-false-beside-a-transient-one-is-quiet
  (testing "conditions persist with status False after recovery"
    (is (empty? (stuck now (terminating fresh
                                        (condition "NamespaceDeletionDiscoveryFailure" "False" "All resources successfully discovered")
                                        (condition "NamespaceDeletionGroupVersionParsingFailure" "False" "All legacy kube types successfully parsed")
                                        (condition "NamespaceDeletionContentFailure" "False" "All content successfully deleted")
                                        (condition "NamespaceContentRemaining" "True" "Some resources are remaining: pods has 1 resource instances")
                                        (condition "NamespaceFinalizersRemaining" "False" "All content-preserving finalizers finished")))))))

(deftest failures-and-transients-differ-in-hint
  (let [hint-of (fn [type deleted] (:hint (first (stuck now (terminating deleted (condition type))))))
        hints (into {} (map (fn [type] [type (hint-of type old)])) (concat failures transient-pair))]
    (is (= 5 (count (set (vals hints)))) "every condition has its own hint")
    (is (re-find #"api-services" (hints "NamespaceDeletionDiscoveryFailure")))
    (is (re-find #"webhook-backends" (hints "NamespaceDeletionDiscoveryFailure")))
    (is (re-find #"crd-health" (hints "NamespaceDeletionGroupVersionParsingFailure")))
    (is (re-find #"admission webhook" (hints "NamespaceDeletionContentFailure")))
    (is (re-find #"do not remove the namespace's finalizer" (hints "NamespaceDeletionContentFailure")))
    (is (re-find #"controller owning" (hints "NamespaceFinalizersRemaining")))))

(deftest a-content-failure-quotes-the-refusal
  (let [refusal "limitranges \"Unknown\" is forbidden: ValidatingAdmissionPolicy 'deny-deletes' denied request"
        [finding] (stuck now (terminating fresh (condition "NamespaceDeletionContentFailure" "True" refusal)))]
    (is (= refusal (:message finding)))
    (is (str/includes? (:hint finding) refusal))
    (is (str/includes? (:hint finding) (str "deleting since " fresh)))))

(deftest a-message-cannot-reach-the-terminal-raw
  (let [hostile (str "pods \"x\" is forbidden:\u001b[2J\u001b[31mdenied\u001b[0m\r\nby‮ webhook\u0007"
                     "\u001b]8;;https://example.invalid\u0007link\u001b]8;;\u0007")
        [finding] (stuck now (terminating fresh (condition "NamespaceDeletionContentFailure" "True" hostile)))]
    (doseq [text [(:message finding) (:hint finding)]]
      (is (not (re-find #"[\p{Cc}\p{Cf}\p{Zl}\p{Zp}]" text)) text))
    (is (= "pods \"x\" is forbidden:denied by webhook link" (:message finding)))))

(deftest a-long-message-is-bounded
  (let [long-message (apply str "Failed to delete all resource types: " (repeat 2000 "x"))
        [finding] (stuck now (terminating fresh (condition "NamespaceDeletionContentFailure" "True" long-message)))]
    (is (= namespaces/message-limit (count (:message finding))))
    (is (str/ends-with? (:message finding) "…"))
    (is (str/starts-with? (:message finding) "Failed to delete all resource types: "))
    (is (< (count (:hint finding)) (+ namespaces/message-limit 1000)))))

(deftest a-condition-without-a-message-still-names-itself
  (let [[finding] (stuck now (terminating fresh (condition "NamespaceDeletionDiscoveryFailure" "True" nil)))]
    (is (not (contains? finding :message)))
    (is (str/starts-with? (:hint finding) "NamespaceDeletionDiscoveryFailure -- deleting since"))))

(deftest mixed-conditions-report-each-cause-and-fold-content-remaining
  (let [findings (stuck now (terminating old
                                         (condition "NamespaceContentRemaining")
                                         (condition "NamespaceFinalizersRemaining")
                                         (condition "NamespaceDeletionDiscoveryFailure")
                                         (condition "NamespaceDeletionContentFailure")))]
    (is (= {"NamespaceFinalizersRemaining" :warning
            "NamespaceDeletionDiscoveryFailure" :critical
            "NamespaceDeletionContentFailure" :critical}
           (into {} (map (juxt :condition :severity)) findings)))))

(deftest a-failure-in-a-fresh-deletion-is-reported-while-the-transients-wait
  (is (= ["NamespaceDeletionGroupVersionParsingFailure"]
         (map :condition (stuck now (terminating fresh
                                                 (condition "NamespaceFinalizersRemaining")
                                                 (condition "NamespaceContentRemaining")
                                                 (condition "NamespaceDeletionGroupVersionParsingFailure")))))))

(deftest a-namespace-that-is-not-terminating-is-never-judged
  (is (empty? (stuck now (assoc-in (terminating old (condition "NamespaceDeletionContentFailure"))
                                   [:status :phase] "Active")))))

(deftest unknown-condition-types-are-ignored
  (is (empty? (stuck now (terminating fresh (condition "SomethingElse")))))
  (is (= [nil] (map :condition (stuck now (terminating old (condition "SomethingElse")))))))

(deftest the-scan-keeps-the-findings
  (testing "through run-detectives, as a scan reports them"
    (let [findings (detective/run-detectives namespaces/detectives
                                             {:now now "namespaces" {:items [(terminating fresh (condition "NamespaceDeletionContentFailure"))]}})]
      (is (= ["NamespaceDeletionContentFailure"] (map :condition findings))))))

;; --- no deletion condition holds ------------------------------------------------------

(defn- with-finalizers [namespace spec-finalizers meta-finalizers]
  (cond-> namespace
    spec-finalizers (assoc-in [:spec :finalizers] spec-finalizers)
    meta-finalizers (assoc-in [:metadata :finalizers] meta-finalizers)))

(def ^:private all-false
  (map #(condition % "False") (concat failures transient-pair)))

(def ^:private silent-summary
  "namespace cannot finish terminating: the namespace controller has recorded no progress")

(deftest no-conditions-within-the-grace-is-quiet
  (is (empty? (stuck now (with-finalizers (terminating fresh) ["kubernetes"] nil)))))

(deftest no-conditions-past-the-grace-names-the-namespace-controller
  (let [[finding & more] (stuck now (with-finalizers (terminating old) ["kubernetes"] nil))]
    (is (nil? more))
    (is (= :warning (:severity finding)))
    (is (= silent-summary (:summary finding)))
    (is (contains? finding :condition))
    (is (nil? (:condition finding)))
    (is (= "gone" (:component finding)))
    (is (= old (:since finding)))
    (is (str/starts-with? (:hint finding) "no deletion condition written -- deleting since "))
    (doseq [part ["kube-controller-manager" "control-plane check" "--controllers" "logs"
                  "do not remove the namespace's finalizer"]]
      (is (str/includes? (str/lower-case (:hint finding)) (str/lower-case part)) part))))

(deftest every-condition-false-past-the-grace-is-silent-too
  (let [[finding & more] (stuck now (with-finalizers (apply terminating old all-false) ["kubernetes"] nil))]
    (is (nil? more))
    (is (= silent-summary (:summary finding)))
    (is (str/starts-with? (:hint finding) "no deletion condition reads True"))))

(deftest every-condition-false-within-the-grace-is-quiet
  (is (empty? (stuck now (apply terminating fresh all-false)))))

(deftest conditions-reading-unknown-or-of-another-type-explain-nothing
  (is (= [silent-summary]
         (map :summary (stuck now (terminating old
                                               (condition "NamespaceContentRemaining" "Unknown")
                                               (condition "SomethingElse")))))))

(deftest no-conditions-without-an-age-is-reported
  (testing "as FinalizersRemaining is without an age"
    (is (= [:warning] (map :severity (stuck nil (terminating nil)))))
    (is (= [:warning] (map :severity (stuck now (terminating nil)))))))

(deftest one-true-condition-suppresses-the-silent-finding
  (doseq [type (concat failures transient-pair)]
    (testing type
      (let [findings (stuck now (apply terminating old (condition type) (remove #(= type (:type %)) all-false)))]
        (is (= [type] (map :condition findings)))
        (is (not-any? #(= silent-summary (:summary %)) findings))))))

(deftest a-transient-condition-within-the-grace-is-not-reported-as-silent
  (is (empty? (stuck now (terminating fresh (condition "NamespaceFinalizersRemaining"))))))

(deftest metadata-finalizers-after-the-controller-finished-name-the-finalizers
  (let [[finding & more] (stuck now (with-finalizers (apply terminating old all-false)
                                                     [] ["example.com/cleanup\u001b[31m"]))]
    (is (nil? more))
    (is (= :warning (:severity finding)))
    (is (nil? (:condition finding)))
    (is (= "namespace cannot finish terminating: a finalizer on the namespace itself has not returned"
           (:summary finding)))
    (is (str/includes? (:hint finding) "metadata.finalizers remain: example.com/cleanup"))
    (is (not (re-find #"\p{Cc}" (:hint finding))))
    (is (not (str/includes? (:hint finding) "kube-controller-manager")))))

(deftest a-spec-finalizer-beside-metadata-finalizers-is-still-the-controller
  (is (= [silent-summary]
         (map :summary (stuck now (with-finalizers (terminating old) ["kubernetes"] ["example.com/cleanup"]))))))

(deftest a-namespace-that-is-not-terminating-never-reads-as-silent
  (doseq [phase ["Active" nil]]
    (testing (str phase)
      (is (empty? (stuck now (assoc-in (terminating old) [:status :phase] phase)))))))

(deftest the-scan-keeps-the-silent-finding
  (let [findings (detective/run-detectives namespaces/detectives
                                           {:now now "namespaces" {:items [(terminating old)
                                                                           (assoc-in (terminating old) [:status :phase] "Active")]}})]
    (is (= [silent-summary] (map :summary findings)))))
