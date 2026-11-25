;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.change-destructive-test
  "One rule for a destructive effect beside :confirm :destructive?, judged
   alike by gumshoe.change/normalize, gumshoe.change/compose (in both orders
   and every grouping) and gumshoe.dsl/compile: an absent :destructive? is
   lifted to true, true stays true, and an explicit false is refused in the
   words gumshoe.change/destructive-contradiction gives."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.conformance :as conformance]
            [gumshoe.dsl :as dsl]
            [gumshoe.mutation :as mutation]
            [gumshoe.playbook :as playbook]
            [gumshoe.runbook :as runbook]
            [gumshoe.steps.objects :as objects]))

(def ^:private delete-step
  (objects/delete {:objects :targets :none "nothing" :timeout 30}))

(def ^:private plain-step
  {:items (fn [_] ["note it"])
   :effect (fn [_] [[:note "nothing destroyed"]])
   :verify (fn [_] [])})

(def ^:private own
  {:title "Clean up"
   :prerequisites {}
   :confirm {:action "clean up"}})

(def ^:private delete-refusal
  (str "the :effect gumshoe.steps.objects/delete destroys what it acts on, and the :confirm says"
       " :destructive? false, which would confirm it with a bare keypress - drop :destructive? false"))

(defn- with-stated
  [spec stated]
  (cond-> spec (not= ::absent stated) (assoc-in [:confirm :destructive?] stated)))

(defn- outcome
  "{:confirm <the canonical :confirm>} or {:refused <message>} for a
   function building a spec through normalize, compose or the DSL."
  [build]
  (try {:confirm (:confirm (build))}
       (catch clojure.lang.ExceptionInfo e
         (if (or (change/invalid? e) (dsl/invalid? e))
           {:refused (ex-message e)}
           (throw e)))))

;; --- normalize and compose -----------------------------------------------------------

(def ^:private paths
  {"normalize of a merge, own keys first" (fn [own step] (change/normalize (merge own step)))
   "normalize of a merge, fragment first" (fn [own step] (change/normalize (merge step own)))
   "compose, own keys first" (fn [own step] (change/compose own step))
   "compose, fragment first" (fn [own step] (change/compose step own))
   "compose of a merge" (fn [own step] (change/compose (merge own step)))})

(def ^:private expected
  "[stated, marked?] -> the outcome every path gives."
  {[::absent true] {:confirm {:action "clean up" :destructive? true}}
   [true true] {:confirm {:action "clean up" :destructive? true}}
   [false true] {:refused delete-refusal}
   [::absent false] {:confirm {:action "clean up"}}
   [true false] {:confirm {:action "clean up" :destructive? true}}
   [false false] {:confirm {:action "clean up" :destructive? false}}})

(deftest normalize-and-compose-judge-a-destructive-effect-alike-in-every-order
  (doseq [stated [::absent false true]
          marked? [true false]
          [how build] paths]
    (testing (format "%s, :destructive? %s, %s effect" how stated (if marked? "marked" "unmarked"))
      (is (= (expected [stated marked?])
             (outcome #(build (with-stated own stated) (if marked? delete-step plain-step))))))))

(deftest a-refusal-carries-the-effect-it-names
  (let [e (try (change/normalize (merge (with-stated own false) delete-step)) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (change/invalid? e))
    (is (= "gumshoe.steps.objects/delete" (:effect (ex-data e))))))

(deftest an-effect-marked-without-a-name-is-named-by-its-mark
  (is (= {:refused (str "the :effect marked by gumshoe.change/destructive destroys what it acts on, and the :confirm"
                        " says :destructive? false, which would confirm it with a bare keypress - drop :destructive? false")}
         (outcome #(change/normalize {:confirm {:action "x" :destructive? false}
                                      :effect (change/destructive (fn [_] []))})))))

(deftest a-false-absorbed-by-another-parts-true-is-still-refused-beside-a-destructive-effect
  ;; true OR false is true, so a later part's mark would otherwise meet a
  ;; :confirm that no longer shows the false
  (let [says-true {:confirm {:action "clean up" :destructive? true}}
        says-false {:confirm {:destructive? false}}]
    (doseq [[how build] {"left grouping" #(change/compose (change/compose says-true says-false) delete-step)
                         "right grouping" #(change/compose says-true (change/compose says-false delete-step))
                         "flat" #(change/compose says-true says-false delete-step)
                         "mark first" #(change/compose delete-step (change/compose says-true says-false))
                         "normalize of a merge over the composed parts" #(change/normalize (merge delete-step (change/compose says-true says-false)))}]
      (testing how
        (is (= {:refused delete-refusal} (outcome build)))))
    (testing "without a destructive effect the parts still OR to true"
      (is (= {:confirm {:action "clean up" :destructive? true}}
             (outcome #(change/compose (change/compose says-true says-false) plain-step)))))))

(deftest the-shared-check-agrees-with-every-effect-unless-false-meets-a-destructive-one
  (is (nil? (change/destructive-contradiction nil false)))
  (is (nil? (change/destructive-contradiction "x" nil)))
  (is (nil? (change/destructive-contradiction "x" true)))
  (is (= (str "the :effect x destroys what it acts on, and the :confirm says :destructive? false, which would"
              " confirm it with a bare keypress - drop :destructive? false")
         (change/destructive-contradiction "x" false))))

(deftest a-mark-keeps-its-name-and-marking-again-without-one-changes-nothing
  (let [f (change/destructive "a/b" (fn [_] []))]
    (is (= "a/b" (change/destructive-effect f)))
    (is (identical? f (change/destructive f)))
    (is (identical? f (change/destructive "a/b" f)))
    (is (= "c/d" (change/destructive-effect (change/destructive "c/d" f))))
    (is (nil? (change/destructive-effect (fn [_] []))))))

;; --- the DSL ---------------------------------------------------------------------------

(def ^:private dsl-base
  {:gumshoe/dsl :v0-experimental
   :description "Changes a ConfigMap"
   :target {:pick :namespaced :label "ConfigMap" :type "configmaps"
            :namespace-flag :namespace :name-flag :name
            :candidates {:unless :state :order :listed}
            :empty "nothing to change"}
   :prerequisites {:installed-tools ["kubectl"]}
   :confirm {:action "clean up"}})

(defn- dsl-change
  [stated marked?]
  (-> (if marked?
        (assoc dsl-base
               :effect [[:kubectl :annotate {:annotations {"a" "b"}}] [:kubectl :delete]]
               :state {:holds :absent :says "{target} is gone"}
               :recovery "recreate {target}")
        (assoc dsl-base
               :effect [[:kubectl :annotate {:annotations {"a" "b"}}]]
               :state {:holds [:field [:metadata :annotations :a] := "b"] :says "{target} is annotated"}
               :recovery "remove the annotation from {target}"))
      (with-stated stated)))

(def ^:private dsl-refusal
  (str "[:confirm :destructive?]: the :effect [:kubectl :delete] at [:effect 1] destroys what it acts on, and the :confirm says"
       " :destructive? false, which would confirm it with a bare keypress - drop :destructive? false,"
       " or :opt-out {:destructive \"why\"}"))

(deftest the-dsl-judges-a-delete-as-normalize-judges-a-marked-effect
  ;; the DSL states :destructive? on every compiled change, false where
  ;; normalize leaves it absent; the path prefixes its refusal
  (doseq [stated [::absent false true]
          marked? [true false]]
    (testing (format ":destructive? %s, %s" stated (if marked? "deleting" "not deleting"))
      (let [got (outcome #(dsl/compile (dsl-change stated marked?)))
            want (expected [stated marked?])]
        (if (:refused want)
          (is (= {:refused dsl-refusal} got))
          (is (= (boolean (get-in want [:confirm :destructive?])) (get-in got [:confirm :destructive?]))
              (pr-str got)))))))

(deftest a-compiled-delete-carries-the-mark-so-a-later-compose-judges-it-too
  (let [compiled (dsl/compile (dsl-change ::absent true))]
    (is (= "[:kubectl :delete] at [:effect 1]" (change/destructive-effect (:effect compiled))))
    (is (= {:refused (str "the :effect [:kubectl :delete] at [:effect 1] destroys what it acts on, and the :confirm says"
                          " :destructive? false, which would confirm it with a bare keypress - drop :destructive? false")}
           (outcome #(change/compose compiled {:confirm {:destructive? false}}))))))

(deftest an-opted-out-dsl-delete-carries-no-mark-and-normalize-accepts-it
  (let [compiled (dsl/compile (-> (dsl-change false true)
                                  (assoc :opt-out {:destructive "a scratch namespace"})
                                  (dissoc :recovery)))]
    (is (false? (get-in compiled [:confirm :destructive?])))
    (is (nil? (change/destructive-effect (:effect compiled))))
    (is (= compiled (change/normalize compiled)))))

;; --- where a refusal surfaces ----------------------------------------------------------

(def ^:private contradicting
  (merge {:description "Deletes the leftovers"
          :select {:mode :one :label "Release"}
          :prerequisites {}
          :confirm {:action "delete the leftovers" :destructive? false}}
         delete-step))

(deftest a-mutation-book-is-refused-as-it-loads
  (let [e (try (binding [runbook/*loading* true] (mutation/book contradicting)) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (change/invalid? e))
    (is (= delete-refusal (ex-message e)))))

(deftest conformance-names-the-refusal-as-a-spec-contract-finding
  (is (= [(str "is refused: " delete-refusal)]
         (conformance/spec-problems {:spec contradicting :select (:select contradicting)}))))

(deftest a-playbook-is-refused-before-its-confirmation-and-before-any-step-runs
  (let [ran (atom [])
        first-step (assoc own :select {:mode :one :label "Release"}
                          :title "first" :effect (fn [_] (swap! ran conj :first) [])
                          :no-verify "a test step")
        later (assoc contradicting :title "later")
        e (try (playbook/prejudge! [first-step later] {:target "web" :opts {}} true) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (change/invalid? e))
    (is (= delete-refusal (ex-message e)))
    (is (empty? @ran))))
