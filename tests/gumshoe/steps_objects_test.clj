;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps-objects-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.mutation :as mutation]
            [gumshoe.playbook :as playbook]
            [gumshoe.shell :as shell]
            [gumshoe.steps.objects :as objects]))

(def ^:private targets
  [{:type "deployments" :namespace "apps" :name "web"}
   {:type "persistentvolumeclaims" :namespace "apps" :name "data"}])

(def ^:private step
  (objects/delete {:objects :targets :none "nothing of the release" :timeout :wait}))

(deftest the-delete-step-deletes-each-object-without-waiting
  (is (= [[:kubectl "ctx" "-n" "apps" "delete" "deployments" "web" "--ignore-not-found" "--wait=false"]
          [:kubectl "ctx" "-n" "apps" "delete" "persistentvolumeclaims" "data" "--ignore-not-found" "--wait=false"]]
         (effect/collect ((:effect step) {:context "ctx" :targets targets}))))
  (is (= ["delete deployments apps/web" "delete persistentvolumeclaims apps/data"]
         ((:items step) {:targets targets}))))

(deftest with-nothing-to-delete-the-step-plans-a-note-and-checks-nothing
  (is (= [[:note "nothing of the release"]]
         (effect/collect ((:effect step) {:context "ctx" :targets []}))))
  (is (= ["nothing of the release to delete"] ((:items step) {:targets []})))
  (is (empty? ((:verify step) {:context "ctx" :targets []}))))

(deftest every-object-gets-a-post-check-with-the-steps-timeout
  (is (= [{:description "deployments apps/web is gone" :timeout 60 :interval 5 :verifies-absence? true}
          {:description "persistentvolumeclaims apps/data is gone" :timeout 60 :interval 5 :verifies-absence? true}]
         (map #(dissoc % :check) ((:verify step) {:context "ctx" :targets targets :wait 60}))))
  (testing "a fixed timeout, interval and wording per object"
    (let [step (objects/delete {:objects :targets :none "none" :timeout 30 :interval 2
                                :item #(str "delete " % " and its data")})]
      (is (= [30 2] ((juxt :timeout :interval) (first ((:verify step) {:targets targets})))))
      (is (= "delete deployments apps/web and its data" (first ((:items step) {:targets targets})))))))

(deftest an-object-is-gone-only-when-a-read-answered-without-it
  (let [check (:check (first ((:verify step) {:context "ctx" :targets targets :wait 60})))]
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out "" :err ""})]
      (is (true? (check))))
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out "deployment.apps/web" :err ""})]
      (is (false? (check)) "still there"))
    (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"})]
      (is (false? (check)) "a read that did not answer is no proof of a deletion"))))

;; --- a delete is destructive, however a caller combines it -------------------------

(def ^:private own
  {:title "Delete the leftovers"
   :prerequisites {:kubectl-can-delete ["secrets"]}
   :confirm {:action "delete the leftovers"}})

(defn- confirm-of
  [spec]
  (:confirm (change/normalize spec)))

(deftest the-delete-step-declares-itself-destructive
  (is (true? (change/destructive? step)))
  (is (true? (change/destructive-effect? (:effect step))))
  (is (= {:destructive? true} (confirm-of step))
      "a fragment alone already says it is destructive"))

(deftest a-caller-composing-the-delete-keeps-its-action-and-the-step-is-destructive
  (doseq [parts [[own step] [step own]]]
    (is (= {:action "delete the leftovers" :destructive? true}
           (:confirm (apply change/compose parts))))))

(deftest a-caller-merging-its-own-keys-before-the-delete-keeps-its-confirmation-action
  (let [merged (merge own step)]
    (is (= {:action "delete the leftovers" :destructive? true} (confirm-of merged)))
    (is (true? (playbook/destructive? merged)))))

(deftest a-caller-cannot-drop-the-destructive-confirmation-by-merging-over-the-delete
  (doseq [spec [(merge step (assoc-in own [:confirm :destructive?] false))
                (merge (assoc-in own [:confirm :destructive?] false) step)
                (change/compose step (assoc-in own [:confirm :destructive?] false))]]
    (is (true? (:destructive? (confirm-of spec))))
    (is (true? (change/destructive? spec)))
    (is (true? (playbook/destructive? spec)))))

(deftest normalize-keeps-a-destructive-delete-destructive-on-every-pass
  (let [once (change/normalize (merge own step))]
    (is (= once (change/normalize once)))
    (is (true? (change/destructive-effect? (:effect once))))))

(deftest a-playbook-of-merged-delete-steps-asks-for-a-typed-yes
  (let [steps [(merge (assoc-in own [:confirm :destructive?] false) step)]
        playbook {:confirm {:action "clean up" :destructive? false} :select {:mode :one}}]
    (is (true? (:destructive? (playbook/confirmation playbook steps {:cluster "c" :target "t"}))))
    (is (some #(re-find #"\(destructive - type yes\)" %)
              (playbook/details (assoc playbook :steps steps))))))

(deftest a-merged-delete-run-alone-is-confirmed-as-destructive
  (let [seen (atom nil)
        spec (merge {:select {:mode :one :label "Release"}}
                    (assoc-in own [:confirm :destructive?] false)
                    (objects/delete {:objects (constantly targets) :none "nothing" :timeout 1}))]
    (with-redefs [flow/change! (fn [config] (reset! seen config) true)]
      (binding [*out* (java.io.StringWriter.)]
        (mutation/run-on! (assoc spec :connect (constantly {:context "ctx" :cluster "c"}))
                          "web" {} nil)))
    (is (true? (get-in @seen [:confirmation :destructive?])))
    (is (= "delete the leftovers" (get-in @seen [:confirmation :action])))))

(deftest an-effect-that-is-not-marked-is-not-destructive-by-itself
  (is (false? (change/destructive? {:confirm {:action "x"} :effect (fn [_] [])})))
  (is (nil? (:destructive? (confirm-of {:confirm {:action "x"} :effect (fn [_] [])}))))
  (is (false? (change/destructive-effect? nil)))
  (is (false? (change/destructive-effect? "not a function"))))

(deftest a-caller-replacing-the-delete-effect-drops-its-destructiveness
  (let [spec (merge step own {:effect (fn [_] [[:note "nothing"]])})]
    (is (false? (change/destructive? spec)))))

(deftest marking-an-effect-destructive-keeps-it-callable-and-idempotent
  (let [f (change/destructive (fn [ctx] [[:note (:x ctx)]]))]
    (is (= [[:note 1]] (f {:x 1})))
    (is (true? (change/destructive-effect? f)))
    (is (= f (change/destructive f)) "marking twice returns the same function")))

(deftest every-way-of-combining-the-delete-with-a-confirmation-stays-destructive
  (doseq [stated [::absent false true]
          :let [own (cond-> own (not= ::absent stated) (assoc-in [:confirm :destructive?] stated))]
          [how spec] {"merge, own first" (merge own step)
                      "merge, fragment first" (merge step own)
                      "compose, own first" (change/compose own step)
                      "compose, fragment first" (change/compose step own)
                      "compose of a merge" (change/compose (merge own step))}]
    (testing (str how " with :destructive? " stated)
      (is (= {:action "delete the leftovers" :destructive? true} (confirm-of spec)))
      (is (true? (playbook/destructive? spec))))))
