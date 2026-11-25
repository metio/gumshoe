;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps-objects-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
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
