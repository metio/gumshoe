;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.effect-plugin-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.effect :as effect]
            [gumshoe.shell :as shell]
            [gumshoe.spec :as spec]))

(deftest a-plugin-effect-keeps-every-flag-after-the-plugin-name
  (is (= [:kubectl-plugin "staging" "cnpg" "promote" "--namespace=db" "pg" "pg-2"]
         (effect/kubectl-plugin "staging" "cnpg" "promote" "--namespace=db" "pg" "pg-2")))
  (is (= ["cnpg" "promote" "--namespace=db" "pg" "pg-2" "--context=staging"]
         (effect/kubectl-plugin-args "staging" "cnpg" ["promote" "--namespace=db" "pg" "pg-2"])))
  (is (= ["netshoot" "--context=c"] (effect/kubectl-plugin-args "c" "netshoot" nil))))

(deftest a-plugin-effect-is-a-valid-plan-step
  (is (spec/valid? :gumshoe.spec/effect (effect/kubectl-plugin "ctx" "netshoot" "debug" "pod")))
  (is (spec/valid? :gumshoe.spec/plan (effect/plan (effect/kubectl-plugin "ctx" "cnpg" "status" "pg")))))

(deftest a-plugin-effect-describes-the-command-it-runs
  (is (= "kubectl cnpg promote --namespace=db pg pg-2 --context=staging"
         (effect/describe (effect/kubectl-plugin "staging" "cnpg" "promote" "--namespace=db" "pg" "pg-2")))))

(deftest a-plugin-effect-runs-kubectl-with-the-plugin-first
  (let [seen (atom nil)]
    (with-redefs [shell/run-with-output (fn [& args] (reset! seen args) 0)]
      (is (true? (effect/run! (effect/plan (effect/kubectl-plugin "staging" "netshoot" "debug" "web" "--namespace" "shop")))))
      (is (= ["kubectl" "netshoot" "debug" "web" "--namespace" "shop" "--context=staging"] @seen)))
    (with-redefs [shell/run-with-output (constantly 1)]
      (is (false? (effect/run! (effect/plan (effect/kubectl-plugin "staging" "cnpg" "promote" "pg" "pg-2"))))))))
