;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.inputs-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.config :as config]
            [infra.inputs :as inputs]))

(def ^:private matrix-host
  {:key :matrix-host :flag :host :env-path [:matrix :host] :default "synapse.example.org"})

(deftest resolve-one-test
  (testing "the flag wins over env and default"
    (is (= "flag.example" (inputs/resolve-one matrix-host
                                              {:host "flag.example"}
                                              (constantly "env.example")))))
  (testing "env is used when no flag is given"
    (is (= "env.example" (inputs/resolve-one matrix-host
                                             {}
                                             (fn [path] (when (= path [:matrix :host]) "env.example"))))))
  (testing "the declared default is the last resort"
    (is (= "synapse.example.org" (inputs/resolve-one matrix-host {} (constantly nil)))))
  (testing "a blank flag or env value counts as absent"
    (is (= "env.example" (inputs/resolve-one matrix-host {:host "  "} (constantly "env.example"))))
    (is (= "synapse.example.org" (inputs/resolve-one matrix-host {:host ""} (constantly "")))))
  (testing "an input with no default falls through to nil"
    (let [repo {:key :restic-repository :flag :repository :env-path [:restic :repository]}]
      (is (nil? (inputs/resolve-one repo {} (constantly nil)))))))

(deftest registry-shape-test
  (testing "every registered input has the fields resolution and the wizard rely on"
    (doseq [{:keys [key env-path prompt]} inputs/registry]
      (is (keyword? key))
      (is (and (vector? env-path) (seq env-path)) (str key " needs an :env-path"))
      (is (string? prompt) (str key " needs a :prompt for the wizard")))))

(deftest missing-test
  (testing "inputs already set for the environment are not reported missing"
    (let [config {:matrix {:host "already.set" :domain "already.set"}}
          missing (inputs/missing config nil)
          missing-keys (set (map :key missing))]
      (is (not (contains? missing-keys :matrix-host)))
      (is (not (contains? missing-keys :matrix-domain)))
      (is (contains? missing-keys :restic-repository)
          "an input with no stored value is still missing")))
  (testing "an empty config leaves every input missing"
    (is (= (count inputs/registry) (count (inputs/missing {} nil))))))

(deftest wizard-inputs-flow-through-config-test
  (testing "registry answers are assoc'd into the env body at their paths"
    (let [body (config/from-answers
                {:environment "staging"
                 :kubernetes-cluster "kube.staging"
                 :inputs [[[:matrix :host] "synapse.staging.example.org"]
                          [[:loki :namespace] "loki-stage"]]})]
      (is (= "synapse.staging.example.org" (get-in body [:environments :staging :matrix :host])))
      (is (= "loki-stage" (get-in body [:environments :staging :loki :namespace])))))
  (testing "a blank registry answer is omitted"
    (let [body (config/from-answers {:inputs [[[:matrix :host] "  "]]})]
      (is (empty? body)))))
