;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detective-properties-test
  "Properties every detective in the registry must hold, whatever it checks:
   it never throws on evidence that is missing or empty, and every finding it
   returns is one the report, the exit code and the drill-down can use."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.registry :as registry]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(def ^:private severities #{:critical :warning :info})

(defn- well-formed?
  [finding]
  (and (contains? severities (:severity finding))
       (string? (:summary finding)) (seq (:summary finding))
       (some? (:component finding))
       (or (nil? (:hint finding)) (string? (:hint finding)))
       (or (nil? (:since finding))
           (and (string? (:since finding)) (java.time.Instant/parse (:since finding))))
       (or (nil? (:subject finding))
           (and (string? (-> finding :subject :kind)) (string? (-> finding :subject :name))))))

(defn- detect
  [detective evidence]
  (try
    {:findings (doall ((:detect detective) evidence))}
    (catch Exception e {:error (or (ex-message e) (str (class e)))})))

(deftest every-detective-survives-missing-evidence
  (doseq [detective (registry/all)]
    (let [{:keys [error findings]} (detect detective {:now now})]
      (is (nil? error) (str (:name detective) " threw: " error))
      (is (every? well-formed? findings) (:name detective)))))

(deftest every-detective-survives-empty-lists
  (doseq [detective (registry/all)
          :let [evidence (into {:now now} (for [key (:requires detective)] [key {:items []}]))
                {:keys [error findings]} (detect detective evidence)]]
    (is (nil? error) (str (:name detective) " threw: " error))
    (is (every? well-formed? findings) (:name detective))))

(def ^:private kubernetes-detectives
  "The detectives that judge Kubernetes objects only, so an empty cluster
   gives them nothing to report."
  #{"services" "load-balancers" "pod-creation" "pod-startup" "pods" "stuck-pods" "stuck-terminating"
    "ingress-references" "tls-secrets" "template-references" "storageclass-references" "role-references"
    "webhook-references" "deployments" "statefulsets" "rollouts" "daemonsets" "jobs" "cronjobs"
    "hpas" "hpa-failures" "pdbs" "api-services" "crd-health" "custom-resource-conditions"})

(deftest an-empty-cluster-is-clean
  (let [by-name (into {} (map (juxt :name identity)) (registry/all))]
    (testing "every listed detective is registered"
      (is (every? by-name kubernetes-detectives)
          (remove by-name kubernetes-detectives)))
    (doseq [name kubernetes-detectives
            :let [detective (by-name name)]
            :when detective]
      (is (empty? (:findings (detect detective (into {:now now} (for [key (:requires detective)] [key {:items []}])))))
          name))))

(deftest every-detective-has-a-unique-name-and-a-description
  (let [all (registry/all)]
    (is (= (count all) (count (set (map :name all)))))
    (doseq [detective all]
      (is (seq (:description detective)) (:name detective))
      (is (every? string? (:requires detective)) (:name detective)))))
