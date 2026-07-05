;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.loki
  "Investigates a microservice-mode Loki: discovers its components in the
   namespace, checks each is healthy, and - when the distributor is reachable -
   inspects the hash rings over a port-forward. The namespace comes from
   env.edn or a flag; the components are found without asking."
  (:require [clojure.string :as str]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.loki :as loki-detectives]
            [gumshoe.inputs :as inputs]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.loki :as loki]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def options
  (merge {:namespace {:desc "The Loki namespace - env.edn (:loki :namespace) or interactive when omitted"
                      :alias :n
                      :coerce :string}
          :selector {:desc "Label selector that finds the Loki workloads"
                     :alias :l
                     :default "app.kubernetes.io/name=loki"
                     :coerce :string}
          :port-name {:desc "The distributor service port to forward for the ring check"
                      :default "http-metrics"
                      :coerce :string}
          :port {:desc "The local port used for the port-forward"
                 :alias :p
                 :default 13100
                 :coerce :long}}
         detective/output-option))

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-get ["deployments" "statefulsets" "services"]})

(defn- resolve-namespace
  "The Loki namespace: the flag or env.edn, else discovered from the namespaces
   that hold Loki workloads - asked only when more than one exists."
  [context opts selector]
  (or (inputs/value :loki-namespace opts)
      (let [deployments (kubectl/get-selected context "deployments" selector)]
        (interact/choose-one "Loki namespace" (kubectl/namespaces-names deployments) nil))))

(defn- distributor-service
  "The distributor's service in the namespace, the one that serves the ring
   endpoints - or nil when there is none to port-forward to."
  [context namespace selector]
  (->> (kubectl/items-of (kubectl/get-selected context "services" selector))
       (filter #(= namespace (kubectl/namespace-of %)))
       (filter #(= "distributor" (get-in % [:metadata :labels (keyword "app.kubernetes.io/component")])))
       first))

(defn- investigate
  [opts _ctx]
  (detective/when-to-run! "Reach for this when logs are missing or queries fail - it discovers the Loki components in the namespace and checks their health and hash rings.")
  (let [context (kubectl/current-context)
        selector (:selector opts)
        namespace (resolve-namespace context opts selector)]
    (if (str/blank? (str namespace))
      (do (stdout/error "no Loki namespace - set [:loki :namespace] in env.edn, pass --namespace, or check --selector")
          false)
      (let [workload-evidence (loki/collect-workloads! {:context context :namespace namespace :selector selector})
            report (fn [ring-evidence]
                     (detective/report-and-offer!
                      context
                      loki-detectives/detectives
                      (detective/run-detectives loki-detectives/detectives
                                                (merge workload-evidence ring-evidence))
                      (:output opts "text")))]
        ;; the ring check is a bonus over the component health above: only when a
        ;; distributor is there to forward to, and never a reason to fail the run
        ;; on its own - a missing distributor is already reported as a component.
        (if-let [service (distributor-service context namespace selector)]
          (kubectl/with-port-forward {:context context
                                      :namespace namespace
                                      :service (kubectl/name-of service)
                                      :local-port (:port opts)
                                      :remote-port (kubectl/service-port-or-first service (:port-name opts))}
            (fn [] (report (loki/collect-rings! (str "http://localhost:" (:port opts))))))
          (report {}))))))

(runbook/execute!
 {:description "Investigates a microservice-mode Loki: component discovery, health, and hash rings"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action investigate})
