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
            [gumshoe.kubectl :as kubectl]
            [gumshoe.loki :as loki]
            [gumshoe.select :as select]))

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
  "The Loki namespace: the flag or env.edn, else picked from the namespaces
   that hold Loki workloads. nil, after saying why, when there is none."
  [context opts selector]
  (or (inputs/value :loki-namespace opts)
      (select/pick {:mode :one :label "Loki namespace"
                    :candidates #(kubectl/namespaces-names (kubectl/get-selected % "deployments" selector))
                    :empty-message (format "no Loki namespace - set [:loki :namespace] in env.edn, pass --namespace, or check --selector (no deployment matches %s)" selector)
                    :on-empty :fail}
                   {} context)))

(defn- distributor-service
  "The distributor's service in the namespace, the one that serves the ring
   endpoints - or nil when there is none to port-forward to."
  [context namespace selector]
  (->> (kubectl/items-of (kubectl/get-selected context "services" selector))
       (filter #(= namespace (kubectl/namespace-of %)))
       (filter #(= "distributor" (get-in % [:metadata :labels (keyword "app.kubernetes.io/component")])))
       first))

(defn- with-distributor
  "Resolves the Loki namespace and runs the collection with it as :namespace,
   over a port-forward to the distributor when there is one, whose base URL
   it hands on as :ring-url. The ring check is a bonus over the component
   health: a missing distributor is already reported as a component, so it is
   never a reason to fail the run on its own."
  [opts run]
  (let [context (kubectl/current-context)
        selector (:selector opts)
        namespace (resolve-namespace context opts selector)]
    (if (str/blank? (str namespace))
      false
      (let [opts (assoc opts :namespace namespace)]
        (if-let [service (distributor-service context namespace selector)]
          (kubectl/with-port-forward {:context context
                                      :namespace namespace
                                      :service (kubectl/name-of service)
                                      :local-port (:port opts)
                                      :remote-port (kubectl/service-port-or-first service (:port-name opts))}
            (fn [] (run (assoc opts :ring-url (str "http://localhost:" (:port opts))))))
          (run opts))))))

(defn- collect
  [{:keys [namespace selector ring-url]}]
  (merge (loki/collect-workloads! {:context (kubectl/current-context) :namespace namespace :selector selector})
         (when ring-url (loki/collect-rings! ring-url))))

(detective/book
 {:description "Investigates a microservice-mode Loki: component discovery, health, and hash rings"
  :options options
  :prerequisites prerequisites
  :detectives loki-detectives/detectives
  :when-to-run "Reach for this when logs are missing or queries fail - it discovers the Loki components in the namespace and checks their health and hash rings."
  :around with-distributor
  :collect collect})
