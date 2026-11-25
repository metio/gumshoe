;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.thanos
  "Investigates Thanos Query over a port-forward: readiness, connected store
   endpoints, and rule evaluation health."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.thanos :as thanos-detectives]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.select :as select]
            [gumshoe.thanos :as thanos]))

(def service-select
  "The Thanos Query service to ask. Its candidates and empty message depend
   on --selector, so the action adds them."
  {:mode :namespaced :label "Thanos Query" :namespace-flag :namespace :name-flag :service
   :on-empty :fail})

(def options
  (merge (select/options service-select
                         {:namespace {:desc "The namespace of the Thanos Query service - interactive selection when omitted"}
                          :service {:desc "The Thanos Query service - interactive selection when omitted"}})
         {:selector {:desc "Label selector used to find Thanos Query services"
                     :alias :l
                     :default "app.kubernetes.io/name=thanos-query"
                     :coerce :string}
          :port-name {:desc "The service port to forward"
                      :default "http"
                      :coerce :string}
          :port {:desc "The local port used for the port-forward"
                 :alias :p
                 :default 19090
                 :coerce :long}}
         detective/output-option))

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-get ["services"]})

(defn- with-query
  "Selects the Thanos Query service and runs the collection over a
   port-forward to it, handing it the forwarded base URL as :query-url."
  [opts run]
  (let [context (kubectl/current-context)
        services (delay (kubectl/get-selected context "services" (:selector opts)))
        target (select/pick (assoc service-select
                                   :candidates (fn [_] (kubectl/namespaces-names @services))
                                   :empty-message (format "no service matches %s - check --selector" (:selector opts)))
                            opts context)]
    (if (nil? target)
      false
      (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
            remote-port (kubectl/service-port-or-first (kubectl/find-item @services target) (:port-name opts))
            local-port (:port opts)]
        (kubectl/with-port-forward {:context context
                                    :namespace namespace
                                    :service name
                                    :local-port local-port
                                    :remote-port remote-port}
          (fn [] (run (assoc opts :query-url (str "http://localhost:" local-port)))))))))

(detective/book
 {:description "Investigates Thanos Query over a port-forward: readiness, stores, rules"
  :options options
  :prerequisites prerequisites
  :detectives thanos-detectives/detectives
  :when-to-run "Reach for this when long-range queries or recording rules misbehave - Thanos query readiness, its store endpoints, and rule health."
  :around with-query
  :collect (fn [opts] (thanos/collect-evidence! (:query-url opts)))})
