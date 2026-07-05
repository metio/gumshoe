;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.thanos
  "Investigates Thanos Query over a port-forward: readiness, connected store
   endpoints, and rule evaluation health."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.thanos :as thanos-detectives]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]
            [gumshoe.thanos :as thanos]))

(def options
  (merge {:namespace {:desc "The namespace of the Thanos Query service - interactive selection when omitted"
                      :alias :n
                      :coerce :string}
          :service {:desc "The Thanos Query service - interactive selection when omitted"
                    :alias :m
                    :coerce :string}
          :selector {:desc "Label selector used to find Thanos Query services"
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

(defn- investigate
  [opts _ctx]
  (detective/when-to-run! "Reach for this when long-range queries or recording rules misbehave - Thanos query readiness, its store endpoints, and rule health.")
  (let [context (kubectl/current-context)
        services (kubectl/get-selected context "services" (:selector opts))
        target (interact/choose-namespaced "Thanos Query" (kubectl/namespaces-names services)
                                           (:namespace opts) (:service opts))]
    (if (nil? target)
      (do (stdout/error "no Thanos Query service selected - check --selector") false)
      (let [{:keys [namespace name]} (kubectl/split-namespace-name target)
            remote-port (kubectl/service-port-or-first (kubectl/find-item services target) (:port-name opts))
            local-port (:port opts)]
        (kubectl/with-port-forward {:context context
                                    :namespace namespace
                                    :service name
                                    :local-port local-port
                                    :remote-port remote-port}
          (fn []
            (detective/report!
             thanos-detectives/detectives
             (detective/run-detectives thanos-detectives/detectives
                                       (thanos/collect-evidence! (str "http://localhost:" local-port)))
             (:output opts "text"))))))))

(runbook/execute!
 {:description "Investigates Thanos Query over a port-forward: readiness, stores, rules"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action investigate})
