;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.flows
  "Investigates Calico flow logs through Whisker: what was denied, which policy
   decided it, what a staged policy would decide differently, and which denials
   reach the client as a timeout."
  (:require [gumshoe.calico.flows :as flows]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.flows :as flow-detectives]
            [gumshoe.inputs :as inputs]
            [gumshoe.runbook :as runbook]
            [gumshoe.secrets :as secrets]))

(def options
  (merge {:whisker {:desc "Whisker base URL, e.g. https://whisker.example.org - overrides env.edn"
                    :alias :w
                    :coerce :string}
          :secret {:desc "Password-store path holding the Whisker credentials (password plus a login field)"
                   :alias :k
                   :coerce :string}
          :user {:desc "Whisker basic-auth user, when the secret does not carry a login field"
                 :alias :u
                 :coerce :string}
          :window {:desc "Window to read, in seconds"
                   :alias :t
                   :default 3600
                   :coerce :long}}
         detective/output-option))

(defn- credentials
  "The Whisker user and password from the password store. The htpasswd user is
   usually a generated string, so it comes from a named field beside the password
   (`login`, else `user`, else `username`) rather than being typed; --user
   overrides it for a secret that carries none."
  [opts url]
  (when-let [secret (or (:secret opts) (secrets/find-path url))]
    {:user (or (:user opts)
               (some #(secrets/field secret %) ["login" "user" "username"]))
     :password (secrets/password secret)}))

(defn- investigate
  [opts _ctx]
  (detective/when-to-run! "Reach for this when traffic is being dropped and you want the verdict rather than another probe - which policy denied a path, what a staged policy would change, and which denials look like timeouts. Also run it before enforcing a staged default-deny.")
  (let [url (inputs/value :whisker-url opts)
        {:keys [user password]} (credentials opts url)]
    (detective/report!
     flow-detectives/detectives
     (detective/run-detectives flow-detectives/detectives
                               (flows/collect-evidence! {:url url
                                                         :user user
                                                         :password password
                                                         :window (:window opts 3600)}))
     (:output opts "text"))))

(runbook/execute!
 {:description "Investigates Calico flow logs through Whisker: denials, the deciding policy, staged disagreement"
  :options options
  :announce? false
  :action investigate})
