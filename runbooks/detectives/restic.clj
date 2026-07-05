;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.restic
  "Investigates restic backups: every repository must be readable, hold
   snapshots, and have a recent backup for each of its targets. The
   repository password comes from gopass or the RESTIC_PASSWORD environment."
  (:require [infra.detective :as detective]
            [infra.detectives.restic :as restic-detectives]
            [infra.secrets :as secrets]
            [infra.restic :as restic]
            [infra.runbook :as runbook]
            [infra.shell :as shell]
            [infra.stdout :as stdout]))

(def options
  (merge {:repository {:desc "A restic repository to check, repeatable"
                       :alias :r
                       :require true
                       :coerce [:string]}
          :password-secret {:desc "gopass path to the repository password - RESTIC_PASSWORD env when omitted"
                            :alias :k
                            :coerce :string}
          :warn-days {:desc "Warn when the newest backup is older than this many days"
                      :default 2
                      :coerce :long}
          :critical-days {:desc "Critical when the newest backup is older than this many days"
                          :default 7
                          :coerce :long}}
         detective/output-option))

(def prerequisites
  {:installed-tools ["restic"]})

(defn- resolve-password
  [secret]
  (if secret
    (secrets/password secret)
    (System/getenv "RESTIC_PASSWORD")))

(defn- investigate
  [opts _ctx]
  (detective/when-to-run! "Reach for this when you need to trust the backups - that each restic repository is readable, actually populated, and has a recent enough snapshot.")
  (let [password (resolve-password (:password-secret opts))]
    (cond
      (>= (:warn-days opts) (:critical-days opts))
      (do (stdout/error "--warn-days must be smaller than --critical-days") false)

      (empty? password)
      (do (stdout/error "no repository password: pass --password-secret or set RESTIC_PASSWORD") false)

      :else
      (detective/report!
       restic-detectives/detectives
       (detective/run-detectives restic-detectives/detectives
                                 (restic/collect-evidence! {:repositories (:repository opts)
                                                            :password password
                                                            :warn-days (:warn-days opts)
                                                            :critical-days (:critical-days opts)}))
       (:output opts "text")))))

(runbook/execute!
 {:description "Investigates restic backups: repositories readable, populated, and current"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action investigate})
