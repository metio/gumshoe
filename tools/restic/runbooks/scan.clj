;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.restic
  "Investigates restic backups: every repository must be readable, hold
   snapshots, and have a recent backup for each of its targets. The
   repository password comes from gopass or the RESTIC_PASSWORD environment."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.restic :as restic-detectives]
            [gumshoe.secrets :as secrets]
            [gumshoe.restic :as restic]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

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

(defn- collect
  "Every repository's evidence, or nil when the thresholds or the password
   rule the read out."
  [opts]
  (let [password (resolve-password (:password-secret opts))]
    (cond
      (>= (:warn-days opts) (:critical-days opts))
      (do (stdout/error "--warn-days must be smaller than --critical-days") nil)

      (empty? password)
      (do (stdout/error "no repository password: pass --password-secret or set RESTIC_PASSWORD") nil)

      :else
      (restic/collect-evidence! {:repositories (:repository opts)
                                 :password password
                                 :warn-days (:warn-days opts)
                                 :critical-days (:critical-days opts)}))))

(detective/book
 {:description "Investigates restic backups: repositories readable, populated, and current"
  :options options
  :prerequisites prerequisites
  :detectives restic-detectives/detectives
  :when-to-run "Reach for this when you need to trust the backups - that each restic repository is readable, actually populated, and has a recent enough snapshot."
  :collect collect})
