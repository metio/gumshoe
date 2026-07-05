;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.upterm
  "Sharing a live terminal with colleagues via upterm. When a read-only book
   surfaces a problem, it can offer to open a shared session so the team jumps
   in together - upterm prints an SSH join command that others use to attach
   instantly. Access is key-gated (and optionally restricted to named GitHub
   users), so a shared session is never wide open."
  (:require [clojure.string :as str]
            [gumshoe.command :as command]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(defn available?
  []
  (command/installed? "upterm"))

(defn interactive?
  "True only on a real terminal - never in a pipe or a cron job, so a shared
   session is never started unattended."
  []
  (some? (System/console)))

(defn host-args
  "Pure assembly of the upterm host command. Named GitHub users, when given,
   restrict who may join."
  [github-users]
  (into ["upterm" "host"]
        (mapcat (fn [user] ["--github-user" user]) github-users)))

(defn start!
  "Opens a shared session in the foreground and blocks until the operator ends
   it by exiting the shell. Returns true on a clean exit."
  [github-users]
  (stdout/print-section "🔗 Shared terminal (upterm)")
  (stdout/err-println "upterm prints an 'SSH Session:' line below - share it with your team and")
  (stdout/err-println "they join instantly. Exit the shell (Ctrl-D) to end the session.")
  (stdout/print-section-marker)
  (zero? (apply shell/run-with-output (host-args github-users))))

(defn offer!
  "Prints how to open a shared session for the team - a hint, not a prompt, so
   it never interrupts. Only when upterm is installed and on a real terminal."
  []
  (when (and (available?) (interactive?))
    (stdout/print-section-marker)
    (stdout/err-println (format "🔗 Investigate together: run `%s` to open a shared session others can join"
                                (str/join " " (host-args []))))))
