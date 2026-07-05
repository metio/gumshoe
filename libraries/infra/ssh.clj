;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.ssh
  "Running commands on remote hosts over SSH. Unattended by design: BatchMode
   with a short connect timeout, so a book fails fast instead of hanging on a
   password prompt. Works against any distribution - all it needs is ssh on
   the far end (and sudo only for the books that ask for it)."
  (:require [infra.shell :as shell]
            [infra.stdout :as stdout]))

(def ^:private options
  ["-o" "BatchMode=yes" "-o" "ConnectTimeout=5"])

(defn target
  [{:keys [host user]}]
  (if user (str user "@" host) host))

(defn ssh-args
  "Pure assembly of the full ssh command. The option terminator '--' precedes
   the destination: after the destination every token is the remote command,
   so a '--' there would be sent to the remote shell instead of ending ssh's
   own options."
  [connection command-args]
  (vec (concat ["ssh" "-q"] options ["--" (target connection)] command-args)))

(defn connects?
  [connection]
  (zero? (apply shell/exit-code-of (ssh-args connection ["exit" "0"]))))

(defn can-sudo?
  "Checks passwordless sudo without ever prompting - a password prompt under
   BatchMode would just hang or fail, so -n (non-interactive) is the only
   safe probe."
  [connection]
  (zero? (apply shell/exit-code-of (ssh-args connection ["sudo" "-n" "true"]))))

(defn check-connection?
  "Prints the connectivity check, and the sudo check only when the connection
   declares it needs sudo (:needs-sudo? true). Returns true when the host is
   usable for what the book will do."
  [connection]
  (let [connected (connects? connection)]
    (if connected
      (stdout/check-ok "can connect to" (target connection))
      (stdout/check-error "can not connect to" (target connection)))
    (cond
      (not connected) false
      (not (:needs-sudo? connection)) true
      :else (let [sudo (can-sudo? connection)]
              (if sudo
                (stdout/check-ok "can use sudo on" (target connection))
                (stdout/check-error "can not use sudo on" (target connection)))
              sudo))))

(defn stdout-of
  [connection & command]
  (apply shell/stdout-of (ssh-args connection command)))

(defn stream!
  "Runs a remote command streaming its output. Returns true on a clean exit."
  [connection & command]
  (zero? (apply shell/run-with-output (ssh-args connection command))))
