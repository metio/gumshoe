;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ssh
  "Running commands on remote hosts over SSH. Unattended by design: BatchMode
   with a short connect timeout, so a book fails fast instead of hanging on a
   password prompt. Works against any distribution - all it needs is ssh on
   the far end (and sudo only for the books that ask for it)."
  (:require [babashka.fs :as fs]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(def ^:private options
  ["-o" "BatchMode=yes" "-o" "ConnectTimeout=5"])

(def ^:dynamic *user-config*
  "Which ssh config to name with -F. ::probe looks for the invoking user's, nil
   names none, a string names that file. Bind it in tests so the assembled command
   does not depend on whether the machine running them has an ssh config."
  ::probe)

(defn- probe-user-config
  "The invoking user's ssh config, when it exists.

  Naming it with -F makes ssh ignore /etc/ssh/ssh_config and everything it
  includes, which is deliberate rather than incidental: ssh refuses a config file
  owned by neither root nor the caller, and inside a rootless user namespace - a
  nix-portable devshell, a container without a uid mapping - uid 0 is unmapped, so
  a perfectly correct system config resolves to an unknown owner and ssh aborts
  before it ever connects. The user's own config is owned by a mapped uid, so it
  stays readable everywhere.

  The cost is that site-wide settings are not consulted. Where a ProxyJump or an
  IdentityAgent lives in /etc/ssh/ssh_config rather than the user's config, copy
  it there."
  []
  (let [path (fs/path (System/getProperty "user.home") ".ssh" "config")]
    (when (fs/regular-file? path)
      (str path))))

(defn- config-arg
  []
  (when-let [config (if (= ::probe *user-config*) (probe-user-config) *user-config*)]
    ["-F" config]))

(defn target
  [{:keys [host user]}]
  (if user (str user "@" host) host))

(defn ssh-args
  "Pure assembly of the full ssh command. The option terminator '--' precedes
   the destination: after the destination every token is the remote command,
   so a '--' there would be sent to the remote shell instead of ending ssh's
   own options."
  [connection command-args]
  (vec (concat ["ssh" "-q"]
               (config-arg)
               options
               ["--" (target connection)]
               command-args)))

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
