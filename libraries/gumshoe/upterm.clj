;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.upterm
  "Sharing a live terminal with colleagues via upterm. When a read-only book
   surfaces a problem, it can offer to open a shared session so the team jumps
   in together - upterm prints an SSH join command that others use to attach
   instantly.

   upterm itself lets anyone holding a session's connect string join unless
   the host names the keys a guest must have. A guest types into a shell that
   holds the operator's kube and SSH credentials, and a connect string leaks
   easily (a chat paste, a screenshot, a log), so we only host a session
   restricted to named users. An open session needs the operator to ask for
   it explicitly."
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

(defn authorized-user
  "The --authorized-user value for one name: a bare name is a GitHub user, and
   a value naming its provider (gitlab:bob, gitea:carol@git.example.com, an
   https:// URL) is passed through as upterm reads it."
  [user]
  (let [user (str/trim (str user))]
    (if (or (str/includes? user ":") (str/starts-with? user "https://"))
      user
      (str "github:" user))))

(defn host-plan
  "Pure: how to host a session for `users` (names allowed to join), or why
   not. {:args [..]} restricts joins to those users' keys; with no users the
   session is refused unless `open?` - the operator's explicit request for a
   session anyone with the connect string can join - which yields
   {:args [..] :open? true}."
  ([users] (host-plan users false))
  ([users open?]
   (let [users (->> users (map #(str/trim (str %))) (remove str/blank?) distinct)]
     (cond
       (seq users)
       {:args (into ["upterm" "host"]
                    (mapcat (fn [user] ["--authorized-user" (authorized-user user)]) users))}

       open?
       {:args ["upterm" "host"] :open? true}

       :else
       {:refused (str "no user is allowed to join - name each with --with <github-user>, "
                      "or pass --open for a session anyone holding its connect string can join")}))))

(defn host-args
  "The upterm host command for `users`, or nil when host-plan refuses it."
  ([users] (host-args users false))
  ([users open?] (:args (host-plan users open?))))

(defn start!
  "Opens a shared session in the foreground and blocks until the operator ends
   it by exiting the shell. Returns true on a clean exit, false when the session
   is refused or upterm fails."
  ([users] (start! users false))
  ([users open?]
   (let [{:keys [args refused] :as plan} (host-plan users open?)]
     (if refused
       (do (stdout/error (str "refusing to share this terminal: " refused))
           false)
       (do
         (stdout/print-section "🔗 Shared terminal (upterm)")
         (when (:open? plan)
           (stdout/warn "this session is OPEN: anyone holding its connect string can join and type into a shell with your credentials"))
         (stdout/err-println "upterm prints an 'SSH Session:' line below - share it with your team and")
         (stdout/err-println "they join instantly. Exit the shell (Ctrl-D) to end the session.")
         (stdout/print-section-marker)
         (zero? (apply shell/run-with-output args)))))))

(def offer-command
  "The command offer! suggests: restricted to a named user, so pasting it
   never opens a session anyone can join."
  (str/join " " (host-args ["<github-user>"])))

(defn offer!
  "Prints how to open a shared session for the team - a hint, not a prompt, so
   it never interrupts. Only when upterm is installed and on a real terminal."
  []
  (when (and (available?) (interactive?))
    (stdout/print-section-marker)
    (stdout/err-println (format "🔗 Investigate together: run `%s` to open a shared session the named colleague can join"
                                offer-command))))
