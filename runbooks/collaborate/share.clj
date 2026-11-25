;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.collaborate.share
  "Opens a shared upterm terminal so colleagues can join and investigate with
   you. upterm prints an SSH join command; share it and they attach instantly.
   Only the users named with --with can join. Without one the book refuses,
   unless --open asks for a session anyone holding its connect string can join."
  (:require [gumshoe.runbook :as runbook]
            [gumshoe.upterm :as upterm]))

(def options
  {:with {:desc "A user allowed to join, repeatable - a GitHub username, or provider:name (gitlab:bob)"
          :alias :w
          :coerce [:string]}
   :open {:desc "Let anyone holding the connect string join - refused without it when no --with is given"
          :coerce :boolean}})

(def prerequisites
  {:installed-tools ["upterm"]})

(defn- share
  [opts _ctx]
  (upterm/start! (or (:with opts) []) (boolean (:open opts))))

(runbook/execute!
 {:description "Opens a shared upterm terminal so colleagues can join and investigate"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action share})
