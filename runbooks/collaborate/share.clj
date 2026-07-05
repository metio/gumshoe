;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.collaborate.share
  "Opens a shared upterm terminal so colleagues can join and investigate with
   you. upterm prints an SSH join command; share it and they attach instantly.
   Restrict who may join with --with, otherwise anyone with an authorized key
   can attach."
  (:require [gumshoe.runbook :as runbook]
            [gumshoe.upterm :as upterm]))

(def options
  {:with {:desc "A GitHub username allowed to join, repeatable - key-holders otherwise"
          :alias :w
          :coerce [:string]}})

(def prerequisites
  {:installed-tools ["upterm"]})

(defn- share
  [opts _ctx]
  (upterm/start! (or (:with opts) [])))

(runbook/execute!
 {:description "Opens a shared upterm terminal so colleagues can join and investigate"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action share})
