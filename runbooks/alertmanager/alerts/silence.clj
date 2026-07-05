;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.alertmanager.alerts.silence
  "Silences alerts matching a set of label matchers on one alertmanager."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [infra.config :as config]
            [infra.effect :as effect]
            [infra.flow :as flow]
            [infra.secrets :as secrets]
            [infra.announce :as announce]
            [infra.runbook :as runbook]
            [infra.shell :as shell]
            [infra.stdout :as stdout]))

(def options
  {:alertmanager {:desc "The alertmanager host (e.g. alertmanager.example.org); defaults to env.edn :alertmanager :host"
                  :alias :a
                  :default (config/env-value {} [:alertmanager :host])
                  :coerce :string}
   :duration {:desc "The duration of the silence"
              :alias :d
              :default "2h"
              :coerce :string}
   :comment {:desc "The comment for the silence"
             :alias :c
             :default "no comment"
             :coerce :string}
   :matchers {:desc "The labels to match (key=value), repeatable"
              :alias :m
              :require true
              :coerce [:string]}})

(def prerequisites
  {:installed-tools ["amtool"]
   :can-ping-using-ipv4 [:alertmanager]
   :access-gopass-secrets [:alertmanager]})

(defn- with-amtool-config
  "Runs f with the credentials in a short-lived owner-only http.config file,
   so the password never shows up in process arguments."
  [alertmanager f]
  (let [secret (secrets/find-path alertmanager)
        username (secrets/field secret "login")
        password (secrets/password secret)]
    (fs/with-temp-dir [directory {:posix-file-permissions "rwx------"}]
      (let [config (str (fs/path directory "http.yaml"))]
        (spit config (format "basic_auth:\n  username: %s\n  password: %s\n"
                             (pr-str username) (pr-str password)))
        (f config)))))

(defn- amtool-args
  [alertmanager config]
  ["amtool" "--no-version-check"
   (str "--alertmanager.url=https://" alertmanager)
   (str "--http.config.file=" config)])

(defn add-silence-effect
  "The plan that adds the silence. The credentials live in the http.config
   file, never in these arguments, so the plan is safe to describe and record."
  [alertmanager config opts]
  (let [author (shell/stdout-of "whoami")
        start (shell/stdout-of "date" "--iso-8601=seconds" "--utc")]
    (effect/plan
     (apply effect/cmd
            (concat (amtool-args alertmanager config)
                    ["silence" "add"
                     (str "--start=" start)
                     (str "--duration=" (:duration opts))
                     (str "--comment=" (:comment opts))
                     (str "--author=" author)]
                    (:matchers opts))))))

(defn- silence-active?
  [alertmanager config matchers]
  (not (str/blank? (apply shell/stdout-of
                          (concat (amtool-args alertmanager config)
                                  ["silence" "query" "--quiet"]
                                  matchers)))))

(defn- silence
  [opts {:keys [announcement-data]}]
  (let [alertmanager (:alertmanager opts)
        matchers (:matchers opts)]
    (with-amtool-config alertmanager
      (fn [config]
        (flow/change!
         {:confirmation {:action (format "silence alerts for %s" (:duration opts))
                         :target alertmanager
                         :items matchers}
          :announce! #(announce/announce! alertmanager announcement-data
                                                            (format "Alerts matching [%s] silenced for %s"
                                                                    (str/join ", " matchers)
                                                                    (:duration opts)))
          :effect (add-silence-effect alertmanager config opts)
          :post-checks [{:description (format "a silence matching [%s] is active" (str/join ", " matchers))
                         :timeout 30
                         :check #(silence-active? alertmanager config matchers)}]})))))

(runbook/execute!
 {:description "Silences alerts matching a set of label matchers on one alertmanager"
  :options options
  :prerequisites prerequisites
  :action silence})
