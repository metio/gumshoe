;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.summary
  "Sharing what a read-only book found - a plugin seam. A summary provider takes
   the run's findings (as structured data and rendered Markdown, with cluster/date
   metadata) and sends them somewhere. The clipboard and a HedgeDoc pad are built
   in; a plugin registers more - Slack, a ticket, a file, a Matrix post - with
   `register-provider!`. When a scan finishes, gumshoe offers the providers usable
   here and runs the one you pick."
  (:require [gumshoe.clipboard :as clipboard]
            [gumshoe.config :as config]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.pad :as pad]
            [gumshoe.stdout :as stdout]))

(defonce ^:private providers (atom []))
(defonce ^:private captured (atom nil))

(defn register-provider!
  "Registers a way to share a run summary:
     {:name kw
      :label \"human label shown in the picker\"
      :available? (fn [] boolean)   ; is it usable here - a tool present, a URL set
      :provide!   (fn [summary] result)} ; share it; may return a location to show
   The summary passed to :provide! is {:title :findings :markdown :meta}. Later
   registrations are offered after earlier ones."
  [provider]
  (swap! providers conj provider))

(defn capture!
  "Records the current run's findings so the harness can offer to share them."
  [findings]
  (reset! captured (vec findings)))

(defn- summary-of
  [title findings]
  (let [meta {:cluster (try (kubectl/current-cluster) (catch Exception _ nil))
              :when (str (java.time.LocalDate/now))}]
    {:title title
     :findings findings
     :meta meta
     :markdown (pad/findings->markdown title meta findings)}))

(defn registered
  "Every registered provider, in registration order."
  []
  @providers)

(defn usable
  "The providers whose :available? holds - a failing check counts as unavailable,
   never breaks the offer."
  ([] (usable @providers))
  ([provs] (filter #(try ((:available? %)) (catch Exception _ false)) provs)))

(def ^:private skip "⏭  keep them in the terminal")

(defn offer!
  "When the run produced findings, offers the usable providers and runs the one
   picked. Best-effort - never throws, never blocks."
  [title]
  (when-let [findings (seq @captured)]
    (try
      (when-let [options (seq (usable))]
        (stdout/print-section-marker)
        (let [by-label (into {} (map (juxt :label identity) options))
              choice (fuzzy/select-single "share these findings?" (conj (mapv :label options) skip))]
          (when-let [provider (get by-label choice)]
            (let [where ((:provide! provider) (summary-of title findings))]
              (stdout/err-println (str "📝 " (or where "shared")))))))
      (catch Exception _ nil))))

;; --- built-in providers ----------------------------------------------------

(register-provider!
 {:name :clipboard
  :label "📋 copy to clipboard (paste into a pad or ticket)"
  :available? #(clipboard/available?)
  :provide! (fn [{:keys [markdown]}]
              (when-let [tool (clipboard/copy! markdown)]
                (format "copied as Markdown (%s)" tool)))})

(register-provider!
 {:name :hedgedoc
  :label "📝 create a HedgeDoc pad"
  :available? #(boolean (config/value [:hedgedoc :url]))
  :provide! (fn [{:keys [markdown]}]
              (when-let [url (pad/create-note! (config/value [:hedgedoc :url]) markdown)]
                (format "pad created - open it with the team: %s" url)))})
