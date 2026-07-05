;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.pad
  "Rendering a scan's findings as Markdown and creating a HedgeDoc pad from them.
   These are the building blocks the built-in summary providers use (see
   infra.summary) - the clipboard provider pastes the Markdown, the HedgeDoc
   provider posts it as a note."
  (:require [babashka.http-client :as http]
            [clojure.string :as str]))

(def ^:private severity-order {:critical 0 :warning 1 :info 2})
(def ^:private severity-icon {:critical "🔥" :warning "🔶" :info "💡"})

(defn findings->markdown
  "Pure: a Markdown document from a scan's findings, grouped by area (the
   detective that raised them, most-severe area first), ready to paste into a
   shared pad or a ticket."
  [title meta findings]
  (let [by-area (->> findings
                     (group-by :detective)
                     (sort-by (fn [[_ fs]] (apply min (map (comp severity-order :severity) fs)))))
        counts (frequencies (map :severity findings))
        subtitle (str/join " · " (remove str/blank? [(str (:cluster meta)) (str (:when meta))]))]
    (str/join
     "\n"
     (concat
      [(str "# " title) ""]
      (when (seq subtitle) [(str "_" subtitle "_") ""])
      (mapcat (fn [[area fs]]
                (concat
                 [(format "## %s (%d)" area (count fs))]
                 (for [{:keys [severity component summary hint]}
                       (sort-by (juxt (comp severity-order :severity) :component) fs)]
                   (str "- " (get severity-icon severity "•") " **" component "** — " summary
                        (when-not (str/blank? (str hint)) (str "  \n  _" hint "_"))))
                 [""]))
              by-area)
      [(format "**%d critical, %d warning, %d info**"
               (get counts :critical 0) (get counts :warning 0) (get counts :info 0))]))))

(defn create-note!
  "Creates a HedgeDoc note from Markdown and returns its URL, or nil. HedgeDoc's
   POST /new answers 302 to the new note, so the redirect is read rather than
   followed. Never throws."
  [hedgedoc-url markdown]
  (try
    (let [response (http/post (str hedgedoc-url "/new")
                              {:headers {"Content-Type" "text/markdown"}
                               :body markdown
                               :throw false
                               :follow-redirects :never
                               :timeout 10000})
          location (or (get-in response [:headers "location"])
                       (get-in response [:headers "Location"]))]
      (when-not (str/blank? (str location))
        (if (str/starts-with? location "http") location (str hedgedoc-url location))))
    (catch Exception _ nil)))
