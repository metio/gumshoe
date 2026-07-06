;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.fuzzy-finder
  "Fuzzy selection via fzf: candidates go to stdin, the selection comes back on
   stdout, and fzf draws its interface on the terminal directly. A single
   candidate is picked automatically; ESC selects nothing."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [gumshoe.ui :as ui]))

(defn- select
  [args values]
  (let [fzf (process/process args)]
    (with-open [writer (io/writer (:in fzf))]
      (binding [*out* writer]
        (run! println values)))
    (not-empty (str/trim (slurp (:out fzf))))))

(def ^:private style
  ["--ansi" "--height=40%" "--reverse" "--cycle" "--border=rounded" "--info=inline"
   "--pointer=▶" "--marker=✓"
   "--color=pointer:red,marker:green,prompt:blue,border:8"])

(defn- single-select-args
  "The fzf argv for a single pick from an options map. `--select-1`/`--exit-0`
   (auto-accept a lone match, exit when none) apply only when :auto-select? is
   set: a caller resolving a resource name for a change turns it off, so a seeded
   query never auto-accepts a fuzzy match - the operator confirms the highlighted
   row by pressing enter, and a typo that matches nothing keeps the picker open to
   edit rather than resolving to the wrong resource. :preview is a shell command
   fzf runs for the highlighted row (its `{}` is the candidate), shown in a side
   pane - a read-only glance at what a pick means before committing to it."
  [prompt {:keys [query auto-select? preview] :or {auto-select? true}}]
  (concat ["fzf" "--no-multi" (str "--prompt=" prompt " ▶ ")]
          (when (not-empty query) [(str "--query=" query)])
          (when auto-select? ["--exit-0" "--select-1"])
          (when (not-empty preview)
            [(str "--preview=" preview) "--preview-window=right,50%,border-left,wrap"])
          style))

(defn select-single
  "Picks one value. The third argument may be a query string (prefills fzf's
   filter) or an options map for more control:
     {:query        seed the filter with what the operator already typed
      :auto-select? (default true) let a lone match resolve without a keypress;
                    false when a mistyped name must never resolve silently
      :preview      an fzf preview command ({} = the highlighted candidate),
                    e.g. \"kubectl describe node {}\" - a side-pane glance}
   The UI backend, when one is active, drives its own selection and ignores the
   fzf-only knobs (auto-select?/preview)."
  ([prompt values] (select-single prompt values {}))
  ([prompt values query-or-opts]
   (let [{:keys [query] :as opts} (if (map? query-or-opts)
                                    query-or-opts
                                    {:query query-or-opts})]
     (if-let [pick (ui/backend :select-one)]
       (pick prompt values query)
       (select (single-select-args prompt opts) values))))
  ([prompt values query auto-select?]
   (select-single prompt values {:query query :auto-select? auto-select?})))

(defn select-multi
  [prompt values]
  (if-let [pick (ui/backend :select-multi)]
    (pick prompt values)
    (when-let [selection (select (concat ["fzf" "--exit-0" "--select-1" "--multi"
                                          "--bind=ctrl-a:select-all"
                                          "--header=TAB selects multiple entries, CTRL-A selects all"
                                          (str "--prompt=" prompt " ▶ ")]
                                         style)
                                 values)]
      (str/split selection #"\n"))))
