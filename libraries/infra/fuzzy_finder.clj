;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.fuzzy-finder
  "Fuzzy selection via fzf: candidates go to stdin, the selection comes back on
   stdout, and fzf draws its interface on the terminal directly. A single
   candidate is picked automatically; ESC selects nothing."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defn select-single
  "Picks one value. An optional initial query prefills fzf's search box, so a
   launcher can seed the filter with whatever the operator already typed."
  ([prompt values] (select-single prompt values nil))
  ([prompt values query]
   (select (concat ["fzf" "--exit-0" "--select-1" "--no-multi"
                    (str "--prompt=" prompt " ▶ ")]
                   (when (not-empty query) [(str "--query=" query)])
                   style)
           values)))

(defn select-multi
  [prompt values]
  (when-let [selection (select (concat ["fzf" "--exit-0" "--select-1" "--multi"
                                        "--bind=ctrl-a:select-all"
                                        "--header=TAB selects multiple entries, CTRL-A selects all"
                                        (str "--prompt=" prompt " ▶ ")]
                                       style)
                               values)]
    (str/split selection #"\n")))
