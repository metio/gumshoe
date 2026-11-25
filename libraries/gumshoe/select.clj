;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.select
  "Choosing the subject of a book: run a candidate query, let the operator pick
   one (or several), and hand back the choice. gumshoe.mutation composes this into
   its confirmed/announced/verified flow, but the same picker serves a read-only
   or bespoke book that just needs 'let the operator choose a namespace' - without
   re-implementing the candidate/empty/nothing-picked dance every time.

   A select spec (the same map mutation/book takes as :select):
     {:mode :one | :many | :namespaced
      :label \"Node\"
      :candidates (fn [context] -> seq of names)
      :flag :node                      ; :one / :many - the opts key holding the pick
      :namespace-flag :namespace       ; :namespaced
      :name-flag :name                 ; :namespaced
      :preview \"kubectl describe node {}\"  ; :one, optional fzf side pane
      :empty-message \"every node is already cordoned\"  ; shown by pick when empty
      :on-empty :ok | :fail}           ; how pick shows it, :ok by default

   `options` derives the command-line options a select reads from the spec:
   each flag with the short alias gumshoe.flags gives it, the coercion its
   mode needs and a description from the :label. A book names only what it
   wants to say differently:

     :options (select/options select {:pick {:desc \"The workload to scale\"}
                                      :replicas {:desc \"The replica count\" :alias :r :coerce :long}})

   An explicit :alias on a select's flag overrides the convention. gumshoe's
   own books never set one, so each select flag has one alias in all of them."
  (:require [clojure.string :as str]
            [gumshoe.flags :as flags]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.stdout :as stdout]))

(defn nothing-selected?
  "Whether the selection is empty for the mode - nil for a single pick, an empty
   seq for a multi pick."
  [mode target]
  (if (= :many mode)
    (empty? target)
    (nil? target)))

(defn items
  "The confirmation items for a selection: the picks as a vector."
  [mode target]
  (if (= :many mode) (vec target) [target]))

(defn- list-candidates
  "{:names [...]} or {:unread <reason>}. Candidates are read under strict
   reads and realized inside the binding, so a lazy seq cannot run its reads
   after it, and an unanswered read surfaces as its message instead of as an
   empty list."
  [candidates context]
  (try
    {:names (binding [kubectl/*strict-reads* true] (into [] (candidates context)))}
    (catch clojure.lang.ExceptionInfo e
      (if (::kubectl/unread (ex-data e)) {:unread (ex-message e)} (throw e)))))

(defn unread!
  "Reports candidates that could not be listed; false, for a run's result."
  [{:keys [label]} reason]
  (stdout/error (format "could not list the %s candidates - %s" label reason))
  false)

(defn resolve-target
  "Runs the candidate query and the interactive pick for a select spec, returning
   {:candidates <names> :target <the pick, or nil/[] when nothing was chosen>},
   or {:unread <reason>} when a read behind the candidates did not answer.
   The lowest-level primitive - use it when you need the raw candidate list too
   (e.g. to distinguish 'nothing to pick' from 'nothing picked' yourself)."
  [{:keys [mode label candidates flag namespace-flag name-flag preview]} opts context]
  (let [{:keys [names unread]} (list-candidates candidates context)]
    (if unread
      {:unread unread}
      {:candidates names
     :target (case mode
               :one (interact/choose-one label names (get opts flag) preview)
               :many (interact/choose-many label names (get opts flag))
               :namespaced (interact/choose-namespaced label names
                                                       (get opts namespace-flag)
                                                       (get opts name-flag)))})))

(defn pick
  "Discovers candidates for a select spec and lets the operator choose, returning
   the chosen target or nil. Prints why it returns nil: that the candidates could
   not be listed, the spec's :empty-message when there is nothing to pick, or an
   error when the pick is empty - so a read-only or bespoke book selects its
   subject in one line:

     (if-let [ns (select/pick namespace-select opts context)]
       ...
       false)

   The :empty-message prints as a success unless the spec says :on-empty :fail,
   which suits a book that cannot do its work without a subject.

   A book that needs the objects behind the names reads them once in a delay
   the :candidates function derefs: pick realizes it under strict reads, and
   the book derefs it again after the pick without a second read.

   mutation/book uses resolve-target instead, because it maps empty candidates to
   its own success (nothing-to-do) rather than to nil."
  [{:keys [mode label empty-message on-empty] :as spec} opts context]
  (let [{:keys [candidates target unread]} (resolve-target spec opts context)
        empty! (if (= :fail on-empty) stdout/error stdout/ok)]
    (cond
      unread (do (unread! spec unread) nil)
      (empty? candidates) (do (empty! (or empty-message "nothing to select")) nil)
      (nothing-selected? mode target) (do (stdout/error (format "no %s selected" label)) nil)
      :else target)))

;; --- the options a select reads ---------------------------------------------------

(defn- flag-option
  [mode flag desc]
  (cond-> {:desc desc
           :coerce (if (= :many mode) [:string] :string)}
    (flags/aliases flag) (assoc :alias (flags/aliases flag))))

(defn derived-options
  "The option of each flag the select reads, as the convention gives it."
  [{:keys [mode label flag namespace-flag name-flag]}]
  (case mode
    :one {flag (flag-option mode flag (format "The %s - interactive selection when omitted" label))}
    :many {flag (flag-option mode flag (format "%s to pick, repeatable - interactive selection when omitted" label))}
    :namespaced {namespace-flag (flag-option mode namespace-flag
                                             (format "The namespace of the %s - interactive selection when omitted" label))
                 name-flag (flag-option mode name-flag
                                        (format "The name of the %s - interactive selection when omitted" label))}))

(defn options
  "The select's derived options with `explicit` merged over them, key by key
   within each option, so a book can reword a flag's :desc and keep its
   alias, or add options of its own. Throws when two options end up with
   one alias, because babashka.cli would silently give the letter to one of
   them."
  ([select] (options select {}))
  ([select explicit]
   (let [merged (merge-with merge (derived-options select) explicit)
         shared (for [[letter holders] (group-by (comp :alias val) merged)
                      :when (and letter (< 1 (count holders)))]
                  [letter (sort (map key holders))])]
     (when (seq shared)
       (throw (ex-info (str "options share an alias: "
                            (str/join "; " (for [[letter holders] shared]
                                             (str "-" (name letter) " for "
                                                  (str/join " and " (map #(str "--" (name %)) holders))))))
                       {:shared (into {} shared)})))
     merged)))
