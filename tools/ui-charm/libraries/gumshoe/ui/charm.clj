;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ui.charm
  "A babashka-native UI backend built on charm.clj, registered through the :ui
   seam. Selecting it (env.edn :ui :charm) makes gumshoe pick, prompt, and confirm
   with an in-process TUI instead of shelling out to fzf and gum - dropping both
   binary dependencies. charm.clj follows the Elm architecture (init / update /
   view); each primitive here runs a tiny program and reads the result out of the
   final state.

   VERIFY ON A REAL TERMINAL: this backend can not be exercised by the headless
   test suite (a TUI needs a tty and charm.clj on the classpath). The Elm loops
   below follow charm.clj's documented API; the two accessors that read a value
   out of a component's final state - `selected-item` and `input-value` - are
   isolated at the top of this namespace so they are a one-line fix if the
   installed charm.clj names them differently."
  (:require [charm.components.list :as clist]
            [charm.components.text-input :as text-input]
            [charm.message :as msg]
            [charm.program :as program]
            [gumshoe.plugin :as plugin]))

;; --- the two accessors to confirm against the installed charm.clj ---

(defn- selected-item
  "The item the operator highlighted in a list component's final state."
  [state]
  (clist/selected-item state))

(defn- input-value
  "The text entered in a text-input component's final state."
  [state]
  (text-input/value state))

;; --- the Elm loop, shared by every primitive ---

(def ^:private cancelled ::cancelled)

(defn- run-until-enter
  "Runs a component program until Enter (commit) or Esc (cancel), forwarding every
   other keypress to the component's own update. Returns the final state, marked
   ::cancelled on Esc so a caller can tell an empty pick from an abort."
  [initial component-update view]
  (program/run
   {:init initial
    :update (fn [state message]
              (cond
                (msg/key-match? message "enter") [state program/quit-cmd]
                (msg/key-match? message "esc") [(assoc state cancelled true) program/quit-cmd]
                (msg/key-press? message) [(component-update state message) nil]
                :else [state nil]))
    :view view}))

(defn- header [prompt body] (str prompt "\n" body))

;; --- the four :ui primitives ---

(defn select-one
  "Pick one value from a list. A single candidate is returned without prompting,
   matching the built-in fzf behaviour; Esc returns nil."
  [prompt values _query]
  (if (= 1 (count values))
    (first values)
    (let [final (run-until-enter (clist/init {:items (vec values)})
                                 clist/update
                                 #(header prompt (clist/view %)))]
      (when-not (get final cancelled)
        (selected-item final)))))

(defn select-multi
  "Pick several values. Returns a vector, or nil on cancel. Relies on the list
   component's multi-select; if the installed charm.clj exposes it differently,
   this is where to adapt."
  [prompt values]
  (let [final (run-until-enter (clist/init {:items (vec values) :multi true})
                               clist/update
                               #(header prompt (clist/view %)))]
    (when-not (get final cancelled)
      (vec (clist/selected-items final)))))

(defn ask-text
  "Prompt for free text. Returns the entered string, or nil when empty/cancelled."
  [prompt]
  (let [final (run-until-enter (text-input/init)
                               text-input/update
                               #(header prompt (text-input/view %)))]
    (when-not (get final cancelled)
      (not-empty (input-value final)))))

(defn confirm
  "A yes/no picker. Returns true only for an explicit yes."
  []
  (= "yes" (select-one "Proceed?" ["yes" "no"] nil)))

(plugin/provide!
 {:ui {:name :charm
       :select-one select-one
       :select-multi select-multi
       :ask-text ask-text
       :confirm confirm}})
