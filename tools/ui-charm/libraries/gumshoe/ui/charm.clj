;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ui.charm
  "A babashka-native UI backend built on charm.clj, registered through the :ui
   seam. Selecting it (env.edn :ui :charm) makes gumshoe pick, prompt, and confirm
   with an in-process TUI instead of shelling out to fzf and gum - dropping both
   binary dependencies. charm.clj follows the Elm architecture (init / update /
   view); each primitive here runs a tiny program and reads the result out of the
   final state.

   The programs' states, their update functions and the values read out of a
   final state are pure, so they are tested without a terminal; only
   `run-until-enter` needs one."
  (:require [charm.components.list :as clist]
            [charm.components.text-input :as text-input]
            [charm.message :as msg]
            [charm.program :as program]
            [clojure.string :as str]
            [gumshoe.plugin :as plugin]))

;; --- the Elm loop, shared by every primitive ---

(def cancelled ::cancelled)

(defn cancel?
  "Whether the message aborts the prompt: Esc or Ctrl+C."
  [message]
  (boolean (or (msg/key-match? message :escape) (msg/key-match? message "ctrl+c"))))

(defn step
  "One turn of a primitive's program: Enter commits, Esc and Ctrl+C cancel,
   and every other keypress goes to `component-update`, which takes the state
   and the message and returns the new state. Returns [state command]; a
   cancelled state is marked ::cancelled, so a caller can tell an empty pick
   from an abort."
  [component-update state message]
  (cond
    (msg/key-match? message :enter) [state program/quit-cmd]
    (cancel? message) [(assoc state cancelled true) program/quit-cmd]
    (msg/key-press? message) [(component-update state message) nil]
    :else [state nil]))

(defn- run-until-enter
  "Runs a component program until Enter or a cancel and returns the final state."
  [initial component-update view]
  (program/run
   {:init initial
    :update (partial step component-update)
    :view view}))

(defn- header [prompt body] (str prompt "\n" body))

;; --- one value from a list ---

(defn list-state
  "The state of a pick from `values`, the cursor on the value at `cursor`."
  ([values] (list-state values 0))
  ([values cursor] (clist/item-list (vec values) :cursor cursor)))

(defn list-update
  "The list after a keypress; charm's list-update also returns a command,
   which navigation never needs."
  [state message]
  (first (clist/list-update state message)))

(defn picked
  "The value a final list state holds, nil when the pick was cancelled."
  [state]
  (when-not (get state cancelled)
    (clist/selected-item state)))

(defn select-one
  "Pick one value from a list. A single candidate is returned without prompting,
   matching the built-in fzf behaviour; Esc returns nil."
  [prompt values _query]
  (if (= 1 (count values))
    (first values)
    (picked (run-until-enter (list-state values)
                             list-update
                             #(header prompt (clist/list-view %))))))

;; --- several values from a list ---

(def chosen ::chosen)

(defn multi-state
  "The state of a pick of several from `values`: a list and the indexes
   chosen so far, none at first."
  [values]
  (assoc (list-state values) chosen (sorted-set)))

(defn- toggle [indexes index]
  (if (contains? indexes index) (disj indexes index) (conj indexes index)))

(defn- space?
  "Whether message is a Space keypress. charm's input reader maps only bytes
   0-31 and 127 to named keys, so the Space a terminal sends (byte 32) arrives
   as the key \" \"; :space is accepted too, should a charm release name it."
  [message]
  (or (msg/key-match? message " ") (msg/key-match? message :space)))

(defn multi-update
  "Space toggles the value under the cursor; every other key moves the list."
  [state message]
  (if (space? message)
    (update state chosen toggle (clist/selected-index state))
    (list-update state message)))

(defn multi-view
  "The list with a mark before every value: [x] chosen, [ ] not."
  [state]
  (let [marks (map-indexed (fn [i item] (str (if (contains? (get state chosen) i) "[x] " "[ ] ") item))
                           (clist/items state))]
    (clist/list-view (clist/set-items state marks))))

(defn picked-many
  "The values a final multi state chose, in list order, or nil when the pick
   was cancelled."
  [state]
  (when-not (get state cancelled)
    (mapv #(get (clist/items state) %) (get state chosen))))

(defn select-multi
  "Pick several values, Space marking each. Returns a vector, or nil on cancel."
  [prompt values]
  (picked-many (run-until-enter (multi-state values)
                                multi-update
                                #(header (str prompt " (space marks, enter accepts)") (multi-view %)))))

;; --- free text ---

(defn text-state
  "The state of a free-text prompt: an empty, focused input."
  []
  (text-input/text-input))

(defn text-update
  "The text input after a keypress."
  [state message]
  (first (text-input/text-input-update state message)))

(defn entered
  "The text a final input state holds, nil when it is empty or cancelled."
  [state]
  (when-not (get state cancelled)
    (not-empty (text-input/value state))))

(defn ask-text
  "Prompt for free text. Returns the entered string, or nil when empty/cancelled."
  [prompt]
  (entered (run-until-enter (text-state)
                            text-update
                            #(header prompt (text-input/text-input-view %)))))

;; --- yes or no ---

(def confirm-choices
  "The confirmation's choices, \"no\" first so the cursor starts on it and a
   bare Enter declines, as `gum confirm --default=false` does."
  ["no" "yes"])

(defn confirmed?
  "Whether a final confirmation state approves: only an explicit yes does."
  [state]
  (= "yes" (picked state)))

(defn option-choices
  "Pure: the choices of a confirmation given gumshoe's options map, the
   negative first so the cursor starts on it, or nil when either label is
   blank or both read the same, since the operator could then not tell which
   one approves."
  [{:keys [affirmative negative]}]
  (let [yes (some-> affirmative str str/trim)
        no (some-> negative str str/trim)]
    (when (and (not (str/blank? yes)) (not (str/blank? no)) (not= yes no))
      [no yes])))

(defn chose-affirmative?
  "Whether a final confirmation state over `choices` approves: only the
   affirmative option, chosen explicitly, does."
  [choices state]
  (boolean (and choices (= (second choices) (picked state)))))

(defn confirm
  "A yes/no picker starting on no. Returns true only for an explicit yes.
   Given gumshoe's options map, it offers the map's negative and affirmative
   labels instead, still starting on the negative, and declines without
   asking when the labels cannot be told apart."
  ([]
   (confirmed? (run-until-enter (list-state confirm-choices)
                                list-update
                                #(header "Proceed?" (clist/list-view %)))))
  ([{:keys [prompt] :as options}]
   (if-let [choices (option-choices options)]
     (chose-affirmative? choices (run-until-enter (list-state choices)
                                                  list-update
                                                  #(header (or prompt "Proceed?") (clist/list-view %))))
     false)))

(plugin/provide!
 {:ui {:name :charm
       :select-one select-one
       :select-multi select-multi
       :ask-text ask-text
       :confirm confirm
       :confirm-options? true}})
