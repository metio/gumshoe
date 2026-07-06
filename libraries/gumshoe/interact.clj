;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.interact
  "Interactive selection and the second-step verification for risky actions.

   Every runbook resolves its target the same way: a valid CLI flag value is
   used as-is, anything else falls back to fuzzy selection. Every change to a
   running system is confirmed by the user before it happens."
  (:require [clojure.string :as str]
            [gumshoe.command :as command]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.ui :as ui]))

;; Every resolved selection is recorded so a book's recording can show exactly
;; what the operator picked, whether by flag or interactively.
(def ^:private selections (atom []))

(defn recorded-selections
  "The label/value pairs chosen so far this run, in order."
  []
  @selections)

(defn- record-selection!
  [label value]
  (when (some? value)
    (swap! selections conj {:label label :value value}))
  value)

(defn ask-text
  "Prompts for a free-text value. The question is always printed to the terminal
   first, so it is visible no matter which input tool answers it - gum's input
   box (whose widget renders to stderr, which must stay on the terminal) when
   available, a plain readline otherwise. Returns nil for an empty answer. The
   value is recorded like any other selection."
  [label prompt]
  (binding [*out* *err*] (println prompt) (flush))
  (record-selection!
   label
   (if-let [ask (ui/backend :ask-text)]
     (ask prompt)
     (if (command/installed? "gum")
       (shell/capture-line "gum" "input" "--prompt" "▸ ")
       (do (binding [*out* *err*] (print "▸ ") (flush))
           (not-empty (some-> (read-line) str/trim)))))))

(defn valid-choice?
  [candidates choice]
  (boolean (some #(= choice %) candidates)))

(defn choose-one
  "Resolves a single item: a valid provided value is used as-is, anything else
   falls back to interactive selection. A provided value that is not an exact
   match seeds the picker's fuzzy query, so a near-miss (a typo or a partial name)
   surfaces the intended candidate - but it is never auto-accepted, so the
   operator confirms the highlighted row rather than a typo resolving to a
   different resource. Returns nil when nothing was chosen."
  [label candidates provided]
  (record-selection!
   label
   (cond
     (empty? candidates)
     nil

     (valid-choice? candidates provided)
     provided

     :else
     (do (when provided
           (stdout/warn (format "%s '%s' is not a valid choice, please select one" label provided)))
         (if-let [seed (some-> provided str not-empty)]
           (fuzzy/select-single label (sort candidates) seed false)
           (fuzzy/select-single label (sort candidates)))))))

(defn choose-many
  "Resolves multiple items: valid provided values are used as-is, anything else
   falls back to interactive multi-selection. Returns nil/empty when nothing was chosen."
  [label candidates provided]
  (record-selection!
   label
   (cond
     (empty? candidates)
     nil

     (and (seq provided) (every? (partial valid-choice? candidates) provided))
     provided

     :else
     (do (when (seq provided)
           (stdout/warn (format "%s %s contains invalid choices, please select" label (vec provided))))
         (fuzzy/select-multi label (sort candidates))))))

(defn choose-namespaced
  "Resolves one namespace/name pair from flags plus interactive selection.
   Candidates are 'namespace/name' strings; whichever flag is given narrows
   the selection."
  [label candidates namespace-flag name-flag]
  (let [provided (when (and namespace-flag name-flag)
                   (str namespace-flag "/" name-flag))
        filtered (cond->> candidates
                   namespace-flag (filter #(str/starts-with? % (str namespace-flag "/")))
                   name-flag (filter #(str/ends-with? % (str "/" name-flag))))]
    (if (and (seq candidates) (empty? filtered))
      (do (stdout/warn (format "no %s matches the given flags, please select from all instead" label))
          (choose-one label candidates provided))
      (choose-one label filtered provided))))

(defn confirmation-message
  "Pure rendering of the confirmation summary shown before a change."
  [{:keys [action target items]}]
  (str/join "\n" (concat [(format "This will %s on '%s':" action target)]
                         (map #(str "  - " %) items))))

(defn- typed-yes?
  "The strong ritual: the user types the literal word."
  []
  (binding [*out* *err*]
    (print "Type 'yes' to proceed: ")
    (flush))
  (= "yes" (some-> (read-line) str/trim str/lower-case)))

(defn- gum-yes?
  "Arrow-key yes/no via gum, defaulting to No."
  []
  (zero? (shell/run-with-output "gum" "confirm" "--default=false" "Proceed?")))

(defn confirm!
  "Second verification before touching a running system: states exactly what
   is about to happen and asks for approval. Destructive actions always
   require the literal word 'yes'; other changes use gum's yes/no picker when
   gum is installed. Returns true only when the user approves."
  [{:keys [destructive?] :as request}]
  (if destructive?
    (stdout/print-banner stdout/red "💥 DESTRUCTIVE ACTION - this cannot be undone automatically")
    (stdout/print-banner stdout/yellow "🔶 You are about to change a running system"))
  (stdout/err-println (confirmation-message request))
  ;; destructive actions always require the literal word 'yes', whatever the UI
  ;; backend - the deliberate friction is the point, not a widget's convenience
  (let [approved (cond
                   destructive? (typed-yes?)
                   (ui/backend :confirm) ((ui/backend :confirm))
                   (command/installed? "gum") (gum-yes?)
                   :else (typed-yes?))]
    (record-selection! (str "confirm: " (:action request)) (if approved "approved" "aborted"))
    (or approved
        (do (stdout/error "aborted, nothing was changed")
            false))))
