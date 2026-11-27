;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.interact
  "Interactive selection and the second-step verification for risky actions.

   Every runbook resolves its target the same way: a valid CLI flag value is
   used as-is, anything else falls back to fuzzy selection. Every change to a
   running system is confirmed by the user before it happens."
  (:require [clojure.string :as str]
            [gumshoe.command :as command]
            [gumshoe.config :as config]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.shell :as shell]
            [gumshoe.outcome :as outcome]
            [gumshoe.render :as render]
            [gumshoe.stdout :as stdout]
            [gumshoe.theme :as theme]
            [gumshoe.ui :as ui]))

;; Every resolved selection is recorded so a book's recording can show exactly
;; what the operator picked, whether by flag or interactively.
(def ^:private selections (atom []))

(defn recorded-selections
  "The label/value pairs chosen so far this run, in order."
  []
  @selections)

(defn- record-only!
  "Appends a resolved selection to the run's audit list without echoing it - for a
   selection that already has its own visible output (a confirmation prints a
   banner and the approve/abort verdict)."
  [label value]
  (when (some? value)
    (swap! selections conj {:label label :value value}))
  value)

(defn- record-selection!
  "Records a resolved selection and echoes it to the terminal, so an interactive
   fuzzy pick leaves a visible trace in the run's transcript - not only in the
   recording's selections.edn. Returns the value so it composes in a threading
   position."
  [label value]
  (when (some? value)
    (stdout/selected label value))
  (record-only! label value))

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
       (shell/capture-line "gum" "input" "--prompt" (str (theme/glyph :info) " "))
       (do (binding [*out* *err*] (print (str (theme/glyph :info) " ")) (flush))
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
   different resource. An optional `preview` is a shell command (its `{}` is the
   highlighted candidate) fzf renders in a side pane, so the operator sees what a
   pick means before committing. Returns nil when nothing was chosen."
  ([label candidates provided] (choose-one label candidates provided nil))
  ([label candidates provided preview]
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
            (fuzzy/select-single label (sort candidates) {:query seed :auto-select? false :preview preview})
            (fuzzy/select-single label (sort candidates) {:preview preview})))))))

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

;; --- the destructive picker, an estate opt-in -------------------------------

(def destructive-setting
  "Where env.edn opts an estate in to the picker for destructive actions:
   top-level `:confirmations {:destructive :picker}`. Read with config/value,
   never per environment and never from the request, so neither a book nor the
   cluster a run points at can change how a destructive action is confirmed."
  [:confirmations :destructive])

(defn destructive-mode
  "Pure: how a destructive action is confirmed for an env.edn setting - :picker
   only for exactly the keyword :picker, :typed for anything else, an absent,
   misspelt or mistyped value included, so a mistake keeps the typed word."
  [setting]
  (if (= :picker setting) :picker :typed))

(def ^:private label-limit
  "The longest action a consequence label quotes before it is cut short; the
   whole action is printed in the confirmation summary above the picker."
  72)

(defn printable
  "Pure: s with every terminal escape sequence removed and every other control,
   format and separator character turned into a space (see
   gumshoe.render/printable)."
  [s]
  (render/printable s))

(defn consequence-label
  "Pure: the affirmative option of a destructive picker, naming what approving
   does - \"Yes, <action>\", with \" (N items)\" when the request lists more
   than one item - or nil when the request names no action, which leaves the
   picker nothing to name. Control characters are stripped (see printable) and
   a long action is cut short with an ellipsis."
  [{:keys [action items]}]
  (let [action (printable action)
        n (count items)]
    (when-not (str/blank? action)
      (str "Yes, "
           (if (> (count action) label-limit)
             (str (str/trimr (subs action 0 (dec label-limit))) "…")
             action)
           (when (> n 1) (format " (%d items)" n))))))

(defn- fd-target
  [fd]
  (try
    (str (java.nio.file.Files/readSymbolicLink
          (.toPath (java.io.File. (str "/proc/self/fd/" fd)))))
    (catch Exception _ nil)))

(defn terminal-device?
  "Pure: whether a file descriptor's /proc target is a terminal device.
   /dev/null and other devices, pipes, sockets and files are not."
  [target]
  (boolean (some #(str/starts-with? (str target) %) ["/dev/pts/" "/dev/tty" "/dev/console"])))

(defn- attended?
  "Whether the operator answers at a terminal: stdin and stderr, where a
   picker reads keys and draws itself, are both terminal devices. /dev/null
   and pipes are not. Where /proc does not say, only a console counts."
  []
  (let [in (fd-target 0) err (fd-target 2)]
    (if (and in err)
      (and (terminal-device? in) (terminal-device? err))
      (some? (System/console)))))

(defn- confirm-options
  "The options map a backend declaring :confirm-options? receives."
  [destructive? affirmative]
  {:prompt "Proceed?"
   :affirmative affirmative
   :negative "No"
   :default false
   :destructive? destructive?})

(defn- gum-destructive-yes?
  "gum's picker for a destructive action: No selected, the affirmative option
   naming the consequence, and no timeout, so an inherited GUM_CONFIRM_TIMEOUT
   can never answer for the operator."
  [affirmative]
  (zero? (shell/run-with-output "gum" "confirm" "--default=false" "--timeout=0s"
                                (str "--affirmative=" affirmative) "--negative=No"
                                "Proceed?")))

(defn- destructive-approved?
  "Asks for a destructive action. The typed word is the default and the
   fallback; the picker is used only when the estate opted in, the request
   names a consequence, the operator is at a terminal and a picker can honour
   a default of No: a backend declaring :confirm-options?, or gum when no
   backend overrides :confirm. A backend that does not declare it may ignore
   the default, so it gets the typed word, as does every other case."
  [request]
  (let [label (consequence-label request)
        backend (ui/backend :confirm)]
    (cond
      (not= :picker (destructive-mode (config/value destructive-setting))) (typed-yes?)
      (nil? label) (typed-yes?)
      (not (attended?)) (typed-yes?)
      backend (if (true? (ui/backend :confirm-options?))
                (true? (backend (confirm-options true label)))
                (typed-yes?))
      (command/installed? "gum") (gum-destructive-yes? label)
      :else (typed-yes?))))

(defn- change-approved?
  "Asks for a change that is not destructive: the backend's yes/no, gum's, or
   the typed word."
  []
  (if-let [backend (ui/backend :confirm)]
    (if (true? (ui/backend :confirm-options?))
      (true? (backend (confirm-options false "Yes")))
      (backend))
    (if (command/installed? "gum")
      (gum-yes?)
      (typed-yes?))))

(defn rehearsing?
  "Whether this run is a --dry-run. The harness binds gumshoe.flow/*dry-run*,
   and flow requires this namespace, so the var is looked up at call time; a
   run that never loaded flow cannot have bound it."
  []
  (boolean (some-> (resolve 'gumshoe.flow/*dry-run*) deref)))

(defn- refuse-rehearsal!
  "A confirmation reached under --dry-run belongs to a change that runs
   outside a plan, so nothing can show it without running it. It is refused,
   never answered yes, and the run fails."
  [request]
  (stdout/print-banner stdout/blue "🔍 DRY RUN - nothing will be changed")
  (stdout/err-println (confirmation-message request))
  (record-only! (str "confirm: " (:action request)) "refused under --dry-run")
  (outcome/aborted! "this book does not support a dry run")
  (stdout/error "this book does not support a dry run - nothing was run")
  false)

(defn- ask!
  [{:keys [destructive?] :as request}]
  (if destructive?
    (stdout/print-banner stdout/red "💥 DESTRUCTIVE ACTION - this cannot be undone automatically")
    (stdout/banner :finding "You are about to change a running system"))
  (stdout/err-println (confirmation-message request))
  ;; A destructive action asks for the typed word 'yes' unless env.edn opts the
  ;; estate in to the picker, and even then only where a picker can start on No
  ;; and name the consequence on its affirmative option; everywhere else the
  ;; typed word stays. Other changes take the backend's or gum's yes/no.
  (let [approved (if destructive?
                   (destructive-approved? request)
                   (change-approved?))]
    (record-only! (str "confirm: " (:action request)) (if approved "approved" "aborted"))
    (or approved
        (do (outcome/aborted! "declined at the confirmation")
            (stdout/error "aborted, this change was not made")
            false))))

(defn confirm!
  "Second verification before touching a running system: states exactly what
   is about to happen and asks for approval. Destructive actions require the
   literal word 'yes', unless env.edn sets `:confirmations {:destructive
   :picker}`: then a picker that starts on No and names the consequence asks
   instead, where one is available at a terminal. Other changes use the UI
   backend's or gum's yes/no picker when there is one. Returns true only when
   the user approves.

   Under --dry-run it asks nothing and returns false, so a book whose change
   is gated only by this confirmation never acts in a rehearsal. A change
   that can be shown runs through gumshoe.flow/change! with an :effect plan,
   whose dry run never reaches this function."
  [request]
  (if (rehearsing?)
    (refuse-rehearsal! request)
    (ask! request)))
