;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.hooks
  "Execution hooks - a plugin seam with two ends.

   POST hooks fire after every book finishes, with the run's outcome, so a plugin
   can push a metric, forward the recording to an audit store, update a status
   page, or trigger a follow-up. A post-hook is a pure observer: it never changes
   the outcome; a throwing one is warned about, and each is time-bounded so a slow
   one can never block the book's exit.

   PRE hooks fire before every book runs and can VETO it - a global gate for
   organisation policy that applies to every book without each declaring it (a
   change freeze that blocks changes, a required acknowledgement, a lock on
   the clusters a book may act on). A pre-hook sees the kube context and
   cluster the run pinned, returns false or {:allow? false :reason \"...\"}
   to block, and fails OPEN by default: a throwing or slow one warns and
   allows, so a broken gate can never block emergency response during an
   incident. A hook that declares itself fail-closed, or that env.edn names
   under :pre-hooks :fail-closed, blocks the book unless it explicitly
   allows it, so an estate that wants a lock gets one (see
   register-pre-hook! and run-pre-hooks!).

   Both differ from announcers, which fire on the operator's final confirmation
   ('this is starting now', for alert correlation), and from prerequisite checks,
   which a single book declares for itself - a pre-hook applies to every book."
  (:require [clojure.string :as str]
            [gumshoe.config :as config]
            [gumshoe.stdout :as stdout]))

(defonce ^:private post-hooks (atom []))
(defonce ^:private pre-hooks (atom []))

(def ^:private hook-timeout-ms
  "A post-hook runs after the book is already done, so a hang would only delay the
   exit; a pre-hook gates the start, so a hang delays it - bound both and move on."
  30000)

(defn register-post-hook!
  "Registers a function called after every book with the run context, a map:
     {:description  the book's one-line description
      :book         the book file that ran
      :opts         the parsed flags
      :outcome      :ok | :failed | :error
      :recording-path  where the audit trail was written (or nil)
      :meta         the announcement-data for a change (nil for a read-only book)}
   A hook runs after a scan printed its report to stdout, so whatever it
   prints goes to stderr (gumshoe.stdout/err-println): a line on stdout would
   follow the JSON or EDN document a script parses."
  [f]
  (swap! post-hooks conj f))

(defn registered-count
  "How many post-hooks are registered - for tests and diagnostics."
  []
  (count @post-hooks))

(defn run-post-hooks!
  "Runs every registered post-hook with the context. Best-effort and bounded: a
   hook that throws is warned about; one that exceeds the timeout is abandoned so
   it can never block the book's exit. Never throws."
  [context]
  (doseq [hook @post-hooks]
    ;; Catch Throwable, not just Exception: a hook that trips an assert throws
    ;; AssertionError (an Error), which would otherwise escape the future and
    ;; break the documented never-throw guarantee.
    (let [result (deref (future (try (hook context) nil (catch Throwable e (or (ex-message e) (str e)))))
                        hook-timeout-ms
                        ::timeout)]
      (cond
        (= ::timeout result) (stdout/warn "post-hook timed out - abandoned")
        (some? result) (stdout/warn "post-hook failed:" result)))))


(defn- hook-label
  "How a message names a pre-hook: its :name, else its place in the
   registration order."
  [{:keys [name]} index]
  (if name (str name) (format "pre-hook #%d" (inc index))))

(defn register-pre-hook!
  "Registers a hook called before every book, as a function or as a map

     {:hook          (fn [context] ...)       ; required
      :name          :estate/cluster-lock     ; optional keyword, names it in messages
                                              ; and in env.edn :pre-hooks
      :fail-closed?  true                     ; optional, default false
      :timeout-ms    30000}                   ; optional, default 30000

   The hook is called with the run context, a map:
     {:description    the book's one-line description
      :book           the book file about to run
      :opts           the parsed flags
      :change?        true for a book that makes a change, false for a read-only one
      :connection     the kube context the run pinned, {:context :cluster
                      :announce-as}, or nil when it pinned none or the pinned
                      one names no context (see gumshoe.kube-context)
      :cluster-access how the book reaches a Kubernetes cluster:
                      :pinned     only through :connection
                      :unresolved it pins its kube context, but the
                                  kubeconfig names no current context, so
                                  nobody can say which cluster it would act on
                      :unpinned   through the kubeconfig's current context as
                                  each call finds it; :connection is nil
                      :none       gumshoe sees no sign that it reaches one}

   A fail-open hook (the default) allows the run with anything truthy and
   vetoes it with false or {:allow? false :reason \"...\"}; one that throws or
   times out warns and allows, so a broken gate never blocks emergency
   response.

   A fail-closed hook allows the run only with true or {:allow? true}.
   Anything else - false, nil, a map without :allow? true, a throw, a timeout
   - blocks the book with a message naming the hook. A fail-closed hook is
   not called for a run whose :cluster-access is :unresolved: the run is
   blocked, because no lock can vouch for a cluster nobody can name.

   A hook is fail-closed when it declares :fail-closed? true or when env.edn
   names it under :pre-hooks :fail-closed (see run-pre-hooks!). Neither
   source can make a hook fail-open that the other made fail-closed, and a
   later registration under the same :name replaces the function but keeps
   it fail-closed once either registration declared it."
  [hook]
  (let [{:keys [name fail-closed? timeout-ms] :as entry} (if (map? hook) hook {:hook hook})]
    (when-not (ifn? (:hook entry))
      (throw (ex-info (format "a pre-hook needs a function under :hook, not %s" (pr-str (:hook entry))) {:hook hook})))
    (when (and (some? name) (not (keyword? name)))
      (throw (ex-info (format "a pre-hook :name is a keyword, such as :estate/cluster-lock, not %s" (pr-str name)) {:hook hook})))
    (when (and (some? timeout-ms) (not (pos-int? timeout-ms)))
      (throw (ex-info (format "a pre-hook :timeout-ms is a positive number of milliseconds, not %s" (pr-str timeout-ms)) {:hook hook})))
    (let [entry (-> (select-keys entry [:hook :name :timeout-ms])
                    (assoc :fail-closed? (true? fail-closed?)))]
      (swap! pre-hooks
             (fn [hooks]
               (if-let [i (and name (first (keep-indexed (fn [i h] (when (= name (:name h)) i)) hooks)))]
                 (update hooks i #(assoc entry :fail-closed? (or (:fail-closed? %) (:fail-closed? entry))))
                 (conj hooks entry)))))))

(defn- vetoed?
  [result]
  (or (false? result) (and (map? result) (false? (:allow? result)))))

(defn- explicitly-allowed?
  [result]
  (or (true? result) (and (map? result) (true? (:allow? result)))))

(defn policy-problem
  "Pure: why an env.edn :pre-hooks value can not be read as a pre-hook
   policy, or nil when it is absent or readable. A readable policy is a map
   whose optional :fail-closed lists hook names (keywords)."
  [declared]
  (cond
    (nil? declared) nil
    (not (map? declared))
    (format ":pre-hooks in env.edn is a map such as {:fail-closed [:estate/cluster-lock]}, not %s" (pr-str declared))
    (let [names (:fail-closed declared)]
      (and (some? names) (not (and (coll? names) (not (map? names)) (every? keyword? names)))))
    (format ":pre-hooks :fail-closed in env.edn lists pre-hook names as keywords, such as [:estate/cluster-lock], not %s"
            (pr-str (:fail-closed declared)))))

(defn- blocked
  [reason label]
  (cond-> {:allowed? false :reason reason} label (assoc :hook label)))

(defn- policy-refusal
  "Why the policy alone blocks this run before any hook is called, or nil:
   an env.edn nobody could read hides the estate's policy, a malformed
   :pre-hooks hides which hooks it requires, and a hook it requires that is
   not registered is a lock whose plugin did not load."
  [{:keys [declared unreadable]} {:keys [cluster-access]} hooks]
  (cond
    (and unreadable (not= :none cluster-access))
    (format "env.edn could not be read (%s), so the pre-hooks it requires are unknown and no book that reaches a cluster runs until it is fixed"
            unreadable)

    (policy-problem declared)
    (str (policy-problem declared) " - no book runs until it is fixed")

    :else
    (let [registered (set (keep :name hooks))]
      (when-let [missing (seq (remove registered (:fail-closed declared)))]
        (format "env.edn requires the fail-closed pre-hook%s %s, but no loaded plugin registered %s - check :plugins in env.edn"
                (if (next missing) "s" "")
                (str/join ", " missing)
                (if (next missing) "them" "it"))))))

(defn- call-hook
  "{:value ..}, {:error ..} or ::timeout for one bounded call of a hook."
  [{:keys [hook timeout-ms]} context]
  ;; Catch Throwable so a hook that trips an assert (AssertionError, an
  ;; Error) is judged by its hook's failure mode rather than crashing the run.
  (deref (future (try {:value (hook context)}
                      (catch Throwable e {:error (or (ex-message e) (str e))})))
         (or timeout-ms hook-timeout-ms)
         ::timeout))

(defn- configured-policy
  []
  {:declared (config/value [:pre-hooks])
   :unreadable (config/unreadable)})

(defn run-pre-hooks!
  "Runs the pre-hooks in registration order and returns {:allowed? bool
   :reason ...}, with :hook naming the hook that blocked when it has a name
   or is fail-closed. The first block stops the book.

   policy is {:declared <env.edn :pre-hooks> :unreadable <why env.edn could
   not be read, or nil>}, read from env.edn when not given. :declared
   {:fail-closed [:estate/cluster-lock ...]} names hooks the estate requires:
   each must be registered, or every book is blocked, and each runs
   fail-closed whatever it declares itself. A malformed :pre-hooks blocks
   every book, and an env.edn that could not be read blocks every book that
   reaches a cluster (see register-pre-hook! for :cluster-access).

   A fail-open hook that throws or times out warns and allows; a fail-closed
   one blocks (see register-pre-hook!)."
  ([context] (run-pre-hooks! context (configured-policy)))
  ([context policy]
   (let [hooks @pre-hooks
         required (set (when-not (policy-problem (:declared policy)) (:fail-closed (:declared policy))))]
     (if-let [why (policy-refusal policy context hooks)]
       (blocked why nil)
       (loop [remaining (map-indexed vector hooks)]
         (if-let [[index entry] (first remaining)]
           (let [closed? (or (:fail-closed? entry) (contains? required (:name entry)))
                 label (hook-label entry index)
                 named (when (or closed? (:name entry)) label)]
             (if (and closed? (= :unresolved (:cluster-access context)))
               (blocked (format "fail-closed pre-hook %s cannot vouch for this run: the book pins its kube context, but the kubeconfig names no current context"
                                label)
                        label)
               (let [result (call-hook entry context)]
                 (cond
                   (= ::timeout result)
                   (if closed?
                     (blocked (format "fail-closed pre-hook %s timed out" label) label)
                     (do (stdout/warn "pre-hook timed out - allowing") (recur (rest remaining))))

                   (contains? result :error)
                   (if closed?
                     (blocked (format "fail-closed pre-hook %s failed: %s" label (:error result)) label)
                     (do (stdout/warn "pre-hook failed - allowing:" (:error result)) (recur (rest remaining))))

                   (vetoed? (:value result))
                   (blocked (or (:reason (:value result))
                                (if closed?
                                  (format "fail-closed pre-hook %s vetoed this run" label)
                                  "a pre-execution hook vetoed this run"))
                            named)

                   (and closed? (not (explicitly-allowed? (:value result))))
                   (blocked (format "fail-closed pre-hook %s returned %s, not an explicit allow (true or {:allow? true})"
                                    label (let [s (pr-str (:value result))]
                                            (if (> (count s) 120) (str (subs s 0 117) "...") s)))
                            label)

                   :else (recur (rest remaining))))))
           {:allowed? true}))))))
