;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prerequisites
  "Custom prerequisite checks - a plugin seam. The harness ships the built-in
   checks (installed tools, tool versions, connectivity, secrets, directories
   an option names, cluster capabilities, kubectl permissions). A plugin registers MORE with
   `register-check!`, so a book can gate on organisation policy the core knows
   nothing about: a change-freeze window, a maintenance ticket, an on-call
   acknowledgement, 'this cluster is not already mid-incident'.

   A book declares the check under its own key in :prerequisites; the registered
   builder turns that value into checklist items that animate in the Prerequisites
   section exactly like the built-ins, and any failure stops the book before it
   touches anything. Custom checks run after the built-in tooling/connectivity/
   permission checks."
  (:refer-clojure :exclude [check])
  (:require [babashka.fs :as fs]
            [briko.paths :as paths]
            [clojure.string :as str]
            [gumshoe.command :as command]))

(defonce ^:private checks (atom {}))

(defn register-check!
  "Registers a prerequisite check. `key` is the :prerequisites map key a book
   declares (e.g. :change-window). `build` is (fn [value opts] -> seq of
   [label thunk] items); a thunk does the check and returns {:ok? bool
   :label \"...\"} (or a plain truthy/falsy value). See `check` for the common
   single-item case."
  [key build]
  (swap! checks assoc key build))

(defn registered-checks
  "Every registered check key, sorted for a stable checklist order."
  []
  (sort (keys @checks)))

(defn check
  "Builds one checklist item from a label, a predicate thunk, and the labels (or
   0-arg fns) to show on pass/fail - the common shape for a register-check!
   builder."
  [label predicate {:keys [pass fail]}]
  [label (fn []
           (if (predicate)
             {:ok? true :label (if (fn? pass) (pass) (or pass label))}
             {:ok? false :label (if (fn? fail) (fail) (or fail label))}))])

(defn items
  "The checklist items contributed by every registered check the book declares,
   in a stable order. The harness appends these after the built-in checks."
  [prerequisites opts]
  (mapcat (fn [key]
            (when (contains? prerequisites key)
              ((get @checks key) (get prerequisites key) opts)))
          (registered-checks)))

(defn directory-path
  "The absolute, normalized path an option's directory value names: a leading
   `~` is the home directory ($HOME, then user.home, by briko.paths) and a
   relative path is resolved against the working directory; `~user` is left
   as written. A book that hands the value to a process resolves it with
   this too, so it uses the directory its prerequisite checked."
  [value]
  (str (fs/normalize (fs/absolutize (paths/expand-home (str value))))))

(defn- path-declaration
  "An :existing-paths entry as {:option :writable}, or nil when the entry is
   neither an option keyword nor a map naming one."
  [entry]
  (cond
    (keyword? entry) {:option entry :writable false}
    (and (map? entry) (keyword? (:option entry))) {:option (:option entry)
                                                    :writable (boolean (:writable entry))}
    :else nil))

(defn- directory-problem
  "Why a path fails as the directory a book needs, or nil when it does not."
  [path writable?]
  (cond
    (not (fs/exists? path)) "does not exist"
    (not (fs/directory? path)) "is not a directory"
    (and writable? (not (fs/writable? path))) "is not writable"
    :else nil))

(defn existing-path-items
  "The checklist items of an :existing-paths declaration: each entry names an
   option whose value must be an existing directory, as a keyword (:fahrplan)
   or as {:option :fahrplan :writable true} when the book also writes to it.
   An option left without a value passes with a note, because a book that
   needs it marks it :require and the parse stops first; an entry that names
   no option fails, so a malformed declaration never passes silently."
  [entries opts]
  (for [entry entries
        :let [{:keys [option writable]} (path-declaration entry)
              heading (if option
                        (str "directory --" (name option))
                        "prerequisite :existing-paths")]]
    [heading
     (fn []
       (let [value (get opts option)]
         (cond
           (nil? option)
           {:ok? false :label (str heading ": cannot read entry " (pr-str entry)
                                   " - name an option keyword or {:option <keyword> :writable true}")}

           (str/blank? (str value))
           {:ok? true :label (str heading ": not set, nothing to check")}

           :else
           (let [path (directory-path value)]
             (if-let [problem (directory-problem path writable)]
               {:ok? false :label (str heading ": " path " " problem)}
               {:ok? true :label (str heading ": " path (when writable " (writable)"))})))))]))

(defn- higher-version
  [a b]
  (if (command/version-at-least? a b) a b))

(defn- combine
  [key a b]
  (cond
    (= :minimum-tool-versions key) (merge-with higher-version a b)
    (= a b) a
    (and (sequential? a) (sequential? b)) (vec (distinct (concat a b)))
    (and (map? a) (map? b)) (merge a b)
    :else (throw (ex-info (format "the steps declare %s differently: %s and %s"
                                  key (pr-str a) (pr-str b))
                          {:key key :values [a b]}))))

(defn union
  "The prerequisites of several steps as one map, so a book that runs them all
   checks everything it will need before its first step. Lists are joined
   without duplicates, a tool's version floor is the highest one any step
   declares, and a key whose values can not be joined throws."
  [& prerequisites]
  (reduce (fn [acc m]
            (reduce-kv (fn [acc k v]
                         (if (contains? acc k) (update acc k #(combine k % v)) (assoc acc k v)))
                       acc m))
          {}
          (remove nil? prerequisites)))

(def kubectl-verbs
  "The permission prerequisites that ask `kubectl auth can-i` about a verb,
   with that verb, in the order they are checked. Each lists resources, and a
   resource may name a subresource, as \"pods/eviction\" or
   \"deployments/scale\": RBAC authorises a subresource apart from its
   resource, so a book lists the one its effect writes through.
   :kubectl-can-exec stands beside them and asks create on pods/exec."
  [[:kubectl-can-get "get"]
   [:kubectl-can-create "create"]
   [:kubectl-can-update "update"]
   [:kubectl-can-patch "patch"]
   [:kubectl-can-delete "delete"]])

(def ^:private summary-rows
  [[:installed-tools "tools"]
   [:kubectl-can-get "kubectl get"]
   [:kubectl-can-create "kubectl create"]
   [:kubectl-can-update "kubectl replace (update)"]
   [:kubectl-can-patch "kubectl patch"]
   [:kubectl-can-delete "kubectl delete"]
   [:kubectl-can-exec "kubectl exec into"]
   [:access-gopass-secrets "secrets named by"]
   [:can-ping-using-ipv4 "IPv4 reachability of"]
   [:can-ping-using-ipv6 "IPv6 reachability of"]])

(defn summary
  "One line per kind of prerequisite, in the words --help shows them."
  [prerequisites]
  (concat
   (for [[key title] summary-rows
         :let [values (get prerequisites key)]
         :when (seq values)]
     (str title ": " (str/join ", " (map name values))))
   (for [[tool version] (sort (:minimum-tool-versions prerequisites))]
     (str tool " at least " version))
   (when-let [entries (seq (keep path-declaration (:existing-paths prerequisites)))]
     [(str "existing directories: "
           (str/join ", " (for [{:keys [option writable]} entries]
                            (str "--" (name option) (when writable " (writable)")))))])
   (when-let [caps (seq (:cluster-capabilities prerequisites))]
     [(str "cluster capabilities: " (str/join ", " (map name caps)))])
   (for [key (registered-checks)
         :when (contains? prerequisites key)]
     (str "check: " (name key)))))
