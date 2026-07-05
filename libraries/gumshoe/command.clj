;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.command
  "Checking that required command line tools are installed and meet version
   floors. The version a book actually ran against is shown in Prerequisites,
   so an operator reading a recording later knows exactly which tooling
   produced the result."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [gumshoe.shell :as shell]))

(defn installed?
  [tool]
  (some? (fs/which tool)))

(defonce ^:private tools
  ;; A tool's profile: how to read its :version-command, an optional :min-version
  ;; every book that uses it must meet, and optional :prerequisites the tool
  ;; brings along. A tool-support plugin registers its tool once here, so a book
  ;; that lists the tool in :installed-tools inherits its version and checks
  ;; instead of repeating them.
  (atom {"kubectl" {:version-command ["version" "--client"]}
         "krew" {:version-command ["version"]}
         "dig" {:version-command ["-v"]}
         "restic" {:version-command ["version"]}
         "openssl" {:version-command ["version"]}}))

(defn register-tool!
  "Registers a tool's profile so every book that lists it in :installed-tools
   gets its checks for free: :version-command (the argv that prints its version),
   an optional :min-version, and optional :prerequisites - (fn [opts] -> seq of
   [label thunk] checklist items) the tool brings along (a service it must reach,
   a login it needs). Merges into any existing profile for the tool."
  [binary profile]
  (swap! tools update binary merge profile))

(defn register-version-command!
  "Teaches the core how to read a tool's version - a shorthand for register-tool!
   with just :version-command."
  [tool args]
  (register-tool! tool {:version-command (vec args)}))

(defn tool-min-version
  "The minimum version registered for a tool, or nil - checked automatically for
   any book that lists the tool in :installed-tools."
  [tool]
  (get-in @tools [tool :min-version]))

(defn tool-prerequisites
  "The extra checklist-item builder a tool brought along ((fn [opts] -> items)),
   or nil."
  [tool]
  (get-in @tools [tool :prerequisites]))

(defn version
  "A short version string for an installed tool, or nil when it can not be
   determined. Best-effort and never throws - a version line is informational,
   never a reason to fail. Only a clean (zero) exit is trusted, so a tool that
   rejects the version flag reports no version rather than its own error text.
   Some tools print the version to stderr, so both streams are considered and
   the first non-blank line wins."
  [tool]
  (let [{:keys [exit out err]} (apply shell/execute tool (get-in @tools [tool :version-command] ["--version"]))]
    (when (zero? exit)
      (->> (str/split-lines (str out "\n" err))
           (map str/trim)
           (remove str/blank?)
           first
           not-empty))))

(defn parse-version
  "The first dotted numeric version (x.y or x.y.z) found in a string, as a
   vector of ints, or nil when there is none. Tolerates the noise tools wrap
   around the number (a leading v, a build suffix, trailing words)."
  [s]
  (when s
    (when-let [m (re-find #"(\d+)\.(\d+)(?:\.(\d+))?" s)]
      (mapv #(Integer/parseInt %) (filter some? (rest m))))))

(defn- pad3
  [v]
  (vec (take 3 (concat (or v []) [0 0 0]))))

(defn version-at-least?
  "Whether actual meets required. Either side may be a version string (from
   which the dotted number is extracted) or an int vector. An unparseable
   version on either side is treated as acceptable - a version floor never
   blocks on output we can not read, it only catches versions we can prove are
   too old."
  [actual required]
  (let [a (if (string? actual) (parse-version actual) actual)
        r (if (string? required) (parse-version required) required)]
    (or (nil? a) (nil? r)
        (>= (compare (pad3 a) (pad3 r)) 0))))

(defn select-by-version
  "Pure: the value from a {version-string -> value} table for an installed
   version - the value for the highest threshold the version meets, or the
   lowest threshold's value when it meets none. An unreadable (nil) version is
   treated as the newest, so it maps to the highest threshold."
  [installed table]
  (let [high->low (reverse (sort-by (comp parse-version key) table))]
    (or (some (fn [[threshold v]] (when (version-at-least? installed threshold) v)) high->low)
        (val (last high->low)))))

(defn dispatch-by-version
  "Selects from a {version-string -> value} table by the installed version of a
   tool - so a tool package stays agnostic to which major of its tool is present,
   choosing the right command form (or any value) at call time. See
   select-by-version for the pure selection."
  [tool table]
  (select-by-version (version tool) table))

(defn describe-installed
  "A label for an installed tool including its version when it can be read - what
   the Prerequisites checklist shows once a tool checks out."
  [tool]
  (if-let [v (version tool)]
    (format "%s (%s)" tool v)
    (format "%s installed" tool)))
