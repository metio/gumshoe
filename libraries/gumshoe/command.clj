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

(defonce ^:private version-commands
  ;; How to ask a tool for its version, when the usual --version does not work.
  ;; A tool-support plugin registers its own tools here (register-version-command!)
  ;; so the core can show their versions in Prerequisites.
  (atom {"kubectl" ["version" "--client"]
         "krew" ["version"]
         "dig" ["-v"]
         "restic" ["version"]
         "openssl" ["version"]}))

(defn register-version-command!
  "Teaches the core how to read a tool's version - the argv that prints it. Used
   by tool-support plugins for tools the core does not ship knowledge of."
  [tool args]
  (swap! version-commands assoc tool (vec args)))

(defn version
  "A short version string for an installed tool, or nil when it can not be
   determined. Best-effort and never throws - a version line is informational,
   never a reason to fail. Only a clean (zero) exit is trusted, so a tool that
   rejects the version flag reports no version rather than its own error text.
   Some tools print the version to stderr, so both streams are considered and
   the first non-blank line wins."
  [tool]
  (let [{:keys [exit out err]} (apply shell/execute tool (get @version-commands tool ["--version"]))]
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

(defn describe-installed
  "A label for an installed tool including its version when it can be read - what
   the Prerequisites checklist shows once a tool checks out."
  [tool]
  (if-let [v (version tool)]
    (format "%s (%s)" tool v)
    (format "%s installed" tool)))
