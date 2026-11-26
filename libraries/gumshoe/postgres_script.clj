;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.postgres-script
  "Reads a SQL script as psql would split it into statements, to judge
   whether it can be replayed in one transaction (psql --single-transaction)
   before anything runs.

   PostgreSQL refuses some statements inside a transaction block (CREATE
   DATABASE, VACUUM, CREATE INDEX CONCURRENTLY, ...), a script's own BEGIN,
   COMMIT or ROLLBACK ends the transaction psql opened, and \\connect starts a
   new session outside it. Any of them breaks the promise that a failed
   replay leaves the database as it was, so `transaction-breakers` names
   each one with its line.

   The scanner follows psql's lexical rules closely enough for pg_dump's
   plain output: single-quoted, E'' and double-quoted strings, dollar-quoted
   bodies, line and nested block comments, COPY ... FROM stdin data up to its
   \\. terminator, psql meta-commands at the start of a statement, and the
   BEGIN ATOMIC ... END body of a SQL-standard function, whose semicolons do
   not end the CREATE statement. It reads a line sequence lazily and keeps
   only the first characters of each statement, so a dump of any size is
   judged in one pass without being held in memory."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private head-limit
  "Characters of a statement kept to judge it by. The opening keywords are
   all a judgement needs."
  200)

(def breakers
  "Statement openings that cannot run inside the transaction psql
   --single-transaction opens, or that end it, each with the name reported.
   Matched against the statement's head with its whitespace collapsed."
  [[#"(?i)^create\s+database\b" "CREATE DATABASE"]
   [#"(?i)^drop\s+database\b" "DROP DATABASE"]
   [#"(?i)^create\s+tablespace\b" "CREATE TABLESPACE"]
   [#"(?i)^drop\s+tablespace\b" "DROP TABLESPACE"]
   [#"(?i)^alter\s+system\b" "ALTER SYSTEM"]
   [#"(?i)^vacuum\b" "VACUUM"]
   [#"(?i)^create\s+(unique\s+)?index\s+concurrently\b" "CREATE INDEX CONCURRENTLY"]
   [#"(?i)^drop\s+index\s+concurrently\b" "DROP INDEX CONCURRENTLY"]
   [#"(?i)^reindex\b.*\bconcurrently\b" "REINDEX CONCURRENTLY"]
   [#"(?i)^reindex\s+(\([^)]*\)\s*)?(database|system)\b" "REINDEX DATABASE/SYSTEM"]
   [#"(?i)^cluster\s*$" "CLUSTER without a table"]
   [#"(?i)^(begin|start\s+transaction)\b" "BEGIN"]
   [#"(?i)^(commit|end)\b(?!\s+prepared)" "COMMIT"]
   [#"(?i)^(rollback|abort)\b(?!\s+prepared)" "ROLLBACK"]
   [#"(?i)^prepare\s+transaction\b" "PREPARE TRANSACTION"]
   [#"(?i)^(commit|rollback)\s+prepared\b" "COMMIT/ROLLBACK PREPARED"]])

(def ^:private meta-breakers
  "psql meta-commands that leave the session the transaction lives in."
  #{"\\c" "\\connect"})

(defn- collapse
  [s]
  (str/trim (str/replace s #"\s+" " ")))

(defn breaker
  "The name of what keeps the statement whose head is `head` from running in
   one transaction, or nil."
  [head]
  (let [head (collapse head)]
    (some (fn [[re name]] (when (re-find re head) name)) breakers)))

(defn- routine?
  "Whether a statement head creates a function or procedure, whose
   SQL-standard body may hold semicolons inside BEGIN ATOMIC ... END."
  [head]
  (boolean (re-find #"(?i)^create\s+(or\s+replace\s+)?(function|procedure)\b" (collapse head))))

(defn- copy-from-stdin?
  [head]
  (boolean (re-find #"(?is)^copy\s.*\bfrom\s+stdin\b" (collapse head))))

(def ^:private dollar-tag #"\$(?:[A-Za-z_\x80-￿][A-Za-z_0-9\x80-￿]*)?\$")

(defn- ident-char?
  [c]
  (and c (or (Character/isLetterOrDigit (char c)) (= \_ c) (= \$ c))))

(def ^:private initial
  {:mode :code :start? true :head nil :line-no 0 :depth 0 :found []})

(defn- grow
  "head with s appended, up to head-limit characters."
  [head s]
  (let [head (or head "")]
    (if (>= (count head) head-limit)
      head
      (let [joined (str head s)]
        (subs joined 0 (min head-limit (count joined)))))))

(defn- end-statement
  [{:keys [head line-no stmt-line] :as state}]
  (let [found (when head (breaker head))]
    (cond-> (assoc state :start? true :head nil :depth 0 :stmt-line nil)
      found (update :found conj {:line (or stmt-line line-no) :statement found})
      (and head (copy-from-stdin? head)) (assoc :copy-pending? true))))

(defn- word-at
  "The identifier word starting at i, or nil."
  [^String line i]
  (when-not (ident-char? (when (pos? i) (.charAt line (dec i))))
    (let [m (re-matcher #"[A-Za-z_][A-Za-z_0-9$]*" line)]
      (when (and (.find m (int i)) (= i (.start m)))
        (.group m)))))

(defn- scan-code
  "Scans line from i in :code mode, returning the state at the line's end."
  [state ^String line i]
  (let [n (.length line)]
    (loop [state state i i]
      (if (>= i n)
        state
        (let [c (.charAt line i)
              nxt (when (< (inc i) n) (.charAt line (inc i)))
              {:keys [mode start?]} state]
          (case mode
            :code
            (cond
              (and start? (Character/isWhitespace c)) (recur state (inc i))
              (and (= \- c) (= \- nxt)) state
              (and (= \/ c) (= \* nxt)) (recur (assoc state :mode :block :block-depth 1) (+ i 2))
              (and start? (= \; c)) (recur state (inc i))
              :else
              (let [state (if start?
                            (assoc state :start? false :head "" :stmt-line (:line-no state))
                            state)]
                (cond
                  (= \; c)
                  (if (pos? (:depth state))
                    (recur (update state :head grow ";") (inc i))
                    (recur (end-statement state) (inc i)))

                  (= \' c)
                  (let [e? (and (pos? i) (#{\E \e} (.charAt line (dec i)))
                                (not (ident-char? (when (> i 1) (.charAt line (- i 2))))))]
                    (recur (-> state (update :head grow "'") (assoc :mode (if e? :esquote :squote))) (inc i)))

                  (= \" c)
                  (recur (-> state (update :head grow "\"") (assoc :mode :dquote)) (inc i))

                  (and (= \$ c) (not (ident-char? (when (pos? i) (.charAt line (dec i))))))
                  (let [m (re-matcher dollar-tag line)]
                    (if (and (.find m (int i)) (= i (.start m)))
                      (recur (-> state (update :head grow (.group m)) (assoc :mode :dollar :tag (.group m)))
                             (.end m))
                      (recur (update state :head grow "$") (inc i))))

                  (Character/isLetter c)
                  (if-let [word (word-at line i)]
                    (let [lower (str/lower-case word)
                          routine (and (#{"begin" "case" "end"} lower) (routine? (:head state)))
                          state (cond
                                  (and routine (#{"begin" "case"} lower)) (update state :depth inc)
                                  (and routine (= "end" lower) (pos? (:depth state))) (update state :depth dec)
                                  :else state)]
                      (recur (update state :head grow word) (+ i (count word))))
                    (recur (update state :head grow (str c)) (inc i)))

                  :else (recur (update state :head grow (str c)) (inc i)))))

            :squote
            (let [j (.indexOf line "'" (int i))]
              (cond
                (neg? j) state
                (and (< (inc j) n) (= \' (.charAt line (inc j)))) (recur state (+ j 2))
                :else (recur (assoc state :mode :code) (inc j))))

            :esquote
            (cond
              (= \\ c) (recur state (+ i 2))
              (and (= \' c) (= \' nxt)) (recur state (+ i 2))
              (= \' c) (recur (assoc state :mode :code) (inc i))
              :else (recur state (inc i)))

            :dquote
            (let [j (.indexOf line "\"" (int i))]
              (cond
                (neg? j) state
                (and (< (inc j) n) (= \" (.charAt line (inc j)))) (recur state (+ j 2))
                :else (recur (assoc state :mode :code) (inc j))))

            :dollar
            (let [tag (:tag state)
                  j (.indexOf line ^String tag (int i))]
              (if (neg? j)
                state
                (recur (assoc state :mode :code :tag nil) (+ j (count tag)))))

            :block
            (cond
              (and (= \/ c) (= \* nxt)) (recur (update state :block-depth inc) (+ i 2))
              (and (= \* c) (= \/ nxt)) (let [d (dec (:block-depth state))]
                                          (recur (assoc state :block-depth d :mode (if (zero? d) :code :block)) (+ i 2)))
              :else (recur state (inc i)))))))))

(defn- scan-line
  [state ^String line]
  (let [state (update state :line-no inc)]
    (cond
      (= :copy (:mode state))
      (if (= "\\." line) (assoc state :mode :code :start? true) state)

      (and (= :code (:mode state)) (:start? state) (str/starts-with? (str/triml line) "\\"))
      (let [command (first (str/split (str/triml line) #"\s+"))]
        (cond-> state
          (meta-breakers command) (update :found conj {:line (:line-no state) :statement command})))

      :else
      (let [state (scan-code state line 0)
            state (if (:head state) (update state :head grow " ") state)]
        (if (:copy-pending? state)
          (assoc state :copy-pending? false :mode :copy)
          state)))))

(defn transaction-breakers
  "Every statement of the script `lines` that cannot run in one transaction,
   as [{:line <1-based line> :statement <what it is>}], in script order; empty
   when the whole script can. A statement the script leaves unterminated is
   judged too."
  [lines]
  (let [state (reduce scan-line initial lines)]
    (:found (if (and (:head state) (not (:start? state))) (end-statement state) state))))

(defn file-transaction-breakers
  "transaction-breakers of the script in file, read as UTF-8 line by line,
   or {:unread <why>} when the file cannot be read."
  [file]
  (try
    (with-open [reader (io/reader (str file) :encoding "UTF-8")]
      (transaction-breakers (line-seq reader)))
    (catch Exception e
      {:unread (or (ex-message e) (str e))})))
