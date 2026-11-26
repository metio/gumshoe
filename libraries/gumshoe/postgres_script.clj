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
   judged in one pass without being held in memory. The few statements whose
   judgement depends on words past their opening (COPY ... FROM stdin, whose
   column list has no length bound, and REINDEX ... CONCURRENTLY) keep their
   complete code text up to the terminating semicolon instead, bounded by
   text-limit. A statement past that bound cannot be judged, so the scan
   refuses the script rather than guess that it is safe."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private head-limit
  "Characters of a statement kept to judge it by. The opening keywords are
   all most judgements need; `full-text-openers` names the exceptions."
  200)

(def ^:private full-text-openers
  "First words of the statements judged on their complete code text, because
   the words that decide them may sit past head-limit: pg_dump writes a
   table's whole column list between COPY and FROM stdin, and CONCURRENTLY
   may follow a long REINDEX option list. Mistaking a COPY for an ordinary
   statement makes its data rows read as SQL, which refuses a sound dump or
   hides a real breaker."
  #{"copy" "reindex"})

(def ^:private text-limit
  "Characters of code text kept for a statement opened by one of
   full-text-openers. pg_dump's longest COPY header (1600 columns of 63-byte
   quoted names) is about 100 KiB, so a statement past this bound is a
   script missing its terminating semicolon, and the rest of the script
   would otherwise be held in memory."
  (* 1024 1024))

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

(defn- too-long
  "The refusal thrown when the kept text of the statement starting at line
   would grow past text-limit."
  [line kept]
  (ex-info (format "the statement starting at line %d is longer than %d characters; cannot verify the dump runs in one transaction"
                   line text-limit)
           {::too-long true :line line :limit text-limit :kept kept}))

(defn- add
  "state with s appended to the statement's head and, for a statement judged
   on its complete text, to that text. The text is a StringBuilder threaded
   through the scan, which uses each state once, and stops at text-limit."
  [state ^String s]
  (when-let [^StringBuilder text (:text state)]
    (when (> (+ (.length text) (.length s)) text-limit)
      (throw (too-long (:stmt-line state) (.length text))))
    (.append text s))
  (update state :head grow s))

(defn- open-text
  "state keeping the statement's complete text from here on when word is its
   first word and one of full-text-openers."
  [state word lower]
  (if (and (= "" (:head state)) (full-text-openers lower))
    (assoc state :text (StringBuilder.))
    state))

(defn- statement-text
  "The text a statement is judged by: its complete code text when it was
   kept, its head otherwise."
  [{:keys [head text]}]
  (if text (str text) head))

(defn- end-statement
  [{:keys [head line-no stmt-line] :as state}]
  (let [text (when head (statement-text state))
        found (when text (breaker text))]
    (cond-> (assoc state :start? true :head nil :text nil :depth 0 :stmt-line nil)
      found (update :found conj {:line (or stmt-line line-no) :statement found})
      (and text (copy-from-stdin? text)) (assoc :copy-pending? true))))

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
                    (recur (add state ";") (inc i))
                    (recur (end-statement state) (inc i)))

                  (= \' c)
                  (let [e? (and (pos? i) (#{\E \e} (.charAt line (dec i)))
                                (not (ident-char? (when (> i 1) (.charAt line (- i 2))))))]
                    (recur (-> state (add "'") (assoc :mode (if e? :esquote :squote))) (inc i)))

                  (= \" c)
                  (recur (-> state (add "\"") (assoc :mode :dquote)) (inc i))

                  (and (= \$ c) (not (ident-char? (when (pos? i) (.charAt line (dec i))))))
                  (let [m (re-matcher dollar-tag line)]
                    (if (and (.find m (int i)) (= i (.start m)))
                      (recur (-> state (add (.group m)) (assoc :mode :dollar :tag (.group m)))
                             (.end m))
                      (recur (add state "$") (inc i))))

                  (Character/isLetter c)
                  (if-let [word (word-at line i)]
                    (let [lower (str/lower-case word)
                          routine (and (#{"begin" "case" "end"} lower) (routine? (:head state)))
                          state (cond
                                  (and routine (#{"begin" "case"} lower)) (update state :depth inc)
                                  (and routine (= "end" lower) (pos? (:depth state))) (update state :depth dec)
                                  :else state)
                          state (open-text state word lower)]
                      (recur (add state word) (+ i (count word))))
                    (recur (add state (str c)) (inc i)))

                  :else (recur (add state (str c)) (inc i)))))

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
            state (if (:head state) (add state " ") state)]
        (if (:copy-pending? state)
          (assoc state :copy-pending? false :mode :copy)
          state)))))

(defn transaction-breakers
  "Every statement of the script `lines` that cannot run in one transaction,
   as [{:line <1-based line> :statement <what it is>}], in script order; empty
   when the whole script can. A statement the script leaves unterminated is
   judged too. Throws ex-info with ::too-long when a statement judged on its
   complete text is longer than text-limit, since nothing then shows it is
   safe."
  [lines]
  (let [state (reduce scan-line initial lines)]
    (:found (if (and (:head state) (not (:start? state))) (end-statement state) state))))

(defn file-transaction-breakers
  "transaction-breakers of the script in file, read as UTF-8 line by line,
   {:unjudged <why>} when a statement is too long to judge, or {:unread <why>}
   when the file cannot be read or scanned. Every outcome but an empty
   vector refuses the restore, so a scan that fails in any way, an
   OutOfMemoryError included, is reported as a refusal naming the dump."
  [file]
  (try
    (with-open [reader (io/reader (str file) :encoding "UTF-8")]
      (transaction-breakers (line-seq reader)))
    (catch clojure.lang.ExceptionInfo e
      (if (::too-long (ex-data e))
        {:unjudged (ex-message e)}
        {:unread (or (ex-message e) (str e))}))
    ;; Throwable, so an Error thrown while scanning (OutOfMemoryError above
    ;; all) refuses the restore instead of escaping as an unexpected error.
    ;; The scan's state is unreachable once this frame unwinds, so the heap
    ;; it held is free again by the time the refusal is printed.
    (catch Exception e
      {:unread (or (ex-message e) (str e))})
    (catch Throwable e
      {:unread (str "scanning it failed with " e)})))
