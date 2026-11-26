;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.progress
  "Live task lists for the books - the kind you watch fill in. Each row is
   printed with the mark of how it ended: :done, :failure, :not-yet with the
   reason when it could not read its answer or never started, or :finding
   when it finished with a caveat. The list writes to stderr, so a captured
   result stays clean.

   `run-steps` runs a static list of steps one after another and returns what
   each step returned. A book whose phases are not a static list opens a board
   with `with-board` and adds, starts and settles rows itself (`add!`, `run!`,
   `start!`, `settle!`). `watch-futures!` draws a set of tasks running in
   parallel, and `checklist` is run-steps for [label thunk] pairs, answering
   whether every step held.

   On a terminal a row is printed for good once it settles, followed by what
   its step printed, and never repainted. Only one line is: a spinner and what
   is still in flight (\"3 running: a, b, c\"), cut to the terminal's width,
   which `status!` replaces with the running step's status (\"waiting for …
   (10/60s)\"). A repaint therefore costs the same however many rows the
   list holds. Rows print in the order they were added, so a row that
   settles behind one still running waits for it. Off a terminal, under
   NO_COLOR or --no-color, a row prints once when it settles, followed by what
   its step printed, with no cursor movement and no colour. What a paint
   writes is decided by the pure `board-frame`, over gumshoe.render/step-lines
   and gumshoe.render/live-line."
  (:refer-clojure :exclude [run!])
  (:require [clojure.string :as str]
            [gumshoe.render :as render]
            [gumshoe.stdout :as stdout]))

(defn animate?
  "Whether the task list repaints in place. The repaint moves the cursor on
   stderr, so stderr must be a terminal: `bb book 2> run.log` gets each item
   once instead of a repaint every 90 ms, and FORCE_COLOR asks for colour, not
   for cursor movement into a log. NO_COLOR and --no-color turn it off as
   well. With stdout captured (`-o json | jq`) colours are off for stdout's
   sake and the terminal on stderr still gets the live list, unless one of
   those asked for no decoration."
  [{:keys [stderr-terminal? no-color? no-color-flag?]}]
  (boolean (and stderr-terminal?
                (not no-color?)
                (not no-color-flag?))))

(defn- terminal?
  "Whether fd points at a device. Where /proc is unavailable the target is
   unknown, and like gumshoe.stdout we assume a terminal and leave NO_COLOR as
   the escape hatch."
  [fd]
  (let [target (try (str (java.nio.file.Files/readSymbolicLink
                          (.toPath (java.io.File. (str "/proc/self/fd/" fd)))))
                    (catch Exception _ nil))]
    (or (nil? target) (str/starts-with? target "/dev/"))))

(def ^:dynamic *board*
  "The board whose body is running, or nil. A list opened inside another
   prints plain into the outer step's text, since two lists repainting one
   terminal would overwrite each other."
  nil)

(def ^:dynamic *step*
  "[board index] of the step whose work is running on this thread, or nil.
   Futures started by the step inherit it, so what they print lands under the
   same row."
  nil)

(defn- live?
  []
  (animate? {:stderr-terminal? (terminal? 2)
             :no-color? (not (str/blank? (System/getenv "NO_COLOR")))
             :no-color-flag? (stdout/no-color-requested?)}))

(def ^:private settled #{:done :failed :unread :warned :skipped})

(def ^:private unread-key
  "The ex-data key gumshoe.kubectl marks a read that did not answer with."
  :gumshoe.kubectl/unread)

(defn- thrown-reason
  [^Throwable e]
  (or (ex-message e) (.getName (class e))))

(defn step-result
  "Pure: what a step's work returned, or the Throwable it threw, as the result
   the list records,

     {:status :done|:failed|:unread|:skipped :ok? bool :value v
      :label \"…\" (replacing the row's label) :reason \"…\"}

   true, or any truthy value other than a map, is :done with that value. false
   and nil are :failed. A map is read as {:ok? bool :value v :label \"…\"
   :reason \"…\" :unread? bool :skipped? bool}: :unread? marks a step that
   could not learn its answer, :skipped? one deliberately left out, and a map
   without :ok? is :failed, since nothing said it held. A throw is :failed with
   its message, or :unread when it is a read that did not answer. Only :done
   and :skipped are ok?, so no step is ever drawn as done on an answer it did
   not get."
  [returned thrown]
  (cond
    thrown
    (if (unread-key (ex-data thrown))
      {:status :unread :ok? false :reason (str "could not read: " (thrown-reason thrown))}
      {:status :failed :ok? false :reason (str "threw: " (thrown-reason thrown))})

    (map? returned)
    (let [{:keys [ok? unread? skipped? label reason value]} returned
          status (cond unread? :unread
                       skipped? :skipped
                       (true? ok?) :done
                       :else :failed)]
      (cond-> {:status status :ok? (contains? #{:done :skipped} status) :value value}
        label (assoc :label (str label))
        reason (assoc :reason (str reason))
        (and (= :failed status) (not (contains? returned :ok?)) (nil? reason))
        (assoc :reason "the step returned a map without :ok?")
        (and (= :unread status) (nil? reason)) (assoc :reason "the step could not read its answer")))

    returned {:status :done :ok? true :value returned}

    :else {:status :failed :ok? false :value returned}))

(defn- board-context
  [{:keys [live?]}]
  (if live? (stdout/context) (assoc (stdout/context) :color? false)))

(defn- write-lines!
  [^java.io.Writer w lines]
  (doseq [l lines] (.write w (str l "\n"))))

(defn board-frame
  "Pure: what a paint of the board writes, from its state,

     {:loose \"text printed outside any step\" :loose-rest \"…\"
      :commit [lines printed for good] :upto n :live line-or-nil}

   :loose holds the whole lines printed outside any step, and :loose-rest
   the unfinished last line, kept back until its newline arrives, so it is
   never cleared with the live line; a closing board prints it, ended.
   :commit holds the rows from the first not yet printed up to the first that
   has not settled, each followed by its reason and what its step printed,
   or every remaining row once the board is :closing?. A row that settles
   behind an unsettled one waits for it, so the rows print in the order they
   were added and each step's text lands under its own row. :upto is the
   index of the first row still to print. :live is the one line a live board
   repaints (gumshoe.render/live-line) and nil off a terminal, where the
   committed lines lose their colour as well. Options: :live?, :closing?,
   :frame (the spinner frame) and :width (the columns the live line must
   fit)."
  [ctx {:keys [rows committed loose]} {:keys [live? closing? frame width]}]
  (let [upto (if closing?
               (count rows)
               (+ committed (count (take-while #(settled (:status %)) (drop committed rows)))))
        lines (mapcat #(render/step-lines ctx %) (subvec rows committed upto))
        text (apply str loose)
        cut (if closing?
              (count text)
              (if-let [at (str/last-index-of text "\n")] (inc at) 0))
        out (subs text 0 cut)]
    {:loose (if (or (empty? out) (str/ends-with? out "\n")) out (str out "\n"))
     :loose-rest (subs text cut)
     :commit (vec (if live? lines (map render/strip-colors lines)))
     :upto upto
     :live (when (and live? (not closing?))
             (render/live-line ctx (subvec rows upto) {:frame frame :width width}))}))

(defn- paint!
  "Writes what board-frame says. Live: go up over the live line drawn last
   time and clear it - one line at most, so a repaint costs the same however
   many rows the board holds - print what is committed, then the live line
   again. Every write ends a line, so with no live line drawn the cursor
   already stands at the start of an empty one. Plain: print only what is
   committed; nothing is ever redrawn."
  ([board] (paint! board false))
  ([{:keys [state live? ^java.io.Writer err lock] :as board} closing?]
   (locking lock
     (let [s @state
           {:keys [loose loose-rest commit upto live]}
           (board-frame (board-context board) s {:live? live? :closing? closing?
                                                 :frame (:frame s)
                                                 :width (when live? (dec (stdout/terminal-columns)))})]
       (if live?
         (do (when (pos? (:height s)) (.write err "\u001b[1A\r\u001b[J"))
             (.write err ^String loose)
             (write-lines! err commit)
             (when live (.write err (str live "\n")))
             (swap! state assoc :height (if live 1 0)))
         (write-lines! err commit))
       ;; text printed outside a step since the read above stays queued
       (let [consumed (- (count (apply str (:loose s))) (count loose-rest))]
         (swap! state (fn [now]
                        (let [left (subs (apply str (:loose now)) consumed)]
                          (assoc now :committed upto :loose (if (empty? left) [] [left]))))))
       (.flush err)))))

(defn- capture-writer
  "A writer for the body of a board: text written while one of its steps runs
   on this thread joins that row's output; other text goes out as it comes,
   uncoloured in plain mode, and in live mode is printed above the live line
   at the next paint."
  [{:keys [state live? ^java.io.Writer err] :as board}]
  (let [take-text (fn [text]
                    (let [[b i] *step*]
                      (cond
                        (and (identical? b board) i) (swap! state update-in [:rows i :output] str text)
                        live? (swap! state update :loose conj text)
                        :else (do (.write err (render/strip-colors text)) (.flush err)))))]
    (proxy [java.io.Writer] []
      (write
        ([x] (take-text (if (int? x) (str (char x)) (str x))))
        ([buf off len]
         (take-text (if (string? buf)
                      (subs buf off (+ off len))
                      (String. ^chars buf (int off) (int len))))))
      (flush [] nil)
      (close [] nil))))

(defn- open-board
  [opts]
  {:state (atom {:rows [] :committed 0 :loose [] :height 0 :frame 0})
   :live? (boolean (and (nil? *board*)
                        (if (contains? opts :live?) (:live? opts) (live?))))
   :err *err*
   :lock (Object.)})

(defn add!
  "Adds a row labelled `label` at the bottom of the board, as not yet
   started, and returns its index."
  [{:keys [state]} label]
  (dec (count (:rows (swap! state update :rows conj {:label (str label) :status :pending})))))

(defn start!
  "Marks row i as running: it draws a spinner until it settles."
  [{:keys [state]} i]
  (swap! state assoc-in [:rows i :status] :running))

(defn settle!
  "Records how row i ended, from what its work returned (see step-result),
   and returns the result with the row's label. A row that was cautioned with
   caveat! settles as :warned when it would otherwise be :done."
  ([board i returned] (settle! board i returned nil))
  ([{:keys [state] :as board} i returned thrown]
   (let [{:keys [label status] :as result} (step-result returned thrown)
         caveat? (get-in @state [:rows i :caveat?])
         status (if (and caveat? (= :done status)) :warned status)
         now (swap! state update-in [:rows i]
                    (fn [row] (cond-> (assoc row :status status :note nil)
                                label (assoc :label label)
                                (:reason result) (assoc :reason (:reason result)))))]
     (paint! board)
     (assoc result :status status :label (get-in now [:rows i :label])))))

(defn run!
  "Runs thunk as the work of row i - a label adds a new row for it - and
   settles the row with what it returned or threw. What the work prints, on
   this thread and in futures it starts, is drawn under the row. Returns the
   result, as settle! does. Nothing the work throws escapes."
  [board i-or-label thunk]
  (let [i (if (integer? i-or-label) i-or-label (add! board i-or-label))]
    (start! board i)
    (let [[returned thrown] (binding [*step* [board i]]
                              ;; Throwable: an AssertionError from a check is a
                              ;; failed step, and escaping would skip the settle.
                              (try [(thunk) nil] (catch Throwable e [nil e])))]
      (settle! board i returned thrown))))

(defn in-step?
  "Whether this thread runs the work of a row of an open task list."
  []
  (some? *step*))

(defn status!
  "Says how the running step is getting on - \"waiting for: … (10/60s)\",
   \"12 of 48 reads\". In a live list it replaces the text of the list's live
   line until the step settles, and is never printed for good. Elsewhere, a
   plain list included, it prints as an :in-progress item at once, so a log
   shows the wait as it happens."
  [text]
  (let [[{:keys [state live? ^java.io.Writer err]} i] *step*]
    (cond
      (and state live?) (swap! state assoc-in [:rows i :note] (str text))
      state (do (.write err (str (render/strip-colors (render/item (assoc (stdout/context) :color? false) :in-progress text)) "\n"))
                (.flush err))
      :else (stdout/item :in-progress text))))

(defn caveat!
  "Marks the running step as finishing with a caveat - a best-effort check
   that could not confirm - so it settles as :warned instead of :done. Does
   nothing outside a step."
  []
  (when-let [[{:keys [state]} i] *step*]
    (swap! state assoc-in [:rows i :caveat?] true)))

(defn caveated?
  "Whether caveat! was called for the running step."
  []
  (boolean (when-let [[{:keys [state]} i] *step*]
             (get-in @state [:rows i :caveat?]))))

(defn call-with-board
  "Opens a task list, calls (f board), and closes the list once f returns or
   throws: the animation stops and every row is printed for good, a row never
   started included, drawn :not-yet. Returns what f returned. Options: :live?
   false forces plain lines, as a pipe gets them."
  ([f] (call-with-board {} f))
  ([opts f]
   (let [{:keys [state live?] :as board} (open-board opts)
         running (atom true)
         writer (capture-writer board)
         stdout-on-screen? (and live? (terminal? 1))]
     (when live? (paint! board))
     (let [animation (when live?
                       (future (loop []
                                 (when @running
                                   (Thread/sleep 90)
                                   (swap! state update :frame inc)
                                   (paint! board)
                                   (recur)))))]
       (try
         (binding [*board* board
                   *err* writer
                   *out* (if stdout-on-screen? writer *out*)
                   stdout/*in-task-list* true]
           (f board))
         (finally
           (reset! running false)
           (when animation (deref animation 1000 nil))
           (paint! board true)))))))

(defn watch-futures!
  "Draws a task list while a set of labelled futures complete in parallel -
   each row flips from a spinner to the :done mark as its future resolves -
   and returns the futures' values in order once all are done. `labelled` is
   a seq of [label future]. Live, it is a board whose rows settle as their
   futures do; elsewhere, inside another list included, each label prints
   once at the start."
  [labelled]
  (let [labels (mapv first labelled)
        futures (mapv second labelled)]
    (cond
      (empty? labelled) []

      (and (nil? *board*) (live?))
      (call-with-board
       {:live? true}
       (fn [board]
         (let [rows (mapv #(add! board %) labels)]
           (doseq [i rows] (start! board i))
           (loop [left rows]
             (let [{done true left false} (group-by #(realized? (nth futures %)) left)]
               (doseq [i done] (settle! board i true))
               (when (seq left)
                 (Thread/sleep 90)
                 (recur left))))
           (mapv deref futures))))

      :else
      (do (doseq [label labels] (stdout/item :info label))
          (mapv deref futures)))))

(defmacro with-board
  "(with-board [b] body…) or (with-board [b {:live? false}] body…): runs body
   with b bound to an open task list, as call-with-board does."
  [[sym opts] & body]
  `(call-with-board ~(or opts {}) (fn [~sym] ~@body)))

(defn run-steps
  "Runs steps one after another as a live task list and returns one result
   per step, in order, as settle! returns them, with :label and :status. A step
   is {:label \"…\" :run (fn [] …)}; what :run returns decides how its row
   ends (see step-result), and what it prints is drawn under its row. By
   default the first step that is not ok stops the rest, which keep :status
   :pending and :ok? false; with {:stop-on-failure? false} every step runs.
   Options also take :live?, as call-with-board does."
  ([steps] (run-steps steps {}))
  ([steps {:keys [stop-on-failure?] :or {stop-on-failure? true} :as opts}]
   (call-with-board
    opts
    (fn [board]
      (let [indices (mapv #(add! board (:label %)) steps)]
        (loop [i 0 results []]
          (if (>= i (count steps))
            results
            (let [result (run! board (nth indices i) (:run (nth steps i)))]
              (if (and stop-on-failure? (not (:ok? result)))
                (into (conj results result)
                      (map (fn [s] {:label (str (:label s)) :status :pending :ok? false}) (drop (inc i) steps)))
                (recur (inc i) (conj results result)))))))))))

(defn checklist
  "Runs [label thunk] steps in order as a live task list, as run-steps does,
   and returns true only when every step that ran is ok?. By default the first
   failure stops the rest (a playbook); with {:stop-on-failure? false} every
   step runs (a prerequisites gate). A thunk may return a plain value or a
   map {:ok? bool :label \"…\"} replacing its displayed label with what it
   found (a version discovered while checking, say)."
  ([steps] (checklist steps {}))
  ([steps {:keys [stop-on-failure?] :or {stop-on-failure? true}}]
   (let [results (run-steps (mapv (fn [[label thunk]] {:label label :run thunk}) steps)
                            {:stop-on-failure? stop-on-failure?})]
     (every? :ok? (remove #(= :pending (:status %)) results)))))
