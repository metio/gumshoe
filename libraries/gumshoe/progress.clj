;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.progress
  "Live task lists for the books - the kind you watch fill in. Each row shows
   the :not-yet mark until its work starts, a spinner while it runs, and the
   mark of how it ended: :done, :failure, :not-yet with the reason when it
   could not read its answer, or :finding when it finished with a caveat. The
   list writes to stderr, so a captured result stays clean.

   `run-steps` runs a static list of steps one after another and returns what
   each step returned. A book whose phases are not a static list opens a board
   with `with-board` and adds, starts and settles rows itself (`add!`, `run!`,
   `start!`, `settle!`). `watch-futures!` draws a set of tasks running in
   parallel, and `checklist` is run-steps for [label thunk] pairs, answering
   whether every step held.

   What a step prints while the list is open is kept and drawn under its row,
   so a repaint never overwrites it; `status!` replaces the row's transient
   status line (\"waiting for … (10/60s)\"). Off a terminal, under NO_COLOR or
   --no-color, a row prints once when it settles, followed by what its step
   printed, with no cursor movement and no colour. The rows are drawn by the
   pure gumshoe.render/step-lines."
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

(defn- line
  [status frame label]
  (render/task (stdout/context) status label frame))

(defn- redraw!
  "Repaints the whole board in place: move the cursor up over the previous
   render, then clear and rewrite each line. The first paint just writes the
   lines (there is nothing above to move over)."
  [lines first?]
  (binding [*out* *err*]
    (when (and (not first?) (pos? (count lines)))
      (print (str "[" (count lines) "A")))
    (doseq [l lines]
      (print (str "\r[K" l "\n")))
    (flush)))

(defn watch-futures!
  "Animates a task list while a set of labelled futures complete in parallel -
   each flips from a spinner to the :done mark as it resolves - and returns the futures'
   values in order once all are done. `labelled` is a seq of [label future]."
  [labelled]
  (let [labels (mapv first labelled)
        futures (mapv second labelled)]
    (cond
      (empty? labelled) []

      (and (nil? *board*) (live?))
      (do
        (loop [frame 0 first? true]
          (redraw! (mapv (fn [label fut] (line (if (realized? fut) :done :running) frame label))
                         labels futures)
                   first?)
          (when-not (every? realized? futures)
            (Thread/sleep 90)
            (recur (inc frame) false)))
        (redraw! (mapv #(line :done 0 %) labels) false)
        (mapv deref futures))

      :else
      (do (doseq [label labels] (stdout/item :info label))
          (mapv deref futures)))))

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

(defn- commit
  "Splits the board state into what may be printed for good - text printed
   outside any step and the longest run of settled rows from the top, or every
   row once the board closes - and what stays in the repainted region."
  [{:keys [rows committed loose]} closing?]
  (let [upto (if closing?
               (count rows)
               (+ committed (count (take-while #(settled (:status %)) (drop committed rows)))))]
    {:loose loose
     :rows (subvec rows committed upto)
     :upto upto
     :region (subvec rows upto)}))

(defn- paint!
  "Draws what changed. Live: move up over the region drawn last time, clear
   to the end of the screen, print what is committed, then draw the region
   again, each line cut to the terminal width. Plain: print only what is
   committed, uncoloured; nothing is ever redrawn."
  ([board] (paint! board false))
  ([{:keys [state live? ^java.io.Writer err lock] :as board} closing?]
   (locking lock
     (let [s @state
           {:keys [loose rows upto region]} (commit s closing?)
           ctx (board-context board)
           width (when live? (stdout/width))
           frame (:frame s)
           settled-lines (mapcat #(render/step-lines ctx %) rows)
           settled-lines (if live? settled-lines (map render/strip-colors settled-lines))]
       (if live?
         (do (when (pos? (:height s)) (.write err (str "\u001b[" (:height s) "A")))
             (.write err "\r\u001b[J")
             (when (seq loose) (.write err (apply str loose)))
             (write-lines! err settled-lines)
             (let [region-lines (vec (mapcat #(render/step-lines ctx % {:frame frame :tail 8 :width width}) region))]
               (write-lines! err region-lines)
               (swap! state assoc :height (count region-lines))))
         (write-lines! err settled-lines))
       (swap! state assoc :committed upto :loose [])
       (.flush err)))))

(defn- capture-writer
  "A writer for the body of a board: text written while one of its steps runs
   on this thread joins that row's output; other text goes out as it comes,
   uncoloured in plain mode, and in live mode is printed above the region at
   the next paint."
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
   \"12 of 48 reads\". In a live list it replaces the row's transient status
   line, which disappears when the row settles. Elsewhere, a plain list
   included, it prints as an :in-progress item at once, so a log shows the
   wait as it happens."
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
