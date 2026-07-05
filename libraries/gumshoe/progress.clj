;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.progress
  "A live task list for the books - the kind you watch fill in. Each item shows a
   spinner while its work runs and a green check when it lands, updating in place
   on stderr so a captured result stays clean. Two shapes: `watch-futures!` for a
   set of tasks running in parallel (a scan collecting evidence), and `checklist`
   for steps that run one after another. Off a terminal (a pipe, cron) it prints
   each item once with no cursor tricks."
  (:require [gumshoe.stdout :as stdout]
            [gumshoe.theme :as theme]))

(defn- spinner
  [frame]
  (let [frames (theme/token :spinner)]
    (stdout/blue (nth frames (mod frame (count frames))))))

(defn- icon
  [status frame]
  (case status
    :done (stdout/green (theme/token :check-ok))
    :failed (stdout/red (theme/token :check-error))
    :running (spinner frame)
    (stdout/blue (theme/token :pending))))

(defn- line
  [status frame label]
  (str "  " (icon status frame) " " label))

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
   each flips from a spinner to ✓ as it resolves - and returns the futures'
   values in order once all are done. `labelled` is a seq of [label future]."
  [labelled]
  (let [labels (mapv first labelled)
        futures (mapv second labelled)]
    (cond
      (empty? labelled) []

      (stdout/colors-enabled?)
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
      (do (doseq [label labels] (stdout/err-println (str "  " (stdout/blue (theme/token :bullet)) " " label)))
          (mapv deref futures)))))

(defn checklist
  "Runs [label thunk] steps in order as a live task list: the current step
   animates, then shows ✓, or ✗ if it throws or returns a falsy value. A thunk
   may return a plain truthy/falsy value, or a map {:ok? bool :label \"...\"} to
   replace its displayed label with the result (a version discovered while
   checking, say). By default the first failure stops the rest and returns false
   (a playbook); with {:stop-on-failure? false} every step runs and the result is
   whether all passed (a prerequisites gate). Returns true only when every step
   that ran succeeded."
  ([steps] (checklist steps {}))
  ([steps {:keys [stop-on-failure?] :or {stop-on-failure? true}}]
   (let [n (count steps)
         labels (atom (mapv first steps))
         statuses (atom (vec (repeat n :pending)))
         animated? (stdout/colors-enabled?)
         paint! (fn [first? frame]
                  (when animated?
                    (redraw! (mapv (fn [i] (line (nth @statuses i) frame (nth @labels i))) (range n))
                             first?)))]
     (paint! true 0)
     (loop [i 0 all-ok true]
       (if (>= i n)
         (do (when animated? (paint! false 0)) all-ok)
         (let [thunk (second (nth steps i))]
           (swap! statuses assoc i :running)
           (let [running (atom true)
                 animation (when animated?
                             (future (loop [frame 0]
                                       (when @running
                                         (paint! false frame)
                                         (Thread/sleep 90)
                                         (recur (inc frame))))))
                 result (try (thunk) (catch Exception _ {:ok? false}))
                 result (if (map? result) result {:ok? (boolean result)})
                 ok (boolean (:ok? result))]
             (reset! running false)
             (when animation @animation)
             (when-let [label (:label result)] (swap! labels assoc i label))
             (swap! statuses assoc i (if ok :done :failed))
             (paint! false 0)
             (when-not animated?
               (stdout/err-println (line (if ok :done :failed) 0 (nth @labels i))))
             (if (and (not ok) stop-on-failure?)
               false
               (recur (inc i) (and all-ok ok))))))))))
