;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.streams-support
  "Test support for the book harnesses: a run's two output streams captured
   apart. A book keeps stdout for its result and everything else on stderr,
   so a transcript records stderr as the run's printed lines and what reached
   stdout separately, under :stdout and only when there was any. A
   diagnostic that moves onto stdout then shows as a golden diff in every
   harnessed book.")

(defn separated
  "Runs thunk with *out* and *err* bound to their own writers:
   {:result <thunk's value> :err <stderr text> :out <stdout text>}."
  [thunk]
  (let [out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        result (binding [*out* out *err* err] (thunk))]
    {:result result :err (str err) :out (str out)}))

(defn with-stdout
  "transcript with :stdout set to (f out) when stdout held anything but
   whitespace."
  [transcript out f]
  (cond-> transcript
    (not (clojure.string/blank? out)) (assoc :stdout (f out))))
