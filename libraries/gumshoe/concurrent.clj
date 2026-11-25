;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.concurrent
  "Independent reads issued at once, so a phase that needs several of them
   waits for the slowest instead of their sum.

   Each thunk runs in a future, which conveys the caller's dynamic bindings:
   kubectl/*strict-reads*, kubectl/*read-cache* and mutation/*phase* hold in
   the thunk as they hold around the call, so a concurrent read is as strict,
   as cached and in the same phase as a sequential one would be.

   Every thunk settles before the call returns, also when one of them throws,
   so no read started here outlives the call - and with it the binding of the
   phase it was issued in. The first failure in the order given is then
   rethrown as the thunk threw it, not wrapped in the ExecutionException a
   future's deref wraps it in, so a caller catching a strict read's
   ::kubectl/unread sees the same exception it would without concurrency.")

(defn- settle
  "{:value v} or {:thrown t} for a future, once it finished."
  [f]
  (try
    {:value @f}
    (catch java.util.concurrent.ExecutionException e
      {:thrown (or (ex-cause e) e)})
    (catch Throwable t
      {:thrown t})))

(defn all
  "The values of thunks, run concurrently, in the order given, as a vector.
   With one thunk or none it runs in the calling thread."
  [thunks]
  (let [thunks (vec thunks)]
    (if (< (count thunks) 2)
      (mapv #(%) thunks)
      (let [settled (mapv settle (mapv #(future (%)) thunks))]
        (when-let [thrown (some :thrown settled)]
          (throw thrown))
        (mapv :value settled)))))

(defn values
  "The map of thunks (key -> thunk) with each thunk replaced by its value,
   the thunks run concurrently as with all. A :derive that makes several
   independent reads returns it directly:

     (fn [{:keys [context]}]
       (concurrent/values {:present #(present-among context)
                           :pins    #(pin-problems context)}))"
  [thunks]
  (zipmap (keys thunks) (all (vals thunks))))
