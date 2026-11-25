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
   ::kubectl/unread sees the same exception it would without concurrency.
   A caller interrupted while it waits cancels the thunks still running
   instead of waiting for them, so abandoned work starts no further command.")

(defn- settle
  "{:value v} or {:thrown t} for a future, once it finished. An interrupt of
   the waiting thread is not the future's outcome, so it is rethrown."
  [f]
  (try
    {:value @f}
    (catch java.util.concurrent.ExecutionException e
      {:thrown (or (ex-cause e) e)})
    (catch InterruptedException e
      (throw e))
    (catch Throwable t
      {:thrown t})))

(defn- limited
  "thunk, made to wait for one of semaphore's permits before it runs."
  [^java.util.concurrent.Semaphore semaphore thunk]
  (fn []
    (.acquire semaphore)
    (try (thunk) (finally (.release semaphore)))))

(defn all
  "The values of thunks, run concurrently, in the order given, as a vector.
   With one thunk or none it runs in the calling thread. :parallelism bounds
   how many run at once; without it all of them do.

   A caller interrupted while it waits - a collector or a post-check whose
   deadline cancelled it - cancels every thunk still running before it
   rethrows, so their commands are refused too (gumshoe.shell refuses to start
   one on an interrupted thread) instead of running on after the work was
   abandoned."
  ([thunks] (all thunks {}))
  ([thunks {:keys [parallelism]}]
   (let [thunks (vec thunks)]
     (if (< (count thunks) 2)
       (mapv #(%) thunks)
       (let [semaphore (when parallelism (java.util.concurrent.Semaphore. (max 1 parallelism)))
             futures (mapv (fn [thunk] (future ((if semaphore (limited semaphore thunk) thunk))))
                           thunks)
             settled (try (mapv settle futures)
                          (catch InterruptedException e
                            (run! future-cancel futures)
                            (.interrupt (Thread/currentThread))
                            (throw e)))]
         (when-let [thrown (some :thrown settled)]
           (throw thrown))
         (mapv :value settled))))))

(defn map-all
  "(f x) for every x of coll, run concurrently as with all, as a vector in
   coll's order. The cancellable replacement for a bare pmap or a vector of
   futures: options are those of all."
  ([f coll] (map-all f coll {}))
  ([f coll options]
   (all (map (fn [x] #(f x)) coll) options)))

(def default-parallelism
  "The bound pmap uses: two more than the processors available."
  (+ 2 (.availableProcessors (Runtime/getRuntime))))

(defn values
  "The map of thunks (key -> thunk) with each thunk replaced by its value,
   the thunks run concurrently as with all. A :derive that makes several
   independent reads returns it directly:

     (fn [{:keys [context]}]
       (concurrent/values {:present #(present-among context)
                           :pins    #(pin-problems context)}))"
  [thunks]
  (zipmap (keys thunks) (all (vals thunks))))
