;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cancellation-test
  "A cancelled collector or post-check stops its command and runs no further
   one, and a book ended by SIGTERM takes its port-forward down with it."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.shell :as shell]
            [gumshoe.verify :as verify])
  (:import [java.net InetAddress ServerSocket]))

(defn- alive? [pid]
  (let [handle (java.lang.ProcessHandle/of (long pid))]
    (and (.isPresent handle) (.isAlive (.get handle)))))

(defn- wait-for
  "Polls (f) every 50ms for up to ms; its last value."
  [ms f]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (let [v (f)]
        (if (or v (> (System/currentTimeMillis) deadline))
          v
          (do (Thread/sleep 50) (recur)))))))

(defn- read-pid [file]
  (wait-for 5000 #(when (fs/exists? file) (parse-long (str/trim (slurp (str file)))))))

(defn- sleeper
  "A command that writes its pid to pid-file and then sleeps."
  [pid-file seconds]
  ["sh" "-c" (format "echo $$ > '%s'; exec sleep %d" pid-file seconds)])

(defn- collector
  "A collector that runs a sleeping command, and after it a second one that
   would leave marker behind - as a multi-step collector does. It swallows
   every exception, as a lax collector may."
  [pid-file marker]
  (fn []
    (try (apply shell/execute (sleeper pid-file 30)) (catch Exception _ nil))
    (try (shell/execute "sh" "-c" (format "touch '%s'" marker)) (catch Exception _ nil))
    :done))

(deftest an-interrupted-capture-destroys-its-process-and-keeps-the-interrupt
  (fs/with-temp-dir [dir {}]
    (let [pid-file (str (fs/path dir "pid"))
          outcome (promise)
          worker (Thread. ^Runnable
                  (fn []
                    (deliver outcome
                             (try (apply shell/execute (sleeper pid-file 30))
                                  (catch InterruptedException _
                                    {:thrown :interrupted
                                     :flag (.isInterrupted (Thread/currentThread))})))))]
      (.start worker)
      (let [pid (read-pid pid-file)]
        (is (alive? pid))
        (.interrupt worker)
        (is (= {:thrown :interrupted :flag true} (deref outcome 5000 :no-answer)))
        (is (wait-for 5000 #(not (alive? pid))) "the sleeping command was destroyed")))))

(deftest an-interrupted-thread-starts-no-command
  (fs/with-temp-dir [dir {}]
    (let [marker (fs/path dir "ran")
          outcome (promise)
          worker (Thread. ^Runnable
                  (fn []
                    (.interrupt (Thread/currentThread))
                    (deliver outcome
                             (try (shell/execute "sh" "-c" (format "touch '%s'" marker))
                                  (catch InterruptedException _ :refused)))))]
      (.start worker)
      (is (= :refused (deref outcome 5000 :no-answer)))
      (is (not (fs/exists? marker))))))

(deftest a-capture-that-finishes-tracks-no-child
  (let [before (shell/live-children-count)]
    (is (= 0 (:exit (shell/execute "true"))))
    (is (= before (shell/live-children-count)))))

(deftest a-timed-out-collector-stops-its-command-and-runs-no-further-one
  (fs/with-temp-dir [dir {}]
    (let [pid-file (str (fs/path dir "pid"))
          marker (fs/path dir "second-command-ran")
          bounded-collect @#'detective/bounded-collect
          value (bounded-collect (collector pid-file marker) 1000)
          pid (read-pid pid-file)]
      (is (= ::detective/timed-out value))
      (is (wait-for 5000 #(not (alive? pid))) "the in-flight command was destroyed")
      (Thread/sleep 500)
      (is (not (fs/exists? marker)) "the cancelled collector ran no further command"))))

(deftest a-timed-out-post-check-attempt-stops-its-command
  (fs/with-temp-dir [dir {}]
    (let [pid-file (str (fs/path dir "pid"))
          marker (fs/path dir "second-command-ran")
          check (fn []
                  (apply shell/execute (sleeper pid-file 30))
                  (shell/execute "sh" "-c" (format "touch '%s'" marker))
                  true)
          result (verify/bounded-attempt check 1000)
          pid (read-pid pid-file)]
      (is (string? result))
      (is (str/includes? result "did not return"))
      (is (wait-for 5000 #(not (alive? pid))) "the check's command was destroyed")
      (Thread/sleep 500)
      (is (not (fs/exists? marker)) "the abandoned attempt overlaps no later one"))))

;; --- SIGTERM -------------------------------------------------------------------

(defn- absolute-classpath []
  (->> (str/split (System/getProperty "java.class.path") #":")
       (map #(str (fs/absolutize %)))
       (str/join ":")))

(defn- free-port []
  (with-open [s (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))]
    (.getLocalPort s)))

(defn- fake-kubectl!
  "A kubectl that records its pid and sleeps without opening the port, so the
   book waits for the forward to come up until it is terminated."
  [dir pid-file]
  (let [path (fs/path dir "kubectl")]
    (spit (str path) (format "#!/bin/sh\necho $$ > '%s'\nexec sleep 300\n" pid-file))
    (fs/set-posix-file-permissions path "rwxr-xr-x")))

(deftest sigterm-takes-a-port-forward-down
  (fs/with-temp-dir [dir {}]
    (let [pid-file (str (fs/path dir "kubectl.pid"))
          _ (fake-kubectl! dir pid-file)
          script (str "(require '[gumshoe.kubectl :as kubectl])"
                      "(kubectl/with-port-forward"
                      "  {:context \"c\" :namespace \"n\" :service \"s\" :local-port " (free-port) " :remote-port 80}"
                      "  (fn [] :forwarded))")
          ;; files, not pipes: an orphaned forward would hold an inherited
          ;; pipe open and block reading the book's output past the test
          book (process/process {:dir (str dir)
                                 :out :write :out-file (fs/file dir "book.out")
                                 :err :write :err-file (fs/file dir "book.err")
                                 :extra-env {"PATH" (str dir ":" (System/getenv "PATH"))
                                             "GUMSHOE_ENV" (str (fs/path dir "env.edn"))
                                             "KUBECONFIG" "/dev/null"
                                             "NO_COLOR" "1"}}
                                "bb" "-cp" (absolute-classpath) "-e" script)
          forward (read-pid pid-file)]
      (try
        (is (some? forward) "the fake kubectl started")
        (is (alive? forward))
        (.destroy ^Process (:proc book))
        (is (.waitFor ^Process (:proc book) 10 java.util.concurrent.TimeUnit/SECONDS)
            "the book ended on SIGTERM")
        (testing "the forward did not outlive the book"
          (is (wait-for 5000 #(not (alive? forward)))))
        (finally
          (when forward (process/shell {:continue true :err :string} "kill" "-KILL" (str forward)))
          (process/destroy-tree book))))))

(deftest sigterm-stops-a-captured-command-in-flight
  (fs/with-temp-dir [dir {}]
    (let [pid-file (str (fs/path dir "sleeper.pid"))
          script (format "(require '[gumshoe.shell :as shell]) (binding [shell/*timeout-ms* nil] (apply shell/execute %s))"
                         (pr-str (sleeper pid-file 300)))
          ;; files, not pipes: an orphaned command would hold an inherited
          ;; pipe open and block reading the book's output past the test
          book (process/process {:dir (str dir)
                                 :out :write :out-file (fs/file dir "book.out")
                                 :err :write :err-file (fs/file dir "book.err")
                                 :extra-env {"GUMSHOE_ENV" (str (fs/path dir "env.edn"))
                                             "KUBECONFIG" "/dev/null"
                                             "NO_COLOR" "1"}}
                                "bb" "-cp" (absolute-classpath) "-e" script)
          command (read-pid pid-file)]
      (try
        (is (some? command) "the captured command started")
        (is (alive? command))
        (.destroy ^Process (:proc book))
        (is (.waitFor ^Process (:proc book) 10 java.util.concurrent.TimeUnit/SECONDS)
            "the book ended on SIGTERM")
        (testing "the captured command did not outlive the book"
          (is (wait-for 5000 #(not (alive? command)))))
        (finally
          (when command (process/shell {:continue true :err :string} "kill" "-KILL" (str command)))
          (process/destroy-tree book))))))

(deftest a-captured-command-is-tracked-only-while-it-runs
  (fs/with-temp-dir [dir {}]
    (let [pid-file (str (fs/path dir "pid"))
          before (shell/live-children-count)
          running (future (binding [shell/*timeout-ms* 10000]
                            (apply shell/execute (sleeper pid-file 2))))]
      (is (some? (read-pid pid-file)))
      (is (= (inc before) (shell/live-children-count)) "tracked while it runs")
      (is (= 0 (:exit (deref running 10000 nil))))
      (is (= before (shell/live-children-count)) "forgotten once it returned"))))

(defn- recorded-with
  "Whether the reproducer's recording holds a command line containing token."
  [token]
  (boolean (some #(str/includes? % token) (shell/recording))))

(deftest a-command-refused-on-a-cancelled-thread-is-not-recorded
  (let [token (str "refused-" (random-uuid))
        outcome (promise)
        worker (Thread. ^Runnable
                (fn []
                  (.interrupt (Thread/currentThread))
                  (deliver outcome
                           (try (shell/execute "echo" token)
                                (catch InterruptedException _ :refused)))))]
    (.start worker)
    (is (= :refused (deref outcome 5000 :no-answer)))
    (is (not (recorded-with token)) "the reproducer lists only commands that ran")))

(deftest a-command-that-could-not-be-launched-is-not-recorded
  (let [binary (str "this-binary-does-not-exist-" (random-uuid))]
    (is (= 127 (:exit (shell/execute binary))))
    (is (not (recorded-with binary)))))

(deftest a-command-that-ran-is-recorded
  (let [token (str "ran-" (random-uuid))]
    (is (= 0 (:exit (shell/execute "echo" token))))
    (is (recorded-with token))))
