;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.port-forward-ownership-test
  "with-port-forward runs its body only against its own kubectl port-forward:
   never against another program holding the local port, never after the
   forward died. Each scenario runs in a fresh bb against a fake kubectl, so
   the test sees the real stdout and stderr."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.postgres :as postgres])
  (:import [java.net InetAddress InetSocketAddress ServerSocket Socket]))

(defn- absolute-classpath []
  (->> (str/split (System/getProperty "java.class.path") #":")
       (map #(str (fs/absolutize %)))
       (str/join ":")))

(defn- loopback [] (InetAddress/getByName "127.0.0.1"))

(defn- fake-kubectl!
  "Writes a bb kubectl into dir whose body is the given forms, with `local`
   bound to the forwarded local port (a long) and `workdir` to the directory. It
   touches started first, so a test can tell whether it ran at all."
  [dir & forms]
  (let [path (fs/path dir "kubectl")]
    (spit (str path)
          (str "#!/usr/bin/env bb\n"
               "(def workdir " (pr-str (str dir)) ")\n"
               "(spit (str workdir \"/started\") \"\")\n"
               "(def local (parse-long (first (clojure.string/split (last *command-line-args*) #\":\"))))\n"
               "(defn squat! []\n"
               "  (babashka.process/process {:out :discard :err :discard :in :discard}\n"
               "    \"bb\" \"-e\" (str \"(spit \\\"\" workdir \"/squatter.pid\\\" (.pid (java.lang.ProcessHandle/current)))\"\n"
               "                    \"(def s (java.net.ServerSocket. \" local \" 50 (java.net.InetAddress/getByName \\\"127.0.0.1\\\")))\"\n"
               "                    \"(spit \\\"\" workdir \"/squatting\\\" \\\"\\\")\"\n"
               "                    \"(future (Thread/sleep 30000) (System/exit 0))\"\n"
               "                    \"(while true (.close (.accept s)))\"))\n"
               "  (while (not (babashka.fs/exists? (str workdir \"/squatting\"))) (Thread/sleep 20)))\n"
               (str/join "\n" forms)
               "\n"))
    (fs/set-posix-file-permissions path "rwxr-xr-x")))

(defn- kill-squatter! [dir]
  (let [pid-file (fs/file dir "squatter.pid")]
    (when (fs/exists? pid-file)
      (process/shell {:continue true :err :string} "kill" "-KILL" (str/trim (slurp pid-file))))))

(defn- run-forward
  "Runs with-port-forward on port in a fresh bb whose body records that it
   ran and whether the forward was alive inside it."
  ([dir port] (run-forward dir port 30000))
  ([dir port start-ms]
   (let [script (str "(require '[gumshoe.kubectl :as kubectl])"
                     "(def ran (atom nil))"
                     "(def r (binding [kubectl/*forward-start-ms* " start-ms "]"
                     "  (kubectl/with-port-forward"
                     "    {:context \"c\" :namespace \"n\" :service \"s\" :local-port " port " :remote-port 80}"
                     "    (fn [] (reset! ran {:alive? (kubectl/port-forward-alive?)})"
                     "           (when (babashka.fs/exists? \"" dir "/die-during-body\")"
                     "             (babashka.fs/create-file \"" dir "/die-now\")"
                     "             (Thread/sleep 1500))"
                     "           :forwarded))))"
                     "(println (pr-str {:result r :ran @ran}))")
         result @(process/process {:dir (str dir) :out :string :err :string :continue true
                                   :extra-env {"PATH" (str dir ":" (System/getenv "PATH"))
                                               "GUMSHOE_ENV" (str (fs/path dir "env.edn"))
                                               "KUBECONFIG" "/dev/null"
                                               "NO_COLOR" "1"}}
                                  "bb" "-cp" (absolute-classpath) "-e" script)]
     (assoc result :value (try (edn/read-string (last (str/split-lines (str/trim (:out result)))))
                               (catch Exception _ nil))))))

(def ^:private listen-forever
  ["(def s (java.net.ServerSocket. local 50 (java.net.InetAddress/getByName \"127.0.0.1\")))"
   "(println (str \"Forwarding from 127.0.0.1:\" local \" -> 80\"))"
   "(println (str \"Forwarding from [::1]:\" local \" -> 80\"))"
   "(flush)"
   "(future (while true (.close (.accept s)) (println \"Handling connection for\" local) (flush)))"
   "(while true"
   "  (when (babashka.fs/exists? (str workdir \"/die-now\")) (System/exit 1))"
   "  (Thread/sleep 50))"])

(deftest the-body-runs-against-our-own-bound-forward
  (fs/with-temp-dir [dir {}]
    (apply fake-kubectl! dir listen-forever)
    (let [{:keys [value out err]} (run-forward dir (kubectl/free-local-port))]
      (is (= {:result :forwarded :ran {:alive? true}} value) (str out err))
      (is (str/includes? err "Forwarding from 127.0.0.1:") err))))

(deftest a-forward-that-loses-the-port-to-another-listener-never-runs-the-body
  (fs/with-temp-dir [dir {}]
    (fake-kubectl! dir
                   "(squat!)"
                   "(binding [*out* *err*] (println (str \"Unable to listen on port \" local \": address already in use\")))"
                   "(System/exit 1)")
    (try
      (let [{:keys [value out err]} (run-forward dir (kubectl/free-local-port))]
        (testing "the other program held the port while the book waited"
          (is (fs/exists? (fs/file dir "squatting"))))
        (is (= {:result false :ran nil} value) (str out err))
        (is (str/includes? err "exited before the port opened") err))
      (finally (kill-squatter! dir)))))

(deftest a-forward-bound-only-on-ipv6-never-runs-the-body-against-the-ipv4-squatter
  (fs/with-temp-dir [dir {}]
    (fake-kubectl! dir
                   "(squat!)"
                   "(println (str \"Forwarding from [::1]:\" local \" -> 80\"))"
                   "(flush)"
                   "(Thread/sleep 60000)")
    (try
      (let [{:keys [value out err]} (run-forward dir (kubectl/free-local-port) 2000)]
        (is (= {:result false :ran nil} value) (str out err))
        (is (str/includes? err "did not report 127.0.0.1:") err))
      (finally (kill-squatter! dir)))))

(deftest a-listening-port-is-refused-before-kubectl-starts
  (fs/with-temp-dir [dir {}]
    (apply fake-kubectl! dir listen-forever)
    (with-open [held (ServerSocket. 0 50 (loopback))]
      (let [{:keys [value out err]} (run-forward dir (.getLocalPort held))]
        (is (= {:result false :ran nil} value) (str out err))
        (is (str/includes? err "already in use") err)
        (is (not (fs/exists? (fs/file dir "started"))) "kubectl never started")))))

(deftest a-port-bound-without-listening-is-refused-before-kubectl-starts
  (fs/with-temp-dir [dir {}]
    (apply fake-kubectl! dir listen-forever)
    (let [port (kubectl/free-local-port)]
      (with-open [held (doto (Socket.) (.bind (InetSocketAddress. (loopback) (int port))))]
        (is (.isBound held))
        (let [{:keys [value out err]} (run-forward dir port)]
          (is (= {:result false :ran nil} value) (str out err))
          (is (str/includes? err "already in use") err)
          (is (not (fs/exists? (fs/file dir "started"))) "kubectl never started"))))))

(deftest a-forward-that-dies-while-the-body-runs-fails-the-result
  (fs/with-temp-dir [dir {}]
    (apply fake-kubectl! dir listen-forever)
    (fs/create-file (fs/path dir "die-during-body"))
    (let [{:keys [value out err]} (run-forward dir (kubectl/free-local-port))]
      (is (= {:result false :ran {:alive? true}} value) (str out err))
      (is (str/includes? err "exited while it was in use") err))))

(deftest no-local-port-is-refused
  (is (false? (kubectl/with-port-forward {:context "c" :namespace "n" :service "s" :local-port nil :remote-port 80}
                (fn [] (throw (ex-info "the body must not run" {})))))))

(deftest port-forward-alive-is-false-outside-a-forward
  (is (false? (kubectl/port-forward-alive?))))

(deftest forwarded-port-reads-only-the-ipv4-listener-line
  (is (= 42538 (kubectl/forwarded-port "Forwarding from 127.0.0.1:42538 -> 5432")))
  (is (nil? (kubectl/forwarded-port "Forwarding from [::1]:42538 -> 5432")))
  (is (nil? (kubectl/forwarded-port "Handling connection for 42538")))
  (is (nil? (kubectl/forwarded-port "Unable to listen on port 42538: address already in use")))
  (is (nil? (kubectl/forwarded-port "x Forwarding from 127.0.0.1:42538 -> 5432")))
  (is (nil? (kubectl/forwarded-port nil))))

(deftest a-free-local-port-is-free-and-an-explicit-one-is-kept
  (let [port (kubectl/free-local-port)]
    (is (< 0 port 65536))
    (is (not (kubectl/local-port-taken? port))))
  (is (= 4242 (kubectl/local-port-or-free 4242)))
  (is (pos? (kubectl/local-port-or-free nil)))
  (with-open [held (ServerSocket. 0 50 (loopback))]
    (is (kubectl/local-port-taken? (.getLocalPort held)))))

(deftest with-postgres-hands-the-forwarded-port-to-the-body
  (let [specs (atom [])]
    (with-redefs [kubectl/with-port-forward (fn [spec f] (swap! specs conj spec) (f))]
      (testing "an explicit port is forwarded and handed on"
        (is (= 4242 (postgres/with-postgres "c" {:namespace "db" :service "pg"} 4242 identity)))
        (is (= 4242 (:local-port (last @specs)))))
      (testing "without one, a free port is chosen, forwarded and handed on"
        (let [port (postgres/with-postgres "c" {:namespace "db" :service "pg"} nil identity)]
          (is (pos? port))
          (is (= port (:local-port (last @specs)))))))))
