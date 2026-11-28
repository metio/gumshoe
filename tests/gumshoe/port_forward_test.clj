;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.port-forward-test
  "kubectl/with-port-forward against a fake kubectl, run in a fresh bb so the
   test sees what reaches the real stdout and stderr file descriptors."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [java.net InetAddress ServerSocket]))

(defn- absolute-classpath []
  (->> (str/split (System/getProperty "java.class.path") #":")
       (map #(str (fs/absolutize %)))
       (str/join ":")))

(defn- free-port []
  (with-open [s (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))]
    (.getLocalPort s)))

(defn- fake-kubectl!
  "Writes a kubectl into dir that prints port-forward's chatter on stdout and,
   when listen? is true, listens on the forwarded local port until killed."
  [dir listen?]
  (let [path (fs/path dir "kubectl")]
    (spit (str path)
          (str "#!/usr/bin/env bb\n"
               "(let [[local _] (clojure.string/split (last *command-line-args*) #\":\")]\n"
               "  (println (str \"Forwarding from 127.0.0.1:\" local \" -> 80\"))\n"
               "  (flush)\n"
               (if listen?
                 (str "  (let [s (java.net.ServerSocket. (parse-long local) 50 (java.net.InetAddress/getByName \"127.0.0.1\"))]\n"
                      "    (while true (.close (.accept s)) (println \"Handling connection for\" local) (flush))))\n")
                 "  (System/exit 1))\n")))
    (fs/set-posix-file-permissions path "rwxr-xr-x")))

(defn- run-forward
  "Runs with-port-forward on port in a fresh bb. body is the source of the
   function the forward runs, path the PATH the book sees."
  ([dir port] (run-forward dir port "(fn [] :forwarded)" (str dir ":" (System/getenv "PATH"))))
  ([dir port body path]
   (let [script (str "(require '[gumshoe.kubectl :as kubectl])"
                     "(println (pr-str {:result (kubectl/with-port-forward"
                     "  {:context \"c\" :namespace \"n\" :service \"s\" :local-port " port " :remote-port 80}"
                     "  " body ")}))")]
     @(process/process {:dir (str dir) :out :string :err :string :continue true
                        :extra-env {"PATH" path
                                    "GUMSHOE_ENV" (str (fs/path dir "env.edn"))
                                    "KUBECONFIG" "/dev/null"
                                    "NO_COLOR" "1"}}
                       (str (fs/which "bb")) "-cp" (absolute-classpath) "-e" script))))

(deftest port-forward-chatter-stays-off-stdout
  (fs/with-temp-dir [dir {}]
    (fake-kubectl! dir true)
    (let [{:keys [out err]} (run-forward dir (free-port))]
      (testing "stdout carries only the result"
        (is (= "{:result :forwarded}" (str/trim out)) (str out err)))
      (testing "kubectl's lines still reach the operator on stderr"
        (is (str/includes? err "Forwarding from") err)))))

(deftest a-port-forward-that-exits-stops-the-wait
  (fs/with-temp-dir [dir {}]
    (fake-kubectl! dir false)
    (let [started (System/nanoTime)
          {:keys [out err]} (run-forward dir (free-port))
          seconds (/ (- (System/nanoTime) started) 1e9)]
      (is (= "{:result false}" (str/trim out)) (str out err))
      (is (str/includes? err "exited before the port opened") err)
      (is (< seconds 20) "the wait ends when kubectl exits, not after 30 attempts"))))

(deftest each-connection-kubectl-reports-reaches-the-operator-on-stderr
  (fs/with-temp-dir [dir {}]
    (fake-kubectl! dir true)
    (let [port (free-port)
          body (str "(fn [] (with-open [_ (java.net.Socket. \"127.0.0.1\" " port ")] (Thread/sleep 300)) :forwarded)")
          {:keys [out err]} (run-forward dir port body (str dir ":" (System/getenv "PATH")))]
      (is (= "{:result :forwarded}" (str/trim out)) (str out err))
      (is (str/includes? err (str "Handling connection for " port)) err))))

(deftest a-kubectl-that-cannot-be-started-fails-the-forward
  (fs/with-temp-dir [dir {}]
    ;; PATH holds only a link to mkfifo, which briko needs for its pipes,
    ;; so only kubectl is missing, also where kubectl sits beside mkfifo
    (let [tools (str (fs/create-dirs (fs/path dir "tools")))
          _ (fs/create-sym-link (fs/path tools "mkfifo") (fs/which "mkfifo"))
          _ (is (nil? (fs/which "kubectl" {:paths [tools]})) "the PATH the book sees holds no kubectl")
          {:keys [out err]} (run-forward dir (free-port) "(fn [] :forwarded)" tools)]
      (is (= "{:result false}" (str/trim out)) (str out err))
      (is (str/includes? err "could not start kubectl port-forward") err)
      (is (not (str/includes? err "output pipes")) "briko created its pipes, so kubectl itself was missing"))))
