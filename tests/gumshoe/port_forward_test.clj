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

(defn- run-forward [dir port]
  (let [script (str "(require '[gumshoe.kubectl :as kubectl])"
                    "(println (pr-str {:result (kubectl/with-port-forward"
                    "  {:context \"c\" :namespace \"n\" :service \"s\" :local-port " port " :remote-port 80}"
                    "  (fn [] :forwarded))}))")]
    @(process/process {:dir (str dir) :out :string :err :string :continue true
                       :extra-env {"PATH" (str dir ":" (System/getenv "PATH"))
                                   "GUMSHOE_ENV" (str (fs/path dir "env.edn"))
                                   "NO_COLOR" "1"}}
                      "bb" "-cp" (absolute-classpath) "-e" script)))

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
