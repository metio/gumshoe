;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.port-forward-reuse-test
  "Consecutive forwards on one local port, as delete_metric runs with --port
   over every pod: each forward has exited and released the port before
   with-port-forward returns, and one that does not exit in time is reported
   as our own, never as another program holding the port. Each scenario runs
   in a fresh bb against a fake kubectl that binds the port and takes a while
   to exit once signalled."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]))

(defn- absolute-classpath []
  (->> (str/split (System/getProperty "java.class.path") #":")
       (map #(str (fs/absolutize %)))
       (str/join ":")))

(defn- fake-kubectl!
  "Writes a bb kubectl into dir that binds the forwarded local port, reports
   it forwarded, and on SIGTERM keeps the port for exit-ms before it exits,
   as a kubectl closing its streams does."
  [dir exit-ms]
  (let [path (fs/path dir "kubectl")]
    (spit (str path)
          (str "#!/usr/bin/env bb\n"
               "(def local (parse-long (first (clojure.string/split (last *command-line-args*) #\":\"))))\n"
               "(def s (java.net.ServerSocket. local 50 (java.net.InetAddress/getByName \"127.0.0.1\")))\n"
               "(.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (Thread/sleep " exit-ms "))))\n"
               "(println (str \"Forwarding from 127.0.0.1:\" local \" -> 9090\"))\n"
               "(flush)\n"
               "(future (Thread/sleep 30000) (System/exit 0))\n"
               "(while true (.close (.accept s)))\n"))
    (fs/set-posix-file-permissions path "rwxr-xr-x")))

(defn- run-script
  [dir script]
  (let [result @(process/process {:dir (str dir) :out :string :err :string :continue true
                                  :extra-env {"PATH" (str dir ":" (System/getenv "PATH"))
                                              "GUMSHOE_ENV" (str (fs/path dir "env.edn"))
                                              "KUBECONFIG" "/dev/null"
                                              "NO_COLOR" "1"}}
                                 "bb" "-cp" (absolute-classpath) "-e" script)]
    (assoc result :value (try (edn/read-string (last (str/split-lines (str/trim (:out result)))))
                              (catch Exception _ nil)))))

(defn- forwards-script
  "Runs with-port-forward once per pod on port, pausing pause-ms before
   each pod named in pause-before, and prints each result. exit-ms binds
   *forward-exit-ms*, nil keeps its default."
  [port pods exit-ms pause-before pause-ms]
  (str "(require '[gumshoe.kubectl :as kubectl])"
       "(def results (binding [" (if exit-ms (str "kubectl/*forward-exit-ms* " exit-ms) "") "]"
       "  (mapv (fn [pod]"
       "          (when (contains? " (pr-str (set pause-before)) " pod) (Thread/sleep " pause-ms "))"
       "          [pod (kubectl/with-port-forward"
       "                 {:context \"c\" :namespace \"m\" :pod pod :local-port " port " :remote-port 9090}"
       "                 (fn [] :ran))])"
       "        " (pr-str pods) ")))"
       "(println (pr-str results))"))

(deftest every-pod-runs-on-the-same-local-port
  (let [dir (fs/create-temp-dir)
        port (kubectl/free-local-port)]
    (try
      (fake-kubectl! dir 400)
      (let [{:keys [value err]} (run-script dir (forwards-script port ["prometheus-k8s-0" "prometheus-k8s-1" "prometheus-k8s-2"]
                                                                 nil #{} 0))]
        (is (= [["prometheus-k8s-0" :ran] ["prometheus-k8s-1" :ran] ["prometheus-k8s-2" :ran]] value) err)
        (is (not (str/includes? err "already in use")) err))
      (finally (fs/delete-tree dir)))))

(deftest a-forward-that-outlives-its-bound-is-reported-as-our-own
  (let [dir (fs/create-temp-dir)
        port (kubectl/free-local-port)]
    (try
      (fake-kubectl! dir 2500)
      (let [{:keys [value err]} (run-script dir (forwards-script port ["a" "b" "c"] 300 #{"c"} 3500))]
        (testing "the first pod ran and its slow exit is named"
          (is (= ["a" :ran] (first value)) err)
          (is (str/includes? err (format "local port %d is still held by our own forward" port)) err))
        (testing "the next pod is refused as held by our own forward, never as another program"
          (is (= ["b" false] (second value)) err)
          (is (str/includes? err (format "local port %d is still held by our own earlier kubectl port-forward" port)) err)
          (is (not (str/includes? err "already in use")) err))
        (testing "once our forward exited, the port is used again"
          (is (= ["c" :ran] (nth value 2)) err)))
      (finally (fs/delete-tree dir)))))
