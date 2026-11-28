;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.kuberc-test
  "Every child gumshoe starts runs with KUBERC=off, so the operator's kuberc
   can neither rewrite a confirmed command nor make a delete prompt. The
   end-to-end tests run fresh bb processes with a fake kubectl first on PATH
   that behaves like a kubectl honouring a kuberc: it records the KUBERC it
   sees, and while a kuberc is in effect it annotates what it reads and
   answers a delete with an interactive prompt."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [briko.process :as briko]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.recording :as recording]
            [gumshoe.reproducer :as reproducer]
            [gumshoe.shell :as shell]))

;; --- in process: every wrapper hands babashka.process or briko.process the setting -----------------------

(deftest every-shell-wrapper-starts-its-child-with-kuberc-off
  (let [seen (atom [])
        record (fn [opts & _] (swap! seen conj (get-in opts [:extra-env "KUBERC"]))
                 (delay {:exit 0 :out "" :err ""}))]
    (with-redefs [process/process record
                  process/shell (fn [opts & args] @(apply record opts args))
                  ;; captured commands run through briko.process
                  briko/run (fn [_argv opts]
                              (swap! seen conj (get-in opts [:extra-env "KUBERC"]))
                              {:exit 0 :out "" :err "" :launched? true})
                  ;; and children started alongside through briko.process/start
                  briko/start (fn [_argv opts]
                                (swap! seen conj (get-in opts [:extra-env "KUBERC"]))
                                {:pid nil :launched? false})]
      (shell/execute "kubectl" "get" "pods")
      (shell/execute-with-stdin "{}" "kubectl" "apply" "-f" "-")
      (shell/execute-env {"PGPASSWORD" "x"} "kubectl" "exec" "p")
      (shell/stdout-of "kubectl" "version")
      (shell/exit-code-of "kubectl" "auth" "can-i" "get" "pods")
      (shell/run-with-output "kubectl" "delete" "pod" "p")
      (shell/run-with-output-env {"A" "b"} "kubectl" "port-forward" "svc/s" "1:1")
      (shell/capture-line "gum" "input")
      (shell/pipe-to! "x" "wl-copy")
      (shell/start {} "kubectl" "port-forward" "svc/s" "1:1")
      (shell/start-alongside ["kubectl" "port-forward" "svc/s" "1:1"] {:extra-env {"KUBERC" "/home/op/.kube/kuberc"}}))
    (is (= 11 (count @seen)))
    (is (every? #{"off"} @seen) (pr-str @seen))))

(deftest a-caller-cannot-hand-a-child-its-own-kuberc
  (let [seen (atom nil)]
    (with-redefs [briko/run (fn [_argv opts] (reset! seen (:extra-env opts)) {:exit 0 :out "" :err "" :launched? true})]
      (shell/execute-env {"KUBERC" "/home/op/.kube/kuberc" "PGPASSWORD" "x"} "kubectl" "get" "pods"))
    (is (= {"KUBERC" "off" "PGPASSWORD" "x"} @seen))))

;; --- nothing spawns past gumshoe.shell --------------------------------------------------

(def ^:private spawning-fns
  "babashka.process functions that start a process."
  #{"process" "shell" "sh" "exec" "start" "pipeline" "pb" "$"})

(defn- spawn-sites
  "The spawn calls in one source file outside gumshoe.shell: a babashka.process
   start under whatever alias the file gives it, the fully qualified form, a
   java.lang.ProcessBuilder, Runtime.exec or clojure.java.shell."
  [text]
  (let [aliases (into #{"babashka.process"}
                      (map second (re-seq #"\[babashka\.process\s+:as\s+([\w.-]+)" text)))]
    (concat
     (for [alias aliases
           [_ f] (re-seq (re-pattern (str "(?<![\\w.-])" (java.util.regex.Pattern/quote alias) "/([\\w$*-]+)")) text)
           :when (spawning-fns f)]
       (str alias "/" f))
     (re-seq #"ProcessBuilder\.|\.exec\s*\(|\(\.exec\b|clojure\.java\.shell" text))))

(def ^:private allowed
  "Spawn sites outside gumshoe.shell, each with the only command it starts.
   stdout asks `stty size` for the terminal's width on an inherited stdin.
   The refusal test helper names the spawning functions only to replace
   them with one that refuses, and starts nothing."
  {"libraries/gumshoe/stdout.clj" #"ProcessBuilder\. \[\"stty\" \"size\"\]"
   "libraries/gumshoe/testing/refusal.clj" #"#'process/process refuse\s+#'process/shell refuse\s+#'process/sh refuse\s+#'java-shell/sh refuse"})

(deftest nothing-starts-a-process-outside-the-shell-namespace
  (let [roots (concat ["libraries" "runbooks" "playbooks" "firebooks" "examples" "casebook-example"]
                      (map str (fs/glob "tools" "*/{libraries,runbooks,playbooks}")))
        files (for [root roots :when (fs/exists? root)
                    f (fs/glob root "**.{clj,bb,cljc}")]
                (str/replace (str f) "\\" "/"))
        sites (into {} (for [f files
                             :when (not= f "libraries/gumshoe/shell.clj")
                             :let [text (slurp f)
                                   found (spawn-sites text)]
                             :when (seq found)
                             :when (not (some-> (allowed f) (re-find text)))]
                         [f (vec found)]))]
    (is (< 100 (count files)) "the scan read the source tree")
    (is (= {} sites) "start children through gumshoe.shell (shell/start for a managed one), so KUBERC=off applies")))

(deftest the-scan-finds-each-way-of-spawning
  (is (seq (spawn-sites "(:require [babashka.process :as p]) (p/process [\"kubectl\"])")))
  (is (seq (spawn-sites "(:require [babashka.process :as proc]) (apply proc/shell {} args)")))
  (is (seq (spawn-sites "(babashka.process/sh \"kubectl\" \"get\")")))
  (is (seq (spawn-sites "(.start (ProcessBuilder. [\"kubectl\"]))")))
  (is (seq (spawn-sites "(.exec (Runtime/getRuntime) \"kubectl\")")))
  (is (seq (spawn-sites "(:require [clojure.java.shell :as sh])")))
  (is (empty? (spawn-sites "(:require [babashka.process :as process]) (process/alive? p) (process/destroy-tree p)"))))

;; --- end to end: a fake kubectl that honours a kuberc -----------------------------------

(def ^:private fake-kubectl
  (str "#!/usr/bin/env bb\n"
       (pr-str
        '(do
           (require '[babashka.fs :as fs] '[cheshire.core :as json] '[clojure.string :as str])
           (def argv (vec *command-line-args*))
           (def kuberc (System/getenv "KUBERC"))
           (spit (System/getenv "FAKE_LOG")
                 (str (pr-str {:program "kubectl" :kuberc kuberc :argv argv}) "\n") :append true)
           (def active? (and (not= "off" kuberc)
                             (if (str/blank? kuberc)
                               (fs/exists? (fs/path (System/getenv "HOME") ".kube" "kuberc"))
                               (fs/exists? kuberc))))
           (defn has [& words] (every? (set argv) words))
           (def object {:apiVersion "v1" :kind "ConfigMap"
                        :metadata (cond-> {:name "web" :namespace "shop" :uid "u-1" :resourceVersion "1"
                                           :creationTimestamp "2026-01-01T00:00:00Z" :labels {} :annotations {}}
                                    active? (assoc-in [:annotations :kuberc] "applied"))
                        :data {}})
           (cond
             (has "delete") (if active?
                              (do (println "You are about to delete configmaps/web. Do you want to continue? (y/N)")
                                  (System/exit 1))
                              (println "configmap \"web\" deleted (server dry run)"))
             (has "port-forward") (System/exit 1)
             (has "can-i") (println "yes")
             (has "config" "view") (println (json/generate-string
                                             {:current-context "ctx"
                                              :contexts [{:name "ctx" :context {:cluster "kind" :namespace "shop"}}]}))
             (has "get") (let [output (or (some #(when (str/starts-with? % "--output=") (subs % 9)) argv)
                                          (second (drop-while #(not= "-o" %) argv)))]
                           (case output
                             "json" (println (json/generate-string
                                              (if (some #{"web"} argv)
                                                object
                                                {:apiVersion "v1" :kind "List" :items [object]})))
                             "name" (println "configmaps/web")
                             nil))
             :else nil)))
       "\n"))

(def ^:private fake-logger
  "Any other tool: records the KUBERC it saw and fails, so no clipboard,
   picker or prompt runs."
  (str "#!/bin/sh\n"
       "printf '{:program \"%s\" :kuberc \"%s\"}\\n' \"$(basename \"$0\")\" \"${KUBERC-}\" >> \"$FAKE_LOG\"\n"
       "exit 1\n"))

(defn- fake-bin!
  [dir]
  (let [bin (fs/path dir "bin")]
    (fs/create-dirs bin)
    (doseq [[name script] (into [["kubectl" fake-kubectl]]
                                (for [tool ["fzf" "gum" "wl-copy" "xclip" "xsel" "pbcopy"]] [tool fake-logger]))]
      (spit (str (fs/path bin name)) script)
      (fs/set-posix-file-permissions (fs/path bin name) "rwx------"))
    (str bin)))

(defn- interactive-kuberc!
  "A kuberc making every delete interactive, at path."
  [path]
  (fs/create-dirs (fs/parent path))
  (spit (str path) (str "apiVersion: kubectl.config.k8s.io/v1beta1\n"
                        "kind: Preference\n"
                        "defaults:\n"
                        "- command: delete\n"
                        "  options:\n"
                        "  - name: interactive\n"
                        "    default: \"true\"\n")))

(defn- absolute-classpath []
  (->> (str/split (System/getProperty "java.class.path") #":")
       (map #(str (fs/absolutize %)))
       (str/join ":")))

(defn- run-bb
  "Runs bb with args in dir, the fakes first on PATH, offline settings and
   env merged in, stdin closed. KUBERC is removed from the inherited
   environment unless env names it."
  [dir env & args]
  (let [log (str (fs/path dir "log.edn"))
        base (-> (into {} (System/getenv))
                 (dissoc "KUBERC")
                 (merge {"PATH" (str (fake-bin! dir) ":" (System/getenv "PATH"))
                         "HOME" (str (fs/path dir "home"))
                         "FAKE_LOG" log
                         "KUBECONFIG" "/dev/null"
                         "GUMSHOE_ENV" (str (fs/path dir "nonexistent.edn"))
                         "XDG_STATE_HOME" (str (fs/path dir "state"))
                         "NO_COLOR" "1"}
                        env))
        result @(apply process/process {:dir (str dir) :out :string :err :string :continue true
                                        :in "" :env base}
                       "bb" "-cp" (absolute-classpath) args)]
    (assoc result :log (if (fs/exists? log)
                         (mapv edn/read-string (str/split-lines (slurp log)))
                         []))))

(def ^:private every-path
  "Reads, can-i, effects of every kubectl kind, a shell wrapper, a
   port-forward, a caller's own KUBERC and a picker, each the way books
   reach them."
  (pr-str
   '(do
      (require '[gumshoe.kubectl :as kubectl] '[gumshoe.effect :as effect]
               '[gumshoe.shell :as shell] '[gumshoe.fuzzy-finder :as fuzzy])
      (kubectl/get-json "ctx" "configmaps" "--namespace=shop")
      (kubectl/can-i? "delete" "configmaps")
      (effect/run! [(effect/kubectl "ctx" "--namespace=shop" "delete" "configmaps" "web")])
      (effect/run! [(effect/kubectl-stdin "ctx" "{}" "apply" "-f" "-")])
      (effect/run! [(effect/kubectl-plugin "ctx" "cnpg" "status" "pg")])
      (effect/run! [[:cmd "sh" "-c" "kubectl get namespaces"]])
      (shell/execute-env {"KUBERC" (str (System/getenv "HOME") "/.kube/kuberc")} "kubectl" "get" "nodes")
      (kubectl/with-port-forward {:context "ctx" :namespace "shop" :service "web"
                                  :local-port (with-open [s (java.net.ServerSocket. 0)] (.getLocalPort s))
                                  :remote-port 80}
        (fn [] :forwarded))
      (try (fuzzy/select-single "Object" ["a" "b"] {:auto-select? false
                                                    :preview "kubectl describe {}"})
           (catch Exception _ nil))
      (println "DONE"))))

(deftest every-kubectl-call-sees-kuberc-off-even-with-a-kuberc-in-place
  (fs/with-temp-dir [dir {}]
    (let [kuberc (fs/path dir "home" ".kube" "kuberc")]
      (interactive-kuberc! kuberc)
      (let [{:keys [out err log]} (run-bb dir {"KUBERC" (str kuberc)} "-e" every-path)
            calls (group-by :program log)
            argvs (map :argv (calls "kubectl"))]
        (is (str/includes? out "DONE") (str out err))
        (testing "each path reached the fake"
          (is (some #(some #{"get"} %) argvs) "a read")
          (is (some #(some #{"can-i"} %) argvs) "a can-i")
          (is (some #(= ["--context=ctx" "--namespace=shop" "delete" "configmaps" "web"] %) argvs) "an effect")
          (is (some #(some #{"apply"} %) argvs) "a stdin effect")
          (is (some #(= "cnpg" (first %)) argvs) "a plugin")
          (is (some #(= ["get" "namespaces"] %) argvs) "a shell wrapper")
          (is (some #(= ["get" "nodes"] %) argvs) "a caller's own KUBERC")
          (is (some #(some #{"port-forward"} %) argvs) "a port-forward")
          (is (seq (calls "fzf")) "the picker, whose preview runs kubectl"))
        (is (= #{"off"} (set (map :kuberc log))) (pr-str log))
        (is (not (str/includes? (str out err) "Do you want to continue")) "no delete prompted")))))

(def ^:private delete-book
  (str (fs/absolutize "runbooks/kubernetes/objects/delete.clj")))

(defn- dry-run-output
  "What --dry-run of the delete book prints, with the recording's
   timestamped directory taken out."
  [dir env]
  (let [{:keys [exit out err log]} (run-bb dir env delete-book "--dry-run" "--pick" "configmaps shop/web")]
    {:exit exit
     :text (str/replace (str out "\n--- stderr ---\n" err) #"recordings/\S+" "recordings/<run>")
     :log log}))

(deftest a-dry-run-plan-is-the-same-with-and-without-a-kuberc
  (fs/with-temp-dir [with-dir {}]
    (fs/with-temp-dir [without-dir {}]
      (let [kuberc (fs/path with-dir "home" ".kube" "kuberc")
            _ (interactive-kuberc! kuberc)
            with (dry-run-output with-dir {"KUBERC" (str kuberc)})
            without (dry-run-output without-dir {})]
        (is (zero? (:exit with)) (:text with))
        (is (str/includes? (:text with) "the API server accepted a dry run: configmap \"web\" deleted (server dry run)")
            "the server dry run answered without a prompt")
        (is (str/includes? (:text with) "would run: kubectl --namespace=shop delete configmaps web --ignore-not-found --interactive=false --wait=false"))
        (is (= (:text without) (:text with)) "byte-identical plans")
        (is (not (str/includes? (:text with) "KUBERC")) "the plan shows the command, not our environment")
        (is (seq (:log with)))
        (is (= #{"off"} (set (map :kuberc (:log with)))))))))

(deftest the-default-kuberc-location-is-ignored-too
  (fs/with-temp-dir [dir {}]
    (interactive-kuberc! (fs/path dir "home" ".kube" "kuberc"))
    (let [{:keys [exit text log]} (dry-run-output dir {})]
      (is (zero? exit) text)
      (is (str/includes? text "the API server accepted a dry run"))
      (is (= #{"off"} (set (map :kuberc log)))))))

;; --- replayed commands set it too -------------------------------------------------------

(deftest a-reproducer-turns-the-kuberc-off-before-its-commands
  (let [script (reproducer/script-content "objects/delete" ["kubectl get pods"])]
    (is (str/includes? script "\nexport KUBERC=off\nkubectl get pods\n"))))

(deftest a-recording-replays-with-the-kuberc-off
  (fs/with-temp-dir [dir {}]
    (with-redefs [recording/base-directory (str dir)]
      (shell/execute "true" "kuberc-recording-probe")
      (let [path (recording/save! {:book-file "runbooks/x.clj" :description "x" :opts {} :outcome :ok})
            commands (slurp (str (fs/path path "commands.sh")))]
        (is (re-find #"(?m)^export KUBERC=off$" commands))
        (is (< (str/index-of commands "export KUBERC=off")
               (str/index-of commands "true kuberc-recording-probe")))))))

(deftest the-replay-preamble-names-every-child-variable
  (is (= "export KUBERC=off\n" (shell/replay-preamble))))
