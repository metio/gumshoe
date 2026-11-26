;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.upload-evidence-test
  "The upload chain from the exec answers of a namespace's containers to the
   findings of upload-limits. The nginx and PHP outputs under
   tools/upload/tests/fixtures are recorded from public images: nginx -T of
   docker.io/nginxinc/nginx-unprivileged (nginx 1.31.7) with and without a
   conf.d file setting client_max_body_size 64m, nginx -T of
   docker.io/library/nginx (1.31.10) failing as a non-root user, and the core
   section of php -i of docker.io/library/php:8-fpm (8.6.0) without its
   System line."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.upload :as upload]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.phpstack :as phpstack]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(defn- fixture
  [file]
  (slurp (io/file "tools/upload/tests/fixtures" file)))

(def ^:private nginx-64m (delay (fixture "nginx-T-64m.txt")))
(def ^:private nginx-default (delay (fixture "nginx-T-default.txt")))
(def ^:private nginx-failed-stderr (delay (fixture "nginx-T-failed.stderr.txt")))
(def ^:private php-info (delay (fixture "php-i.txt")))

(defn- answered [out] {:exit 0 :out out :err ""})

(def ^:private not-found
  "What kubectl exec returns for a command the container does not have."
  {:exit 1 :out ""
   :err "OCI runtime exec failed: exec failed: unable to start container process: exec: \"nginx\": executable file not found in $PATH: unknown\ncommand terminated with exit code 126\n"})

(def ^:private php-not-found
  {:exit 1 :out ""
   :err "OCI runtime exec failed: exec failed: unable to start container process: exec: \"php\": executable file not found in $PATH: unknown\ncommand terminated with exit code 126\n"})

(defn- pods-json
  [containers]
  (str "{\"items\":[{\"metadata\":{\"name\":\"app-0\"},\"spec\":{\"containers\":["
       (str/join "," (map #(format "{\"name\":\"%s\"}" %) containers))
       "]}}]}"))

(defn- exec-target
  "The container and the command of a kubectl exec argv."
  [argv]
  (let [args (vec (flatten argv))
        container (some (fn [[flag value]] (when (= "-c" flag) value)) (partition 2 1 args))
        command (vec (rest (drop-while #(not= "--" %) args)))]
    [container command]))

(defn- fake-cluster
  "A shell/execute that answers the pod list of one pod holding the named
   containers, and each exec from execs, keyed by [container command]. An exec
   it was not given answers like a container without that command."
  [containers execs]
  (fn [& argv]
    (let [args (flatten argv)]
      (cond
        (some #{"exec"} args) (let [[container command] (exec-target args)]
                                (get execs [container command]
                                     (if (= ["php" "-i"] command) php-not-found not-found)))
        (some #{"get"} args) (answered (pods-json containers))
        :else {:exit 1 :out "" :err "unexpected command"}))))

(defn- collect
  [containers execs]
  (with-redefs [shell/execute (fake-cluster containers execs)
                stdout/print-section (fn [& _])
                stdout/err-println (fn [& _])]
    (phpstack/collect-evidence! "ctx" "moodle")))

(defn- judge
  "The upload-limits check as a report shows it: whether it counts as unread,
   and its findings."
  [evidence]
  (let [checks (detective/mark-unread upload/detectives evidence)]
    {:unread (boolean (:unread (first checks)))
     :findings (vec (detective/run-detectives checks evidence))}))

(defn- summaries [findings] (mapv :summary findings))

(defn- mentions-nginx-default?
  "Whether a finding states nginx's 1 MB default as nginx's limit."
  [{:keys [summary hint]}]
  (boolean (or (re-find #"nginx caps uploads at 1M" (str summary))
               (re-find #"nginx 1M" (str hint))
               (re-find #"1 MB default" (str hint)))))

(def ^:private cases
  "Each case: the containers of the pod, the exec answers by [container
   command], the evidence keys it must produce and what upload-limits makes
   of them."
  [{:case "nginx answered with a client_max_body_size directive"
    :containers ["nginx" "php"]
    :execs (fn [] {["nginx" ["nginx" "-T"]] (answered @nginx-64m)
                   ["php" ["php" "-i"]] (answered @php-info)})
    :evidence {:found-nginx? true :nginx-explicit? true :nginx-body-size 67108864
               :found-php? true :upload-max 2097152 :post-max 8388608 :memory-limit 134217728}
    :unread false
    :expected ["effective upload limit is 2M (capped by PHP upload_max_filesize)"
               "PHP's limits were read from the PHP CLI ('php -i'), whose values php-fpm may not apply"]}
   {:case "nginx answered without a directive, so its 1 MB default applies"
    :containers ["nginx" "php"]
    :execs (fn [] {["nginx" ["nginx" "-T"]] (answered @nginx-default)
                   ["php" ["php" "-i"]] (answered @php-info)})
    :evidence {:found-nginx? true :nginx-explicit? false :nginx-body-size 1048576
               :found-php? true :upload-max 2097152 :post-max 8388608}
    :unread false
    :expected ["effective upload limit is 1M (capped by nginx client_max_body_size)"
               "PHP's limits were read from the PHP CLI ('php -i'), whose values php-fpm may not apply"
                "nginx caps uploads at 1M, below PHP's 2M - nginx rejects larger uploads with 413"]}
   {:case "no nginx container"
    :containers ["php"]
    :execs (fn [] {["php" ["php" "-i"]] (answered @php-info)})
    :evidence {:found-nginx? false :nginx-explicit? nil :nginx-body-size nil
               :found-php? true :upload-max 2097152 :post-max 8388608}
    :unread false
    :expected ["nginx's client_max_body_size was not read, so the effective upload limit is unknown - PHP alone accepts uploads up to 2M (capped by PHP upload_max_filesize)"
               "PHP's limits were read from the PHP CLI ('php -i'), whose values php-fpm may not apply"]}
   {:case "an nginx container whose nginx -T failed"
    :containers ["nginx" "php"]
    :execs (fn [] {["nginx" ["nginx" "-T"]] {:exit 1 :out "" :err @nginx-failed-stderr}
                   ["php" ["php" "-i"]] (answered @php-info)})
    :evidence {:found-nginx? false :nginx-explicit? nil :nginx-body-size nil
               :found-php? true :upload-max 2097152 :post-max 8388608}
    :unread false
    :expected ["nginx's client_max_body_size was not read, so the effective upload limit is unknown - PHP alone accepts uploads up to 2M (capped by PHP upload_max_filesize)"
               "PHP's limits were read from the PHP CLI ('php -i'), whose values php-fpm may not apply"]}
   {:case "an nginx -T that printed a dump and still exited non-zero"
    :containers ["nginx" "php"]
    :execs (fn [] {["nginx" ["nginx" "-T"]] {:exit 1 :out @nginx-64m :err "command terminated with exit code 1"}
                   ["php" ["php" "-i"]] (answered @php-info)})
    :evidence {:found-nginx? false :nginx-explicit? nil :nginx-body-size nil :found-php? true}
    :unread false
    :expected ["nginx's client_max_body_size was not read, so the effective upload limit is unknown - PHP alone accepts uploads up to 2M (capped by PHP upload_max_filesize)"
               "PHP's limits were read from the PHP CLI ('php -i'), whose values php-fpm may not apply"]}
   {:case "PHP did not answer php -i"
    :containers ["nginx"]
    :execs (fn [] {["nginx" ["nginx" "-T"]] (answered @nginx-64m)})
    :evidence {:found-nginx? true :nginx-explicit? true :nginx-body-size 67108864
               :found-php? false :upload-max nil :post-max nil :memory-limit nil}
    :unread false
    :expected ["PHP's upload limits were not read, so the effective upload limit is unknown - nginx alone accepts request bodies up to 64M (client_max_body_size)"]}
   {:case "PHP did not answer, nginx at its default"
    :containers ["nginx"]
    :execs (fn [] {["nginx" ["nginx" "-T"]] (answered @nginx-default)})
    :evidence {:found-nginx? true :nginx-explicit? false :nginx-body-size 1048576 :found-php? false}
    :unread false
    :expected ["PHP's upload limits were not read, so the effective upload limit is unknown - nginx alone accepts request bodies up to 1M (client_max_body_size, nginx's default - not set)"]}
   {:case "neither answered"
    :containers ["nginx" "php"]
    :execs (fn [] {})
    :evidence {:found-nginx? false :found-php? false :nginx-body-size nil :upload-max nil :post-max nil}
    :unread true
    :expected []}])

(deftest each-case-reads-and-judges-what-its-containers-answered
  (doseq [{:keys [case containers execs evidence unread expected]} cases]
    (testing case
      (let [collected (collect containers (execs))
            judged (judge collected)]
        (is (= evidence (select-keys collected (keys evidence))))
        (is (= unread (:unread judged)))
        (is (= expected (summaries (:findings judged))))))))

(deftest no-finding-states-the-assumed-default-when-nginx-did-not-answer
  (doseq [{:keys [case containers execs]} cases
          :let [collected (collect containers (execs))]
          :when (not (:found-nginx? collected))]
    (testing case
      (is (not-any? mentions-nginx-default? (:findings (judge collected)))))))

(deftest an-unread-side-never-leaves-the-check-without-a-warning
  (doseq [{:keys [case containers execs]} cases
          :let [collected (collect containers (execs))
                {:keys [unread findings]} (judge collected)]
          :when (and (not unread)
                     (not (and (:found-nginx? collected) (:found-php? collected))))]
    (testing case
      (is (some #(= :warning (:severity %)) findings)
          "a chain judged on one side only must not read as a pass"))))

(deftest an-exec-is-only-an-answer-when-it-exited-cleanly
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "partial" :err "boom"})]
    (is (nil? (kubectl/exec-answer "ctx" "ns" "pod" "c" ["nginx" "-T"]))))
  (with-redefs [shell/execute (fn [& _] {:exit 0 :out "  dump\n" :err ""})]
    (is (= "dump" (kubectl/exec-answer "ctx" "ns" "pod" "c" ["nginx" "-T"])))))

(deftest an-nginx-dump-is-recognised-by-its-file-headers
  (is (phpstack/nginx-dump? @nginx-default))
  (is (phpstack/nginx-dump? @nginx-64m))
  (is (not (phpstack/nginx-dump? @php-info)))
  (is (not (phpstack/nginx-dump? "")))
  (is (not (phpstack/nginx-dump? nil)))
  (is (not (phpstack/nginx-dump? "client_max_body_size 1m;"))
      "a directive without nginx's file header is not a dump"))

(deftest the-recorded-default-config-sets-no-body-size
  (is (= [] (phpstack/nginx-body-sizes @nginx-default)))
  (is (= ["64m"] (phpstack/nginx-body-sizes @nginx-64m))))

(deftest parse-evidence-separates-unanswered-from-unset
  (testing "nginx answered without a directive: the default, marked not explicit"
    (let [e (phpstack/parse-evidence {:component "app" :nginx-config @nginx-default :php-info @php-info})]
      (is (= [true false 1048576] ((juxt :found-nginx? :nginx-explicit? :nginx-body-size) e)))))
  (testing "nginx did not answer: nothing about nginx is known"
    (doseq [config [nil "" "   "]]
      (let [e (phpstack/parse-evidence {:component "app" :nginx-config config :php-info @php-info})]
        (is (= [false nil nil] ((juxt :found-nginx? :nginx-explicit? :nginx-body-size) e)))
        (is (= 2097152 (:upload-max e)))))))

(def ^:private gen-limit
  (gen/one-of [(gen/return nil)
               (gen/return :unlimited)
               (gen/fmap #(* 1048576 %) (gen/choose 1 4096))]))

(def ^:private seed 20261125)

(defn- no-finding-judges-an-unread-nginx
  "The property that, with nginx's limit given as nginx-body-size by a
   collection where nginx did not answer, no finding states an effective
   limit or an nginx limit, and a PHP side that answered carries a warning."
  [nginx-body-size]
  (prop/for-all [upload-max gen-limit
                 post-max gen-limit
                 memory-limit gen-limit]
    (let [findings (upload/analyze {:component "app"
                                    :found-nginx? false
                                    :nginx-body-size nginx-body-size
                                    :upload-max upload-max
                                    :post-max post-max
                                    :memory-limit memory-limit})]
      (and (not-any? #(str/starts-with? (str (:summary %)) "nginx caps uploads") findings)
           (not-any? #(str/starts-with? (str (:summary %)) "effective upload limit is") findings)
           (not-any? mentions-nginx-default? findings)
           (or (every? nil? [upload-max post-max])
               (some #(= :warning (:severity %)) findings))))))

(deftest no-finding-judges-nginx-when-nginx-was-not-read
  (let [result (tc/quick-check 300 (no-finding-judges-an-unread-nginx nil) :seed seed)]
    (is (:pass? result) (pr-str (:shrunk result)))))

(deftest the-property-fails-on-an-assumed-default
  (is (not (:pass? (tc/quick-check 300 (no-finding-judges-an-unread-nginx 1048576) :seed seed)))
      "evidence carrying the 1 MB default for an unanswered nginx must trip the property"))

(deftest no-finding-judges-php-when-php-was-not-read
  (let [result (tc/quick-check
                300
                (prop/for-all [nginx gen-limit
                               explicit? gen/boolean]
                  (let [findings (upload/analyze {:component "app" :found-php? false
                                                  :nginx-body-size nginx :nginx-explicit? explicit?
                                                  :upload-max nil :post-max nil :memory-limit nil})]
                    (and (not-any? #(str/starts-with? (str (:summary %)) "effective upload limit is") findings)
                         (if (nil? nginx)
                           (empty? findings)
                           (= [:warning] (map :severity findings))))))
                :seed seed)]
    (is (:pass? result) (pr-str (:shrunk result)))))

(deftest a-php-directive-is-read-only-where-its-name-starts-the-line
  (is (= "128M" (phpstack/php-directive @php-info "memory_limit")))
  (is (= "-1" (phpstack/php-directive @php-info "max_memory_limit")))
  (is (= "128M" (phpstack/php-directive "max_memory_limit => -1 => -1\nmemory_limit => 128M => 128M" "memory_limit")))
  (is (nil? (phpstack/php-directive "max_memory_limit => -1 => -1" "memory_limit"))))
