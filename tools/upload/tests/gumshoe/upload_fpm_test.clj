;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.upload-fpm-test
  "PHP's upload limits are php-fpm's own: its SAPI's ini from 'php-fpm -i'
   with each pool's php_value and php_admin_value from 'php-fpm -tt'. The
   fixtures are recorded from docker.io/library/php:8-fpm (PHP 8.6.0, digest
   sha256:b6b4d0a28fad): php-fpm-i.txt is the core section of 'php-fpm -i'
   without its System line, and php-fpm-tt-pool.txt is 'php-fpm -tt' with a
   pool file setting php_admin_value[upload_max_filesize] = 64M,
   php_value[post_max_size] = 80M and php_admin_value[memory_limit] = 256M."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.upload :as upload]
            [gumshoe.phpstack :as phpstack]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(defn- fixture [file] (slurp (io/file "tools/upload/tests/fixtures" file)))

(def ^:private fpm-info (delay (fixture "php-fpm-i.txt")))
(def ^:private fpm-pools (delay (fixture "php-fpm-tt-pool.txt")))
(def ^:private cli-info (delay (fixture "php-i.txt")))
(def ^:private nginx-64m (delay (fixture "nginx-T-64m.txt")))

(defn- answered [out] {:exit 0 :out out :err ""})

(def ^:private missing
  {:exit 127 :out "" :err "command terminated with exit code 127"})

(defn- exec-command
  "The command after -- of a kubectl exec argv, and its container."
  [argv]
  (let [args (vec argv)
        container (some (fn [[flag value]] (when (= "-c" flag) value)) (partition 2 1 args))]
    [container (vec (rest (drop-while #(not= "--" %) args)))]))

(defn- fake-cluster
  "kubectl answering one pod with containers, and each exec from execs keyed
   by [container command]; anything else execs like a missing command.
   Records every exec under commands."
  [containers execs commands]
  (fn [& argv]
    (if (some #{"exec"} argv)
      (let [target (exec-command argv)]
        (swap! commands conj target)
        (get execs target missing))
      (answered (str "{\"items\":[{\"metadata\":{\"name\":\"app-0\"},\"spec\":{\"containers\":["
                     (str/join "," (map #(format "{\"name\":\"%s\"}" %) containers))
                     "]}}]}")))))

(defn- collect
  ([containers execs] (collect containers execs (atom [])))
  ([containers execs commands]
   (with-redefs [shell/execute (fake-cluster containers execs commands)
                 stdout/print-section (fn [& _])
                 stdout/err-println (fn [& _])]
     (phpstack/collect-evidence! "ctx" "moodle"))))

(defn- summaries [evidence]
  (mapv :summary (detective/run-detectives (detective/mark-unread upload/detectives evidence) evidence)))

(def ^:private fpm-i ["php" (phpstack/fpm-command "-i")])
(def ^:private fpm-tt ["php" (phpstack/fpm-command "-tt")])
(def ^:private cli ["php" ["php" "-i"]])

;; --- parsing ------------------------------------------------------------------------

(deftest the-recorded-fpm-info-is-the-fpm-sapi
  (is (phpstack/fpm-info? @fpm-info))
  (is (not (phpstack/fpm-info? @cli-info)) "the CLI's phpinfo is not php-fpm's"))

(deftest pool-overrides-are-read-from-the-recorded-dump
  (is (= {"www" {"upload_max_filesize" "64M" "post_max_size" "80M" "memory_limit" "256M"}}
         (phpstack/fpm-pool-values @fpm-pools))))

(deftest an-admin-value-wins-over-a-plain-value
  (is (= {"www" {"upload_max_filesize" "8M"}}
         (phpstack/fpm-pool-values (str "NOTICE: [global]\n"
                                        "NOTICE: [www]\n"
                                        "NOTICE: \tphp_admin_value[upload_max_filesize] = 8M\n"
                                        "NOTICE: \tphp_value[upload_max_filesize] = 512M\n"
                                        "NOTICE: \tphp_value[display_errors] = On\n")))))

(deftest a-pool-without-overrides-applies-the-ini
  (is (= {"www" {}} (phpstack/fpm-pool-values "NOTICE: [global]\nNOTICE: \tpid = undefined\nNOTICE: [www]\nNOTICE: \tlisten = 9000\n"))))

;; --- what the chain reads -----------------------------------------------------------

(deftest php-fpm-with-its-pools-is-the-php-side
  (let [commands (atom [])
        evidence (collect ["nginx" "php"]
                          {["nginx" ["nginx" "-T"]] (answered @nginx-64m)
                           fpm-i (answered @fpm-info)
                           fpm-tt (answered @fpm-pools)
                           cli (answered @cli-info)}
                          commands)]
    (is (= {:php-source :fpm :upload-max 67108864 :post-max 83886080 :memory-limit 268435456}
           (select-keys evidence [:php-source :upload-max :post-max :memory-limit]))
        "the pool's overrides, never the CLI's 2M and 8M")
    (is (not-any? #{cli} @commands) "the CLI is never asked once php-fpm answered")
    (is (= ["effective upload limit is 64M (capped by nginx client_max_body_size)"]
           (summaries evidence)))))

(deftest php-fpm-without-its-pools-is-marked
  (let [evidence (collect ["php"] {fpm-i (answered @fpm-info)})]
    (is (= {:php-source :fpm-ini :upload-max 2097152 :post-max 8388608}
           (select-keys evidence [:php-source :upload-max :post-max])))
    (is (some #(str/starts-with? % "php-fpm's pools were not read") (summaries evidence)))))

(deftest the-cli-is-read-only-without-php-fpm-and-says-so
  (let [evidence (collect ["php"] {cli (answered @cli-info)})]
    (is (= :cli (:php-source evidence)))
    (is (some #{"PHP's limits were read from the PHP CLI ('php -i'), whose values php-fpm may not apply"}
              (summaries evidence)))))

(deftest a-cli-phpinfo-answering-the-fpm-command-is-not-fpm
  (testing "a php-fpm wrapper that prints the CLI's phpinfo is read as the CLI"
    (let [evidence (collect ["php"] {fpm-i (answered @cli-info) cli (answered @cli-info)})]
      (is (= :cli (:php-source evidence))))))

(deftest pools-that-disagree-take-the-most-restrictive-and-say-so
  (let [dump (str "NOTICE: [global]\n"
                  "NOTICE: [www]\n"
                  "NOTICE: \tphp_admin_value[upload_max_filesize] = 64M\n"
                  "NOTICE: [status]\n")
        evidence (phpstack/parse-evidence {:component "app" :php-info @fpm-info :php-source :fpm
                                           :fpm-pools (phpstack/fpm-pool-values dump)})]
    (is (= 2097152 (:upload-max evidence)) "the status pool applies the ini's 2M")
    (is (:php-pools-differ? evidence))
    (is (some #{"php-fpm's pools apply different PHP limits; the most restrictive is used"}
              (summaries (assoc evidence :nginx-body-size 67108864 :found-nginx? true :nginx-explicit? true))))))

(deftest unlimited-pools-stay-unlimited
  (let [evidence (phpstack/parse-evidence
                  {:component "app" :php-info @fpm-info :php-source :fpm
                   :fpm-pools {"www" {"post_max_size" "0"} "api" {"post_max_size" "-1"}}})]
    (is (= :unlimited (:post-max evidence)))
    (is (not (:php-pools-differ? evidence)))))

(deftest no-php-answer-has-no-source
  (is (nil? (:php-source (phpstack/parse-evidence {:component "app" :nginx-config @nginx-64m})))))
