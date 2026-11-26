;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.phpstack
  "Evidence collection for the nginx + php-fpm upload chain. Walks the pods of
   a namespace, execs into each container to read the *effective* config
   ('nginx -T', and php-fpm's own 'php-fpm -i' and 'php-fpm -tt'), and
   auto-detects which container is the web layer and which is PHP - the
   operator only has to name the app's namespace.

   PHP's limits are read from php-fpm itself where it answers: each SAPI may
   load its own php.ini (Debian's packages keep /etc/php/<v>/cli and
   /etc/php/<v>/fpm apart), and a pool's php_value and php_admin_value
   override the ini for the requests that pool serves, which the CLI never
   reads. Only when no container answers as php-fpm are the CLI's values
   ('php -i') read, and the evidence then says so in :php-source."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.size :as size]
            [gumshoe.stdout :as stdout]))

;; ---------------------------------------------------------------------------
;; pure parsing of the effective config

(defn nginx-body-sizes
  "Every client_max_body_size value in an 'nginx -T' dump (one per block).
   The dump carries the files verbatim, so a commented-out directive is
   dropped first: a # at the start of a line or after whitespace, ; { or }
   starts a comment."
  [nginx-config]
  (->> (re-seq #"client_max_body_size\s+([^;]+);"
               (str/replace (str nginx-config) #"(?m)(^|[\s;{}])#.*$" "$1"))
       (map (comp str/trim second))
       distinct
       vec))

(defn php-directive
  "The active (left-hand) value of a directive in a 'php -i' dump. The name
   must start its line, so memory_limit never reads max_memory_limit."
  [php-info directive]
  (some-> (re-find (re-pattern (str "(?m)^" (java.util.regex.Pattern/quote directive) #"\s*=>\s*(\S+)"))
                   (str php-info))
          second))

(def php-directives
  "The PHP directives the upload chain reads."
  ["upload_max_filesize" "post_max_size" "memory_limit"])

(defn fpm-info?
  "Whether output is phpinfo of the FPM SAPI, as 'php-fpm -i' prints it."
  [output]
  (boolean (re-find #"(?m)^Server API => FPM/FastCGI\s*$" (str output))))

(defn fpm-pool-values
  "The php_value and php_admin_value settings of each pool in a 'php-fpm -tt'
   dump, as {pool {directive value}} for the directives the upload chain
   reads; a pool that sets none maps to {}. php-fpm logs each pool as a
   \"[name]\" line followed by its settings, after the [global] section. A
   php_admin_value wins over a php_value of the same directive, since a
   script can never override it."
  [dump]
  (let [wanted (set php-directives)]
    (loop [[line & more] (str/split-lines (str dump))
           pool nil
           pools {}
           admin #{}]
      (if (nil? line)
        pools
        (if-let [[_ name] (re-find #"NOTICE:\s*\[([^\]]+)\]\s*$" line)]
          (if (= "global" name)
            (recur more nil pools admin)
            (recur more name (assoc pools name (get pools name {})) admin))
          (let [[_ admin? directive value] (re-find #"php_(admin_)?value\[([^\]]+)\]\s*=\s*(.*?)\s*$" line)]
            (if (and pool directive (wanted directive)
                     (or admin? (not (admin [pool directive]))))
              (recur more pool (assoc-in pools [pool directive] value)
                     (cond-> admin admin? (conj [pool directive])))
              (recur more pool pools admin))))))))

(defn- pool-limit
  "The most restrictive of the values the pools apply for one directive,
   :unlimited when every pool is unlimited, nil when none parses."
  [values]
  (let [parsed (remove nil? (map size/parse values))]
    (cond
      (empty? parsed) nil
      (every? #{:unlimited} parsed) :unlimited
      :else (size/minimum parsed))))

(defn fpm-limits
  "The limits php-fpm applies, from the ini values of 'php-fpm -i' and the
   pools of 'php-fpm -tt': {directive {:value <parsed> :pools-differ? bool}}.
   Each pool applies its own override or else the ini value. With several
   pools that disagree, the most restrictive value is taken, since the pool
   serving the app is not known, and :pools-differ? says so. Without pools
   (nil), the ini values alone."
  [php-info pools]
  (into {}
        (for [directive php-directives
              :let [ini (php-directive php-info directive)
                    applied (if (seq pools)
                              (map #(get % directive ini) (vals pools))
                              [ini])]]
          [directive {:value (pool-limit applied)
                      :pools-differ? (< 1 (count (distinct (keep size/parse applied))))}])))

(defn nginx-dump?
  "Whether output is an 'nginx -T' dump: nginx heads every file it dumps with
   a '# configuration file <path>:' line, whether or not the file sets
   client_max_body_size."
  [output]
  (boolean (re-find #"(?m)^# configuration file \S.*:\s*$" (str output))))

(def nginx-default-body-size
  "nginx's client_max_body_size when no block sets it: 1 MB."
  1048576)

(defn- nginx-limit
  "The most restrictive configured client_max_body_size, :unlimited when every
   one is 0, or nil when none parses."
  [sizes]
  (let [parsed (map size/parse sizes)]
    (or (size/minimum parsed)
        (when (seq (remove nil? parsed)) :unlimited))))

(defn parse-evidence
  "The upload limits of an 'nginx -T' dump and a phpinfo dump, either nil when
   its container did not answer. nginx's limit is its 1 MB default only when
   nginx answered without setting client_max_body_size; when nginx did not
   answer, :nginx-body-size and :nginx-explicit? are nil, so nothing judges a
   limit nobody read.

   php-source names where the PHP values come from: :fpm for 'php-fpm -i'
   with fpm-pools, the pools of 'php-fpm -tt', applied over it; :fpm-ini
   when php-fpm answered -i but its pools could not be read (fpm-pools nil),
   so a pool override may be missing; :cli for 'php -i', the CLI's ini, which
   php-fpm may not share. :php-source is nil when PHP did not answer."
  [{:keys [component nginx-config php-info php-source fpm-pools]}]
  (let [nginx? (not (str/blank? (str nginx-config)))
        php? (not (str/blank? (str php-info)))
        sizes (nginx-body-sizes nginx-config)
        explicit? (boolean (seq sizes))
        source (when php?
                 (cond
                   (not= :fpm php-source) :cli
                   (nil? fpm-pools) :fpm-ini
                   :else :fpm))
        limits (if (#{:fpm :fpm-ini} source)
                 (fpm-limits php-info fpm-pools)
                 (into {} (for [d php-directives]
                            [d {:value (some-> (php-directive php-info d) size/parse)}])))]
    {:component component
     :found-nginx? nginx?
     :found-php? php?
     :php-source source
     :php-pools-differ? (boolean (some :pools-differ? (vals limits)))
     :nginx-explicit? (when nginx? explicit?)
     :nginx-body-size (cond
                        (not nginx?) nil
                        explicit? (nginx-limit sizes)
                        :else nginx-default-body-size)
     :upload-max (get-in limits ["upload_max_filesize" :value])
     :post-max (get-in limits ["post_max_size" :value])
     :memory-limit (get-in limits ["memory_limit" :value])}))

;; ---------------------------------------------------------------------------
;; collecting from the cluster

(defn- looks-like-php? [output] (str/includes? (str output) "=>"))

(def ^:private fpm-binary-script
  ;; php-fpm is on the PATH in the docker.io/library/php images, while
  ;; Debian's packages install a versioned /usr/sbin/php-fpm<version> only.
  ;; stderr joins stdout because 'php-fpm -tt' logs the parsed pools there.
  (str "for b in php-fpm /usr/local/sbin/php-fpm* /usr/sbin/php-fpm*; do "
       "if command -v \"$b\" >/dev/null 2>&1; then exec \"$b\" \"$@\" 2>&1; fi; "
       "done; exit 127"))

(defn fpm-command
  "The exec command running php-fpm with mode (\"-i\" or \"-tt\") in a
   container, wherever the image installed it."
  [mode]
  ["sh" "-c" fpm-binary-script "sh" mode])

(defn- first-answer
  "The first answer of command among the pod's containers that answered? accepts,
   logged as read from that container."
  [context namespace pod-name containers command answered? label]
  (some (fn [c]
          (let [out (kubectl/exec-answer context namespace pod-name c command)]
            (when (answered? out)
              (stdout/err-println (format "  %s %s config from %s/%s" (stdout/blue "▸") label pod-name c))
              out)))
        containers))

(defn- php-answer
  "The PHP side of one container, or nil when it answers neither as php-fpm
   nor as the PHP CLI: {:php-source :fpm :php-info <php-fpm -i> :fpm-pools
   <pools of php-fpm -tt, nil when -tt did not answer>}, else {:php-source
   :cli :php-info <php -i>}. php-fpm is asked first, so the CLI is read
   only from a container without it."
  [context namespace pod-name container]
  (let [answered (fn [command] (kubectl/exec-answer context namespace pod-name container command))
        report (fn [label] (stdout/err-println (format "  %s %s config from %s/%s" (stdout/blue "▸") label pod-name container)))]
    (if-let [info (let [out (answered (fpm-command "-i"))] (when (fpm-info? out) out))]
      (let [dump (answered (fpm-command "-tt"))]
        (report (if dump "php-fpm" "php-fpm (its pools did not answer 'php-fpm -tt')"))
        {:php-source :fpm :php-info info :fpm-pools (some-> dump fpm-pool-values)})
      (when-let [info (let [out (answered ["php" "-i"])] (when (looks-like-php? out) out))]
        (report "php CLI")
        {:php-source :cli :php-info info}))))

(defn collect-evidence!
  "Finds the first container that answers 'nginx -T' with a config dump and the
   first that answers as php-fpm, or failing that as the PHP CLI, across the
   namespace, and returns parsed evidence."
  [context namespace]
  (stdout/print-section "🔍 Evidence (kubectl exec)")
  (let [pods (kubectl/items-of (kubectl/get-namespaced context namespace "pods"))]
    (loop [pods pods, nginx-config nil, php nil]
      (if (or (and nginx-config php) (empty? pods))
        (parse-evidence (merge {:component namespace :nginx-config nginx-config} php))
        (let [pod (first pods)
              pod-name (kubectl/name-of pod)
              containers (kubectl/container-names pod)
              nginx-config (or nginx-config
                               (first-answer context namespace pod-name containers ["nginx" "-T"] nginx-dump? "nginx"))
              php (or php (some #(php-answer context namespace pod-name %) containers))]
          (recur (rest pods) nginx-config php))))))
