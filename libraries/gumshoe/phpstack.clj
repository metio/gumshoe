;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.phpstack
  "Evidence collection for the nginx + php-fpm upload chain. Walks the pods of
   a namespace, execs into each container to read the *effective* config
   ('nginx -T' and 'php -i'), and auto-detects which container is the web
   layer and which is PHP - the operator only has to name the app's namespace."
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
  "The upload limits of an 'nginx -T' dump and a 'php -i' dump, either nil when
   its container did not answer. nginx's limit is its 1 MB default only when
   nginx answered without setting client_max_body_size; when nginx did not
   answer, :nginx-body-size and :nginx-explicit? are nil, so nothing judges a
   limit nobody read."
  [{:keys [component nginx-config php-info]}]
  (let [nginx? (not (str/blank? (str nginx-config)))
        sizes (nginx-body-sizes nginx-config)
        explicit? (boolean (seq sizes))]
    {:component component
     :found-nginx? nginx?
     :found-php? (boolean (not (str/blank? (str php-info))))
     :nginx-explicit? (when nginx? explicit?)
     :nginx-body-size (cond
                        (not nginx?) nil
                        explicit? (nginx-limit sizes)
                        :else nginx-default-body-size)
     :upload-max (some-> (php-directive php-info "upload_max_filesize") size/parse)
     :post-max (some-> (php-directive php-info "post_max_size") size/parse)
     :memory-limit (some-> (php-directive php-info "memory_limit") size/parse)}))

;; ---------------------------------------------------------------------------
;; collecting from the cluster

(defn- looks-like-php? [output] (str/includes? (str output) "=>"))

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

(defn collect-evidence!
  "Finds the first container that answers 'nginx -T' with a config dump and the
   first that answers 'php -i' across the namespace, and returns parsed
   evidence."
  [context namespace]
  (stdout/print-section "🔍 Evidence (kubectl exec)")
  (let [pods (kubectl/items-of (kubectl/get-namespaced context namespace "pods"))]
    (loop [pods pods, nginx-config nil, php-info nil]
      (if (or (and nginx-config php-info) (empty? pods))
        (parse-evidence {:component namespace :nginx-config nginx-config :php-info php-info})
        (let [pod (first pods)
              pod-name (kubectl/name-of pod)
              containers (kubectl/container-names pod)
              nginx-config (or nginx-config
                               (first-answer context namespace pod-name containers ["nginx" "-T"] nginx-dump? "nginx"))
              php-info (or php-info
                           (first-answer context namespace pod-name containers ["php" "-i"] looks-like-php? "php"))]
          (recur (rest pods) nginx-config php-info))))))
