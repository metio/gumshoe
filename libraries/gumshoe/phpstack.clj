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
  "Every client_max_body_size value in an 'nginx -T' dump (one per block)."
  [nginx-config]
  (->> (re-seq #"client_max_body_size\s+([^;]+);" (str nginx-config))
       (map (comp str/trim second))
       distinct
       vec))

(defn php-directive
  "The active (left-hand) value of a directive in a 'php -i' dump."
  [php-info directive]
  (some-> (re-find (re-pattern (str directive #"\s*=>\s*(\S+)")) (str php-info))
          second))

(defn- nginx-limit
  "The most restrictive configured client_max_body_size, or nil when none is
   set (so the caller applies nginx's 1 MB default)."
  [sizes]
  (let [parsed (map size/parse sizes)]
    (or (size/minimum parsed)
        (when (seq (remove nil? parsed)) :unlimited))))

(defn parse-evidence
  [{:keys [component nginx-config php-info]}]
  (let [sizes (nginx-body-sizes nginx-config)
        explicit? (boolean (seq sizes))]
    {:component component
     :found-nginx? (boolean (not (str/blank? (str nginx-config))))
     :found-php? (boolean (not (str/blank? (str php-info))))
     :nginx-explicit? explicit?
     :nginx-body-size (if explicit? (nginx-limit sizes) 1048576)
     :upload-max (some-> (php-directive php-info "upload_max_filesize") size/parse)
     :post-max (some-> (php-directive php-info "post_max_size") size/parse)
     :memory-limit (some-> (php-directive php-info "memory_limit") size/parse)}))

;; ---------------------------------------------------------------------------
;; collecting from the cluster

(defn- looks-like-nginx? [output] (str/includes? (str output) "client_max_body_size"))
(defn- looks-like-php? [output] (str/includes? (str output) "=>"))

(defn collect-evidence!
  "Finds the first container that answers 'nginx -T' and the first that answers
   'php -i' across the namespace, and returns parsed evidence."
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
                               (some (fn [c]
                                       (let [out (kubectl/exec-stdout context namespace pod-name c ["nginx" "-T"])]
                                         (when (looks-like-nginx? out)
                                           (stdout/err-println (format "  %s nginx config from %s/%s" (stdout/blue "▸") pod-name c))
                                           out)))
                                     containers))
              php-info (or php-info
                           (some (fn [c]
                                   (let [out (kubectl/exec-stdout context namespace pod-name c ["php" "-i"])]
                                     (when (looks-like-php? out)
                                       (stdout/err-println (format "  %s php config from %s/%s" (stdout/blue "▸") pod-name c))
                                       out)))
                                 containers))]
          (recur (rest pods) nginx-config php-info))))))
