;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.upload
  "Detective for the file-upload limit chain of an nginx + php-fpm app. The
   effective limit is the smallest link: nginx's client_max_body_size, PHP's
   post_max_size, and PHP's upload_max_filesize. This finds the classic
   mismatches behind 'users can not upload files' - most often an nginx
   client_max_body_size left at its 1 MB default in front of a PHP that would
   happily accept much more.

   PHP's limits are php-fpm's own where it answered: its SAPI's ini with the
   serving pool's php_value and php_admin_value applied. Limits read only
   from the CLI's ini, or from php-fpm's ini without its pools, are
   reported as such, since php-fpm may apply other values."
  (:require [gumshoe.size :as size]))

(defn- limiter
  "The name of the link whose value is the effective limit."
  [effective {:keys [nginx-body-size post-max upload-max]}]
  (cond
    (= effective nginx-body-size) "nginx client_max_body_size"
    (= effective post-max) "PHP post_max_size"
    (= effective upload-max) "PHP upload_max_filesize"
    :else "the chain"))

(defn- accepts
  [noun limit]
  (if (= :unlimited limit)
    (format "accepts %s of any size" noun)
    (format "accepts %s up to %s" noun (size/human limit))))

(defn- one-side-read
  "The warning for a chain judged on one side only: the side nobody read
   leaves the effective limit unknown, so the check names it instead of
   passing, and reports what the side that answered accepts."
  [{:keys [component nginx-body-size nginx-explicit? found-nginx? found-php? upload-max post-max]
    :as evidence}]
  (let [nginx? (some? nginx-body-size)
        php? (or (some? upload-max) (some? post-max))]
    (cond
      (and php? (not nginx?))
      (let [php-limit (size/minimum [post-max upload-max])]
        [{:severity :warning
          :component component
          :summary (str "nginx's client_max_body_size was not read, so the effective upload limit is unknown - PHP alone "
                        (if php-limit
                          (format "%s (capped by %s)" (accepts "uploads" php-limit) (limiter php-limit evidence))
                          (accepts "uploads" :unlimited)))
          :hint (if (true? found-nginx?)
                  "client_max_body_size did not parse in 'nginx -T' - read it in the nginx container"
                  "no nginx container answered 'nginx -T' - if nginx fronts this app, read client_max_body_size in its container; otherwise the server in front of PHP sets its own limit")}])

      (and nginx? (not php?))
      [{:severity :warning
        :component component
        :summary (format "PHP's upload limits were not read, so the effective upload limit is unknown - nginx alone %s (client_max_body_size%s)"
                         (accepts "request bodies" nginx-body-size)
                         (if (false? nginx-explicit?) ", nginx's default - not set" ""))
        :hint (if (true? found-php?)
                "upload_max_filesize and post_max_size did not parse in the phpinfo output - read them in the php-fpm container"
                "no container answered 'php-fpm -i' or 'php -i' - read upload_max_filesize and post_max_size in the php-fpm container")}])))

(defn- php-source-findings
  "What the source of PHP's limits leaves open: values from the CLI or from
   php-fpm's ini without its pools may not be the ones php-fpm applies, and
   pools that disagree leave the serving pool's value unknown."
  [{:keys [component php-source php-container php-pools-differ?]}]
  (concat
   (case php-source
     :cli
     [{:severity :warning
       :component component
       :summary "PHP's limits were read from the PHP CLI ('php -i'), whose values php-fpm may not apply"
       :hint (str "no container answered 'php-fpm -i'"
                  (when php-container (str ", the CLI was read in " php-container))
                  "; the CLI and php-fpm may load different php.ini files, "
                  "and a php-fpm pool's php_value or php_admin_value never reaches the CLI - "
                  "read upload_max_filesize and post_max_size in the php-fpm container")}]
     :fpm-ini
     [{:severity :warning
       :component component
       :summary "php-fpm's pools were not read, so a pool's php_value or php_admin_value may override these PHP limits"
       :hint (str "'php-fpm -tt' did not answer with a pool in "
                  (or php-container "the php-fpm container")
                  " - read the pool configuration (php-fpm.d) for upload_max_filesize and post_max_size")}]
     nil)
   (when php-pools-differ?
     [{:severity :warning
       :component component
       :summary "php-fpm's pools apply different PHP limits; the most restrictive is used"
       :hint "which pool serves the app depends on the address nginx passes requests to (fastcgi_pass) - compare it with each pool's listen"}])))

(defn analyze
  "Pure: takes the parsed evidence and returns findings. Keys are byte counts,
   :unlimited, or nil (not found / could not read). An effective limit is only
   stated when both nginx and PHP were read; with one side unread the check
   warns that the limit is unknown and still judges the side that answered."
  [{:keys [component nginx-body-size nginx-explicit? upload-max post-max memory-limit]
    :as evidence}]
  (let [both? (and (some? nginx-body-size) (or (some? upload-max) (some? post-max)))
        effective (size/minimum [nginx-body-size post-max upload-max])]
    (concat
     ;; the headline: what an upload actually maxes out at, and which link caps it
     (when (and both? effective)
       [{:severity :info
         :component component
         :summary (format "effective upload limit is %s (capped by %s)" (size/human effective) (limiter effective evidence))
         :hint (format "nginx %s%s / upload_max_filesize %s / post_max_size %s"
                       (size/human nginx-body-size)
                       (if nginx-explicit? "" " (default - not set)")
                       (size/human upload-max)
                       (size/human post-max))}])

     (one-side-read evidence)

     (when (or (some? upload-max) (some? post-max))
       (php-source-findings evidence))

     ;; nginx in front is stricter than PHP behind it -> 413 before PHP is even reached
     (when (size/smaller? nginx-body-size upload-max)
       [{:severity :warning
         :component component
         :summary (format "nginx caps uploads at %s, below PHP's %s - nginx rejects larger uploads with 413"
                          (size/human nginx-body-size) (size/human upload-max))
         :hint (if nginx-explicit?
                 "raise client_max_body_size in the nginx config to match the PHP limit"
                 "client_max_body_size is at nginx's 1 MB default - set it explicitly for an upload app")}])

     ;; PHP's POST cap below its file cap -> the file limit is a mirage
     (when (size/smaller? post-max upload-max)
       [{:severity :warning
         :component component
         :summary (format "PHP post_max_size (%s) is smaller than upload_max_filesize (%s) - uploads fail inside PHP"
                          (size/human post-max) (size/human upload-max))
         :hint "post_max_size must be at least as large as upload_max_filesize"}])

     ;; memory below the POST cap -> large uploads can exhaust the worker
     (when (size/smaller? memory-limit post-max)
       [{:severity :info
         :component component
         :summary (format "PHP memory_limit (%s) is below post_max_size (%s)"
                          (size/human memory-limit) (size/human post-max))
         :hint "large request bodies may exhaust the php-fpm worker - raise memory_limit if uploads fail"}]))))

(def detectives
  [{:name "upload-limits"
    :description "The nginx + php-fpm file-upload limit chain is coherent"
    :requires [:nginx-body-size :upload-max :post-max]
    :detect analyze}])
