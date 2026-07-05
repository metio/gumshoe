;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.upload
  "Detective for the file-upload limit chain of an nginx + php-fpm app. The
   effective limit is the smallest link: nginx's client_max_body_size, PHP's
   post_max_size, and PHP's upload_max_filesize. This finds the classic
   mismatches behind 'users can not upload files' - most often an nginx
   client_max_body_size left at its 1 MB default in front of a PHP that would
   happily accept much more."
  (:require [gumshoe.size :as size]))

;; nginx's built-in default when client_max_body_size is never set.
(def ^:private nginx-default-bytes 1048576)

(defn analyze
  "Pure: takes the parsed evidence and returns findings. Keys are byte counts,
   :unlimited, or nil (not found / could not read)."
  [{:keys [component nginx-body-size nginx-explicit? upload-max post-max memory-limit]}]
  (let [effective (size/minimum [nginx-body-size post-max upload-max])]
    (concat
     ;; the headline: what an upload actually maxes out at, and which link caps it
     (when effective
       (let [limiter (cond
                       (= effective nginx-body-size) "nginx client_max_body_size"
                       (= effective post-max) "PHP post_max_size"
                       (= effective upload-max) "PHP upload_max_filesize"
                       :else "the chain")]
         [{:severity :info
           :component component
           :summary (format "effective upload limit is %s (capped by %s)" (size/human effective) limiter)
           :hint (format "nginx %s%s / upload_max_filesize %s / post_max_size %s"
                         (size/human nginx-body-size)
                         (if nginx-explicit? "" " (default - not set)")
                         (size/human upload-max)
                         (size/human post-max))}]))

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
    :requires ["pods"]
    :detect analyze}])
