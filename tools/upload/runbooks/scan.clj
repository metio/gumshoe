;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.upload-limits
  "Investigates the file-upload limit chain of an nginx + php-fpm app (Moodle,
   Nextcloud, ...). Name the app's namespace and it auto-detects the web and
   PHP containers, reads their effective config, and shows what an upload
   actually maxes out at - and which layer is the bottleneck."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.upload :as upload]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.phpstack :as phpstack]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]))

(def namespace-select
  {:mode :one :label "Namespace" :flag :namespace
   :candidates #(kubectl/names-of (kubectl/get-all % "namespaces"))
   :empty-message "the cluster has no namespaces"
   :on-empty :fail})

(def options
  (merge (select/options namespace-select
                         {:namespace {:desc "The namespace of the app - interactive selection when omitted"}})
         detective/output-option))

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]
   :kubectl-can-exec ["pods"]})

(defn- collect
  "The upload limits the app's containers report, or nil when no namespace was
   chosen or neither an nginx nor a php-fpm container answered."
  [opts]
  (let [context (kubectl/current-context)
        namespace (select/pick namespace-select opts context)]
    (if (nil? namespace)
      nil
      (let [evidence (phpstack/collect-evidence! context namespace)]
        (if-not (or (:found-nginx? evidence) (:found-php? evidence))
          (do (stdout/error (format "found no nginx or php-fpm container in %s - is this an nginx+php-fpm app?" namespace))
              nil)
          (do
            (when-not (:found-nginx? evidence)
              (stdout/warn "no nginx container answered 'nginx -T' - reporting the PHP side only"))
            (when-not (:found-php? evidence)
              (stdout/warn "no php container answered 'php -i' - reporting the nginx side only"))
            evidence))))))

(detective/book
 {:description "Investigates the nginx + php-fpm file-upload limit chain of an app"
  :options options
  :prerequisites prerequisites
  :detectives upload/detectives
  :when-to-run "Reach for this when a Moodle/Nextcloud upload fails at a size limit - the nginx client_max_body_size and php-fpm post/upload limits along the whole chain."
  :collect collect})
