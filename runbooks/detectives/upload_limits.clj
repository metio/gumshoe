;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.upload-limits
  "Investigates the file-upload limit chain of an nginx + php-fpm app (Moodle,
   Nextcloud, ...). Name the app's namespace and it auto-detects the web and
   PHP containers, reads their effective config, and shows what an upload
   actually maxes out at - and which layer is the bottleneck."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.upload :as upload]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.phpstack :as phpstack]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def options
  (merge {:namespace {:desc "The namespace of the app - interactive selection when omitted"
                      :alias :n
                      :coerce :string}}
         detective/output-option))

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]
   :kubectl-can-exec ["pods"]})

(defn- investigate
  [opts _ctx]
  (detective/when-to-run! "Reach for this when a Moodle/Nextcloud upload fails at a size limit - the nginx client_max_body_size and php-fpm post/upload limits along the whole chain.")
  (let [context (kubectl/current-context)
        namespaces (kubectl/names-of (kubectl/get-all context "namespaces"))
        namespace (interact/choose-one "Namespace" namespaces (:namespace opts))]
    (if (nil? namespace)
      (do (stdout/error "no namespace selected") false)
      (let [evidence (phpstack/collect-evidence! context namespace)]
        (cond
          (not (or (:found-nginx? evidence) (:found-php? evidence)))
          (do (stdout/error (format "found no nginx or php-fpm container in %s - is this an nginx+php-fpm app?" namespace))
              false)

          :else
          (do
            (when-not (:found-nginx? evidence)
              (stdout/warn "no nginx container answered 'nginx -T' - reporting the PHP side only"))
            (when-not (:found-php? evidence)
              (stdout/warn "no php container answered 'php -i' - reporting the nginx side only"))
            (detective/report!
             upload/detectives
             (detective/run-detectives upload/detectives evidence)
             (:output opts "text"))))))))

(runbook/execute!
 {:description "Investigates the nginx + php-fpm file-upload limit chain of an app"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action investigate})
