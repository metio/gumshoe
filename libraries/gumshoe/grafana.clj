;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.grafana
  "Grafana's app-platform API (Grafana >= 11): folders and dashboards are
   kubernetes-style resources served under /apis/<group>/<version>/. The org
   maps to a namespace - 'default' for org 1 on self-hosted instances."
  (:require [gumshoe.http :as http]))

(def ^:private folder-api "folder.grafana.app/v1beta1")
(def ^:private dashboard-api "dashboard.grafana.app/v1beta1")

(defn- api-get
  "Fetches a Grafana app-platform resource. Returns nil for any non-success -
   an unreachable host, a bad token, or an error status - so callers see a
   uniform 'no data' instead of an exception."
  [base-url token path]
  (let [response (http/fetch (format "%s/apis/%s" base-url path)
                             {"Authorization" (str "Bearer " token)})]
    (when (http/ok? response)
      (:json response))))

(defn folders
  [base-url token namespace]
  (api-get base-url token (format "%s/namespaces/%s/folders" folder-api namespace)))

(defn dashboards
  [base-url token namespace]
  (api-get base-url token (format "%s/namespaces/%s/dashboards" dashboard-api namespace)))

(defn dashboard
  "The full dashboard resource; its :spec is the dashboard JSON model."
  [base-url token namespace uid]
  (api-get base-url token (format "%s/namespaces/%s/dashboards/%s" dashboard-api namespace uid)))

(defn logged-in?
  "True when the token can list folders - the cheapest end-to-end check."
  [base-url token namespace]
  (some? (folders base-url token namespace)))

;; ---------------------------------------------------------------------------
;; pure helpers over fetched resource lists

(defn titles
  [resource-list]
  (vec (sort (keep #(-> % :spec :title) (:items resource-list)))))

(defn find-by-title
  [resource-list title]
  (first (filter #(= title (-> % :spec :title)) (:items resource-list))))

(defn uid-of
  "The uid of an app-platform resource - its kubernetes-style name."
  [resource]
  (-> resource :metadata :name))

(defn folder-uid-of
  "The folder a dashboard lives in, taken from its folder annotation."
  [dashboard-resource]
  (get-in dashboard-resource [:metadata :annotations (keyword "grafana.app/folder")]))

(defn in-folder
  "Only the dashboards that live in the given folder uid."
  [dashboards-list folder-uid]
  {:items (vec (filter #(= folder-uid (folder-uid-of %)) (:items dashboards-list)))})
