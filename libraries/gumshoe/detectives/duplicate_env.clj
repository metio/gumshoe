;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.duplicate-env
  "Containers whose env lists the same name more than once. The container
   sees the last definition only, and because env merges by name, an apply can
   drop one of them - so which value runs depends on how the manifest was last
   written. The API server only warns about it on admission, and Helm values
   merged from several files produce it easily."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.subject :as subject]))

(def ^:private workload-types
  [["deployments" "Deployment" [:spec :template :spec]]
   ["statefulsets" "StatefulSet" [:spec :template :spec]]
   ["daemonsets" "DaemonSet" [:spec :template :spec]]
   ["cronjobs" "CronJob" [:spec :jobTemplate :spec :template :spec]]])

(defn duplicates
  "Each name a container's env defines more than once, with its definitions in
   order: [{:name :definitions [env …]}]."
  [container]
  (for [[name definitions] (group-by :name (:env container))
        :when (> (count definitions) 1)]
    {:name name :definitions definitions}))

(defn- describe
  [container {:keys [name definitions]}]
  (let [differ? (> (count (distinct (map #(dissoc % :name) definitions))) 1)]
    (format "%s: %s defined %d times%s"
            (:name container) name (count definitions)
            (if differ? ", with different values" ", all alike"))))

(defn detect-duplicate-env
  [evidence]
  (for [[type kind path] workload-types
        workload (kubectl/items-of (get evidence type))
        :let [spec (get-in workload path)
              found (for [container (concat (:initContainers spec) (:containers spec))
                          duplicate (duplicates container)]
                      (describe container duplicate))]
        :when (seq found)]
    {:severity :warning
     :component (kubectl/namespace-name-of workload)
     :subject (subject/subject kind (kubectl/namespace-of workload) (kubectl/name-of workload))
     :summary "containers define an environment variable more than once"
     :hint (str kind " " (kubectl/namespace-name-of workload) ": " (str/join "; " found)
                " - the container sees only the last definition, and an apply may drop either; keep one")}))

(def detectives
  [{:name "duplicate-env"
    :description "Containers that define an environment variable more than once"
    :requires (mapv first workload-types)
    :detect detect-duplicate-env}])
