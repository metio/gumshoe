;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.helm
  "Detective for releases installed with the helm CLI: a latest revision that
   failed, one stuck pending, which makes helm refuse every further install,
   upgrade and rollback of the release, and one stuck uninstalling. Each
   release is judged by its newest revision; older ones are history."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def ^:private stuck-minutes
  "How long an operation may hold a release before it counts as stuck rather
   than running - longer than helm's default --timeout of five minutes."
  10)

(defn changed-at
  "When a revision last changed: Helm's modifiedAt label, else createdAt
   (both unix seconds), else the Secret's creationTimestamp."
  [{:keys [modified-at created-at creation-timestamp]}]
  (cond
    modified-at (java.time.Instant/ofEpochSecond modified-at)
    created-at (java.time.Instant/ofEpochSecond created-at)
    creation-timestamp (java.time.Instant/parse creation-timestamp)
    :else nil))

(defn- minutes-since
  [revision now]
  (when-let [at (changed-at revision)]
    (when now (.toMinutes (java.time.Duration/between at ^java.time.Instant now)))))

(defn latest-revisions
  "The newest revision of every release, keyed by [namespace release], with the
   revisions of that release that are still deployed."
  [revisions]
  (for [[_ history] (group-by (juxt :namespace :release) revisions)
        :let [ordered (sort-by #(or (:version %) 0) history)]]
    (assoc (last ordered)
           :deployed (keep #(when (= "deployed" (:status %)) (:version %)) (butlast ordered)))))

(defn- secret-name
  [{:keys [release version]}]
  (format "sh.helm.release.v1.%s.v%s" release version))

(defn release-finding
  [{:keys [namespace release status version deployed] :as revision} now]
  (let [minutes (minutes-since revision now)
        base {:component (str namespace "/" release)}
        stuck? (or (nil? minutes) (>= minutes stuck-minutes))
        last-good (last deployed)]
    (some->
     (cond
       (str/starts-with? status "pending-")
       (when stuck?
         (assoc base :severity :critical
                :summary (format "Helm release is stuck in %s (revision %s)" status version)
                :hint (str "helm refuses every install, upgrade and rollback of it with \"another operation (install/upgrade/rollback) is in progress\" - "
                           "once no helm process works on it, "
                           (if last-good
                             (format "helm rollback %s %d -n %s" release last-good namespace)
                             (format "helm uninstall %s -n %s" release namespace))
                           " or delete Secret " (secret-name revision) " frees it")))

       (= "failed" status)
       (assoc base :severity (if last-good :warning :critical)
              :summary (format "Helm release's last operation failed (revision %s)" version)
              :hint (if last-good
                      (format "revision %d is still deployed - helm history %s -n %s shows what failed" last-good release namespace)
                      (format "no revision of it is deployed - helm history %s -n %s shows what failed" release namespace)))

       (= "uninstalling" status)
       (when stuck?
         (assoc base :severity :warning
                :summary (format "Helm release is stuck uninstalling (revision %s)" version)
                :hint "a resource it deletes is held by a finalizer, or the helm process died - helm uninstall again once the resources are gone"))

       :else nil)
     (onset/since (some-> (changed-at revision) str)))))

(defn detect-helm-releases
  [evidence]
  (keep #(release-finding % (:now evidence))
        (latest-revisions (kubectl/items-of (get evidence "helm-releases")))))

(def detectives
  [{:name "helm-releases"
    :description "Helm releases whose latest revision failed or is stuck pending or uninstalling"
    :requires ["helm-releases"]
    :detect detect-helm-releases}])
