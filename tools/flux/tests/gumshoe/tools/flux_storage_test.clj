;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux-storage-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.shell :as shell]
            [gumshoe.tools.flux :as flux]
            [gumshoe.tools.flux-storage :as storage]))

;; --- the pending predicate and durations --------------------------------------

(deftest a-pending-revision-is-read-from-its-status-label
  (doseq [status ["pending-install" "pending-upgrade" "pending-rollback"]]
    (is (true? (flux/pending-release? {:status status})) status))
  (doseq [status ["deployed" "superseded" "failed" "uninstalling" "" nil]]
    (is (false? (flux/pending-release? {:status status})) (pr-str status)))
  (is (false? (flux/pending-release? nil))))

(deftest go-durations-become-seconds
  (is (= 300 (flux/duration-seconds "5m0s")))
  (is (= 3600 (flux/duration-seconds "1h")))
  (is (= 5430 (flux/duration-seconds "1h30m30s")))
  (is (= 1 (flux/duration-seconds "500ms")) "a fraction of a second rounds up, so a wait is never zero")
  (is (= 90 (flux/duration-seconds "1.5m")))
  (doseq [bad [nil "" "5" "five minutes" "-5m" "5x"]]
    (is (nil? (flux/duration-seconds bad)) (pr-str bad))))

(def ^:private release
  {:metadata {:namespace "apps" :name "podinfo"}
   :spec {:interval "10m" :timeout "3m"}})

(deftest the-release-waits-come-from-its-spec
  (is (= 180 (flux/helm-timeout-seconds release)))
  (is (= 300 (flux/helm-timeout-seconds {:spec {}})) "helm-controller's default timeout is 5m")
  (is (= 600 (flux/interval-seconds release))))

(deftest an-action-is-in-flight-while-a-pending-revision-is-younger-than-the-timeout
  (let [now 10000
        row (fn [status modified] {:namespace "apps" :release "podinfo" :status status :modified-at modified})]
    (is (true? (flux/action-in-flight? [(row "pending-upgrade" (- now 60))] release now)))
    (is (false? (flux/action-in-flight? [(row "pending-upgrade" (- now 181))] release now))
        "a pending revision older than spec.timeout is parked, which is what the book clears")
    (is (false? (flux/action-in-flight? [(row "deployed" now)] release now)))
    (testing "another release's pending revision does not count"
      (is (false? (flux/action-in-flight? [(assoc (row "pending-install" now) :release "other")] release now))))
    (testing "a pending revision without a modified time falls back to its creation time"
      (is (true? (flux/action-in-flight? [{:namespace "apps" :release "podinfo" :status "pending-install"
                                           :created-at (- now 10)}] release now))))))

;; --- the owner ----------------------------------------------------------------

(defn- inventory [& ids] {:status {:inventory {:entries (mapv (fn [id] {:id id :v "v2"}) ids)}}})

(deftest the-owner-is-whoever-lists-the-helmrelease-in-its-inventory
  (let [id "apps_podinfo_helm.toolkit.fluxcd.io_HelmRelease"
        ks (merge {:metadata {:namespace "flux-system" :name "apps"} :spec {:interval "5m"}}
                  (inventory "apps_other_helm.toolkit.fluxcd.io_HelmRelease" id))
        rset (merge {:metadata {:namespace "apps" :name "podinfo-set"
                                :annotations {:fluxcd.controlplane.io/reconcileEvery "30m"}}}
                    (inventory id))
        unrelated (merge {:metadata {:namespace "flux-system" :name "infra"}} (inventory "x_y_apps_Deployment"))]
    (is (= id (storage/inventory-id release)))
    (is (= [{:kind "Kustomization" :type flux/kustomization-type :namespace "flux-system" :name "apps"}]
           (map #(dissoc % :object) (storage/owners release {"Kustomization" [ks unrelated] "ResourceSet" []}))))
    (is (= [{:kind "ResourceSet" :type storage/resourceset-type :namespace "apps" :name "podinfo-set"}]
           (map #(dissoc % :object) (storage/owners release {"Kustomization" [unrelated] "ResourceSet" [rset]}))))
    (testing "the owner's reconcile interval"
      (is (= 300 (storage/owner-interval-seconds {:kind "Kustomization" :object ks})))
      (is (= 1800 (storage/owner-interval-seconds {:kind "ResourceSet" :object rset})))
      (is (= 3600 (storage/owner-interval-seconds {:kind "ResourceSet" :object {:metadata {}}}))
          "a ResourceSet without reconcileEvery reconciles hourly"))))

(deftest suspending-an-owner-uses-its-kinds-switch
  (let [ks {:kind "Kustomization" :type flux/kustomization-type :namespace "flux-system" :name "apps"}
        rs {:kind "ResourceSet" :type storage/resourceset-type :namespace "apps" :name "set"}
        flat (fn [plan] (str/join " " (flatten plan)))]
    (is (str/includes? (flat (storage/owner-suspend-effect "ctx" ks true "T")) "{\"spec\":{\"suspend\":true}}"))
    (is (str/includes? (flat (storage/owner-suspend-effect "ctx" rs true "T")) "fluxcd.controlplane.io/reconcile=disabled"))
    (is (str/includes? (flat (storage/owner-suspend-effect "ctx" rs false "T")) "fluxcd.controlplane.io/reconcile=enabled"))
    (is (true? (storage/owner-suspended? "ResourceSet" {:metadata {:annotations {:fluxcd.controlplane.io/reconcile "disabled"}}})))
    (is (false? (storage/owner-suspended? "ResourceSet" {:metadata {}})))
    (is (true? (storage/owner-suspended? "Kustomization" {:spec {:suspend true}})))))

;; --- what the release owns --------------------------------------------------------

(defn- object [kind name & {:keys [release namespace spec] :or {namespace "apps"}}]
  {:kind kind
   :metadata (cond-> {:namespace namespace :name name}
               release (assoc :annotations {:meta.helm.sh/release-name release
                                            :meta.helm.sh/release-namespace namespace}))
   :spec spec})

(deftest workloads-and-volumes-are-the-ones-helm-recorded-for-this-release
  (let [sts (object "StatefulSet" "db" :release "podinfo"
                    :spec {:volumeClaimTemplates [{:metadata {:name "data"}}]})
        lists {"deployments" [(object "Deployment" "web" :release "podinfo")
                              (object "Deployment" "other" :release "other")
                              (object "Deployment" "by-hand")]
               "statefulsets" [sts]}
        workloads (storage/release-workloads "podinfo" "apps" lists)
        pvcs [(object "PersistentVolumeClaim" "cache" :release "podinfo")
              (object "PersistentVolumeClaim" "data-db-0")
              (object "PersistentVolumeClaim" "data-db-1")
              (object "PersistentVolumeClaim" "data-dbx-0")
              (object "PersistentVolumeClaim" "unrelated")]]
    (is (= #{["deployments" "web"] ["statefulsets" "db"]} (set (map (juxt :type :name) workloads))))
    (is (= ["cache" "data-db-0" "data-db-1"]
           (sort (map :name (storage/release-volumes "podinfo" "apps" pvcs [sts])))))))

;; --- the plan order ---------------------------------------------------------------

(def ^:private owner
  {:kind "Kustomization" :type flux/kustomization-type :namespace "flux-system" :name "apps"
   :object {:spec {:interval "5m"}}})

(def ^:private observed
  {:release (assoc release :status {})
   :release-name "podinfo" :release-namespace "apps" :storage-namespace "apps"
   :owner owner
   :known-owner owner
   :workloads [{:type "deployments" :namespace "apps" :name "web"}]
   :volumes [{:type "persistentvolumeclaims" :namespace "apps" :name "cache"}]})

(defn- plan-of [step]
  (vec (flatten ((:effect step) {:context "ctx" :target "apps/podinfo" :observed observed :timestamp "T"}))))

(deftest the-owner-is-suspended-before-anything-is-deleted-and-resumed-last
  (let [steps (storage/steps)
        changes (filterv :effect steps)
        plans (mapv plan-of changes)
        index-of (fn [pred] (first (keep-indexed #(when (pred %2) %1) plans)))
        deletes? (fn [plan] (some #{"delete"} plan))
        mentions (fn [s] (fn [plan] (some #(and (string? %) (str/includes? % s)) plan)))
        first-delete (index-of deletes?)]
    (is (some? first-delete))
    (is (< (index-of (every-pred (mentions "flux-system") (mentions "\"suspend\":true"))) first-delete)
        "the owner's suspension comes before the first delete")
    (is (< (index-of (every-pred (mentions "podinfo") (mentions "\"suspend\":true"))) first-delete)
        "the HelmRelease is suspended before the first delete")
    (is (< (index-of (mentions "deployments")) (index-of (mentions "persistentvolumeclaims")))
        "workloads go before volumes")
    (is (< (index-of (mentions "persistentvolumeclaims")) (index-of (mentions "owner=helm")))
        "the storage goes last among the deletes")
    (is (= (dec (count plans)) (index-of (every-pred (mentions "flux-system") (mentions "\"suspend\":false"))))
        "resuming the owner is the final change")
    (testing "a step that deletes refuses while an action is in flight"
      (doseq [step changes
              :when (deletes? (plan-of step))]
        (is (fn? (:precondition step)) (:title step))))))

(deftest every-wait-comes-from-live-values
  (let [w (storage/waits observed)]
    (is (= 180 (:in-flight w)) "the HelmRelease's spec.timeout")
    (is (= 180 (:deletion w)))
    (is (= (+ 180 600) (:release-ready w)) "spec.timeout plus spec.interval")
    (is (= (+ 300 180) (:owner-ready w)) "the owner's interval plus spec.timeout")))

(deftest help-names-the-owner-suspend-and-resume
  (let [titles (map :title (storage/steps))]
    (is (some #(re-find #"(?i)suspend the owner" %) titles))
    (is (some #(re-find #"(?i)resume the owner" %) titles))))

;; --- the steps against a cluster --------------------------------------------------

(defn- fake-cluster
  "A kubectl that answers `get` from state, an atom of type -> objects. A type
   mapped to :fail does not answer, one mapped to :no-type is not served."
  [state]
  (fn [& argv]
    (let [[type name] (rest (drop-while #(not= "get" %) argv))
          name (when-not (str/starts-with? (str name) "--") name)
          objects (get @state type)]
      (cond
        (= :fail objects) {:exit 1 :out "" :err "Unable to connect to the server"}
        (= :no-type objects) {:exit 1 :out "" :err (str "error: the server doesn't have a resource type \"" type "\"")}
        name (if-let [object (first (filter #(= name (-> % :metadata :name)) objects))]
               {:exit 0 :out (json/generate-string object) :err ""}
               {:exit 1 :out "" :err (str "Error from server (NotFound): " type " \"" name "\" not found")})
        :else {:exit 0 :out (json/generate-string {:items (or objects [])}) :err ""}))))

(defn- quietly
  "The value of (f), with what it prints kept off the test output."
  [f]
  (let [result (atom nil)]
    (with-out-str (binding [*err* *out*] (reset! result (f))))
    @result))

(defn- step-titled [steps re]
  (first (filter #(re-find re (str (:title %))) steps)))

(defn- derive-in [step]
  ((:derive step) {:context "ctx" :target "apps/podinfo"}))

(def ^:private template-sts
  (object "StatefulSet" "db" :release "podinfo" :spec {:volumeClaimTemplates [{:metadata {:name "data"}}]}))

(defn- cluster-state []
  {flux/helmrelease-type [release]
   "statefulsets" [template-sts]
   "persistentvolumeclaims" [(object "PersistentVolumeClaim" "data-db-0")
                             (object "PersistentVolumeClaim" "data-db-1")
                             (object "PersistentVolumeClaim" "unrelated")]})

(deftest the-volumes-step-keeps-the-template-claims-of-statefulsets-the-workloads-step-deleted
  (let [state (atom (cluster-state))
        steps (storage/steps)]
    (with-redefs [shell/execute (fake-cluster state)]
      (is (= ["statefulsets/db"] (map #(str (:type %) "/" (:name %))
                                      (:workloads (:observed (derive-in (step-titled steps #"workloads")))))))
      (swap! state assoc "statefulsets" [])
      (is (= ["data-db-0" "data-db-1"]
             (sort (map :name (:volumes (:observed (derive-in (step-titled steps #"volumes")))))))
          "the claims the deleted StatefulSet made from its templates are still the release's volumes"))))

(def ^:private owning-kustomization
  (merge {:metadata {:namespace "flux-system" :name "apps"} :spec {:interval "5m"}}
         (inventory "apps_podinfo_helm.toolkit.fluxcd.io_HelmRelease")))

(defn- step-ctx [step]
  (merge {:context "ctx" :target "apps/podinfo"} (derive-in step)))

(deftest an-unanswered-owner-read-is-not-read-as-no-owner
  (let [state (atom (assoc (cluster-state)
                           flux/kustomization-type :fail
                           storage/resourceset-type :no-type
                           "secrets" []))
        steps (storage/steps)
        suspend (step-titled steps #"Suspend the owner")]
    (with-redefs [shell/execute (fake-cluster state)]
      (is (false? (quietly #((:check (first steps)) {:context "ctx" :target "apps/podinfo"})))
          "the report refuses to go on")
      (is (false? (quietly #((:precondition suspend) (step-ctx suspend))))
          "suspending the owner refuses, where it would have planned a note")
      (doseq [step (filter :effect steps)
              :when (re-find #"Delete" (:title step))]
        (is (false? (quietly #((:precondition step) (step-ctx step)))) (:title step))))))

(deftest a-missing-resourceset-crd-means-no-resourceset-owner
  (let [state (atom (assoc (cluster-state)
                           flux/kustomization-type [owning-kustomization]
                           storage/resourceset-type :no-type))
        steps (storage/steps)
        suspend (step-titled steps #"Suspend the owner")]
    (with-redefs [shell/execute (fake-cluster state)]
      (let [ctx (step-ctx suspend)]
        (is (= "apps" (-> ctx :observed :owner :name)))
        (is (true? ((:precondition suspend) ctx)))))))

(deftest the-owner-the-run-suspended-is-resumed-when-the-owner-read-fails-later
  (let [state (atom (assoc (cluster-state)
                           flux/kustomization-type [owning-kustomization]
                           storage/resourceset-type []))
        steps (storage/steps)
        suspend (step-titled steps #"Suspend the owner")
        resume (step-titled steps #"Resume the owner")]
    (with-redefs [shell/execute (fake-cluster state)]
      (step-ctx suspend)
      (swap! state assoc flux/kustomization-type :fail)
      (let [ctx (step-ctx resume)
            plan (str/join " " (flatten ((:effect resume) ctx)))]
        (is (str/includes? plan "flux-system") plan)
        (is (str/includes? plan "\"suspend\":false") plan)
        (is (true? (if-let [precondition (:precondition resume)] (precondition ctx) true)))))))

(deftest an-unanswered-workload-or-claim-read-is-not-read-as-none
  (doseq [type ["statefulsets" "deployments" "persistentvolumeclaims"]]
    (let [state (atom (assoc (cluster-state)
                             flux/kustomization-type [owning-kustomization]
                             storage/resourceset-type []
                             "secrets" []
                             type :fail))
          steps (storage/steps)]
      (with-redefs [shell/execute (fake-cluster state)]
        (is (false? (quietly #((:check (first steps)) {:context "ctx" :target "apps/podinfo"})))
            (str "the report refuses when " type " cannot be listed"))
        (doseq [step (filter :effect steps)
                :when (re-find #"Delete" (:title step))]
          (is (false? (quietly #((:precondition step) (step-ctx step))))
              (str (:title step) " refuses when " type " cannot be listed")))))))

;; --- caller-supplied steps --------------------------------------------------------

(def ^:private database-type "database.kinda.rocks")

(def ^:private db-operator-step
  "A casebook's step, shaped like infra.run's moodle reset: db-operator's
   Database CR and the Secret and ConfigMap it renders from the CR's
   credentials.templates carry no meta.helm.sh annotations, so the playbook
   cannot discover them, and they must go after the volumes and before the
   storage."
  {:title "🗑  Delete the db-operator objects Helm never stamped"
   :prerequisites {:kubectl-can-delete [database-type "secrets" "configmaps"]}
   :confirm {:action "delete the Database and the credentials db-operator rendered from it"
             :destructive? true}
   :items (fn [_] ["delete the Database moodle, secret/moodle-database, configmap/moodle-database"])
   :effect (fn [{:keys [context]}]
             [[ "kubectl" "--context" context "-n" "apps" "delete" database-type "moodle" "--ignore-not-found"]
              [ "kubectl" "--context" context "-n" "apps" "delete" "secret" "moodle-database" "--ignore-not-found"]
              [ "kubectl" "--context" context "-n" "apps" "delete" "configmap" "moodle-database" "--ignore-not-found"]])})

(defn- index-of-plan
  "The index of the first step whose plan mentions s."
  [steps s]
  (first (keep-indexed (fn [i step]
                         (when (and (:effect step)
                                    (some #(and (string? %) (str/includes? % s)) (plan-of step)))
                           i))
                       steps)))

(deftest a-casebook-step-lands-between-the-volumes-and-the-storage-by-position-name
  (let [steps (storage/steps {:after-volumes [db-operator-step]})
        at (.indexOf steps db-operator-step)]
    (is (= (inc (count (storage/steps))) (count steps)))
    (is (= (inc (index-of-plan steps "persistentvolumeclaims")) at)
        "right after the step that deletes the volumes")
    (is (= (inc at) (index-of-plan steps "owner=helm"))
        "right before the step that deletes the storage")
    (is (< at (index-of-plan steps "\"suspend\":false"))
        "before anything resumes")))

(deftest the-playbook-takes-the-same-positions
  (let [book (storage/playbook-with {:after-volumes [db-operator-step]})]
    (is (= (dissoc storage/playbook :steps) (dissoc book :steps)))
    (is (= (inc (index-of-plan (:steps book) "persistentvolumeclaims"))
           (.indexOf (:steps book) db-operator-step)))))

(deftest every-position-sits-where-its-name-says
  (let [base (storage/steps)
        markers (into {} (for [p storage/positions :when (not= :after-resume p)]
                           [p {:title (str "marker " p) :items (fn [_] []) :effect (fn [_] [])}]))
        steps (storage/steps (update-vals markers vector))
        at #(.indexOf steps (markers %))]
    (is (= (+ (count base) 4) (count steps)))
    (is (< (index-of-plan steps "\"suspend\":true") (index-of-plan steps flux/helmrelease-type) (at :after-suspend) (index-of-plan steps "deployments")))
    (is (< (index-of-plan steps "deployments") (at :after-workloads) (index-of-plan steps "persistentvolumeclaims")))
    (is (< (index-of-plan steps "persistentvolumeclaims") (at :after-volumes) (index-of-plan steps "owner=helm")))
    (is (< (index-of-plan steps "owner=helm") (at :after-storage) (index-of-plan steps "\"suspend\":false")))))

(deftest a-step-after-the-resume-must-not-be-destructive
  (let [note {:title "📋 What to do once the install has written the credentials"
              :confirm {:action "print the follow-up"} :items (fn [_] ["print it"]) :effect (fn [_] [])}]
    (is (= note (last (storage/steps {:after-resume [note]})))
        "a step that changes nothing may follow the resume")
    (let [e (try (storage/steps {:after-resume [db-operator-step]}) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e) "a destructive step after the resume is refused when the steps are built")
      (is (re-find #"after-resume" (str (ex-message e)))))))

(deftest an-unknown-position-is-refused-when-the-steps-are-built
  (let [e (try (storage/steps {:before-storage [db-operator-step]}) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (some? e))
    (is (re-find #":before-storage" (str (ex-message e))))
    (is (= storage/positions (:positions (ex-data e))) "the error names the positions there are")))

(deftest insertion-reads-no-titles
  (let [anchored [[nil {:id 1}] [:after-volumes {:id 2}] [nil {:id 3}]]
        extra {:id :x}]
    (is (= [{:id 1} {:id 2} {:id :x} {:id 3}]
           (storage/insert-at anchored {:after-volumes [extra]}))
        "steps without any title splice by position name alone")))

;; --- recovery guidance -------------------------------------------------------------

(deftest every-step-from-the-owner-suspend-to-its-resume-carries-recovery
  (let [steps (storage/steps)
        from (first (keep-indexed #(when (re-find #"Suspend the owner" (:title %2)) %1) steps))
        to (first (keep-indexed #(when (re-find #"Resume the owner" (:title %2)) %1) steps))]
    (is (nil? (:recovery (first steps))) "the report changes nothing")
    (doseq [step (subvec steps from (inc to))]
      (is (fn? (:recovery step)) (:title step)))
    (testing "a casebook's own steps fall back to the playbook's guidance"
      (is (fn? (:recovery storage/playbook)))
      (is (fn? (:recovery (storage/playbook-with {:after-volumes [{:check (constantly true)}]})))))))

(defn- recovery-text [state]
  (let [recovery (:recovery (step-titled (storage/steps) #"Delete the release's workloads"))]
    (with-redefs [shell/execute (fake-cluster state)]
      (recovery {:context "ctx" :target "apps/podinfo" :applied 2}))))

(deftest the-recovery-names-what-is-still-suspended-and-how-to-resume-it
  (let [text (recovery-text (atom (assoc (cluster-state)
                                         flux/helmrelease-type [(assoc-in release [:spec :suspend] true)]
                                         flux/kustomization-type [(assoc-in owning-kustomization [:spec :suspend] true)]
                                         storage/resourceset-type [])))]
    (is (str/includes? text "Flux does not reconcile HelmRelease apps/podinfo while these stay suspended") text)
    (is (str/includes? text "kubectl --context=ctx -n apps patch helmreleases.helm.toolkit.fluxcd.io podinfo --type=merge --patch='{\"spec\":{\"suspend\":false}}'") text)
    (is (str/includes? text "kubectl --context=ctx -n flux-system patch kustomizations.kustomize.toolkit.fluxcd.io apps --type=merge --patch='{\"spec\":{\"suspend\":false}}'") text)
    (is (< (str/index-of text "HelmRelease apps/podinfo -") (str/index-of text "Kustomization flux-system/apps -"))
        "the HelmRelease resumes before its owner"))
  (testing "a ResourceSet owner resumes through its annotation"
    (let [text (recovery-text (atom (assoc (cluster-state)
                                           flux/kustomization-type []
                                           storage/resourceset-type
                                           [(-> owning-kustomization
                                                (assoc-in [:metadata :annotations :fluxcd.controlplane.io/reconcile] "disabled"))])))]
      (is (str/includes? text "ResourceSet flux-system/apps - resume: kubectl --context=ctx -n flux-system annotate resourcesets.fluxcd.controlplane.io apps --overwrite fluxcd.controlplane.io/reconcile=enabled") text)
      (is (not (str/includes? text "HelmRelease apps/podinfo - resume")) "the HelmRelease is not suspended here")))
  (testing "nothing suspended is said as such"
    (is (str/includes? (recovery-text (atom (assoc (cluster-state)
                                                   flux/kustomization-type [owning-kustomization]
                                                   storage/resourceset-type [])))
                       "Neither HelmRelease apps/podinfo nor its owner is suspended")))
  (testing "an unanswered read says the state is unknown rather than guessing"
    (let [text (recovery-text (atom (assoc (cluster-state)
                                           flux/kustomization-type :fail
                                           storage/resourceset-type [])))]
      (is (str/includes? text "whether HelmRelease apps/podinfo and its owner are still suspended is unknown") text))))
