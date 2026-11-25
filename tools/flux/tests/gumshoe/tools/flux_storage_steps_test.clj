;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux-storage-steps-test
  "Pins what an operator and a casebook see of every flux-storage step: its
   title, prerequisites, confirmation, items, plan, post-checks and the
   recovery text, for a full, an empty and an unanswered observation. The
   steps are built from the shared step library, so a change there that
   alters a single word of what the playbook shows fails here."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.shell :as shell]
            [gumshoe.tools.flux :as flux]
            [gumshoe.tools.flux-storage :as storage]))

(def ^:private owner
  {:kind "Kustomization" :type flux/kustomization-type :namespace "flux-system" :name "apps"
   :object {:metadata {:namespace "flux-system" :name "apps"} :spec {:interval "5m"}}})

(def ^:private release
  {:metadata {:namespace "apps" :name "podinfo"}
   :spec {:interval "10m" :timeout "3m"}
   :status {}})

(def ^:private full
  {:release release
   :release-name "podinfo" :release-namespace "apps" :storage-namespace "apps"
   :owners [owner]
   :owner owner
   :known-owner owner
   :workloads [{:type "deployments" :namespace "apps" :name "web"}
               {:type "statefulsets" :namespace "apps" :name "db"}]
   :volumes [{:type "persistentvolumeclaims" :namespace "apps" :name "data-db-0"}]})

(def ^:private empty-release
  (assoc full :owners [] :owner nil :known-owner nil :workloads [] :volumes []))

(def ^:private unread {:unread "Unable to connect to the server"})

(defn- ctx [observed]
  {:context "ctx" :target "apps/podinfo" :observed observed :timestamp "T"})

(defn- surface
  "What a step shows for one observation: items, plan, pin and post-checks."
  [step observed]
  (let [c (ctx observed)]
    (cond-> {}
      (:items step) (assoc :items (vec ((:items step) c)))
      (:effect step) (assoc :plan (vec ((:effect step) c)))
      (:pin step) (assoc :pin ((:pin step) c))
      (:verify step) (assoc :verify (mapv #(select-keys % [:description :timeout :interval])
                                          ((:verify step) c))))))

(defn- shape [step]
  {:title (:title step)
   :keys (set (keys step))
   :prerequisites (:prerequisites step)
   :confirm (:confirm step)})

(def ^:private kustomization flux/kustomization-type)
(def ^:private helmrelease flux/helmrelease-type)

(def ^:private expected-shapes
  [{:title "🔬 Read the HelmRelease, its owner and its storage"
    :keys #{:title :prerequisites :check}
    :prerequisites {:installed-tools ["kubectl" "fzf"]
                    :cluster-capabilities [:flux]
                    :kubectl-can-get [helmrelease kustomization "persistentvolumeclaims"
                                      "deployments" "statefulsets" "daemonsets" "jobs" "cronjobs"]}
    :confirm nil}
   {:title "⏸  Suspend the owner (Kustomization or ResourceSet) that renders the HelmRelease"
    :keys #{:title :prerequisites :confirm :derive :recovery :precondition :items :effect :verify}
    :prerequisites {:kubectl-can-patch [kustomization]}
    :confirm {:action "suspend the owner that renders the HelmRelease" :destructive? false}}
   {:title "⏸  Suspend the HelmRelease and wait for a running action to finish"
    :keys #{:title :prerequisites :confirm :derive :recovery :pin :precondition :items :effect :verify}
    :prerequisites {:kubectl-can-patch [helmrelease]}
    :confirm {:action "suspend the HelmRelease" :destructive? false}}
   {:title "🗑  Delete the release's workloads"
    :keys #{:title :prerequisites :confirm :derive :recovery :precondition :items :effect :verify}
    :prerequisites {:kubectl-can-delete ["deployments" "statefulsets" "daemonsets" "jobs" "cronjobs"]}
    :confirm {:action "delete the release's workloads" :destructive? true}}
   {:title "🗑  Delete the release's volumes"
    :keys #{:title :prerequisites :confirm :derive :recovery :precondition :items :effect :verify}
    :prerequisites {:kubectl-can-delete ["persistentvolumeclaims"]}
    :confirm {:action "delete the release's volumes - their data is lost" :destructive? true :separately? true}}
   {:title "🗑  Delete the Helm storage and confirm it is empty"
    :keys #{:title :prerequisites :confirm :derive :recovery :precondition :items :effect :verify}
    :prerequisites {:kubectl-can-delete ["secrets"]}
    :confirm {:action "delete the release's Helm storage" :destructive? true}}
   {:title "▶  Resume the HelmRelease, which installs"
    :keys #{:title :prerequisites :confirm :derive :recovery :precondition :items :effect :verify}
    :prerequisites {:kubectl-can-patch [helmrelease]}
    :confirm {:action "resume the HelmRelease" :destructive? false}}
   {:title "▶  Resume the owner (Kustomization or ResourceSet)"
    :keys #{:title :prerequisites :confirm :derive :recovery :precondition :items :effect :verify}
    :prerequisites {:kubectl-can-patch [kustomization]}
    :confirm {:action "resume the owner that renders the HelmRelease" :destructive? false}}])

(deftest every-step-keeps-its-title-prerequisites-and-confirmation
  (is (= expected-shapes (mapv shape (storage/steps)))))

(deftest the-positions-are-the-ones-casebooks-name
  (is (= [:after-suspend :after-workloads :after-volumes :after-storage :after-resume] storage/positions)))

(def ^:private suspend-patch "--patch={\"spec\":{\"suspend\":true}}")
(def ^:private resume-patch "--patch={\"spec\":{\"suspend\":false}}")

(def ^:private expected-full
  [{}
   {:items ["suspend Kustomization flux-system/apps, so it does not re-apply the HelmRelease meanwhile"]
    :plan [[:kubectl "ctx" "-n" "flux-system" "patch" kustomization "apps" "--type=merge" suspend-patch]]
    :verify [{:description "Kustomization flux-system/apps is suspended" :timeout 180 :interval 5}]}
   {:items ["suspend HelmRelease apps/podinfo and wait up to 180s (spec.timeout) for a running Helm action to end"]
    :plan [[:kubectl "ctx" "-n" "apps" "patch" helmrelease "podinfo" "--type=merge" suspend-patch]]
    :pin {:type helmrelease :namespace "apps" :name "podinfo"}
    :verify [{:description "the HelmRelease is suspended and no Helm action is running" :timeout 180 :interval 5}]}
   {:items ["delete deployments apps/web" "delete statefulsets apps/db"]
    :plan [[:kubectl "ctx" "-n" "apps" "delete" "deployments" "web" "--ignore-not-found" "--wait=false"]
           [:kubectl "ctx" "-n" "apps" "delete" "statefulsets" "db" "--ignore-not-found" "--wait=false"]]
    :verify [{:description "deployments apps/web is gone" :timeout 180 :interval 5}
             {:description "statefulsets apps/db is gone" :timeout 180 :interval 5}]}
   {:items ["delete persistentvolumeclaims apps/data-db-0 and the data on it"]
    :plan [[:kubectl "ctx" "-n" "apps" "delete" "persistentvolumeclaims" "data-db-0" "--ignore-not-found" "--wait=false"]]
    :verify [{:description "persistentvolumeclaims apps/data-db-0 is gone" :timeout 180 :interval 5}]}
   {:items ["delete every Helm revision Secret of podinfo in apps, so the next action is an install"]
    :plan [[:kubectl "ctx" "-n" "apps" "delete" "secrets" "--selector=owner=helm,name=podinfo" "--ignore-not-found"]]
    :verify [{:description "the release's Helm storage is empty" :timeout 180 :interval 5}]}
   {:items ["resume HelmRelease apps/podinfo and request a reconcile, which installs it"]
    :plan [[:kubectl "ctx" "-n" "apps" "patch" helmrelease "podinfo" "--type=merge" resume-patch]
           [:kubectl "ctx" "-n" "apps" "annotate" helmrelease "podinfo" "--overwrite" "reconcile.fluxcd.io/requestedAt=T"]]
    :verify [{:description "the HelmRelease is Ready on a fresh install (revision 1, deployed)" :timeout 780 :interval 15}]}
   {:items ["resume Kustomization flux-system/apps and request a reconcile"]
    :plan [[:kubectl "ctx" "-n" "flux-system" "patch" kustomization "apps" "--type=merge" resume-patch]
           [:kubectl "ctx" "-n" "flux-system" "annotate" kustomization "apps" "--overwrite" "reconcile.fluxcd.io/requestedAt=T"]]
    :verify [{:description "Kustomization flux-system/apps is resumed and Ready" :timeout 480 :interval 15}]}])

(def ^:private expected-empty
  [{}
   {:items ["no Kustomization or ResourceSet lists the HelmRelease - nothing to suspend"]
    :plan [[:note "no owner to suspend"]]
    :verify []}
   (nth expected-full 2)
   {:items ["the release has no workloads to delete"]
    :plan [[:note "the release has no workloads"]]
    :verify []}
   {:items ["the release has no volumes to delete"]
    :plan [[:note "the release has no volumes"]]
    :verify []}
   (nth expected-full 5)
   (nth expected-full 6)
   {:items ["no owner to resume"]
    :plan [[:note "no owner to resume"]]
    :verify []}])

(def ^:private why "unknown - the cluster did not answer: Unable to connect to the server")

(deftest every-step-shows-the-same-items-plan-and-checks
  (testing "an observation with an owner, workloads and volumes"
    (is (= expected-full (mapv #(surface % full) (storage/steps)))))
  (testing "a release without an owner, workloads or volumes"
    (is (= expected-empty (mapv #(surface % empty-release) (storage/steps)))))
  (testing "an observation the cluster did not answer"
    (is (= [[why] [why] [why] [why] [why]]
           (mapv #((:items %) (ctx unread)) (subvec (storage/steps) 1 6))))
    (is (= [["resume HelmRelease apps/podinfo and request a reconcile, which installs it"] ["no owner to resume"]]
           (mapv #((:items %) (ctx unread)) (subvec (storage/steps) 6 8))))))

;; --- the checks against a cluster ------------------------------------------------

(defn- cluster
  "A kubectl that answers `get <type> <name>` from objects (type -> name ->
   object), lists Helm storage as `storage`, and answers gone-checks from
   `present` (a set of [type name])."
  [objects storage present]
  (fn [& argv]
    (let [[type name] (rest (drop-while #(not= "get" %) argv))]
      (cond
        (= "secrets" type) {:exit 0 :out storage :err ""}
        (some #{"--ignore-not-found"} argv)
        {:exit 0 :out (if (present [type name]) (str type "/" name) "") :err ""}
        :else (if-let [object (get-in objects [type name])]
                {:exit 0 :out (json/generate-string object) :err ""}
                {:exit 1 :out "" :err "Error from server (NotFound)"})))))

(defn- revision [status version]
  (str/join "\t" ["apps" "podinfo" status (str version) "" "" "2026-11-24T08:00:00Z"]))

(def ^:private ready {:conditions [{:type "Ready" :status "True"}]})

(defn- checks
  "Whether each post-check of every step holds, for one observation."
  [observed]
  (mapv (fn [step] (mapv #(boolean ((:check %))) (if-let [v (:verify step)] (v (ctx observed)) [])))
        (storage/steps)))

(deftest every-post-check-reads-what-it-did-before
  (testing "everything done"
    (with-redefs [shell/execute (cluster {kustomization {"apps" {:spec {:suspend true}}}
                                          helmrelease {"podinfo" (assoc-in release [:spec :suspend] true)}}
                                         (revision "deployed" 3) #{})]
      (is (= [[] [true] [true] [true true] [true] [false] [false] [false]] (checks full))
          "suspended and gone; the storage still holds a revision and nothing is Ready"))
    (with-redefs [shell/execute (cluster {kustomization {"apps" {:spec {:suspend false} :status ready}}
                                          helmrelease {"podinfo" (assoc release :status ready)}}
                                         "" #{})]
      (is (= [[] [false] [false] [true true] [true] [true] [false] [true]] (checks full))
          "the storage is empty, the owner resumed and Ready; no revision 1 yet"))
    (with-redefs [shell/execute (cluster {helmrelease {"podinfo" (assoc release :status ready)}}
                                         (revision "deployed" 1) #{})]
      (is (true? (first (nth (checks full) 6))) "Ready on revision 1, deployed")))
  (testing "nothing done"
    (with-redefs [shell/execute (cluster {kustomization {"apps" {:spec {}}}
                                          helmrelease {"podinfo" (assoc-in release [:spec :suspend] true)}}
                                         (revision "pending-upgrade" 4)
                                         #{["deployments" "web"] ["statefulsets" "db"] ["persistentvolumeclaims" "data-db-0"]})]
      (is (= [[] [false] [false] [false false] [false] [false] [false] [false]] (checks full))
          "the HelmRelease is suspended, but a pending revision without a time is an action still running"))))

;; --- recovery ----------------------------------------------------------------------

(deftest the-recovery-text-is-kept-word-for-word
  (is (= (str "Flux does not reconcile HelmRelease apps/podinfo while these stay suspended:\n"
              "  HelmRelease apps/podinfo - resume: kubectl --context=ctx -n apps patch helmreleases.helm.toolkit.fluxcd.io podinfo --type=merge --patch='{\"spec\":{\"suspend\":false}}'\n"
              "  Kustomization flux-system/apps - resume: kubectl --context=ctx -n flux-system patch kustomizations.kustomize.toolkit.fluxcd.io apps --type=merge --patch='{\"spec\":{\"suspend\":false}}'\n"
              "The release's workloads, volumes and Helm storage stay as the steps before this one left them.\n"
              "Resume the HelmRelease once its Helm storage no longer holds the revision that blocked it, then the owner.\n"
              "Running this playbook again repeats every step, but no longer finds the claims a StatefulSet it\n"
              "already deleted made from its volumeClaimTemplates; delete those by hand.")
         (storage/recovery-text "ctx" "apps/podinfo"
                                {:release (assoc-in release [:spec :suspend] true)
                                 :owner (assoc-in owner [:object :spec :suspend] true)})))
  (is (= "  ResourceSet apps/set - resume: kubectl --context=ctx -n apps annotate resourcesets.fluxcd.controlplane.io set --overwrite fluxcd.controlplane.io/reconcile=enabled"
         (second (str/split-lines
                  (storage/recovery-text "ctx" "apps/podinfo"
                                         {:release release
                                          :owner {:kind "ResourceSet" :type storage/resourceset-type
                                                  :namespace "apps" :name "set"
                                                  :object {:metadata {:annotations {:fluxcd.controlplane.io/reconcile "disabled"}}}}})))))
  (is (= "Neither HelmRelease apps/podinfo nor its owner is suspended; Flux reconciles the release again."
         (storage/recovery-text "ctx" "apps/podinfo" {:release release :owner owner})))
  (is (= "HelmRelease apps/podinfo cannot be read. Check whether the Kustomization or ResourceSet that rendered it is still suspended."
         (storage/recovery-text "ctx" "apps/podinfo" nil)))
  (is (= (str "The cluster did not answer (boom), so whether HelmRelease apps/podinfo and its owner are still suspended"
              " is unknown. Read both before leaving them: while either is suspended, Flux does not reconcile the release.")
         (storage/recovery-text "ctx" "apps/podinfo" {:unread "boom"}))))

(deftest the-same-recovery-covers-every-changing-step-and-the-playbook
  (let [steps (storage/steps)]
    (is (apply = storage/recovery (keep :recovery steps)))
    (is (= 7 (count (keep :recovery steps))))
    (is (= storage/recovery (:recovery storage/playbook)))))

;; --- the position machinery's refusals ----------------------------------------------

(defn- refusal [extra]
  (try (storage/steps extra) nil (catch clojure.lang.ExceptionInfo e [(ex-message e) (ex-data e)])))

(deftest the-refusals-keep-their-wording
  (is (= ["the extra steps must be a map of position -> steps"
          {:extra [:x] :positions storage/positions}]
         (refusal [:x])))
  (is (= [":before-storage is not a position of the flux-storage steps; the positions are [:after-suspend :after-workloads :after-volumes :after-storage :after-resume]"
          {:position :before-storage :positions storage/positions}]
         (refusal {:before-storage []})))
  (is (= ["the steps for :after-volumes must be a vector of step maps"
          {:position :after-volumes :steps [:not-a-step]}]
         (refusal {:after-volumes [:not-a-step]})))
  (is (= ["a destructive step cannot go at :after-resume, after the HelmRelease installed afresh: \"wipe\""
          {:position :after-resume :step "wipe"}]
         (refusal {:after-resume [{:title "wipe" :confirm {:destructive? true}}]}))))
