;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.claims-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.claims :as claims]
            [gumshoe.cleanup :as cleanup]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation-books-test]))

(defn- pvc
  [namespace name & {:keys [labels owners]}]
  {:metadata (cond-> {:namespace namespace :name name :uid (str "uid-" name) :resourceVersion "1"}
               labels (assoc :labels labels)
               owners (assoc :ownerReferences owners))})

(defn- volume [claim] {:name "v" :persistentVolumeClaim {:claimName claim}})

(defn- statefulset
  [name replicas & {:keys [templates volumes retention]}]
  {:metadata {:namespace "db" :name name :uid (str "uid-" name)}
   :spec (cond-> {:replicas replicas
                  :selector {:matchLabels {:app name}}
                  :template {:spec {:volumes (vec volumes)}}
                  :volumeClaimTemplates (vec (for [t (or templates ["data"])] {:metadata {:name t}}))}
           retention (assoc :persistentVolumeClaimRetentionPolicy retention))})

(defn- workload
  [type kind name & {:keys [claim replicas suspend]}]
  (let [pod-spec {:volumes [(volume claim)]}]
    {:metadata {:namespace "db" :name name}
     :kind kind
     :spec (cond-> (if (= "cronjobs" type)
                     {:jobTemplate {:spec {:template {:spec pod-spec}}}}
                     {:template {:spec pod-spec}})
             (some? replicas) (assoc :replicas replicas)
             (some? suspend) (assoc :suspend suspend))}))

(defn- reads
  "Answered holder reads of the objects, [type object] pairs."
  [objects]
  (into {} (for [type claims/holder-types]
             [type {:ok? true :doc {:items (vec (for [[t o] objects :when (= t type)] o))}}])))

(defn- holders
  [objects claim]
  (claims/holders (claims/index (reads objects)) claim))

;; --- the finder's case --------------------------------------------------------------

(deftest a-statefulset-at-zero-and-a-cronjob-keep-their-claims-with-no-pod-running
  (let [pvcs {:items [(pvc "db" "data-pg-0") (pvc "db" "backup-scratch") (pvc "db" "spare")]}
        objects [["statefulsets" (statefulset "pg" 0)]
                 ["cronjobs" (workload "cronjobs" "CronJob" "backup" :claim "backup-scratch")]]]
    (is (= ["db/spare"] (claims/unused pvcs (reads objects))))
    (is (= "PersistentVolumeClaim db/data-pg-0 is kept by StatefulSet db/pg (scaled to 0) as the claim of its ordinal 0"
           (cleanup/claim-refusal {:namespace "db" :name "data-pg-0"} (pvc "db" "data-pg-0") (reads objects))))
    (is (= "PersistentVolumeClaim db/backup-scratch is named in the pod template of CronJob db/backup"
           (cleanup/claim-refusal {:namespace "db" :name "backup-scratch"} (pvc "db" "backup-scratch") (reads objects))))))

;; --- each holder ---------------------------------------------------------------------

(deftest every-workload-kind-holds-the-claims-its-pod-template-names
  (doseq [[type kind extra state] [["deployments" "Deployment" {:replicas 0} " (scaled to 0)"]
                                   ["statefulsets" "StatefulSet" {:replicas 0} " (scaled to 0)"]
                                   ["daemonsets" "DaemonSet" {} ""]
                                   ["replicasets" "ReplicaSet" {:replicas 0} " (scaled to 0)"]
                                   ["jobs" "Job" {:suspend true} " (suspended)"]
                                   ["cronjobs" "CronJob" {:suspend true} " (suspended)"]
                                   ["deployments" "Deployment" {:replicas 2} ""]]]
    (testing kind
      (let [object (apply workload type kind "app" :claim "shared" (mapcat identity extra))]
        (is (= [(format "named in the pod template of %s db/app%s" kind state)]
               (holders [[type object]] (pvc "db" "shared"))))
        (is (= [] (holders [[type object]] (pvc "db" "other"))) "another claim")
        (is (= [] (holders [[type object]] (pvc "web" "shared"))) "the same name in another namespace")))))

(deftest a-statefulset-keeps-the-claims-of-every-ordinal-of-every-template
  (let [sts (statefulset "pg" 1 :templates ["data" "wal"])]
    (doseq [[claim ordinal] [["data-pg-0" 0] ["data-pg-1" 1] ["data-pg-7" 7] ["wal-pg-2" 2]]]
      (is (= [(format "kept by StatefulSet db/pg as the claim of its ordinal %d" ordinal)]
             (holders [["statefulsets" sts]] (pvc "db" claim)))
          claim))
    (doseq [claim ["data-pg" "data-pg-" "data-pg-x" "data-pg-01" "data-pgx-0" "logs-pg-0" "data-pg-0-old"]]
      (is (= [] (holders [["statefulsets" sts]] (pvc "db" claim))) claim))))

(deftest a-statefulset-that-deletes-claims-on-scale-down-still-keeps-those-left
  ;; the most careful reading: a claim the controller left is the operator's to delete
  (is (seq (holders [["statefulsets" (statefulset "pg" 0 :retention {:whenScaled "Delete" :whenDeleted "Delete"})]]
                    (pvc "db" "data-pg-3")))))

(deftest an-owner-reference-holds-the-claim
  (is (= ["owned by Cluster pg"]
         (holders [] (pvc "db" "pg-1" :owners [{:apiVersion "postgresql.cnpg.io/v1" :kind "Cluster" :name "pg"}])))))

(deftest pods-come-first-and-every-holder-is-named
  (let [objects [["pods" {:metadata {:namespace "db" :name "pg-0"} :spec {:volumes [(volume "data-pg-0")]}}]
                 ["statefulsets" (statefulset "pg" 1 :volumes [(volume "data-pg-0")])]]]
    (is (= ["used by pod(s) pg-0"
            "kept by StatefulSet db/pg as the claim of its ordinal 0"
            "named in the pod template of StatefulSet db/pg"]
           (holders objects (pvc "db" "data-pg-0"))))))

;; --- unread reads ---------------------------------------------------------------------

(deftest an-unread-workload-list-never-means-unused
  (doseq [type claims/holder-types]
    (testing type
      (let [partial (assoc (reads []) type {:ok? false :doc nil})]
        (is (= [type] (claims/unread partial)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo (re-pattern (str "could not read the " type))
                              (claims/unused {:items [(pvc "db" "spare")]} partial)))
        (is (re-find (re-pattern (str "the " type " of namespace db could not be read"))
                     (cleanup/claim-refusal {:namespace "db" :name "spare"} (pvc "db" "spare") partial)))))))

(deftest a-pods-read-alone-refuses
  ;; the shape the refusal took when it consulted the pods only
  (is (re-find #"could not be read"
               (cleanup/claim-refusal {:namespace "db" :name "spare"} (pvc "db" "spare") {:ok? true :doc {:items []}}))))

;; --- the candidate query ---------------------------------------------------------------

(defn- answering
  "A get-json-result over objects that fails the types in failing."
  [objects failing]
  (fn [_ type & _]
    (if (contains? failing type)
      {:ok? false :doc nil}
      {:ok? true :doc {:items (vec (for [[t o] objects :when (= t type)] o))}})))

(deftest the-candidates-leave-out-held-claims-and-fail-closed
  (let [objects [["persistentvolumeclaims" (pvc "db" "data-pg-0")]
                 ["persistentvolumeclaims" (pvc "db" "spare")]
                 ["statefulsets" (statefulset "pg" 0)]]]
    (with-redefs [kubectl/get-json-result (answering objects #{})]
      (is (= ["db/spare"] (claims/unused-candidates "ctx"))))
    (doseq [type (conj claims/holder-types "persistentvolumeclaims")]
      (with-redefs [kubectl/get-json-result (answering objects #{type})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"refusing to call any claim unused"
                              (claims/unused-candidates "ctx"))
            type)))))

(deftest the-reads-ask-every-holder-type-in-the-claims-namespace
  (let [asked (atom [])]
    (with-redefs [kubectl/get-json-result (fn [_ & args] (swap! asked conj (vec args)) {:ok? true :doc {:items []}})]
      (claims/read-holders "ctx" "db"))
    (is (= (set (for [type claims/holder-types] [type "--namespace=db"])) (set @asked)))))

;; --- the book ---------------------------------------------------------------------------

(def ^:private judged @#'gumshoe.mutation-books-test/judged)
(def ^:private path "runbooks/kubernetes/volumes/delete_unused.clj")

(defn- refused?
  [{:keys [result out]} re]
  (and (false? result) (boolean (re-find re (str out)))))

(deftest the-book-refuses-a-claim-a-workload-keeps
  (let [claim ["persistentvolumeclaims" (pvc "db" "data-pg-0")]]
    (testing "a StatefulSet scaled to 0 refuses before the confirmation, naming it"
      (let [verdict (judged path {:objects [claim ["statefulsets" (statefulset "pg" 0)]] :target "db/data-pg-0"})]
        (is (false? (:asked? verdict)))
        (is (refused? verdict #"refusing: .*kept by StatefulSet db/pg \(scaled to 0\)") (:out verdict))))
    (testing "a CronJob created during the confirmation refuses after it"
      (let [verdict (judged path {:objects [claim] :target "db/data-pg-0"
                                  :between {:objects [claim ["cronjobs" (workload "cronjobs" "CronJob" "backup" :claim "data-pg-0")]]}})]
        (is (refused? verdict #"refusing: .*CronJob db/backup") (:out verdict))
        (is (not-any? #(some #{"delete"} %) (:commands verdict)) "nothing was deleted")))
    (testing "a claim nothing holds is deleted"
      (is (true? (:result (judged path {:objects [claim] :target "db/data-pg-0"})))))))
