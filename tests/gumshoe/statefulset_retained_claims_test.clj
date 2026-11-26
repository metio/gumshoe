;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.statefulset-retained-claims-test
  "A StatefulSet scaled down keeps the claims of its higher ordinals and
   mounts them again when it scales up, so enlarge_volumes grows them with
   the claims of the running replicas and verifies them too."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.resize-books-test :as resize]
            [gumshoe.storage :as storage]))

(defn- claim
  [name & {:keys [labels owners size namespace] :or {size "10Gi" namespace "db"}}]
  {:metadata (cond-> {:namespace namespace :name name :uid (str "uid-" name)}
               labels (assoc :labels labels)
               owners (assoc :ownerReferences owners))
   :spec {:storageClassName "fast" :resources {:requests {:storage size}}}
   :status {:capacity {:storage size}}})

(def ^:private web
  {:apiVersion "apps/v1" :kind "StatefulSet"
   :metadata {:namespace "db" :name "web" :uid "uid-web" :generation 1 :resourceVersion "1"}
   :spec {:replicas 1
          :selector {:matchLabels {:app "web"}}
          :volumeClaimTemplates [{:metadata {:name "data"}
                                  :spec {:storageClassName "fast" :resources {:requests {:storage "10Gi"}}}}]}
   :status {:replicas 1 :readyReplicas 1}})

(def ^:private labelled {:app "web"})

(def ^:private classes {:items [{:metadata {:name "fast"} :allowVolumeExpansion true}]})

(deftest the-finders-case-plans-every-claim-the-template-made
  ;; replicas 1 with data-web-0..2 present: the plan held data-web-0 alone
  (let [pvcs {:items (vec (for [i (range 3)] (claim (str "data-web-" i) :labels labelled)))}
        names (storage/statefulset-claim-names web "data" pvcs)
        plan (storage/expansion-plan {:pvc-names names :pvcs pvcs :storage-classes classes :capacity "20Gi"})]
    (is (= ["data-web-0" "data-web-1" "data-web-2"] (mapv :pvc plan)))
    (is (= [] (storage/expansion-problems plan)))
    (is (= [:claims :claims :claims :delete :recreate]
           (mapv :stage (storage/statefulset-resize-steps "ctx" web "data" "20Gi" plan))))))

(deftest a-retained-claim-belongs-by-selector-labels-or-owner
  (let [pvcs {:items [(claim "data-web-0")
                      (claim "data-web-10" :labels labelled)
                      (claim "data-web-3" :owners [{:kind "StatefulSet" :name "web" :uid "uid-web"}])
                      (claim "data-web-4" :owners [{:kind "StatefulSet" :name "web" :uid "uid-other"}])
                      (claim "data-web-5" :labels {:app "other"})
                      (claim "data-web-6" :labels labelled :namespace "elsewhere")
                      (claim "logs-web-1" :labels labelled)
                      (claim "data-webx-1" :labels labelled)]}]
    (is (= ["data-web-0" "data-web-3" "data-web-10"] (storage/statefulset-claim-names web "data" pvcs))
        "current replicas first, then retained claims by ordinal")
    (testing "a claim with an ordinal's name but nothing tying it to the StatefulSet stops the resize"
      (let [problems (storage/unclaimed-template-names web "data" pvcs)]
        (is (= 2 (count problems)))
        (is (re-find #"^PVC data-web-4 has the name of a claim of StatefulSet db/web" (first problems)))
        (is (re-find #"^PVC data-web-5 " (second problems)))))))

(deftest the-current-replicas-are-planned-by-name-as-before
  (is (= ["data-web-0"] (storage/statefulset-claim-names web "data" {:items []})))
  (is (= [] (storage/unclaimed-template-names web "data" {:items [(claim "data-web-0")]}))))

;; --- the whole book -----------------------------------------------------------------------

(def ^:private load-config @#'resize/load-config)
(def ^:private path "runbooks/kubernetes/statefulsets/enlarge_volumes.clj")
(def ^:private argv ["--namespace" "db" "--statefulset" "web" "--template" "data" "--capacity" "20"])

(defn- cluster
  [claims]
  {:objects {"statefulsets" [web]
             "persistentvolumeclaims" claims
             "storageclasses" (:items classes)
             "events" []}})

(deftest the-book-grows-and-verifies-the-retained-claims
  (let [{:keys [result out mutations checks confirmations]}
        (resize/transcript (load-config path)
                           {:state (cluster (vec (for [i (range 3)] (claim (str "data-web-" i) :labels labelled))))
                            :argv argv})
        patched (set (for [m mutations
                           :let [s (str/join " " (flatten (seq [m])))]
                           claim ["data-web-0" "data-web-1" "data-web-2"]
                           :when (and (str/includes? s "patch") (str/includes? s claim))]
                       claim))]
    (is (true? result) (str/join "\n" out))
    (is (= #{"data-web-0" "data-web-1" "data-web-2"} patched))
    (is (some #(re-find #"plan: data-web-2: 10Gi -> 20Gi \(kept from a replica that does not run now\)" %) out))
    (is (some #(re-find #"plan: data-web-0: 10Gi -> 20Gi$" %) out))
    (is (some #(= "all 3 PVCs report capacity 20Gi" (:description %)) checks))
    (is (re-find #"data-web-2" (pr-str confirmations)) "the confirmation names the retained claim")))

(deftest the-book-refuses-a-claim-it-cannot-tie-to-the-statefulset
  (let [{:keys [result out mutations]}
        (resize/transcript (load-config path)
                           {:state (cluster [(claim "data-web-0" :labels labelled) (claim "data-web-2")])
                            :argv argv})]
    (is (false? result))
    (is (some #(re-find #"PVC data-web-2 has the name of a claim of StatefulSet db/web" %) out) (str/join "\n" out))
    (is (empty? mutations) "nothing was changed")))
