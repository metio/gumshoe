;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.claim-volume-line-test
  "Every book that deletes a PersistentVolumeClaim says in its confirmation
   what happens to the bound volume: under the Delete reclaim policy the
   volume and the data on it are destroyed with the claim."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cleanup :as cleanup]
            [gumshoe.deletion :as deletion]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]))

(def ^:private claim
  {:metadata {:namespace "shop" :name "data-web-0"} :spec {:volumeName "pv-1"}})

(defn- volume
  [policy]
  {:metadata {:name "pv-1"} :spec {:persistentVolumeReclaimPolicy policy}})

(deftest the-line-names-the-reclaim-policy
  (is (= "its volume pv-1 has reclaim policy Delete - the volume and the data on it are destroyed with the claim"
         (cleanup/claim-volume-line claim {:volume (volume "Delete")})))
  (is (= "its volume pv-1 has reclaim policy Retain - the volume and its data stay, as a Released PersistentVolume"
         (cleanup/claim-volume-line claim {:volume (volume "Retain")})))
  (is (str/includes? (cleanup/claim-volume-line claim {:volume (volume "Recycle")}) "scrubbed"))
  (is (= "the reclaim policy of its volume pv-1 could not be read - under Delete the volume and the data on it are destroyed with the claim"
         (cleanup/claim-volume-line claim {:unread true})))
  (is (= "its volume pv-1 no longer exists" (cleanup/claim-volume-line claim {:volume nil})))
  (is (= "the claim is bound to no volume, so no data is deleted with it"
         (cleanup/claim-volume-line (assoc claim :spec {}) nil))))

(defn- kubectl
  "A kubectl answering the claim, its volume and every namespace listing;
   volume-answer answers the read of the volume."
  [volume-answer]
  (fn [& argv]
    (cond
      (some #{"persistentvolumes"} argv) volume-answer
      (and (some #{"persistentvolumeclaims"} argv) (some #{"data-web-0"} argv))
      {:exit 0 :out (json/generate-string claim) :err ""}
      :else {:exit 0 :out (json/generate-string {:items []}) :err ""})))

(deftest objects-delete-says-the-volume-is-destroyed
  (with-redefs [shell/execute (kubectl {:exit 0 :out (json/generate-string (volume "Delete")) :err ""})]
    (is (= ["its volume pv-1 has reclaim policy Delete - the volume and the data on it are destroyed with the claim"]
           (deletion/read-dependents "ctx" {:type "persistentvolumeclaims" :namespace "shop" :name "data-web-0"})))))

(deftest objects-delete-says-when-the-policy-is-unknown
  (with-redefs [shell/execute (kubectl {:exit 1 :out "" :err "Error from server (Forbidden): persistentvolumes is forbidden"})]
    (is (= ["the reclaim policy of its volume pv-1 could not be read - under Delete the volume and the data on it are destroyed with the claim"]
           (deletion/read-dependents "ctx" {:type "persistentvolumeclaims" :namespace "shop" :name "data-web-0"})))))

(deftest objects-delete-adds-nothing-for-other-types
  (with-redefs [shell/execute (kubectl {:exit 0 :out "{}" :err ""})]
    (is (= [] (deletion/read-dependents "ctx" {:type "configmaps" :namespace "shop" :name "c"})))))

(deftest delete-unused-confirms-with-the-same-line
  (let [spec (with-redefs [mutation/book identity] (load-file "runbooks/kubernetes/volumes/delete_unused.clj"))
        derived (with-redefs [shell/execute (kubectl {:exit 0 :out (json/generate-string (volume "Retain")) :err ""})]
                  ((:derive spec) {:context "ctx" :object claim}))]
    (is (= ["delete shop/data-web-0"
            "its volume pv-1 has reclaim policy Retain - the volume and its data stay, as a Released PersistentVolume"]
           ((:items spec) (merge {:target "shop/data-web-0"} derived))))
    (testing "the same function words both books"
      (is (= (:volume-line derived)
             (cleanup/claim-volume-line claim {:volume (volume "Retain")}))))))
