;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-storage-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.cnpg.storage :as storage]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.cnpg-storage :as detective]))

(def ^:private cluster
  {:metadata {:namespace "cloudnative-pg-cluster-general"
              :name "cloudnative-pg-cluster-general"}
   :spec {:storage {:size "40Gi"}}})

(defn- pvc
  [name requested capacity]
  {:metadata {:namespace "cloudnative-pg-cluster-general" :name name
              :labels {:cnpg.io/cluster "cloudnative-pg-cluster-general"}}
   :spec {:resources {:requests {:storage requested}}}
   :status (when capacity {:capacity {:storage capacity}})})

(defn- findings
  [& pvcs]
  (detective/detect-pending-storage {cnpg/cluster-type {:items [cluster]}
                                     "persistentvolumeclaims" {:items pvcs}}))

;; The state on kube.bastelgenosse.de after storage.size went to 40Gi: the spec
;; said 40Gi, every claim still requested 20Gi, and the operator was waiting.
(deftest a-claim-below-the-spec-is-the-enlargement-nobody-ran-test
  (let [[finding :as all] (findings (pvc "cloudnative-pg-cluster-general-1" "20Gi" "20Gi"))]
    (is (= 1 (count all)))
    (is (= :critical (:severity finding)))
    (is (= "cloudnative-pg-cluster-general/cloudnative-pg-cluster-general-1"
           (:component finding)))
    (is (re-find #"asks for 40.0GiB and this volume requests 20.0GiB" (:summary finding)))
    (is (re-find #"does not enlarge a volume" (:hint finding)))))

(deftest every-volume-short-of-the-spec-is-reported-test
  (is (= 3 (count (findings (pvc "cloudnative-pg-cluster-general-1" "20Gi" "20Gi")
                            (pvc "cloudnative-pg-cluster-general-2" "20Gi" "20Gi")
                            (pvc "cloudnative-pg-cluster-general-3" "20Gi" "20Gi"))))))

(deftest a-patched-claim-still-expanding-is-reported-as-in-flight-test
  (let [finding (first (findings (pvc "cloudnative-pg-cluster-general-1" "40Gi" "20Gi")))]
    (is (= :warning (:severity finding)))
    (is (re-find #"requests 40.0GiB and has 20.0GiB" (:summary finding)))
    (is (re-find #"pod restarted" (:hint finding)))))

(deftest a-volume-at-the-requested-size-is-quiet-test
  (is (empty? (findings (pvc "cloudnative-pg-cluster-general-1" "40Gi" "40Gi")))))

(deftest a-claim-larger-than-the-spec-is-not-a-finding-test
  (is (empty? (findings (pvc "cloudnative-pg-cluster-general-1" "80Gi" "80Gi")))
      "a volume bigger than asked for costs money, not availability"))

(deftest a-claim-with-no-reported-capacity-is-judged-on-its-request-alone-test
  (is (empty? (findings (pvc "cloudnative-pg-cluster-general-1" "40Gi" nil))))
  (is (seq (findings (pvc "cloudnative-pg-cluster-general-1" "20Gi" nil)))))

(deftest an-unreadable-size-is-not-guessed-at-test
  (is (= :unknown (:state (storage/assess-volume nil (pvc "a-1" "20Gi" "20Gi")))))
  (is (= :unknown (:state (storage/assess-volume 100.0 (pvc "a-1" nil nil)))))
  (is (empty? (findings (pvc "cloudnative-pg-cluster-general-1" nil nil)))))

(deftest another-clusters-volume-is-not-compared-test
  (let [other {:metadata {:namespace "cloudnative-pg-cluster-general" :name "elsewhere-1"
                          :labels {:cnpg.io/cluster "elsewhere"}}
               :spec {:resources {:requests {:storage "1Gi"}}}}]
    (is (empty? (findings other)))))

(deftest a-volume-in-another-namespace-is-not-compared-test
  (let [elsewhere (assoc-in (pvc "cloudnative-pg-cluster-general-1" "20Gi" "20Gi")
                            [:metadata :namespace] "other")]
    (is (empty? (findings elsewhere)))))

(deftest a-volume-matching-only-by-name-is-still-compared-test
  (let [unlabelled (update (pvc "cloudnative-pg-cluster-general-1" "20Gi" "20Gi")
                           :metadata dissoc :labels)]
    (is (seq (findings unlabelled))
        "a volume the operator adopted without the label is still short of the spec")))
