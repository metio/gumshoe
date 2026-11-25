;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.resources-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.detectives.capacity :as capacity]
            [gumshoe.detectives.controlplane :as controlplane]
            [gumshoe.detectives.csi :as csi]
            [gumshoe.detectives.quotas :as quotas]
            [gumshoe.quantity :as quantity]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest quantity-parsing-test
  (testing "cpu quantities become cores"
    (is (= 0.1 (quantity/quantity->number "100m")))
    (is (= 2.0 (quantity/quantity->number "2")))
    (is (= 1.5 (quantity/quantity->number "1500m"))))
  (testing "memory quantities become bytes, binary and decimal"
    (is (= 134217728.0 (quantity/quantity->number "128Mi")))
    (is (= 1073741824.0 (quantity/quantity->number "1Gi")))
    (is (= 5.0E8 (quantity/quantity->number "500M"))))
  (testing "plain counts and unparseable input"
    (is (= 10.0 (quantity/quantity->number "10")))
    (is (nil? (quantity/quantity->number "notaquantity")))
    (is (nil? (quantity/quantity->number nil))))
  (testing "sum skips unparseable, ratio guards zero and nil"
    (is (= 2.6 (quantity/sum ["100m" "2500m" "bad"])))
    (is (= 3/4 (rationalize (quantity/ratio "1500m" "2"))))
    (is (nil? (quantity/ratio "1" "0")))
    (is (nil? (quantity/ratio "1" "bad")))))

(deftest control-plane-detective-test
  (let [evidence {"pods" {:items [{:metadata {:namespace "kube-system" :name "kube-apiserver-cp1"
                                              :labels {:component "kube-apiserver"}}
                                   :status {:phase "Running"
                                            :conditions [{:type "Ready" :status "True"}]}}
                                  {:metadata {:namespace "kube-system" :name "kube-scheduler-cp1"
                                              :labels {:component "kube-scheduler"}}
                                   :status {:phase "Running"
                                            :conditions [{:type "Ready" :status "False"}]}}
                                  {:metadata {:namespace "kube-system" :name "etcd-cp1"
                                              :labels {:component "etcd"}}
                                   :status {:phase "CrashLoopBackOff"}}
                                  {:metadata {:namespace "app" :name "web"
                                              :labels {:component "web"}}
                                   :status {:phase "Running"
                                            :conditions [{:type "Ready" :status "True"}]}}
                                  ;; a user workload that merely carries a
                                  ;; control-plane component label, outside kube-system
                                  {:metadata {:namespace "app" :name "fake-etcd"
                                              :labels {:component "etcd"}}
                                   :status {:phase "CrashLoopBackOff"}}]}}
        findings (controlplane/detect-control-plane evidence)]
    (testing "only unready control-plane components are reported, app pods are ignored"
      (is (= #{"control-plane scheduler is not ready (phase Running)"
               "control-plane etcd is not ready (phase CrashLoopBackOff)"}
             (summaries findings)))
      (is (every? #(= :critical (:severity %)) findings)))
    (testing "a workload labelled like a control-plane component outside kube-system is not a control-plane outage"
      (is (not (some #(= "app/fake-etcd" (:component %)) findings))))))

(deftest orphaned-local-volumes-detective-test
  (let [pv (fn [name host] {:metadata {:name name}
                            :spec {:nodeAffinity
                                   {:required {:nodeSelectorTerms
                                               [{:matchExpressions
                                                 [{:key "kubernetes.io/hostname" :operator "In" :values [host]}]}]}}}})
        pvs {:items [(pv "data-worker-1" "worker-1") (pv "data-gone" "worker-99")]}]
    (testing "a local PV pinned to a node that no longer exists is critical"
      (let [findings (csi/detect-orphaned-local-volumes
                      {"nodes" {:items [{:metadata {:name "worker-1"}}]}
                       "persistentvolumes" pvs})]
        (is (= ["data-gone"] (map :component findings)))
        (is (every? #(= :critical (:severity %)) findings))))
    (testing "with no node evidence nothing is flagged - a failed nodes fetch must not orphan every volume"
      (is (empty? (csi/detect-orphaned-local-volumes {"nodes" nil "persistentvolumes" pvs}))))))

(deftest quota-detective-test
  (let [evidence {"resourcequotas"
                  {:items [{:metadata {:namespace "team-a" :name "compute"}
                            :status {:hard {:requests.cpu "10" :requests.memory "20Gi"}
                                     :used {:requests.cpu "10" :requests.memory "12Gi"}}}
                           {:metadata {:namespace "team-b" :name "compute"}
                            :status {:hard {:pods "50"}
                                     :used {:pods "47"}}}
                           {:metadata {:namespace "team-c" :name "roomy"}
                            :status {:hard {:requests.cpu "10"}
                                     :used {:requests.cpu "2"}}}
                           {:metadata {:namespace "team-d" :name "counts"}
                            :status {:hard {:count/pods "5"}
                                     :used {:count/pods "5"}}}]}}
        findings (quotas/detect-quota-pressure evidence)]
    (testing "exhausted quotas are critical, near-full warn, roomy ones are silent"
      (is (= #{"quota is 100% used (10 of 10)"
               "quota is 94% used (47 of 50)"
               "quota is 100% used (5 of 5)"}
             (summaries findings)))
      (is (= :critical (:severity (first (filter #(re-find #"cpu" (:component %)) findings)))))
      (is (= :warning (:severity (first (filter #(re-find #"pods" (:component %)) findings))))))
    (testing "a count/ quota keeps its 'count/' prefix in the component label"
      (is (some #(= "team-d/counts (count/pods)" (:component %)) findings)))))

(deftest overcommit-detective-test
  (testing "requests over total capacity are critical"
    (is (= #{"CPU requests (10.0 cores) exceed total allocatable (8.0 cores)"}
           (summaries (filter #(re-find #"CPU" (:summary %))
                              (capacity/detect-overcommit
                               {"nodes" {:items [{:status {:allocatable {:cpu "4" :memory "8Gi"}}}
                                                 {:status {:allocatable {:cpu "4" :memory "8Gi"}}}]}
                               "pods" {:items [{:spec {:containers [{:resources {:requests {:cpu "10" :memory "1Gi"}}}]}}]}}))))))
  (testing "requests over capacity-minus-largest-node warn about node-failure tolerance"
    (let [findings (capacity/detect-overcommit
                    {"nodes" {:items [{:status {:allocatable {:cpu "4" :memory "8Gi"}}}
                                      {:status {:allocatable {:cpu "4" :memory "8Gi"}}}]}
                     "pods" {:items [{:spec {:containers [{:resources {:requests {:cpu "6" :memory "1Gi"}}}]}}]}})]
      (is (contains? (summaries findings)
                     "CPU requests (6.0 cores) exceed capacity without the largest node (4.0 cores)"))
      (is (= :warning (:severity (first (filter #(re-find #"CPU" (:summary %)) findings)))))))
  (testing "a comfortably-provisioned cluster is silent"
    (is (empty? (capacity/detect-overcommit
                 {"nodes" {:items [{:status {:allocatable {:cpu "8" :memory "16Gi"}}}
                                   {:status {:allocatable {:cpu "8" :memory "16Gi"}}}]}
                  "pods" {:items [{:spec {:containers [{:resources {:requests {:cpu "2" :memory "4Gi"}}}]}}]}}))))
  (testing "terminated pods (Succeeded/Failed) do not count toward the commitment"
    (is (empty? (capacity/detect-overcommit
                 {"nodes" {:items [{:status {:allocatable {:cpu "8" :memory "16Gi"}}}
                                   {:status {:allocatable {:cpu "8" :memory "16Gi"}}}]}
                  "pods" {:items [{:status {:phase "Succeeded"}
                                   :spec {:containers [{:resources {:requests {:cpu "100" :memory "200Gi"}}}]}}
                                  {:status {:phase "Failed"}
                                   :spec {:containers [{:resources {:requests {:cpu "100" :memory "200Gi"}}}]}}
                                  {:status {:phase "Running"}
                                   :spec {:containers [{:resources {:requests {:cpu "1" :memory "1Gi"}}}]}}]}})))))

(deftest a-pod-is-charged-for-its-init-containers-and-sidecars
  (let [cpu (fn [n] {:resources {:requests {:cpu n}}})
        pod {:status {:phase "Running"}
             :spec {:overhead {:cpu "500m"}
                    :initContainers [(assoc (cpu "1") :name "proxy" :restartPolicy "Always")
                                     (assoc (cpu "3") :name "migrate")]
                    :containers [(assoc (cpu "1") :name "app")]}}
        node (fn [n] {:status {:allocatable {:cpu n :memory "64Gi"}}})]
    ;; max(app 1 + proxy 1, migrate 3 + proxy 1) + overhead 0.5 = 4.5 cores
    (is (= ["CPU requests (4.5 cores) exceed capacity without the largest node (4.0 cores)"]
           (map :summary (capacity/detect-overcommit {"pods" {:items [pod]}
                                                      "nodes" {:items [(node "4") (node "4")]}}))))))

(defn- close?
  [expected actual]
  (and (some? actual)
       (<= (Math/abs (- (double expected) (double actual)))
           (* 1e-9 (max 1.0 (Math/abs (double expected)))))))

(deftest quantity-grammar-forms-test
  (testing "the decimal exponent form apimachinery keeps when it serialises such a value"
    (doseq [[text expected] [["129e6" 1.29e8] ["1E3" 1000.0] ["1e3" 1000.0] ["5e-1" 0.5]
                             ["1.5E+2" 150.0] ["2e0" 2.0] ["0e9" 0.0] [".5e1" 5.0]]]
      (is (close? expected (quantity/quantity->number text)) text)))
  (testing "129e6 and 129M are the same memory request"
    (is (= (quantity/quantity->number "129M") (quantity/quantity->number "129e6"))))
  (testing "a bare E is the exa suffix, not an exponent"
    (is (= 1e18 (quantity/quantity->number "1E")))
    (is (close? 1.152921504606846976E18 (quantity/quantity->number "1Ei"))))
  (testing "signs, a leading point and a trailing point"
    (doseq [[text expected] [["+1" 1.0] ["-1" -1.0] [".5" 0.5] ["1." 1.0] ["+.5Gi" 5.36870912E8]
                             ["-100m" -0.1] ["+2Ki" 2048.0]]]
      (is (close? expected (quantity/quantity->number text)) text)))
  (testing "malformed and non-finite values are nil, never an exception or infinity"
    (doseq [text ["1e" "e3" "E" "1.2.3" "1Kii" "1 Mi" "--1" "+-1" "." "1e3Mi" "1ee3" "Mi"
                  "1e400" "1e99999999999999999999" "" "  " "1e-400x"]]
      (is (nil? (quantity/quantity->number text)) (pr-str text)))))

(def ^:private suffixes
  {"" 1.0 "m" 1e-3 "k" 1e3 "M" 1e6 "G" 1e9 "T" 1e12 "P" 1e15 "E" 1e18
   "Ki" 1024.0 "Mi" 1048576.0 "Gi" 1073741824.0 "Ti" 1.099511627776E12})

(deftest quantity-suffix-and-sign-property
  (let [property (prop/for-all [whole gen/nat
                                fraction (gen/choose 0 999)
                                suffix (gen/elements (keys suffixes))
                                sign (gen/elements ["" "+" "-"])]
                   (let [text (str sign whole "." fraction suffix)
                         expected (* (if (= "-" sign) -1 1)
                                     (Double/parseDouble (str whole "." fraction))
                                     (get suffixes suffix))]
                     (close? expected (quantity/quantity->number text))))]
    (is (:pass? (tc/quick-check 500 property :seed 20261125)))))

(deftest quantity-exponent-property
  (let [property (prop/for-all [mantissa (gen/choose 0 99999)
                                exponent (gen/choose -9 18)
                                e (gen/elements ["e" "E"])
                                plus? gen/boolean]
                   (let [text (str mantissa e (when (and plus? (not (neg? exponent))) "+") exponent)]
                     (close? (* mantissa (Math/pow 10 exponent)) (quantity/quantity->number text))))]
    (is (:pass? (tc/quick-check 500 property :seed 20261125)))))

(deftest quantity-never-throws-property
  (let [property (prop/for-all [text (gen/fmap #(apply str %)
                                               (gen/vector (gen/elements ["1" "9" "0" "." "e" "E" "+" "-"
                                                                          "Ki" "M" "m" "i" " " "x"])
                                                           0 12))]
                   (let [n (quantity/quantity->number text)]
                     (or (nil? n) (Double/isFinite n))))]
    (is (:pass? (tc/quick-check 1000 property :seed 20261125)))))
