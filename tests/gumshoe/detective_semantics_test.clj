;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detective-semantics-test
  "What each detective's finding claims must hold for the object it reads:
   an owner's unread VTEP is no conflict, an event's lifetime count is no
   hourly rate, a deletion in flight is not stuck, a duplicated env name is no
   drift, a hostname label is what a local volume is pinned to, pod-level
   requests are charged, Calico's namespaced policies count, an attachment
   still attaching is not failing, and events come newest first."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.capacity :as capacity]
            [gumshoe.detectives.csi :as csi]
            [gumshoe.detectives.events :as events]
            [gumshoe.detectives.evpn :as evpn-detectives]
            [gumshoe.detectives.namespaces :as namespaces]
            [gumshoe.detectives.network :as network]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.detectives.template-drift :as drift]
            [gumshoe.evpn :as evpn]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]))

;; --- evpn: an owner whose own VTEP was not read ------------------------------------

(def ^:private guest "52:54:00:aa:bb:cc")

(deftest an-owner-without-a-vtep-is-no-ownership-conflict
  (let [evidence {"evpn-macs" [{:host "a" :host-vtep nil :vni 1 :mac guest :type :local :intf "tap0"}
                               {:host "b" :host-vtep "10.0.0.2" :vni 1 :mac guest :type :remote :vtep "10.0.0.1"}]}]
    (is (empty? (evpn-detectives/detect-ownership-conflicts evidence)))))

(deftest an-owner-with-a-vtep-still-proves-a-conflict
  (let [evidence {"evpn-macs" [{:host "a" :host-vtep "10.0.0.1" :vni 1 :mac guest :type :local :intf "tap0"}
                               {:host "b" :host-vtep "10.0.0.2" :vni 1 :mac guest :type :remote :vtep "10.0.0.9"}]}]
    (is (= [:critical] (map :severity (evpn-detectives/detect-ownership-conflicts evidence))))))

(def ^:private mac-output
  (str "MAC               Type   Flags Intf/Remote ES/VTEP            VLAN  Seq #'s\n"
       guest " local        tap0                                 0/0\n"))

(defn- collect [vni-output]
  (evpn/collect-host {:host "a"} 1
                     (fn [_ argv]
                       (case (last argv)
                         "show evpn vni 1" vni-output
                         "show evpn mac vni 1" mac-output
                         ""))
                     (constantly true)))

(deftest a-vni-output-without-a-vtep-marks-the-host
  (let [result (collect " VNI: 1\n Type: L2\n")]
    (is (nil? (:error result)) "the host answered, so its tables are kept")
    (is (nil? (:host-vtep result)))
    (is (re-find #"no local VTEP address" (:vtep-error result)))
    (is (= 1 (count (:macs result))))
    (testing "evpn-hosts names the host as unjudged"
      (let [[finding & more] (evpn-detectives/detect-host-problems {"evpn-hosts" [result]})]
        (is (nil? more))
        (is (= :warning (:severity finding)))
        (is (= "a/vni 1" (:component finding)))
        (is (re-find #"own VTEP address could not be read" (:summary finding)))
        (is (re-find #"evpn-mac-ownership" (:hint finding)))))))

(deftest a-vtep-in-either-family-is-read
  (is (= "10.0.0.1" (evpn/parse-local-vtep " Local VTEP IP: 10.0.0.1\n")))
  (is (= "fd00:21::1" (evpn/parse-local-vtep " Local VTEP IP: fd00:21::1\n")))
  (is (= "2001:db8:0:1:0:0:0:7" (evpn/parse-local-vtep " Local VTEP IP: 2001:db8:0:1:0:0:0:7\n")))
  (is (nil? (evpn/parse-local-vtep " VTEP MAC: 52:54:00:aa:bb:cc\n")) "a MAC is no address")
  (is (nil? (evpn/parse-local-vtep " VNI: 1\n")))
  (let [result (collect " Local VTEP IP: fd00:21::1\n")]
    (is (= "fd00:21::1" (:host-vtep result)))
    (is (nil? (:vtep-error result)))
    (is (empty? (evpn-detectives/detect-host-problems {"evpn-hosts" [result]})))))

;; --- events: a lifetime count is no hourly rate --------------------------------------

(def ^:private now (java.time.Instant/parse "2026-11-26T12:00:00Z"))

(defn- warning [reason name & {:as fields}]
  (merge {:type "Warning" :reason reason :message (str reason " happened")
          :involvedObject {:kind "Pod" :namespace "app" :name name}}
         fields))

(deftest a-long-running-event-is-counted-since-it-began
  (let [[finding] (events/detect-warning-events
                   {:now now "events" {:items [(warning "BackOff" "web" :count 4000
                                                        :firstTimestamp "2026-11-20T12:00:00Z"
                                                        :lastTimestamp "2026-11-26T11:59:00Z")]}})]
    (is (= "BackOff seen 4000x since 2026-11-20T12:00:00Z, most recently in the last hour" (:summary finding)))
    (is (= "2026-11-20T12:00:00Z" (:since finding)))
    (is (= "2026-11-26T11:59:00Z" (:last-seen finding)))))

(deftest an-event-begun-within-the-hour-is-an-hourly-count
  (let [[finding] (events/detect-warning-events
                   {:now now "events" {:items [(warning "BackOff" "web" :count 12
                                                        :firstTimestamp "2026-11-26T11:20:00Z"
                                                        :lastTimestamp "2026-11-26T11:59:00Z")]}})]
    (is (= "BackOff seen 12x in the last hour" (:summary finding)))))

(deftest an-events-k8s-io-series-starts-at-its-event-time
  (let [series (warning "FailedMount" "db" :eventTime "2026-11-25T08:00:00Z"
                        :series {:count 300 :lastObservedTime "2026-11-26T11:50:00Z"})
        fresh (warning "FailedMount" "cache" :eventTime "2026-11-26T11:10:00Z"
                       :series {:count 3 :lastObservedTime "2026-11-26T11:50:00Z"})]
    (is (= ["FailedMount seen 300x since 2026-11-25T08:00:00Z, most recently in the last hour"]
           (map :summary (events/detect-warning-events {:now now "events" {:items [series]}}))))
    (is (= ["FailedMount seen 3x in the last hour"]
           (map :summary (events/detect-warning-events {:now now "events" {:items [fresh]}}))))))

(deftest a-repeated-event-without-a-first-time-says-so
  (is (= ["BackOff seen 9x, most recently in the last hour (first occurrence not recorded)"]
         (map :summary (events/detect-warning-events
                        {:now now "events" {:items [(warning "BackOff" "web" :count 9
                                                             :lastTimestamp "2026-11-26T11:59:00Z")]}})))))

(deftest events-come-newest-first
  (let [items (for [i (range 12)]
                (warning (str "Reason" i) (str "pod-" i)
                         :lastTimestamp (format "2026-11-26T11:%02d:00Z" (inc (* 4 i)))))
        findings (events/detect-warning-events {:now now "events" {:items (shuffle items)}})]
    (is (= (map #(str "app/pod-" %) (reverse (range 12))) (map :component findings)))
    (testing "the report keeps that order past eight entries"
      (is (= (map :summary findings)
             (map :summary (detective/roll-up (map #(assoc % :detective "events") findings))))))))

(deftest roll-up-orders-by-severity-then-first-appearance
  (let [findings (concat (for [i (range 10)] {:severity :warning :summary (str "w" i) :component "c"})
                         [{:severity :critical :summary "boom" :component "c"}
                          {:severity :warning :summary "w3" :component "d"}])]
    (is (= (cons "boom" (map #(str "w" %) (range 10)))
           (map :summary (detective/roll-up findings))))
    (is (= 2 (:count (nth (detective/roll-up findings) 4))))))

;; --- namespaces: a deletion in flight is not stuck -----------------------------------

(defn- terminating [deleted & conditions]
  {:metadata {:name "gone" :deletionTimestamp deleted}
   :status {:phase "Terminating"
            :conditions (for [[type message] conditions] {:type type :status "True" :message message})}})

(defn- stuck [now & namespaces]
  (vec (namespaces/detect-stuck-namespaces {:now now "namespaces" {:items namespaces}})))

(deftest a-fresh-deletion-with-finalizers-remaining-is-not-stuck
  (is (empty? (stuck now (terminating "2026-11-26T11:59:58Z"
                                      ["NamespaceFinalizersRemaining" "kubernetes.io/pvc-protection in 1 resource instances"]
                                      ["NamespaceContentRemaining" "pods has 1 resource instances"])))))

(deftest finalizers-remaining-past-the-grace-is-stuck
  (let [[finding & more] (stuck now (terminating "2026-11-26T11:40:00Z"
                                                 ["NamespaceFinalizersRemaining" "kubernetes.io/pvc-protection in 1 resource instances"]
                                                 ["NamespaceContentRemaining" "pods has 1 resource instances"]))]
    (is (nil? more) "content remaining folds into the finalizer that explains it")
    (is (= :warning (:severity finding)))
    (is (= "namespace cannot finish terminating: a finalizer has not returned" (:summary finding)))
    (is (= "2026-11-26T11:40:00Z" (:since finding)))))

(deftest content-remaining-alone-past-the-grace-is-stuck
  (is (= ["namespace cannot finish terminating: content has not been deleted"]
         (map :summary (stuck now (terminating "2026-11-26T11:00:00Z" ["NamespaceContentRemaining" "pods has 1 resource instances"]))))))

(deftest a-refused-deletion-is-reported-at-once
  (is (= [:critical]
         (map :severity (stuck now (terminating "2026-11-26T11:59:58Z" ["NamespaceDeletionDiscoveryFailure" "metrics.k8s.io/v1beta1: the server is currently unable"]))))))

(deftest without-an-age-finalizers-remaining-is-still-reported
  (is (= ["namespace cannot finish terminating: a finalizer has not returned"]
         (map :summary (stuck nil (terminating nil ["NamespaceFinalizersRemaining" "x"] ["NamespaceContentRemaining" "y"]))))))

;; --- template drift: a duplicated env name resolves to its last value -----------------

(deftest a-pod-matching-a-template-with-a-duplicate-env-name-has-no-drift
  (let [spec {:containers [{:name "c" :image "x:1" :env [{:name "X" :value "a"} {:name "X" :value "b"}]}]}]
    (is (empty? (drift/pod-diffs spec spec)))))

(deftest a-duplicate-env-name-compares-the-value-the-process-sees
  (let [template {:containers [{:name "c" :image "x:1" :env [{:name "X" :value "a"} {:name "X" :value "b"}]}]}]
    (is (empty? (drift/pod-diffs template {:containers [{:name "c" :image "x:1" :env [{:name "X" :value "b"}]}]})))
    (is (= ["c: env X changed"]
           (drift/pod-diffs template {:containers [{:name "c" :image "x:1" :env [{:name "X" :value "b"} {:name "X" :value "a"}]}]})))))

;; --- local volumes: the hostname label is what the affinity names ---------------------

(defn- pinned [hostname]
  {:metadata {:name "pv1"}
   :spec {:nodeAffinity {:required {:nodeSelectorTerms [{:matchExpressions [{:key "kubernetes.io/hostname" :operator "In" :values [hostname]}]}]}}}})

(deftest a-local-volume-is-matched-against-the-hostname-label
  (let [nodes {:items [{:metadata {:name "node-a.example.com" :labels {(keyword "kubernetes.io/hostname") "node-a"}}}]}]
    (is (empty? (csi/detect-orphaned-local-volumes {"nodes" nodes "persistentvolumes" {:items [(pinned "node-a")]}})))
    (is (= ["pv1"] (map :component (csi/detect-orphaned-local-volumes
                                    {"nodes" nodes "persistentvolumes" {:items [(pinned "node-b")]}}))))))

(deftest a-node-without-the-label-is-known-by-its-name
  (is (empty? (csi/detect-orphaned-local-volumes {"nodes" {:items [{:metadata {:name "node-a"}}]}
                                                  "persistentvolumes" {:items [(pinned "node-a")]}}))))

;; --- capacity: pod-level requests are charged -----------------------------------------

(def ^:private two-nodes
  {:items [{:status {:allocatable {:cpu "4" :memory "8Gi"}}} {:status {:allocatable {:cpu "4" :memory "8Gi"}}}]})

(deftest pod-level-requests-count-towards-overcommit
  (let [findings (capacity/detect-overcommit
                  {"nodes" two-nodes
                   "pods" {:items [{:spec {:resources {:requests {:cpu "6" :memory "12Gi"}} :containers [{:name "a"}]}
                                    :status {:phase "Running"}}]}})]
    (is (= #{:warning} (set (map :severity findings))))
    (is (= 2 (count findings)) "cpu and memory both exceed the headroom")))

(deftest pod-level-requests-replace-the-container-aggregate
  (is (empty? (capacity/detect-overcommit
               {"nodes" two-nodes
                "pods" {:items [{:spec {:resources {:requests {:cpu "1" :memory "1Gi"}}
                                        :containers [{:name "a" :resources {:requests {:cpu "6" :memory "12Gi"}}}]}
                                 :status {:phase "Running"}}]}}))
      "the pod-level value is what the scheduler charges"))

;; --- network: Calico's namespaced v3 policies ------------------------------------------

(def ^:private calico-allow
  {:metadata {:namespace "a" :name "allow-web"} :spec {:selector "app == 'x'" :ingress [{:action "Allow"}]}})

(defn- isolated [calico]
  (vec (network/detect-isolated-services
        {"services" {:items [{:metadata {:namespace "a" :name "s"} :spec {:selector {:app "x"}}}]}
         "pods" {:items [{:metadata {:namespace "a" :name "p" :labels {:app "x"}}
                          :status {:phase "Running" :conditions [{:type "Ready" :status "True"}]}}]}
         "networkpolicies" {:items [{:metadata {:namespace "a" :name "deny"} :spec {:podSelector {} :policyTypes ["Ingress"]}}]}
         network/global-policy-type {:items []}
         network/calico-policy-type calico})))

(deftest a-calico-allow-in-the-namespace-lowers-the-claim
  (let [[finding & more] (isolated {:items [calico-allow]})]
    (is (nil? more))
    (is (= :info (:severity finding)))
    (is (re-find #"Calico NetworkPolicy allow-web" (:hint finding)))
    (is (not (re-find #"no policy allows any traffic in" (:hint finding))))))

(deftest a-calico-policy-elsewhere-changes-nothing
  (is (= [:warning] (map :severity (isolated {:items [(assoc-in calico-allow [:metadata :namespace] "b")]}))))
  (is (= [:warning] (map :severity (isolated {:items [(update calico-allow :spec dissoc :ingress)]})))))

(deftest an-unread-calico-read-judges-nothing
  (is (empty? (isolated nil)))
  (let [check (first (filter #(= "isolated-services" (:name %)) network/detectives))]
    (is (detective/unread? check {"services" {:items []} "pods" {:items []} "networkpolicies" {:items []}
                                  network/global-policy-type {:items []}})))
  (let [check (first (filter #(= "network-policies" (:name %)) network/detectives))]
    (is (detective/unread? check {"pods" {:items []} "networkpolicies" {:items []}
                                  network/global-policy-type {:items []}}))))

(deftest a-calico-policy-protects-its-namespace
  (is (= ["open"]
         (map :component (network/detect-unprotected-namespaces
                          {"networkpolicies" {:items []}
                           "pods" {:items [{:metadata {:namespace "a" :name "p"}} {:metadata {:namespace "open" :name "q"}}]}
                           network/global-policy-type {:items []}
                           network/calico-policy-type {:items [calico-allow]}})))))

(deftest calico-reads-use-the-v3-group-only
  (doseq [type network/calico-policy-types]
    (is (str/ends-with? type ".projectcalico.org"))
    (is (not (str/includes? type "crd.projectcalico.org")))))

(defn- answering [result]
  (fn [& argv] (assoc result :argv argv)))

(def ^:private group-not-found
  {:exit 1 :out "" :err "Error from server (NotFound): the server could not find the requested resource"})

(defn- raw-read? [argv] (some #{"--raw"} argv))

(deftest an-optional-type-answers-empty-where-it-is-not-served
  (with-redefs [shell/execute (fn [& argv]
                                (if (raw-read? argv)
                                  group-not-found
                                  {:exit 1 :out "" :err "error: the server doesn't have a resource type \"networkpolicies\""}))]
    (is (= {:items []} (kubectl/get-all-optional "ctx" network/calico-policy-type))))
  (with-redefs [shell/execute (answering {:exit 1 :out "" :err "Unable to connect to the server: dial tcp: i/o timeout"})]
    (is (nil? (kubectl/get-all-optional "ctx" network/calico-policy-type))))
  (with-redefs [shell/execute (answering {:exit 0 :out "{\"items\":[{\"metadata\":{\"name\":\"p\"}}]}" :err ""})]
    (is (= {:items [{:metadata {:name "p"}}]} (kubectl/get-all-optional "ctx" network/calico-policy-type)))))

(deftest the-calico-types-read-through-the-optional-collector
  (let [seen (atom [])]
    (with-redefs [shell/execute (fn [& argv] (swap! seen conj (vec argv))
                                  (if (raw-read? argv)
                                    group-not-found
                                    {:exit 1 :out "" :err "error: the server doesn't have a resource type"}))]
      (doseq [type network/calico-policy-types]
        (is (= {:items []} ((registry/collector-for type) "ctx" type)) type)))
    (is (every? #(some #{"get"} %) @seen))
    (is (= (set network/calico-policy-types)
           (set (keep #(some (set network/calico-policy-types) %) (remove raw-read? @seen))))
        "one list read per Calico type")
    (is (= 2 (count (remove raw-read? @seen))))))

;; --- csi attachments: attaching takes time, detaching can fail -----------------------

(defn- attachment [& {:keys [attached created deleted attach-error detach-error]}]
  {:metadata (cond-> {:name "csi-1" :creationTimestamp created}
               deleted (assoc :deletionTimestamp deleted))
   :spec {:nodeName "n1" :source {:persistentVolumeName "pv-1"}}
   :status (cond-> {:attached attached}
             attach-error (assoc :attachError {:message attach-error :time "2026-11-26T11:58:00Z"})
             detach-error (assoc :detachError {:message detach-error :time "2026-11-26T11:58:00Z"}))})

(defn- attachments [& items]
  (vec (csi/detect-attachment-problems {:now now "volumeattachments" {:items items}})))

(deftest an-attachment-still-attaching-is-not-reported
  (is (empty? (attachments (attachment :attached false :created "2026-11-26T11:59:30Z")))))

(deftest an-attachment-attaching-past-the-grace-warns
  (let [[finding] (attachments (attachment :attached false :created "2026-11-26T11:30:00Z"))]
    (is (= :warning (:severity finding)))
    (is (= "volume is not attached to node n1" (:summary finding)))))

(deftest an-attach-error-is-critical-at-once
  (is (= [:critical] (map :severity (attachments (attachment :attached false :created "2026-11-26T11:59:30Z" :attach-error "rpc error"))))))

(deftest a-failed-detach-is-reported
  (let [[finding] (attachments (attachment :attached true :created "2026-11-20T00:00:00Z" :deleted "2026-11-26T11:59:00Z"
                                           :detach-error "rpc error: volume is busy"))]
    (is (= :critical (:severity finding)))
    (is (= "volume cannot be detached from node n1" (:summary finding)))
    (is (re-find #"volume is busy" (:hint finding)))
    (is (re-find #"Multi-Attach" (:hint finding)))))

(deftest a-detach-that-does-not-finish-warns-past-the-grace
  (is (empty? (attachments (attachment :attached true :created "2026-11-20T00:00:00Z" :deleted "2026-11-26T11:59:00Z"))))
  (is (= ["volume is still attached to node n1 after its attachment was deleted"]
         (map :summary (attachments (attachment :attached true :created "2026-11-20T00:00:00Z" :deleted "2026-11-26T11:00:00Z"))))))

(deftest a-healthy-attachment-is-silent
  (is (empty? (attachments (attachment :attached true :created "2026-11-20T00:00:00Z")))))
