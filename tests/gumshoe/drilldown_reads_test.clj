;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.drilldown-reads-test
  "What the drill-down reads and how it names what it could not read: owner
   kinds it has no type for, hostnames as DNS compares them, the Warning
   events of one object, and lookups of related objects whose read was
   refused."
  (:require [cheshire.core :as cheshire]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.investigation :as investigation]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.subject :as subject]))

(def ^:private forbidden
  {:exit 1 :out ""
   :err "Error from server (Forbidden): pods is forbidden: User \"viewer\" cannot list resource \"pods\" in API group \"\" at the cluster scope"})

(defn- answered [doc] {:exit 0 :out (cheshire/generate-string doc) :err ""})

(def ^:private empty-list (answered {:items []}))

(defn- captured
  "{:result .. :out <stdout and stderr, colours stripped>} of f."
  [f]
  (let [out (java.io.StringWriter.)
        result (binding [*out* out *err* out] (f))]
    {:result result :out (stdout/strip-colors (str out))}))

(defn- get-type
  "The resource type of a kubectl get argv, or nil for another command."
  [argv]
  (second (drop-while #(not= "get" %) argv)))

;; --- owner kinds without a type ----------------------------------------------------

(deftest cronjob-is-a-built-in-kind
  (is (= "cronjobs" (subject/kind->type "CronJob")))
  (is (subject/readable? "CronJob")))

(deftest an-owner-kind-without-a-type-is-not-readable
  (is (not (subject/readable? "AcmeDatabaseCluster")))
  (is (not (subject/readable? nil))))

(deftest an-unreadable-owner-is-offered-as-unsupported-never-as-a-pivot
  (let [focus {:subject (subject/subject "StatefulSet" "db" "pg")
               :situation :ok
               :edges [{:relation "owned by" :subject (subject/subject "AcmeDatabaseCluster" "db" "pg")}
                       {:relation "owned by" :subject (subject/subject "CronJob" "db" "nightly")}]}
        items (investigation/menu-items focus [])
        by-kind (into {} (keep (fn [item] (when-let [s (:subject item)] [(:kind s) item]))) items)]
    (is (= :unsupported (:type (get by-kind "AcmeDatabaseCluster"))))
    (is (str/includes? (:label (get by-kind "AcmeDatabaseCluster")) "unsupported kind AcmeDatabaseCluster"))
    (is (= :pivot (:type (get by-kind "CronJob"))) "a CronJob owner is a pivot like any built-in kind")))

(deftest focusing-an-unreadable-kind-asks-the-cluster-nothing
  (let [commands (atom [])]
    (with-redefs [shell/execute (fn [& argv] (swap! commands conj (vec argv)) forbidden)]
      (is (= {:subject (subject/subject "AcmeDatabaseCluster" "db" "pg") :unsupported? true}
             (investigation/focus! "ctx" (subject/subject "AcmeDatabaseCluster" "db" "pg")))))
    (is (empty? @commands))))

(def ^:private job
  {:kind "Job"
   :metadata {:namespace "batch" :name "nightly-123"
              :ownerReferences [{:kind "AcmeScheduler" :name "nightly" :controller true}]}
   :spec {:selector {:matchLabels {:job-name "nightly-123"}}}
   :status {:succeeded 1}})

(defn- job-cluster
  "kubectl answering the Job and empty lists, recording every argv."
  [commands]
  (fn [& argv]
    (swap! commands conj (vec argv))
    (if (and (= "jobs" (get-type argv)) (some #{"nightly-123"} argv))
      (answered job)
      empty-list)))

(deftest choosing-an-unsupported-owner-names-it-and-keeps-the-run-green
  (let [commands (atom [])
        picks (atom ["· owned by: AcmeScheduler batch/nightly  · unsupported kind AcmeScheduler"
                     "✓ done - end the investigation"])
        {:keys [result out]}
        (with-redefs [shell/execute (job-cluster commands)
                      shell/run-with-output (fn [& _] 0)
                      investigation/save-trail! (fn [& _] nil)
                      fuzzy/select-single (fn [_ labels]
                                            (let [pick (first @picks)]
                                              (swap! picks rest)
                                              (is (some #{pick} (map stdout/strip-colors labels)) pick)
                                              pick))]
          (captured #(investigation/investigate! "ctx" (subject/subject "Job" "batch" "nightly-123"))))]
    (is (true? result) "nothing went unread: the unsupported owner was never asked for")
    (is (str/includes? out "AcmeScheduler batch/nightly is of an unsupported kind AcmeScheduler"))
    (is (not (str/includes? out "could not read AcmeScheduler")))
    (is (not-any? (fn [argv] (some #(or (nil? %) (= "" %)) argv)) @commands)
        "kubectl never sees an empty or missing type")))

(deftest a-trail-resumed-on-an-unsupported-kind-is-named-not-failed
  (let [commands (atom [])
        {:keys [result out]}
        (with-redefs [shell/execute (fn [& argv] (swap! commands conj (vec argv)) forbidden)
                      investigation/save-trail! (fn [& _] nil)]
          (captured #(investigation/investigate! "ctx" (subject/subject "AcmeScheduler" "batch" "nightly"))))]
    (is (true? result))
    (is (str/includes? out "unsupported kind AcmeScheduler"))
    (is (not (str/includes? out "could not read")))
    (is (empty? @commands))))

;; --- hostnames ---------------------------------------------------------------------

(deftest an-ingress-wildcard-covers-exactly-one-label
  (is (investigation/ingress-host-matches? "*.example.org" "a.example.org"))
  (is (not (investigation/ingress-host-matches? "*.example.org" "a.b.example.org")))
  (is (not (investigation/ingress-host-matches? "*.example.org" "example.org")))
  (is (not (investigation/ingress-host-matches? "*.example.org" ".example.org"))))

(deftest an-httproute-wildcard-covers-any-number-of-labels
  (is (investigation/host-matches? "*.example.org" "a.example.org"))
  (is (investigation/host-matches? "*.example.org" "a.b.example.org"))
  (is (not (investigation/host-matches? "*.example.org" "example.org"))))

(deftest hostnames-compare-case-insensitively-and-without-the-root-dot
  (doseq [match? [investigation/host-matches? investigation/ingress-host-matches?]]
    (is (match? "moodle.example.org" "Moodle.Example.ORG"))
    (is (match? "moodle.example.org" "moodle.example.org."))
    (is (match? "moodle.example.org" "  moodle.example.org "))
    (is (match? "*.example.org" "WWW.Example.org"))))

(deftest ingresses-and-routes-for-a-host-use-their-own-wildcard-rules
  (let [ingresses [{:metadata {:namespace "a" :name "wild"} :spec {:rules [{:host "*.example.org"}]}}]
        routes [{:metadata {:namespace "a" :name "wild"} :spec {:hostnames ["*.example.org"]}}]]
    (is (empty? (investigation/ingresses-for-host ingresses "a.b.example.org")))
    (is (= ["wild"] (map #(-> % :metadata :name) (investigation/ingresses-for-host ingresses "B.example.org"))))
    (is (= ["wild"] (map #(-> % :metadata :name) (investigation/routes-for-host routes "a.b.Example.org"))))))

(deftest the-host-flag-is-normalised
  (is (= "moodle.example.org" (investigation/normalize-host " Moodle.Example.ORG. "))))

;; --- recent warnings ---------------------------------------------------------------

(defn- field-selector [argv]
  (some #(when (str/starts-with? % "--field-selector=") %) argv))

(deftest warnings-of-a-namespaced-object-are-selected-by-kind-and-name
  (let [argv (investigation/warning-events-args "ctx" (subject/subject "Deployment" "shop" "web"))]
    (is (some #{"--namespace=shop"} argv))
    (is (= "--field-selector=involvedObject.kind=Deployment,involvedObject.name=web,type=Warning"
           (field-selector argv)))))

(deftest warnings-of-a-node-are-searched-everywhere-by-kind
  (let [argv (investigation/warning-events-args "ctx" (subject/subject "Node" nil "worker-1"))]
    (is (some #{"--all-namespaces"} argv))
    (is (not-any? #(str/starts-with? % "--namespace=") argv))
    (is (= "--field-selector=involvedObject.kind=Node,involvedObject.name=worker-1,type=Warning"
           (field-selector argv)))))

;; --- related objects whose read was refused -----------------------------------------

(def ^:private pod
  {:metadata {:namespace "shop" :name "web-1" :labels {:app "web"}}
   :spec {:nodeName "worker-1"}
   :status {:phase "Running"}})

(def ^:private cases
  [{:case "a pod's Services" :subject (subject/subject "Pod" "shop" "web-1") :object pod
    :unread ["the Services that select this pod"]}
   {:case "a node's pods" :subject (subject/subject "Node" nil "worker-1") :object {:metadata {:name "worker-1"}}
    :unread ["the pods on this node"]}
   {:case "a workload's pods" :subject (subject/subject "Deployment" "shop" "web")
    :object {:metadata {:namespace "shop" :name "web"} :spec {:selector {:matchLabels {:app "web"}}}}
    :unread ["the pods of this workload"]}
   {:case "a Service's routes, ingresses and backend pods" :subject (subject/subject "Service" "shop" "web")
    :object {:metadata {:namespace "shop" :name "web"} :spec {:selector {:app "web"}}}
    :unread ["the HTTPRoutes that expose this Service" "the Ingresses that expose this Service"
             "the backend pods of this Service"]}])

(deftest a-refused-lookup-is-unread-never-nothing-related
  (doseq [{:keys [case subject object unread]} cases]
    (testing case
      (with-redefs [shell/execute (fn [& _] forbidden)]
        (let [result (investigation/fetched-edges "ctx" subject object)]
          (is (empty? (:edges result)))
          (is (= unread (mapv :what (:unread result))))
          (is (every? #(str/includes? (:why %) "Forbidden") (:unread result))))))))

(deftest an-answered-empty-lookup-is-nothing-related
  (doseq [{:keys [case subject object]} cases]
    (testing case
      (with-redefs [shell/execute (fn [& _] empty-list)]
        (is (= {:edges [] :unread []} (investigation/fetched-edges "ctx" subject object)))))))

(deftest an-unserved-route-type-answers-with-nothing
  (with-redefs [shell/execute (fn [& argv]
                                (if (some #(str/starts-with? (str %) "httproutes") argv)
                                  {:exit 1 :out "" :err "error: the server doesn't have a resource type \"httproutes\""}
                                  empty-list))]
    (is (= {:edges [] :unread []}
           (investigation/fetched-edges "ctx" (subject/subject "Service" "shop" "web")
                                        {:metadata {:namespace "shop" :name "web"} :spec {:selector {:app "web"}}})))))

(deftest a-refused-plugin-lookup-is-named
  (investigation/register-fetched-edges!
   "DrilldownReadsKind"
   (fn [context _ _]
     (for [item (:items (kubectl/get-all context "widgets.example.org"))]
       {:relation "widget" :subject (subject/subject "Pod" "a" (-> item :metadata :name))})))
  (with-redefs [shell/execute (fn [& _] forbidden)]
    (let [result (investigation/plugin-fetched-edges-result
                  "ctx" (subject/subject "DrilldownReadsKind" "a" "x") {})]
      (is (= [] (:edges result)))
      (is (= ["what relates to this DrilldownReadsKind"] (mapv :what (:unread result)))))
    (let [{:keys [result out]} (captured #(investigation/plugin-fetched-edges
                                           "ctx" (subject/subject "DrilldownReadsKind" "a" "x") {}))]
      (is (= [] result))
      (is (str/includes? out "could not read what relates to this DrilldownReadsKind")))))

(deftest a-drill-down-with-a-refused-lookup-ends-red
  (let [{:keys [result out]}
        (with-redefs [shell/execute (fn [& argv]
                                      (cond
                                        (and (= "nodes" (get-type argv)) (some #{"worker-1"} argv))
                                        (answered {:metadata {:name "worker-1"}
                                                   :status {:conditions [{:type "Ready" :status "True"}]}})
                                        (= "pods" (get-type argv)) forbidden
                                        :else empty-list))
                      investigation/save-trail! (fn [& _] nil)
                      fuzzy/select-single (fn [_ _] "✓ done - end the investigation")]
          (captured #(investigation/investigate! "ctx" (subject/subject "Node" nil "worker-1"))))]
    (is (false? result))
    (is (str/includes? out "could not read the pods on this node"))))

;; --- a user whose RBAC is one namespace ----------------------------------------------

(def ^:private shop-pod
  {:metadata {:namespace "shop" :name "web-1" :labels {:app "web"}}
   :status {:phase "Running"}})

(def ^:private shop-ingress
  {:metadata {:namespace "shop" :name "web"}
   :spec {:rules [{:http {:paths [{:backend {:service {:name "web"}}}]}}]}})

(defn- namespace-scoped-cluster
  "kubectl as a user bound to the shop namespace sees it: any read across
   every namespace is refused, a read in shop answers with its pods and
   ingresses. Records every argv."
  [commands]
  (fn [& argv]
    (swap! commands conj (vec argv))
    (cond
      (some #{"--all-namespaces"} argv) forbidden
      (not (some #{"--namespace=shop"} argv)) forbidden
      (= "pods" (get-type argv)) (answered {:items [shop-pod]})
      (= "ingresses" (get-type argv)) (answered {:items [shop-ingress]})
      :else empty-list)))

(deftest a-workloads-pods-are-read-in-its-namespace
  (let [commands (atom [])
        result (with-redefs [shell/execute (namespace-scoped-cluster commands)]
                 (investigation/fetched-edges "ctx" (subject/subject "StatefulSet" "shop" "web")
                                              {:metadata {:namespace "shop" :name "web"}
                                               :spec {:selector {:matchLabels {:app "web"}}}}))]
    (is (= [] (:unread result)))
    (is (= [{:relation "pod" :subject (subject/subject "Pod" "shop" "web-1") :situation :ok}]
           (:edges result)))
    (is (some #(some #{"--selector=app=web"} %) @commands))))

(deftest a-services-pods-and-ingresses-are-read-in-its-namespace
  (with-redefs [shell/execute (namespace-scoped-cluster (atom []))]
    (let [result (investigation/fetched-edges "ctx" (subject/subject "Service" "shop" "web")
                                              {:metadata {:namespace "shop" :name "web"}
                                               :spec {:selector {:app "web"}}})]
      (is (= #{{:relation "exposed by" :subject (subject/subject "Ingress" "shop" "web")}
               {:relation "backend pod" :subject (subject/subject "Pod" "shop" "web-1") :situation :ok}}
             (set (:edges result)))
          "a refused HTTPRoute list costs only the routes")
      (is (= ["the HTTPRoutes that expose this Service"] (mapv :what (:unread result)))
          "HTTPRoutes may reach across namespaces, so they are still read everywhere"))))

(deftest a-refused-ingress-list-keeps-the-route-edges
  (let [route {:metadata {:namespace "edge" :name "web"}
               :spec {:rules [{:backendRefs [{:name "web" :namespace "shop"}]}]}}]
    (with-redefs [shell/execute (fn [& argv]
                                  (cond
                                    (= "ingresses" (get-type argv)) forbidden
                                    (str/starts-with? (str (get-type argv)) "httproutes") (answered {:items [route]})
                                    :else empty-list))]
      (let [result (investigation/fetched-edges "ctx" (subject/subject "Service" "shop" "web")
                                                {:metadata {:namespace "shop" :name "web"} :spec {}})]
        (is (= [{:relation "exposed by" :subject (subject/subject "HTTPRoute" "edge" "web")}] (:edges result)))
        (is (= ["the Ingresses that expose this Service"] (mapv :what (:unread result))))))))

(deftest a-workload-drill-down-of-a-namespace-scoped-user-ends-green
  (let [{:keys [result out]}
        (with-redefs [shell/execute (let [inner (namespace-scoped-cluster (atom []))]
                                      (fn [& argv]
                                        (if (and (= "statefulsets" (get-type argv)) (some #{"web"} argv))
                                          (answered {:kind "StatefulSet"
                                                     :metadata {:namespace "shop" :name "web" :generation 1}
                                                     :spec {:replicas 1 :selector {:matchLabels {:app "web"}}}
                                                     :status {:observedGeneration 1 :replicas 1 :readyReplicas 1
                                                              :updatedReplicas 1 :currentReplicas 1}})
                                          (apply inner argv))))
                      investigation/save-trail! (fn [& _] nil)
                      fuzzy/select-single (fn [_ _] "✓ done - end the investigation")]
          (captured #(investigation/investigate! "ctx" (subject/subject "StatefulSet" "shop" "web"))))]
    (is (true? result) out)
    (is (not (str/includes? out "could not read")) out)))
