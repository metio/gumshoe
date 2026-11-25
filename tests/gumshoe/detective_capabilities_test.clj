;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detective-capabilities-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.plugin :as plugin]
            [gumshoe.tools.argocd]
            [gumshoe.tools.crossplane]
            [gumshoe.tools.flux]))

;; --- tagging from the manifest ---------------------------------------------------

(deftest the-manifest-tags-its-detectives
  (is (= {:gitops [{:name "a" :capabilities [:argocd]} {:name "b" :capabilities [:argocd]}]}
         (plugin/tag-capabilities {:detectives {:gitops [{:name "a"} {:name "b"}]}
                                   :capabilities {:argocd (constantly true)}}))))

(deftest several-capabilities-are-sorted
  (is (= [:cnpg :prometheus-operator]
         (-> (plugin/tag-capabilities {:detectives {:x [{:name "a"}]}
                                       :capabilities {:prometheus-operator identity :cnpg identity}})
             :x first :capabilities))))

(deftest a-detective-naming-its-own-capabilities-keeps-them
  (is (= [:flux] (-> (plugin/tag-capabilities {:detectives {:x [{:name "a" :capabilities [:flux]}]}
                                               :capabilities {:argocd identity}})
                     :x first :capabilities)))
  (is (= [] (-> (plugin/tag-capabilities {:detectives {:x [{:name "a" :capabilities []}]}
                                          :capabilities {:argocd identity}})
                :x first :capabilities))
      "an explicit empty list says the detective inspects no capability"))

(deftest a-manifest-without-capabilities-tags-nothing
  (is (= {:x [{:name "a"}]} (plugin/tag-capabilities {:detectives {:x [{:name "a"}]}})))
  (is (= {} (plugin/tag-capabilities {:capabilities {:argocd identity}}))))

(defn- named [scope prefix]
  (filter #(str/starts-with? (:name %) prefix) (registry/for-scope scope)))

(deftest loaded-tool-packages-carry-their-capability
  (testing "every detective a package registers carries the capability it detects"
    (is (seq (named :gitops "argocd-")))
    (is (every? #(= [:argocd] (:capabilities %)) (named :gitops "argocd-")))
    (is (every? #(= [:crossplane] (:capabilities %)) (named :platform "crossplane-"))))
  (testing "the core's own detectives carry none"
    (is (not-any? :capabilities (filter #(#{"nodes" "pods"} (:name %)) (registry/all))))))

(deftest a-book-listing-detectives-directly-gets-their-tags
  (is (= [{:name "argocd-applications" :capabilities [:argocd]}
          {:name "nodes"}
          {:name "argocd-applicationsets" :capabilities [:flux]}
          {:name "not-registered"}]
         (mapv #(select-keys % [:name :capabilities])
               (registry/with-capabilities [{:name "argocd-applications"}
                                            {:name "nodes"}
                                            {:name "argocd-applicationsets" :capabilities [:flux]}
                                            {:name "not-registered"}])))))

;; --- unread evidence ---------------------------------------------------------------

(def ^:private evidence
  {"applications.argoproj.io" nil
   "applicationsets.argoproj.io" nil
   "pods" {:items []}
   "nodes" {:items [{:metadata {:name "n1"}}]}})

(deftest a-detective-reads-nothing-only-when-every-key-is-nil
  (is (detective/unread? {:requires ["applications.argoproj.io"]} evidence))
  (is (detective/unread? {:requires ["applications.argoproj.io" "applicationsets.argoproj.io"]} evidence))
  (is (not (detective/unread? {:requires ["applications.argoproj.io" "pods"]} evidence)) "one key read")
  (is (not (detective/unread? {:requires ["pods"]} evidence)) "an empty list is a read")
  (is (detective/unread? {:requires ["never-collected"]} evidence) "a key missing from the evidence")
  (is (not (detective/unread? {:requires []} evidence)) "a detective that requires nothing probes on its own")
  (is (not (detective/unread? {} evidence))))

(deftest marking-keeps-the-detectives-in-order
  (is (= [{:name "a" :requires ["pods"]}
          {:name "b" :requires ["applications.argoproj.io"] :unread true}]
         (detective/mark-unread [{:name "a" :requires ["pods"]} {:name "b" :requires ["applications.argoproj.io"]}] evidence))))

(deftest the-unread-line-groups-by-capability
  (is (nil? (detective/unread-line [{:name "a"}])))
  (is (= (str "3 checks read nothing on this cluster - argocd (argocd-applications, argocd-sync), "
              "other (gateway-routes); what they read is not served, not readable, or not gathered by this scan")
         (detective/unread-line [{:name "gateway-routes" :unread true}
                                 {:name "argocd-applications" :capabilities [:argocd] :unread true}
                                 {:name "pods"}
                                 {:name "argocd-sync" :capabilities [:argocd] :unread true}])))
  (is (str/starts-with? (detective/unread-line [{:name "x" :capabilities [:cnpg :prometheus-operator] :unread true}])
                        "1 check read nothing on this cluster - cnpg/prometheus-operator (x)")))

;; --- the report ------------------------------------------------------------------

(defn- text-report [detectives findings]
  (with-out-str (binding [*err* (java.io.StringWriter.)]
                  (detective/report! detectives findings "text"))))

(def ^:private scanned
  [{:name "nodes" :description "Nodes are Ready"}
   {:name "pods" :description "Pods run"}
   {:name "argocd-applications" :description "Applications are healthy" :capabilities [:argocd] :unread true}])

(deftest a-clean-run-does-not-count-unread-checks-as-passed
  (let [out (text-report scanned [])]
    (is (str/includes? out "Nodes are Ready"))
    (is (not (str/includes? out "Applications are healthy")) "no tick for a check that read nothing")
    (is (str/includes? out "1 check read nothing on this cluster - argocd (argocd-applications)"))))

(deftest a-run-with-findings-separates-clean-from-unread
  (let [out (text-report scanned [{:severity :warning :detective "pods" :component "web/a" :summary "restarts"}])]
    (is (str/includes? out "1 other check came back clean: nodes"))
    (is (str/includes? out "1 check read nothing on this cluster - argocd (argocd-applications)"))))

(deftest a-run-where-everything-read-says-nothing-extra
  (is (not (str/includes? (text-report (take 2 scanned) []) "read nothing"))))

(deftest machine-output-carries-the-capability-and-the-mark
  (let [out (with-out-str (binding [*err* (java.io.StringWriter.)]
                            (detective/report! scanned [] "json")))
        checked (:checked (json/parse-string out true))]
    (is (= {:name "argocd-applications" :description "Applications are healthy" :capabilities ["argocd"] :unread true}
           (last checked)))
    (is (= {:name "nodes" :description "Nodes are Ready"} (first checked)) "no keys added where nothing applies")))

;; --- subjects a detective judges ------------------------------------------------

(def ^:private scoped-evidence
  {:hosts []
   :instances nil
   :load-balancers {"lb-1" "192.0.2.1"}
   :zones {:items []}
   :routes ["a.example"]})

(deftest a-detective-judging-an-empty-set-read-nothing
  (is (detective/unread? {:judges [:hosts]} scoped-evidence) "an empty vector of subjects")
  (is (detective/unread? {:judges [:instances]} scoped-evidence) "a nil subject key")
  (is (detective/unread? {:judges [:never-collected]} scoped-evidence) "a subject key never collected")
  (is (detective/unread? {:judges [:zones]} scoped-evidence) "a list object with no items")
  (is (detective/unread? {:judges [:hosts :instances]} scoped-evidence) "every subject key empty"))

(deftest a-detective-with-one-subject-set-filled-read-something
  (is (not (detective/unread? {:judges [:routes]} scoped-evidence)))
  (is (not (detective/unread? {:judges [:hosts :routes]} scoped-evidence)) "one of two subject keys filled")
  (is (not (detective/unread? {:judges [:load-balancers]} scoped-evidence)) "a filled map")
  (is (not (detective/unread? {:judges []} scoped-evidence)) "declaring no subjects judges nothing extra"))

(deftest supporting-evidence-does-not-stand-in-for-subjects
  (is (detective/unread? {:requires [:load-balancers] :judges [:hosts]} scoped-evidence)
      "the load balancers were read, but there was no host to check against them"))

;; --- reads a detective joins ------------------------------------------------------

(def ^:private joined-evidence
  {"poolers" {:items [{:metadata {:name "p"}}]}
   "services" {:items []}
   "endpointslices" nil})

(deftest a-join-missing-one-side-read-nothing
  (is (detective/unread? {:requires ["poolers" "services" "endpointslices"]
                          :joins ["poolers" "services" "endpointslices"]}
                         joined-evidence)
      "the slices did not answer, so no Service can be judged against them")
  (is (not (detective/unread? {:requires ["poolers" "services" "endpointslices"]} joined-evidence))
      "without :joins one answered key is enough, as before")
  (is (detective/unread? {:joins ["poolers" "never-collected"]} joined-evidence) "a key never collected"))

(deftest a-join-whose-every-side-answered-read-something
  (is (not (detective/unread? {:joins ["poolers" "services"]} joined-evidence))
      "an empty list is an answer")
  (is (not (detective/unread? {:joins []} joined-evidence)) "declaring no join judges nothing extra"))

(deftest joins-and-judges-apply-together
  (is (detective/unread? {:joins ["services"] :judges ["services"]} joined-evidence)
      "the join answered, but it held no subject")
  (is (not (detective/unread? {:joins ["poolers" "services"] :judges ["poolers"]} joined-evidence))))

(deftest marking-applies-the-subject-rule
  (is (= [{:name "records" :judges [:hosts] :unread true}
          {:name "routes" :judges [:routes]}]
         (detective/mark-unread [{:name "records" :judges [:hosts]} {:name "routes" :judges [:routes]}]
                                scoped-evidence))))

;; --- a run that judged nothing ---------------------------------------------------

(def ^:private blind
  [{:name "records" :description "Records match the cell" :unread true}
   {:name "ttl" :description "TTLs are low enough" :unread true}])

(defn- report-streams [detectives findings output]
  (let [err (java.io.StringWriter.)
        result (atom nil)
        out (with-out-str (binding [*err* err]
                            (reset! result (detective/report! detectives findings output))))]
    {:out out :err (str err) :result @result}))

(deftest a-run-where-every-check-read-nothing-is-not-a-pass
  (doseq [output ["text" "json" "edn"]]
    (testing output
      (is (false? (:result (report-streams blind [] output)))))))

(deftest a-run-with-no-detectives-is-not-a-pass
  (is (false? (:result (report-streams [] [] "text"))))
  (is (false? (:result (report-streams [] [] "json")))))

(deftest the-text-report-says-nothing-was-checked-instead-of-checks-passed
  (let [{:keys [out err]} (report-streams blind [] "text")
        both (str out err)]
    (is (str/includes? both "every check read nothing, so nothing was checked - records, ttl"))
    (is (not (str/includes? both "no issues found")))
    (is (not (str/includes? both "passed")))))

(deftest machine-output-marks-a-run-that-checked-nothing
  (is (true? (:nothing-checked (json/parse-string (:out (report-streams blind [] "json")) true))))
  (is (true? (:nothing-checked (read-string (:out (report-streams blind [] "edn"))))))
  (is (not (contains? (json/parse-string (:out (report-streams scanned [] "json")) true) :nothing-checked))
      "a run where something read carries no mark"))

(deftest one-check-that-read-keeps-the-run-a-pass
  (is (true? (:result (report-streams (conj blind {:name "nodes" :description "Nodes are Ready"}) [] "text")))))

(deftest findings-from-unread-checks-still-report-and-judge-health
  (let [critical [{:severity :critical :detective "records" :component "a.example" :summary "wrong address"}]
        warning [{:severity :warning :detective "records" :component "a.example" :summary "high TTL"}]]
    (is (false? (:result (report-streams blind critical "json"))))
    (is (true? (:result (report-streams blind warning "json"))) "a finding is evidence something was read")
    (is (not (contains? (json/parse-string (:out (report-streams blind warning "json")) true) :nothing-checked)))))
