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
