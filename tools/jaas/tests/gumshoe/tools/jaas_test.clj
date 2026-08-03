;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.jaas-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.subject :as subject]
            [gumshoe.tools.jaas :as jaas]))

(defn- summaries [findings] (set (map :summary findings)))

(def ^:private snippets
  {jaas/jsonnetsnippet-type
   {:items [{:metadata {:namespace "apps" :name "dashboards"}
             :status {:conditions [{:type "Ready" :status "False" :reason "EvaluationFailed"
                                    :message "main.jsonnet:3 field does not exist"}]}}
            {:metadata {:namespace "apps" :name "paused"}
             :spec {:suspend true}
             :status {:conditions [{:type "Ready" :status "False" :reason "Suspended"}]}}
            {:metadata {:namespace "apps" :name "typo"}
             :status {:conditions [{:type "Ready" :status "False" :reason "InvalidSpec"}]}}
            {:metadata {:namespace "apps" :name "healthy"}
             :status {:conditions [{:type "Ready" :status "True" :reason "Synced"}]}}]}})

(deftest snippet-severity-by-reason-test
  (testing "Ready=False maps its reason to a severity; evaluation failures are critical, authoring errors warn, deliberate pauses inform"
    (let [findings (jaas/detect-snippet-problems snippets)
          by-summary (into {} (map (juxt :summary :severity) findings))]
      (is (= #{"JsonnetSnippet is not Ready (EvaluationFailed)"
               "JsonnetSnippet is not Ready (Suspended)"
               "JsonnetSnippet is not Ready (InvalidSpec)"}
             (summaries findings)) "a synced snippet produces nothing")
      (is (= :critical (by-summary "JsonnetSnippet is not Ready (EvaluationFailed)")))
      (is (= :info (by-summary "JsonnetSnippet is not Ready (Suspended)")))
      (is (= :warning (by-summary "JsonnetSnippet is not Ready (InvalidSpec)"))))))

(deftest unknown-reason-still-reported-test
  (testing "a reason this package has not seen yet still surfaces, as a warning"
    (let [findings (jaas/detect-snippet-problems
                    {jaas/jsonnetsnippet-type
                     {:items [{:metadata {:namespace "apps" :name "future"}
                               :status {:conditions [{:type "Ready" :status "False"
                                                      :reason "SomethingNewEntirely"}]}}]}})]
      (is (= [:warning] (map :severity findings))))))

(deftest finding-names-its-subject-test
  (testing "a finding points at the snippet, so a scan drills straight into it"
    (is (= (subject/subject "JsonnetSnippet" "apps" "dashboards")
           (:subject (first (jaas/detect-snippet-problems snippets)))))))

(deftest missing-library-detected-test
  (testing "a snippet importing a library that is not in the cluster is reported, named, and aliased"
    (let [evidence {jaas/jsonnetsnippet-type
                    {:items [{:metadata {:namespace "apps" :name "dashboards"}
                              :spec {:libraries [{:kind "JsonnetLibrary" :name "grafonnet"
                                                  :importPath "g"}
                                                 {:kind "JsonnetLibrary" :name "present"}]}}]}
                    jaas/jsonnetlibrary-type
                    {:items [{:metadata {:namespace "apps" :name "present"}}]}}
          findings (jaas/detect-missing-libraries evidence)]
      (is (= #{"JsonnetLibrary apps/grafonnet is missing"} (summaries findings)))
      (is (= [:warning] (map :severity findings)))
      (is (re-find #"imported as 'g'" (:hint (first findings)))))))

(deftest library-ref-resolves-in-its-own-namespace-test
  (testing "an explicit namespace on the ref is where the library is looked for"
    (let [evidence {jaas/jsonnetsnippet-type
                    {:items [{:metadata {:namespace "apps" :name "dashboards"}
                              :spec {:libraries [{:kind "JsonnetLibrary" :name "shared"
                                                  :namespace "platform"}]}}]}
                    ;; same name, wrong namespace - it must not satisfy the ref
                    jaas/jsonnetlibrary-type
                    {:items [{:metadata {:namespace "apps" :name "shared"}}]}}]
      (is (= #{"JsonnetLibrary platform/shared is missing"}
             (summaries (jaas/detect-missing-libraries evidence)))))))

(deftest snippet-edges-walk-the-render-chain-test
  (testing "a snippet reaches its libraries, its source, and the artifact it publishes"
    (let [edges (jaas/snippet-edges
                 {:metadata {:namespace "apps" :name "dashboards"}
                  :spec {:sourceRef {:kind "GitRepository" :name "config"}
                         :libraries [{:kind "JsonnetLibrary" :name "grafonnet"}]}})]
      (is (= [{:relation "imports"
               :subject (subject/subject "JsonnetLibrary" "apps" "grafonnet")}
              {:relation "renders"
               :subject (subject/subject "GitRepository" "apps" "config")}
              {:relation "publishes"
               :subject (subject/subject "ExternalArtifact" "apps" "dashboards")}]
             edges)))))

(deftest inline-snippet-still-publishes-test
  (testing "a snippet with inline files has no source edge but still names its artifact"
    (is (= [{:relation "publishes"
             :subject (subject/subject "ExternalArtifact" "apps" "inline")}]
           (jaas/snippet-edges {:metadata {:namespace "apps" :name "inline"}
                                :spec {:files {"main.jsonnet" "{}"}}})))))

(deftest snippet-facts-test
  (let [facts (into {} (jaas/snippet-facts
                        {:metadata {:namespace "apps" :name "dashboards"}
                         :spec {:entryFile "main.jsonnet"
                                :sourceRef {:kind "GitRepository" :name "config"}}
                         :status {:revision "sha256:abc"}}))]
    (is (= "GitRepository/config" (get facts "source")))
    (is (= "main.jsonnet" (get facts "entry file")))
    ;; nil pairs are dropped by the facts panel, so an unpublished snippet shows
    ;; no artifact line at all.
    (is (nil? (get facts "artifact")))))

(deftest package-registers-gitops-scope-and-capability-test
  (is (some #(= "jsonnetsnippets" (:name %)) (registry/for-scope :gitops))
      "the package joins the gitops scan rather than inventing a scope")
  (is (contains? (set (capabilities/registered)) :jaas)))

(deftest snippet-is-a-drill-down-subject-test
  (is (= jaas/jsonnetsnippet-type (subject/kind->type "JsonnetSnippet")))
  (is (= jaas/jsonnetlibrary-type (subject/kind->type "JsonnetLibrary"))))
