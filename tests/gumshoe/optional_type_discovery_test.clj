;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.optional-type-discovery-test
  "kubectl reports a type it has no mapping for as \"doesn't have a resource
   type\" both when the API server does not serve it and when discovery of
   its group failed and client-go carried on with partial discovery (an
   aggregated API server that is down). get-all-optional answers an empty
   list only once discovery confirms the type is not served, and nil
   otherwise, so a failed discovery stays unread."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.network :as network]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]))

(def ^:private group-doc
  "{\"kind\":\"APIGroup\",\"name\":\"projectcalico.org\",\"versions\":[{\"groupVersion\":\"projectcalico.org/v3\",\"version\":\"v3\"}]}")

(defn- resource-list
  [& plurals]
  (str "{\"kind\":\"APIResourceList\",\"groupVersion\":\"projectcalico.org/v3\",\"resources\":["
       (str/join "," (map #(format "{\"name\":\"%s\",\"namespaced\":false}" %) plurals))
       "]}"))

(def ^:private ok (fn [out] {:exit 0 :out out :err ""}))
(def ^:private not-found {:exit 1 :out "" :err "Error from server (NotFound): the server could not find the requested resource"})
(def ^:private unavailable {:exit 1 :out "" :err "Error from server (ServiceUnavailable): the server is currently unable to handle the request"})
(def ^:private no-mapping {:exit 1 :out "" :err "error: the server doesn't have a resource type \"globalnetworkpolicies\""})

;; --- the pure judgement -------------------------------------------------------------

(deftest a-group-the-server-does-not-know-is-not-served
  (is (true? (kubectl/unserved-in-discovery? "globalnetworkpolicies" not-found []))))

(deftest a-group-read-that-failed-otherwise-confirms-nothing
  (is (false? (kubectl/unserved-in-discovery? "globalnetworkpolicies" unavailable [])))
  (is (false? (kubectl/unserved-in-discovery? "globalnetworkpolicies"
                                               {:exit 1 :out "" :err "Unable to connect to the server: dial tcp: i/o timeout"}
                                               []))))

(deftest a-listed-group-is-judged-by-its-versions
  (testing "every version answered without the type"
    (is (true? (kubectl/unserved-in-discovery? "globalnetworkpolicies" (ok group-doc)
                                                [(ok (resource-list "tiers"))]))))
  (testing "a version lists the type"
    (is (false? (kubectl/unserved-in-discovery? "globalnetworkpolicies" (ok group-doc)
                                                 [(ok (resource-list "tiers" "globalnetworkpolicies"))]))))
  (testing "a version did not answer - the aggregated API server is down"
    (is (false? (kubectl/unserved-in-discovery? "globalnetworkpolicies" (ok group-doc) [unavailable]))))
  (testing "a version answered with output that does not parse"
    (is (false? (kubectl/unserved-in-discovery? "globalnetworkpolicies" (ok group-doc) [(ok "{trunc")]))))
  (testing "no version to judge"
    (is (false? (kubectl/unserved-in-discovery? "globalnetworkpolicies" (ok group-doc) [])))))

(deftest the-core-group-is-judged-by-its-resource-list
  (is (true? (kubectl/unserved-in-discovery? "widgets" nil [(ok (resource-list "pods"))])))
  (is (false? (kubectl/unserved-in-discovery? "pods" nil [(ok (resource-list "pods"))])))
  (is (false? (kubectl/unserved-in-discovery? "widgets" nil [unavailable]))))

;; --- get-all-optional against stubbed reads -----------------------------------------

(defn- by-path
  "A shell/execute stub: the list read answers list, each raw read the
   answer its path maps to, and records every argv."
  [seen list answers]
  (fn [& argv]
    (swap! seen conj (vec argv))
    (if-let [path (second (drop-while #(not= "--raw" %) argv))]
      (get answers path {:exit 1 :out "" :err (str "unexpected path " path)})
      list)))

(defn- optional-read
  [list answers]
  (let [seen (atom [])]
    {:value (with-redefs [shell/execute (by-path seen list answers)]
              (kubectl/get-all-optional "ctx" network/global-policy-type))
     :seen @seen}))

(deftest a-type-discovery-confirms-absent-answers-empty
  (is (= {:items []} (:value (optional-read no-mapping {"/apis/projectcalico.org" not-found}))))
  (is (= {:items []} (:value (optional-read no-mapping {"/apis/projectcalico.org" (ok group-doc)
                                                         "/apis/projectcalico.org/v3" (ok (resource-list "tiers"))})))))

(deftest a-failed-discovery-of-the-group-is-unread
  (is (nil? (:value (optional-read no-mapping {"/apis/projectcalico.org" (ok group-doc)
                                               "/apis/projectcalico.org/v3" unavailable}))))
  (is (nil? (:value (optional-read no-mapping {"/apis/projectcalico.org" unavailable})))))

(deftest a-type-discovery-lists-is-unread-when-the-list-read-says-otherwise
  (is (nil? (:value (optional-read no-mapping {"/apis/projectcalico.org" (ok group-doc)
                                               "/apis/projectcalico.org/v3" (ok (resource-list "globalnetworkpolicies"))})))))

(deftest a-not-found-list-read-is-confirmed-too
  (is (= {:items []} (:value (optional-read not-found {"/apis/projectcalico.org" not-found}))))
  (is (nil? (:value (optional-read not-found {"/apis/projectcalico.org" (ok group-doc)
                                              "/apis/projectcalico.org/v3" unavailable})))))

(deftest a-list-that-answered-asks-discovery-nothing
  (let [{:keys [value seen]} (optional-read (ok "{\"items\":[{\"metadata\":{\"name\":\"p\"}}]}") {})]
    (is (= {:items [{:metadata {:name "p"}}]} value))
    (is (not-any? #(some #{"--raw"} %) seen))))

(deftest the-discovery-reads-name-the-context
  (let [{:keys [seen]} (optional-read no-mapping {"/apis/projectcalico.org" not-found})]
    (is (every? #(some #{"--context=ctx"} %) seen))))

;; --- a fake kubectl printing what kubectl prints on a failed discovery --------------

(def ^:private klog-unhandled
  "E1126 12:00:00.000000    4242 memcache.go:265] \"Unhandled Error\" err=\"couldn't get resource list for projectcalico.org/v3: the server is currently unable to handle the request\"")

(def ^:private klog-structured
  "E1126 12:00:00.000000    4242 memcache.go:133] \"Couldn't get resource list\" groupVersion=\"projectcalico.org/v3\" err=\"the server is currently unable to handle the request\"")

(defn- fake-kubectl!
  "Writes an executable that answers like kubectl: the list read fails with
   the discovery lines followed by the missing-mapping error, and each raw
   read answers as raw maps its path."
  [dir discovery-lines raw]
  (let [script (fs/path dir "kubectl")
        quote-sh #(str "'" (str/replace % "'" "'\\''") "'")
        cases (str/join "\n" (for [[path {:keys [exit out err]}] raw]
                               (format "  %s) printf '%%s' %s; printf '%%s\\n' %s >&2; exit %d ;;"
                                       (quote-sh path) (quote-sh out) (quote-sh err) exit)))]
    (spit (str script)
          (str "#!/bin/sh\n"
               "raw=''\nprev=''\n"
               "for a in \"$@\"; do [ \"$prev\" = --raw ] && raw=\"$a\"; prev=\"$a\"; done\n"
               "if [ -n \"$raw\" ]; then\n case \"$raw\" in\n" cases "\n"
               "  *) echo \"unexpected $raw\" >&2; exit 1 ;;\n esac\nfi\n"
               (str/join (for [line discovery-lines] (format "printf '%%s\\n' %s >&2\n" (quote-sh line))))
               "echo 'error: the server doesn'\\''t have a resource type \"globalnetworkpolicies\"' >&2\n"
               "exit 1\n"))
    (fs/set-posix-file-permissions script "rwx------")
    (str script)))

(defn- through-fake
  [discovery-lines raw]
  (let [dir (fs/create-temp-dir {:prefix "gumshoe-fake-kubectl"})]
    (try
      (let [kubectl (fake-kubectl! dir discovery-lines raw)
            execute shell/execute]
        (with-redefs [shell/execute (fn [command & args]
                                      (apply execute (if (= "kubectl" command) kubectl command) args))]
          (kubectl/get-all-optional "ctx" network/global-policy-type)))
      (finally (fs/delete-tree dir)))))

(def ^:private calico-down
  {"/apis/projectcalico.org" (ok group-doc)
   "/apis/projectcalico.org/v3" unavailable})

(deftest a-failed-calico-discovery-is-unread-in-either-log-shape
  (doseq [[shape line] [["klog Unhandled Error" klog-unhandled] ["structured" klog-structured]]]
    (testing shape
      (is (nil? (through-fake [line] calico-down))))))

(deftest a-cluster-without-calico-answers-empty-through-the-fake
  (is (= {:items []} (through-fake [] {"/apis/projectcalico.org" not-found}))))
