;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mail-unread-test
  "The mail evidence holds nil for what was not probed or did not answer, so
   the checks over it are unread instead of clean, and a run that probed
   nothing fails instead of ending \"nothing critical\"."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.mail :as detectives]
            [gumshoe.dns :as dns]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mail :as mail]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]))

(defn- collect
  "The evidence collect-evidence! gathers for opts against a DNS server
   answering every query with `answer` (a fn of type and name) and ports that
   refuse every probe. Returns [evidence probed-hosts]."
  [opts answer]
  (let [probed (atom [])]
    [(with-redefs [dns/query (fn [_ type name] (answer type name))
                   dns/ptr-records (fn [& _] nil)
                   shell/execute-with-stdin (fn [_ & args]
                                              (swap! probed conj (vec args))
                                              {:exit 1 :out "" :err "connect: Connection refused"})]
       (let [evidence (atom nil)]
         (binding [*err* (java.io.StringWriter.)]
           (with-out-str (reset! evidence (mail/collect-evidence! opts))))
         @evidence))
     @probed]))

(defn- unread-names
  [evidence]
  (set (map :name (filter :unread (detective/mark-unread detectives/detectives evidence)))))

(def ^:private silent (fn [& _] nil))

(deftest without-a-host-the-services-and-rdns-checks-are-unread-test
  (let [[evidence probed] (collect {:domain "example.org" :dkim-selectors ["default"]}
                                   (fn [_ _] []))]
    (is (nil? (get evidence "probes")))
    (is (nil? (get evidence "ptr")))
    (is (empty? probed) "no port was probed on a host nobody named")
    (is (contains? (unread-names evidence) "mail-services"))
    (is (contains? (unread-names evidence) "mail-rdns"))
    (testing "the domain checks still read"
      (is (not-any? (unread-names evidence) ["mail-mx" "mail-spf" "mail-dmarc"])))))

(deftest a-blank-host-is-no-host-test
  (let [[evidence probed] (collect {:domain "example.org" :host "  " :dkim-selectors []} (fn [_ _] []))]
    (is (nil? (get evidence "probes")))
    (is (empty? probed))))

(deftest with-a-host-every-port-is-probed-and-read-test
  (let [[evidence probed] (collect {:domain "example.org" :host "mail.example.org"
                                    :dkim-selectors ["default"]}
                                   (fn [_ _] []))]
    (is (= (count mail/services) (count (get evidence "probes"))))
    (is (seq probed))
    (is (not (contains? (unread-names evidence) "mail-services")))))

(deftest no-answering-selector-leaves-dkim-unread-test
  (testing "every selector unanswered"
    (let [[evidence] (collect {:domain "example.org" :dkim-selectors ["default" "mail"]}
                              (fn [_ name] (when-not (re-find #"_domainkey" name) [])))]
      (is (nil? (get evidence "dkim")))
      (is (contains? (unread-names evidence) "mail-dkim"))))
  (testing "no selector to ask"
    (let [[evidence] (collect {:domain "example.org" :dkim-selectors []} (fn [_ _] []))]
      (is (nil? (get evidence "dkim")))
      (is (contains? (unread-names evidence) "mail-dkim"))))
  (testing "one selector answered: the check reads, and names the unanswered one"
    (let [[evidence] (collect {:domain "example.org" :dkim-selectors ["default" "mail"]}
                              (fn [_ name] (when (= name "default._domainkey.example.org")
                                             ["\"v=DKIM1; k=rsa; p=AAAA\""])))]
      (is (= {"default" ["\"v=DKIM1; k=rsa; p=AAAA\""] "mail" nil} (get evidence "dkim")))
      (is (not (contains? (unread-names evidence) "mail-dkim")))
      (is (= ["mail._domainkey.example.org"]
             (map :component (detectives/detect-dkim-problems evidence)))))))

(deftest dkim-evidence-test
  (is (nil? (mail/dkim-evidence {})))
  (is (nil? (mail/dkim-evidence {"default" nil "mail" nil})))
  (is (= {"default" [] "mail" nil} (mail/dkim-evidence {"default" [] "mail" nil}))))

(deftest without-a-domain-no-record-is-queried-test
  (let [queried (atom [])
        [evidence] (collect {:dkim-selectors ["default"]}
                            (fn [type name] (swap! queried conj [type name]) []))]
    (is (empty? @queried))
    (is (= (set (map :name detectives/detectives)) (unread-names evidence)))))

(defn- run-book
  "Runs the mail book's action with opts against an unanswering DNS server and
   refused ports, the report in EDN. Returns [result report]."
  [opts]
  (let [{:keys [action]} (with-redefs [kubectl/current-cluster (constantly nil)
                                       config/env-value (fn [& _] nil)]
                           (binding [runbook/*loading* true]
                             (load-file "runbooks/detectives/mail.clj")))
        result (atom nil)
        out (with-redefs [detective/interactive? (constantly false)
                          kubectl/current-cluster (constantly nil)
                          kubectl/current-context (constantly "ctx")
                          config/env-value (fn [& _] nil)
                          dns/query silent
                          dns/ptr-records (fn [& _] nil)
                          shell/execute-with-stdin (fn [& _] {:exit 1 :out "" :err "refused"})]
              (binding [*err* (java.io.StringWriter.)]
                (with-out-str (reset! result (action (merge {:dkim-selectors ["default" "mail"] :output "edn"} opts)
                                                     nil)))))]
    [@result (edn/read-string out)]))

(deftest a-run-that-probed-nothing-fails-test
  (testing "a domain whose DNS server never answers, and no host"
    (let [[result report] (run-book {:domain "example.org"})]
      (is (false? result) "the run must not end nothing critical")
      (is (true? (:nothing-checked report)))
      (is (every? :unread (:checked report)) (pr-str (remove :unread (:checked report))))))
  (testing "neither domain nor host"
    (let [[result report] (run-book {})]
      (is (false? result))
      (is (true? (:nothing-checked report))))))
