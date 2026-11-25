;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.certificate-renew-test
  "certificates/renew verifies a renewal by a revision above the one read
   before it, never by the Ready condition the old certificate still holds."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.runbook :as runbook]))

(binding [runbook/*loading* true]
  (load-file "runbooks/kubernetes/certificates/renew.clj"))

(defn- book-fn [sym] @(requiring-resolve (symbol "runbooks.kubernetes.certificates.renew" (name sym))))

(defn- certificate
  [& {:keys [revision ready issuing failed-at message] :or {ready "True"}}]
  {:metadata {:namespace "shop" :name "web-tls"}
   :status (cond-> {:conditions (cond-> [{:type "Ready" :status ready}]
                                  issuing (conj {:type "Issuing" :status issuing :message message}))}
             revision (assoc :revision revision)
             failed-at (assoc :lastFailureTime failed-at))})

(deftest issuance-test
  (let [issuance (book-fn 'issuance)]
    (is (= {:revision 3 :last-failure nil} (issuance (certificate :revision 3))))
    (is (= {:revision 0 :last-failure "t"} (issuance (certificate :failed-at "t")))
        "a Certificate never issued is at revision 0")
    (is (nil? (issuance nil)))))

(deftest renewed-test
  (let [renewed? (book-fn 'renewed?)
        before {:revision 1}]
    (testing "Ready on the old certificate is no renewal"
      (is (false? (renewed? before (certificate :revision 1)))))
    (testing "a raised revision on a Ready certificate is"
      (is (true? (renewed? before (certificate :revision 2)))))
    (testing "the first issuance of a Certificate that had none"
      (is (true? (renewed? nil (certificate :revision 1)))))
    (testing "still issuing, or not Ready, is not yet renewed"
      (is (false? (renewed? before (certificate :revision 2 :issuing "True"))))
      (is (false? (renewed? before (certificate :revision 2 :ready "False")))))
    (testing "a Certificate that is gone is not renewed"
      (is (false? (renewed? before nil))))))

(deftest renewal-failure-test
  (let [renewal-failure (book-fn 'renewal-failure)]
    (testing "a failure stamped before the renewal is not this renewal's"
      (is (nil? (renewal-failure {:revision 1 :last-failure "t0"} (certificate :revision 1 :failed-at "t0")))))
    (testing "a new failure stamp ends the wait, naming cert-manager's message"
      (let [reason (renewal-failure {:revision 1 :last-failure nil}
                                    (certificate :revision 1 :failed-at "t1" :issuing "False"
                                                 :message "rate limited\nretrying in 1h"))]
        (is (str/includes? reason "t1"))
        (is (str/ends-with? reason ": rate limited"))))
    (testing "no failure"
      (is (nil? (renewal-failure {:revision 1} (certificate :revision 2))))
      (is (nil? (renewal-failure {:revision 1} nil))))))

(deftest renewed-checks-compare-with-the-issuance-read-before
  (let [renewed-checks (book-fn 'renewed-checks)
        [check] (with-redefs [gumshoe.kubectl/get-namespaced-resource (constantly (certificate :revision 4))]
                  (doall (renewed-checks "ctx" ["shop/web-tls"] {"shop/web-tls" {:revision 4}})))]
    (is (str/includes? (:description check) "revision above 4"))
    (with-redefs [gumshoe.kubectl/get-namespaced-resource (constantly (certificate :revision 4))]
      (is (false? ((:check check))) "the revision read before the renewal does not verify it")
      (is (nil? ((:abort check)))))
    (with-redefs [gumshoe.kubectl/get-namespaced-resource (constantly (certificate :revision 5))]
      (is (true? ((:check check)))))))
