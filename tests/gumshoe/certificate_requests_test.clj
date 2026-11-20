;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.certificate-requests-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.csr :as csr]
            [gumshoe.detectives.registry :as registry]))

(def ^:private now (java.time.Instant/parse "2026-11-20T12:00:00Z"))

(defn- request [name signer & {:keys [user created conditions certificate]
                               :or {user "system:node:worker-1" created "2026-11-20T11:00:00Z"}}]
  {:metadata {:name name :creationTimestamp created}
   :spec {:signerName signer :username user}
   :status (cond-> {:conditions (vec conditions)}
             certificate (assoc :certificate certificate))})

(defn- approved [at] {:type "Approved" :status "True" :reason "AutoApproved" :lastUpdateTime at})

(defn- detect [& requests]
  (vec (csr/detect-stuck-requests {"certificatesigningrequests" {:items requests} :now now})))

(deftest states-as-the-controllers-read-them
  (is (= :pending (csr/state (request "a" csr/kubelet-serving))))
  (is (= :approved (csr/state (request "a" csr/kubelet-serving :conditions [(approved "x")]))))
  (is (= :approved (csr/state (request "a" csr/kubelet-serving :conditions [{:type "Approved"}]))) "an empty status holds")
  (is (= :pending (csr/state (request "a" csr/kubelet-serving :conditions [{:type "Approved" :status "False"}]))))
  (is (= :issued (csr/state (request "a" csr/kubelet-serving :conditions [(approved "x")] :certificate "LS0t"))))
  (is (= :denied (csr/state (request "a" csr/kubelet-serving :conditions [(approved "x") {:type "Denied" :status "True"}]))))
  (is (= :failed (csr/state (request "a" csr/kubelet-serving :conditions [(approved "x") {:type "Failed" :status "True"}])))))

(deftest settled-and-recent-requests-are-quiet
  (is (= [] (detect (request "issued" csr/kubelet-serving :conditions [(approved "2026-11-20T11:00:00Z")] :certificate "LS0t")
                    (request "denied" csr/kubelet-serving :conditions [{:type "Denied" :status "True"}])
                    (request "fresh" csr/kubelet-serving :created "2026-11-20T11:50:00Z")
                    (request "signing" csr/kubelet-client :conditions [(approved "2026-11-20T11:58:00Z")])))))

(deftest a-pending-kubelet-serving-request-names-its-node
  (let [[finding] (detect (request "csr-abc" csr/kubelet-serving))]
    (is (= :warning (:severity finding)))
    (is (= "csr-abc" (:component finding)))
    (is (= "kubelet serving certificate waits for approval" (:summary finding)))
    (is (str/starts-with? (:hint finding) "requested by worker-1; nothing in Kubernetes approves these"))
    (is (str/includes? (:hint finding) "no serving certificate available for the kubelet"))
    (is (= "2026-11-20T11:00:00Z" (:since finding)))))

(deftest a-pending-kubelet-client-request
  (let [[finding] (detect (request "node-csr-x" csr/kubelet-client :user "system:bootstrap:abcdef"))]
    (is (= "kubelet client certificate waits for approval" (:summary finding)))
    (is (str/starts-with? (:hint finding) "requested by system:bootstrap:abcdef; kube-controller-manager approves these"))))

(deftest a-pending-request-for-another-signer
  (is (= "signer example.com/mesh, requested by alice - nothing approved or denied it; whoever asked has no certificate"
         (:hint (first (detect (request "mesh" "example.com/mesh" :user "alice")))))))

(deftest approved-but-unsigned
  (testing "a built-in signer points at kube-controller-manager"
    (let [[finding] (detect (request "c" csr/kubelet-client :conditions [(approved "2026-11-20T11:30:00Z")]))]
      (is (= "CertificateSigningRequest is approved but not signed" (:summary finding)))
      (is (re-find #"csrsigning controller" (:hint finding)))
      (is (= "2026-11-20T11:30:00Z" (:since finding)))))
  (testing "another signer points at its own component"
    (is (re-find #"no controller signs for this signer"
                 (:hint (first (detect (request "m" "example.com/mesh" :conditions [(approved "2026-11-20T11:30:00Z")])))))))
  (testing "lastTransitionTime when lastUpdateTime is missing"
    (is (= 1 (count (detect (request "t" csr/kubelet-client
                                     :conditions [{:type "Approved" :status "True" :lastTransitionTime "2026-11-20T11:00:00Z"}])))))))

(deftest a-failed-request-carries-the-signers-reason
  (let [[finding] (detect (request "bad" csr/kubelet-serving
                                   :conditions [(approved "2026-11-20T11:00:00Z")
                                                {:type "Failed" :status "True" :reason "SignerValidationFailure"
                                                 :message "invalid usage for client certificate: server auth"
                                                 :lastUpdateTime "2026-11-20T11:00:05Z"}]))]
    (is (= "CertificateSigningRequest failed (SignerValidationFailure)" (:summary finding)))
    (is (= "invalid usage for client certificate: server auth - signer kubernetes.io/kubelet-serving, requested by system:node:worker-1"
           (:hint finding)))))

(deftest joins-the-platform-scope
  (is (some #(= "certificate-requests" (:name %)) (registry/for-scope :platform))))
