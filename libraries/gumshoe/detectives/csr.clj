;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.csr
  "CertificateSigningRequests (certificates.k8s.io/v1) that do not end in a
   certificate. A request waits for an approver, then for the signer its
   signerName names; a request that stalls in either step leaves whoever asked
   without a certificate.

   kube-controller-manager signs the built-in kubernetes.io signers. It
   auto-approves kubelet client certificates for requesters RBAC allows, and
   never approves kubelet serving certificates or other client certificates,
   so those wait for a person or another approver. It deletes a pending
   request after 24 hours and a decided one after an hour, so what this sees
   is recent."
  (:require [clojure.string :as str]
            [gumshoe.onset :as onset]))

(def kubelet-serving "kubernetes.io/kubelet-serving")
(def kubelet-client "kubernetes.io/kube-apiserver-client-kubelet")

(def built-in-signers
  #{kubelet-serving kubelet-client "kubernetes.io/kube-apiserver-client" "kubernetes.io/legacy-unknown"})

(def pending-grace (java.time.Duration/ofMinutes 15))
(def signing-grace (java.time.Duration/ofMinutes 5))

(defn- instant
  [value]
  (try (java.time.Instant/parse (str value)) (catch Exception _ nil)))

(defn- condition
  "A condition of the type that holds: status True, or no status, as the
   controllers read it."
  [csr type]
  (first (filter #(and (= type (:type %)) (contains? #{nil "" "True"} (:status %)))
                 (-> csr :status :conditions))))

(defn state
  "Where a request stands: :issued, :denied, :failed, :approved (awaiting the
   signer) or :pending (awaiting approval)."
  [csr]
  (cond
    (not (str/blank? (-> csr :status :certificate))) :issued
    (condition csr "Denied") :denied
    (condition csr "Failed") :failed
    (condition csr "Approved") :approved
    :else :pending))

(defn- node-of
  [csr]
  (second (re-matches #"system:node:(.+)" (str (-> csr :spec :username)))))

(defn- older-than?
  [at now grace]
  (and at now (.isAfter ^java.time.Instant now (.plus ^java.time.Instant at grace))))

(defn- pending-finding
  [csr now]
  (let [created (instant (-> csr :metadata :creationTimestamp))
        signer (-> csr :spec :signerName)
        node (node-of csr)
        [summary hint]
        (condp = signer
          kubelet-serving
          ["kubelet serving certificate waits for approval"
           (str "requested by " (or node (-> csr :spec :username))
                "; nothing in Kubernetes approves these - a kubelet without a serving certificate yet refuses TLS"
                " (\"no serving certificate available for the kubelet\"), so kubectl logs, exec and port-forward through"
                " that node fail, while a renewal keeps serving until its current certificate expires; check it was this"
                " node that asked, then kubectl certificate approve, or run an approver")]
          kubelet-client
          ["kubelet client certificate waits for approval"
           (str "requested by " (-> csr :spec :username)
                "; kube-controller-manager approves these when RBAC lets the requester create node client"
                " certificates - a node cannot join, or cannot renew its credentials, until it is approved")]
          ["CertificateSigningRequest waits for approval"
           (str "signer " signer ", requested by " (-> csr :spec :username)
                " - nothing approved or denied it; whoever asked has no certificate")])]
    (when (older-than? created now pending-grace)
      (onset/since {:severity :warning :component (-> csr :metadata :name) :summary summary :hint hint}
                   (str created)))))

(defn- unsigned-finding
  [csr now]
  (let [approved (condition csr "Approved")
        at (instant (or (:lastUpdateTime approved) (:lastTransitionTime approved)))
        signer (-> csr :spec :signerName)]
    (when (older-than? at now signing-grace)
      (onset/since
       {:severity :warning
        :component (-> csr :metadata :name)
        :summary "CertificateSigningRequest is approved but not signed"
        :hint (str "signer " signer " - "
                   (if (built-in-signers signer)
                     "kube-controller-manager's csrsigning controller signs this signer; check that it runs with a cluster signing CA"
                     "no controller signs for this signer; check that the component that implements it runs"))}
       (str at)))))

(defn- failed-finding
  [csr]
  (let [{:keys [reason message lastUpdateTime lastTransitionTime]} (condition csr "Failed")]
    (onset/since
     {:severity :warning
      :component (-> csr :metadata :name)
      :summary (str "CertificateSigningRequest failed" (when-not (str/blank? reason) (str " (" reason ")")))
      :hint (str (or message "no message") " - signer " (-> csr :spec :signerName) ", requested by " (-> csr :spec :username))}
     (or lastUpdateTime lastTransitionTime))))

(defn detect-stuck-requests
  [evidence]
  (let [now (get evidence :now)]
    (for [csr (some-> (get evidence "certificatesigningrequests") :items)
          :let [finding (case (state csr)
                          :pending (pending-finding csr now)
                          :approved (unsigned-finding csr now)
                          :failed (failed-finding csr)
                          nil)]
          :when finding]
      finding)))

(def detectives
  [{:name "certificate-requests"
    :description "CertificateSigningRequests left pending, approved but unsigned, or failed"
    :requires ["certificatesigningrequests"]
    :detect detect-stuck-requests}])
