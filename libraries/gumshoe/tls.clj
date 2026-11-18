;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tls
  "Reads X.509 certificates as data. The leaf certificate of a PEM bundle is
   the first one, so that is the one judged."
  (:require [clojure.string :as str])
  (:import (java.io ByteArrayInputStream)
           (java.security.cert CertificateFactory X509Certificate)
           (java.util Base64)))

(defn certificate
  "The first certificate in PEM text, as {:subject :not-before :not-after
   :dns-names}, or nil when the text holds none that parses."
  [pem]
  (try
    (let [^X509Certificate cert (.generateCertificate (CertificateFactory/getInstance "X.509")
                                                      (ByteArrayInputStream. (.getBytes (str pem) "US-ASCII")))]
      {:subject (str (.getSubjectX500Principal cert))
       :not-before (.toInstant (.getNotBefore cert))
       :not-after (.toInstant (.getNotAfter cert))
       :dns-names (vec (for [[kind value] (.getSubjectAlternativeNames cert)
                             :when (= 2 kind)]
                         value))})
    (catch Exception _ nil)))

(defn certificate-base64
  "The first certificate of a base64-encoded PEM bundle, as a Secret's
   tls.crt holds it."
  [encoded]
  (try
    (certificate (String. (.decode (Base64/getMimeDecoder) (str/trim (str encoded))) "US-ASCII"))
    (catch Exception _ nil)))
