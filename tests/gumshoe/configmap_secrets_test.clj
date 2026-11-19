;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.configmap-secrets-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.posture :as posture]
            [gumshoe.detectives.registry :as registry]))

;; Credential-shaped fixtures are assembled at run time, so the source holds no
;; string a secret scanner would rightly flag.
(def ^:private private-header (str "-----BEGIN " "PRIVATE" " KEY-----"))
(def ^:private pem (str "-----BEGIN RSA " "PRIVATE" " KEY-----\nMIIEow\n-----END RSA " "PRIVATE" " KEY-----"))
(def ^:private access-key (str "AK" "IA" "ABCDEFGHIJKLMNOP"))
(defn- with-password [scheme rest] (str scheme "://app" ":" "hunter2" "@" rest))

(deftest values-that-are-credentials-whatever-the-key
  (is (= "a private key" (posture/secret-reasons :tls.key pem)))
  (is (= "a private key" (posture/secret-reasons :bundle (str private-header "\nx"))))
  (is (= "an AWS access key" (posture/secret-reasons :config (str "aws_access_key_id = " access-key))))
  (is (= "a URL with a password" (posture/secret-reasons :DATABASE_URL (with-password "postgres" "db:5432/app"))))
  (is (= "a URL with a password" (posture/secret-reasons :settings.yaml (str "broker: " (with-password "amqp" "rabbit/"))))))

(deftest public-material-is-not-a-credential
  (is (nil? (posture/secret-reasons :ca.crt "-----BEGIN CERTIFICATE-----\nMII")))
  (is (nil? (posture/secret-reasons :url "https://example.org/path")))
  (is (nil? (posture/secret-reasons :url "postgres://app@db:5432/app")) "user without password")
  (is (nil? (posture/secret-reasons :image "registry.example.org:5000/app:1.0"))))

(deftest secret-sounding-keys-need-a-secret-shaped-value
  (doseq [[k v] [[:DB_PASSWORD "s3cr3t-value"] [:api_key "abcdef123456"] [:apiKey "x9"] [:client-secret "zzz"]
                 [:AUTH_TOKEN "eyJhbGciOi"] [:smtp.passwd "pw"] [:credentials "user:pw"]]]
    (is (= "a secret-sounding key with a value" (posture/secret-reasons k v)) (name k)))
  (doseq [[k v] [[:password_min_length "12"] [:token_ttl "30m"] [:require_password "true"] [:SECRET_FILE "/run/secrets/db"]
                 [:password ""] [:password "  "] [:token "${TOKEN}"] [:token "{{ .Values.token }}"] [:secret "<set me>"]
                 [:password "changeme"] [:password "****"] [:secret_ref "file:/etc/secret"] [:token_lifetime "1.5h"]]]
    (is (nil? (posture/secret-reasons k v)) (str (name k) "=" v)))
  (is (nil? (posture/secret-reasons :hostname "db.example.org")) "an ordinary key"))

(defn- detect [& configmaps]
  (vec (posture/detect-configmap-secrets {"configmaps" {:items (vec configmaps)}})))

(defn- configmap [name data & {:keys [namespace] :or {namespace "shop"}}]
  {:metadata {:namespace namespace :name name} :data data})

(deftest the-finding-names-keys-and-never-values
  (let [[finding & more] (detect (configmap "app" {:DB_PASSWORD "hunter2" :DATABASE_URL (with-password "postgres" "db/x") :LOG_LEVEL "debug"}))]
    (is (nil? more))
    (is (= :warning (:severity finding)))
    (is (= "shop/app" (:component finding)))
    (is (= "ConfigMap may hold credentials in DATABASE_URL, DB_PASSWORD" (:summary finding)))
    (is (str/starts-with? (:hint finding) "DATABASE_URL: a URL with a password; DB_PASSWORD: a secret-sounding key with a value - "))
    (is (not (re-find #"hunter2" (pr-str finding))) "the value never leaves the cluster")))

(deftest clean-and-exempt-configmaps-are-silent
  (is (empty? (detect (configmap "app" {:LOG_LEVEL "debug" :token_ttl "30m"}))))
  (is (empty? (detect (configmap "kube-root-ca.crt" {:ca.crt "-----BEGIN CERTIFICATE-----"}))))
  (is (empty? (detect (configmap "boot" {:token "abc"} :namespace "kube-system"))) "platform namespaces")
  (is (empty? (detect {:metadata {:namespace "shop" :name "binary"} :binaryData {:password "aGk="}})) "binaryData is not read"))

(deftest the-check-lives-in-the-posture-scope
  (is (contains? (set (map :name (registry/for-scope :posture))) "configmap-secrets"))
  (testing "left out of whole-cluster scans"
    (is (not (contains? (set (map :name (registry/for-scope :all))) "configmap-secrets")))))
