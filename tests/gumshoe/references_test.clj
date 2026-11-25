;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.references-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.references :as references]))

(defn- object
  ([namespace name] (object namespace name {}))
  ([namespace name more] (merge {:metadata (cond-> {:name name} namespace (assoc :namespace namespace))} more)))

(defn- service
  [name & ports]
  (object "shop" name {:spec {:ports (vec (for [[port port-name] ports] {:port port :name port-name}))}}))

(defn- ingress
  [spec]
  (object "shop" "web" {:spec spec}))

(defn- summaries [findings] (mapv :summary findings))

(deftest ingress-references-are-checked
  (let [evidence {"services" {:items [(service "web" [80 "http"])]}
                  "ingressclasses" {:items [(object nil "nginx")]}
                  "secret-names" ["shop/web-tls"]}
        run #(references/detect-ingress-references (assoc evidence "ingresses" {:items [(ingress %)]}))]
    (is (empty? (run {:ingressClassName "nginx"
                      :tls [{:hosts ["shop.example"] :secretName "web-tls"}]
                      :rules [{:http {:paths [{:backend {:service {:name "web" :port {:number 80}}}}
                                              {:backend {:service {:name "web" :port {:name "http"}}}}]}}]})))
    (is (= ["Ingress backend Service api does not exist"
            "Ingress backend Service web has no port 8080"
            "Ingress TLS Secret shop-tls does not exist"
            "IngressClass traefik does not exist"]
           (summaries (run {:ingressClassName "traefik"
                            :tls [{:hosts ["shop.example"] :secretName "shop-tls"}]
                            :defaultBackend {:service {:name "api" :port {:number 80}}}
                            :rules [{:http {:paths [{:backend {:service {:name "web" :port {:number 8080}}}}
                                                    {:backend {:service {:name "web" :port {:number 8080}}}}]}}]}))))
    (is (= [:critical :warning] (map :severity (run {:defaultBackend {:service {:name "api"}}
                                                     :tls [{:secretName "gone"}]}))))
    (testing "without the Secret names the TLS check stays silent instead of guessing"
      (is (empty? (references/detect-ingress-references
                   (assoc evidence "secret-names" nil
                          "ingresses" {:items [(ingress {:tls [{:secretName "gone"}]})]})))))))

(defn- redirect-ingress
  "An Ingress shaped like a redirect product's: the work is done in
   annotations and the backend names a placeholder Service that does not exist."
  [annotations]
  {:apiVersion "networking.k8s.io/v1"
   :kind "Ingress"
   :metadata {:namespace "eh-ludwigsburg-de-redirect-1" :name "redirect" :annotations annotations}
   :spec {:ingressClassName "nginx"
          :rules [{:host "eh-ludwigsburg.de"
                   :http {:paths [{:path "/" :pathType "Prefix"
                                   :backend {:service {:name "no-backend" :port {:number 8000}}}}]}}]}})

(defn- run-redirect
  [annotations]
  (references/detect-ingress-references
   {"ingresses" {:items [(redirect-ingress annotations)]}
    "services" {:items []}
    "ingressclasses" {:items [(object nil "nginx")]}
    "secret-names" []}))

(deftest a-redirect-only-ingress-is-not-critical-for-its-missing-backend
  (doseq [[annotations target]
          [[{:nginx.ingress.kubernetes.io/temporal-redirect "https://www.eh-ludwigsburg.de"} "https://www.eh-ludwigsburg.de"]
           [{:nginx.ingress.kubernetes.io/permanent-redirect "https://www.eh-ludwigsburg.de/"} "https://www.eh-ludwigsburg.de/"]
           [{:nginx.ingress.kubernetes.io/configuration-snippet "rewrite ^/(.*)$ https://www.eh-ludwigsburg.de/$1 redirect;\n"}
            "https://www.eh-ludwigsburg.de/$1"]
           [{:nginx.ingress.kubernetes.io/configuration-snippet "more_set_headers \"X-Robots-Tag: noindex\";\nrewrite ^ https://example.org permanent;"}
            "https://example.org"]
           [{:nginx.ingress.kubernetes.io/configuration-snippet "rewrite ^/(.*)$ /landing/$1 permanent;"} "/landing/$1"]
           [{:nginx.ingress.kubernetes.io/configuration-snippet "rewrite \"^/(.*)$\" https://example.org/$1 last;"}
            "https://example.org/$1"]
           [{:nginx.ingress.kubernetes.io/configuration-snippet "return 301 https://example.org$request_uri;"}
            "https://example.org$request_uri"]]]
    (testing (pr-str annotations)
      (let [findings (run-redirect annotations)]
        (is (= [:info] (map :severity findings)))
        (is (= [(format "Ingress redirects to %s - backend Service no-backend does not exist and receives no requests" target)]
               (summaries findings)))))))

(deftest an-ingress-that-still-proxies-keeps-its-missing-backend-critical
  (doseq [annotations
          [{}
           {:nginx.ingress.kubernetes.io/temporal-redirect ""}
           {:nginx.ingress.kubernetes.io/temporal-redirect-code "308"}
           {:example.com/permanent-redirect "https://example.org"}
           {:nginx.ingress.kubernetes.io/configuration-snippet "rewrite ^/(.*)$ /index.php?q=$1 last;"}
           {:nginx.ingress.kubernetes.io/configuration-snippet "rewrite ^/(.*)$ /app/$1 break;"}
           {:nginx.ingress.kubernetes.io/configuration-snippet "rewrite ^/old/(.*)$ https://example.org/new/$1 permanent;"}
           {:nginx.ingress.kubernetes.io/configuration-snippet "if ($host = old.example) { rewrite ^ https://example.org redirect; }"}
           {:nginx.ingress.kubernetes.io/configuration-snippet "# rewrite ^ https://example.org redirect;\nproxy_set_header X-Foo bar;"}
           {:nginx.ingress.kubernetes.io/configuration-snippet "return 200 ok;"}]]
    (testing (pr-str annotations)
      (is (= [[:critical "Ingress backend Service no-backend does not exist"]]
             (map (juxt :severity :summary) (run-redirect annotations)))))))

(deftest a-redirect-only-ingress-is-not-critical-for-a-missing-port
  (is (= [:info]
         (map :severity
              (references/detect-ingress-references
               {"ingresses" {:items [(redirect-ingress {:nginx.ingress.kubernetes.io/permanent-redirect "https://example.org"})]}
                "services" {:items [(object "eh-ludwigsburg-de-redirect-1" "no-backend" {:spec {:ports [{:port 80}]}})]}
                "ingressclasses" {:items [(object nil "nginx")]}
                "secret-names" []})))))

(deftest service-addresses-are-read-from-env-values
  (doseq [[value expected] [["redis.cache:6379" ["cache" "redis" 6379]]
                            ["http://api.shop.svc.cluster.local:8080/v1" ["shop" "api" 8080]]
                            ["postgres://user@db.data.svc:5432/app" ["data" "db" 5432]]
                            ["api.shop.svc" ["shop" "api" nil]]
                            ["redis" nil]
                            ["localhost:6379" nil]
                            ["$(REDIS_HOST):6379" nil]
                            ["api.example.com" nil]
                            ["https://api.example.com/v1" nil]]]
    (is (= expected (#'references/service-address "shop" value)) value)))

(defn- deployment
  [spec]
  (object "shop" "web" {:spec {:template {:spec spec}}}))

(deftest template-references-are-checked
  (let [evidence {"services" {:items [(object "cache" "redis" {:spec {:ports [{:port 6379}]}})]}
                  "serviceaccounts" {:items [(object "shop" "default") (object "shop" "web")]}
                  "secret-names" ["shop/registry"]
                  "namespaces" {:items [(object nil "shop") (object nil "cache")]}
                  "statefulsets" {:items []}
                  "daemonsets" {:items []}
                  "cronjobs" {:items []}}
        run #(references/detect-template-references (assoc evidence "deployments" {:items [(deployment %)]}))]
    (is (empty? (run {:serviceAccountName "web"
                      :imagePullSecrets [{:name "registry"}]
                      :containers [{:name "app" :env [{:name "REDIS_HOST" :value "redis.cache"}
                                                      {:name "REDIS_URL" :value "redis://redis.cache:6379"}
                                                      {:name "PUBLIC_URL" :value "https://shop.example.com"}]}]})))
    (let [findings (run {:serviceAccountName "deployer"
                         :imagePullSecrets [{:name "gone"}]
                         :containers [{:name "app" :env [{:name "REDIS_ADDR" :value "redis.cache:6380"}
                                                         {:name "QUEUE_HOST" :value "rabbit.cache"}
                                                         {:name "LOG_LEVEL" :value "debug.cache"}]}]})]
      (is (= ["Deployment uses ServiceAccount deployer, which does not exist"
              "Deployment pulls images with Secret gone, which does not exist"
              "env REDIS_ADDR points at port 6380, which Service cache/redis does not expose"
              "env QUEUE_HOST points at Service cache/rabbit, which does not exist"]
             (summaries findings)))
      (is (= [:critical :warning :warning :warning] (map :severity findings)))
      (is (= {:kind "Deployment" :namespace "shop" :name "web"} (:subject (first findings)))))
    (testing "a two-label host in a namespace that does not exist is an external name"
      (is (empty? (run {:containers [{:name "app" :env [{:name "API_HOST" :value "api.partner"}]}]}))))))

(deftest storage-class-references-are-checked
  (let [claim (fn [class phase] (object "shop" "data" {:spec {:storageClassName class} :status {:phase phase}}))
        run #(references/detect-storage-class-references {"persistentvolumeclaims" {:items [%]}
                                                          "storageclasses" {:items [(object nil "ceph")]}})]
    (is (empty? (run (claim "ceph" "Pending"))))
    (is (empty? (run (claim "" "Bound"))) "an empty class asks for static binding")
    (is (= [:critical] (map :severity (run (claim "fast" "Pending")))))
    (is (= [:info] (map :severity (run (claim "fast" "Bound")))))))

(deftest role-references-are-checked
  (let [findings (references/detect-role-references
                  {"roles" {:items [(object "shop" "reader")]}
                   "clusterroles" {:items [(object nil "view")]}
                   "rolebindings" {:items [(object "shop" "ok" {:roleRef {:kind "Role" :name "reader"}})
                                           (object "shop" "ok-cluster" {:roleRef {:kind "ClusterRole" :name "view"}})
                                           (object "shop" "dangling" {:roleRef {:kind "Role" :name "writer"}})]}
                   "clusterrolebindings" {:items [(object nil "old" {:roleRef {:kind "ClusterRole" :name "psp:restricted"}})]}})]
    (is (= ["RoleBinding grants Role writer, which does not exist"
            "ClusterRoleBinding grants ClusterRole psp:restricted, which does not exist"]
           (summaries findings)))
    (is (= ["shop/dangling" "old"] (map :component findings)))))

(deftest webhook-references-are-checked
  (let [config (fn [policy] {:metadata {:name "policy"}
                             :webhooks [{:name "v.example" :failurePolicy policy
                                         :clientConfig {:service {:namespace "policy" :name "webhook"}}}
                                        {:name "url.example" :clientConfig {:url "https://hook.example"}}]})
        run #(references/detect-webhook-references {"validatingwebhookconfigurations" {:items [(config %)]}
                                                    "mutatingwebhookconfigurations" {:items []}
                                                    "services" {:items []}})]
    (is (= [:critical] (map :severity (run nil))) "failurePolicy defaults to Fail")
    (is (= [:warning] (map :severity (run "Ignore"))))
    (is (empty? (references/detect-webhook-references
                 {"validatingwebhookconfigurations" {:items [(config "Fail")]}
                  "mutatingwebhookconfigurations" {:items []}
                  "services" {:items [(object "policy" "webhook")]}})))))
