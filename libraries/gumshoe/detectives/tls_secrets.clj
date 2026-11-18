;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.tls-secrets
  "Detective for the certificates clients actually see: the TLS Secrets an
   Ingress or a Gateway listener serves. A certificate nobody renews - copied
   in by hand, issued by a tool that stopped running - expires silently. A
   Secret cert-manager manages is left to the certificates detective, which
   reads cert-manager's own view of the renewal."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]
            [gumshoe.tls :as tls]))

(def gateway-type "gateways.gateway.networking.k8s.io")

(def ^:private warning-days 14)

(defn- served-secrets
  "namespace/name of each Secret served, with who serves it."
  [ingresses gateways]
  (reduce (fn [acc [secret user]] (update acc secret (fnil conj (sorted-set)) user))
          {}
          (concat
           (for [ingress (kubectl/items-of ingresses)
                 tls (-> ingress :spec :tls)
                 :when (:secretName tls)]
             [(str (kubectl/namespace-of ingress) "/" (:secretName tls))
              (str "Ingress " (kubectl/name-of ingress))])
           (for [gateway (kubectl/items-of gateways)
                 listener (-> gateway :spec :listeners)
                 ref (-> listener :tls :certificateRefs)
                 :when (contains? #{nil "" "Secret"} (:kind ref))
                 :when (contains? #{nil ""} (:group ref))]
             [(str (or (:namespace ref) (kubectl/namespace-of gateway)) "/" (:name ref))
              (str "Gateway " (kubectl/namespace-of gateway) "/" (kubectl/name-of gateway)
                   " listener " (:name listener))]))))

(defn- days-between
  [^java.time.Instant from ^java.time.Instant to]
  (.toDays (java.time.Duration/between from to)))

(defn detect-tls-secret-problems
  [evidence]
  (let [now ^java.time.Instant (:now evidence)
        secrets (into {} (map (juxt #(str (:namespace %) "/" (:name %)) identity)) (get evidence "tls-secrets"))]
    (for [[key users] (sort-by key (served-secrets (get evidence "ingresses") (get evidence gateway-type)))
          :let [secret (get secrets key)]
          :when (and secret now (not (:certificate-name secret)))
          :let [certificate (tls/certificate-base64 (:crt secret))
                served (str "served by " (str/join ", " users))
                base {:component key}]
          finding [(cond
                     (nil? certificate)
                     (assoc base :severity :warning
                            :summary "TLS Secret holds no certificate that parses"
                            :hint (str served " - clients get a handshake error or the controller's default certificate"))

                     (.isBefore ^java.time.Instant (:not-after certificate) now)
                     (onset/since
                      (assoc base :severity :critical
                             :summary (format "TLS certificate expired %s" (:not-after certificate))
                             :hint (str served " - " (:subject certificate) "; replace the certificate"))
                      (:not-after certificate))

                     (.isAfter ^java.time.Instant (:not-before certificate) now)
                     (assoc base :severity :critical
                            :summary (format "TLS certificate is not valid before %s" (:not-before certificate))
                            :hint (str served " - the node clocks or the issuer's clock are wrong, or it was installed early"))

                     (< (days-between now (:not-after certificate)) warning-days)
                     (assoc base :severity :warning
                            :summary (format "TLS certificate expires in %d days" (days-between now (:not-after certificate)))
                            :hint (str served " - " (:subject certificate) "; nothing renews it automatically"))

                     :else nil)]
          :when finding]
      finding)))

(def detectives
  [{:name "tls-secrets"
    :description "TLS Secrets served by Ingresses and Gateways whose certificate expired or expires soon"
    :requires ["ingresses" gateway-type "tls-secrets"]
    :detect detect-tls-secret-problems}])
