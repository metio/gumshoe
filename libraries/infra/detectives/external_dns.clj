;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.external-dns
  "Detective for external-dns: every hostname the cluster declares must
   actually resolve on the DNS server. A running external-dns pod means
   nothing - only existing records do."
  (:require [clojure.string :as str]))

(defn detect-unresolved-hostnames
  [evidence]
  (let [server (get evidence "server")
        resolved (get evidence "resolved")]
    (apply concat
           (for [[host kinds] (get evidence "sources")]
             (let [{:keys [a aaaa]} (get resolved host)
                   source (str/join ", " kinds)]
               (cond
                 (and (nil? a) (nil? aaaa))
                 [{:severity :warning
                   :component host
                   :summary (format "%s did not answer the query" server)
                   :hint "the DNS server was unreachable - rerun once it answers"}]

                 (and (empty? a) (empty? aaaa))
                 [{:severity :critical
                   :component host
                   :summary (format "hostname does not resolve (declared by %s)" source)
                   :hint "external-dns has not synced this record - check its logs and annotations"}]

                 :else
                 []))))))

(def detectives
  [{:name "external-dns"
    :description "Cluster-declared hostnames that do not resolve in DNS"
    :requires ["sources" "resolved" "server"]
    :detect detect-unresolved-hostnames}])
