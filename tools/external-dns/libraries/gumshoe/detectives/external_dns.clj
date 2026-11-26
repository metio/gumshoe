;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.external-dns
  "Detective for external-dns: every hostname the cluster declares must
   actually resolve on the DNS server. A running external-dns pod means
   nothing - only existing records do."
  (:require [clojure.string :as str]))

(defn detect-unresolved-hostnames
  "A critical finding for a host only when both its A and AAAA queries
   answered with no records. An answer is nil when its query got none, and a
   host whose one query did not answer while the other found nothing is
   unread: the record may sit behind the query that was lost. A host marked
   :invalid was never queried, because it is no DNS name, and is reported as
   such rather than as a server that did not answer."
  [evidence]
  (let [server (get evidence "server")
        resolved (get evidence "resolved")]
    (apply concat
           (for [[host kinds] (get evidence "sources")]
             (let [{:keys [a aaaa invalid]} (get resolved host)
                   source (str/join ", " kinds)]
               (cond
                 invalid
                 [{:severity :warning
                   :component host
                   :summary (format "declared hostname is not a valid DNS name (declared by %s)" source)
                   :hint (str "the name was not queried - it holds characters or a form no DNS name has "
                              "(a space, an option-like token); correct it where it is declared")}]

                 (and (nil? a) (nil? aaaa))
                 [{:severity :warning
                   :component host
                   :summary (format "%s did not answer the query" server)
                   :hint "the DNS server was unreachable - rerun once it answers"}]

                 (and (some? a) (some? aaaa) (empty? a) (empty? aaaa))
                 [{:severity :critical
                   :component host
                   :summary (format "hostname does not resolve (declared by %s)" source)
                   :hint "external-dns has not synced this record - check its logs and annotations"}]

                 (and (empty? a) (empty? aaaa))
                 (let [[lost answered] (if (nil? a) ["A" "AAAA"] ["AAAA" "A"])]
                   [{:severity :warning
                     :component host
                     :summary (format "%s did not answer the %s query, and the %s query found no records"
                                      server lost answered)
                     :hint (format "whether the hostname resolves is unknown until the %s query answers - rerun" lost)}])

                 :else
                 []))))))

(def detectives
  [{:name "external-dns"
    :description "Cluster-declared hostnames that do not resolve in DNS"
    :requires ["sources" "resolved" "server"]
    ;; "server" is always set, so only a join marks the check unread when the
    ;; cluster reads behind "sources" did not answer
    :joins ["sources"]
    :detect detect-unresolved-hostnames}])
