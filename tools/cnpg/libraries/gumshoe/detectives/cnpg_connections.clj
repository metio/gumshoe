;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.cnpg-connections
  "A CloudNativePG cluster at its connection ceiling, and who holds it.

   Once the client backends reach max_connections less the reserved slots,
   PostgreSQL refuses every ordinary role with `remaining connection slots are
   reserved`, so every application on the cluster is down while the Cluster,
   its pods and its poolers all read healthy. The finding names the holders by
   client address and splits them three ways: addresses a running pod holds,
   addresses no running pod holds, and backends without an address, which
   connected over the local socket.

   An address no running pod holds is usually a client that vanished, such as
   a replaced pooler pod. PostgreSQL does not notice a client gone in the
   middle of returning results, so its backend stays active and no idle
   timeout reclaims it. A pod that ran to completion may have held the
   address since, so only a Running pod counts as holding it. The server's
   own processes hold no client slot; the hint counts them apart."
  (:require [clojure.string :as str]
            [gumshoe.cnpg.connections :as connections]
            [gumshoe.evidence :as evidence]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.postgres :as postgres]))

(def cluster-type connections/cluster-type)

(def joins
  "The Clusters, the pods whose addresses tell live clients from vanished
   ones, and the connection readings."
  [cluster-type "pods" connections/evidence-key])

(def ^:private shown-addresses
  "How many addresses the hint names, largest holder first."
  5)

(defn- normalize
  "An IP address in one spelling, so an IPv4-mapped IPv6 address and a
   differently compressed IPv6 address compare equal. Only a dotted IPv4
   literal or a literal holding a colon is parsed, so no name is ever
   resolved."
  [address]
  (when (string? address)
    (if (or (re-matches #"[0-9]{1,3}(\.[0-9]{1,3}){3}" address)
            (and (str/includes? address ":") (re-matches #"[0-9A-Fa-f.:]+" address)))
      (try (.getHostAddress (java.net.InetAddress/getByName address))
           (catch Exception _ address))
      address)))

(defn- running-pod-addresses
  "address -> namespace/name of each Running pod holding it."
  [pods]
  (into {}
        (for [pod (kubectl/items-of pods)
              :when (= "Running" (-> pod :status :phase))
              ip (distinct (cons (-> pod :status :podIP) (map :ip (-> pod :status :podIPs))))
              :when ip]
          [(normalize ip) (kubectl/namespace-name-of pod)])))

(defn- client? [row] (= "client backend" (:backend-type row)))

(defn- holders
  "One entry per client address, largest first: its backends, the pod holding
   it or nil, and its rows."
  [rows owners]
  (->> (filter client? rows)
       (group-by :client-addr)
       (map (fn [[address group]]
              {:address address
               :pod (when address (owners (normalize address)))
               :backends (reduce + (map :backends group))
               :rows group}))
       (sort-by (juxt (comp - :backends) (comp str :address)))))

(defn- plural [n one many] (if (= 1 n) one many))

(defn- split-phrase
  [entries]
  (let [vanished (filter #(and (:address %) (not (:pod %))) entries)
        live (filter :pod entries)
        socket (filter (complement :address) entries)
        total #(reduce + (map :backends %))]
    (str/join ", "
              (remove nil?
                      [(when (seq vanished)
                         (format "%d from %d %s with no running pod" (total vanished) (count vanished)
                                 (plural (count vanished) "address" "addresses")))
                       (when (seq live)
                         (let [pods (count (distinct (map :pod live)))]
                           (format "%d from %d running %s" (total live) pods (plural pods "pod" "pods"))))
                       (when (seq socket)
                         (format "%d without a client address (local socket)" (total socket)))]))))

(defn- counted
  "\"5 active, 10 idle\": the values of one field, each with its backends."
  [rows field]
  (->> (group-by field rows)
       (map (fn [[v group]] [(or v "unknown") (reduce + (map :backends group))]))
       (sort-by first)
       (map (fn [[v n]] (format "%d %s" n v)))
       (str/join ", ")))

(defn- distinct-values
  [rows field]
  (str/join ", " (sort (distinct (map #(or (field %) "unknown") rows)))))

(defn- holder-line
  [{:keys [address pod backends rows]}]
  (format "%s holds %d: %s; user %s; application %s"
          (cond (nil? address) "the local socket"
                pod (format "%s (pod %s)" address pod)
                :else (format "%s (no running pod)" address))
          backends (counted rows :state) (distinct-values rows :user) (distinct-values rows :application)))

(defn- hint
  [entries rows]
  (let [shown (take shown-addresses entries)
        more (- (count entries) (count shown))
        background (reduce + (map :backends (remove client? rows)))]
    (str (str/join "; " (map holder-line shown))
         (when (pos? more) (format "; and %d more %s" more (plural more "address" "addresses")))
         (when (pos? background)
           (format ". %d background %s hold no connection slot"
                   background (plural background "process" "processes")))
         ". A backend whose client vanished mid-query stays active and is never reclaimed "
         "unless client_connection_check_interval is set; confirm no running pod holds an "
         "address before ending its backends with pg_terminate_backend")))

(defn- ceiling-finding
  [cluster {:keys [reading]} owners]
  (let [in-use (postgres/client-backends reading)
        ceiling (postgres/connection-ceiling reading)]
    (when (>= in-use ceiling)
      (let [entries (holders (:rows reading) owners)]
        {:severity :critical
         :component (kubectl/namespace-name-of cluster)
         :summary (format "%d of max_connections %d in use, at or above the %d left for ordinary roles: %s"
                          in-use (:max-connections reading) ceiling (split-phrase entries))
         :hint (hint entries (:rows reading))}))))

(defn- unread-summary
  [{:keys [namespace pod reason]}]
  (case reason
    :no-primary "the Cluster names no current primary and no pod carries cnpg.io/instanceRole=primary"
    :primary-unread (str "the Cluster names no current primary and the read of pods labelled "
                         "cnpg.io/instanceRole=primary did not answer")
    :in-recovery (format "pod %s/%s answered from recovery, so it is not the primary" namespace pod)
    (format "kubectl exec into pod %s/%s, container %s, did not answer" namespace pod connections/container)))

(defn- unread-hint
  [{:keys [reason]}]
  (case reason
    :in-recovery "a switchover may be under way; scan again once the Cluster names its new primary"
    (:no-primary :primary-unread) "the cnpg detectives report a cluster without a primary; nothing was read here"
    (str "exec may be denied (create on pods/exec), psql may have failed, or every slot including "
         "the superuser-reserved ones is taken; a read that did not answer is not zero connections")))

(defn- unread-finding
  [cluster row]
  {:severity :warning
   :component (kubectl/namespace-name-of cluster)
   :summary (str "connections unread: " (unread-summary row))
   :hint (unread-hint row)})

(defn detect-connection-ceiling
  "Clusters whose client backends are at or above the ceiling ordinary roles
   may reach, as critical, naming the holders; and every cluster whose
   connections could not be read, as a warning. Only clusters the Cluster read
   holds are judged. Nothing is judged when a joined read did not answer."
  [evidence]
  (when (evidence/joined? evidence joins)
    (let [clusters (kubectl/items-of (get evidence cluster-type))
          by-key (into {} (map (juxt (juxt kubectl/namespace-of kubectl/name-of) identity)) clusters)
          owners (running-pod-addresses (get evidence "pods"))
          {:keys [items unread]} (get evidence connections/evidence-key)
          cluster-of #(by-key [(:namespace %) (:cluster %)])]
      (concat
       (for [row (sort-by (juxt :namespace :cluster) items)
             :let [cluster (cluster-of row)]
             :when cluster
             :let [finding (ceiling-finding cluster row owners)]
             :when finding]
         finding)
       (for [row (sort-by (juxt :namespace :cluster) unread)
             :let [cluster (cluster-of row)]
             :when cluster]
         (unread-finding cluster row))))))

(def detectives
  [{:name "cnpg-connections"
    :description "Clusters at their connection ceiling, a total outage for ordinary roles, with the holders split into running pods, addresses no running pod holds and the local socket"
    :requires joins
    :joins joins
    :judges [connections/evidence-key]
    :detect detect-connection-ceiling}])
