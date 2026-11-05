;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.verifier-churn
  "Reads a cluster's stored password verifiers twice, a reconcile interval
   apart, and says whether something is re-deriving them behind the poolers."
  (:require [gumshoe.cnpg.verifier-churn :as churn]
            [gumshoe.detectives.pooler :as pooler]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(def options
  {:namespace {:desc "Namespace of the cluster - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :cluster {:desc "Name of the cluster - interactive selection when omitted"
             :alias :c
             :coerce :string}
   :window {:desc "Seconds between the two readings - cover one reconcile interval"
            :alias :w
            :default churn/default-window-seconds
            :coerce :long}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [pooler/cluster-type "pods"]})

(def ^:private verifier-sql
  "select rolname || '|' || md5(rolpassword) from pg_authid
    where rolcanlogin and rolpassword is not null order by rolname")

(defn- read-verifiers
  [context namespace pod]
  (shell/stdout-of "kubectl" (str "--context=" context) (str "--namespace=" namespace)
                   "exec" pod "-c" "postgres" "--"
                   "psql" "-At" "-c" verifier-sql))

(defn- snapshot
  [context namespace pod]
  (churn/parse-snapshot (read-verifiers context namespace pod)))

(defn- churn-check
  [opts _ctx]
  (let [context (kubectl/current-context)
        clusters (kubectl/items-of (kubectl/get-all context pooler/cluster-type))
        candidates (mapv kubectl/namespace-name-of clusters)
        chosen (interact/choose-namespaced "Cluster" candidates
                                           (:namespace opts) (:cluster opts))]
    (if (nil? chosen)
      (do (stdout/error "no cluster selected") false)
      (let [{:keys [namespace]} (kubectl/split-namespace-name chosen)
            primary (-> (kubectl/find-item clusters chosen) :status :currentPrimary)
            window (:window opts)]
        (if (nil? primary)
          (do (stdout/error (format "cluster %s names no currentPrimary" chosen)) false)
          (let [before (snapshot context namespace primary)]
            (stdout/print-data-table {:cluster chosen
                                      :primary primary
                                      :roles (count before)
                                      :window (format "%ds" window)})
            (if (zero? (count before))
              (do (doseq [line (churn/guidance {:verdict :no-roles})] (stdout/warn line))
                  false)
              (do
                (stdout/with-spinner
                  (format "waiting %ds for a reconcile interval to pass" window)
                  #(Thread/sleep (* 1000 window)))
                (let [after (snapshot context namespace primary)
                      changes (churn/compare-snapshots before after)
                      result (churn/verdict changes)]
                  (stdout/print-section "Verdict")
                  (stdout/print-data-table (select-keys result [:verdict :summary]))
                  (when-let [note (:note result)] (stdout/warn note))
                  (when (seq (:moved changes))
                    (stdout/print-section "Roles whose verifier moved")
                    (doseq [role (take 20 (:moved changes))] (println role))
                    (when (> (count (:moved changes)) 20)
                      (println (format "... and %d more" (- (count (:moved changes)) 20)))))
                  (doseq [line (churn/guidance result)]
                    (if (= :churning (:verdict result)) (stdout/warn line) (stdout/ok line)))
                  (not= :churning (:verdict result)))))))))))

(runbook/execute!
 {:description "Reads a cluster's password verifiers twice and says whether something re-derives them"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action churn-check})
