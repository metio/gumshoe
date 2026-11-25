;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.check-reads-test
  "Post-checks that assert an absence or a negation, run through
   verify/eventually against a kubectl that does not answer, or that answers
   that the object is gone. Each would read nil and hold if its reads were
   lax, or if it judged a missing object as being in the state it waits for."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.ceph :as ceph]
            [gumshoe.cnpg.operations :as operations]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.prometheus :as prometheus]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.steps.flux :as flux]
            [gumshoe.steps.nodes :as nodes]
            [gumshoe.steps.objects :as objects]
            [gumshoe.storage :as storage]
            [gumshoe.tools.jaas :as jaas]
            [gumshoe.velero.action-books :as velero-books]
            [gumshoe.verify :as verify]
            [gumshoe.workloads :as workloads]))

;; Loading a book with *loading* bound defines its functions and runs nothing.
(binding [runbook/*loading* true]
  (run! load-file ["runbooks/kubernetes/nodes/remove_taint.clj"
                   "runbooks/kubernetes/orders/recreate.clj"
                   "tools/jaas/runbooks/jaas/resume.clj"
                   "tools/jaas/runbooks/jaas/suspend.clj"]))

(defn- unanswered
  [& _]
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"})

(defn- verified?
  [check]
  (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
    (verify/eventually (assoc check :timeout 0 :interval 0))))

(defn- book-fn
  [sym]
  @(requiring-resolve sym))

(deftest absence-checks-do-not-hold-on-an-unanswered-read
  (with-redefs [shell/execute unanswered]
    (testing "uncordon: the node is schedulable"
      (is (false? (verified? (nodes/schedulable-check "ctx" "n1")))))
    (testing "remove-taint: the taint is gone"
      (is (false? (verified? ((book-fn 'runbooks.kubernetes.nodes.remove-taint/gone-check)
                              "ctx" "n1" "k=v:NoSchedule")))))
    (testing "orders/recreate: the invalid order is cleared"
      (is (false? (verified? (first ((book-fn 'runbooks.kubernetes.orders.recreate/cleared-checks)
                                     "ctx" ["web/order-1"]))))))
    (testing "jaas resume: the snippet is no longer suspended"
      (is (false? (verified? ((book-fn 'runbooks.jaas.resume/resumed-check) "ctx" "web" "site" "web/site")))))
    (testing "cronjobs/resume: the CronJob is resumed"
      (is (false? (verified? {:description "resumed"
                              :check #(= false (workloads/suspended?
                                                (kubectl/get-namespaced-resource "ctx" "web" "cronjobs" "nightly")))}))))
    (testing "flux resume: the object is no longer suspended"
      (is (false? (verified? {:description "resumed"
                              :check #(= false (true? (-> (kubectl/get-namespaced-resource "ctx" "flux-system" "kustomizations" "apps")
                                                          :spec :suspend)))}))))
    (testing "cnpg unfence: the instance is no longer fenced"
      (is (false? (verified? {:description "unfenced"
                              :check #(not (contains? (or (operations/fenced (kubectl/get-namespaced-resource "ctx" "db" "clusters.postgresql.cnpg.io" "pg"))
                                                          #{})
                                                      "pg-1"))}))))))

(deftest absence-checks-hold-on-not-found
  (with-redefs [shell/execute (fn [& argv]
                                (if (some #{"--ignore-not-found"} argv)
                                  {:exit 0 :out "" :err ""}
                                  {:exit 1 :out "" :err "Error from server (NotFound): orders.acme.cert-manager.io \"order-1\" not found"}))]
    (testing "orders/recreate: an order that is gone is cleared"
      (is (true? (verified? (first ((book-fn 'runbooks.kubernetes.orders.recreate/cleared-checks)
                                    "ctx" ["web/order-1"]))))))))

;; --- an object that is gone is in no state ---------------------------------------------

(defn- answering
  "A shell/execute answering a kubectl get of one named object with object,
   NotFound when it is nil, and a get that ignores a missing object as the
   API server does."
  [object]
  (fn [& argv]
    (cond
      (some #{"--ignore-not-found"} argv) {:exit 0 :out (if object "found/it" "") :err ""}
      object {:exit 0 :out (json/generate-string object) :err ""}
      :else {:exit 1 :out "" :err "Error from server (NotFound): x \"y\" not found"})))

(defn- holds-against
  [object check]
  (with-redefs [shell/execute (answering object)]
    (verified? check)))

(defn- load-spec
  [path]
  (with-redefs [mutation/book identity] (load-file path)))

(deftest a-node-that-is-gone-is-neither-cordoned-nor-schedulable
  (is (false? (holds-against nil (nodes/schedulable-check "ctx" "n1"))))
  (is (false? (holds-against nil (nodes/unschedulable-check "ctx" "n1"))))
  (is (true? (holds-against {:metadata {:name "n1"} :spec {}} (nodes/schedulable-check "ctx" "n1"))))
  (is (true? (holds-against {:metadata {:name "n1"} :spec {:unschedulable true}} (nodes/unschedulable-check "ctx" "n1"))))
  (testing "remove-taint: a node that is gone carries no taint, and that verifies nothing"
    (let [check ((book-fn 'runbooks.kubernetes.nodes.remove-taint/gone-check) "ctx" "n1" "k=v:NoSchedule")]
      (is (false? (holds-against nil check)))
      (is (true? (holds-against {:metadata {:name "n1"} :spec {:taints []}} check))))))

(deftest a-snippet-is-resumed-once-the-operator-reports-a-reason-other-than-suspended
  (let [check ((book-fn 'runbooks.jaas.resume/resumed-check) "ctx" "web" "site" "web/site")
        snippet (fn [conditions] {:metadata {:name "site"} :spec {} :status {:conditions conditions}})]
    (is (false? (holds-against nil check)) "a snippet that is gone")
    (is (false? (holds-against (snippet []) check)) "no Ready condition: the operator has not looked since")
    (is (false? (holds-against (snippet [{:type "Ready" :status "False" :reason "Suspended"}]) check)))
    (is (true? (holds-against (snippet [{:type "Ready" :status "False" :reason "Pending"}]) check)))
    (is (true? (holds-against (snippet [{:type "Ready" :status "True" :reason "Succeeded"}]) check))))
  (is (false? (jaas/resumed? nil)))
  (is (= "Suspended" (jaas/reason-of {:status {:conditions [{:type "Ready" :reason "Suspended"}]}}))))

(deftest an-instance-is-unfenced-only-on-a-cluster-whose-fence-reads
  (let [{:keys [verify]} (load-spec "tools/cnpg/runbooks/unfence.clj")
        ctx {:context "ctx" :ref {:type "clusters.postgresql.cnpg.io" :namespace "db" :name "pg" :cluster "pg" :instance "pg-2"}}
        [check] (verify ctx)
        cluster (fn [fenced] {:metadata (cond-> {:name "pg"}
                                          fenced (assoc :annotations {:cnpg.io/fencedInstances fenced}))})]
    (is (false? (holds-against nil check)) "a cluster that is gone")
    (is (false? (holds-against (cluster "not json") check)) "an annotation that does not parse")
    (is (false? (holds-against (cluster "[\"*\"]") check)) "every instance is fenced")
    (is (false? (holds-against (cluster "[\"pg-2\"]") check)))
    (is (true? (holds-against (cluster "[\"pg-1\"]") check)))
    (is (true? (holds-against (cluster nil) check)) "no annotation, no fence")))

(deftest a-statefulset-that-is-gone-has-no-pods-running
  (let [[_ _ running] (storage/statefulset-checks "ctx" "cluster" "db" "pg" "data" "20Gi" [{:pvc "data-pg-0"}])]
    (is (false? (holds-against nil running)))
    (is (true? (holds-against {:metadata {:name "pg"} :spec {:replicas 2} :status {:readyReplicas 2}} running)))))

(deftest a-deletion-holds-only-on-a-read-that-answered-not-found
  (testing "orders/recreate: an order the read did not find is confirmed gone even outside eventually"
    (let [[check] ((book-fn 'runbooks.kubernetes.orders.recreate/cleared-checks) "ctx" ["web/order-1"])]
      (with-redefs [shell/execute unanswered]
        (is (false? ((:check check))) "a lax read that did not answer"))
      (with-redefs [shell/execute (answering nil)]
        (is (true? ((:check check)))))))
  (testing "velero delete_backup: the Backup is gone"
    (let [[check] ((:verify velero-books/delete-backup)
                   {:context "ctx" :target "velero/daily-1" :uid "uid-1"
                    :ref {:type "backups.velero.io" :namespace "velero" :name "daily-1"}})]
      (with-redefs [shell/execute unanswered]
        (is (false? ((:check check))) "a lax read that did not answer"))
      (is (false? (holds-against {:metadata {:name "daily-1"}} check)))
      (is (true? (holds-against nil check))))))

(deftest no-crash-left-needs-ceph-to-answer
  (let [connection {:host "mgr-1"}
        check-of (fn [] (let [{:keys [ok? doc]} (ceph/ceph-json-result connection "crash" "ls-new")]
                          (and ok? (empty? doc))))]
    (with-redefs [shell/execute (fn [& argv]
                                  (is (= ["ceph" "crash" "ls-new" "--format" "json"] (take-last 5 argv)))
                                  {:exit 255 :out "" :err "ssh: connect to host mgr-1: Connection refused"})]
      (is (= {:ok? false :doc nil} (ceph/ceph-json-result connection "crash" "ls-new")))
      (is (false? (check-of))))
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out "[]" :err ""})]
      (is (= {:ok? true :doc []} (ceph/ceph-json-result connection "crash" "ls-new")))
      (is (true? (check-of))))
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out "[{\"crash_id\":\"c1\"}]" :err ""})]
      (is (false? (check-of))))))

(deftest every-library-post-check-fails-closed
  (let [ctx {:context "ctx" :timestamp "2026-11-25T00:00:00Z"}
        kustomization (constantly {:kind "Kustomization" :type "kustomizations.kustomize.toolkit.fluxcd.io"
                                   :namespace "flux-system" :name "apps"})
        flux-spec {:object kustomization :none "none" :timeout 30 :interval 5}]
    (doseq [[label checks]
            {"nodes/cordon" [(nodes/unschedulable-check "ctx" "n1")]
             "nodes/uncordon" [(nodes/schedulable-check "ctx" "n1")]
             "nodes/drain" (nodes/drain-checks "ctx" "n1")
             "objects/delete" (objects/gone-checks "ctx" [{:type "secrets" :namespace "web" :name "s"}] 30)
             "storage/statefulset-checks" (storage/statefulset-checks "ctx" "cluster" "db" "pg" "data" "20Gi" [{:pvc "data-pg-0"}])
             "prometheus/resize-checks" (prometheus/resize-checks "ctx" "cluster" "monitoring" "k8s" "50Gi" [{:pvc "prometheus-k8s-db-0"}])
             "flux/suspend" ((:verify (flux/suspend flux-spec)) ctx)
             "flux/resume" ((:verify (flux/resume flux-spec)) ctx)
             "flux/reconcile" ((:verify (flux/reconcile flux-spec)) ctx)
             "jaas/suspend" [((book-fn 'runbooks.jaas.suspend/suspended-check) "ctx" "web" "site" "web/site")]
             "jaas/resume" [((book-fn 'runbooks.jaas.resume/resumed-check) "ctx" "web" "site" "web/site")]
             "nodes/remove-taint" [((book-fn 'runbooks.kubernetes.nodes.remove-taint/gone-check) "ctx" "n1" "k=v:NoSchedule")]}]
      (testing label
        (is (seq checks))
        (is (= [] (verify/fail-closed-findings checks)))))))
