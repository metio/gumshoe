;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.plan-check-test
  "gumshoe.testing.plan holds a spec's dry-run plan to its effect: it passes a
   spec whose :items list every object its effect touches, computed from
   the context its :derive phase read once, and fails a spec whose :items
   or :effect read the cluster themselves, whose :items leave an object
   out, or whose effect it cannot identify. The engine refuses the same
   reads at run time."
  (:require [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :as test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.testing.plan :as plan]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

;; --- what an effect touches ----------------------------------------------------------

(defn- refs [effect & [touches]] (:refs (plan/touched effect touches)))

(defn- unidentified? [effect] (contains? (plan/touched effect) :unidentified))

(deftest kubectl-effects-touch-the-objects-their-arguments-name
  (is (= [{:type "secrets" :namespace "shop" :name "a"} {:type "secrets" :namespace "shop" :name "b"}]
         (refs (effect/kubectl "ctx" "-n" "shop" "delete" "secrets" "a" "b" "--ignore-not-found" "--wait=false"))))
  (is (= [{:type "applications.argoproj.io" :namespace "argocd" :name "site"}]
         (refs (effect/kubectl "ctx" "--namespace=argocd" "annotate" "applications.argoproj.io" "site"
                               "--overwrite" "argocd.argoproj.io/refresh=normal"))))
  (is (= [{:type "deployment" :namespace "shop" :name "web"}]
         (refs (effect/kubectl "ctx" "rollout" "restart" "deployment/web" "-n" "shop"))))
  (is (= [{:type "cronjobs" :namespace "batch" :name "nightly"}]
         (refs (effect/kubectl "ctx" "--namespace=batch" "patch" "cronjobs" "nightly" "-p" "{\"spec\":{\"suspend\":true}}"))))
  (is (= [{:type "job" :namespace "batch" :name "nightly-x"}]
         (refs (effect/kubectl "ctx" "--namespace=batch" "create" "job" "nightly-x" "--from" "cronjob/nightly"))))
  (is (= [{:type "deployments" :namespace "shop" :name "web"}]
         (refs (effect/kubectl "ctx" "--namespace=shop" "scale" "deployments/web" "--replicas=3")))))

(deftest node-verbs-touch-nodes
  (is (= [{:type "nodes" :namespace nil :name "n1"}] (refs (effect/kubectl "ctx" "cordon" "n1"))))
  (is (= [{:type "nodes" :namespace nil :name "n1"}]
         (refs (effect/kubectl "ctx" "drain" "n1" "--ignore-daemonsets" "--grace-period" "-1"))))
  (is (= [{:type "nodes" :namespace nil :name "n1"}]
         (refs (effect/kubectl "ctx" "taint" "nodes" "n1" "dedicated=db:NoSchedule-")))))

(deftest exec-touches-its-pod-and-not-the-command-after-the-dashes
  (is (= [{:type "pods" :namespace "db" :name "pg-1"}]
         (refs (effect/kubectl "ctx" "-n" "db" "exec" "pg-1" "--" "psql" "-c" "select 1")))))

(deftest a-raw-api-path-touches-the-object-it-names
  (is (= {:type "namespaces" :namespace nil :name "stuck"} (plan/raw-path-ref "/api/v1/namespaces/stuck/finalize")))
  (is (= {:type "deployments" :namespace "shop" :name "web"} (plan/raw-path-ref "/apis/apps/v1/namespaces/shop/deployments/web/scale")))
  (is (= {:type "nodes" :namespace nil :name "n1"} (plan/raw-path-ref "/api/v1/nodes/n1")))
  (is (nil? (plan/raw-path-ref "/healthz")))
  (is (= [{:type "namespaces" :namespace nil :name "stuck"}]
         (refs (effect/kubectl-stdin "ctx" "{}" "replace" "--raw" "/api/v1/namespaces/stuck/finalize" "-f" "-")))))

(deftest a-manifest-on-stdin-touches-the-objects-it-holds
  (is (= [{:type "Backup" :namespace "db" :name "pg-now"}]
         (refs (effect/kubectl-stdin "ctx" (json/generate-string {:kind "Backup" :metadata {:namespace "db" :name "pg-now"}})
                                     "create" "-f" "-"))))
  (is (= [{:type "ConfigMap" :namespace "web" :name "a"} {:type "Secret" :namespace "other" :name "b"}]
         (refs (effect/kubectl-stdin "ctx" "kind: ConfigMap\nmetadata:\n  name: a\n---\nkind: Secret\nmetadata:\n  name: b\n  namespace: other\n"
                                     "-n" "web" "apply" "-f" "-"))))
  (is (= [{:type "ConfigMap" :namespace "web" :name "a"}]
         (refs (effect/kubectl-stdin "ctx" (json/generate-string {:kind "List" :items [{:kind "ConfigMap" :metadata {:namespace "web" :name "a"}}]})
                                     "apply" "-f" "-")))))

(deftest tools-gumshoe-books-call-touch-what-they-name
  (is (= [{:type "helmrelease" :namespace "apps" :name "podinfo"}]
         (refs (effect/cmd "flux" "reconcile" "helmrelease" "podinfo" "--context=ctx" "--namespace=apps"))))
  (is (= [{:type "git" :namespace "flux-system" :name "repo"}]
         (refs (effect/cmd "flux" "reconcile" "source" "git" "repo" "-n" "flux-system"))))
  (is (= [{:type "helm-release" :namespace "shop" :name "web"}]
         (refs (effect/cmd "helm" "rollback" "web" "1" "--namespace=shop" "--kube-context=ctx"))))
  (is (= [{:type "certificates" :namespace "shop" :name "web-tls"}]
         (refs (effect/cmd "cmctl" "renew" "web-tls" "--context=ctx" "--namespace=shop"))))
  (is (= [{:type "clusters.postgresql.cnpg.io" :namespace "db" :name "pg"}]
         (refs (effect/cmd "kubectl-cnpg" "promote" "pg" "pg-2" "--context=ctx" "--namespace=db"))))
  (is (= [{:type "clusters.postgresql.cnpg.io" :namespace "db" :name "pg"}]
         (refs (effect/kubectl-plugin "ctx" "cnpg" "hibernate" "on" "pg" "--namespace=db"))))
  (is (= [{:type "host" :namespace nil :name "cp-1"}]
         (refs (effect/ssh {:host "cp-1" :user "root"} "sudo" "crictl" "stop" "abc")))))

(deftest a-note-touches-nothing
  (is (= [] (refs (effect/note "nothing to do")))))

(deftest effects-whose-objects-cannot-be-told-are-unidentified
  (testing "a set no argument names"
    (is (unidentified? (effect/kubectl "ctx" "-n" "shop" "delete" "pods" "-l" "app=web")))
    (is (unidentified? (effect/kubectl "ctx" "-n" "shop" "delete" "pods" "--all")))
    (is (unidentified? (effect/kubectl "ctx" "delete" "pods" "--field-selector=status.phase=Failed" "-A")))
    (is (unidentified? (effect/cmd "cmctl" "renew" "--all"))))
  (testing "a file, several types, a generated name, a type without a name"
    (is (unidentified? (effect/kubectl "ctx" "apply" "-f" "manifests/")))
    (is (unidentified? (effect/kubectl "ctx" "delete" "pods,services" "web")))
    (is (unidentified? (effect/kubectl-stdin "ctx" (json/generate-string {:kind "DeleteBackupRequest" :metadata {:generateName "x-"}})
                                             "create" "-f" "-")))
    (is (unidentified? (effect/kubectl "ctx" "delete" "pods"))))
  (testing "programs and verbs it does not know"
    (is (unidentified? (effect/cmd "terraform" "apply")))
    (is (unidentified? (effect/kubectl-plugin "ctx" "cnpg" "psql" "pg")))
    (is (unidentified? [:custom-op "x"]))))

(deftest touches-is-asked-first-and-nil-falls-back
  (let [touches (fn [[op]] (when (= :cmd op) [{:type "host" :name "h"}]))]
    (is (= [{:type "host" :name "h"}] (refs (effect/cmd "terraform" "apply") touches)))
    (is (= [{:type "nodes" :namespace nil :name "n1"}] (refs (effect/kubectl "ctx" "cordon" "n1") touches))))
  (testing "[] declares an effect that touches no object"
    (is (= [] (refs (effect/cmd "terraform" "apply") (constantly []))))))

;; --- what an item names --------------------------------------------------------------

(deftest an-item-names-a-namespaced-object-by-namespace-and-name
  (let [web {:type "deployments" :namespace "shop" :name "web"}]
    (is (plan/names? "Deployment shop/web" web))
    (is (plan/names? "shop/web" web))
    (is (plan/names? "scale web in namespace shop to 3." web))
    (is (plan/names? "deployment/web (shop)" web))
    (testing "negative controls"
      (is (not (plan/names? "shop/webby" web)) "a longer name")
      (is (not (plan/names? "web" web)) "no namespace")
      (is (not (plan/names? "Deployment other/web" web)) "another namespace")
      (is (not (plan/names? "shop/web-1 and shop/web-2" web))))))

(deftest an-item-names-a-cluster-scoped-object-by-name
  (is (plan/names? "n1" {:type "nodes" :name "n1"}))
  (is (plan/names? "cordon node n1" {:type "nodes" :namespace nil :name "n1"}))
  (is (not (plan/names? "n10" {:type "nodes" :name "n1"}))))

(deftest namespace-and-name-must-share-one-item
  (is (not (plan/listed? ["namespace shop" "delete web"] {:type "x" :namespace "shop" :name "web"})))
  (is (plan/listed? ["namespace shop" "delete shop/web"] {:type "x" :namespace "shop" :name "web"})))

(deftest coverage-lists-each-touched-object-once-and-the-missing-ones
  (let [plan [(effect/kubectl "ctx" "-n" "shop" "annotate" "secrets" "a" "k=v")
              (effect/kubectl "ctx" "-n" "shop" "label" "secrets" "a" "k=v")
              (effect/kubectl "ctx" "-n" "shop" "delete" "secrets" "b")
              (effect/note "done")]]
    (is (= {:touched [{:type "secrets" :namespace "shop" :name "a"} {:type "secrets" :namespace "shop" :name "b"}]
            :missing [{:type "secrets" :namespace "shop" :name "b"}]
            :unidentified []}
           (plan/coverage ["stamp shop/a"] plan nil)))))

;; --- a spec over a recorded cluster --------------------------------------------------

(defn- secret [name] {:metadata {:namespace "shop" :name name :uid (str name "-uid")}})

(defn- recorded
  "A shell/execute that answers a list of the shop namespace's secrets from
   the answers in turn - the last one for every read after it - and counts
   the lists it served in reads."
  [answers reads]
  (fn [& argv]
    (let [n (swap! reads inc)
          names (nth answers (min (dec n) (dec (count answers))))]
      (if (= :unanswered names)
        {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"}
        {:exit 0 :out (json/generate-string {:items (map secret names)}) :err ""}))))

(defn- secrets-in [context] (kubectl/names-of (kubectl/get-namespaced context "shop" "secrets")))

(def ^:private base
  {:description "Deletes the secrets of a namespace"
   :select {:mode :one :label "Namespace" :candidates (constantly ["shop"])}
   :confirm {:action "delete the secrets of a namespace" :destructive? true}
   :prerequisites {}
   :no-guard "a test"
   :no-verify "a test"})

(def ^:private consistent
  (merge base
         {:reads {:secrets (fn [{:keys [context]}] (secrets-in context))}
          :items (fn [{:keys [secrets]}] (map #(str "delete Secret shop/" %) secrets))
          :effect (fn [{:keys [context secrets]}]
                    (apply effect/plan (for [s secrets] (effect/kubectl context "-n" "shop" "delete" "secrets" s))))}))

(defn- check
  [spec answers & [scenario]]
  (let [reads (atom 0)]
    (with-redefs [shell/execute (recorded answers reads)]
      (assoc (plan/check spec (merge {:target "shop"} scenario)) :reads @reads))))

(deftest a-plan-computed-from-the-derived-context-passes
  (let [{:keys [ok? items plan touched missing reads]} (check consistent [["a" "b"]])]
    (is ok?)
    (is (= ["delete Secret shop/a" "delete Secret shop/b"] items))
    (is (= 2 (count plan)))
    (is (= 2 (count touched)))
    (is (empty? missing))
    (is (= 1 reads) "the :derive phase read once, and :items and :effect read nothing")))

(deftest an-items-that-re-reads-the-cluster-fails-and-reads-nothing
  (let [spec (assoc consistent :items (fn [{:keys [context]}]
                                        (map #(str "delete Secret shop/" %) (secrets-in context))))
        {:keys [ok? problem failure reads]} (check spec [["a" "b"] ["a"]])]
    (is (not ok?))
    (is (str/includes? problem "its :items reached outside the process"))
    (is (str/includes? problem "get secrets"))
    (is (= [problem] failure))
    (is (= 1 reads) "the second read, which would have answered differently, never ran")))

(deftest an-effect-that-re-reads-the-cluster-fails
  (let [spec (assoc consistent :effect (fn [{:keys [context]}]
                                         (apply effect/plan (for [s (secrets-in context)]
                                                              (effect/kubectl context "-n" "shop" "delete" "secrets" s)))))
        {:keys [ok? problem reads]} (check spec [["a"] ["a" "c"]])]
    (is (not ok?))
    (is (str/includes? problem "its :effect reached outside the process"))
    (is (= 1 reads))))

(deftest an-items-that-omits-an-object-the-effect-touches-fails-and-names-it
  (let [spec (assoc consistent :items (fn [{:keys [secrets]}] [(str "delete Secret shop/" (first secrets))]))
        {:keys [ok? missing failure]} (check spec [["a" "b" "c"]])]
    (is (not ok?))
    (is (= [{:type "secrets" :namespace "shop" :name "b"} {:type "secrets" :namespace "shop" :name "c"}] missing))
    (is (= ["the effect plan touches objects the dry-run plan (:items) does not list:"
            "  secrets shop/b"
            "  secrets shop/c"]
           failure))))

(deftest the-default-items-must-cover-the-effect-too
  (testing "without :items the confirmation lists the pick, which names no secret"
    (let [{:keys [ok? missing]} (check (dissoc consistent :items) [["a"]])]
      (is (not ok?))
      (is (= [{:type "secrets" :namespace "shop" :name "a"}] missing)))))

(deftest an-effect-it-cannot-identify-fails-until-touches-names-it
  (let [spec (assoc consistent :effect (fn [{:keys [secrets]}]
                                         (apply effect/plan (for [s secrets] (effect/cmd "rotate-secret" "shop" s)))))]
    (let [{:keys [ok? unidentified failure]} (check spec [["a"]])]
      (is (not ok?))
      (is (= [(effect/cmd "rotate-secret" "shop" "a")] unidentified))
      (is (= ["the effect plan holds effects whose objects cannot be told - name them with :touches:"
              "  rotate-secret shop a"]
             failure)))
    (testing "with :touches it is compared like any other"
      (let [touches (fn [[op _ ns name]] (when (= :cmd op) [{:type "secrets" :namespace ns :name name}]))]
        (is (:ok? (check spec [["a"]] {:touches touches})))
        (is (= [{:type "secrets" :namespace "shop" :name "b"}]
               (:missing (check (assoc spec :items (constantly ["shop/a"])) [["a" "b"]] {:touches touches}))))))))

(deftest a-derive-phase-that-did-not-answer-fails-the-check
  (let [{:keys [ok? problem reads]} (check consistent [:unanswered])]
    (is (not ok?))
    (is (str/includes? problem "a read of the :derive phase did not answer"))
    (is (= 1 reads))))

(deftest a-refused-pick-fails-the-check
  (let [spec (assoc consistent :subject {:type "namespaces" :parse (constantly nil) :invalid "not a namespace"})
        {:keys [ok? problem]} (check spec [["a"]])]
    (is (not ok?))
    (is (str/includes? problem "not a namespace"))))

(deftest an-effect-that-is-not-a-plan-fails-the-check
  (let [{:keys [ok? problem]} (check (assoc consistent :effect (constantly {:not "a plan"})) [["a"]])]
    (is (not ok?))
    (is (str/includes? problem "not a plan"))))

(deftest covers-reports-one-assertion-listing-the-missing-objects
  (let [reports (atom [])
        spec (assoc consistent :items (constantly ["delete Secret shop/a"]))]
    (binding [test/report (fn [m] (swap! reports conj m))]
      (with-redefs [shell/execute (recorded [["a" "b"]] (atom 0))]
        (plan/covers! spec {:target "shop"})
        (plan/covers! consistent {:target "shop"})))
    (let [[failed passed] @reports]
      (is (= :fail (:type failed)))
      (is (str/includes? (:message failed) "secrets shop/b"))
      (is (not (str/includes? (:message failed) "secrets shop/a")))
      (is (= :pass (:type passed))))))

;; --- the engine at run time ----------------------------------------------------------

(defn- run
  "[result stderr launched changes] of spec run on shop through run-on!,
   with the real shell (babashka.process stubbed, recording every launch)
   or a recorded shell/execute, flow/change! captured."
  [spec & {:keys [dry-run?]}]
  (let [launched (atom [])
        changes (atom [])
        err (java.io.StringWriter.)]
    (with-redefs [process/process (fn [& args] (swap! launched conj (vec args)) (throw (ex-info "stubbed" {})))
                  kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (constantly true)
                  flow/change! (fn [config] (swap! changes conj config) true)]
      (let [result (binding [*err* err flow/*dry-run* (boolean dry-run?)]
                     (mutation/run-on! spec "shop" {} nil))]
        [result (stdout/strip-colors (str err)) @launched @changes]))))

(def ^:private reading-items
  (merge base
         {:items (fn [{:keys [context]}] (secrets-in context))
          :effect (fn [{:keys [context]}] (effect/plan (effect/kubectl context "-n" "shop" "delete" "secrets" "a")))}))

(deftest the-engine-refuses-an-items-that-reads-before-anything-starts
  (doseq [dry-run? [false true]]
    (testing (if dry-run? "a rehearsal" "a run")
      (let [[result err launched changes] (run reading-items :dry-run? dry-run?)]
        (is (false? result))
        (is (str/includes? err "refusing: its :items reached outside the process: not running 'kubectl"))
        (is (str/includes? err "move the read into :reads, :values or :derive"))
        (is (empty? launched) "no kubectl was started")
        (is (empty? changes) "nothing was confirmed or applied")))))

(deftest the-engine-refuses-an-effect-that-reads
  (let [spec (assoc base :effect (fn [{:keys [context]}]
                                   (apply effect/plan (for [s (secrets-in context)]
                                                        (effect/kubectl context "-n" "shop" "delete" "secrets" s)))))
        [result err launched changes] (run spec)]
    (is (false? result))
    (is (str/includes? err "refusing: its :effect reached outside the process"))
    (is (empty? launched))
    (is (empty? changes))))

(deftest the-engine-hands-flow-the-plan-a-reading-free-spec-computed
  (let [spec (merge base {:items (constantly ["delete Secret shop/a"])
                          :effect (fn [{:keys [context]}] (effect/plan (effect/kubectl context "-n" "shop" "delete" "secrets" "a")))})
        [result _ launched [change]] (run spec)]
    (is (true? result))
    (is (empty? launched))
    (is (= ["delete Secret shop/a"] (get-in change [:confirmation :items])))
    (is (= [[:kubectl "ctx" "-n" "shop" "delete" "secrets" "a"]] (:effect change)))))

(deftest planned-is-the-plan-the-engine-shows
  (let [spec (change/normalize (merge base {:effect (fn [{:keys [target]}] (effect/plan (effect/kubectl "ctx" "label" "namespaces" target "x=y")))}))]
    (is (= {:items ["shop"] :effect [[:kubectl "ctx" "label" "namespaces" "shop" "x=y"]]}
           (mutation/planned spec {:target "shop"} "shop")))))
