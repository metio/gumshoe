;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pick-books-test
  "Characterises the books that let the operator pick their subject from a
   kubectl read before they read, exec or change anything: the etcd books,
   list-taints, netshoot run, the db-operator and cnpg consoles, flux drift
   and the Prometheus metric delete. Each book runs its whole :action against
   a recorded cluster, with gumshoe.shell/execute answering for kubectl, the
   fuzzy finder answering as an operator who picks nothing, streamed commands
   recorded instead of run, and flow/change! captured.

   Every book runs four scenarios: its flags name a candidate (:picked), its
   flags name none and the operator picks nothing (:unpicked), the cluster
   holds no candidate (:empty), and the cluster does not answer (:unanswered).
   A transcript keeps the result, what was printed, the commands the book
   streamed or ran besides its reads, and the confirmation it asked for. The
   transcripts and each book's surface are kept in tests/golden/pick_books.edn.

   Regenerate the file after an intended change with
   (gumshoe.pick-books-test/write-golden!) and review its diff."
  (:require [babashka.cli :as cli]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [gumshoe.announce :as announce]
            [gumshoe.flow :as flow]
            [gumshoe.streams-support :as streams]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(def golden-path "tests/golden/pick_books.edn")

;; --- the recorded cluster -------------------------------------------------------------

(def ^:private unanswered
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"})

(defn- flag-value
  [argv flag]
  (some #(when (str/starts-with? % flag) (subs % (count flag))) argv))

(defn- answer-get
  "A kubectl get: the objects of the type, or the one object a name asks for."
  [objects argv]
  (let [[type maybe-name] (rest (drop-while #(not= "get" %) argv))
        items (get objects type [])
        namespace (flag-value argv "--namespace=")]
    (if (and maybe-name (not (str/starts-with? maybe-name "-")))
      (if-let [object (first (filter #(and (= maybe-name (get-in % [:metadata :name]))
                                           (or (nil? namespace)
                                               (= namespace (get-in % [:metadata :namespace]))))
                                     items))]
        {:exit 0 :out (json/generate-string object) :err ""}
        {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" type maybe-name)})
      {:exit 0 :out (json/generate-string {:items items}) :err ""})))

(defn- answer
  [{:keys [objects exec answering?]} commands argv]
  (cond
    (and (= "kubectl" (first argv)) (not answering?)) unanswered
    (and (= "kubectl" (first argv)) (some #{"get"} argv) (not-any? #{"exec"} argv)) (answer-get objects argv)
    :else (do (swap! commands conj argv)
              {:exit 0 :out (or exec "") :err ""})))

;; --- running a book ------------------------------------------------------------------

(defn- load-config
  [path]
  (binding [runbook/*loading* true]
    (load-file path)))

(defn- capture
  [thunk]
  (let [{:keys [result err out]} (streams/separated thunk)
        lines #(str/split-lines (stdout/strip-colors (str/trim %)))]
    (streams/with-stdout {:result result :out (lines err)} out lines)))

(defn- attempt
  [f]
  (try (f) (catch Exception e (str "threw: " (ex-message e)))))

(defn- printable
  "x with every function replaced by :fn, so a transcript prints the same on
   every run."
  [x]
  (walk/postwalk #(if (fn? %) :fn %) x))

(defn transcript
  "What the book does against state {:objects .. :exec .. :answering?} with
   opts."
  [config {:keys [state opts]}]
  (let [state (merge {:answering? true} state)
        commands (atom [])
        seen (atom nil)]
    (with-redefs [shell/execute (fn [& argv] (answer state commands (vec argv)))
                  shell/execute-with-stdin (fn [_ & argv] (answer state commands (vec argv)))
                  shell/run-with-output (fn [& argv] (swap! commands conj (vec argv)) 0)
                  shell/run-attributed (fn [& argv] (swap! commands conj (vec argv)) 0)
                  kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  kubectl/with-port-forward (fn [spec f] (swap! commands conj [:port-forward spec]) (f))
                  fuzzy/select-single (constantly nil)
                  fuzzy/select-multi (constantly nil)
                  announce/announce! (fn [system _ message] [system message])
                  flow/change! (fn [config] (reset! seen config) true)]
      (let [{:keys [result out stdout]} (capture #(attempt (fn [] ((:action config) opts {:announcement-data nil}))))
            {:keys [confirmation effect announce!]} @seen]
        (cond-> {:result result :out out :commands @commands}
          stdout (assoc :stdout stdout)
          @seen (assoc :change (printable
                                (cond-> {:action (:action confirmation)
                                         :target (:target confirmation)
                                         :items (vec (:items confirmation))}
                                  effect (assoc :plan (vec effect))
                                  announce! (assoc :announce (announce!))))))))))

(defn surface
  "What the book declares outside a run, and the opts its options parse argv
   into."
  [config argv]
  (printable
   {:description (:description config)
    :options (:options config)
    :prerequisites (:prerequisites config)
    :announce? (get config :announce? true)
    :parsed (when argv (attempt #(cli/parse-opts argv {:spec (:options config)})))}))

;; --- the books and their scenarios ---------------------------------------------------

(defn- object
  ([name] {:metadata {:name name}})
  ([namespace name] {:metadata {:namespace namespace :name name}}))

(defn- scenarios
  "The four scenarios over objects, with opts that name a candidate and opts
   that name none."
  [objects picked unpicked & [exec]]
  (let [empty-objects (update-vals objects (constantly []))]
    {:picked {:state {:objects objects :exec exec} :opts picked}
     :unpicked {:state {:objects objects :exec exec} :opts unpicked}
     :empty {:state {:objects empty-objects :exec exec} :opts picked}
     :unanswered {:state {:objects objects :exec exec :answering? false} :opts picked}}))

(def ^:private etcd-pods
  {"pods" [(object "kube-system" "etcd-cp-1") (object "kube-system" "etcd-cp-2")]})

(def ^:private etcd-picked {:namespace "kube-system" :pod "etcd-cp-2"})
(def ^:private etcd-unpicked {:namespace "kube-system" :pod "etcd-cp-9"})

(defn- tainted-node
  [name & taints]
  (assoc (object name) :spec {:taints (vec taints)}))

(def ^:private helm-release
  (assoc (object "shop" "web")
         :spec {:releaseName "web"}
         :status {:conditions [{:type "Drifted" :status "True" :message "Deployment/shop/web changed"}]}))

(def ^:private prometheus-service
  (assoc (object "monitoring" "prometheus")
         :spec {:selector {:app.kubernetes.io/name "prometheus"}
                :ports [{:name "http-web" :port 9090 :targetPort "web"}]}))

(defn- prometheus-pod
  [name]
  (-> (object "monitoring" name)
      (assoc-in [:metadata :labels] {:app.kubernetes.io/name "prometheus" :app.kubernetes.io/instance "k8s"})
      (assoc :spec {:containers [{:name "prometheus" :ports [{:name "web" :containerPort 9090}]}]}
             :status {:phase "Running" :conditions [{:type "Ready" :status "True"}]})))

(def books
  {"runbooks/etcd/check_performance.clj"
   {:argv ["--namespace" "kube-system" "--pod" "etcd-cp-2" "--workload" "m"]
    :scenarios (scenarios etcd-pods (assoc etcd-picked :workload "m") (assoc etcd-unpicked :workload "m"))}

   "runbooks/etcd/endpoint_health.clj"
   {:argv ["-n" "kube-system" "-p" "etcd-cp-2"]
    :scenarios (scenarios etcd-pods etcd-picked etcd-unpicked)}

   "runbooks/etcd/endpoint_status.clj"
   {:argv ["-n" "kube-system" "-p" "etcd-cp-2"]
    :scenarios (scenarios etcd-pods etcd-picked etcd-unpicked)}

   "runbooks/etcd/member_list.clj"
   {:argv ["-n" "kube-system" "-p" "etcd-cp-2"]
    :scenarios (scenarios etcd-pods etcd-picked etcd-unpicked)}

   "runbooks/etcd/get_value.clj"
   {:argv ["-n" "kube-system" "-p" "etcd-cp-2" "-k" "/registry/b"]
    :scenarios (assoc (scenarios etcd-pods (assoc etcd-picked :key "/registry/b") etcd-unpicked
                                 "/registry/a\n/registry/b\n")
                      :unpicked-key {:state {:objects etcd-pods :exec "/registry/a\n/registry/b\n"}
                                     :opts (assoc etcd-picked :key "/registry/z")})}

   "runbooks/kubernetes/nodes/list_taints.clj"
   {:argv ["--node" "n2"]
    :scenarios (assoc (scenarios {"nodes" [(tainted-node "n1")
                                           (tainted-node "n2" {:key "dedicated" :value "db" :effect "NoSchedule"})]}
                                 {:node "n2"} {:node "n9"})
                      :untainted {:state {:objects {"nodes" [(tainted-node "n1")]}} :opts {:node "n1"}})}

   "runbooks/kubernetes/netshoot/run.clj"
   {:argv ["--namespace" "shop" "--host-network"]
    :scenarios (scenarios {"namespaces" [(object "shop") (object "db")]}
                          {:namespace "shop" :host-network true} {:namespace "nowhere"})}

   "tools/db-operator/runbooks/admin_connect.clj"
   {:argv ["--instance" "pg-main"]
    :scenarios (scenarios {"dbinstances.kinda.rocks" [(object "pg-main") (object "pg-other")]}
                          {:instance "pg-main" :port 42538} {:instance "pg-none" :port 42538})}

   "tools/cnpg/runbooks/cnpg_console.clj"
   {:argv ["--namespace" "db" "--cluster" "pg"]
    :scenarios (scenarios {"clusters.postgresql.cnpg.io" [(assoc (object "db" "pg") :status {:currentPrimary "pg-1"})]
                           "pods" []}
                          {:namespace "db" :cluster "pg"} {:namespace "db" :cluster "nope"})}

   "tools/cnpg/runbooks/pooler_console.clj"
   {:argv ["--namespace" "db" "--pooler" "pg-rw"]
    :scenarios (scenarios {"poolers.postgresql.cnpg.io" [(object "db" "pg-rw")]
                           "pods" []}
                          {:namespace "db" :pooler "pg-rw" :lines 200} {:namespace "db" :pooler "nope" :lines 200})}

   "tools/cnpg/runbooks/verifier_churn.clj"
   {:argv ["--namespace" "db" "--cluster" "pg"]
    :scenarios (scenarios {"clusters.postgresql.cnpg.io" [(assoc (object "db" "pg") :status {:currentPrimary "pg-1"})]}
                          {:namespace "db" :cluster "pg" :window 1} {:namespace "db" :cluster "nope" :window 1})}

   "tools/flux/runbooks/flux/drift.clj"
   {:argv ["--namespace" "shop" "--name" "web"]
    :scenarios (scenarios {"helmreleases.helm.toolkit.fluxcd.io" [helm-release]}
                          {:namespace "shop" :name "web"} {:namespace "shop" :name "nope"})}

   "tools/prometheus/runbooks/delete_metric.clj"
   {:argv ["--metric" "up" "--namespace" "monitoring" "--service" "prometheus"]
    :scenarios (scenarios {"services" [prometheus-service]
                           "prometheuses.monitoring.coreos.com" [(object "monitoring" "k8s")]
                           "pods" [(prometheus-pod "prometheus-k8s-0") (prometheus-pod "prometheus-k8s-1")]}
                          {:metric "up" :namespace "monitoring" :service "prometheus"
                           :selector "app.kubernetes.io/name=prometheus" :port-name "http-web" :port 19090}
                          {:metric "up" :namespace "monitoring" :service "nope"
                           :selector "app.kubernetes.io/name=prometheus" :port-name "http-web" :port 19090})}})

(defn transcripts
  []
  (into (sorted-map)
        (for [[path {:keys [argv scenarios]}] books]
          (let [config (load-config path)]
            [path (assoc (into (sorted-map) (for [[k scenario] scenarios] [k (transcript config scenario)]))
                         :surface (surface config argv))]))))

(defn write-golden!
  []
  (spit golden-path
        ;; split, so a REUSE scan does not read these strings as this file's header
        (str ";; SPDX" "-FileCopyrightText: The gumshoe Authors\n"
             ";; SPDX" "-License-Identifier: 0BSD\n"
             ";; Generated by (gumshoe.pick-books-test/write-golden!); review its diff.\n"
             (with-out-str (pprint/pprint (transcripts))))))

(deftest every-book-keeps-its-transcripts
  (let [golden (edn/read-string (slurp golden-path))
        actual (transcripts)]
    (is (= (set (keys golden)) (set (keys actual))) "the same books")
    (doseq [[path scenarios] actual
            [scenario transcript] scenarios]
      (testing (str path " " scenario)
        (is (= (get-in golden [path scenario]) transcript))))))
