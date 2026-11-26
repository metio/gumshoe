;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pod-containers-test
  "Every container of a pod counts - init containers, native sidecars and
   ephemeral containers alongside the regular ones - and a rollout whose
   strategy keeps it from completing is refused before it starts."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.workloads :as workloads]))

;; --- rollouts that never complete -------------------------------------------------

(defn- with-strategy
  [kind strategy]
  (case kind
    "DaemonSet" {:spec {:updateStrategy strategy} :status {:desiredNumberScheduled 3}}
    {:spec {:replicas 3 :updateStrategy strategy}}))

(deftest a-restart-is-refused-for-every-kind-that-updates-on-delete
  (doseq [kind ["StatefulSet" "DaemonSet"]]
    (testing kind
      (is (= (format "the %s updates OnDelete, so a restart replaces no pod until each is deleted by hand" kind)
             (workloads/restart-refusal kind (with-strategy kind {:type "OnDelete"})))))))

(deftest a-restart-is-refused-for-a-partitioned-statefulset
  (is (= "the StatefulSet's rollingUpdate.partition is 2, so pods below it stay on their revision and the restart never completes"
         (workloads/restart-refusal "StatefulSet" (with-strategy "StatefulSet" {:type "RollingUpdate" :rollingUpdate {:partition 2}}))))
  (is (nil? (workloads/restart-refusal "StatefulSet" (with-strategy "StatefulSet" {:type "RollingUpdate" :rollingUpdate {:partition 0}})))))

(deftest a-rolling-update-is-restarted
  (doseq [kind ["StatefulSet" "DaemonSet" "Deployment"]]
    (is (nil? (workloads/restart-refusal kind (with-strategy kind {:type "RollingUpdate"}))) kind)))

(deftest restart-and-rollback-refuse-the-same-strategies
  (doseq [kind ["StatefulSet" "DaemonSet"]
          strategy [{:type "OnDelete"} {:type "RollingUpdate" :rollingUpdate {:partition 1}} {:type "RollingUpdate"}]
          :let [workload (with-strategy kind strategy)
                restart (workloads/restart-refusal kind workload)
                rollback (workloads/rollback-refusal kind workload [] 1)
                blocker? #(boolean (some-> % (str/includes? "never completes")))
                on-delete? #(boolean (some-> % (str/includes? "OnDelete")))]]
    (testing [kind strategy]
      (is (= (blocker? restart) (blocker? rollback)))
      (is (= (on-delete? restart) (on-delete? rollback))))))

;; --- images -----------------------------------------------------------------------

(def ^:private pod
  {:metadata {:namespace "shop" :name "web-1"}
   :spec {:containers [{:name "web" :image "app:1"
                        :resources {:requests {:cpu "100m" :memory "64Mi"} :limits {:memory "128Mi"}}}]
          :initContainers [{:name "proxy" :image "istio/proxy:1" :restartPolicy "Always"
                            :resources {:requests {:cpu "50m"}}}
                           {:name "migrate" :image "busybox:1"}]
          :ephemeralContainers [{:name "debugger" :image "nicolaka/netshoot:1"}]}})

(deftest every-image-a-pod-runs-is-counted
  (is (= {"app:1" 1 "istio/proxy:1" 1 "busybox:1" 1 "nicolaka/netshoot:1" 1}
         (kubectl/image-counts {:items [pod]})))
  (is (= {"app:1" 2 "istio/proxy:1" 2 "busybox:1" 2 "nicolaka/netshoot:1" 2}
         (kubectl/image-counts {:items [pod pod]}))))

(deftest image-usage-names-the-role-of-each-image
  (is (= {"app:1" #{:container} "istio/proxy:1" #{:sidecar} "busybox:1" #{:init} "nicolaka/netshoot:1" #{:ephemeral}}
         (into {} (map (juxt :image :roles)) (kubectl/image-usage {:items [pod]})))))

(deftest every-container-that-declares-resources-gets-a-row
  (is (= [{:namespace "shop" :pod "web-1" :container "web" :role :container
           :memory-request "64Mi" :memory-limit "128Mi" :cpu-request "100m" :cpu-limit nil}
          {:namespace "shop" :pod "web-1" :container "proxy" :role :sidecar
           :memory-request nil :memory-limit nil :cpu-request "50m" :cpu-limit nil}
          {:namespace "shop" :pod "web-1" :container "migrate" :role :init
           :memory-request nil :memory-limit nil :cpu-request nil :cpu-limit nil}]
         (kubectl/container-resource-rows {:items [pod]}))))

;; --- the list books ---------------------------------------------------------------

(defn- run-book
  "{:result .. :out .. :commands} of the book at path against kubectl answering
   through execute."
  [path execute]
  (let [config (binding [runbook/*loading* true] (load-file path))
        commands (atom [])
        out (java.io.StringWriter.)]
    (with-redefs [shell/execute (fn [& argv] (swap! commands conj (vec argv)) (apply execute argv))
                  shell/run-with-output (fn [& argv] (swap! commands conj (vec argv)) 0)
                  kubectl/current-context (constantly "ctx")]
      (let [result (binding [*out* out *err* out] ((:action config) {} {}))]
        {:result result :out (stdout/strip-colors (str out)) :commands @commands}))))

(defn- answering [& _] {:exit 0 :out (json/generate-string {:items [pod]}) :err ""})

(deftest list-images-marks-images-outside-the-regular-containers
  (let [{:keys [result out]} (run-book "runbooks/kubernetes/pods/list_images.clj" answering)]
    (is (true? result))
    (is (re-find #"(?m)^\s+1 app:1$" out))
    (is (re-find #"(?m)^\s+1 istio/proxy:1 \(sidecar\)$" out))
    (is (re-find #"(?m)^\s+1 busybox:1 \(init\)$" out))
    (is (re-find #"(?m)^\s+1 nicolaka/netshoot:1 \(ephemeral\)$" out))))

(deftest list-resources-shows-init-containers-and-sidecars
  (let [{:keys [result out commands]} (run-book "runbooks/kubernetes/pods/list_resources.clj" answering)]
    (is (true? result))
    (is (re-find #"(?m)^shop\s+web-1\s+proxy\s+sidecar\s+<none>\s+<none>\s+50m\s+<none>$" out))
    (is (re-find #"(?m)^shop\s+web-1\s+migrate\s+init\s" out))
    (is (not (str/includes? out "debugger")))
    (testing "the read is bounded"
      (is (seq commands))
      (is (every? (fn [argv] (some #(str/starts-with? % "--request-timeout=") argv)) commands)))))

(deftest list-resources-reports-a-read-that-did-not-answer
  (let [{:keys [result out]} (run-book "runbooks/kubernetes/pods/list_resources.clj"
                                       (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"}))]
    (is (false? result))
    (is (str/includes? out "could not read the pods of the cluster"))))
