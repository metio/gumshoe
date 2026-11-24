;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.kubectl-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.shell :as shell]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.select :as select]))

(def nodes
  {:items [{:metadata {:name "worker-1"}
            :spec {:taints [{:key "dedicated" :value "database" :effect "NoSchedule"}
                            {:key "node.kubernetes.io/unreachable" :effect "NoExecute"}]}}
           {:metadata {:name "worker-2"}
            :spec {:unschedulable true}}]})

(def pods
  {:items [{:metadata {:namespace "moodle" :name "web-1"}
            :spec {:volumes [{:persistentVolumeClaim {:claimName "moodle-data"}}
                             {:configMap {:name "config"}}]
                   :containers [{:image "docker.io/library/nginx:1.27"}]}}
           {:metadata {:namespace "keycloak" :name "keycloak-0"}
            :spec {:containers [{:image "quay.io/keycloak/keycloak:26.0"}
                                {:image "docker.io/library/nginx:1.27"}]}}]})

(def pvcs
  {:items [{:metadata {:namespace "moodle" :name "moodle-data"}}
           {:metadata {:namespace "moodle" :name "moodle-backup"}}]})

;; `kubectl api-resources --no-headers` - a core row with a short name, a core
;; row without one, a compound-cased core Kind, and a CRD row carrying its group.
(def api-resources-output
  (str "pods                              po   v1                        true    Pod\n"
       "nodes                                  v1                        false   Node\n"
       "persistentvolumeclaims            pvc  v1                        true    PersistentVolumeClaim\n"
       "helmreleases                      hr   helm.toolkit.fluxcd.io/v2 true    HelmRelease\n"))

(deftest parse-api-resources-test
  (let [kinds (kubectl/parse-api-resources api-resources-output)]
    (testing "the KIND column maps from the bare plural, with the cluster's own casing"
      (is (= "Pod" (get kinds "pods")))
      (is (= "Node" (get kinds "nodes")))
      (is (= "PersistentVolumeClaim" (get kinds "persistentvolumeclaims"))
          "a compound Kind keeps the cluster's casing - no lossy title-casing"))
    (testing "a grouped resource is also keyed by its plural.group form"
      (is (= "HelmRelease" (get kinds "helmreleases")))
      (is (= "HelmRelease" (get kinds "helmreleases.helm.toolkit.fluxcd.io"))))
    (testing "a core (groupless) resource is not given a spurious dotted key"
      (is (nil? (get kinds "pods.v1")))))
  (testing "empty or unreachable output parses to an empty map, never throws"
    (is (= {} (kubectl/parse-api-resources "")))
    (is (= {} (kubectl/parse-api-resources nil)))))

(deftest names-test
  (testing "names-of"
    (is (= ["worker-1" "worker-2"] (kubectl/names-of nodes))))
  (testing "namespaces-names"
    (is (= ["moodle/web-1" "keycloak/keycloak-0"] (kubectl/namespaces-names pods))))
  (testing "namespaces-of"
    (is (= ["moodle" "keycloak"] (kubectl/namespaces-of pods))))
  (testing "plain seqs work like parsed lists"
    (is (= ["worker-1" "worker-2"] (kubectl/names-of (:items nodes))))))

(deftest find-item-test
  (testing "finds namespaced items"
    (is (= "web-1" (kubectl/name-of (kubectl/find-item pods "moodle/web-1")))))
  (testing "finds cluster-scoped items"
    (is (= "worker-2" (kubectl/name-of (kubectl/find-item nodes "worker-2"))))))

(deftest namespace-name-map-test
  (is (= {:namespace "moodle" :name "web-1"} (kubectl/namespace-name-map "moodle/web-1")))
  (testing "only the first slash separates, so a name keeps any further one"
    (is (= {:namespace "moodle" :name "a/b"} (kubectl/namespace-name-map "moodle/a/b"))))
  (testing "the docstring names the map it returns"
    (is (re-find #"\{:namespace .* :name .*\}" (:doc (meta #'kubectl/namespace-name-map))))))

(deftest split-namespace-name-is-a-deprecated-alias
  (is (= (kubectl/namespace-name-map "moodle/web-1") (#'kubectl/split-namespace-name "moodle/web-1")))
  (is (string? (:deprecated (meta #'kubectl/split-namespace-name))))
  (is (re-find #"\{:namespace .* :name .*\}" (:doc (meta #'kubectl/split-namespace-name))))
  (is (re-find #"namespace-name-map" (:doc (meta #'kubectl/split-namespace-name)))))

(deftest taints-test
  (testing "renders key=value:effect"
    (is (= "dedicated=database:NoSchedule"
           (kubectl/taint->str {:key "dedicated" :value "database" :effect "NoSchedule"}))))
  (testing "renders key:effect when there is no value"
    (is (= "node.kubernetes.io/unreachable:NoExecute"
           (kubectl/taint->str {:key "node.kubernetes.io/unreachable" :effect "NoExecute"}))))
  (testing "lists nodes with taints"
    (is (= ["worker-1"] (kubectl/nodes-with-taints nodes))))
  (testing "removal spec drops the value - kubectl removes by key:effect only"
    (is (= "dedicated:NoSchedule" (kubectl/taint-removal-spec "dedicated=database:NoSchedule")))
    (is (= "node.kubernetes.io/unreachable:NoExecute"
           (kubectl/taint-removal-spec "node.kubernetes.io/unreachable:NoExecute")))))

(deftest schedulability-test
  (is (= ["worker-2"] (kubectl/unschedulable-nodes nodes)))
  (is (= ["worker-1"] (kubectl/schedulable-nodes nodes))))

(deftest unused-pvcs-test
  (testing "claims referenced by no pod"
    (is (= ["moodle/moodle-backup"] (kubectl/unused-pvcs pvcs pods)))))

(deftest terminating-namespaces-test
  (is (= ["stuck"]
         (kubectl/terminating-namespaces
          {:items [{:metadata {:name "stuck"} :status {:phase "Terminating"}}
                   {:metadata {:name "fine"} :status {:phase "Active"}}]}))))

(deftest remove-kubernetes-finalizer-test
  (is (= {:spec {:finalizers ["custom"]}}
         (kubectl/remove-kubernetes-finalizer {:spec {:finalizers ["kubernetes" "custom"]}}))))

(deftest service-port-test
  (let [service {:spec {:ports [{:name "http" :port 9090}
                                {:name "grpc" :port 10901}]}}]
    (is (= 9090 (kubectl/service-port service "http")))
    (is (nil? (kubectl/service-port service "https")))))

(deftest image-counts-test
  (is (= {"docker.io/library/nginx:1.27" 2
          "quay.io/keycloak/keycloak:26.0" 1}
         (kubectl/image-counts pods))))

(deftest pods-with-image-pull-back-off-test
  (is (= ["broken/pod-1"]
         (kubectl/pods-with-image-pull-back-off
          {:items [{:metadata {:namespace "broken" :name "pod-1"}
                    :status {:containerStatuses [{:state {:waiting {:reason "ImagePullBackOff"}}}]}}
                   {:metadata {:namespace "fine" :name "pod-2"}
                    :status {:containerStatuses [{:state {:running {}}}]}}]}))))

(deftest drainable-pods-test
  (let [node-pods {:items [{:metadata {:namespace "app" :name "web-1"}}
                           {:metadata {:namespace "kube-system" :name "calico-abc"
                                       :ownerReferences [{:kind "DaemonSet" :name "calico"}]}}
                           {:metadata {:namespace "kube-system" :name "etcd-cp1"
                                       :annotations {(keyword "kubernetes.io/config.mirror") "hash"}}}]}]
    (testing "daemonset and mirror pods never block a drain"
      (is (= ["app/web-1"]
             (kubectl/namespaces-names (kubectl/drainable-pods node-pods)))))))

(deftest namespaces-with-ingress-host-test
  (let [ingresses {:items [{:metadata {:namespace "moodle" :name "web"}
                            :spec {:rules [{:host "moodle.example.org"}]}}
                           {:metadata {:namespace "keycloak" :name "sso"}
                            :spec {:rules [{:host "sso.example.org"}]}}]}]
    (is (= ["moodle"] (kubectl/namespaces-with-ingress-host ingresses "moodle.example.org")))
    (is (= [] (kubectl/namespaces-with-ingress-host ingresses "nope.example.org")))))

(deftest hpa-scaling-metrics-test
  (let [hpas {:items [{:spec {:metrics [{:type "Resource"
                                         :resource {:name "cpu" :target {:type "Utilization"}}}
                                        {:type "Pods"
                                         :pods {:metric {:name "requests_per_second"}
                                                :target {:type "AverageValue"}}}]}}]}]
    (is (= ["cpu/Utilization (Resource)"
            "requests_per_second/AverageValue (Pods)"]
           (kubectl/hpa-scaling-metrics hpas)))))

(deftest any-matching?-test
  (testing "a non-empty name list means at least one resource matches the selector"
    (with-redefs [shell/stdout-of (fn [& _] "deployment.apps/loki\nstatefulset.apps/loki-backend")]
      (is (kubectl/any-matching? "deployment,statefulset" "app.kubernetes.io/name=loki"))))
  (testing "empty output - no match, or an unreachable API - reads as absent, never throws"
    (with-redefs [shell/stdout-of (fn [& _] "")]
      (is (not (kubectl/any-matching? "deployment" "app.kubernetes.io/name=loki"))))))

(deftest serves-crd?-test
  (testing "serves-crd? checks the named CRD via resource-exists?"
    (with-redefs [shell/exit-code-of
                  (fn [& args]
                    (if (= (vec args) ["kubectl" "get" "customresourcedefinition"
                                       "certificates.cert-manager.io" "--request-timeout=5s"])
                      0 1))]
      (is (kubectl/serves-crd? "certificates.cert-manager.io"))
      (is (not (kubectl/serves-crd? "absent.example.com"))))))

(defn- helm-owner? [item] (= "helm" (get-in item [:metadata :labels :owner])))

(deftest get-namespaced-result-distinguishes-empty-from-failed-test
  (testing "a successful read is ok? with its parsed items"
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out "{\"items\":[{\"metadata\":{\"name\":\"a\"}}]}"})]
      (let [r (kubectl/get-namespaced-result "ctx" "ns" "secrets")]
        (is (true? (:ok? r)))
        (is (= ["a"] (map kubectl/name-of (:items r)))))))
  (testing "a genuinely empty read is ok? with no items"
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out "{\"items\":[]}"})]
      (let [r (kubectl/get-namespaced-result "ctx" "ns" "secrets")]
        (is (true? (:ok? r)))
        (is (empty? (:items r))))))
  (testing "a FAILED read is not ok? - so it can not be mistaken for empty"
    (with-redefs [shell/execute (fn [& _] {:exit 1 :out ""})]
      (is (false? (:ok? (kubectl/get-namespaced-result "ctx" "ns" "secrets")))))))

(deftest confirmed-absent-fails-safe-test
  (testing "absent is confirmed only when the read succeeded and nothing matches"
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out "{\"items\":[{\"metadata\":{\"labels\":{\"owner\":\"db-operator\"}}}]}"})]
      (is (true? (kubectl/confirmed-absent? "ctx" "ns" "secrets" helm-owner?)))))
  (testing "a matching item means NOT absent"
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out "{\"items\":[{\"metadata\":{\"labels\":{\"owner\":\"helm\"}}}]}"})]
      (is (false? (kubectl/confirmed-absent? "ctx" "ns" "secrets" helm-owner?)))))
  (testing "a failed read is NEVER 'confirmed absent' (the whole point)"
    (with-redefs [shell/execute (fn [& _] {:exit 1 :out ""})]
      (is (false? (kubectl/confirmed-absent? "ctx" "ns" "secrets" (constantly false)))))))

(deftest parse-json-degrades-to-nil-test
  (is (= {:items []} (kubectl/parse-json "{\"items\":[]}")))
  (is (nil? (kubectl/parse-json "")))
  (is (nil? (kubectl/parse-json nil)))
  (is (nil? (kubectl/parse-json "{\"items\":[{\"meta"))))

(deftest get-json-passes-any-flag-to-kubectl-get-test
  (let [argv (atom nil)]
    (with-redefs [shell/stdout-of (fn [& args] (reset! argv (vec args)) "{\"items\":[]}")]
      (is (= {:items []} (kubectl/get-json "ctx" "namespaces" "--selector=team=web"))))
    (is (= ["kubectl" "--context=ctx" "--request-timeout=30s" "get" "namespaces" "--selector=team=web" "--output=json"]
           @argv))))

(deftest get-json-result-tells-a-failed-read-from-an-empty-one-test
  (with-redefs [shell/execute (fn [& _] {:exit 0 :out "{\"items\":[]}"})]
    (is (= {:ok? true :doc {:items []}} (kubectl/get-json-result "ctx" "namespaces"))))
  (testing "kubectl failed"
    (with-redefs [shell/execute (fn [& _] {:exit 1 :out ""})]
      (is (false? (:ok? (kubectl/get-json-result "ctx" "namespaces"))))))
  (testing "kubectl answered with output that does not parse"
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out "{\"items\":[{"})]
      (is (= {:ok? false :doc nil} (kubectl/get-json-result "ctx" "namespaces"))))))

(deftest get-json-result-honours-strict-reads-test
  (let [unreachable (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"})]
    (testing "a plain read that did not answer is not ok?"
      (with-redefs [shell/execute unreachable]
        (is (= {:ok? false :doc nil} (kubectl/get-json-result "ctx" "namespaces")))))
    (testing "a strict read that did not answer throws ::unread with kubectl's reason"
      (with-redefs [shell/execute unreachable]
        (let [e (try (binding [kubectl/*strict-reads* true] (kubectl/get-json-result "ctx" "namespaces"))
                     (catch clojure.lang.ExceptionInfo e e))]
          (is (instance? clojure.lang.ExceptionInfo e))
          (is (::kubectl/unread (ex-data e)))
          (is (re-find #"Unable to connect" (ex-message e))))))
    (testing "a strict read whose output does not parse did not answer either"
      (with-redefs [shell/execute (fn [& _] {:exit 0 :out "{\"items\":[{" :err ""})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"did not answer"
                              (binding [kubectl/*strict-reads* true] (kubectl/get-json-result "ctx" "namespaces"))))))
    (testing "a strict read that answered NotFound is not ok?, and does not throw"
      (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Error from server (NotFound): namespaces \"web\" not found"})]
        (is (= {:ok? false :doc nil}
               (binding [kubectl/*strict-reads* true] (kubectl/get-json-result "ctx" "namespaces" "web"))))))
    (testing "a strict read that answered is returned as it is"
      (with-redefs [shell/execute (fn [& _] {:exit 0 :out "{\"items\":[]}" :err ""})]
        (is (= {:ok? true :doc {:items []}}
               (binding [kubectl/*strict-reads* true] (kubectl/get-json-result "ctx" "namespaces"))))))))

(deftest a-candidate-fn-built-on-get-json-result-reports-an-unanswered-read-test
  (let [candidates (fn [context]
                     (let [{:keys [doc]} (kubectl/get-json-result context "namespaces")]
                       (kubectl/names-of doc)))]
    (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Error from server (TooManyRequests): the server has received too many requests"})]
      (let [{:keys [unread candidates]} (select/resolve-target {:mode :one :label "Namespace" :candidates candidates} {} "ctx")]
        (is (nil? candidates) "an unanswered read is not an empty candidate list")
        (is (re-find #"TooManyRequests" (str unread)))))))

(deftest a-claim-counts-as-unused-only-when-the-pods-were-read
  (let [pvcs {:items [{:metadata {:namespace "db" :name "data"}}
                      {:metadata {:namespace "db" :name "spare"}}]}
        pods {:items [{:metadata {:namespace "db" :name "pg-0"}
                       :spec {:volumes [{:persistentVolumeClaim {:claimName "data"}}]}}]}
        reads (fn [pod-read]
                (fn [_ & args]
                  (if (some #{"pods"} args)
                    pod-read
                    {:ok? true :doc pvcs})))]
    (testing "with the pods read, only the claim no pod uses is offered"
      (with-redefs [kubectl/get-json-result (reads {:ok? true :doc pods})]
        (is (= ["db/spare"] (kubectl/unused-pvc-candidates "ctx")))))
    (testing "a failed pod read offers nothing and says why"
      (with-redefs [kubectl/get-json-result (reads {:ok? false :doc nil})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"could not read the pods"
                              (kubectl/unused-pvc-candidates "ctx")))))
    (testing "right before deleting, the claim is checked again in its namespace"
      (with-redefs [kubectl/get-json-result (reads {:ok? true :doc pods})]
        (is (kubectl/pvc-unused? "ctx" "db" "spare"))
        (is (not (kubectl/pvc-unused? "ctx" "db" "data"))))
      (with-redefs [kubectl/get-json-result (reads {:ok? false :doc nil})]
        (is (not (kubectl/pvc-unused? "ctx" "db" "spare")) "an unanswered read is not an absence")))))

(deftest a-generic-ephemeral-volume-claims-its-pod-named-pvc
  (let [pods {:items [{:metadata {:namespace "ci" :name "runner-abc"}
                       :spec {:volumes [{:name "scratch"
                                         :ephemeral {:volumeClaimTemplate {:spec {}}}}]}}]}]
    (is (= #{"ci/runner-abc-scratch"} (kubectl/claimed-pvcs pods)))
    (is (= [] (kubectl/unused-pvcs {:items [{:metadata {:namespace "ci" :name "runner-abc-scratch"}}]} pods)))))

(deftest an-object-is-gone-only-when-the-read-answered
  (with-redefs [gumshoe.shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"})]
    (is (false? (kubectl/confirmed-gone? "ctx" "ns" "pvc" "data")) "a failed read proves nothing"))
  (with-redefs [gumshoe.shell/execute (fn [& _] {:exit 0 :out "persistentvolumeclaim/data\n" :err ""})]
    (is (false? (kubectl/confirmed-gone? "ctx" "ns" "pvc" "data")) "still there"))
  (let [argv (atom nil)]
    (with-redefs [gumshoe.shell/execute (fn [& args] (reset! argv args) {:exit 0 :out "" :err ""})]
      (is (true? (kubectl/confirmed-gone? "ctx" nil "namespaces" "drill")))
      (is (some #{"--ignore-not-found"} @argv))
      (is (not-any? #(clojure.string/starts-with? (str %) "--namespace") @argv)))))
