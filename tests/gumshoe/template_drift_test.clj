;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.template-drift-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.detectives.template-drift :as drift]
            [gumshoe.subject :as subject]))

;; --- images ----------------------------------------------------------------------

(deftest image-spellings-are-the-same-image
  (is (= "nginx:latest" (drift/normalize-image "nginx")))
  (is (= "nginx:1.27" (drift/normalize-image "docker.io/library/nginx:1.27")))
  (is (= "bitnami/redis:7" (drift/normalize-image "docker.io/bitnami/redis:7")))
  (is (= "nginx:latest" (drift/normalize-image "index.docker.io/library/nginx")))
  (is (= "registry.local:5000/app:latest" (drift/normalize-image "registry.local:5000/app")) "a port is not a tag")
  (is (= "ghcr.io/org/app:v1" (drift/normalize-image "ghcr.io/org/app:v1@sha256:abc"))))

(deftest image-changes
  (is (not (drift/image-changed? "nginx" "docker.io/library/nginx:latest")))
  (is (not (drift/image-changed? "nginx:1.27" "nginx:1.27@sha256:aaa")) "pinning the same reference")
  (is (drift/image-changed? "nginx:1.27@sha256:aaa" "nginx:1.27@sha256:bbb"))
  (is (drift/image-changed? "nginx:1.27" "mirror.local/library/nginx:1.27") "a registry rewrite")
  (is (drift/image-changed? "nginx:1.27" "nginx:1.28")))

;; --- containers ------------------------------------------------------------------

(def ^:private template-container
  {:name "app" :image "nginx:1.27"
   :resources {:requests {:cpu "100m" :memory "128Mi"} :limits {:memory "256Mi"}}
   :env [{:name "MODE" :value "prod"} {:name "TOKEN" :valueFrom {:secretKeyRef {:name "s" :key "t"}}}]})

(deftest an-identical-container-has-no-diffs
  (is (empty? (drift/container-diffs template-container template-container))))

(deftest equal-quantities-written-differently
  (is (empty? (drift/container-diffs template-container
                                     (assoc template-container :resources {:requests {:cpu "0.1" :memory "134217728"}
                                                                           :limits {:memory "0.25Gi"}})))))

(deftest resource-changes
  (is (= ["cpu request 100m → 500m"]
         (drift/container-diffs template-container (assoc-in template-container [:resources :requests :cpu] "500m"))))
  (is (= ["cpu limit 1, which the template does not set"]
         (drift/container-diffs template-container (assoc-in template-container [:resources :limits :cpu] "1")))
      "a LimitRange default")
  (is (= ["memory limit 256Mi removed"]
         (drift/container-diffs template-container (update-in template-container [:resources :limits] dissoc :memory)))))

(deftest requests-copied-from-limits-stay-quiet
  (let [template {:name "app" :image "x:1" :resources {:limits {:cpu "500m" :memory "1Gi"}}}
        pod (assoc-in template [:resources :requests] {:cpu "500m" :memory "1Gi"})]
    (is (empty? (drift/container-diffs template pod)))
    (is (= ["cpu request 200m, which the template does not set"]
           (drift/container-diffs template (assoc-in pod [:resources :requests :cpu] "200m")))
        "a request other than the limit was not copied from it")))

(deftest other-resources-only-when-the-template-names-them
  (is (empty? (drift/container-diffs template-container
                                     (assoc-in template-container [:resources :limits :vpc.amazonaws.com/pod-eni] "1"))))
  (let [template (assoc-in template-container [:resources :limits :nvidia.com/gpu] "1")]
    (is (= ["nvidia.com/gpu limit 1 → 2"]
           (drift/container-diffs template (assoc-in template [:resources :limits :nvidia.com/gpu] "2"))))))

(deftest environment-changes
  (is (empty? (drift/container-diffs template-container (update template-container :env conj {:name "INJECTED" :value "1"})))
      "an added variable")
  (is (= ["env MODE changed"]
         (drift/container-diffs template-container (assoc-in template-container [:env 0 :value] "debug"))))
  (is (= ["env TOKEN removed"]
         (drift/container-diffs template-container (update template-container :env #(vec (take 1 %)))))))

(deftest process-changes
  (let [template (assoc template-container :command ["/app"] :args ["--port=8080"])]
    (is (= ["command changed" "args changed"]
           (drift/container-diffs template (assoc template :command ["/wrapper" "/app"] :args ["--port=9090"])))))
  (is (empty? (drift/container-diffs template-container (assoc template-container :command ["/x"])))
      "a command the template leaves to the image is not compared"))

(deftest pod-level-diffs
  (let [template {:containers [template-container] :initContainers [{:name "init" :image "busybox:1.37"}]}]
    (is (empty? (drift/pod-diffs template (update template :containers conj {:name "istio-proxy" :image "proxy:1"})))
        "an injected sidecar")
    (is (= ["container app removed"] (drift/pod-diffs template (assoc template :containers []))))
    (is (= ["init: image busybox:1.37 → busybox:1.36"]
           (drift/pod-diffs template (assoc-in template [:initContainers 0 :image] "busybox:1.36"))))))

;; --- the detective ----------------------------------------------------------------

(def ^:private template-spec {:containers [template-container]})

(defn- pod [name owner-kind owner-name & {:keys [spec hash deleting] :or {spec template-spec}}]
  {:metadata (cond-> {:namespace "web" :name name
                      :ownerReferences [{:kind owner-kind :name owner-name :controller true}]}
               hash (assoc :labels {:controller-revision-hash hash})
               deleting (assoc :deletionTimestamp "2026-11-20T10:00:00Z"))
   :spec spec})

(def ^:private mutated (assoc-in template-spec [:containers 0 :image] "mirror.local/nginx:1.27"))

(defn- evidence [& {:keys [pods]}]
  {"pods" {:items pods}
   "replicasets" {:items [{:metadata {:namespace "web" :name "shop-7d9"
                                      :ownerReferences [{:kind "Deployment" :name "shop" :controller true}]}
                           :spec {:template {:spec template-spec}}}
                          {:metadata {:namespace "web" :name "bare"}
                           :spec {:template {:spec template-spec}}}]}
   "statefulsets" {:items [{:metadata {:namespace "web" :name "db"}
                            :spec {:template {:spec template-spec}}
                            :status {:updateRevision "db-abc"}}]}
   "daemonsets" {:items [{:metadata {:namespace "web" :name "agent"}
                          :spec {:template {:spec template-spec}}}]}
   "jobs" {:items [{:metadata {:namespace "web" :name "nightly-29000"
                               :ownerReferences [{:kind "CronJob" :name "nightly" :controller true}]}
                    :spec {:template {:spec template-spec}}}]}
   "controllerrevisions" {:items [{:metadata {:namespace "web" :name "agent-old"
                                              :ownerReferences [{:kind "DaemonSet" :name "agent" :controller true}]}
                                   :revision 1}
                                  {:metadata {:namespace "web" :name "agent-new"
                                              :ownerReferences [{:kind "DaemonSet" :name "agent" :controller true}]}
                                   :revision 2}]}})

(defn- detect [& pods]
  (vec (drift/detect-template-drift (evidence :pods pods))))

(deftest pods-matching-their-template-are-quiet
  (is (= [] (detect (pod "shop-7d9-a" "ReplicaSet" "shop-7d9") (pod "db-0" "StatefulSet" "db" :hash "db-abc")))))

(deftest a-mutated-deployment-pod-is-reported-on-the-deployment
  (let [[finding & more] (detect (pod "shop-7d9-a" "ReplicaSet" "shop-7d9" :spec mutated)
                                 (pod "shop-7d9-b" "ReplicaSet" "shop-7d9" :spec mutated)
                                 (pod "shop-7d9-c" "ReplicaSet" "shop-7d9"))]
    (is (nil? more))
    (is (= :warning (:severity finding)))
    (is (= "web/shop" (:component finding)))
    (is (= (subject/subject "Deployment" "web" "shop") (:subject finding)))
    (is (= (str "Deployment web/shop: 2 pods, app: image nginx:1.27 → mirror.local/nginx:1.27 - a mutating webhook or a LimitRange "
                "changed them on admission, so what runs is not what the manifest says")
           (:hint finding)))))

(deftest owners-are-lifted-and-kept
  (testing "a CronJob's Job and a bare ReplicaSet"
    (is (= #{(subject/subject "CronJob" "web" "nightly") (subject/subject "ReplicaSet" "web" "bare")}
           (set (map :subject (detect (pod "n-1" "Job" "nightly-29000" :spec mutated)
                                      (pod "bare-1" "ReplicaSet" "bare" :spec mutated))))))))

(deftest only-the-current-revision-counts
  (testing "a StatefulSet pod of an older revision is a rollout in progress"
    (is (= [] (detect (pod "db-1" "StatefulSet" "db" :spec mutated :hash "db-old"))))
    (is (= 1 (count (detect (pod "db-1" "StatefulSet" "db" :spec mutated :hash "db-abc"))))))
  (testing "a DaemonSet pod counts at its newest ControllerRevision's hash"
    (is (= [] (detect (pod "agent-x" "DaemonSet" "agent" :spec mutated :hash "old"))))
    (is (= 1 (count (detect (pod "agent-x" "DaemonSet" "agent" :spec mutated :hash "new"))))))
  (testing "no hash label, no comparison"
    (is (= [] (detect (pod "db-2" "StatefulSet" "db" :spec mutated))))))

(deftest pods-without-a-comparable-template
  (is (= [] (detect (pod "static" "Node" "worker-1" :spec mutated))))
  (is (= [] (detect (pod "gone" "ReplicaSet" "deleted-rs" :spec mutated))))
  (is (= [] (detect (pod "shop-7d9-z" "ReplicaSet" "shop-7d9" :spec mutated :deleting true))) "a terminating pod"))

(deftest many-diffs-are-summed-up
  (let [spec (-> template-spec
                 (assoc-in [:containers 0 :image] "other:1")
                 (assoc-in [:containers 0 :resources :requests :cpu] "1")
                 (assoc-in [:containers 0 :resources :requests :memory] "1Gi")
                 (assoc-in [:containers 0 :env 0 :value] "x")
                 (update-in [:containers 0 :env] #(vec (take 1 %))))
        [finding] (detect (pod "shop-7d9-a" "ReplicaSet" "shop-7d9" :spec spec))]
    (is (str/includes? (:hint finding) "(+1 more)"))))

(deftest joins-the-posture-scope-only
  (is (some #(= "template-drift" (:name %)) (registry/for-scope :posture)))
  (is (not-any? #(= "template-drift" (:name %)) (registry/for-scope :all)) "posture is opt-in"))
