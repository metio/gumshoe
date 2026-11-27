;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.storage-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.quantity :as quantity]
            [gumshoe.shell :as shell]
            [gumshoe.storage :as storage]))

(def statefulset
  {:metadata {:name "postgres"
              :namespace "databases"
              :uid "abc-123"
              :resourceVersion "42"
              :creationTimestamp "2026-01-01T00:00:00Z"
              :generation 7
              :managedFields [{:manager "kubectl"}]
              :annotations {(keyword "kubectl.kubernetes.io/last-applied-configuration") "{...}"
                            :keep-me "yes"}}
   :spec {:replicas 2
          :volumeClaimTemplates [{:metadata {:name "data"}
                                  :spec {:resources {:requests {:storage "50Gi"}}}}
                                 {:metadata {:name "wal"}
                                  :spec {:resources {:requests {:storage "10Gi"}}}}]}
   :status {:readyReplicas 2}})

(deftest capacity-test
  (is (= "100Gi" (storage/normalize-capacity "100")))
  (is (= "100Gi" (storage/normalize-capacity "100Gi")))
  (is (= 100 (storage/gi-value "100Gi")))
  (testing "the API server reads a multiple of 1024Gi back as Ti"
    (is (= 1024 (storage/gi-value "1Ti")))
    (is (= 1536 (storage/gi-value "1.5Ti"))))
  (is (= 0 (storage/gi-value "0Gi")))
  (testing "anything that is not a whole number of Gi"
    (doseq [value ["100" "20G" "1536Mi" "-1Gi" "lots" nil]]
      (is (nil? (storage/gi-value value)) (pr-str value)))))

(deftest quantities-compare-as-amounts
  (is (quantity/same? "1Ti" "1024Gi"))
  (is (quantity/at-least? "1Ti" "1024Gi"))
  (is (quantity/at-least? "21Gi" "20Gi"))
  (is (not (quantity/at-least? "20Gi" "21Gi")))
  (is (not (quantity/same? "20Gi" "20G")))
  (testing "a value that does not parse satisfies nothing"
    (is (not (quantity/at-least? nil "20Gi")))
    (is (not (quantity/at-least? "20Gi" "lots")))
    (is (not (quantity/same? nil nil)))))

(deftest a-claim-read-back-as-ti-is-compared-not-refused
  (let [plan [{:pvc "data-pg-0" :exists? true :resizable? true :storage-class "fast" :current "1Ti" :target "2048Gi"}]]
    (is (= [] (storage/expansion-problems plan)))
    (is (= ["PVC data-pg-0 already has 1Ti - shrinking to 512Gi is not possible"]
           (storage/expansion-problems (assoc-in plan [0 :target] "512Gi"))))))

(deftest statefulset-pvc-names-test
  (is (= ["data-postgres-0" "data-postgres-1"]
         (storage/statefulset-pvc-names statefulset "data"))))

(deftest expansion-plan-test
  (let [plan (storage/expansion-plan
              {:pvc-names ["data-postgres-0" "data-postgres-1"]
               :pvcs {:items [{:metadata {:name "data-postgres-0"}
                               :spec {:storageClassName "expandable"
                                      :resources {:requests {:storage "50Gi"}}}}]}
               :storage-classes {:items [{:metadata {:name "expandable"}
                                          :allowVolumeExpansion true}]}
               :capacity "100Gi"})]
    (testing "a missing PVC surfaces as a problem"
      (is (= [true false] (map :exists? plan)))
      (is (some #(re-find #"does not exist" %) (storage/expansion-problems plan))))))

(deftest expansion-problems-test
  (testing "a valid plan has no problems"
    (is (empty? (storage/expansion-problems
                 [{:pvc "x" :exists? true :storage-class "ok" :resizable? true
                   :current "50Gi" :target "100Gi"}]))))
  (testing "shrinking and non-expandable classes are rejected"
    (is (seq (storage/expansion-problems
              [{:pvc "x" :exists? true :storage-class "ok" :resizable? true
                :current "100Gi" :target "50Gi"}])))
    (is (seq (storage/expansion-problems
              [{:pvc "x" :exists? true :storage-class "fixed" :resizable? false
                :current "50Gi" :target "100Gi"}])))))

(deftest recreate-manifest-test
  (let [manifest (storage/recreate-manifest statefulset "data" "100Gi")]
    (testing "server-managed fields are gone"
      (is (nil? (:status manifest)))
      (is (nil? (-> manifest :metadata :uid)))
      (is (nil? (-> manifest :metadata :resourceVersion)))
      (is (nil? (-> manifest :metadata :managedFields)))
      (is (nil? (get-in manifest [:metadata :annotations
                                  (keyword "kubectl.kubernetes.io/last-applied-configuration")]))))
    (testing "other annotations survive"
      (is (= "yes" (-> manifest :metadata :annotations :keep-me))))
    (testing "only the chosen template is resized"
      (is (= ["100Gi" "10Gi"]
             (mapv #(-> % :spec :resources :requests :storage)
                   (-> manifest :spec :volumeClaimTemplates)))))))

(deftest resize-preflight-is-fail-closed-test
  (reset! @#'storage/resize-preflights [])
  (testing "a preflight that reports a problem surfaces it"
    (storage/register-resize-preflight! (fn [_] ["no room in the pool"]))
    (is (= ["no room in the pool"] (storage/resize-preflight-problems {:plan []}))))
  (testing "a preflight that THROWS becomes a problem, never silently skipped (fail closed)"
    (reset! @#'storage/resize-preflights [])
    (storage/register-resize-preflight! (fn [_] (throw (ex-info "boom" {}))))
    (let [problems (storage/resize-preflight-problems {:plan []})]
      (is (= 1 (count problems)))
      (is (clojure.string/includes? (first problems) "treated as unsafe"))))
  (testing "a preflight returning nil/empty contributes no problem"
    (reset! @#'storage/resize-preflights [])
    (storage/register-resize-preflight! (fn [_] nil))
    (storage/register-resize-preflight! (fn [_] []))
    (is (empty? (storage/resize-preflight-problems {:plan []}))))
  (reset! @#'storage/resize-preflights []))

(deftest the-claims-start-at-the-start-ordinal
  (is (= ["data-db-1" "data-db-2"]
         (storage/statefulset-pvc-names {:metadata {:name "db"} :spec {:replicas 2 :ordinals {:start 1}}} "data"))))

;; --- the resize as a plan ----------------------------------------------------------

(def two-claims
  [{:pvc "data-postgres-0" :current "50Gi" :target "80Gi"}
   {:pvc "data-postgres-1" :current "50Gi" :target "80Gi"}])

(deftest a-planned-patch-runs-the-command-patch!-runs
  (let [seen (atom nil)]
    (with-redefs [shell/execute (fn [& argv] (reset! seen (vec argv)) {:exit 0})]
      (kubectl/patch! "ctx" "databases" "persistentvolumeclaims" "data-postgres-0" (storage/claim-patch "80Gi")))
    (is (= @seen
           (into ["kubectl" "--context=ctx"]
                 (kubectl/merge-patch-args "databases" "persistentvolumeclaims" "data-postgres-0"
                                           (storage/claim-patch "80Gi")))))
    (is (= (drop 2 @seen)
           (drop 2 (first (storage/resize-claim-effects "ctx" "databases" two-claims "80Gi"))))
        "the effect hands kubectl the same arguments after the context")))

(deftest one-patch-per-claim-in-plan-order
  (let [steps (storage/resize-claim-steps "ctx" "databases" two-claims "80Gi")]
    (is (= ["data-postgres-0" "data-postgres-1"] (mapv :claim steps)))
    (is (every? #(= :claims (:stage %)) steps))
    (is (= [[:kubectl "ctx" "--namespace=databases" "patch" "persistentvolumeclaims" "data-postgres-0"
             "--type=merge" "--patch={\"spec\":{\"resources\":{\"requests\":{\"storage\":\"80Gi\"}}}}"]
            [:kubectl "ctx" "--namespace=databases" "patch" "persistentvolumeclaims" "data-postgres-1"
             "--type=merge" "--patch={\"spec\":{\"resources\":{\"requests\":{\"storage\":\"80Gi\"}}}}"]]
           (effect/collect (storage/plan-of steps))))
    (is (= (storage/plan-of steps) (storage/resize-claim-effects "ctx" "databases" two-claims "80Gi")))))

(deftest an-empty-plan-has-no-claim-steps
  (is (= [] (storage/resize-claim-steps "ctx" "databases" [] "80Gi"))))

(def steps
  (storage/statefulset-resize-steps "ctx" statefulset "data" "80Gi" two-claims))

(deftest the-statefulset-is-resized-claims-first-then-deleted-orphaned-then-applied
  (is (= [:claims :claims :delete :gone :recreate] (mapv :stage steps)))
  (is (= [:kubectl "ctx" "--namespace=databases" "delete" "statefulset" "postgres" "--ignore-not-found" "--interactive=false" "--cascade=orphan"
          "--wait=false" "--request-timeout=30s"]
         (:effect (nth steps 2)))
      "the delete returns once the API server accepted it")
  (is (= [:kubectl "ctx" "--namespace=databases" "wait" "--for=delete" "statefulset/postgres" "--timeout=120s"]
         (:effect (nth steps 3)))
      "the wait for the orphan finalizer is bounded by kubectl's own timeout")
  (let [[op context manifest & args] (:effect (nth steps 4))]
    (is (= [:kubectl-stdin "ctx" ["apply" "--filename" "-"]] [op context (vec args)]))
    (is (= (storage/recreate-manifest statefulset "data" "80Gi") (json/parse-string manifest true))
        "the manifest applied is the recreate manifest of the StatefulSet the steps were built from")
    (is (= manifest (:manifest (nth steps 4))) "the step names its manifest for the recovery"))
  (is (every? effect/effect? (storage/plan-of steps)) "every step carries exactly one effect"))

(deftest progress-names-what-ran-and-where-it-stopped
  (testing "nothing ran"
    (is (= {:resized [] :unresized ["data-postgres-0" "data-postgres-1"] :stages #{} :stopped (first steps)}
           (storage/progress steps 0)))
    (is (= (storage/progress steps 0) (storage/progress steps nil)) "an absent count is none"))
  (testing "stopped at the second claim"
    (let [{:keys [resized unresized stopped stages]} (storage/progress steps 1)]
      (is (= ["data-postgres-0"] resized))
      (is (= ["data-postgres-1"] unresized))
      (is (= #{:claims} stages))
      (is (= "data-postgres-1" (:claim stopped)))))
  (testing "stopped at the delete"
    (let [{:keys [resized unresized stopped]} (storage/progress steps 2)]
      (is (= ["data-postgres-0" "data-postgres-1"] resized))
      (is (= [] unresized))
      (is (= :delete (:stage stopped)))))
  (testing "every step ran"
    (let [{:keys [stopped stages]} (storage/progress steps 5)]
      (is (nil? stopped))
      (is (= #{:claims :delete :gone :recreate} stages)))))

(defn- recovery
  ([applied] (recovery applied :present))
  ([applied state]
   (storage/statefulset-recovery {:context "ctx" :target "databases/postgres" :state state} steps applied)))

(deftest no-recovery-when-nothing-was-applied
  (is (nil? (recovery 0)))
  (is (nil? (recovery nil))))

(deftest a-stop-among-the-claims-names-the-enlarged-and-the-rest
  (let [text (recovery 1)]
    (is (str/includes? text "StatefulSet databases/postgres was not touched"))
    (is (str/includes? text "cannot be undone:\n  data-postgres-0\nNot enlarged:\n  data-postgres-1"))
    (is (str/includes? text "why data-postgres-1 could not be patched"))
    (is (str/includes? text "run this book again"))
    (is (not (str/includes? text "DELETED")))))

(deftest a-stop-at-the-delete-says-the-statefulset-still-exists
  (let [text (recovery 2)]
    (is (str/includes? text "could not be deleted: it still exists with its old volume claim template"))
    (is (str/includes? text "cannot be undone:\n  data-postgres-0\n  data-postgres-1"))
    (is (not (str/includes? text "Not enlarged")))))

(deftest a-delete-the-api-server-accepted-is-reported-as-being-deleted
  (doseq [applied [2 3]]
    (testing (str "stopped at " (:stage (:stopped (storage/progress steps applied))))
      (let [text (recovery applied :deleting)]
        (is (str/starts-with? text "StatefulSet databases/postgres IS BEING DELETED"))
        (is (not (str/includes? text "could not be deleted")))
        (is (str/includes? text "will not find it once it is gone"))
        (is (str/includes? text (str "  " (:manifest (last steps)) "
  EOF")) "the re-apply manifest is handed over")))))

(deftest a-statefulset-gone-after-the-delete-is-reported-deleted-with-the-manifest
  (let [text (recovery 3 :gone)]
    (is (str/starts-with? text "THE STATEFULSET databases/postgres IS DELETED and was not recreated"))
    (is (str/includes? text "kubectl --context=ctx apply --filename - <<'EOF'"))))

(deftest a-statefulset-whose-state-is-unread-gets-both-ways-out
  (doseq [state [:unread nil]]
    (let [text (recovery 2 state)]
      (is (str/starts-with? text "Whether StatefulSet databases/postgres still exists is unknown"))
      (is (str/includes? text "kubectl --context=ctx --namespace=databases get statefulset postgres"))
      (is (str/includes? text "run this book again"))
      (is (str/includes? text "<<'EOF'")))))

(deftest a-stop-at-the-recreate-hands-over-the-manifest
  (let [text (recovery 4)
        manifest (:manifest (last steps))]
    (is (str/starts-with? text "THE STATEFULSET databases/postgres IS DELETED"))
    (is (str/includes? text "kubectl --context=ctx apply --filename - <<'EOF'"))
    (is (str/includes? text (str "  " manifest "\n  EOF")) "the manifest is printed whole, ready to paste")))

(deftest a-run-that-applied-everything-says-nothing-was-rolled-back
  (let [text (recovery 5)]
    (is (str/starts-with? text "Every effect ran: StatefulSet databases/postgres was recreated"))
    (is (str/includes? text "Nothing was rolled back"))))

(deftest the-statefulset-checks-keep-their-names-and-timeouts
  (let [checks (storage/statefulset-checks "ctx" "cluster" "databases" "postgres" "data" "80Gi" two-claims)]
    (is (= [["all 2 PVCs report capacity 80Gi" 300 15 true]
            ["StatefulSet databases/postgres exists again with the new template" 60 nil nil]
            ["every pod of the StatefulSet is Running" 300 10 nil]]
           (mapv (juxt :description :timeout :interval :soft?) checks)))
    (is (fn? (:watch (first checks))) "the capacity check watches the resize")))

(deftest the-template-check-holds-only-for-the-new-capacity
  (let [[_ template-check pods-check] (storage/statefulset-checks "ctx" "cluster" "databases" "postgres" "data" "80Gi" two-claims)
        answer (fn [object] (fn [& _] {:exit 0 :out (json/generate-string object) :err ""}))]
    (with-redefs [shell/execute (answer (assoc-in statefulset [:spec :volumeClaimTemplates 0 :spec :resources :requests :storage] "80Gi"))]
      (is (true? ((:check template-check)))))
    (with-redefs [shell/execute (answer statefulset)]
      (is (false? ((:check template-check)))))
    (with-redefs [shell/execute (answer (assoc statefulset :status {:readyReplicas 2 :observedGeneration 7}))]
      (is (true? ((:check pods-check)))))
    (with-redefs [shell/execute (answer (assoc statefulset :status {:readyReplicas 1}))]
      (is (false? ((:check pods-check)))))))
