;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.namespace-finalize-test
  "namespaces/remove_finalizers tells the operator that forcing finalization
   orphans what is left in the namespace, shows the namespace's deletion
   conditions and the objects it still holds, and refuses while objects may
   remain unless --orphan-remaining acknowledges it."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cleanup :as cleanup]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation-books-test :as books]
            [gumshoe.shell :as shell]))

(def ^:private path "runbooks/kubernetes/namespaces/remove_finalizers.clj")

(def ^:private content-remaining
  {:type "NamespaceContentRemaining" :status "True" :reason "SomeResourcesRemain"
   :message "Some resources are remaining: persistentvolumeclaims. has 1 resource instances"})

(def ^:private finalizers-remaining
  {:type "NamespaceFinalizersRemaining" :status "True" :reason "SomeFinalizersRemain"
   :message "Some content in the namespace has finalizers remaining: kubernetes.io/pvc-protection in 1 resource instances"})

(def ^:private content-failure
  {:type "NamespaceDeletionContentFailure" :status "False" :reason "ContentDeleted"
   :message "All content successfully deleted, may be waiting on finalization"})

(defn- stuck
  [& conditions]
  {:metadata {:name "stuck" :uid "uid-stuck" :resourceVersion "100"}
   :spec {:finalizers ["kubernetes"]}
   :status (cond-> {:phase "Terminating"}
             (seq conditions) (assoc :conditions (vec conditions)))})

(def ^:private claim
  {:metadata {:namespace "stuck" :name "data" :uid "uid-data" :finalizers ["kubernetes.io/pvc-protection"]}})

(defn- run
  [scenario]
  (books/transcript (books/load-spec path) (merge {:target "stuck"} scenario)))

;; --- the decisions ---------------------------------------------------------------

(deftest remaining-content-names-why-objects-may-remain
  (is (nil? (cleanup/remaining-content (stuck content-failure) {:ok? true :objects []})))
  (is (= "1 object(s) are left in it"
         (cleanup/remaining-content (stuck) {:ok? true :objects ["persistentvolumeclaim/data"]})))
  (is (= "NamespaceContentRemaining, NamespaceFinalizersRemaining hold"
         (cleanup/remaining-content (stuck content-remaining finalizers-remaining) {:ok? true :objects []}))
      "a holding condition is a reason even when the listing came back empty")
  (is (= "NamespaceDeletionDiscoveryFailure hold"
         (cleanup/remaining-content (stuck {:type "NamespaceDeletionDiscoveryFailure" :status "True"})
                                    {:ok? true :objects []}))
      "a group the controller could not enumerate may hold objects nobody listed")
  (is (str/includes? (cleanup/remaining-content (stuck) {:ok? false :reason "exit 1"})
                     "could not be listed (exit 1)")
      "a listing that did not answer never stands for an empty namespace"))

(deftest orphan-refusal-is-lifted-only-by-the-acknowledgement
  (let [contents {:ok? true :objects ["persistentvolumeclaim/data"]}]
    (is (re-find #"--orphan-remaining" (cleanup/orphan-refusal "stuck" (stuck) contents false)))
    (is (re-find #"etcd without cleanup" (cleanup/orphan-refusal "stuck" (stuck) contents false)))
    (is (nil? (cleanup/orphan-refusal "stuck" (stuck) contents true)))
    (is (nil? (cleanup/orphan-refusal "stuck" (stuck) {:ok? true :objects []} false)))))

(deftest namespace-items-show-conditions-and-objects
  (is (= ["stuck"
          "NamespaceContentRemaining True: Some resources are remaining: persistentvolumeclaims. has 1 resource instances"
          "NamespaceDeletionContentFailure False: All content successfully deleted, may be waiting on finalization"
          "1 object(s) left in it:"
          "  persistentvolumeclaim/data"]
         (vec (cleanup/namespace-items "stuck" (stuck content-failure content-remaining)
                                       {:ok? true :objects ["persistentvolumeclaim/data"]}))))
  (is (= ["stuck" "the namespace reports no deletion conditions" "could not list the objects left in it: boom"]
         (vec (cleanup/namespace-items "stuck" (stuck) {:ok? false :reason "boom"})))))

(deftest namespace-objects-reads-every-namespaced-type
  (let [calls (atom [])]
    (with-redefs [shell/execute (fn [& argv]
                                  (swap! calls conj (vec argv))
                                  (if (some #{"api-resources"} argv)
                                    {:exit 0 :out "pods\npersistentvolumeclaims\n" :err ""}
                                    {:exit 0 :out "persistentvolumeclaim/data\n" :err ""}))]
      (is (= {:ok? true :objects ["persistentvolumeclaim/data"]} (kubectl/namespace-objects "ctx" "stuck")))
      (is (some #(and (some #{"pods,persistentvolumeclaims"} %) (some #{"--namespace=stuck"} %)) @calls))))
  (testing "a discovery that answered in part is unread"
    (with-redefs [shell/execute (fn [& _] {:exit 1 :out "pods\n"
                                           :err "unable to retrieve the complete list of server APIs: metrics.k8s.io/v1beta1"})]
      (let [{:keys [ok? reason]} (kubectl/namespace-objects "ctx" "stuck")]
        (is (false? ok?))
        (is (str/includes? reason "metrics.k8s.io")))))
  (testing "a listing that failed is unread"
    (with-redefs [shell/execute (fn [& argv]
                                  (if (some #{"api-resources"} argv)
                                    {:exit 0 :out "pods\n" :err ""}
                                    {:exit 1 :out "" :err "Error from server (Forbidden): pods is forbidden"}))]
      (let [{:keys [ok? reason]} (kubectl/namespace-objects "ctx" "stuck")]
        (is (false? ok?))
        (is (str/includes? reason "Forbidden"))))))

;; --- the book --------------------------------------------------------------------

(deftest confirmation-states-that-remaining-objects-are-orphaned
  (let [{:keys [action destructive?]} (run {:objects [["namespaces" (stuck)]]})]
    (is (true? destructive?))
    (is (str/includes? action "NOT deleted"))
    (is (str/includes? action "stay in etcd without cleanup"))
    (is (str/includes? action "created later under this name exposes them again"))
    (is (not (str/includes? action "is deleted")))))

(deftest refuses-without-the-flag-when-content-remains
  (testing "objects listed in the namespace"
    (let [{:keys [stopped action]} (run {:objects [["namespaces" (stuck)] ["persistentvolumeclaims" claim]]})]
      (is (nil? action) "the operator is never asked")
      (is (re-find #"refusing: namespace stuck may still hold objects: 1 object\(s\) are left in it" stopped))
      (is (str/includes? stopped "--orphan-remaining"))))
  (testing "a condition saying content remains, with nothing the listing could see"
    (let [{:keys [stopped]} (run {:objects [["namespaces" (stuck content-remaining)]]})]
      (is (re-find #"refusing: .*NamespaceContentRemaining hold" stopped)))))

(deftest proceeds-with-the-flag-and-shows-what-is-orphaned
  (let [{:keys [items plan stopped]} (run {:objects [["namespaces" (stuck content-remaining finalizers-remaining)]
                                                     ["persistentvolumeclaims" claim]]
                                           :opts {:orphan-remaining true}})]
    (is (nil? stopped))
    (is (some #(str/starts-with? % "NamespaceContentRemaining True: Some resources are remaining") items))
    (is (some #(str/starts-with? % "NamespaceFinalizersRemaining True: ") items))
    (is (some #{"  persistentvolumeclaims/data"} items))
    (is (some #(= :kubectl-stdin (first %)) plan))))

(deftest proceeds-without-the-flag-when-nothing-remains
  (let [{:keys [items plan stopped]} (run {:objects [["namespaces" (stuck content-failure)]]})]
    (is (nil? stopped))
    (is (some #{"no object is left in it"} items))
    (is (some #(= :kubectl-stdin (first %)) plan))))

(deftest an-unread-listing-refuses-and-is-named
  (let [discovery-down {:exit 1 :out "" :err "the server is currently unable to handle the request"}]
    (let [{:keys [stopped]} (run {:objects [["namespaces" (stuck)]] :api-resources discovery-down})]
      (is (re-find #"refusing: .*could not be listed .*unable to handle the request" stopped)))
    (testing "with the flag the confirmation says the listing was not read"
      (let [{:keys [items]} (run {:objects [["namespaces" (stuck)]] :api-resources discovery-down
                                  :opts {:orphan-remaining true}})]
        (is (some #(str/starts-with? % "could not list the objects left in it: ") items))))))

(deftest an-unread-namespace-stops-before-asking
  (let [{:keys [stopped action]} (run {:objects [["namespaces" (stuck content-remaining)]] :answering? false})]
    (is (nil? action))
    (is (re-find #"did not answer|i/o timeout" stopped))))
