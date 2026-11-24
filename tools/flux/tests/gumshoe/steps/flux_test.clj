;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps.flux-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.shell :as shell]
            [gumshoe.steps.flux :as flux-steps]
            [gumshoe.tools.flux :as flux]))

(def ^:private kustomization
  {:kind "Kustomization" :type flux/kustomization-type :namespace "flux-system" :name "apps"})

(def ^:private resourceset
  {:kind "ResourceSet" :type flux-steps/resourceset-type :namespace "apps" :name "set"})

(defn- ctx [object]
  {:context "ctx" :object object :timestamp "T"})

(defn- serving
  "A kubectl that answers every get with `object`."
  [object]
  (fn [& _] {:exit 0 :out (json/generate-string object) :err ""}))

(def ^:private suspend (flux-steps/suspend {:object :object :none "nothing to suspend" :timeout 30 :interval 5}))
(def ^:private resume (flux-steps/resume {:object :object :none "nothing to resume" :timeout :wait :interval 15}))

(deftest every-suspendable-kind-switches-through-spec-suspend
  (doseq [[kind type] flux/suspendable]
    (let [object {:kind kind :type type :namespace "ns" :name "n"}]
      (is (= [[:kubectl "ctx" "-n" "ns" "patch" type "n" "--type=merge" "--patch={\"spec\":{\"suspend\":true}}"]]
             (effect/collect ((:effect suspend) (ctx object))))
          kind)
      (is (= [[:kubectl "ctx" "-n" "ns" "patch" type "n" "--type=merge" "--patch={\"spec\":{\"suspend\":false}}"]
              [:kubectl "ctx" "-n" "ns" "annotate" type "n" "--overwrite" "reconcile.fluxcd.io/requestedAt=T"]]
             (effect/collect ((:effect resume) (ctx object))))
          kind))))

(deftest a-resourceset-switches-through-its-reconcile-annotation
  (is (= [[:kubectl "ctx" "-n" "apps" "annotate" flux-steps/resourceset-type "set" "--overwrite" "fluxcd.controlplane.io/reconcile=disabled"]]
         (effect/collect ((:effect suspend) (ctx resourceset)))))
  (is (= [[:kubectl "ctx" "-n" "apps" "annotate" flux-steps/resourceset-type "set" "--overwrite" "fluxcd.controlplane.io/reconcile=enabled"]
          [:kubectl "ctx" "-n" "apps" "annotate" flux-steps/resourceset-type "set" "--overwrite" "reconcile.fluxcd.io/requestedAt=T"]]
         (effect/collect ((:effect resume) (ctx resourceset)))))
  (is (true? (flux-steps/suspended? "ResourceSet" {:metadata {:annotations {:fluxcd.controlplane.io/reconcile "disabled"}}})))
  (is (false? (flux-steps/suspended? "ResourceSet" {:spec {:suspend true}}))
      "a ResourceSet has no spec.suspend switch"))

(deftest without-an-object-a-step-plans-a-note-and-checks-nothing
  (is (= [[:note "nothing to suspend"]] (effect/collect ((:effect suspend) (ctx nil)))))
  (is (= [[:note "nothing to resume"]] (effect/collect ((:effect resume) (ctx nil)))))
  (is (nil? ((:verify suspend) (ctx nil))))
  (is (nil? ((:verify resume) (ctx nil)))))

(deftest the-post-checks-wait-for-the-switch-the-step-flipped
  (let [[suspended] ((:verify suspend) (ctx kustomization))
        [resumed] ((:verify resume) (assoc (ctx kustomization) :wait 90))]
    (is (= {:description "Kustomization flux-system/apps is suspended" :timeout 30 :interval 5}
           (dissoc suspended :check)))
    (is (= {:description "Kustomization flux-system/apps is resumed and Ready" :timeout 90 :interval 15}
           (dissoc resumed :check)))
    (with-redefs [shell/execute (serving {:spec {:suspend true}})]
      (is (true? (boolean ((:check suspended)))))
      (is (false? (boolean ((:check resumed))))))
    (with-redefs [shell/execute (serving {:spec {} :status {:conditions [{:type "Ready" :status "True"}]}})]
      (is (false? (boolean ((:check suspended)))))
      (is (true? (boolean ((:check resumed))))))
    (with-redefs [shell/execute (serving {:spec {} :status {:conditions [{:type "Ready" :status "False"}]}})]
      (is (false? (boolean ((:check resumed)))) "resumed but not Ready"))))

(deftest a-step-can-wait-for-more-under-its-own-description
  (let [seen (atom nil)
        step (flux-steps/suspend {:object :object :none "-" :timeout 10 :interval 1
                                  :description "suspended and quiet"
                                  :also (fn [c live] (reset! seen [(:extra c) live]) (:quiet? c))})
        verify-in #(first ((:verify step) (merge (ctx kustomization) %)))]
    (with-redefs [shell/execute (serving {:spec {:suspend true}})]
      (is (= "suspended and quiet" (:description (verify-in {}))))
      (is (true? (boolean ((:check (verify-in {:quiet? true :extra 1}))))))
      (is (= [1 {:spec {:suspend true}}] @seen) "the condition sees the step's context and the object as read")
      (is (false? (boolean ((:check (verify-in {:quiet? false})))))))))

(deftest a-reconcile-waits-for-its-own-request-to-be-handled
  (let [step (flux-steps/reconcile {:object :object :none "nothing to reconcile" :timeout 120 :interval 5})
        token (:timestamp ((:derive step) {}))
        c (assoc (ctx kustomization) :timestamp token)
        [handled] ((:verify step) c)]
    (is (string? token))
    (is (= [[:kubectl "ctx" "-n" "flux-system" "annotate" flux/kustomization-type "apps" "--overwrite"
             (str "reconcile.fluxcd.io/requestedAt=" token)]]
           (effect/collect ((:effect step) c))))
    (is (= {:description "Kustomization flux-system/apps handled the reconcile request" :timeout 120 :interval 5}
           (dissoc handled :check)))
    (with-redefs [shell/execute (serving {:status {:lastHandledReconcileAt token
                                                   :conditions [{:type "Ready" :status "True"}]}})]
      (is (true? ((:check handled)))))
    (testing "an object Ready before the request has not handled it"
      (with-redefs [shell/execute (serving {:status {:lastHandledReconcileAt "earlier"
                                                     :conditions [{:type "Ready" :status "True"}]}})]
        (is (false? ((:check handled))))))
    (is (= [[:note "nothing to reconcile"]] (effect/collect ((:effect step) (ctx nil)))))))
