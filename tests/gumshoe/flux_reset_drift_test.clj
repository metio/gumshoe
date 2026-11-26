;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.flux-reset-drift-test
  "flux/reset verifies by the token it stamped, so an already-Ready release
   does not verify before helm-controller handled the reset; flux/drift
   stops with the reason when the HelmRelease or Helm's manifest could not
   be read, instead of reporting no drift or an absent manifest."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.mutation-books-test :as books]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.streams-support :as streams]
            [gumshoe.verify :as verify]))

(def ^:private token "2026-11-26T10:00:00Z")

(defn- helmrelease
  [status]
  {:apiVersion "helm.toolkit.fluxcd.io/v2" :kind "HelmRelease"
   :metadata {:namespace "apps" :name "web" :uid "uid-web" :generation 1 :resourceVersion "100"}
   :spec {:interval "10m" :chart {:spec {:chart "web"}}}
   :status (merge {:observedGeneration 1
                   :conditions [{:type "Ready" :status "True" :reason "UpgradeSucceeded" :message "ok"}]}
                  status)})

;; --- flux/reset ------------------------------------------------------------------

(defn- reset-holds?
  "Whether flux/reset's post-check holds on one read of `release`."
  [release]
  (let [spec (books/load-spec "tools/flux/runbooks/flux/reset.clj")
        [{:keys [check]} & more] ((:verify spec) {:context "ctx" :target "apps/web" :timestamp token})]
    (is (empty? more))
    (with-redefs [shell/execute (fn [& _] {:exit 0 :out (json/generate-string release) :err ""})]
      (verify/attempt check))))

(deftest reset-does-not-verify-a-release-that-was-already-ready
  (is (false? (reset-holds? (helmrelease {})))
      "Ready without lastHandledReconcileAt says nothing about the reset")
  (is (false? (reset-holds? (helmrelease {:lastHandledReconcileAt "2026-11-01T00:00:00Z"})))
      "a token from an earlier request is not this run's"))

(deftest reset-verifies-once-the-token-is-handled-and-ready
  (is (true? (reset-holds? (helmrelease {:lastHandledReconcileAt token}))))
  (is (true? (reset-holds? (helmrelease {:lastHandledReconcileAt token :lastHandledResetAt token})))))

(deftest reset-does-not-verify-a-reset-the-controller-did-not-handle
  (is (false? (reset-holds? (helmrelease {:lastHandledReconcileAt token
                                          :lastHandledResetAt "2026-11-01T00:00:00Z"}))))
  (is (false? (reset-holds? (assoc-in (helmrelease {:lastHandledReconcileAt token})
                                      [:status :conditions 0 :status] "False")))
      "a handled reset whose release failed again is not verified"))

(deftest reset-does-not-verify-on-a-read-that-did-not-answer
  (let [spec (books/load-spec "tools/flux/runbooks/flux/reset.clj")
        [{:keys [check]}] ((:verify spec) {:context "ctx" :target "apps/web" :timestamp token})]
    (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"})]
      (is (not (true? (verify/attempt check)))))))

;; --- flux/drift ------------------------------------------------------------------

(defn- compare-release
  []
  (binding [runbook/*loading* true]
    (load-file "tools/flux/runbooks/flux/drift.clj"))
  (requiring-resolve 'runbooks.flux.drift/compare-release))

(defn- drift
  "{:result :err :calls} of comparing apps/web against shell answers from
   `answer`, given each argv."
  [answer]
  (let [calls (atom [])
        f (compare-release)]
    (with-redefs [shell/execute (fn [& argv] (swap! calls conj (vec argv)) (answer (vec argv)))
                  shell/execute-with-stdin (fn [& _] {:exit 0 :out "" :err ""})]
      (let [{:keys [result err out]} (streams/separated #(f "ctx" "apps/web"))]
        {:result result :printed (str err out) :calls @calls}))))

(defn- helm? [argv] (= "helm" (first argv)))

(deftest drift-stops-when-the-helmrelease-read-did-not-answer
  (let [{:keys [result printed calls]} (drift (fn [_] {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"}))]
    (is (false? result))
    (is (str/includes? printed "could not read HelmRelease apps/web"))
    (is (str/includes? printed "i/o timeout"))
    (is (not (str/includes? printed "no drift")))
    (is (not-any? helm? calls) "helm never runs with names nobody read")))

(deftest drift-stops-when-the-helmrelease-is-gone
  (let [{:keys [result printed calls]}
        (drift (fn [_] {:exit 1 :out "" :err "Error from server (NotFound): helmreleases.helm.toolkit.fluxcd.io \"web\" not found"}))]
    (is (false? result))
    (is (str/includes? printed "HelmRelease apps/web no longer exists"))
    (is (not (str/includes? printed "no drift")))
    (is (not-any? helm? calls))))

(deftest drift-reports-helms-error-rather-than-an-absent-manifest
  (let [{:keys [result printed calls]}
        (drift (fn [argv] (if (helm? argv)
                            {:exit 1 :out "" :err "Error: Kubernetes cluster unreachable"}
                            {:exit 0 :out (json/generate-string (helmrelease {})) :err ""})))]
    (is (false? result))
    (is (str/includes? printed "helm get manifest web in apps failed: Error: Kubernetes cluster unreachable"))
    (is (not (str/includes? printed "has no manifest")))
    (is (some #(and (helm? %) (some #{"web"} %) (some #{"apps"} %)) calls))))

(deftest drift-names-an-empty-manifest-as-absent
  (let [{:keys [result printed]}
        (drift (fn [argv] (if (helm? argv)
                            {:exit 0 :out "" :err ""}
                            {:exit 0 :out (json/generate-string (helmrelease {})) :err ""})))]
    (is (false? result))
    (is (str/includes? printed "helm has no manifest for release web in apps"))))

(deftest drift-diffs-a-manifest-it-read
  (let [{:keys [result printed]}
        (drift (fn [argv] (if (helm? argv)
                            {:exit 0 :out "apiVersion: v1\nkind: ConfigMap\n" :err ""}
                            {:exit 0 :out (json/generate-string (helmrelease {})) :err ""})))]
    (is (true? result))
    (is (str/includes? printed "reports no drift - diffing anyway"))
    (is (str/includes? printed "no differences"))))
