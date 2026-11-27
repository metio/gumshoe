;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux-retire-test
  "gumshoe.tools.flux-retire: decoding the release payload within its
   bounds, reading the manifest's claims, the writer predicates, every
   refusal, the four ordering predicates over the plan with a negative
   control each, and the flux/retire book run through gumshoe.mutation
   against a fake cluster whose helm-controller uninstalls a HelmRelease
   deleted with its finalizer and without a suspend. The payloads are built
   from synthetic JSON at test time."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]
            [gumshoe.testing.plan :as plan]
            [gumshoe.testing.refusal :as refusal]
            [gumshoe.tools.flux :as flux]
            [gumshoe.tools.flux-retire :as retire]
            [gumshoe.tools.flux-storage :as storage]
            [gumshoe.verify :as verify])
  (:import (java.io ByteArrayOutputStream)
           (java.util Base64)
           (java.util.zip GZIPOutputStream)))

;; --- payloads ------------------------------------------------------------------------

(defn- b64 [^bytes bs] (.encodeToString (Base64/getEncoder) bs))

(defn- gzipped
  [^String text]
  (let [out (ByteArrayOutputStream.)]
    (with-open [gz (GZIPOutputStream. out)]
      (.write gz (.getBytes text "UTF-8")))
    (.toByteArray out)))

(defn- payload
  "A storage Secret's .data.release for release, as Helm writes it: the
   Secret's base64 over Helm's base64 over gzip over JSON."
  [release]
  (b64 (.getBytes (b64 (gzipped (json/generate-string release))) "UTF-8")))

(def ^:private manifest
  (str "---\n# Source: wiki/templates/settings.yaml\n"
       "apiVersion: v1\nkind: ConfigMap\nmetadata:\n  name: settings\ndata:\n  mode: synthetic\n"
       "---\n# Source: wiki/templates/web.yaml\n"
       "apiVersion: apps/v1\nkind: Deployment\nmetadata:\n  name: web\n  namespace: wiki\n"
       "---\n# Source: wiki/templates/empty.yaml\n"
       "---\napiVersion: rbac.authorization.k8s.io/v1\nkind: ClusterRole\nmetadata:\n  name: wiki-reader\n"
       "---\napiVersion: v1\nkind: ConfigMap\nmetadata:\n  name: old\n"))

(def ^:private release-payload (payload {"name" "wiki" "version" 2 "manifest" manifest}))

;; --- decoding ------------------------------------------------------------------------

(deftest a-release-payload-decodes-to-its-manifest
  (is (= {:manifest manifest} (retire/decode-release release-payload)))
  (testing "a payload of plain JSON, as very old Helm versions wrote it"
    (is (= {:manifest "x: 1"}
           (retire/decode-release (b64 (.getBytes (b64 (.getBytes (json/generate-string {"manifest" "x: 1"}) "UTF-8")) "UTF-8")))))))

(deftest a-payload-past-its-bounds-is-a-problem
  (is (re-find #"more than the 10 a Helm release can be"
               (:problem (retire/decode-release release-payload {:max-chars 10}))))
  (testing "a gzip stream that inflates past the bound is cut off, never held"
    (let [bomb (b64 (.getBytes (b64 (gzipped (json/generate-string {"manifest" (apply str (repeat 100000 "a"))}))) "UTF-8"))]
      (is (< (count bomb) 2000) "the fixture compresses well, as a bomb does")
      (is (re-find #"inflates to more than 4096 bytes" (:problem (retire/decode-release bomb {:max-bytes 4096})))))))

(deftest a-corrupt-payload-is-a-problem-not-an-empty-release
  (doseq [[p why] [[nil "holds no release payload"]
                   ["" "holds no release payload"]
                   ["%%%" "is not base64"]
                   [(b64 (.getBytes "%%%" "UTF-8")) "not Helm's base64"]
                   [(b64 (.getBytes (b64 (byte-array [0x1f 0x8b 1 2 3])) "UTF-8")) "not a readable gzip stream"]
                   [(b64 (.getBytes (b64 (gzipped "{not json")) "UTF-8")) "is not JSON"]
                   [(b64 (.getBytes (b64 (gzipped "[1]")) "UTF-8")) "not a JSON object"]
                   [(b64 (.getBytes (b64 (gzipped "{\"name\":\"wiki\"}")) "UTF-8")) "records no manifest"]]]
    (is (str/includes? (str (:problem (retire/decode-release p))) why) (pr-str p))))

(deftest the-manifest-claims-each-object-once-in-order
  (is (= {:claims [{:kind "ConfigMap" :group nil :namespace nil :name "settings"}
                   {:kind "Deployment" :group "apps" :namespace "wiki" :name "web"}
                   {:kind "ClusterRole" :group "rbac.authorization.k8s.io" :namespace nil :name "wiki-reader"}
                   {:kind "ConfigMap" :group nil :namespace nil :name "old"}]}
         (retire/manifest-claims (str manifest "---\napiVersion: v1\nkind: ConfigMap\nmetadata:\n  name: old\n")))))

(deftest a-manifest-whose-objects-cannot-be-named-is-a-problem
  (doseq [[m why] [["kind: ConfigMap\n" "names no kind and name"]
                   ["- a\n- b\n" "is not an object"]
                   ["apiVersion: v1\nkind: ConfigMapList\nitems: []\n" "holds a ConfigMapList"]
                   ["a: [unclosed\n" "is not YAML"]]]
    (is (str/includes? (str (:problem (retire/manifest-claims m))) why) m)))

(deftest claims-are-read-as-their-kind-and-group
  (is (= "deployment.apps" (retire/claim-type {:kind "Deployment" :group "apps"})))
  (is (= "configmap" (retire/claim-type {:kind "ConfigMap" :group nil}))))

(deftest the-newest-revision-and-the-newest-deployed-one-are-read
  (is (= [3 2] (retire/revisions-to-read [{:version 1 :status "superseded"} {:version 2 :status "deployed"}
                                          {:version 3 :status "failed"}])))
  (is (= [2] (retire/revisions-to-read [{:version 1 :status "superseded"} {:version 2 :status "deployed"}])))
  (is (= [] (retire/revisions-to-read []))))

;; --- writers -------------------------------------------------------------------------

(deftest another-writer-is-a-server-side-apply-by-a-manager-other-than-helm
  (is (true? (retire/default-writer? {:manager "product-reconciler" :operation "Apply"})))
  (is (false? (retire/default-writer? {:manager "helm-controller" :operation "Apply"})))
  (is (false? (retire/default-writer? {:manager "helm" :operation "Update"})))
  (is (false? (retire/default-writer? {:manager "kubectl-edit" :operation "Update"})) "an update declares no ownership")
  (is (false? (retire/default-writer? {:manager "kube-controller-manager" :operation "Apply" :subresource "status"}))
      "a status writer manages the status, not the object"))

(deftest a-named-writer-counts-whatever-its-operation-and-helm-never-does
  (let [w (retire/writer-named ["argocd-controller" "helm-controller"])]
    (is (true? (w {:manager "argocd-controller" :operation "Update"})))
    (is (false? (w {:manager "argocd-controller" :operation "Update" :subresource "status"})))
    (is (false? (w {:manager "product-reconciler" :operation "Apply"})) "only the named managers count")
    (is (false? (w {:manager "helm-controller" :operation "Apply"})) "naming Helm does not make it another writer")))

;; --- problems ------------------------------------------------------------------------

(def ^:private hr-object
  {:metadata {:namespace "wiki" :name "wiki" :finalizers ["finalizers.fluxcd.io"]}
   :spec {:interval "10m"}
   :status {:history [{:name "wiki" :namespace "wiki"}]}})

(def ^:private taken {:state :present :uid "u1" :managers [{:manager "product-reconciler" :operation "Apply"}]})
(def ^:private helm-only {:state :present :uid "u2" :managers [{:manager "helm-controller" :operation "Update"}]})

(def ^:private record
  {:release "wiki" :release-namespace "wiki" :storage-namespace "wiki" :versions [2] :timeout 300
   :claimed [{:claim {:kind "ConfigMap" :name "settings"} :reading taken}]})

(deftest a-release-whose-objects-another-writer-took-over-has-no-problem
  (is (= [] (retire/problems hr-object record [] retire/default-writer?))))

(deftest every-reason-not-to-retire-is-named
  (let [p #(retire/problems %1 %2 %3 retire/default-writer?)]
    (is (re-find #"being deleted already" (first (p (assoc-in hr-object [:metadata :deletionTimestamp] "2026-11-28T00:00:00Z") record []))))
    (is (re-find #"cannot say what it owns: its manifest claims no object" (first (p hr-object (assoc record :problem "its manifest claims no object") []))))
    (is (re-find #"right now.*300s" (first (p hr-object (assoc record :in-flight? true) []))))
    (is (= ["one owner still renders this HelmRelease and is not suspended, so it could recreate it or revert the suspend before the delete: Kustomization flux-system/apps. Remove it from the source or suspend the owner first"]
           (p hr-object record [{:kind "Kustomization" :namespace "flux-system" :name "apps" :suspended? false}])))
    (is (= [] (p hr-object record [{:kind "Kustomization" :namespace "flux-system" :name "apps" :suspended? true}]))
        "a suspended owner is named in the confirmation, not refused")
    (is (= ["could not read ConfigMap settings, which the release claims, so whether another writer manages it is unknown"]
           (p hr-object (assoc-in record [:claimed 0 :reading] {:state :unread :reason "timeout"}) [])))
    (is (= ["nothing but Helm writes ConfigMap settings, which the release claims; retiring the release would leave it unmanaged. Hand it over to another writer first"]
           (p hr-object (assoc-in record [:claimed 0 :reading] helm-only) [])))
    (is (= [] (p hr-object (assoc-in record [:claimed 0 :reading] {:state :absent}) []))
        "a claimed object that is gone has nothing left to protect")))

;; --- the plan and its order ----------------------------------------------------------

(def ^:private hr-ref {:type flux/helmrelease-type :namespace "wiki" :name "wiki"})
(def ^:private the-plan (retire/retire-plan "ctx" hr-ref ["finalizers.fluxcd.io"]))

(deftest the-plan-suspends-strips-and-deletes-only-the-helmrelease
  (is (= [[:kubectl "ctx" "--namespace=wiki" "patch" flux/helmrelease-type "wiki" "--type=merge" "--patch={\"spec\":{\"suspend\":true}}"]
          [:kubectl "ctx" "--namespace=wiki" "patch" flux/helmrelease-type "wiki" "--type=json"
           "--patch=[{\"op\":\"test\",\"path\":\"/metadata/finalizers\",\"value\":[\"finalizers.fluxcd.io\"]},{\"op\":\"replace\",\"path\":\"/metadata/finalizers\",\"value\":[]}]"]
          [:kubectl "ctx" "--namespace=wiki" "delete" flux/helmrelease-type "wiki" "--ignore-not-found" "--interactive=false" "--wait=false"]]
         the-plan))
  (is (true? (retire/strips-finalizer-before-delete? the-plan)))
  (is (true? (retire/suspends-before-stripping? the-plan)))
  (is (true? (retire/holds-storage? the-plan)))
  (is (true? (retire/deletes-only-the-release? the-plan hr-ref))))

(deftest the-strip-keeps-every-other-finalizer-and-tests-the-list-it-confirmed
  (is (= [{"op" "test" "path" "/metadata/finalizers" "value" ["example.com/keep" "finalizers.fluxcd.io"]}
          {"op" "replace" "path" "/metadata/finalizers" "value" ["example.com/keep"]}]
         (json/parse-string (retire/strip-patch ["example.com/keep" "finalizers.fluxcd.io"]))))
  (is (= [{"op" "add" "path" "/metadata/finalizers" "value" []}] (json/parse-string (retire/strip-patch [])))))

(deftest negative-control-a-delete-before-the-strip-fails-its-predicate
  (let [[suspend strip del] the-plan]
    (is (false? (retire/strips-finalizer-before-delete? [suspend del strip])))
    (is (false? (retire/strips-finalizer-before-delete? [suspend del])) "no strip at all")))

(deftest negative-control-a-strip-before-the-suspend-fails-its-predicate
  (let [[suspend strip del] the-plan]
    (is (false? (retire/suspends-before-stripping? [strip suspend del])))
    (is (false? (retire/suspends-before-stripping? [strip del])) "no suspend at all")))

(deftest negative-control-a-plan-dropping-the-storage-fails-its-predicate
  (doseq [extra [[:kubectl "ctx" "--namespace=wiki" "delete" "secrets" "--selector=owner=helm,name=wiki"]
                 [:kubectl "ctx" "--namespace=wiki" "delete" "secret" "sh.helm.release.v1.wiki.v2"]]]
    (is (false? (retire/holds-storage? (conj the-plan extra))) (pr-str extra)))
  (is (true? (retire/holds-storage? (conj the-plan [:note "drop it with kubectl delete secrets --selector owner=helm,name=wiki"])))
      "a note only carries words"))

(deftest negative-control-a-second-delete-or-another-target-fails-its-predicate
  (is (false? (retire/deletes-only-the-release? (conj the-plan [:kubectl "ctx" "--namespace=wiki" "delete" "deployments" "wiki"]) hr-ref))
      "a Deployment sharing the release's name is still not the release")
  (is (false? (retire/deletes-only-the-release? (assoc the-plan 2 [:kubectl "ctx" "--namespace=wiki" "delete" flux/helmrelease-type "other"]) hr-ref)))
  (is (false? (retire/deletes-only-the-release? (pop the-plan) hr-ref)) "no delete at all"))

;; --- the fake cluster ----------------------------------------------------------------

(def ^:private hr-type flux/helmrelease-type)

(defn- managed
  [& entries]
  (vec (for [[manager operation subresource] entries]
         (cond-> {"manager" manager "operation" operation} subresource (assoc "subresource" subresource)))))

(defn- obj
  [kind namespace name uid fields]
  {"kind" kind
   "metadata" (cond-> {"name" name "uid" uid "managedFields" fields
                       "labels" {"app.kubernetes.io/managed-by" "Helm"}
                       "annotations" {"meta.helm.sh/release-name" "wiki" "meta.helm.sh/release-namespace" "wiki"}}
                namespace (assoc "namespace" namespace))})

(def ^:private taken-fields (managed ["helm-controller" "Update"] ["product-reconciler" "Apply"]
                                     ["kube-controller-manager" "Update" "status"]))

(defn- helmrelease
  [& {:keys [finalizers suspend] :or {finalizers ["finalizers.fluxcd.io"] suspend false}}]
  {"apiVersion" "helm.toolkit.fluxcd.io/v2" "kind" "HelmRelease"
   "metadata" {"name" "wiki" "namespace" "wiki" "uid" "hr-uid" "resourceVersion" "1" "finalizers" finalizers}
   "spec" {"interval" "10m" "suspend" suspend}
   "status" {"history" [{"name" "wiki" "namespace" "wiki"}]}})

(defn- estate
  "A cluster holding the HelmRelease wiki/wiki, its storage (revision 2,
   deployed), and three of the four objects its manifest claims, each taken
   over by product-reconciler."
  [& {:keys [objects owners]}]
  (atom {:answering? true
         :objects (merge {[hr-type "wiki" "wiki"] (helmrelease)
                          ["configmap" "wiki" "settings"] (obj "ConfigMap" "wiki" "settings" "settings-uid" taken-fields)
                          ["deployment.apps" "wiki" "web"] (obj "Deployment" "wiki" "web" "web-uid" taken-fields)
                          ["clusterrole.rbac.authorization.k8s.io" nil "wiki-reader"]
                          (obj "ClusterRole" nil "wiki-reader" "reader-uid" (managed ["product-reconciler" "Apply"]))}
                         objects)
         :storage {"sh.helm.release.v1.wiki.v1" {:version 1 :status "superseded" :payload release-payload}
                   "sh.helm.release.v1.wiki.v2" {:version 2 :status "deployed" :payload release-payload}}
         :owners (or owners [])}))

(defn- positionals
  [argv]
  (let [after (rest (drop-while #(not= "get" %) argv))]
    (vec (remove #(str/starts-with? % "-") after))))

(defn- flag
  [argv prefix]
  (some #(when (str/starts-with? % prefix) (subs % (count prefix))) argv))

(defn- storage-listing
  [storage]
  (apply str (for [[_ {:keys [version status]}] (sort storage)]
               (str "wiki\twiki\t" status "\t" version "\t1700000000\t1700000000\t2023-11-14T22:13:20Z\n"))))

(defn- fake-cluster
  [state]
  (fn [& args]
    (let [argv (mapv str (filter string? (flatten args)))
          {:keys [objects storage owners answering?]} @state
          out (fn [x] {:exit 0 :out (if (string? x) x (json/generate-string x)) :err ""})]
      (swap! state update :commands (fnil conj []) argv)
      (cond
        (not answering?)
        {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 192.0.2.1:6443: i/o timeout"}

        (not (some #{"get"} argv))
        (out "")

        (some #(str/starts-with? % "--selector=owner=helm") argv)
        (out (storage-listing storage))

        (some #(= "--output=jsonpath={.data.release}" %) argv)
        (out (or (:payload (get storage (second (positionals argv)))) ""))

        (= [flux/kustomization-type] (positionals argv))
        (out {:items owners})

        (= [storage/resourceset-type] (positionals argv))
        (out {:items []})

        :else
        (let [[type name] (positionals argv)
              ns (flag argv "--namespace=")
              o (or (get objects [type ns name]) (get objects [type nil name]))]
          (cond
            o (out o)
            (some #{"--ignore-not-found"} argv) (out "")
            :else {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" type name)}))))))

(defn- uninstall!
  "What helm-controller's uninstall does: deletes every object the manifest
   claims."
  [state]
  (swap! state update :objects #(into {} (remove (fn [[[t]]] (not= hr-type t)) %)))
  (swap! state assoc :uninstalled? true))

(defn- apply-effect!
  "Applies one effect to the fake cluster. A HelmRelease deleted while it
   carries its finalizer and is not suspended is uninstalled, as
   helm-controller does; one deleted suspended or without the finalizer is
   just removed."
  [state [op _context & args]]
  (when (= :kubectl op)
    (let [args (vec args)
          verb (some #{"patch" "delete"} args)
          [type name] (rest (drop-while #(not= verb %) args))
          k [type (flag args "--namespace=") name]
          patch (flag args "--patch=")]
      (swap! state update :writes (fnil conj []) [verb type name])
      (case verb
        "patch" (if (= "--type=merge" (some #{"--type=merge" "--type=json"} args))
                  (swap! state assoc-in [:objects k "spec" "suspend"] (get-in (json/parse-string patch) ["spec" "suspend"]))
                  (let [ops (json/parse-string patch)
                        current (get-in @state [:objects k "metadata" "finalizers"])]
                    (when (every? (fn [{:strs [op value]}] (or (not= "test" op) (= value current))) ops)
                      (doseq [{:strs [op value]} ops :when (#{"replace" "add"} op)]
                        (swap! state assoc-in [:objects k "metadata" "finalizers"] value)))
                    ;; helm-controller adds its finalizer back on the
                    ;; reconcile the strip triggers, suspended or not
                    (when (:readds? @state)
                      (swap! state update-in [:objects k "metadata" "finalizers"]
                             #(vec (distinct (conj (vec %) "finalizers.fluxcd.io")))))))
        "delete" (let [hr (get-in @state [:objects k])]
                   (when (and (= hr-type type) hr
                              (some #{"finalizers.fluxcd.io"} (get-in hr ["metadata" "finalizers"]))
                              (not (get-in hr ["spec" "suspend"])))
                     (uninstall! state))
                   (swap! state update :objects dissoc k))
        nil)))
  true)

(defn- run-book
  [state target opts]
  (let [seen (atom nil)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  shell/execute (fake-cluster state)
                  flow/change! (fn [{:keys [precondition effect post-checks] :as config}]
                                 (reset! seen config)
                                 (and (if precondition (precondition) true)
                                      (every? #(apply-effect! state %) effect)
                                      (every? #(true? (verify/attempt (:check %))) post-checks)))]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out]
                     (mutation/run-on! retire/retire-named target opts nil))]
        {:result result :out (str out) :config @seen}))))

;; --- the book end to end -------------------------------------------------------------

(deftest the-objects-survive-and-the-storage-stays
  (let [state (estate)
        {:keys [result out config]} (run-book state "wiki/wiki" {})]
    (is (true? result) out)
    (is (nil? (get-in @state [:objects [hr-type "wiki" "wiki"]])) "the HelmRelease is gone")
    (is (not (:uninstalled? @state)))
    (is (= 3 (count (remove (fn [[[t]]] (= hr-type t)) (:objects @state)))) "every claimed object that existed is still there")
    (is (= 2 (count (:storage @state))))
    (is (= [["patch" hr-type "wiki"] ["patch" hr-type "wiki"] ["delete" hr-type "wiki"]] (:writes @state)))
    (is (= ["suspend HelmRelease wiki/wiki, so nothing reconciles it while it is retired"
            "strip finalizers.fluxcd.io from HelmRelease wiki/wiki, keeping no other finalizer - without it the delete reaches no helm uninstall"
            "delete HelmRelease wiki/wiki"
            "keep ConfigMap settings, written by product-reconciler"
            "keep Deployment wiki/web, written by product-reconciler"
            "keep ClusterRole wiki-reader, written by product-reconciler"
            "ConfigMap old, which the release claims, does not exist - nothing to keep"
            "leave the Helm storage of release wiki in wiki (revision 2) for you to drop once the post-checks pass"]
           (get-in config [:confirmation :items])))
    (is (= ["ConfigMap settings, Deployment wiki/web, ClusterRole wiki-reader survived the retirement as the same objects (an uninstall would have deleted them)"
            "the Helm storage of release wiki in wiki still holds revision 2"]
           (map :description (:post-checks config))))))

(deftest a-claimed-object-nothing-else-writes-refuses-before-the-confirmation
  (let [state (estate :objects {["configmap" "wiki" "settings"]
                                (obj "ConfigMap" "wiki" "settings" "settings-uid" (managed ["helm-controller" "Update"]))})
        {:keys [result out config]} (run-book state "wiki/wiki" {})]
    (is (false? result))
    (is (nil? config) "nothing is asked")
    (is (str/includes? out "nothing but Helm writes ConfigMap settings") out)
    (is (empty? (:writes @state)))))

(deftest a-named-writer-replaces-the-default-rule
  (testing "product-reconciler applies every object, but --writer names another manager"
    (let [state (estate)
          {:keys [result out]} (run-book state "wiki/wiki" {:writer ["argocd-controller"]})]
      (is (false? result))
      (is (str/includes? out "nothing but Helm writes ConfigMap settings, Deployment wiki/web, ClusterRole wiki-reader") out)
      (is (empty? (:writes @state)))))
  (let [state (estate)
        {:keys [result out]} (run-book state "wiki/wiki" {:writer ["product-reconciler"]})]
    (is (true? result) out)))

(deftest an-owner-that-still-renders-the-helmrelease-refuses
  (let [owner {"kind" "Kustomization"
               "metadata" {"namespace" "flux-system" "name" "apps"}
               "spec" {"interval" "5m"}
               "status" {"inventory" {"entries" [{"id" "wiki_wiki_helm.toolkit.fluxcd.io_HelmRelease" "v" "v2"}]}}}
        state (estate :owners [owner])
        {:keys [result out]} (run-book state "wiki/wiki" {})]
    (is (false? result))
    (is (str/includes? out "Kustomization flux-system/apps") out)
    (is (empty? (:writes @state)))
    (testing "a suspended owner is named and the retirement proceeds"
      (let [state (estate :owners [(assoc-in owner ["spec" "suspend"] true)])
            {:keys [result out config]} (run-book state "wiki/wiki" {})]
        (is (true? result) out)
        (is (some #(str/includes? % "Kustomization flux-system/apps still renders this HelmRelease and is suspended")
                  (get-in config [:confirmation :items])))))))

(deftest a-helmrelease-already-gone-is-retired-already
  (let [state (estate)]
    (swap! state update :objects dissoc [hr-type "wiki" "wiki"])
    (let [{:keys [result out config]} (run-book state "wiki/wiki" {})]
      (is (true? result) out)
      (is (nil? config) "nothing is asked")
      (is (str/includes? out "already done") out)
      (is (empty? (:writes @state))))))

(deftest negative-control-a-plan-in-the-wrong-order-loses-the-objects-and-fails-its-post-check
  (let [state (estate)
        {:keys [result]} (with-redefs [retire/retire-plan (fn [context ref _finalizers]
                                                            [(retire/delete-effect context ref)])]
                           (run-book state "wiki/wiki" {}))]
    (is (true? (:uninstalled? @state)) "the fake helm-controller uninstalled the release")
    (is (false? result) "the survival check does not hold")))

(deftest a-controller-that-adds-the-finalizer-back-is-held-off-by-the-suspend
  (let [state (estate)]
    (swap! state assoc :readds? true)
    (let [{:keys [result out]} (run-book state "wiki/wiki" {})]
      (is (true? result) out)
      (is (not (:uninstalled? @state)))))
  (testing "negative control: without the suspend the re-added finalizer reaches the uninstall"
    (let [state (estate)]
      (swap! state assoc :readds? true)
      (let [{:keys [result]} (with-redefs [retire/retire-plan (fn [context ref finalizers]
                                                                [(retire/strip-effect context ref finalizers)
                                                                 (retire/delete-effect context ref)])]
                               (run-book state "wiki/wiki" {}))]
        (is (true? (:uninstalled? @state)))
        (is (false? result))))))

(deftest the-post-checks-fail-closed
  (let [claimed [{:claim {:kind "ConfigMap" :name "settings"} :reading {:state :present :uid "settings-uid"}}]]
    (is (empty? (verify/fail-closed-findings
                 [(retire/survived-check "ctx" hr-ref "wiki" claimed)
                  (retire/storage-kept-check "ctx" "wiki" "wiki" [2])
                  (retire/gone-check "ctx" hr-ref)])))
    (testing "negative control: a gone check that does not declare it verifies an absence is caught"
      (is (seq (verify/fail-closed-findings [(dissoc (retire/gone-check "ctx" hr-ref) :verifies-absence?)]))))))

(deftest an-object-replaced-under-the-same-name-did-not-survive
  (let [state (estate)
        claimed [{:claim {:kind "ConfigMap" :name "settings"} :reading {:state :present :uid "settings-uid"}}]]
    (swap! state update :objects dissoc [hr-type "wiki" "wiki"])
    (with-redefs [shell/execute (fake-cluster state)]
      (is (true? (retire/survived? "ctx" hr-ref "wiki" claimed)))
      (swap! state assoc-in [:objects ["configmap" "wiki" "settings" ] "metadata" "uid"] "new-uid")
      (is (false? (retire/survived? "ctx" hr-ref "wiki" claimed)) "a recreated object is not the one that survived")
      (swap! state assoc-in [:objects [hr-type "wiki" "wiki"]] (helmrelease))
      (is (false? (retire/survived? "ctx" hr-ref "wiki" claimed)) "nothing survived while the HelmRelease is still there"))))

;; --- plan coverage and refusal polarity ----------------------------------------------

(deftest the-dry-run-plan-lists-every-object-the-effect-touches
  (with-redefs [shell/execute (fake-cluster (estate))]
    (plan/covers! retire/retire-named {:target "wiki/wiki" :opts {}})))

(defn- cluster-answers
  "Every read of the estate but the one under test, as refusal matcher pairs."
  []
  (let [state (estate)
        fake (fake-cluster state)]
    (fn [argv] (let [{:keys [exit out err]} (apply fake (rest argv))]
                 {:exit exit :out out :err err}))))

(defn- polar
  [read answer refusal]
  (refusal/is-polar retire/retire-named
                    {:target "wiki/wiki"
                     :opts {}
                     :read read
                     :answer answer
                     :cluster (cluster-answers)
                     :refusal refusal}))

(deftest an-unread-claimed-object-refuses-and-an-answered-one-proceeds
  (polar (refusal/kubectl-get {:type "configmap" :namespace "wiki" :name "settings"})
         (obj "ConfigMap" "wiki" "settings" "settings-uid" taken-fields)
         "could not read ConfigMap settings, which the release claims, so whether another writer manages it is unknown"))

(deftest an-unread-storage-listing-refuses-and-an-answered-one-proceeds
  (polar (refusal/kubectl-get {:type "secrets" :namespace "wiki" :args ["--selector=owner=helm,name=wiki"]})
         (storage-listing (:storage @(estate)))
         #"could not read what the change rests on"))

(deftest an-unread-release-payload-refuses-and-an-answered-one-proceeds
  (polar (refusal/kubectl-get {:type "secrets" :namespace "wiki" :name "sh.helm.release.v1.wiki.v2"})
         release-payload
         #"could not read what the change rests on - could not read secrets wiki/sh.helm.release.v1.wiki.v2"))

(deftest an-unread-owner-listing-refuses-and-an-answered-one-proceeds
  (polar (refusal/kubectl-get {:type flux/kustomization-type :namespace :all})
         {:items []}
         #"could not read what the change rests on"))
