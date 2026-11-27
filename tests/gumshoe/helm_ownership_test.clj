;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.helm-ownership-test
  "gumshoe.helm-ownership and gumshoe.steps.helm-ownership: the ownership
   states, the patches and plans, both post-check polarities over present,
   absent and unread objects, and the stamp and unstamp books run through
   gumshoe.mutation against a fake cluster that applies their patches. The
   negative controls show that each kind of test fails when the property
   it guards is broken."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.flow :as flow]
            [gumshoe.helm-ownership :as own]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.reads :as reads]
            [gumshoe.shell :as shell]
            [gumshoe.steps.helm-ownership :as steps]
            [gumshoe.testing.plan :as plan]
            [gumshoe.testing.refusal :as refusal]
            [gumshoe.verify :as verify]))

;; --- fixtures ------------------------------------------------------------------------

(def ^:private release "wiki")
(def ^:private release-ns "wiki")

(defn- object
  "An object with string-keyed metadata, as JSON reads it back."
  [{:keys [type namespace name]} & {:keys [labels annotations rv] :or {rv "1"}}]
  {"apiVersion" "v1"
   "kind" type
   "metadata" (cond-> {"name" name "uid" (str name "-uid") "resourceVersion" rv}
                namespace (assoc "namespace" namespace)
                labels (assoc "labels" labels)
                annotations (assoc "annotations" annotations))})

(def ^:private stamp-labels {own/managed-by-label "Helm"})
(def ^:private stamp-annotations {own/release-name-annotation release
                                  own/release-namespace-annotation release-ns})

(def ^:private settings {:type "configmaps" :namespace "wiki" :name "settings"})
(def ^:private data {:type "persistentvolumeclaims" :namespace "wiki" :name "data"})
(def ^:private tls {:type "secrets" :namespace "wiki" :name "tls"})
(def ^:private gone {:type "deployments" :namespace "wiki" :name "old"})
(def ^:private reader {:type "clusterroles" :namespace nil :name "wiki-reader"})
(def ^:private shared {:type "configmaps" :namespace "wiki" :name "shared"})

(defn- keywordized
  "o as kubectl's JSON reader hands it to a book."
  [o]
  (when o (kubectl/parse-json (json/generate-string o))))

(def ^:private bare-object (keywordized (object settings :labels {own/managed-by-label "kustomize"})))
(def ^:private partial-object (keywordized (object data :labels stamp-labels)))
(def ^:private stamped-object (keywordized (object tls :labels stamp-labels :annotations stamp-annotations)))
(def ^:private foreign-object (keywordized (object shared :labels stamp-labels
                                                   :annotations {own/release-name-annotation "forum"
                                                                 own/release-namespace-annotation "forum"})))

(defn- present [o] {:state :present :ref {} :object o})
(def ^:private absent {:state :absent :ref {}})
(def ^:private unread {:state :unread :ref {} :reason "i/o timeout"})

;; --- names and refs ------------------------------------------------------------------

(deftest names-that-kubectl-could-read-as-flags-are-refused
  (is (nil? (own/release-problem "wiki")))
  (is (some? (own/release-problem "-wiki")))
  (is (some? (own/release-problem "Wiki")))
  (is (some? (own/release-problem (apply str (repeat 54 "a")))))
  (is (some? (own/release-problem nil)))
  (is (nil? (own/namespace-problem "wiki")))
  (is (some? (own/namespace-problem "--all-namespaces")))
  (is (nil? (own/ref-problem settings)))
  (is (nil? (own/ref-problem reader)))
  (is (nil? (own/ref-problem {:type "clusterroles" :namespace nil :name "system:wiki:reader"})))
  (is (some? (own/ref-problem {:type "configmaps" :namespace "wiki" :name "--overwrite"})))
  (is (some? (own/ref-problem {:type "-f" :namespace "wiki" :name "x"})))
  (is (some? (own/ref-problem {:type "configmaps" :namespace "-n" :name "x"})))
  (is (some? (own/ref-problem nil))))

(deftest command-line-objects-parse-into-refs
  (is (= settings (own/parse-object "configmaps/settings" "wiki")))
  (is (= {:type "configmaps" :namespace "other" :name "x"} (own/parse-object "configmaps/other/x" "wiki")))
  (is (nil? (own/parse-object "configmaps" "wiki")))
  (is (nil? (own/parse-object "a/b/c/d" "wiki")))
  (is (= reader (own/parse-cluster-object "clusterroles/wiki-reader")))
  (is (nil? (own/parse-cluster-object "clusterroles/ns/x"))))

(deftest from-flags-keeps-every-unparsable-object-as-a-problem
  (let [r (steps/from-flags {:target "wiki"
                             :opts {:release "wiki"
                                    :object ["configmaps/settings" "nonsense"]
                                    :cluster-object ["clusterroles/wiki-reader" "a/b/c"]}})]
    (is (= [settings reader] (:objects r)))
    (is (= 2 (count (:problems r))))
    (is (str/includes? (first (:problems r)) "\"nonsense\""))))

(deftest a-request-with-nothing-to-stamp-is-a-problem
  (is (= ["no object was named"] (:problems (steps/request steps/from-flags {:target "wiki" :opts {:release "wiki"}}))))
  (is (= ["no release name was given"]
         (:problems (steps/request steps/from-flags {:target "wiki" :opts {:object ["configmaps/settings"]}}))))
  (is (= [settings] (:refs (steps/request steps/from-flags {:target "wiki" :opts {:release "wiki" :object ["configmaps/settings" "configmaps/settings"]}})))
      "a ref named twice is stamped once"))

;; --- ownership -----------------------------------------------------------------------

(deftest ownership-tells-the-four-states-apart
  (is (= :bare (own/ownership release release-ns bare-object)) "another manager's label is no stamp")
  (is (= :bare (own/ownership release release-ns (keywordized (object settings)))))
  (is (= :partial (own/ownership release release-ns partial-object)) "helm template output carries only the label")
  (is (= :stamped (own/ownership release release-ns stamped-object)))
  (is (= :foreign (own/ownership release release-ns foreign-object)))
  (testing "one annotation naming another release or namespace is enough to be foreign"
    (is (= :foreign (own/ownership release release-ns (keywordized (object settings :annotations {own/release-name-annotation "forum"})))))
    (is (= :foreign (own/ownership release release-ns (keywordized (object settings :annotations {own/release-name-annotation release
                                                                                                    own/release-namespace-annotation "elsewhere"}))))))
  (testing "this release's annotations without the label are partial"
    (is (= :partial (own/ownership release release-ns (keywordized (object settings :annotations stamp-annotations)))))))

;; --- patches and plans ---------------------------------------------------------------

(deftest the-stamp-is-one-merge-patch-and-the-strip-nulls-it
  (is (= {"metadata" {"labels" {own/managed-by-label "Helm"}
                      "annotations" stamp-annotations}}
         (json/parse-string (own/stamp-patch release release-ns))))
  (is (= {"metadata" {"labels" {own/managed-by-label nil}
                      "annotations" {own/release-name-annotation nil own/release-namespace-annotation nil}}}
         (json/parse-string own/unstamp-patch))))

(deftest a-stamp-plan-writes-only-the-objects-that-need-it
  (let [plan (own/stamp-plan "ctx" release release-ns
                             [settings data tls gone reader]
                             [bare-object partial-object stamped-object nil (keywordized (object reader))])]
    (is (= [[:kubectl "ctx" "--namespace" "wiki" "patch" "configmaps" "settings" "--type=merge" "--patch" (own/stamp-patch release release-ns)]
            [:kubectl "ctx" "--namespace" "wiki" "patch" "persistentvolumeclaims" "data" "--type=merge" "--patch" (own/stamp-patch release release-ns)]
            [:kubectl "ctx" "patch" "clusterroles" "wiki-reader" "--type=merge" "--patch" (own/stamp-patch release release-ns)]]
           plan)
        "the stamped object and the absent one get no effect; a cluster-scoped one gets no --namespace"))
  (is (= [[:note "every listed object that exists carries the stamp already"]]
         (own/stamp-plan "ctx" release release-ns [tls gone] [stamped-object nil]))))

(deftest a-plan-never-takes-an-object-from-another-release
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"belongs to release forum/forum"
                        (own/stamp-plan "ctx" release release-ns [settings shared] [bare-object foreign-object])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"belongs to release forum/forum"
                        (own/unstamp-plan "ctx" release release-ns [shared] [foreign-object]))))

(deftest an-unstamp-plan-strips-what-carries-part-of-the-stamp
  (is (= [[:kubectl "ctx" "--namespace" "wiki" "patch" "persistentvolumeclaims" "data" "--type=merge" "--patch" own/unstamp-patch]
          [:kubectl "ctx" "--namespace" "wiki" "patch" "secrets" "tls" "--type=merge" "--patch" own/unstamp-patch]]
         (own/unstamp-plan "ctx" release release-ns [settings data tls gone] [bare-object partial-object stamped-object nil]))
      "a bare object keeps its own manager's label"))

(deftest a-malformed-ref-never-reaches-kubectl
  (is (thrown? IllegalArgumentException
               (own/stamp-effect "ctx" release release-ns {:type "configmaps" :namespace "wiki" :name "--all"})))
  (is (thrown? IllegalArgumentException
               (own/stamp-effect "ctx" "-r" release-ns settings))))

(deftest the-foreign-refusal-names-the-owner
  (is (nil? (own/foreign-refusal release release-ns [settings tls gone] [bare-object stamped-object nil])))
  (is (= "configmaps wiki/shared (release forum/forum) already belongs to another Helm release; take it from that release first, never by overwriting its stamp"
         (own/foreign-refusal release release-ns [settings shared] [bare-object foreign-object]))))

;; --- the two post-check polarities ---------------------------------------------------

(deftest stamped-holds-only-when-every-object-was-read-exists-and-is-stamped
  (is (true? (own/stamped-results? release release-ns [(present stamped-object)])))
  (is (false? (own/stamped-results? release release-ns [(present stamped-object) unread])) "unread")
  (is (false? (own/stamped-results? release release-ns [(present stamped-object) absent])) "absent")
  (is (false? (own/stamped-results? release release-ns [(present partial-object)])))
  (is (false? (own/stamped-results? release release-ns [(present foreign-object)])))
  (is (false? (own/stamped-results? release release-ns [])) "nothing read is nothing stamped")
  (is (false? (own/stamped-results? release release-ns [nil])) "a malformed result is unread"))

(deftest no-stamp-holds-only-when-every-read-answered-and-none-is-stamped
  (is (true? (own/no-stamp-results? release release-ns [(present bare-object) absent])))
  (is (true? (own/no-stamp-results? release release-ns [(present foreign-object)])) "another release's stamp is not this release's")
  (is (false? (own/no-stamp-results? release release-ns [(present bare-object) unread])) "unread")
  (is (false? (own/no-stamp-results? release release-ns [(present partial-object)])))
  (is (false? (own/no-stamp-results? release release-ns [(present stamped-object)])))
  (is (false? (own/no-stamp-results? release release-ns [nil])) "a malformed result is unread"))

(deftest an-unread-object-satisfies-neither-polarity
  (doseq [results [[unread] [(present stamped-object) unread] [absent unread]]]
    (is (false? (own/stamped-results? release release-ns results)))
    (is (false? (own/no-stamp-results? release release-ns results)))
    (is (true? (not (own/stamped-results? release release-ns results)))
        "the negation of stamped? holds on silence, which is why a strip does not judge with it")))

;; --- collection, through gumshoe.reads at the shell boundary ------------------------

(defn- positional-after-get
  [argv]
  (let [after (rest (drop-while #(not= "get" %) argv))]
    (vec (remove #(str/starts-with? % "-") after))))

(defn- namespace-flag
  [argv]
  (some #(when (str/starts-with? % "--namespace=") (subs % (count "--namespace="))) argv))

(defn- fake-cluster
  "A shell/execute answering kubectl get and auth can-i from state, an atom
   of {:objects {[type namespace name] object} :answering? :denied #{type}}."
  [state]
  (fn [& args]
    (let [argv (mapv str (filter string? (flatten args)))
          {:keys [objects answering? denied]} @state]
      (swap! state update :commands (fnil conj []) argv)
      (cond
        (not answering?)
        {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"}

        (some #{"can-i"} argv)
        {:exit (if (some (set argv) (or denied #{})) 1 0) :out "" :err ""}

        (some #{"get"} argv)
        (let [[type name] (positional-after-get argv)
              ns (namespace-flag argv)]
          (cond
            (nil? name)
            {:exit 0 :out (json/generate-string {:items (vec (for [[[t _ _] o] objects :when (= t type)] o))}) :err ""}

            (get objects [type ns name])
            {:exit 0 :out (json/generate-string (get objects [type ns name])) :err ""}

            (some #{"--ignore-not-found"} argv)
            {:exit 0 :out "" :err ""}

            :else
            {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" type name)}))

        :else {:exit 0 :out "" :err ""}))))

(defn- cluster-of
  [& pairs]
  (atom {:answering? true
         :objects (into {} (for [[{:keys [type namespace name]} o] (partition 2 pairs)]
                             [[type namespace name] o]))}))

(deftest stamped-and-no-stamp-read-the-cluster-through-gumshoe-reads
  (let [state (cluster-of tls (object tls :labels stamp-labels :annotations stamp-annotations)
                          settings (object settings))]
    (with-redefs [shell/execute (fake-cluster state)]
      (is (true? (own/stamped? "ctx" release release-ns [tls])))
      (is (false? (own/stamped? "ctx" release release-ns [tls settings])))
      (is (false? (own/stamped? "ctx" release release-ns [tls gone])) "an object that does not exist is not stamped")
      (is (true? (own/no-stamp-confirmed? "ctx" release release-ns [settings gone])))
      (is (false? (own/no-stamp-confirmed? "ctx" release release-ns [settings tls])))
      (swap! state assoc :answering? false)
      (is (false? (own/stamped? "ctx" release release-ns [tls])) "unanswered: not stamped")
      (is (false? (own/no-stamp-confirmed? "ctx" release release-ns [settings])) "unanswered: no strip confirmed"))))

(deftest the-post-checks-fail-closed
  (is (empty? (verify/fail-closed-findings
               [(own/stamped-check "ctx" release release-ns [tls settings])
                (own/unstamped-check "ctx" release release-ns [tls settings])]))
      "neither holds on a cluster that does not answer; only the strip holds where nothing exists")
  (testing "negative control: a strip check that does not declare it verifies an absence is caught"
    (is (seq (verify/fail-closed-findings
              [(dissoc (own/unstamped-check "ctx" release release-ns [tls]) :verifies-absence?)])))))

;; --- the books through gumshoe.mutation ---------------------------------------------

(defn- merge-patch
  "RFC 7386 over string-keyed maps: a null deletes a key."
  [target patch]
  (if (map? patch)
    (reduce-kv (fn [acc k v]
                 (if (nil? v)
                   (dissoc acc k)
                   (assoc acc k (merge-patch (get acc k) v))))
               (if (map? target) target {})
               patch)
    patch))

(defn- apply-effect!
  "Applies a kubectl patch effect to the fake cluster, bumping the object's
   resourceVersion; true when it found its object."
  [state [op _context & args]]
  (if (not= :kubectl op)
    true
    (let [args (vec args)
          ns (when (= "--namespace" (first args)) (second args))
          [_patch type name] (if ns (subvec args 2 5) (subvec args 0 3))
          patch (json/parse-string (last args))
          k [type ns name]]
      (swap! state update :patched (fnil conj []) k)
      (if (get-in @state [:objects k])
        (do (swap! state update-in [:objects k]
                   #(-> (merge-patch % patch)
                        (update-in ["metadata" "resourceVersion"] (fn [rv] (str (inc (parse-long rv)))))))
            true)
        false))))

(defn- run-book
  "Runs spec on target with opts through mutation/run-on! against the fake
   cluster: flow/change! is replaced by one that records the confirmation,
   calls :between (a function of the state) to play what happens while the
   operator reads it, judges the precondition, applies every effect to the
   cluster and attempts each post-check once."
  [spec state target opts & {:keys [between]}]
  (let [seen (atom nil)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  shell/execute (fake-cluster state)
                  flow/change! (fn [{:keys [precondition effect post-checks] :as config}]
                                 (reset! seen config)
                                 (when between (between state))
                                 (and (if precondition (precondition) true)
                                      (every? #(apply-effect! state %) effect)
                                      (every? #(true? (verify/attempt (:check %))) post-checks)))]
      (let [out (java.io.StringWriter.)
            result (binding [*out* out *err* out]
                     (mutation/run-on! spec target opts nil))]
        {:result result :out (str out) :config @seen}))))

(def ^:private stamp-opts
  {:release "wiki"
   :object ["configmaps/settings" "persistentvolumeclaims/data" "secrets/tls" "deployments/old"]
   :cluster-object ["clusterroles/wiki-reader"]})

(defn- estate
  []
  (cluster-of settings (object settings :labels {own/managed-by-label "kustomize"})
              data (object data :labels stamp-labels)
              tls (object tls :labels stamp-labels :annotations stamp-annotations)
              reader (object reader)))

(defn- stored [state ref] (keywordized (get-in @state [:objects [(:type ref) (:namespace ref) (:name ref)]])))

(deftest the-stamp-book-stamps-every-existing-object-and-verifies-it
  (let [state (estate)
        {:keys [result config out]} (run-book steps/stamp-named state "wiki" stamp-opts)]
    (is (true? result) out)
    (is (= #{["configmaps" "wiki" "settings"] ["persistentvolumeclaims" "wiki" "data"] ["clusterroles" nil "wiki-reader"]}
           (set (:patched @state)))
        "the stamped Secret and the absent Deployment are not written")
    (is (every? #(= :stamped (own/ownership release release-ns (stored state %))) [settings data tls reader]))
    (is (= "kustomize" (:managed-by (own/stamp-of bare-object))) "the fixture had another manager")
    (is (= ["stamp configmaps wiki/settings as owned by Helm release wiki/wiki (replacing managed-by=kustomize)"
            "stamp persistentvolumeclaims wiki/data as owned by Helm release wiki/wiki"
            "secrets wiki/tls carries the stamp of wiki/wiki already"
            "deployments wiki/old does not exist - helm install creates it"
            "stamp clusterroles wiki-reader as owned by Helm release wiki/wiki"]
           (get-in config [:confirmation :items])))
    (is (= 1 (count (:post-checks config))))))

(deftest the-stamp-book-asks-the-patch-rights-before-the-confirmation
  (let [state (estate)]
    (swap! state assoc :denied #{"clusterroles"})
    (let [{:keys [result out config]} (run-book steps/stamp-named state "wiki" stamp-opts)]
      (is (false? result))
      (is (nil? config) "refused before the confirmation")
      (is (str/includes? out "you may not patch clusterroles across the cluster"))
      (is (empty? (:patched @state))))))

(deftest the-stamp-book-refuses-to-take-another-releases-object
  (let [state (estate)]
    (swap! state assoc-in [:objects ["configmaps" "wiki" "shared"]]
           (object shared :labels stamp-labels :annotations {own/release-name-annotation "forum"
                                                             own/release-namespace-annotation "forum"}))
    (let [{:keys [result out config]} (run-book steps/stamp-named state "wiki"
                                                (update stamp-opts :object conj "configmaps/shared"))]
      (is (false? result))
      (is (nil? config))
      (is (str/includes? out "configmaps wiki/shared (release forum/forum) already belongs to another Helm release"))
      (is (empty? (:patched @state))))))

(deftest the-stamp-book-refuses-a-stamp-that-moved-after-the-confirmation
  (let [state (estate)
        {:keys [result out]} (run-book steps/stamp-named state "wiki" stamp-opts
                                       :between (fn [s]
                                                  (swap! s update-in [:objects ["configmaps" "wiki" "settings"]]
                                                         #(assoc-in % ["metadata" "annotations"]
                                                                    {own/release-name-annotation "forum"}))))]
    (is (false? result))
    (is (re-find #"refusing: .*configmaps wiki/settings" out) out)
    (is (empty? (:patched @state)))))

(deftest the-stamp-book-refuses-an-adoptee-created-after-the-confirmation
  (let [state (estate)
        {:keys [result out]} (run-book steps/stamp-named state "wiki" stamp-opts
                                       :between (fn [s]
                                                  (swap! s assoc-in [:objects ["deployments" "wiki" "old"]] (object gone))))]
    (is (false? result))
    (is (str/includes? out "deployments wiki/old: it was created after the confirmation") out)
    (is (empty? (:patched @state)))))

(deftest the-stamp-book-refuses-a-malformed-request-before-reading-anything-it-names
  (doseq [[opts why] [[(dissoc stamp-opts :release) "no release name was given"]
                      [(assoc stamp-opts :object ["configmaps/--all"]) "\"--all\" is not an object name"]
                      [(assoc stamp-opts :object ["nonsense"]) "--object \"nonsense\" is not"]
                      [{:release "wiki"} "no object was named"]]]
    (let [state (estate)
          {:keys [result out config]} (run-book steps/stamp-named state "wiki" opts)]
      (is (false? result) why)
      (is (nil? config))
      (is (str/includes? out why) out)
      (is (empty? (:patched @state))))))

(deftest a-stamp-already-in-place-is-skipped-as-done
  (let [state (cluster-of tls (object tls :labels stamp-labels :annotations stamp-annotations))
        {:keys [result out config]} (run-book steps/stamp-named state "wiki"
                                              {:release "wiki" :object ["secrets/tls" "deployments/old"]})]
    (is (true? result))
    (is (nil? config) "nothing is asked")
    (is (str/includes? out "skipped, already done") out)
    (is (empty? (:patched @state)))))

(deftest a-stamp-that-did-not-take-fails-its-post-check
  (let [state (estate)
        {:keys [result]} (with-redefs [own/stamp-patch (constantly "{}")]
                           (run-book steps/stamp-named state "wiki" stamp-opts))]
    (is (false? result) "an effect that wrote nothing leaves the post-check unheld")))

(deftest the-unstamp-book-strips-the-stamp-and-confirms-it
  (let [state (estate)
        {:keys [result config out]} (run-book steps/unstamp-named state "wiki" stamp-opts)]
    (is (true? result) out)
    (is (= #{["persistentvolumeclaims" "wiki" "data"] ["secrets" "wiki" "tls"]} (set (:patched @state))))
    (is (every? #(= :bare (own/ownership release release-ns (stored state %))) [settings data tls reader]))
    (is (= "kustomize" (:managed-by (own/stamp-of (stored state settings)))) "another manager's label stays")
    (is (true? (:verifies-absence? (first (:post-checks config)))))))

(deftest an-unstamp-with-nothing-stamped-is-skipped-as-done
  (let [state (cluster-of settings (object settings))
        {:keys [result config]} (run-book steps/unstamp-named state "wiki" {:release "wiki" :object ["configmaps/settings"]})]
    (is (true? result))
    (is (nil? config))))

;; --- plan coverage -------------------------------------------------------------------

(defn- with-cluster
  [state f]
  (with-redefs [shell/execute (fake-cluster state)] (f)))

(deftest the-dry-run-plan-lists-every-object-the-effect-touches
  (with-cluster (estate)
    #(do (plan/covers! steps/stamp-named {:target "wiki" :opts stamp-opts})
         (plan/covers! steps/unstamp-named {:target "wiki" :opts stamp-opts}))))

(deftest negative-control-items-that-leave-out-an-object-fail-the-plan-check
  (with-cluster (estate)
    #(let [spec (assoc steps/stamp-named :items (fn [ctx] (butlast ((:items steps/stamp-named) ctx))))
           {:keys [ok? missing]} (plan/check spec {:target "wiki" :opts stamp-opts})]
       (is (false? ok?))
       (is (= [{:type "clusterroles" :namespace nil :name "wiki-reader"}] missing)))))

(deftest the-plan-reads-nothing-itself
  (with-cluster (estate)
    #(let [spec (assoc steps/stamp-named :effect (fn [{:keys [context] :as ctx}]
                                                   (kubectl/get-json context "namespaces")
                                                   ((:effect steps/stamp-named) ctx)))
           {:keys [ok? failure]} (plan/check spec {:target "wiki" :opts stamp-opts})]
       (is (false? ok?) "negative control: an effect that reads is refused")
       (is (re-find #"reached outside the process" (first failure))))))

;; --- refusal polarity ----------------------------------------------------------------

(def ^:private one-object-opts {:release "wiki" :object ["configmaps/settings"]})

(def ^:private settings-read
  (refusal/kubectl-get {:type "configmaps" :namespace "wiki" :name "settings"}))

(defn- can-i
  [type]
  (with-meta (fn [argv] (and (some #{"can-i"} argv) (some #{type} argv)))
    {:description (str "kubectl auth can-i patch " type)}))

(deftest an-unread-adoptee-refuses-and-an-answered-one-proceeds
  (refusal/is-polar steps/stamp-named
                    {:target "wiki"
                     :opts one-object-opts
                     :read settings-read
                     :answer (object settings)
                     :cluster [[(can-i "configmaps") {:exit 0 :out "" :err ""}]]
                     :refusal #"could not read"}))

(deftest an-unanswered-rights-check-refuses-and-a-granted-one-proceeds
  (refusal/is-polar steps/stamp-named
                    {:target "wiki"
                     :opts one-object-opts
                     :read (can-i "configmaps")
                     :answer {:exit 0 :out "" :err ""}
                     :cluster [[settings-read (object settings)]]
                     :refusal "you may not patch configmaps in namespace wiki, which writing the stamp needs"}))

(deftest the-strip-refuses-on-an-unread-object-too
  (refusal/is-polar steps/unstamp-named
                    {:target "wiki"
                     :opts one-object-opts
                     :read settings-read
                     :answer (object settings :labels stamp-labels :annotations stamp-annotations)
                     :cluster [[(can-i "configmaps") {:exit 0 :out "" :err ""}]]
                     :refusal #"could not read"}))

(deftest negative-control-a-refusal-that-ignores-the-rights-fails-the-polarity
  (let [spec (change/compose (dissoc steps/stamp-named :refusal)
                             {:refusal (fn [{:keys [refs objects]}]
                                         (own/foreign-refusal release release-ns refs objects))})
        {:keys [holds? failures]} (refusal/polarity spec
                                                    {:target "wiki"
                                                     :opts one-object-opts
                                                     :read (can-i "configmaps")
                                                     :answer {:exit 0 :out "" :err ""}
                                                     :cluster [[settings-read (object settings)]]
                                                     :refusal "you may not patch configmaps in namespace wiki, which writing the stamp needs"})]
    (is (false? holds?))
    (is (seq failures))))

(deftest negative-control-adoptees-read-laxly-fail-the-polarity
  (let [spec (-> steps/stamp-named
                 (dissoc :subjects)
                 (assoc :derive (fn [{:keys [context] :as ctx}]
                                  (let [refs (:refs (steps/request steps/from-flags ctx))]
                                    {:adoptees refs
                                     :found (mapv #(when (reads/present? %) (reads/object! %))
                                                  (own/read-objects context refs))}))))
        {:keys [holds? failures]} (refusal/polarity spec
                                                    {:target "wiki"
                                                     :opts one-object-opts
                                                     :read settings-read
                                                     :answer (object settings)
                                                     :cluster [[(can-i "configmaps") {:exit 0 :out "" :err ""}]]
                                                     :refusal #"could not read"})]
    (is (false? holds?) "a read that did not answer passes for an absent adoptee")
    (is (some #(str/includes? % "does not answer") failures) (pr-str failures))))
