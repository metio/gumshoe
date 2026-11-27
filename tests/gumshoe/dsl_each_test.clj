;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dsl-each-test
  "gumshoe.dsl's [:each <read> {..} <effect> ..], :pin and :problems: what
   they compile to, what compile refuses, and whole runs through
   gumshoe.mutation/run-on! against a fake cluster that answers every kubectl
   call and applies patches and deletes, shaped like a stamp of the objects
   a chart adopts in place and a purge of the leftovers it supersedes."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.change :as change]
            [gumshoe.conformance :as conformance]
            [gumshoe.dsl :as dsl]
            [gumshoe.dsl.registry :as registry]
            [gumshoe.interact :as interact]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.reads]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.testing.plan :as plan]
            [gumshoe.testing.refusal :as refusal]
            [gumshoe.verify :as verify]))

(registry/register! :gumshoe.dsl-each-test/namespaces
                    {:fn (fn [_context] ["shop"]) :kind :candidates :args ["context"]
                     :doc "the namespaces a test offers"})

(registry/register! :gumshoe.dsl-each-test/release-problems
                    {:fn (fn [release] (if (= "moodle" release) [] [(str "no release named " release)]))
                     :kind :problems :args ["release"]
                     :doc "why a test release is not the one to stamp for"})

(registry/register! :gumshoe.dsl-each-test/broken-problems
                    {:fn (fn [_] nil) :kind :problems :args ["release"]
                     :doc "a :problems function returning no list"})

;; --- the changes ---------------------------------------------------------------------

(def ^:private routes "httproutes.gateway.networking.k8s.io")
(def ^:private database "databases.kinda.rocks")

(def ^:private stamped?
  [:and
   [:field [:metadata :labels :app.kubernetes.io/managed-by] := "Helm"]
   [:field [:metadata :annotations :meta.helm.sh/release-name] := "moodle"]
   [:field [:metadata :annotations :meta.helm.sh/release-namespace] := [:target]]])

(def ^:private stamp
  "Stamps Helm's ownership onto the adoptees present in a namespace, and
   pins the Database whose secret must not move during the confirmation."
  {:gumshoe/dsl :v0-experimental
   :description "Stamps Helm ownership onto the objects a chart adopts in place"
   :target {:pick :one :label "namespace" :type "namespaces" :flag :namespace
            :candidates [:call :gumshoe.dsl-each-test/namespaces [:context]]}
   :prerequisites {}
   :reads {:adoptees [:present [{:type "persistentvolumeclaims" :name "data"}
                                {:type database :name "moodle"}
                                {:type "ingresses" :name "moodle"}
                                {:type "listenersets.gateway.networking.x-k8s.io" :name "moodle"}
                                {:type routes :name "moodle"}
                                {:type routes :name "moodle-redirects"}]
                      {:namespace [:target]}]}
   :pin [{:type database :namespace [:target] :name "moodle"}]
   :confirm {:action "stamp Helm ownership onto the adoptees" :destructive? false}
   :announce false
   :effect [[:each :adoptees
             {:state {:holds stamped? :says "{each.metadata.name} carries Helm's ownership" :timeout 30 :interval 5}}
             [:kubectl :patch {:merge {:metadata {:labels {"app.kubernetes.io/managed-by" "Helm"}
                                                  :annotations {"meta.helm.sh/release-name" "moodle"
                                                                "meta.helm.sh/release-namespace" [:target]}}}}]]]})

(def ^:private purge
  "Deletes the leftovers of an allow-list that are actually there."
  {:gumshoe/dsl :v0-experimental
   :description "Deletes the ansible leftovers the chart supersedes"
   :target {:pick :one :label "namespace" :type "namespaces" :flag :namespace
            :candidates [:call :gumshoe.dsl-each-test/namespaces [:context]]}
   :prerequisites {}
   :reads {:leftovers [:present [{:type "services" :name "moodle-tideways"}
                                 {:type "configmaps" :name "mdl-settings"}
                                 {:type "configmaps" :name "moodle-webserver"}]
                       {:namespace [:target]}]}
   :confirm {:action "delete the ansible leftovers"}
   :announce false
   :recovery "a deleted leftover is gone; the ansible role recreates it"
   :effect [[:each :leftovers [:kubectl :delete]]]})

;; --- the fake cluster ----------------------------------------------------------------

(defn- object
  [type namespace name & {:as extra}]
  (merge-with merge
              {:kind type
               :metadata (cond-> {:name name :uid (str "uid-" name) :resourceVersion "1"}
                           namespace (assoc :namespace namespace))}
              extra))

(def ^:private helm-owned
  {:metadata {:labels {:app.kubernetes.io/managed-by "Helm"}
              :annotations {:meta.helm.sh/release-name "moodle" :meta.helm.sh/release-namespace "shop"}}})

(defn- shop
  "The namespace with its adoptees: a claim, the Database and two routes;
   no Ingress and no ListenerSet. extra are more objects."
  [& extra]
  (into {}
        (map (fn [o] [[(:kind o) (get-in o [:metadata :namespace]) (get-in o [:metadata :name])] o]))
        (concat [(object "namespaces" nil "shop")
                 (object "persistentvolumeclaims" "shop" "data")
                 (object database "shop" "moodle" :metadata {:generation 1} :spec {:secretName "moodle-db-credentials"})
                 (object routes "shop" "moodle")
                 (object routes "shop" "moodle-redirects")]
                extra)))

(defn- leftovers
  []
  (shop (object "services" "shop" "moodle-tideways")
        (object "configmaps" "shop" "mdl-settings")))

(defn- parse-argv
  [argv]
  (reduce (fn [acc arg]
            (if-let [[_ k v] (re-matches #"--([^=]+)(?:=(.*))?" arg)]
              (assoc-in acc [:flags k] (or v true))
              (update acc :words conj arg)))
          {:flags {} :words []}
          (rest argv)))

(def ^:private unanswered refusal/unanswered)

(defn- deep-merge
  [a b]
  (if (and (map? a) (map? b)) (merge-with deep-merge a b) b))

(defn- bump
  [o]
  (update-in o [:metadata :resourceVersion] #(str (inc (parse-long %)))))

(defn- respond!
  [state log argv]
  (let [{:keys [flags words]} (parse-argv argv)
        [verb type name] words
        ns (get flags "namespace")
        k [type ns name]
        {:keys [objects silent]} @state]
    (case verb
      "get" (cond
              (some #(% [type ns name]) silent) unanswered
              name (if-let [o (get objects k)]
                     {:exit 0 :out (json/generate-string o) :err ""}
                     (if (get flags "ignore-not-found")
                       {:exit 0 :out "" :err ""}
                       (refusal/not-found type name)))
              :else {:exit 0
                     :out (json/generate-string {:items (vec (for [[[t n] o] (map (fn [[[t n _] o]] [[t n] o]) objects)
                                                                    :when (and (= t type) (= n ns))]
                                                                o))})
                     :err ""})
      "patch" (do (swap! log conj [:patch type ns name])
                  (if (contains? objects k)
                    (do (swap! state update-in [:objects k]
                               #(bump (deep-merge % (json/parse-string (get flags "patch") true))))
                        {:exit 0 :out "patched" :err ""})
                    (refusal/not-found type name)))
      "delete" (do (swap! log conj [:delete type ns name])
                   (if (or (contains? objects k) (get flags "ignore-not-found"))
                     (do (swap! state update :objects dissoc k)
                         {:exit 0 :out "deleted" :err ""})
                     (refusal/not-found type name)))
      {:exit 1 :out "" :err (str "the fake cluster does not answer " (pr-str argv))})))

(defn- run-change
  "Runs dsl compiled against the namespace shop. scenario: :objects the
   cluster, :opts the parsed flags, :silent predicates over [type namespace
   name] whose reads do not answer, :between (fn [objects] -> objects) applied while the operator
   confirms. Returns the result, what was printed, the confirmation items,
   the mutations, the post-checks and the objects left."
  [dsl {:keys [objects silent between opts] :or {between identity opts {}}}]
  (let [spec (dsl/compile dsl)
        state (atom {:objects objects :silent silent})
        log (atom [])
        confirmations (atom [])
        checks (atom [])
        eventually verify/eventually
        out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        shell! (fn [& argv] (respond! state log (vec argv)))
        streamed (fn [& argv] (:exit (apply shell! argv)))]
    (with-redefs-fn
      {#'shell/execute shell!
       #'shell/run-attributed streamed
       #'shell/run-with-output streamed
       #'interact/confirm! (fn [request]
                             (swap! confirmations conj (vec (:items request)))
                             (swap! state update :objects between)
                             true)
       #'announce/announce! (constantly nil)
       #'verify/eventually (fn [check]
                             (swap! checks conj (:description check))
                             (eventually (assoc check :timeout 0)))}
      (fn []
        (let [result (binding [*out* out *err* err
                               outcome/*record* (outcome/fresh)
                               kube-context/*pinned* {:context "ctx" :cluster "cluster"}]
                       (mutation/run-on! spec "shop" opts nil))]
          {:result result
           :printed (stdout/strip-colors (str out err))
           :confirmations @confirmations
           :mutations @log
           :checks @checks
           :objects (:objects @state)})))))

(defn- refusing?
  [{:keys [printed]} words]
  (str/includes? printed (str "refusing: " words)))

;; --- what it compiles to -------------------------------------------------------------

(deftest an-each-compiles-to-a-snapshot-a-refusal-a-skip-and-a-post-check-per-object
  (let [c (dsl/compile stamp)]
    (is (= c (change/normalize c)) "normalize finds nothing to change")
    (is (= :pick (:from (first (:subjects c)))) "the namespace is still the pinned subject")
    (is (= :target (:from (second (:subjects c)))) "the :pin is a subjects entry the engine reads and pins")
    (is (= [{:type database :name "moodle" :namespace "shop"}]
           ((:refs (second (:subjects c))) {:target "shop" :ref {:type "namespaces" :name "shop"}})))
    (is (fn? (:done c)) "done when no object is left to act on")
    (is (contains? (:values c) :gumshoe.dsl/each-0) "the confirmed objects are a value, computed once")
    (is (false? (get-in c [:confirm :destructive?])))
    (is (= {:subject-pin :on :strict-reads :on :post-check :subject-not-acted-on :done :on
            :destructive-confirmation :not-destructive :recovery :not-destructive :pins :on
            :each [{:over [:adoptees] :max 25 :pin :on :post-check :on :grown :refused}]}
           (dsl/safeguards-of c)))
    (is (= [] (conformance/contract-problems c)))
    (is (= [] (conformance/guard-problems c))))
  (let [c (dsl/compile purge)]
    (is (true? (get-in c [:confirm :destructive?])) "an :each that deletes makes the change destructive")
    (is (change/destructive-effect? (:effect c)) "and marks its effect, so no composition drops it")
    (is (= [] (conformance/guard-problems c)))))

(deftest the-post-checks-of-an-each-fail-closed
  (doseq [[dsl what] [[stamp "a stamp"] [purge "a purge"]]]
    (let [c (dsl/compile dsl)
          members [{:ref {:type "configmaps" :namespace "shop" :name "x"}
                    :object (object "configmaps" "shop" "x")}]
          ctx {:context "ctx" :target "shop" :object (object "namespaces" nil "shop")
               :ref {:type "namespaces" :name "shop"} :gumshoe.dsl/each-0 members}
          checks ((:verify c) ctx)]
      (is (= 1 (count checks)) what)
      (is (= [] (verify/fail-closed-findings checks)) what))))

;; --- what compile refuses ------------------------------------------------------------

(defn- refusal-of
  [dsl]
  (try (dsl/compile dsl) nil
       (catch clojure.lang.ExceptionInfo e
         (when (dsl/invalid? e) {:path (:path (ex-data e)) :message (ex-message e)}))))

(def ^:private refused
  [[(assoc purge :effect [[:each :nope [:kubectl :delete]]]) [:effect 0 1] ":nope names no declared :reads entry - the declared names are :leftovers"]
   [(-> purge (assoc :values {:snap [:opt :namespace]}) (assoc :effect [[:each :snap [:kubectl :delete]]]))
    [:effect 0 1] ":snap is a :values entry, computed once and never read again"]
   [(assoc purge :effect [[:each "leftovers" [:kubectl :delete]]]) [:effect 0 1] "names the :reads entry"]
   [(assoc purge :effect [[:each [:leftovers :leftovers] [:kubectl :delete]]]) [:effect 0 1] "each :reads entry once"]
   [(assoc purge :effect [[:each :leftovers]]) [:effect 0] "one or more effects after its read"]
   [(assoc purge :effect [[:each :leftovers {:max 3}]]) [:effect 0] "one or more effects after its read"]
   [(assoc purge :effect [[:each :leftovers {:max 0} [:kubectl :delete]]]) [:effect 0 2 :max] "above zero"]
   [(assoc purge :effect [[:each :leftovers {:max 2.5} [:kubectl :delete]]]) [:effect 0 2 :max] "above zero"]
   [(assoc purge :effect [[:each :leftovers {:limit 3} [:kubectl :delete]]]) [:effect 0 2 :limit] "not a key of the options of an :each"]
   [(assoc purge :effect [[:each :leftovers [:each :leftovers [:kubectl :delete]]]]) [:effect 0 2] "not with another :each"]
   [(assoc purge :effect [[:each :leftovers [:kubectl :cordon]]]) [:effect 0 2 1] "no kubectl verb"]
   [(assoc purge :effect [[:each :leftovers [:kubectl :patch {:merge {:metadata {:labels {"a" "b"}}}}]]]) [:effect 0 2 :state]
    "an :each that does not delete says what holds for each object"]
   [(assoc purge :effect [[:each :leftovers {:state {:holds [:field [:metadata :name] := "x"] :says "x"}} [:kubectl :delete]]])
    [:effect 0 2 :state :holds] "brings about an absence"]
   [(assoc purge :effect [[:each :leftovers {:state {:holds :absent}} [:kubectl :delete]]]) [:effect 0 2 :state :says] "non-blank"]
   [(assoc stamp :effect [[:each :adoptees {:state {:holds [:field [:metadata :name] := [:read :adoptees]] :says "x"}}
                           [:kubectl :patch {:merge {}}]]])
    nil nil]
   [(assoc-in stamp [:effect 0 3 2 :merge :metadata :labels "x"] [:each :metadata :name]) nil nil]
   [(assoc stamp :announce "{each.metadata.name}") [:announce] "[:each :metadata :name] is not available in this place"]
   [(assoc stamp :effect [[:kubectl :patch {:merge {:metadata {:labels {"a" [:each :metadata :name]}}}}]] :opt-out {:verify "x is here"})
    [:effect 0 2 :merge :metadata :labels "a" 0] "is not available in this place"]
   [(assoc stamp :reads {:adoptees [:present []]}) [:reads :adoptees 1] "one or more objects"]
   [(assoc stamp :reads {:adoptees [:present [{:type "pods"}]]}) [:reads :adoptees 1 0 :name] "names itself as :name"]
   [(assoc stamp :reads {:adoptees [:present [{:type "pods" :name "x" :kind "Pod"}]]}) [:reads :adoptees 1 0 :kind] "not a key of an object of [:present ..]"]
   [(assoc stamp :pin []) [:pin] "one or more objects other than the subject"]
   [(assoc stamp :pin [{:type database}]) [:pin 0 :name] "names itself as :name"]
   [(assoc stamp :pin [{:type database :name "moodle" :compare :status}]) [:pin 0 :compare] ":status"]
   [(assoc stamp :pin [{:type database :name [:read :adoptees]}]) [:pin 0 :name 0] "not available in a :pin entry"]
   [(assoc stamp :problems []) [:problems] "one or more"]
   [(assoc stamp :problems [{:when [:state] :says "x"}]) [:problems 0 :when] "[:state] is not available"]
   [(assoc stamp :problems [{:reasons [:call :gumshoe.workloads/scaled? [:object] 2]}]) [:problems 0 :reasons 1] "is a :predicate function, and this place takes :problems"]
   [(assoc stamp :problems [{:reasons "no" :says "x"}]) [:problems 0] "not both"]
   [(assoc stamp :opt-out {:verify "each object has its own check"}) [:opt-out :verify] "acts only through :each"]])

(deftest every-malformed-each-pin-and-problems-is-refused-precisely
  (doseq [[dsl path words] refused]
    (let [r (refusal-of dsl)]
      (if (nil? path)
        (is (nil? r) (str "accepted: " (pr-str (get-in dsl [:effect 0])) " - " (:message r)))
        (testing (pr-str path words)
          (is (= path (:path r)) (:message r))
          (is (str/includes? (str (:message r)) words) (:message r)))))))

;; --- a stamp, end to end -------------------------------------------------------------

(deftest a-stamp-patches-every-adoptee-present-and-proves-each
  (let [{:keys [result confirmations mutations checks objects]} (run-change stamp {:objects (shop)})]
    (is (true? result))
    (is (= [[:patch "persistentvolumeclaims" "shop" "data"]
            [:patch database "shop" "moodle"]
            [:patch routes "shop" "moodle"]
            [:patch routes "shop" "moodle-redirects"]]
           mutations)
        "the absent Ingress and ListenerSet are not acted on")
    (is (= [["shop"
             "patch persistentvolumeclaims shop/data"
             (str "patch " database " shop/moodle")
             (str "patch " routes " shop/moodle")
             (str "patch " routes " shop/moodle-redirects")]]
           confirmations)
        "the confirmation shows every object acted on")
    (is (= ["data carries Helm's ownership" "moodle carries Helm's ownership"
            "moodle carries Helm's ownership" "moodle-redirects carries Helm's ownership"]
           checks)
        "one post-check per object")
    (is (= "Helm" (get-in objects [[routes "shop" "moodle-redirects"] :metadata :labels :app.kubernetes.io/managed-by])))))

(deftest a-stamp-skips-each-object-already-stamped
  (let [{:keys [result confirmations mutations]}
        (run-change stamp {:objects (update (shop) ["persistentvolumeclaims" "shop" "data"] deep-merge helm-owned)})]
    (is (true? result))
    (is (not-any? #(= "data" (last %)) mutations) "the stamped claim is not patched again")
    (is (= 4 (count (first confirmations))) "nor listed"))
  (testing "every adoptee stamped: the change is done and asks nothing"
    (let [{:keys [result confirmations mutations printed]}
          (run-change stamp {:objects (into {} (map (fn [[k o]] [k (if (= "namespaces" (first k)) o (deep-merge o helm-owned))])) (shop))})]
      (is (true? result))
      (is (empty? confirmations))
      (is (empty? mutations))
      (is (str/includes? printed "skipped, already done")))))

(deftest a-stamp-refuses-an-adoptee-that-moved-or-vanished-after-the-confirmation
  (testing "changed"
    (let [r (run-change stamp {:objects (shop) :between #(update % [routes "shop" "moodle"] bump)})]
      (is (false? (:result r)))
      (is (refusing? r (str routes " shop/moodle: it changed after the confirmation")) (:printed r))
      (is (empty? (:mutations r)))))
  (testing "vanished: a patch planned for it would fail, so the change is refused"
    (let [r (run-change stamp {:objects (shop) :between #(dissoc % [routes "shop" "moodle-redirects"])})]
      (is (false? (:result r)))
      (is (refusing? r (str routes " shop/moodle-redirects: it was deleted after the confirmation, and the patch planned for it would fail")) (:printed r))
      (is (empty? (:mutations r)))))
  (testing "appeared"
    (let [ingress (object "ingresses" "shop" "moodle")
          r (run-change stamp {:objects (shop) :between #(assoc % ["ingresses" "shop" "moodle"] ingress)})]
      (is (false? (:result r)))
      (is (refusing? r "ingresses shop/moodle: it appeared after the confirmation, which did not show it") (:printed r))
      (is (empty? (:mutations r))))))

(def ^:private stamp-routes
  "A stamp of the routes only, still pinning the Database it does not touch."
  (assoc stamp :reads {:adoptees [:present [{:type routes :name "moodle"} {:type routes :name "moodle-redirects"}]
                                  {:namespace [:target]}]}))

(deftest a-pin-on-an-object-other-than-the-subject-refuses-when-it-moved-or-is-absent
  (testing "control: the Database unchanged, the routes are stamped"
    (let [r (run-change stamp-routes {:objects (shop)})]
      (is (true? (:result r)))
      (is (= 2 (count (:mutations r))))))
  (testing "the Database's spec changed while the operator confirmed"
    (let [r (run-change stamp-routes {:objects (shop)
                                      :between #(update % [database "shop" "moodle"]
                                                        (fn [o] (-> o (assoc-in [:spec :secretName] "renamed")
                                                                    (update-in [:metadata :generation] inc) bump)))})]
      (is (false? (:result r)))
      (is (refusing? r (str database " shop/moodle: its spec changed after the confirmation")) (:printed r))
      (is (empty? (:mutations r)))))
  (testing "the Database does not exist: refused before the confirmation"
    (let [r (run-change stamp-routes {:objects (dissoc (shop) [database "shop" "moodle"])})]
      (is (false? (:result r)))
      (is (refusing? r (str database " shop/moodle: it does not exist")) (:printed r))
      (is (empty? (:confirmations r))))))

;; --- a purge, end to end -------------------------------------------------------------

(deftest a-purge-deletes-the-leftovers-present-and-proves-each-gone
  (let [{:keys [result confirmations mutations checks objects]} (run-change purge {:objects (leftovers)})]
    (is (true? result))
    (is (= [[:delete "services" "shop" "moodle-tideways"] [:delete "configmaps" "shop" "mdl-settings"]] mutations))
    (is (= [["shop" "delete services shop/moodle-tideways" "delete configmaps shop/mdl-settings"]] confirmations))
    (is (= ["services shop/moodle-tideways is gone" "configmaps shop/mdl-settings is gone"] checks))
    (is (not (contains? objects ["configmaps" "shop" "mdl-settings"])))))

(deftest an-empty-list-is-done-and-an-unread-one-refuses
  (testing "nothing left: done, nothing asked"
    (let [r (run-change purge {:objects (shop)})]
      (is (true? (:result r)))
      (is (empty? (:confirmations r)))
      (is (str/includes? (:printed r) "skipped, already done"))))
  (testing "a read that did not answer is not an empty list"
    (let [r (run-change purge {:objects (leftovers) :silent [#(= ["configmaps" "shop" "moodle-webserver"] %)]})]
      (is (false? (:result r)))
      (is (empty? (:confirmations r)))
      (is (empty? (:mutations r)))
      (is (str/includes? (:printed r) "could not read") (:printed r)))))

(deftest a-purge-refuses-a-leftover-that-appeared-and-skips-one-that-vanished
  (testing "appeared after the confirmation"
    (let [r (run-change purge {:objects (leftovers)
                               :between #(assoc % ["configmaps" "shop" "moodle-webserver"] (object "configmaps" "shop" "moodle-webserver"))})]
      (is (false? (:result r)))
      (is (refusing? r "configmaps shop/moodle-webserver: it appeared after the confirmation") (:printed r))
      (is (empty? (:mutations r)))))
  (testing "vanished after the confirmation: a delete of it already happened"
    (let [r (run-change purge {:objects (leftovers) :between #(dissoc % ["configmaps" "shop" "mdl-settings"])})]
      (is (true? (:result r)) (:printed r))
      (is (= ["services shop/moodle-tideways is gone" "configmaps shop/mdl-settings is gone"] (:checks r))))))

(deftest a-list-past-its-bound-is-refused-before-the-confirmation
  (let [bounded (assoc-in purge [:effect 0] [:each :leftovers {:max 1} [:kubectl :delete]])
        r (run-change bounded {:objects (leftovers)})]
    (is (false? (:result r)))
    (is (refusing? r "the :each over :leftovers read 2 objects, more than the 1 one confirmation may act on") (:printed r))
    (is (empty? (:confirmations r))))
  (testing "control: within its bound it runs"
    (let [r (run-change (assoc-in purge [:effect 0] [:each :leftovers {:max 2} [:kubectl :delete]]) {:objects (leftovers)})]
      (is (true? (:result r))))))

(deftest the-grown-list-test-would-catch-a-refusal-that-compares-nothing
  (testing "a negative control: without the :each refusal, a leftover that appeared is not refused"
    (with-redefs-fn {#'gumshoe.dsl/each-refusal (constantly nil)}
      #(let [r (run-change purge {:objects (leftovers)
                                  :between (fn [o] (assoc o ["configmaps" "shop" "moodle-webserver"]
                                                          (object "configmaps" "shop" "moodle-webserver")))})]
         (is (true? (:result r)))))))

;; --- problems ------------------------------------------------------------------------

(deftest problems-print-every-message-and-refuse
  (let [dsl (assoc purge
                   :options {:release {:desc "the release" :coerce :string}}
                   :problems [{:reasons [:call :gumshoe.dsl-each-test/release-problems [:opt :release]]}
                              {:when [:nil? [:opt :release]] :says "name the release with --release"}])
        spec (dsl/compile dsl)]
    (is (nil? (:problems spec)) "normalize folds :problems into the :refusal")
    (let [r (run-change dsl {:objects (leftovers)})]
      (is (false? (:result r)))
      (is (refusing? r "no release named") (:printed r))
      (is (refusing? r "name the release with --release") (:printed r))
      (is (empty? (:confirmations r)))))
  (testing "a :problems function returning no list refuses, naming it"
    (let [r (run-change (assoc purge :problems [{:reasons [:call :gumshoe.dsl-each-test/broken-problems "x"]}])
                        {:objects (leftovers)})]
      (is (false? (:result r)))
      (is (str/includes? (:printed r) "broken-problems returned nil") (:printed r))
      (is (empty? (:confirmations r)))))
  (testing "control: the release named, nothing stands in the way"
    (let [r (run-change (assoc purge
                               :options {:release {:desc "the release" :coerce :string}}
                               :problems [{:reasons [:call :gumshoe.dsl-each-test/release-problems [:opt :release]]}])
                        {:objects (leftovers) :opts {:release "moodle"}})]
      (is (seq (:confirmations r)) (:printed r)))))

;; --- plan coverage and refusal polarity ----------------------------------------------

(defn- stubbed-cluster
  [objects]
  (let [state (atom {:objects objects})]
    (fn [& argv] (respond! state (atom []) (vec argv)))))

(deftest the-dry-run-plan-lists-every-object-an-each-touches
  (doseq [[dsl objects] [[stamp (shop)] [purge (leftovers)]]]
    (with-redefs-fn {#'shell/execute (stubbed-cluster objects)}
      #(binding [kube-context/*pinned* {:context "ctx" :cluster "cluster"}]
         (let [{:keys [touched]} (plan/covers! (dsl/compile dsl) {:target "shop"})]
           (is (seq touched)))))))

(deftest an-unread-adoptee-refuses-and-an-answered-one-proceeds
  (let [cluster (into [] (for [[[type ns name] o] (shop)
                               :when (not= [routes "shop" "moodle"] [type ns name])]
                           [(refusal/kubectl-get {:type type :namespace ns :name name}) o]))
        absent (for [[type name] [["ingresses" "moodle"] ["listenersets.gateway.networking.x-k8s.io" "moodle"]]]
                 [(refusal/kubectl-get {:type type :namespace "shop" :name name}) refusal/absent])]
    (refusal/is-polar
     (dsl/compile stamp)
     {:target "shop"
      :read (refusal/kubectl-get {:type routes :namespace "shop" :name "moodle"})
      :answer (get (shop) [routes "shop" "moodle"])
      :cluster (into cluster absent)
      :refusal #"could not read .*moodle"})))

;; --- other reads an :each acts on ----------------------------------------------------

(registry/register! :gumshoe.dsl-each-test/leftover-results
                    {:fn (fn [context namespace]
                           [(gumshoe.reads/object context {:type "services" :namespace namespace :name "moodle-tideways"})
                            (gumshoe.reads/listing context {:type "configmaps" :namespace namespace})])
                     :kind :read :args ["context" "namespace"]
                     :doc "a test read returning gumshoe.reads results"})

(deftest an-each-acts-on-a-list-read-and-on-results-a-registered-read-returns
  (testing "[:list ..]"
    (let [r (run-change (assoc purge :reads {:leftovers [:list "configmaps" {:namespace [:target]}]}) {:objects (leftovers)})]
      (is (true? (:result r)) (:printed r))
      (is (= [[:delete "configmaps" "shop" "mdl-settings"]] (:mutations r)))))
  (testing "a registered :read returning gumshoe.reads results"
    (let [r (run-change (assoc purge :reads {:leftovers [:call :gumshoe.dsl-each-test/leftover-results [:context] [:target]]})
                        {:objects (leftovers)})]
      (is (true? (:result r)) (:printed r))
      (is (= #{[:delete "services" "shop" "moodle-tideways"] [:delete "configmaps" "shop" "mdl-settings"]} (set (:mutations r))))))
  (testing "a registered :read whose result did not answer refuses"
    (let [r (run-change (assoc purge :reads {:leftovers [:call :gumshoe.dsl-each-test/leftover-results [:context] [:target]]})
                        {:objects (leftovers) :silent [#(= ["configmaps" "shop" nil] %)]})]
      (is (false? (:result r)))
      (is (empty? (:confirmations r)))
      (is (str/includes? (:printed r) "could not read") (:printed r)))))

(deftest a-change-acting-on-its-subject-and-on-each-object-is-judged-by-both
  (let [c (dsl/compile (assoc stamp
                              :state {:holds [:field [:metadata :labels :adopted] := "true"] :says "{target} is marked adopted"}
                              :effect (conj (:effect stamp) [:kubectl :patch {:merge {:metadata {:labels {"adopted" "true"}}}}])))
        ctx {:context "ctx" :target "shop" :ref {:type "namespaces" :name "shop"}
             :object (object "namespaces" nil "shop")
             :gumshoe.dsl/each-0 [{:ref {:type routes :namespace "shop" :name "moodle"} :object (object routes "shop" "moodle")}]}]
    (is (fn? (:done c)))
    (is (= ["shop is marked adopted" "moodle carries Helm's ownership"] (map :description ((:verify c) ctx)))
        "the subject's post-check and each object's")
    (is (= 2 (count ((:effect c) ctx))) "one patch of the route, one of the namespace")
    (is (= :on (:post-check (dsl/safeguards-of c))))))

(deftest the-fail-closed-test-would-catch-an-each-post-check-that-always-holds
  (with-redefs-fn {#'gumshoe.dsl/each-checks (fn [_ _] [{:description "x" :timeout 1 :interval 1 :check (constantly true)}])}
    #(let [c (dsl/compile purge)
           checks ((:verify c) {:context "ctx" :target "shop" :gumshoe.dsl/each-0 []})]
       (is (seq (verify/fail-closed-findings checks))))))
