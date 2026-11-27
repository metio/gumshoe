;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.listener-renames-test
  "gumshoe.listener-renames and gumshoe.steps.listener-renames: the rename
   plan over listeners and route attachments, the JSON patch that touches
   names alone, the post-check over present and unread routes, and the
   rename book run through gumshoe.mutation against a fake cluster whose
   controller accepts a route once its section exists. The negative
   controls show that each kind of test fails when the property it guards
   is broken."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.listener-renames :as renames]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]
            [gumshoe.steps.listener-renames :as steps]
            [gumshoe.testing.plan :as plan]
            [gumshoe.testing.refusal :as refusal]
            [gumshoe.verify :as verify]))

;; --- fixtures ------------------------------------------------------------------------

(def ^:private ls-type "listenersets.gateway.networking.k8s.io")
(def ^:private xls-type "xlistenersets.gateway.networking.x-k8s.io")
(def ^:private httproutes "httproutes.gateway.networking.k8s.io")
(def ^:private grpcroutes "grpcroutes.gateway.networking.k8s.io")
(def ^:private ref {:type ls-type :namespace "shop" :name "web"})
(def ^:private pick "ListenerSet shop/web")

(defn- listener
  [name hostname]
  {:name name :hostname hostname :port 443 :protocol "HTTPS"
   :tls {:mode "Terminate" :certificateRefs [{:kind "Secret" :name (str hostname "-tls")}]}})

(def ^:private issuer-annotations
  {:cert-manager.io/issuer "adopted-issuer" :cert-manager.io/issuer-kind "Issuer"})

(defn- listener-set
  [listeners & {:keys [namespace name generation] :or {namespace "shop" name "web" generation 1}}]
  {:apiVersion "gateway.networking.k8s.io/v1" :kind "ListenerSet"
   :metadata {:name name :namespace namespace :uid (str name "-uid") :generation generation
              :resourceVersion (str generation) :annotations issuer-annotations}
   :spec {:parentRef {:name "gateway" :namespace "infra"} :listeners listeners}})

(defn- parent
  [& {:keys [section kind group namespace name] :or {kind "ListenerSet" name "web"}}]
  (cond-> {:kind kind :name name}
    group (assoc :group group)
    namespace (assoc :namespace namespace)
    section (assoc :sectionName section)))

(defn- route
  [kind namespace name & parent-refs]
  {:apiVersion "gateway.networking.k8s.io/v1" :kind kind
   :metadata {:name name :namespace namespace}
   :spec {:parentRefs (vec parent-refs)}})

(defn- shop-route
  [section]
  (route "HTTPRoute" "shop" "web" (parent :section section)))

;; --- attachments ---------------------------------------------------------------------

(deftest a-parent-ref-attaches-only-with-the-kind-group-name-and-namespace-of-the-listener-set
  (is (renames/attaches? ref "shop" (parent)))
  (is (renames/attaches? ref "blog" (parent :namespace "shop")) "a route in another namespace names it explicitly")
  (is (renames/attaches? ref "shop" (parent :group "gateway.networking.k8s.io")))
  (is (not (renames/attaches? ref "blog" (parent))) "without a namespace the parentRef names blog/web")
  (is (not (renames/attaches? ref "shop" (parent :kind "Gateway"))))
  (is (not (renames/attaches? ref "shop" (dissoc (parent) :kind))) "kind defaults to Gateway")
  (is (not (renames/attaches? ref "shop" (parent :kind "XListenerSet"))))
  (is (not (renames/attaches? ref "shop" (parent :group "gateway.networking.x-k8s.io"))))
  (is (not (renames/attaches? ref "shop" (parent :name "api"))))
  (testing "the experimental kind attaches under its own group only"
    (let [xref (assoc ref :type xls-type)]
      (is (renames/attaches? xref "shop" (parent :kind "XListenerSet" :group "gateway.networking.x-k8s.io")))
      (is (not (renames/attaches? xref "shop" (parent :kind "XListenerSet")))))))

(deftest attachments-collect-every-section-of-every-route-that-names-the-listener-set
  (let [routes [(route "HTTPRoute" "shop" "web" (parent :section "https") (parent :section "http"))
                (route "GRPCRoute" "blog" "api" (parent :section "grpc" :namespace "shop"))
                (route "HTTPRoute" "blog" "other" (parent :section "https"))
                (route "HTTPRoute" "shop" "any" (parent))]]
    (is (= [{:route "HTTPRoute shop/web" :section "https"}
            {:route "HTTPRoute shop/web" :section "http"}
            {:route "GRPCRoute blog/api" :section "grpc"}
            {:route "HTTPRoute shop/any" :section nil}]
           (renames/attachments ref routes)))
    (is (= #{"https" "http" "grpc"} (renames/wanted (renames/attachments ref routes)))
        "a parentRef without a sectionName asks for no name")))

;; --- the plan ------------------------------------------------------------------------

(defn- plan-of
  [names sections]
  (renames/plan (mapv (fn [n] (if n {:name n} {})) names)
                (mapv (fn [s] {:route "HTTPRoute shop/web" :section s}) sections)))

(deftest a-numbered-listener-is-renamed-to-the-section-the-routes-ask-for
  (let [p (plan-of ["https-0"] ["https"])]
    (is (= [{:index 0 :from "https-0" :to "https"}] (:renames p)))
    (is (= ["https"] (:missing p)))
    (is (empty? (renames/problems ref p)))
    (is (not (renames/done? p)))))

(deftest listeners-that-already-match-need-nothing
  (let [p (plan-of ["https-0" "http"] ["https-0" "http"])]
    (is (renames/done? p))
    (is (empty? (:renames p)))
    (is (empty? (renames/problems ref p)))))

(deftest routes-without-a-section-name-ask-for-no-rename
  (let [p (plan-of ["https-0"] [nil])]
    (is (renames/done? p))
    (is (empty? (:renames p)))))

(deftest two-numbered-listeners-for-one-section-are-refused
  (let [p (plan-of ["https-0" "https-1"] ["https"])]
    (is (empty? (:renames p)))
    (is (= [{:to "https" :from ["https-0" "https-1"]}] (:ambiguous p)))
    (is (= ["ListenerSet shop/web carries the listeners \"https-0\" and \"https-1\" and the routes attach to \"https\": nothing outside the cluster can decide which one they mean, so rename one by hand"]
           (renames/problems ref p)))))

(deftest a-numbered-sibling-the-routes-use-still-makes-the-section-ambiguous
  ;; https-1 serves its own hostname; that the routes use it does not say
  ;; that https-0 is the listener they mean by https.
  (let [p (plan-of ["https-0" "https-1"] ["https" "https-1"])]
    (is (empty? (:renames p)))
    (is (= [{:to "https" :from ["https-0" "https-1"]}] (:ambiguous p)))
    (is (seq (renames/problems ref p)))))

(deftest a-candidate-the-routes-attach-to-is-held
  (let [p (plan-of ["https-0"] ["https" "https-0"])]
    (is (empty? (:renames p)))
    (is (= [{:to "https" :from "https-0"}] (:held p)))
    (is (re-find #"which renaming it would detach" (first (renames/problems ref p))))))

(deftest one-ambiguous-section-refuses-the-renames-of-every-other
  (let [p (plan-of ["http-0" "https-0" "https-1"] ["http" "https"])]
    (is (= [{:index 0 :from "http-0" :to "http"}] (:renames p)))
    (is (= 1 (count (renames/problems ref p))) "the whole plan is refused, not applied in part")))

(deftest several-sections-are-renamed-at-their-own-indices
  (let [p (plan-of ["other" "http-0" "https-3"] ["http" "https" "other"])]
    (is (= [{:index 1 :from "http-0" :to "http"} {:index 2 :from "https-3" :to "https"}] (:renames p)))
    (is (empty? (renames/problems ref p)))))

(deftest a-section-without-any-candidate-is-unmatched
  (testing "alone, it refuses: there is nothing to rename"
    (let [p (plan-of ["https-0"] ["grpc"])]
      (is (= ["grpc"] (:unmatched p)))
      (is (= ["the routes attach to \"grpc\" of ListenerSet shop/web, which no listener carries under that name or a numbered form of it, so there is nothing to rename"]
             (renames/problems ref p)))))
  (testing "beside a rename, the rename proceeds"
    (let [p (plan-of ["https-0"] ["https" "grpc"])]
      (is (= ["grpc"] (:unmatched p)))
      (is (= 1 (count (:renames p))))
      (is (empty? (renames/problems ref p))))))

(deftest only-a-trailing-numeric-suffix-makes-a-candidate
  (is (= ["https"] (:unmatched (plan-of ["https-v2" "https0" "xhttps-0"] ["https"]))))
  (is (= [{:index 0 :from "https-12" :to "https"}] (:renames (plan-of ["https-12"] ["https"])))))

(deftest a-listener-without-a-name-is-skipped-and-indices-stay-live
  (is (= [{:index 1 :from "https-0" :to "https"}] (:renames (plan-of [nil "https-0"] ["https"])))))

;; --- the patch -----------------------------------------------------------------------

(defn- path-of
  [pointer]
  (mapv #(if (re-matches #"\d+" %) (parse-long %) (keyword %)) (rest (str/split pointer #"/"))))

(defn- json-patch
  "RFC 6902 test and replace over keyword maps; nil when a test fails."
  [object operations]
  (reduce (fn [o {:keys [op path value]}]
            (let [p (path-of path)]
              (case op
                "test" (if (= value (get-in o p)) o (reduced nil))
                "replace" (if (some? (get-in o p)) (assoc-in o p value) (reduced nil)))))
          object operations))

(deftest the-patch-tests-the-old-name-and-replaces-the-name-alone
  (let [renames [{:index 0 :from "https-0" :to "https"} {:index 2 :from "http-1" :to "http"}]]
    (is (= [{:op "test" :path "/spec/listeners/0/name" :value "https-0"}
            {:op "replace" :path "/spec/listeners/0/name" :value "https"}
            {:op "test" :path "/spec/listeners/2/name" :value "http-1"}
            {:op "replace" :path "/spec/listeners/2/name" :value "http"}]
           (renames/patch-operations renames)))
    (let [before (listener-set [(listener "https-0" "shop.example.org")
                                (listener "admin" "admin.example.org")
                                (listener "http-1" "shop.example.org")])
          after (json-patch before (renames/patch-operations renames))]
      (is (= ["https" "admin" "http"] (mapv :name (-> after :spec :listeners))))
      (is (= (update-in before [:spec :listeners] #(mapv (fn [l] (dissoc l :name)) %))
             (update-in after [:spec :listeners] #(mapv (fn [l] (dissoc l :name)) %)))
          "hostnames, ports, TLS and the annotations are byte for byte what they were"))))

(deftest a-listener-list-that-moved-fails-the-patch-instead-of-renaming-another-listener
  (let [moved (listener-set [(listener "admin" "admin.example.org") (listener "https-0" "shop.example.org")])]
    (is (nil? (json-patch moved (renames/patch-operations [{:index 0 :from "https-0" :to "https"}]))))
    (testing "negative control: without the test operation the wrong listener is renamed"
      (is (= "https" (-> (json-patch moved [{:op "replace" :path "/spec/listeners/0/name" :value "https"}])
                         :spec :listeners first :name))))))

(deftest the-effect-is-one-json-patch-on-the-listener-set
  (let [[op context & args] (renames/rename-effect "ctx" ref [{:index 0 :from "https-0" :to "https"}])]
    (is (= :kubectl op))
    (is (= "ctx" context))
    (is (= ["--namespace" "shop" "patch" ls-type "web" "--type=json" "--patch"] (butlast args)))
    (is (= 2 (count (json/parse-string (last args)))))))

;; --- picks ---------------------------------------------------------------------------

(deftest picks-parse-into-listener-set-refs
  (is (= ref (renames/parse-pick "ListenerSet shop/web")))
  (is (= (assoc ref :type xls-type) (renames/parse-pick "XListenerSet shop/web")))
  (doseq [bad ["Gateway shop/web" "shop/web" "ListenerSet web" "" nil]]
    (is (nil? (renames/parse-pick bad)) (pr-str bad))))

;; --- the fake cluster ----------------------------------------------------------------

(def ^:private served-by-default
  (into #{ls-type xls-type} renames/route-types))

(defn- group-of [type] (second (str/split type #"\." 2)))
(defn- plural-of [type] (first (str/split type #"\." 2)))

(defn- controller-status
  "The status a Gateway API controller would give route: per parentRef
   naming a ListenerSet the cluster holds, Accepted when that ListenerSet
   carries the section, NoMatchingParent when it does not."
  [objects route]
  (let [route-namespace (get-in route [:metadata :namespace])
        parents (for [pr (-> route :spec :parentRefs)
                      [[type ns name] ls] objects
                      :when (renames/kind-of-type type)
                      :when (renames/attaches? {:type type :namespace ns :name name} route-namespace pr)]
                  {:parentRef pr
                   :conditions [(if (or (nil? (:sectionName pr))
                                        (some #(= (:sectionName pr) (:name %)) (-> ls :spec :listeners)))
                                  {:type "Accepted" :status "True" :reason "Accepted"}
                                  {:type "Accepted" :status "False" :reason "NoMatchingParent"})]})]
    (assoc route :status {:parents (vec parents)})))

(defn- positionals
  [argv]
  (let [after (rest (drop-while #(not= "get" %) argv))]
    (vec (remove #(str/starts-with? % "-") after))))

(defn- namespace-flag
  [argv]
  (some #(when (str/starts-with? % "--namespace=") (subs % (count "--namespace="))) argv))

(defn- discovery
  [served path]
  (let [[_ group version] (re-matches #"/apis/([^/]+)(?:/([^/]+))?" path)
        types (filter #(= group (group-of %)) served)]
    (cond
      (empty? types) {:exit 1 :out "" :err "Error from server (NotFound): the server could not find the requested resource"}
      (nil? version) {:exit 0 :out (json/generate-string {:versions [{:version "v1"}]}) :err ""}
      :else {:exit 0 :out (json/generate-string {:resources (vec (for [t types] {:name (plural-of t)}))}) :err ""})))

(defn- answer
  "The answer of the fake cluster to argv, over the state map."
  [{:keys [objects answering? unanswered denied served frozen?] :or {served served-by-default}} argv]
  (cond
    (not answering?)
    {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"}

    (some #{"can-i"} argv)
    {:exit (if (some (set argv) (or denied #{})) 1 0) :out "" :err ""}

    (some #{"--raw"} argv)
    (discovery served (last argv))

    (some #{"get"} argv)
    (let [[type name] (positionals argv)
          ns (namespace-flag argv)
          status (if frozen? identity (partial controller-status objects))]
      (cond
        (contains? (or unanswered #{}) type)
        {:exit 1 :out "" :err "Error from server (Forbidden): forbidden"}

        (not (contains? served type))
        {:exit 1 :out "" :err (format "error: the server doesn't have a resource type \"%s\"" (plural-of type))}

        (nil? name)
        {:exit 0 :out (json/generate-string
                       {:items (vec (for [[[t _ _] o] (sort-by key objects) :when (= t type)]
                                      (if (renames/kind-of-type t) o (status o))))})
         :err ""}

        (get objects [type ns name])
        {:exit 0 :out (json/generate-string (get objects [type ns name])) :err ""}

        (some #{"--ignore-not-found"} argv)
        {:exit 0 :out "" :err ""}

        :else
        {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" type name)}))

    :else {:exit 0 :out "" :err ""}))

(defn- fake-cluster
  [state]
  (fn [& args]
    (let [argv (mapv str (filter string? (flatten args)))]
      (swap! state update :commands (fnil conj []) argv)
      (answer @state argv))))

(defn- key-of
  [o type]
  [type (get-in o [:metadata :namespace]) (get-in o [:metadata :name])])

(defn- cluster
  [ls & routes]
  (atom {:answering? true
         :objects (into {[ls-type "shop" "web"] ls}
                        (for [r routes] [(key-of r (if (= "GRPCRoute" (:kind r)) grpcroutes httproutes)) r]))}))

(defn- apply-effect!
  "Applies a kubectl JSON patch to the fake cluster, bumping generation and
   resourceVersion; false when its object is missing or a test fails."
  [state [op _context & args]]
  (if (not= :kubectl op)
    true
    (let [[_ ns _patch type name] args
          ops (json/parse-string (last args) true)
          k [type ns name]]
      (swap! state update :patched (fnil conj []) k)
      (if-let [patched (some-> (get-in @state [:objects k]) (json-patch ops))]
        (do (swap! state assoc-in [:objects k]
                   (-> patched
                       (update-in [:metadata :generation] inc)
                       (assoc-in [:metadata :resourceVersion] (str (inc (get-in patched [:metadata :generation]))))))
            true)
        false))))

(defn- run-book
  "Runs spec on target through mutation/run-on! against the fake cluster:
   flow/change! is replaced by one that records the confirmation, calls
   :between to play what happens while the operator reads it, judges the
   precondition, applies every effect and attempts each post-check once."
  [spec state target & {:keys [between]}]
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
                     (mutation/run-on! spec target {} nil))]
        {:result result :out (str out) :config @seen}))))

(defn- stored [state] (get-in @state [:objects [ls-type "shop" "web"]]))

(def ^:private adopted
  (listener-set [(listener "https-0" "shop.example.org")]))

;; --- the book through gumshoe.mutation ----------------------------------------------

(deftest the-book-renames-the-listener-the-routes-ask-for-and-verifies-the-routes
  (let [state (cluster adopted (shop-route "https") (route "GRPCRoute" "blog" "api" (parent :section "https" :namespace "shop")))
        {:keys [result config out]} (run-book steps/rename-picked state pick)]
    (is (true? result) out)
    (is (= [[ls-type "shop" "web"]] (:patched @state)) "one patch")
    (is (= ["https"] (mapv :name (-> (stored state) :spec :listeners))))
    (is (= (assoc-in adopted [:spec :listeners 0 :name] "https")
           (-> (stored state) (assoc-in [:metadata :generation] 1) (assoc-in [:metadata :resourceVersion] "1")))
        "the hostname, TLS and the issuer annotations are untouched")
    (is (= ["rename listener \"https-0\" of ListenerSet shop/web to \"https\", which HTTPRoute shop/web, GRPCRoute blog/api attach to"
            "patch only the listener names of ListenerSet shop/web: hostnames, TLS and annotations stay as they are"]
           (get-in config [:confirmation :items])))
    (is (= ["the routes attaching to \"https\" of ListenerSet shop/web are Accepted"]
           (mapv :description (:post-checks config))))))

(deftest the-book-refuses-an-ambiguous-host-before-the-confirmation
  (let [state (cluster (listener-set [(listener "https-0" "shop.example.org") (listener "https-1" "www.shop.example.org")])
                       (shop-route "https"))
        {:keys [result out config]} (run-book steps/rename-picked state pick)]
    (is (false? result))
    (is (nil? config) "refused before anything is asked")
    (is (str/includes? out "nothing outside the cluster can decide which one they mean") out)
    (is (empty? (:patched @state)))))

(deftest the-book-skips-a-listener-set-whose-routes-all-attach
  (let [state (cluster (listener-set [(listener "https" "shop.example.org")]) (shop-route "https"))
        {:keys [result out config]} (run-book steps/rename-picked state pick)]
    (is (true? result))
    (is (nil? config))
    (is (str/includes? out "skipped, already done") out)
    (is (empty? (:patched @state)))))

(deftest the-book-refuses-a-listener-set-whose-spec-changed-after-the-confirmation
  (let [state (cluster adopted (shop-route "https"))
        {:keys [result out]} (run-book steps/rename-picked state pick
                                       :between (fn [s]
                                                  (swap! s update-in [:objects [ls-type "shop" "web"]]
                                                         #(-> %
                                                              (update-in [:spec :listeners] (fn [ls] (into [(listener "admin" "admin.example.org")] ls)))
                                                              (assoc-in [:metadata :generation] 2)))))]
    (is (false? result))
    (is (re-find #"refusing: .*shop/web" out) out)
    (is (empty? (:patched @state)))))

(deftest the-book-refuses-when-a-route-moved-onto-the-listener-after-the-confirmation
  (let [state (cluster adopted (shop-route "https"))
        {:keys [result out]} (run-book steps/rename-picked state pick
                                       :between (fn [s]
                                                  (swap! s assoc-in [:objects [httproutes "blog" "old"]]
                                                         (route "HTTPRoute" "blog" "old" (parent :section "https-0" :namespace "shop")))))]
    (is (false? result))
    (is (str/includes? out "which renaming it would detach") out)
    (is (str/includes? out "changed since the plan was made") out)
    (is (empty? (:patched @state)))))

(deftest the-book-asks-the-patch-right-before-the-confirmation
  (let [state (cluster adopted (shop-route "https"))]
    (swap! state assoc :denied #{ls-type})
    (let [{:keys [result out config]} (run-book steps/rename-picked state pick)]
      (is (false? result))
      (is (nil? config))
      (is (str/includes? out "you may not patch listenersets.gateway.networking.k8s.io in namespace shop") out)
      (is (empty? (:patched @state))))))

(deftest the-book-refuses-when-a-route-type-does-not-answer
  (let [state (cluster adopted (shop-route "https"))]
    (swap! state assoc :unanswered #{grpcroutes})
    (let [{:keys [result out config]} (run-book steps/rename-picked state pick)]
      (is (false? result))
      (is (nil? config))
      (is (re-find #"could not read .*grpcroutes" out) out)
      (is (empty? (:patched @state))))))

(deftest route-types-the-cluster-does-not-serve-read-as-no-routes
  (let [state (cluster adopted (shop-route "https"))]
    (swap! state assoc :served #{ls-type httproutes})
    (let [{:keys [result out]} (run-book steps/rename-picked state pick)]
      (is (true? result) out)
      (is (= ["https"] (mapv :name (-> (stored state) :spec :listeners)))))))

(deftest the-experimental-kind-is-renamed-through-its-own-type
  (let [xls (assoc (listener-set [(listener "https-0" "shop.example.org")]) :kind "XListenerSet"
                   :apiVersion "gateway.networking.x-k8s.io/v1alpha1")
        state (atom {:answering? true
                     :served (disj served-by-default ls-type)
                     :objects {[xls-type "shop" "web"] xls
                               [httproutes "shop" "web"] (route "HTTPRoute" "shop" "web"
                                                                (parent :section "https" :kind "XListenerSet"
                                                                        :group "gateway.networking.x-k8s.io"))}})
        {:keys [result out]} (run-book steps/rename-picked state "XListenerSet shop/web")]
    (is (true? result) out)
    (is (= [[xls-type "shop" "web"]] (:patched @state)))
    (is (= "https" (get-in @state [:objects [xls-type "shop" "web"] :spec :listeners 0 :name])))))

(deftest a-rename-the-controller-does-not-accept-fails-its-post-check
  (let [state (cluster adopted (shop-route "https"))]
    (swap! state assoc :frozen? true)
    (let [{:keys [result]} (run-book steps/rename-picked state pick)]
      (is (false? result) "routes still not Accepted leave the post-check unheld")
      (is (= 1 (count (:patched @state)))))))

(deftest a-malformed-pick-is-refused-before-anything-is-patched
  (let [state (cluster adopted (shop-route "https"))
        {:keys [result out config]} (run-book steps/rename-picked state "Gateway shop/web")]
    (is (false? result))
    (is (nil? config))
    (is (str/includes? out "is not \"<Kind> <namespace>/<name>\" of a ListenerSet or XListenerSet") out)
    (is (empty? (:patched @state)))))

(deftest a-listener-set-that-does-not-exist-is-refused
  (let [state (cluster adopted (shop-route "https"))
        {:keys [result config out]} (run-book steps/rename-picked state "ListenerSet shop/gone")]
    (is (false? result))
    (is (nil? config))
    (is (str/includes? out "ListenerSet shop/gone does not exist") out)
    (is (empty? (:patched @state)))))

(deftest the-candidates-list-both-kinds
  (let [state (atom {:answering? true
                     :objects {[ls-type "shop" "web"] adopted
                               [xls-type "blog" "api"] (listener-set [] :namespace "blog" :name "api")}})]
    (with-redefs [shell/execute (fake-cluster state)]
      (is (= ["ListenerSet shop/web" "XListenerSet blog/api"] (renames/picks "ctx")))
      (swap! state assoc :served #{ls-type})
      (is (= ["ListenerSet shop/web"] (renames/picks "ctx")) "an unserved kind offers nothing")
      (swap! state assoc :answering? false)
      (is (thrown? clojure.lang.ExceptionInfo (renames/picks "ctx")) "an unanswered list is never an empty cluster"))))

;; --- the post-check ------------------------------------------------------------------

(deftest the-post-check-holds-only-when-every-route-on-a-renamed-section-is-accepted
  (let [renamed (listener-set [(listener "https" "shop.example.org")])
        objects {[ls-type "shop" "web"] renamed}
        accepted (controller-status objects (shop-route "https"))
        rejected (controller-status {} (shop-route "https"))
        list-of (fn [& rs] {:state (if (seq rs) :present :absent) :ref {} :items (vec rs)})
        unread {:state :unread :ref {} :reason "i/o timeout"}
        none (repeat 4 (list-of))]
    (is (true? (renames/accepted-results? ref ["https"] (cons (list-of accepted) none))))
    (is (false? (renames/accepted-results? ref ["https"] (cons (list-of accepted rejected) none))) "one route left behind")
    (is (false? (renames/accepted-results? ref ["https"] (cons (list-of) none))) "no route attaches")
    (is (false? (renames/accepted-results? ref ["https"] (cons (list-of accepted) (cons unread (rest none))))) "a type did not answer")
    (is (false? (renames/accepted-results? ref [] (cons (list-of accepted) none))) "nothing to verify is not verified")))

(deftest the-post-check-fails-closed
  (is (empty? (verify/fail-closed-findings [(renames/accepted-check "ctx" ref [{:index 0 :from "https-0" :to "https"}])]))))

;; --- plan coverage -------------------------------------------------------------------

(defn- with-cluster
  [state f]
  (with-redefs [shell/execute (fake-cluster state)] (f)))

(deftest the-dry-run-plan-lists-the-listener-set-the-effect-patches
  (with-cluster (cluster adopted (shop-route "https"))
    #(plan/covers! steps/rename-picked {:target pick})))

(deftest negative-control-items-that-leave-out-the-listener-set-fail-the-plan-check
  (with-cluster (cluster adopted (shop-route "https"))
    #(let [spec (assoc steps/rename-picked :items (constantly ["rename some listeners"]))
           {:keys [ok? missing]} (plan/check spec {:target pick})]
       (is (false? ok?))
       (is (= [ref] missing)))))

(deftest the-plan-reads-nothing-itself
  (with-cluster (cluster adopted (shop-route "https"))
    #(let [spec (assoc steps/rename-picked :effect (fn [{:keys [context] :as ctx}]
                                                     (renames/read-route-results context)
                                                     ((:effect steps/rename-picked) ctx)))
           {:keys [ok? failure]} (plan/check spec {:target pick})]
       (is (false? ok?) "negative control: an effect that reads is refused")
       (is (re-find #"reached outside the process" (first failure))))))

;; --- refusal polarity ----------------------------------------------------------------

(def ^:private polar-state
  {:answering? true
   :objects {[ls-type "shop" "web"] adopted
             [httproutes "shop" "web"] (shop-route "https")}})

(defn- rest-of-cluster
  [argv]
  (answer polar-state argv))

(deftest an-unanswered-route-list-refuses-and-an-answered-one-proceeds
  (refusal/is-polar steps/rename-picked
                    {:target pick
                     :read (refusal/kubectl-get {:type grpcroutes :namespace :all})
                     :answer {:items []}
                     :cluster rest-of-cluster
                     :refusal #"could not read .*grpcroutes"}))

(defn- can-i-patch
  [type]
  (with-meta (fn [argv] (and (some #{"can-i"} argv) (some #{type} argv)))
    {:description (str "kubectl auth can-i patch " type)}))

(deftest an-unanswered-rights-check-refuses-and-a-granted-one-proceeds
  (refusal/is-polar steps/rename-picked
                    {:target pick
                     :read (can-i-patch ls-type)
                     :answer {:exit 0 :out "" :err ""}
                     :cluster rest-of-cluster
                     :refusal "you may not patch listenersets.gateway.networking.k8s.io in namespace shop, which renaming its listeners needs"}))

(deftest negative-control-routes-read-laxly-fail-the-polarity
  (let [lax (fn [{:keys [context]}]
              (vec (mapcat #(or (:items %) []) (renames/read-route-results context))))
        spec (change/compose (dissoc steps/rename-picked :reads) {:reads {:routes lax}})
        {:keys [holds? failures]} (refusal/polarity spec
                                                    {:target pick
                                                     :read (refusal/kubectl-get {:type grpcroutes :namespace :all})
                                                     :answer {:items []}
                                                     :cluster rest-of-cluster
                                                     :refusal #"could not read .*grpcroutes"})]
    (is (false? holds?) "a route list that did not answer passes for an empty one")
    (is (seq failures))))
