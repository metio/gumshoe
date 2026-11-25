;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.firebook-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing are]]
            [gumshoe.change :as change]
            [gumshoe.conformance :as conformance]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.runbook :as runbook]
            [gumshoe.detectives.evpn :as evpn-detectives]
            [gumshoe.detectives.pods :as pods]
            [gumshoe.effect :as effect]
            [gumshoe.evpn :as evpn]
            [gumshoe.firebook :as firebook]
            [gumshoe.verify :as verify]))

(deftest manifest-test
  (testing "every drill resource lives in the drill namespace"
    (is (= firebook/drill-namespace
           (-> (firebook/deployment-manifest {:name "crash-loop" :image "img"})
               :metadata :namespace)))
    (is (= firebook/drill-namespace
           (-> (firebook/pvc-manifest {:name "pvc" :storage-class "none"})
               :metadata :namespace))))
  (testing "a command is only set when given"
    (is (= ["sh" "-c" "exit 1"]
           (-> (firebook/deployment-manifest {:name "x" :image "img" :command ["sh" "-c" "exit 1"]})
               :spec :template :spec :containers first :command)))
    (is (nil? (-> (firebook/deployment-manifest {:name "x" :image "img"})
                  :spec :template :spec :containers first :command))))
  (testing "the pvc requests the broken storage class"
    (is (= "none"
           (-> (firebook/pvc-manifest {:name "pvc" :storage-class "none"})
               :spec :storageClassName)))))

(deftest fabric-drill-values-cannot-collide-with-a-real-fabric
  (testing "the MAC derives from RFC 5737 documentation space, so no guest can hold it"
    (is (= "02:00:cb:00:71:01" firebook/drill-mac))
    (is (str/starts-with? firebook/drill-mac "02:00:") "still an OpenNebula-shaped guest MAC"))
  (testing "the destination is not a VTEP anywhere, so nothing is attracted to it"
    (is (= "192.0.2.1" firebook/drill-vtep))))

(deftest fabric-drill-command-assembly
  (testing "replace, so relighting a burning drill is not an error"
    (is (= ["sudo" "bridge" "fdb" "replace" "02:00:cb:00:71:01" "dev" "lo.4208"
            "dst" "192.0.2.1" "self" "static"]
           (firebook/fabric-pin-args 4208))))
  (testing "putting it out names the same device and MAC"
    (is (= ["sudo" "bridge" "fdb" "del" "02:00:cb:00:71:01" "dev" "lo.4208"]
           (firebook/fabric-unpin-args 4208))))
  (testing "the drill entry is what the fabric detective reports"
    (let [row (str firebook/drill-mac " dev lo.4208 dst " firebook/drill-vtep " self static\n")
          rows (vec (evpn/parse-fdb 4208 "host-a" row))
          findings (evpn-detectives/detect-static-fdb {"evpn-fdb" rows})]
      (is (= 1 (count findings)))
      (is (= :warning (:severity (first findings))))
      (is (str/includes? (:summary (first findings)) firebook/drill-vtep)))))

;; --- the detectives prove the fire --------------------------------------------

(defn- pod
  [namespace reason]
  {:metadata {:namespace namespace :name "crash-loop-7d9f8-abcde"
              :ownerReferences [{:kind "ReplicaSet" :name "crash-loop-7d9f8"}]}
   :status {:phase "Running"
            :containerStatuses [(if reason
                                  {:name "crash-loop" :restartCount 4
                                   :state {:waiting {:reason reason}}}
                                  {:name "crash-loop" :restartCount 0
                                   :state {:running {:startedAt "2026-01-01T00:00:00Z"}}})]}})

(def ^:private crash-loop-expect
  (:expect (firebook/drill-spec
            {:description "d" :drill "crash-loop" :fire "f"
             :manifests [(firebook/deployment-manifest {:name "crash-loop" :image "img"})]
             :expect {:detectives pods/detectives
                      :fire? (fn [{:keys [summary]} _] (str/includes? summary "CrashLoopBackOff"))}})))

(defn- fire-in
  [& pods]
  (vec (firebook/fire-findings crash-loop-expect {"pods" {:items (vec pods)} :now (java.time.Instant/now)} {})))

(deftest a-kubernetes-fire-is-what-its-detective-reports-in-the-drill-namespace
  (testing "a crash-looping drill pod is the fire"
    (is (= ["fire-drill/crash-loop-7d9f8-abcde"] (map :component (fire-in (pod "fire-drill" "CrashLoopBackOff"))))))
  (testing "a healthy drill pod is not"
    (is (empty? (fire-in (pod "fire-drill" nil)))))
  (testing "a real crash loop elsewhere on the cluster never stands in for the drill"
    (is (empty? (fire-in (pod "shop" "CrashLoopBackOff"))))))

(deftest a-detective-that-throws-is-not-a-fire-that-is-out
  (let [expect {:detectives [{:name "broken" :requires ["pods"] :detect (fn [_] (throw (ex-info "boom" {})))}]
                :fire? (constantly true)}]
    (is (thrown? clojure.lang.ExceptionInfo (doall (firebook/fire-findings expect {"pods" {:items [{}]}} {}))))
    (testing "so the out check does not hold"
      (is (false? (binding [*err* (java.io.StringWriter.)]
                    (verify/eventually (assoc (firebook/out-check (assoc expect :collect (constantly {"pods" {:items [{}]}})) {})
                                              :timeout 0))))))))

(deftest a-check-over-evidence-its-detective-read-nothing-from-waits-for-an-answer
  (let [expect {:detectives pods/detectives
                :collect (constantly {"pods" nil})
                :fire? (constantly true)}
        out (java.io.StringWriter.)
        held (binding [*out* out *err* out]
               (verify/eventually (assoc (firebook/out-check expect {}) :timeout 0)))]
    (is (false? held))
    (is (str/includes? (str out) "the pods, stuck-pods detectives find nothing to read"))))

(deftest fabric-evidence-from-a-host-that-did-not-answer-is-unread
  (with-redefs [evpn/collect-host (fn [_ vni] {:host "hv1" :vni vni :error "host is not reachable over ssh"})]
    (let [e (try (firebook/fabric-evidence {:host "hv1"} 4208) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (:gumshoe.kubectl/unread (ex-data e)))
      (is (= "hv1: host is not reachable over ssh" (ex-message e)))))
  (with-redefs [evpn/collect-host (fn [_ vni] {:host "hv1" :vni vni :fdb [{:mac "m"}] :macs []})]
    (is (= [{:mac "m"}] (get (firebook/fabric-evidence {:host "hv1"} 4208) "evpn-fdb")))))

;; --- the spec ------------------------------------------------------------------

(def ^:private kubernetes-spec
  {:description "d" :drill "crash-loop" :fire "a crash-looping deployment"
   :manifests [(firebook/deployment-manifest {:name "crash-loop" :image "img"})]
   :prerequisites {:kubectl-can-create ["deployments"]}
   :expect {:detectives pods/detectives :fire? (constantly true)}})

(deftest a-kubernetes-drill-plans-both-changes
  (let [{:keys [ignite extinguish]} (firebook/drill-spec kubernetes-spec)
        ctx {:context "ctx"}
        [[op context stdin & args]] (effect/collect (effect/as-plan ((:effect ignite) ctx)))]
    (testing "the ignite applies the namespace and the manifests as one list"
      (is (= [:kubectl-stdin "ctx" ["apply" "--filename" "-"]] [op context (vec args)]))
      (is (= ["Namespace" "Deployment"] (map :kind (:items (json/parse-string stdin true))))))
    (testing "the extinguish deletes the drill namespace without waiting, so a namespace stuck Terminating fails the bounded gone check instead of hanging kubectl"
      (is (= [[:kubectl "ctx" "delete" "namespace" "fire-drill" "--ignore-not-found" "--wait=false"]]
             (effect/collect (effect/as-plan ((:effect extinguish) ctx))))))))

(deftest a-kubernetes-drill-derives-what-it-needs
  (let [{:keys [options prerequisites]} (firebook/drill-spec kubernetes-spec)]
    (is (= {:extinguish {:desc "Put out the fire: delete the fire-drill namespace" :alias :e :coerce :boolean}} options))
    (is (= ["namespaces" "deployments"] (:kubectl-can-create prerequisites)))
    (is (= ["namespaces" "pods"] (:kubectl-can-get prerequisites)) "read access to what its detectives read")))

(deftest a-firebook-leaves-extinguish-to-the-firebook
  (is (thrown? clojure.lang.ExceptionInfo
               (firebook/drill-spec (assoc kubernetes-spec :options {:extinguish {:desc "x"}}))))
  (is (thrown? clojure.lang.ExceptionInfo
               (firebook/drill-spec (assoc kubernetes-spec :options {:everything {:desc "x" :alias :e}})))))

;; --- an empty or unanswered reading never puts the fire out ---------------------

(def ^:private fdb-detective
  {:name "evpn-static-fdb" :requires ["evpn-fdb"]
   :detect (fn [evidence] (for [{:keys [mac]} (get evidence "evpn-fdb") :when (= "fire" mac)]
                            {:component mac}))})

(def ^:private fdb-expect
  {:detectives [fdb-detective] :fire? (fn [{:keys [component]} _] (= "fire" component))})

(deftest an-empty-reading-is-never-evidence-the-fire-is-out
  (are [evidence expect-extra blind?]
       (= blind? (some? (firebook/empty-reading (merge fdb-expect expect-extra) evidence)))
    {"evpn-fdb" []}                    {}                                   true
    {"evpn-fdb" {:items []}}           {}                                   true
    {"evpn-fdb" nil}                   {}                                   true
    {}                                 {}                                   true
    {"evpn-fdb" [{:mac "guest"}]}      {}                                   false
    {"evpn-fdb" {:items [{}]}}         {}                                   false
    {"evpn-fdb" []}                    {:empty-is-out "the namespace is confirmed gone"} false))

(deftest an-empty-reading-names-the-detective-and-what-it-read
  (is (= "the evpn-static-fdb detective reads only an empty evpn-fdb, and an empty reading never shows the fire is out"
         (firebook/empty-reading fdb-expect {"evpn-fdb" []}))))

(deftest a-detective-with-several-reads-is-blind-only-when-all-are-empty
  (let [expect {:detectives [{:name "d" :requires ["a" "b"] :detect (constantly [])}]}]
    (is (some? (firebook/empty-reading expect {"a" [] "b" {:items []}})))
    (is (nil? (firebook/empty-reading expect {"a" [] "b" [{:x 1}]})))))

(defn- attempt-check
  "What one attempt of a firebook check over the evidence collect returns:
   true, false, or the message of a read that did not answer."
  [make-check expect collect]
  (verify/attempt (:check (make-check (assoc expect :collect collect) {}))))

(def ^:private unanswered-collect
  (fn [_] (throw (firebook/unread "hv1: host is not reachable over ssh"))))

(deftest the-out-check-holds-only-on-an-answered-reading-without-the-fire
  (are [collect expect-extra verdict]
       (= verdict (let [r (attempt-check firebook/out-check (merge fdb-expect expect-extra) collect)]
                    (if (string? r) :unread r)))
    (constantly {"evpn-fdb" [{:mac "guest"}]})               {} true
    (constantly {"evpn-fdb" [{:mac "guest"} {:mac "fire"}]}) {} false
    (constantly {"evpn-fdb" []})                             {} :unread
    (constantly {"evpn-fdb" nil})                            {} :unread
    unanswered-collect                                       {} :unread
    (constantly {"evpn-fdb" []})                             {:empty-is-out "confirmed gone"} true
    (constantly {"evpn-fdb" nil})                            {:empty-is-out "confirmed gone"} :unread
    unanswered-collect                                       {:empty-is-out "confirmed gone"} :unread))

(deftest the-fire-check-holds-only-on-an-answered-reading-with-the-fire
  (are [collect verdict]
       (= verdict (let [r (attempt-check firebook/fire-check fdb-expect collect)]
                    (if (string? r) :unread r)))
    (constantly {"evpn-fdb" [{:mac "fire"}]})  true
    (constantly {"evpn-fdb" [{:mac "guest"}]}) false
    (constantly {"evpn-fdb" []})               false
    (constantly {"evpn-fdb" nil})              :unread
    unanswered-collect                         :unread))

(deftest a-firebook-whose-out-check-could-not-fail-closed-is-refused
  (let [fabric {:description "d" :drill "x" :fire "f"
                :derive (fn [_] {:target "t" :system "s"})
                :ignite {:items (constantly ["i"]) :effect (constantly [])}
                :extinguish {:put-out "p" :items (constantly ["i"]) :effect (constantly [])}}]
    (are [expect refused?]
         (= refused? (try (firebook/drill-spec (assoc fabric :expect expect)) false
                          (catch clojure.lang.ExceptionInfo _ true)))
      fdb-expect                                                         false
      (assoc fdb-expect :empty-is-out "confirmed gone")                  false
      (assoc fdb-expect :empty-is-out "")                                true
      (assoc fdb-expect :empty-is-out "  ")                              true
      (assoc fdb-expect :empty-is-out true)                              true
      (assoc fdb-expect :detectives [(dissoc fdb-detective :requires)])  true
      (assoc fdb-expect :detectives [(dissoc fdb-detective :requires)]
             :empty-is-out "the collect throws on an empty table")       false)))

(deftest a-kubernetes-drill-states-why-its-empty-listing-is-out
  (is (str/includes? (get-in (firebook/drill-spec kubernetes-spec) [:expect :empty-is-out])
                     "namespace is gone")))

;; --- the drill namespace guard ----------------------------------------------------

(deftest a-drill-acts-only-on-a-namespace-a-drill-created
  (are [namespace refused?]
       (= refused? (some? (firebook/foreign-namespace namespace)))
    nil                                                                                 false
    (firebook/namespace-manifest)                                                       false
    {:metadata {:name "fire-drill"}}                                                    true
    {:metadata {:name "fire-drill" :labels {:team "payments"}}}                         true
    {:metadata {:name "fire-drill" :labels {(keyword "app.kubernetes.io/managed-by") "Helm"}}} true))

(deftest the-kubernetes-guard-reads-the-namespace-and-refuses-a-foreign-one
  (let [{:keys [reads refusal]} (firebook/drill-spec kubernetes-spec)
        read-args (atom nil)]
    (with-redefs [kubectl/get-cluster-resource (fn [& args] (reset! read-args args) {:metadata {:name "fire-drill"}})]
      (is (= {:metadata {:name "fire-drill"}} ((:drill-namespace reads) {:context "ctx"})))
      (is (= ["ctx" "namespaces" "fire-drill"] @read-args)))
    (is (nil? (refusal {:drill-namespace nil})))
    (is (str/includes? (refusal {:drill-namespace {:metadata {:name "fire-drill"}}}) "no drill created it"))))

;; --- every stage is a change spec the mutation engine runs ----------------------

(def ^:private firebook-files
  ["firebooks/kubernetes/crash_loop.clj"
   "firebooks/kubernetes/image_pull.clj"
   "firebooks/kubernetes/pending_pvc.clj"
   "firebooks/fabric/stale_fdb_entry.clj"])

(defn- firebook-spec
  "The spec a firebook file hands firebook/book."
  [file]
  (let [captured (atom nil)]
    (with-redefs [firebook/book (fn [spec] (reset! captured spec))]
      (binding [runbook/*loading* true] (load-file file)))
    @captured))

(deftest every-firebook-file-is-listed-here
  (is (= (set firebook-files)
         (set (map str (filter #(str/ends-with? % ".clj")
                               (map #(.getPath ^java.io.File %) (file-seq (java.io.File. "firebooks")))))))))

(deftest every-firebook-stage-is-a-change-spec-that-meets-the-contract
  (doseq [file firebook-files
          :let [drill (firebook/drill-spec (firebook-spec file))]
          [stage spec] (firebook/stages drill)]
    (testing (str file " " stage)
      (let [canonical (change/normalize spec)]
        (is (empty? (conformance/contract-problems canonical)))
        (is (empty? (conformance/guard-problems canonical)))
        (is (false? (get-in canonical [:confirm :destructive?])))
        (is (fn? (:connect canonical)) "acts on what the drill derives, never on an implicit context")
        (is (nil? (:done canonical)) "a drill is never skipped as already done")))))

(deftest conformance-judges-each-stage-a-firebook-hands-the-harness
  (doseq [file firebook-files]
    (is (= [(str "b :extinguish") (str "b :ignite")]
           (map :name (conformance/captured-specs 'b [[:firebook (firebook-spec file)]])))
        file))
  (testing "a stage missing its effect is a finding"
    (let [spec (-> (firebook-spec "firebooks/fabric/stale_fdb_entry.clj")
                   (update :ignite dissoc :effect))
          ignite (some #(when (str/ends-with? (:name %) ":ignite") %)
                       (conformance/captured-specs 'b [[:firebook spec]]))]
      (is (some #{"declares no :effect"} (conformance/spec-problems ignite))))))

(def ^:private fabric-opts {:host "hv1" :vni 4208})

(deftest a-firebook-runs-its-stage-through-the-mutation-engine-only
  (let [drill (firebook/drill-spec (firebook-spec "firebooks/fabric/stale_fdb_entry.clj"))]
    (are [opts action]
         (let [runs (atom [])]
           (with-redefs [mutation/run-on! (fn [spec target o _] (swap! runs conj [spec target o]) true)
                         flow/change! (fn [_] (throw (ex-info "a firebook never calls gumshoe.flow itself" {})))]
             (binding [*err* (java.io.StringWriter.) *out* (java.io.StringWriter.)]
               (firebook/run! drill opts nil)))
           (let [[[spec target o] & more] @runs]
             (and (empty? more)
                  (= "hv1 lo.4208" target)
                  (= opts o)
                  (= action (get-in spec [:confirm :action])))))
      fabric-opts                         "start a fire drill: a static fdb entry that outranks EVPN"
      (assoc fabric-opts :extinguish true) "put out the fire: remove the drill's fdb entry")))

(deftest a-drill-is-lit-only-after-a-real-ignite-proved-the-fire
  (are [opts ok dry-run? lit?]
       (let [lit (atom false)
             drill {:drill "d" :fire "f" :derive (fn [_] {:target "t" :system "s"})
                    :ignite {} :extinguish {} :expect fdb-expect
                    :lit (fn [_] (reset! lit true))}]
         (with-redefs [mutation/run-on! (constantly ok)]
           (binding [flow/*dry-run* dry-run?]
             (firebook/run! drill opts nil)))
         (= lit? @lit))
    {}                  true  false true
    {}                  false false false
    {}                  true  true  false
    {:extinguish true}  true  false false))

(deftest a-firebook-derives-its-context-once-per-run
  (let [calls (atom 0)
        drill {:drill "d" :fire "f" :derive (fn [_] (swap! calls inc) {:target "t" :system "s"})
               :ignite {} :extinguish {} :expect fdb-expect}]
    (with-redefs [mutation/run-on! (fn [spec target opts _]
                                     ((:connect spec) opts)
                                     ((get-in spec [:values :gumshoe.firebook/drill]) {:opts opts})
                                     true)]
      (firebook/run! drill {} nil))
    (is (= 1 @calls))))

(deftest a-stage-hands-its-callbacks-the-drill-context
  (let [seen (atom [])
        record (fn [k] (fn [ctx] (swap! seen conj [k (:vni ctx) (:cluster ctx)]) [k]))
        drill (firebook/drill-spec
               {:description "d" :drill "d" :fire "f"
                :derive (fn [{:keys [vni]}] {:vni vni :target "t" :system "s"})
                :ignite {:items (record :items) :effect (record :effect) :recovery (record :recovery)}
                :extinguish {:put-out "p" :items (record :items) :effect (record :effect)
                             :checks (fn [ctx] [{:description (str "checks " (:vni ctx)) :check (constantly true)}])}
                :expect fdb-expect})
        {:keys [ignite extinguish]} (firebook/stages drill)
        ctx {:context nil :cluster "t" :opts {:vni 7} :gumshoe.firebook/drill {:vni 7 :target "t" :system "s"}}]
    (testing "the connection names the target and the system"
      (is (= {:context nil :cluster "t" :announce-as "s"} ((:connect ignite) {:vni 7}))))
    ((:items ignite) ctx) ((:effect ignite) ctx) ((:recovery ignite) ctx)
    (is (= [[:items 7 "t"] [:effect 7 "t"] [:recovery 7 "t"]] @seen))
    (testing "the ignite announces the drill and proves the fire"
      (is (= "Fire drill started: d" ((:announce ignite) ctx)))
      (is (= ["the fire is burning: the evpn-static-fdb detective reports it"]
             (map :description ((:verify ignite) ctx)))))
    (testing "the extinguish announces nothing and proves the fire out after its own checks"
      (is (nil? (:announce extinguish)))
      (is (= ["checks 7" "the fire is out: the evpn-static-fdb detective no longer reports it"]
             (map :description ((:verify extinguish) ctx)))))
    (testing "an ignite without recovery still says the drill may be lit; an extinguish without one gets the engine's guidance"
      (is (string? (:recovery (:ignite (firebook/stages (update drill :ignite dissoc :recovery))))))
      (is (nil? (:recovery extinguish))))))

(deftest every-firebook-post-check-fails-closed
  (doseq [file firebook-files
          :let [drill (firebook/drill-spec (firebook-spec file))
                opts (if (str/includes? file "fabric") fabric-opts {})]
          [stage spec] (firebook/stages drill)]
    (testing (str file " " stage)
      (with-redefs [kubectl/current-context (constantly "ctx")
                    kubectl/current-cluster (constantly "cluster")]
        (let [ctx {:context "ctx" :opts opts :gumshoe.firebook/drill ((:derive drill) opts)}]
          (is (= [] (binding [*err* (java.io.StringWriter.)]
                      (verify/fail-closed-findings ((:verify spec) ctx))))))))))

(defn- run-extinguish
  "Runs the extinguish script against a fake `bridge` whose `fdb show` prints
   `table` and exits `show-exit`, and whose `fdb del` exits `del-exit`.
   Returns {:exit .. :calls [argument lines]}."
  [{:keys [table show-exit del-exit] :or {show-exit 0 del-exit 0}}]
  (let [dir (fs/create-temp-dir)
        log (str (fs/path dir "calls"))
        bridge (fs/path dir "bridge")]
    (try
      (spit (str bridge)
            (str "#!/bin/sh\n"
                 "echo \"$*\" >> '" log "'\n"
                 "case \"$2\" in\n"
                 "  show) printf '%s' '" table "'; exit " show-exit ";;\n"
                 "  del) exit " del-exit ";;\n"
                 "esac\n"))
      (fs/set-posix-file-permissions bridge "rwx------")
      (let [[_sudo & argv] (firebook/fabric-extinguish-args 4208)
            result (apply process/shell {:out :string :err :string :continue true
                                         :extra-env {"PATH" (str dir ":" (System/getenv "PATH"))}}
                          argv)]
        {:exit (:exit result)
         :calls (if (fs/exists? log) (str/split-lines (slurp log)) [])})
      (finally (fs/delete-tree dir)))))

(deftest fabric-extinguish-is-idempotent
  (let [row (str firebook/drill-mac " dev lo.4208 dst " firebook/drill-vtep " self static\n")]
    (testing "a burning drill is deleted"
      (is (= {:exit 0
              :calls ["fdb show dev lo.4208" (str "fdb del " firebook/drill-mac " dev lo.4208")]}
             (run-extinguish {:table row}))))
    (testing "an entry already gone is out, and no delete the kernel would refuse is attempted"
      (is (= {:exit 0 :calls ["fdb show dev lo.4208"]}
             (run-extinguish {:table "aa:bb:cc:dd:ee:ff dev lo.4208 dst 10.0.0.1 self\n" :del-exit 255}))))
    (testing "a table that cannot be read is a failure, never out"
      (is (= 1 (:exit (run-extinguish {:table "" :show-exit 1})))))
    (testing "a delete that fails while the entry is there is a failure"
      (is (= 255 (:exit (run-extinguish {:table row :del-exit 255})))))
    (testing "the MAC matches in any case"
      (is (= 2 (count (:calls (run-extinguish {:table (str/upper-case row)}))))))))

(deftest fabric-extinguish-runs-as-root-and-the-recovery-stays-a-plain-command
  (is (= ["sudo" "sh" "-c"] (take 3 (firebook/fabric-extinguish-args 4208))))
  (is (str/includes? (firebook/fabric-recovery {:conn {:host "hv1"} :vni 4208})
                     (str "sudo bridge fdb del " firebook/drill-mac " dev lo.4208"))))
