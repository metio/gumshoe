;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.fabric-inputs-test
  "The values the fabric detective judges against - hosts, VNIs, gateway
   VTEPs and guest MAC prefixes - come from an option or from env.edn, the
   option winning. A value neither supplies leaves every check that judges
   against it unjudged: named on stderr, marked unread in the report and
   named by an evpn-inputs finding, never counted as passing.

   The book runs whole against a stubbed SSH collection and a stubbed
   env.edn, from a shell with no kubectl context, so nothing is contacted."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.evpn :as evpn-detectives]
            [gumshoe.evpn :as evpn]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]
            [gumshoe.theme :as theme]))

(def ^:private book-path "runbooks/detectives/fabric.clj")

(def ^:private gateway "10.21.0.1")
(def ^:private other-gateway "10.21.0.9")
(def ^:private hijacked "02:00:0a:18:00:0d")
(def ^:private unclaimed-guest "02:00:0a:18:00:08")
(def ^:private router-mac "e4:8d:8c:00:13:37")

(def ^:private fabric-evidence
  "One partition as the SSH collection returns it: a guest MAC local on
   host-a that host-b reaches through a gateway, a guest MAC no scanned host
   owns behind a gateway, and a router's own MAC behind a gateway."
  {"evpn-macs" [{:host "host-a" :host-vtep "10.21.2.70" :vni 4208 :mac hijacked
                 :type :local :intf "one-256-0" :local-seq 2}
                {:host "host-b" :host-vtep "10.21.2.73" :vni 4208 :mac hijacked
                 :type :remote :vtep gateway :remote-seq 1}
                {:host "host-a" :host-vtep "10.21.2.70" :vni 4208 :mac unclaimed-guest
                 :type :remote :vtep gateway}
                {:host "host-a" :host-vtep "10.21.2.70" :vni 4208 :mac router-mac
                 :type :remote :vtep gateway}]
   "evpn-fdb" []
   "evpn-fdb-context" {}
   "evpn-vtep-paths" {}
   "evpn-vni-coverage" []
   "evpn-hosts" [{:host "host-a" :vni 4208} {:host "host-b" :vni 4208}]})

(defn- env-lookup
  "A stub of config/env-value answering from env, as env.edn would for the
   environment it selects."
  [env]
  (fn [_signals path & [default]]
    (get-in env path default)))

(defn- run-fabric
  "Runs the fabric book's action with opts against env, from a shell with no
   kubectl context, the SSH collection answering with collected. Returns its
   exit value, stdout (parsed when json), stderr, and the evidence its
   detectives were handed."
  ([opts env] (run-fabric opts env fabric-evidence))
  ([opts env collected]
  (let [{:keys [action]} (with-redefs [kubectl/current-cluster (constantly nil)
                                       config/env-value (env-lookup env)]
                           (binding [runbook/*loading* true]
                             (load-file book-path)))
        handed (atom nil)
        run-detectives detective/run-detectives
        err (java.io.StringWriter.)
        result (atom nil)
        out (with-redefs-fn {#'detective/interactive? (constantly false)
                             #'kubectl/current-cluster (constantly nil)
                             #'kubectl/current-context (constantly nil)
                             #'config/env-value (env-lookup env)
                             #'config/active-config (constantly env)
                             #'evpn/connections (fn [hosts _] (vec hosts))
                             #'evpn/collect-evidence! (fn [& _] collected)
                             #'detective/run-detectives (fn [ds evidence]
                                                          (reset! handed evidence)
                                                          (run-detectives ds evidence))}
              (fn []
                (binding [*err* err]
                  (with-out-str (reset! result (action opts nil))))))]
    {:result @result
     :stdout (if (= "json" (:output opts)) (json/parse-string out) (stdout/strip-colors out))
     :stderr (stdout/strip-colors (str err))
     :evidence @handed})))

(def ^:private base-opts {:hosts "host-a,host-b" :vni "4208" :output "json"})

(def ^:private both-in-env
  {:evpn {:gateway-vteps [gateway] :guest-mac-prefixes ["02:00:"]}})

(defn- checked-entry
  [report check]
  (some #(when (= check (get % "name")) %) (get report "checked")))

(defn- findings-of
  [report check]
  (filter #(= check (get % "detective")) (get report "findings")))

(defn- unjudged?
  "Whether the report marks the check as one that judged nothing."
  [report check]
  (true? (get (checked-entry report check) "unread")))

(deftest each-value-comes-from-its-option-or-env-edn-and-the-option-wins
  (doseq [{:keys [label opts env gateways prefixes sources]}
          [{:label "flags only"
            :opts {:gateway-vteps gateway :guest-mac-prefixes "02:00:"}
            :env nil
            :gateways [gateway] :prefixes ["02:00:"]
            :sources {"gateway-vteps" "option" "guest-mac-prefixes" "option"}}
           {:label "env.edn only"
            :opts {}
            :env both-in-env
            :gateways [gateway] :prefixes ["02:00:"]
            :sources {"gateway-vteps" "env.edn" "guest-mac-prefixes" "env.edn"}}
           {:label "both, the option wins"
            :opts {:gateway-vteps (str other-gateway ", " gateway) :guest-mac-prefixes "aa:"}
            :env both-in-env
            :gateways [other-gateway gateway] :prefixes ["aa:"]
            :sources {"gateway-vteps" "option" "guest-mac-prefixes" "option"}}
           {:label "neither"
            :opts {}
            :env nil
            :gateways nil :prefixes nil
            :sources {"gateway-vteps" "missing" "guest-mac-prefixes" "missing"}}
           {:label "a blank option falls through to env.edn"
            :opts {:gateway-vteps " , " :guest-mac-prefixes ""}
            :env both-in-env
            :gateways [gateway] :prefixes ["02:00:"]
            :sources {"gateway-vteps" "env.edn" "guest-mac-prefixes" "env.edn"}}]]
    (testing label
      (let [{:keys [evidence stdout]} (run-fabric (merge base-opts opts) env)]
        (is (= gateways (get evidence "evpn-gateway-vteps")))
        (is (= prefixes (get evidence "evpn-guest-mac-prefixes")))
        (is (= sources (into {} (for [{:strs [input source]} (get stdout "inputs")
                                      :when (#{"gateway-vteps" "guest-mac-prefixes"} input)]
                                  [input source]))))
        (testing "hosts and VNIs came from their options"
          (is (= #{["hosts" "option"] ["vnis" "option"]}
                 (set (for [{:strs [input source]} (get stdout "inputs")
                            :when (#{"hosts" "vnis"} input)]
                        [input source])))))))))

(deftest a-check-without-its-value-is-unjudged-and-named-everywhere
  (doseq [{:keys [label opts env gateway-check prefix-note stderr-names]}
          [{:label "flags only" :opts {:gateway-vteps gateway :guest-mac-prefixes "02:00:"}
            :gateway-check :judged :prefix-note false :stderr-names #{}}
           {:label "env.edn only" :env both-in-env
            :gateway-check :judged :prefix-note false :stderr-names #{}}
           {:label "both" :opts {:gateway-vteps gateway} :env both-in-env
            :gateway-check :judged :prefix-note false :stderr-names #{}}
           {:label "gateways without prefixes" :opts {:gateway-vteps gateway}
            :gateway-check :judged :prefix-note true :stderr-names #{"guest MAC prefixes"}}
           {:label "neither"
            :gateway-check :unjudged :prefix-note true
            :stderr-names #{"gateway VTEPs" "guest MAC prefixes"}}]]
    (testing label
      (let [{:keys [stdout stderr]} (run-fabric (merge base-opts opts) env)
            inputs-findings (findings-of stdout "evpn-inputs")]
        (testing "stderr names each missing value and the option that supplies it"
          (doseq [[value flag] [["gateway VTEPs" "--gateway-vteps"]
                                ["guest MAC prefixes" "--guest-mac-prefixes"]]]
            (is (= (contains? stderr-names value) (str/includes? stderr (str "no " value)))
                (str value " on stderr"))
            (when (contains? stderr-names value)
              (is (str/includes? stderr flag)))))
        (case gateway-check
          :unjudged
          (do (is (unjudged? stdout "evpn-gateway-only-mac"))
              (is (str/includes? stderr "evpn-gateway-only-mac cannot judge"))
              (is (some #(and (= "warning" (get % "severity"))
                              (str/includes? (get % "summary") "evpn-gateway-only-mac"))
                        inputs-findings)
                  "a finding names the unjudged check")
              (is (empty? (findings-of stdout "evpn-gateway-only-mac"))))
          :judged
          (do (is (not (unjudged? stdout "evpn-gateway-only-mac")))
              (is (not (str/includes? stderr "cannot judge")))
              (is (not-any? #(= "warning" (get % "severity")) inputs-findings))))
        (is (= prefix-note
               (boolean (some #(and (= "info" (get % "severity"))
                                    (str/includes? (get % "summary") "guest MAC prefixes"))
                              inputs-findings))))
        (testing "the checks that need neither value judge either way"
          (doseq [check ["evpn-mac-ownership" "evpn-fdb-conflict" "evpn-orphan-fdb" "evpn-hosts" "evpn-static-fdb" "evpn-inputs"]]
            (is (not (unjudged? stdout check)) check))
          (is (= 1 (count (findings-of stdout "evpn-mac-ownership")))))))))

(deftest what-the-gateway-check-finds-follows-the-values
  (testing "with both values, only the guest behind a gateway is reported"
    (let [{:keys [stdout]} (run-fabric (merge base-opts {:gateway-vteps gateway :guest-mac-prefixes "02:00:"}) nil)]
      (is (= [(str "vni 4208/" unclaimed-guest)]
             (map #(get % "component") (findings-of stdout "evpn-gateway-only-mac"))))))
  (testing "without prefixes, every MAC counts as a guest, the router's included"
    (let [{:keys [stdout]} (run-fabric (merge base-opts {:gateway-vteps gateway}) nil)]
      (is (= #{(str "vni 4208/" unclaimed-guest) (str "vni 4208/" router-mac)}
             (set (map #(get % "component") (findings-of stdout "evpn-gateway-only-mac")))))))
  (testing "a flag naming another gateway replaces env.edn's, so env.edn's VTEP is no gateway"
    (let [{:keys [stdout]} (run-fabric (merge base-opts {:gateway-vteps other-gateway}) both-in-env)]
      (is (empty? (findings-of stdout "evpn-gateway-only-mac")))
      (is (str/includes? (get (first (findings-of stdout "evpn-mac-ownership")) "hint")
                         "two VTEPs disagree")))))

(deftest the-ownership-hint-never-guesses-whether-a-vtep-is-a-gateway
  (testing "with gateways known, a gateway claim names proxy-macip-advertisement"
    (let [{:keys [stdout]} (run-fabric (merge base-opts {:gateway-vteps gateway}) nil)]
      (is (str/includes? (get (first (findings-of stdout "evpn-mac-ownership")) "hint")
                         "proxy-macip-advertisement"))))
  (testing "without them, the hint says it could not tell"
    (let [hint (get (first (findings-of (:stdout (run-fabric base-opts nil)) "evpn-mac-ownership")) "hint")]
      (is (str/includes? hint (str "Whether " gateway " is a gateway VTEP could not be judged")))
      (is (not (str/includes? hint "proxy-macip-advertisement"))))))

(deftest the-report-of-a-run-without-either-value
  ;; The whole machine-readable report of the run infra.run described: --hosts
  ;; and --vni from a shell without a kubectl context, so env.edn supplies
  ;; nothing. Each part a reader trusts says what could not be judged.
  (let [{:keys [result stdout stderr]} (run-fabric base-opts nil)]
    (is (false? result) "the ownership conflict is critical")
    (is (= {"critical" 1 "warning" 1 "info" 1} (get stdout "summary")))
    (is (= [{"name" "evpn-mac-ownership"
             "description" "guest MACs the fabric attributes to a VTEP that is not their host's"}
            {"name" "evpn-gateway-only-mac"
             "description" "guest MACs no scanned host owns that every VTEP reaches through a gateway VTEP"
             "unread" true}
            {"name" "evpn-fdb-conflict"
             "description" "hosts holding both a local tap and a remote entry for one MAC, with how long FRR has held it local, whether BGP still advertises the remote one, and whether the tap learns"}
            {"name" "evpn-orphan-fdb"
             "description" "learned remote fdb rows naming a VTEP no BGP path advertises, counted per host and VTEP whether or not the host also holds the MAC on a local tap"}
            {"name" "evpn-hosts"
             "description" "hosts whose fabric state could not be read, making the scan incomplete"}
            {"name" "evpn-vni-coverage"
             "description" "VNIs a host carries that the scan did not read, which leave its verdict incomplete, and VNIs read beyond the configured ones"}
            {"name" "evpn-static-fdb"
             "description" "static fdb entries, which outrank EVPN learning and go stale silently"}
            {"name" "evpn-inputs"
             "description" "values the scan judges against that neither an option nor env.edn supplied, and the checks that could not judge without them"}]
           (get stdout "checked")))
    (is (= [{"input" "hosts" "option" "--hosts" "env-path" ["evpn" "hosts"] "source" "option"}
            {"input" "vnis" "option" "--vni" "env-path" ["evpn" "vnis"] "source" "option"}
            {"input" "gateway-vteps" "option" "--gateway-vteps" "env-path" ["evpn" "gateway-vteps"] "source" "missing"}
            {"input" "guest-mac-prefixes" "option" "--guest-mac-prefixes" "env-path" ["evpn" "guest-mac-prefixes"] "source" "missing"}]
           (get stdout "inputs")))
    (is (= [{"severity" "warning"
             "component" "--gateway-vteps"
             "summary" "no gateway VTEPs were given, so evpn-gateway-only-mac could not judge"
             "hint" "pass --gateway-vteps or set :evpn :gateway-vteps in env.edn; until then evpn-gateway-only-mac is unjudged, and its silence is not a clean result"
             "detective" "evpn-inputs"}
            {"severity" "info"
             "component" "--guest-mac-prefixes"
             "summary" "no guest MAC prefixes were given, so every MAC counts as a guest"
             "hint" "pass --guest-mac-prefixes or set :evpn :guest-mac-prefixes in env.edn; until then evpn-gateway-only-mac also reports infrastructure MACs that sit behind a gateway"
             "detective" "evpn-inputs"}]
           (findings-of stdout "evpn-inputs")))
    (is (str/includes? stderr "no gateway VTEPs - pass --gateway-vteps or set :evpn :gateway-vteps in env.edn; evpn-gateway-only-mac cannot judge without them and is reported as unjudged"))
    (is (str/includes? stderr "no guest MAC prefixes - pass --guest-mac-prefixes or set :evpn :guest-mac-prefixes in env.edn; evpn-gateway-only-mac counts every MAC as a guest"))))

(deftest the-text-report-never-lists-an-unjudged-check-as-passing
  (let [clean (assoc fabric-evidence "evpn-macs" [])
        run (fn [opts] (run-fabric (merge base-opts {:output "text"} opts) nil clean))]
    (testing "a fabric with nothing wrong and no gateway VTEPs"
      (let [{:keys [stdout stderr]} (run {})
            description "guest MACs no scanned host owns that every VTEP reaches through a gateway VTEP"]
        (is (not (str/includes? stdout (str (theme/glyph :success) " " description))))
        (is (not (str/includes? stderr "no issues found")))
        (is (str/includes? stdout "evpn-gateway-only-mac"))
        (is (str/includes? stdout "read nothing"))))
    (testing "the same fabric with gateway VTEPs passes every check"
      (let [{:keys [stdout stderr]} (run {:gateway-vteps gateway :guest-mac-prefixes "02:00:"})]
        (is (= 8 (count (re-seq (re-pattern (java.util.regex.Pattern/quote (theme/glyph :success))) stdout))))
        (is (str/includes? stderr "no issues found - 8 checks passed"))))))

(deftest a-collection-that-read-nothing-still-checked-nothing
  (testing "missing inputs add no finding when the fabric itself was not read"
    (let [unread (update-vals fabric-evidence (constantly nil))
          {:keys [result stdout stderr]} (run-fabric base-opts nil unread)]
      (is (false? result))
      (is (true? (get stdout "nothing-checked")))
      (is (every? #(true? (get % "unread")) (get stdout "checked")))
      (is (empty? (get stdout "findings")))
      (is (str/includes? stderr "no gateway VTEPs") "stderr still names the missing value"))))

;; --- fail closed ----------------------------------------------------------------

(def ^:private gen-mac
  (gen/fmap (fn [[p a b]] (format "%s:%02x:%02x" p a b))
            (gen/tuple (gen/elements ["02:00:0a:18" "e4:8d:8c:00"]) (gen/choose 0 255) (gen/choose 0 255))))

(def ^:private gen-row
  (gen/let [host (gen/elements ["host-a" "host-b" "host-c"])
            mac gen-mac
            local? gen/boolean
            vtep (gen/elements [gateway other-gateway "10.21.2.70" "10.21.2.73"])]
    (if local?
      {:host host :host-vtep "10.21.2.70" :vni 4208 :mac mac :type :local :intf "one-1-0"}
      {:host host :host-vtep "10.21.2.70" :vni 4208 :mac mac :type :remote :vtep vtep})))

(def ^:private gen-source
  "Where a value comes from in one run: the option, env.edn, both, or neither."
  (gen/elements [:option :env :both :neither]))

(defn- supplied
  [source flag-value env-value]
  {:opt (when (#{:option :both} source) flag-value)
   :env (when (#{:env :both} source) env-value)})

(def ^:private fail-closed
  "A run whose gateway VTEPs neither an option nor env.edn supplied marks
   evpn-gateway-only-mac unread, names it on stderr and in an evpn-inputs
   finding, and marks nothing else unread; a run with them marks nothing
   unread."
  (prop/for-all [rows (gen/vector gen-row 0 12)
                 gateway-source gen-source
                 prefix-source gen-source]
    (let [g (supplied gateway-source gateway gateway)
          p (supplied prefix-source "02:00:" "02:00:")
          opts (cond-> base-opts
                 (:opt g) (assoc :gateway-vteps (:opt g))
                 (:opt p) (assoc :guest-mac-prefixes (:opt p)))
          env {:evpn (cond-> {}
                       (:env g) (assoc :gateway-vteps [(:env g)])
                       (:env p) (assoc :guest-mac-prefixes [(:env p)]))}
          {:keys [stdout stderr]} (run-fabric opts env (assoc fabric-evidence "evpn-macs" rows))
          no-gateways? (= :neither gateway-source)]
      (and
       ;; a check whose value is missing is unread, named on stderr and
       ;; named by a finding, whatever the fabric holds
       (= no-gateways? (unjudged? stdout "evpn-gateway-only-mac"))
       (= no-gateways? (str/includes? stderr "evpn-gateway-only-mac cannot judge"))
       (= no-gateways? (boolean (some #(str/includes? (get % "summary") "evpn-gateway-only-mac could not judge")
                                      (findings-of stdout "evpn-inputs"))))
       ;; every unread check has a value missing that it judges
       (every? (fn [{:strs [name unread]}]
                 (or (not unread) (and no-gateways? (= "evpn-gateway-only-mac" name))))
               (get stdout "checked"))))))

(deftest no-check-passes-for-lack-of-its-value
  (let [result (tc/quick-check 60 fail-closed :seed 4208)]
    (is (:pass? result) (pr-str (select-keys result [:fail :shrunk :seed])))))

(deftest the-fail-closed-property-catches-a-check-that-passes-without-its-value
  (testing "a gateway check that does not declare what it judges breaks the property"
    (let [unguarded (mapv #(dissoc % :judges) evpn-detectives/detectives)
          result (with-redefs [evpn-detectives/detectives unguarded]
                   (tc/quick-check 30 fail-closed :seed 4208))]
      (is (false? (:pass? result))))))

(deftest every-judged-input-names-the-checks-that-judge-it
  (testing "each input the detectives judge against is declared, with the option that supplies it"
    (let [judged (set (mapcat :judges evpn-detectives/detectives))
          declared (set (keep :evidence evpn/inputs))]
      (is (= #{"evpn-gateway-vteps"} judged))
      (is (every? declared judged))))
  (testing "a missing judged input names exactly the checks whose :judges hold it"
    (is (= [{:input "gateway-vteps" :option "--gateway-vteps" :env-path [:evpn :gateway-vteps]
             :unjudged ["evpn-gateway-only-mac"]}]
           (map #(select-keys % [:input :option :env-path :unjudged])
                (filter :unjudged (evpn-detectives/missing-inputs
                                   {"evpn-inputs" [{:input "gateway-vteps" :option "--gateway-vteps"
                                                    :env-path [:evpn :gateway-vteps] :source "missing"}]})))))))
