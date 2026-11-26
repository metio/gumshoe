;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.evpn-vni-coverage-test
  "A fabric scan judges every VNI a host carries, or says which it did not.

  The fixtures follow a staging scan whose configuration listed VNIs 4208 and
  4201 while both hosts carried a third VXLAN device, lo.4200. A dead VTEP,
  10.21.2.70, left 25 learned rows on lo.4200 of one host and 9 on the
  other, each a MAC learned through EVPN: the device's own `dst … self
  extern_learn` row and one `vlan 1 … master` bridge row. BGP answers the
  path count for that VTEP in VNI 4200 with prefixes and no paths, as FRR
  printed it. The hosts answer over a stubbed ssh, so nothing is contacted."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.clear-orphan-fdb-test :as clear-test]
            [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.evpn :as evpn-detectives]
            [gumshoe.evpn :as evpn]
            [gumshoe.evpn-clear :as clear]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]))

(def ^:private emu "secure-emu.srv.example")
(def ^:private cobra "upward-cobra.srv.example")
(def ^:private dead "10.21.2.70")
(def ^:private configured [4208 4201])
(def ^:private carried [4208 4201 4200])

(def ^:private live-path-json
  "FRR's answer to `show bgp vni 4200 vtep 10.21.2.70 json` on staging,
  2026-11-26: prefixes in both tables, no path from the VTEP."
  "{\"numPrefix\":11,\"numPaths\":0,\"macTable\":{\"numPrefix\":22,\"numPaths\":0}}")

(defn- mac
  [host n]
  (format "02:00:2d:%s:%02x" (if (= host emu) "5c:31" "6a:07") n))

(defn- learned
  "The rows `bridge fdb show` printed for one MAC learned through EVPN: the
  overlay device's own row naming the VTEP, and the vlan-tagged bridge row."
  [m vni vtep]
  [(str m " dev lo." vni " dst " vtep " self extern_learn")
   (str m " dev lo." vni " vlan 1 extern_learn master br" vni)])

(defn- table
  [host orphans]
  (str/join "\n"
            (concat (mapcat #(learned (mac host %) 4200 dead) (range 1 (inc orphans)))
                    (learned "02:00:0a:18:00:30" 4208 "10.21.2.71")
                    (learned "02:00:0a:15:09:30" 4201 "10.21.2.71")
                    ["02:00:0a:18:00:13 dev one-278-0 master br4208"
                     "33:33:00:00:00:01 dev br4208 self permanent"])))

(defn- links-json
  [vnis]
  (json/generate-string
   (for [vni vnis]
     {:ifindex (+ 100 vni) :ifname (str "lo." vni)
      :linkinfo {:info_kind "vxlan" :info_data {:id vni :local "10.21.2.71" :port 4789}}})))

(defn- ip-links
  "What `ip -d -j link show` prints for these links: each bridge port's flags
  in its own `linkinfo.info_slave_data`, as iproute2 prints them. A link
  without `:slave` is no bridge port."
  [links]
  (json/generate-string
   (for [{:keys [ifname master kind slave-kind slave]} links]
     (cond-> {:ifindex 7 :ifname ifname :flags ["BROADCAST" "MULTICAST" "UP" "LOWER_UP"] :mtu 1500}
       master (assoc :master master)
       slave (assoc :linkinfo {:info_kind (or kind "tun")
                               :info_slave_kind (or slave-kind "bridge")
                               :info_slave_data slave})))))

(defn- port
  "A tap or overlay device enslaved to br4200 with these flags."
  [ifname flags]
  {:ifname ifname :master "br4200" :kind (if (str/starts-with? ifname "lo.") "vxlan" "tun")
   :slave (merge {:state "forwarding" :priority 32 :cost 100 :hairpin false :guard false
                  :root_block false :fastleave false :proxy_arp false :proxy_arp_wifi false
                  :mcast_flood true :neigh_suppress false :isolated false}
                 flags)})

(def ^:private bridge-detail-json
  "What `bridge -d -j link show` printed on an infra.run staging hypervisor on
  2026-11-26 (Ubuntu 20.04.6, iproute2-ss200127), the flags list filled in:
  the `-d` detail object stands where a key belongs, so it is no JSON."
  (str "[{\"ifindex\":7,\"ifname\":\"docker0\",\"flags\":[\"NO-CARRIER\",\"BROADCAST\",\"MULTICAST\",\"UP\"],"
       "\"mtu\":1500,\"master\":\"docker0\",{\"ifname\":\"docker0\",\"vlans\":[]}}]"))

(def ^:private fabric
  {emu {:links (links-json carried) :fdb (table emu 25)}
   cobra {:links (links-json carried) :fdb (table cobra 9)}})

(defn- unquoted
  [token]
  (if (re-matches #"'[^']*'" token) (subs token 1 (dec (count token))) token))

(defn- answer
  "One host's answer to one remote command."
  [{:keys [links fdb ports unreachable? paths]} command]
  (let [line (str/join " " (remove #{"sudo"} command))
        ok (fn [out] {:exit 0 :out (or out "") :err ""})]
    (cond
      unreachable? {:exit 255 :out "" :err "ssh: connect to host: Connection timed out"}
      (= line "exit 0") (ok "")
      (= line "ip -d -j link show type vxlan") (ok links)
      (= line "bridge fdb show") (ok fdb)
      (= line "ip -d -j link show") (ok ports)
      (re-find #"^vtysh -c show evpn vni \d+$" line) (ok " Local VTEP IP: 10.21.2.71\n")
      (re-find #"^vtysh -c show evpn mac vni \d+$" line) (ok "02:00:0a:18:00:13 local one-278-0 1/0\n")
      (re-find #"^vtysh -c show bgp vni \d+ vtep \S+ json$" line)
      (let [[_ vni vtep] (re-find #"show bgp vni (\d+) vtep (\S+) json" line)]
        (ok (get paths [(parse-long vni) vtep]
                 (if (= dead vtep)
                   live-path-json
                   "{\"numPrefix\":11,\"numPaths\":4,\"macTable\":{\"numPrefix\":22,\"numPaths\":8}}"))))
      :else (ok ""))))

(defn- collect
  "The fabric's evidence as collect-evidence! gathers it from hosts answering
  out of `hosts`, with every remote command each host received."
  [hosts vnis opts]
  (let [seen (atom [])]
    {:evidence (binding [ssh/*user-config* nil]
                 (with-redefs [shell/execute
                               (fn [& args]
                                 (let [argv (vec (flatten args))
                                       [host & command] (rest (drop-while #(not= "--" %) argv))
                                       command (mapv unquoted command)]
                                   (swap! seen conj [host command])
                                   (answer (get hosts host) command)))]
                   (evpn/collect-evidence! (evpn/connections (keys hosts) {}) vnis opts)))
     :seen @seen}))

(defn- findings
  [evidence check]
  (let [{:keys [detect]} (some #(when (= check (:name %)) %) evpn-detectives/detectives)]
    (vec (detect evidence))))

(defn- vtysh-vnis
  "The VNIs whose MAC table each host was asked for."
  [seen]
  (into {} (for [[host rows] (group-by first seen)]
             [host (set (keep #(some->> (str/join " " (second %))
                                        (re-find #"show evpn mac vni (\d+)$")
                                        second parse-long)
                              rows))])))

;; --- discovery reads every VNI a host carries --------------------------------------

(deftest the-vnis-a-host-carries-are-read-beside-the-configured-ones
  (let [{:keys [evidence seen]} (collect fabric configured {})]
    (testing "every host is asked about the VNI the configuration misses"
      (is (= {emu #{4208 4201 4200} cobra #{4208 4201 4200}} (vtysh-vnis seen))))
    (testing "the orphan rows on lo.4200 are judged, counted per MAC"
      (is (= {emu "25 fdb rows (25 MACs) name 10.21.2.70, which no BGP path advertises in VNI 4200"
              cobra "9 fdb rows (9 MACs) name 10.21.2.70, which no BGP path advertises in VNI 4200"}
             (into {} (map (juxt :host :summary) (findings evidence "evpn-orphan-fdb"))))))
    (testing "each orphan's one bridge row goes with it, and no count assumes more"
      (let [f (first (filter #(= emu (:host %)) (findings evidence "evpn-orphan-fdb")))]
        (is (= 25 (count (:rows f))))
        (is (= 25 (count (:bridge-rows f))))))
    (testing "coverage says the VNI was read beyond the configuration, and leaves no gap"
      (let [fs (findings evidence "evpn-vni-coverage")]
        (is (= [[emu :info] [cobra :info]] (map (juxt :host :severity) fs)))
        (is (every? #(= [4200] (:discovered %)) fs))
        (is (= (str "host carries VNI 4200 that the configuration does not list; the scan read it "
                    "because it reads every VNI a host carries")
               (:summary (first fs))))))
    (testing "the evidence names what each host carries and what was read"
      (is (= {:host emu :answered? true :configured [4201 4208] :carried [4200 4201 4208]
              :named [4200 4201 4208] :scanned [4200 4201 4208] :discovered [4200] :uncovered []}
             (first (get evidence "evpn-vni-coverage")))))))

;; --- a scan of the configured VNIs alone reports the gap -------------------------

(deftest a-host-carrying-a-vni-the-scan-skipped-is-not-clean
  (let [{:keys [evidence seen]} (collect fabric configured {:discover? false})
        fs (findings evidence "evpn-vni-coverage")]
    (testing "only the configured VNIs are read"
      (is (= {emu #{4208 4201} cobra #{4208 4201}} (vtysh-vnis seen))))
    (testing "the rows on lo.4200 are not judged"
      (is (empty? (findings evidence "evpn-orphan-fdb"))))
    (testing "each host gets a warning naming the VNI and saying its verdict is incomplete"
      (is (= [[emu :warning] [cobra :warning]] (map (juxt :host :severity) fs)))
      (is (= "host carries VNI 4200 that the scan did not cover, so its verdict is incomplete"
             (:summary (first fs))))
      (is (= [4200] (:uncovered (first fs))))
      (is (str/starts-with? (:hint (first fs))
                            "its VXLAN devices list VNI 4200, and no check read them")))
    (testing "the run is reported with findings, not clean"
      (is (seq (detective/run-detectives evpn-detectives/detectives evidence))))))

(deftest the-fdb-tables-device-names-back-up-a-device-list-that-did-not-answer
  (doseq [discover? [true false]]
    (testing (str "discover? " discover?)
      (let [hosts (update-vals fabric #(assoc % :links ""))
            {:keys [evidence seen]} (collect hosts configured {:discover? discover?})
            fs (findings evidence "evpn-vni-coverage")
            by-host (group-by :host fs)]
        (is (= {emu #{4208 4201} cobra #{4208 4201}} (vtysh-vnis seen))
            "nothing the device list did not name is read")
        (doseq [host [emu cobra]]
          (is (= #{"host carries VNI 4200 that the scan did not cover, so its verdict is incomplete"
                   "host's VXLAN devices could not be listed (no output), so VNIs it carries beyond the scanned ones are unjudged"}
                 (set (map :summary (get by-host host)))))
          (is (some #(str/starts-with? (:hint %) "its fdb table names lo.4200") (get by-host host))))))))

(deftest a-device-without-a-vni-leaves-what-it-carries-unjudged
  (let [external (json/generate-string [{:ifname "vxlan0" :linkinfo {:info_kind "vxlan" :info_data {:external true}}}])
        hosts {emu {:links external :fdb (table emu 0)}}
        fs (findings (:evidence (collect hosts configured {})) "evpn-vni-coverage")]
    (is (= [{:severity :warning
             :summary "host's VXLAN device vxlan0 list no VNI, so the VNIs it carries are unjudged"}]
           (map #(select-keys % [:severity :summary])
                (filter #(seq (:unlisted %)) fs))))))

(deftest a-host-nobody-could-read-is-left-to-evpn-hosts
  (let [hosts (assoc fabric cobra {:unreachable? true})
        {:keys [evidence]} (collect hosts configured {})]
    (is (= [emu] (distinct (map :host (findings evidence "evpn-vni-coverage")))))
    (is (seq (filter #(str/starts-with? (:component %) cobra) (findings evidence "evpn-hosts"))))))

(deftest positive-control-every-vni-configured-leaves-no-gap
  (doseq [discover? [true false]]
    (testing (str "discover? " discover?)
      (let [{:keys [evidence seen]} (collect fabric carried {:discover? discover?})]
        (is (= {emu #{4208 4201 4200} cobra #{4208 4201 4200}} (vtysh-vnis seen)))
        (is (empty? (findings evidence "evpn-vni-coverage")))
        (is (= 2 (count (findings evidence "evpn-orphan-fdb"))))))))

(deftest the-coverage-check-is-unread-without-its-evidence
  (is (contains? (set (map :name (filter :unread (detective/mark-unread evpn-detectives/detectives {}))))
                 "evpn-vni-coverage")))

;; --- the parsers -----------------------------------------------------------------------

(deftest parsing-the-vxlan-device-list
  (is (= {:read? true :devices [{:dev "lo.4208" :vni 4208} {:dev "lo.4200" :vni 4200}]}
         (evpn/parse-vxlan-links (links-json [4208 4200]))))
  (testing "a device named any way is read by its id"
    (is (= [{:dev "vxlan-guests" :vni 77}]
           (:devices (evpn/parse-vxlan-links "[{\"ifname\":\"vxlan-guests\",\"linkinfo\":{\"info_kind\":\"vxlan\",\"info_data\":{\"id\":77}}}]")))))
  (testing "no VXLAN device is a read"
    (is (= {:read? true :devices []} (evpn/parse-vxlan-links "[]"))))
  (testing "silence or another shape is unread"
    (is (false? (:read? (evpn/parse-vxlan-links ""))))
    (is (false? (:read? (evpn/parse-vxlan-links "Object \"link\" is unknown, try \"ip help\".\n"))))
    (is (false? (:read? (evpn/parse-vxlan-links "{\"ifname\":\"lo.4200\"}")))))
  (testing "a list that is no JSON is unread, never a crash"
    (is (false? (:read? (evpn/parse-vxlan-links bridge-detail-json))))))

(deftest parsing-the-bridge-ports
  (testing "each port's flags come from its own info_slave_data"
    (is (= {:read? true :ports {"one-140-0" {:master "br4200" :learning true :flood true}
                                "lo.4200" {:master "br4200" :learning false :flood true}
                                "one-301-0" {:master "br4200" :learning nil :flood nil}}}
           (evpn/parse-bridge-ports
            (ip-links [{:ifname "eno1"}
                       (port "one-140-0" {:learning true :flood true})
                       (port "lo.4200" {:learning false :flood true})
                       {:ifname "one-301-0" :master "br4200" :slave {:state "forwarding"}}
                       {:ifname "eno2" :master "bond0" :kind "device" :slave-kind "bond"
                        :slave {:state "ACTIVE" :learning true}}])))))
  (testing "a version printing on/off words reads the same"
    (is (= {:master "br4200" :learning false :flood true}
           (get-in (evpn/parse-bridge-ports (ip-links [(port "one-301-0" {:learning "off" :flood "on"})]))
                   [:ports "one-301-0"]))))
  (testing "a flag in neither form is unread, never off"
    (is (nil? (get-in (evpn/parse-bridge-ports (ip-links [(port "one-301-0" {:learning 0})]))
                      [:ports "one-301-0" :learning]))))
  (testing "flags outside info_slave_data are not the port's"
    (is (= {} (:ports (evpn/parse-bridge-ports
                       "[{\"ifname\":\"one-301-0\",\"master\":\"br4200\",\"learning\":false}]")))))
  (is (false? (:read? (evpn/parse-bridge-ports ""))))
  (is (false? (:read? (evpn/parse-bridge-ports "not json")))))

(deftest invalid-json-is-unread-with-the-parser-error
  (let [r (evpn/parse-bridge-ports bridge-detail-json)]
    (is (false? (:read? r)))
    (is (str/starts-with? (:error r) "invalid JSON: "))
    (is (<= (count (:error r)) (+ (count "invalid JSON: ") 161)))))

;; --- the book passes the choice through ------------------------------------------------

(defn- run-book
  [opts]
  (let [called (atom nil)
        {:keys [action]} (with-redefs [kubectl/current-cluster (constantly nil)]
                           (binding [runbook/*loading* true]
                             (load-file "runbooks/detectives/fabric.clj")))]
    (with-redefs-fn {#'detective/interactive? (constantly false)
                     #'kubectl/current-cluster (constantly nil)
                     #'kubectl/current-context (constantly nil)
                     #'config/env-value (fn [_ path & _] (get-in {:evpn {:vnis configured}} path))
                     #'config/active-config (constantly nil)
                     #'evpn/connections (fn [hosts _] (vec hosts))
                     #'evpn/collect-evidence! (fn [& args] (reset! called args) {"evpn-hosts" []})}
      (fn [] (binding [*err* (java.io.StringWriter.)]
               (with-out-str (action (merge {:hosts emu :output "json"} opts) nil)))))
    @called))

(deftest the-book-reads-every-carried-vni-unless-told-otherwise
  (is (= [[emu] configured {:discover? true}] (run-book {})))
  (is (= [[emu] configured {:discover? false}] (run-book {:configured-vnis-only true}))))

;; --- the cleanup book never counts a VNI it did not read as clear ---------------------

(def ^:private emu-4200-lines
  "secure-emu's table from the change book's incident, plus three orphans on
  lo.4200, which the configuration does not list."
  (vec (concat clear-test/emu-lines
               (mapcat #(learned (mac emu %) 4200 dead) [1 2 3]))))

(deftest the-cleanup-book-reads-every-overlay-the-table-names
  (let [fabric {:tables {clear-test/emu emu-4200-lines}
                :paths (assoc clear-test/incident-paths [clear-test/emu 4200 dead] 0)}
        run (clear-test/run-book {:fabric fabric :opts {:hosts clear-test/emu :vtep dead}})]
    (is (true? (:result run)) (pr-str (:out run)))
    (testing "the rows on lo.4200 are deleted with the rest"
      (is (some #(str/includes? (str/join " " (second %)) "dev lo.4200 dst 10.21.2.70 self")
                (:deleted run))))
    (testing "BGP was asked about the VTEP in VNI 4200"
      (is (some #(= ["vtysh" "-c" "show bgp vni 4200 vtep 10.21.2.70 json"] (second %)) (:commands run))))
    (is (empty? (filter #(str/includes? % "lo.4200") (get-in run [:tables clear-test/emu]))))))

(deftest an-unread-count-in-an-unlisted-vni-keeps-the-host-unjudged
  (let [fabric {:tables {clear-test/emu emu-4200-lines}
                :paths (assoc clear-test/incident-paths [clear-test/emu 4200 dead] :unread)}
        run (clear-test/run-book {:fabric fabric :opts {:hosts clear-test/emu :vtep dead}})]
    (is (not (true? (:result run))))
    (is (empty? (:deleted run)))
    (is (some #(re-find #"could not be read in VNI 4200" %) (:out run)) (pr-str (:out run)))))

(defn- reading
  [lines paths]
  {:host clear-test/emu :read? true :output (str/join "\n" lines)
   :rows (evpn/parse-fdb-table clear-test/emu (str/join "\n" lines))
   :paths paths})

(deftest the-cleanup-done-check-and-post-check-see-rows-in-an-unlisted-vni
  (let [left (vec (mapcat #(learned (mac emu %) 4200 dead) [1 2]))
        r (reading left {[4200 dead] {:read? true :paths 0}})]
    (testing ":done is false while a row on lo.4200 names the VTEP"
      (is (false? (clear/cleared? (clear/judge-fabric [r] dead)))))
    (testing "the post-check names the rows and evpn-orphan-fdb's finding for VNI 4200"
      (let [reasons (clear/remaining r dead)]
        (is (some #(str/includes? % "dev lo.4200 dst 10.21.2.70 self extern_learn") reasons))
        (is (some #(str/includes? % "evpn-orphan-fdb 2 fdb rows (2 MACs) name 10.21.2.70, which no BGP path advertises in VNI 4200") reasons))))
    (testing "a table without them is cleared"
      (let [r (reading (remove #(str/includes? % "lo.4200") clear-test/emu-lines) {})]
        (is (empty? (filter #(str/includes? % "lo.4200") (clear/remaining r dead))))))))

;; --- the collision hint states as fact only what was read ------------------------------

(def ^:private guest "02:00:2d:5c:31:d1")

(def ^:private collision-fdb
  (evpn/parse-fdb 4200 emu (str/join "\n" (concat (learned guest 4200 dead)
                                                  [(str guest " dev one-301-0 master br4200")]))))

(def ^:private mac-detail-weeks
  (str "MAC: " guest "\n Intf: one-301-0(16) VLAN: 1\n Local Seq: 2 Remote Seq: 1\n Uptime: 05w0d22h\n"))

(defn- ports
  [learning]
  {emu (evpn/parse-bridge-ports
        (ip-links [(port "one-301-0" {:learning learning :flood true})
                   (port "lo.4200" {:learning false :flood true})]))})

(defn- conflict
  [frr learning]
  (first (evpn-detectives/detect-fdb-conflicts
          (cond-> {"evpn-fdb" collision-fdb}
            (= :read frr) (assoc "evpn-fdb-context"
                                 {[emu 4200 guest] {:mac-detail (evpn/parse-mac-detail mac-detail-weeks)
                                                    :bgp-route (evpn/parse-bgp-mac-route "% Network not in table")}})
            (boolean? learning) (assoc "evpn-bridge-ports" (ports learning))))))

(def ^:private rows-read
  (str "Both fdb rows were read: " guest " on the tap one-301-0 and on lo.4200 with dst 10.21.2.70. "))

(def ^:private frr-read
  (str "FRR has held the MAC as local for 05w0d22h. No BGP path names 10.21.2.70, so this is an "
       "orphan: the route that installed the entry is gone and a local delete holds. A migrated "
       "guest leaves a dst self row and bridge rows on the overlay device, so check every row for "
       "the MAC before deleting. "))

(def ^:private frr-unread
  (str "FRR's view of the MAC could not be read (not collected), so whether and how long FRR has "
       "held it as local is unknown. BGP could not be read (not collected), so whether a route "
       "stands behind the entry is unknown. "))

(def ^:private learning-off
  (str "one-301-0 has learning off, as `ip -d -j link show` reads it, so a frame from the guest "
       "does not move the bridge's entry back to the tap: frames the bridge forwards out of lo.4200 "
       "go to 10.21.2.70, and traffic for the guest is black-holed rather than flooded"))

(def ^:private learning-on
  (str "one-301-0 has learning on, as `ip -d -j link show` reads it, so a frame from the guest "
       "may move the bridge's entry back to the tap; until it does, frames the bridge forwards out "
       "of lo.4200 go to 10.21.2.70. Whether a learned frame replaces an entry EVPN installed "
       "(extern_learn) depends on the kernel and was not read"))

(def ^:private learning-unread
  (str "The bridge's learning flag for one-301-0 was not read (not collected). If one-301-0 does "
       "not learn (nolearning), a frame from the guest does not move the bridge's entry back to the "
       "tap, frames the bridge forwards out of lo.4200 go to 10.21.2.70, and traffic for the guest "
       "is black-holed rather than flooded"))

(deftest the-collision-hint-for-each-reading
  (doseq [[frr learning expected] [[:read false (str rows-read frr-read learning-off)]
                                   [:read true (str rows-read frr-read learning-on)]
                                   [:read nil (str rows-read frr-read learning-unread)]
                                   [:unread false (str rows-read frr-unread learning-off)]
                                   [:unread true (str rows-read frr-unread learning-on)]
                                   [:unread nil (str rows-read frr-unread learning-unread)]]]
    (testing (str "FRR " frr ", learning " (pr-str learning))
      (let [f (conflict frr learning)]
        (is (= expected (:hint f)))
        (is (= :critical (:severity f)) "the rows alone are read evidence of the conflict")
        (is (= (case learning true "on" false "off" "unread") (:tap-learning f)))))))

(deftest the-black-hole-is-a-fact-only-when-the-tap-was-read-not-to-learn
  (doseq [learning [true nil]]
    (is (not (str/includes? (:hint (conflict :unread learning)) "because a nolearning bridge"))))
  (is (str/includes? (:hint (conflict :unread nil)) "If one-301-0 does not learn (nolearning)")))

(deftest a-tap-the-bridge-does-not-list-is-unread
  (let [f (first (evpn-detectives/detect-fdb-conflicts
                  {"evpn-fdb" collision-fdb
                   "evpn-bridge-ports" {emu (evpn/parse-bridge-ports (ip-links [(port "lo.4200" {:learning false :flood true})]))}}))]
    (is (= "unread" (:tap-learning f)))
    (is (= "one-301-0 is not listed among the bridge ports" (:bridge-ports-error f)))))

(deftest the-bridge-ports-are-read-only-on-hosts-with-a-collision
  (let [hosts {emu {:links (links-json carried)
                    :fdb (str (table emu 2) "\n" (str/join "\n" (concat (learned guest 4200 dead)
                                                                       [(str guest " dev one-301-0 master br4200")])))
                    :ports (ip-links [(port "one-301-0" {:learning false :flood false})])}
               cobra {:links (links-json carried) :fdb (table cobra 2)}}
        {:keys [evidence seen]} (collect hosts configured {})
        asked (set (for [[host command] seen :when (= ["ip" "-d" "-j" "link" "show"] command)] host))
        f (first (findings evidence "evpn-fdb-conflict"))]
    (is (= #{emu} asked))
    (is (= "off" (:tap-learning f)))
    (is (= "off" (:tap-flood f)))
    (is (str/ends-with? (:hint f) "traffic for the guest is black-holed rather than flooded"))))

(defn- conflict-on
  "The collision finding when emu's links read as `ports-output`."
  [ports-output]
  (first (evpn-detectives/detect-fdb-conflicts
          {"evpn-fdb" collision-fdb
           "evpn-bridge-ports" {emu (evpn/parse-bridge-ports ports-output)}})))

(deftest the-tap-flag-is-the-taps-never-the-overlay-devices
  (testing "tap learning on, lo.4200 learning off"
    (let [f (conflict-on (ip-links [(port "lo.4200" {:learning false :flood false})
                                    (port "one-301-0" {:learning true :flood true})]))]
      (is (= "on" (:tap-learning f)))
      (is (= "on" (:tap-flood f)))
      (is (str/includes? (:hint f) "one-301-0 has learning on"))
      (is (not (str/includes? (:hint f) "learning off")))))
  (testing "tap learning off, lo.4200 learning on"
    (let [f (conflict-on (ip-links [(port "lo.4200" {:learning true :flood true})
                                    (port "one-301-0" {:learning false :flood true})]))]
      (is (= "off" (:tap-learning f)))
      (is (str/includes? (:hint f) "one-301-0 has learning off")))))

(deftest the-staging-reading-states-no-black-hole
  (let [f (conflict-on (ip-links [(port "one-301-0" {:learning true :flood true})
                                  (port "lo.4200" {:learning false :flood true})]))]
    (is (= :critical (:severity f)))
    (is (= ["on" "on"] [(:tap-learning f) (:tap-flood f)]))
    (is (not (re-find #"(?i)black-hol" (:hint f))))
    (is (str/includes? (:hint f) "may move the bridge's entry back to the tap"))
    (is (str/includes? (:hint f) "depends on the kernel and was not read"))))

(deftest the-bridge-detail-json-reads-as-unread
  (let [f (conflict-on bridge-detail-json)]
    (is (= :critical (:severity f)))
    (is (= ["unread" "unread"] [(:tap-learning f) (:tap-flood f)]))
    (is (str/starts-with? (:bridge-ports-error f) "invalid JSON: "))
    (is (str/includes? (:hint f) "If one-301-0 does not learn (nolearning)"))))

(deftest a-host-answering-with-invalid-json-is-unread-in-a-scan
  (let [hosts {emu {:links (links-json carried)
                    :fdb (str/join "\n" (concat (learned guest 4200 dead)
                                                [(str guest " dev one-301-0 master br4200")]))
                    :ports bridge-detail-json}}
        f (first (findings (:evidence (collect hosts configured {})) "evpn-fdb-conflict"))]
    (is (= "unread" (:tap-learning f)))
    (is (str/starts-with? (:bridge-ports-error f) "invalid JSON: "))))
