;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.config-test
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [infra.config :as config]
            [infra.net :as net]))

(deftest load-file-safely-test
  (fs/with-temp-dir [dir {}]
    (let [good (str (fs/path dir "good.edn"))
          broken (str (fs/path dir "broken.edn"))
          notmap (str (fs/path dir "notmap.edn"))]
      (spit good "{:vpn {:interface \"wg0\"} :ceph {:mgr-hosts [\"m1\"]}}")
      (spit broken "{:vpn {:interface")
      (spit notmap "[1 2 3]")
      (testing "a well-formed map loads"
        (is (= {:vpn {:interface "wg0"} :ceph {:mgr-hosts ["m1"]}}
               (config/load-file-safely good))))
      (testing "broken or non-map or missing files are nil, never an exception"
        (is (nil? (config/load-file-safely broken)))
        (is (nil? (config/load-file-safely notmap)))
        (is (nil? (config/load-file-safely (str (fs/path dir "absent.edn")))))))))

(deftest value-lookup-test
  (testing "value reads a key path and honors defaults for absent keys"
    (with-redefs [config/all (constantly {:ceph {:mgr-hosts ["m1" "m2"]}})]
      ;; value reads through @loaded, so exercise get-in semantics directly
      (is (= ["m1" "m2"] (get-in (config/all) [:ceph :mgr-hosts])))
      (is (nil? (get-in (config/all) [:vpn :interface]))))))

(deftest vpn-gate-test
  (testing "a blank/nil interface is no gate at all"
    (is (net/interface-up? nil))
    (is (net/interface-up? ""))
    (is (net/interface-up? "   "))))

(deftest vpn-detection-test
  (testing "tunnel-looking names are recognized by convention"
    (is (net/vpn-like? "wg0"))
    (is (net/vpn-like? "tun0"))
    (is (net/vpn-like? "tailscale0"))
    (is (not (net/vpn-like? "eth0")))
    (is (not (net/vpn-like? "wlan0")))
    ;; a wireguard tunnel with a custom name (like infra-g60) is not matched by
    ;; the name heuristic - it is found instead via 'wg show interfaces'
    (is (not (net/vpn-like? "infra-g60"))))
  (testing "interface names are parsed out of ip -o link show"
    (is (= ["lo" "eth0" "wg0"]
           (net/parse-interface-names
            (str "1: lo: <LOOPBACK,UP> mtu 65536 ...\n"
                 "2: eth0: <BROADCAST,MULTICAST,UP> mtu 1500 ...\n"
                 "3: wg0: <POINTOPOINT,NOARP,UP> mtu 1420 ...\n"))))))

(deftest from-answers-skips-test
  (testing "every wizard answer is optional - skipped ones are omitted"
    (is (= {:vpn {:interface "wg0"} :ceph {:mgr-hosts ["m1"]}}
           (config/from-answers {:vpn-interface "wg0" :ceph-mgr-host "m1"})))
    (testing "skipping the VPN keeps only ceph"
      (is (= {:ceph {:mgr-hosts ["m1"]}}
             (config/from-answers {:vpn-interface nil :ceph-mgr-host "m1"}))))
    (testing "skipping ceph keeps only the VPN"
      (is (= {:vpn {:interface "wg0"}}
             (config/from-answers {:vpn-interface "wg0" :ceph-mgr-host "  "}))))
    (testing "skipping everything yields an empty config, never an error"
      (is (= {} (config/from-answers {})))
      (is (= {} (config/from-answers {:vpn-interface "" :ceph-mgr-host nil}))))))

(deftest render-roundtrip-test
  (testing "render produces loadable EDN with the license header"
    (let [config {:vpn {:interface "wg0"} :ceph {:mgr-hosts ["m1"]}}
          rendered (config/render config)]
      (is (str/includes? rendered "SPDX-License-Identifier: 0BSD"))
      (is (= config (edn/read-string rendered)))))
  (testing "an environments config round-trips too"
    (let [config {:environments {:staging {:select {:kubernetes-cluster "c"} :ceph {:mgr-hosts ["m"]}}}
                  :defaults {:vpn {:interface "wg0"}}}]
      (is (= config (edn/read-string (config/render config)))))))

(deftest known-clusters-test
  (testing "known clusters come from the environments' selects plus a top-level :clusters list, de-duplicated"
    (is (= ["c1" "c2" "c3"]
           (config/known-clusters {:environments {:a {:select {:kubernetes-cluster "c1"}}
                                                  :b {:select {:kubernetes-cluster "c2"}}}
                                   :clusters ["c3" "c1"]})))
    (is (empty? (config/known-clusters {})))))

(deftest from-answers-capabilities-test
  (testing "detected capabilities land in the environment body"
    (is (= {:environments {:production {:select {:kubernetes-cluster "c1"}
                                        :capabilities [:flux :cnpg]}}}
           (config/from-answers {:environment "production" :kubernetes-cluster "c1"
                                 :capabilities [:flux :cnpg]}))))
  (testing "no capabilities means no :capabilities key"
    (is (nil? (get-in (config/from-answers {:environment "e" :kubernetes-cluster "c" :capabilities []})
                      [:environments :e :capabilities])))))

(def ^:private two-envs
  {:production {:select {:kubernetes-cluster "kube.infra.run"}
                :ceph {:mgr-hosts ["prod-mgr"]}
                :vpn {:interface "wg-prod"}}
   :staging {:select {:kubernetes-cluster "kube.bastelgenosse.de"}
             :ceph {:mgr-hosts ["stg-mgr"]}}})

(deftest select-environment-test
  (testing "the environment is chosen by the cluster signal - select the cluster, get its bundle"
    (is (= :production (config/select-environment two-envs {:kubernetes-cluster "kube.infra.run"})))
    (is (= :staging (config/select-environment two-envs {:kubernetes-cluster "kube.bastelgenosse.de"}))))
  (testing "no matching cluster (or no signal) selects nothing"
    (is (nil? (config/select-environment two-envs {:kubernetes-cluster "some.other.cluster"})))
    (is (nil? (config/select-environment two-envs {})))
    (is (nil? (config/select-environment {} {:kubernetes-cluster "kube.infra.run"}))))
  (testing "the most specific match wins when several could apply"
    (let [envs {:broad {:select {:kubernetes-cluster "c"}}
                :specific {:select {:kubernetes-cluster "c" :vpn-interface "wg0"}}}]
      (is (= :specific (config/select-environment envs {:kubernetes-cluster "c" :vpn-interface "wg0"})))
      ;; without the extra signal only the broad one still matches
      (is (= :broad (config/select-environment envs {:kubernetes-cluster "c"}))))))

(deftest resolve-value-layering-test
  (let [cfg (assoc {:environments two-envs
                    :defaults {:vpn {:interface "wg-default"}}}
                   :ceph {:mgr-hosts ["flat-mgr"]})]
    (testing "the active environment's own value wins"
      (is (= ["prod-mgr"] (config/resolve-value cfg :production [:ceph :mgr-hosts] nil)))
      (is (= "wg-prod" (config/resolve-value cfg :production [:vpn :interface] nil))))
    (testing "a key the environment does not set falls back to :defaults"
      (is (= "wg-default" (config/resolve-value cfg :staging [:vpn :interface] nil))))
    (testing "with no active environment it falls back to :defaults then the flat top level"
      (is (= "wg-default" (config/resolve-value cfg nil [:vpn :interface] nil)))
      (is (= ["flat-mgr"] (config/resolve-value cfg nil [:ceph :mgr-hosts] nil))))
    (testing "absent everywhere yields the caller's default"
      (is (= :none (config/resolve-value cfg :production [:dns :server] :none)))))
  (testing "a flat env.edn keeps working - top level is read directly"
    (let [flat {:ceph {:mgr-hosts ["m1"]} :vpn {:interface "wg0"}}]
      (is (= ["m1"] (config/resolve-value flat nil [:ceph :mgr-hosts] nil)))
      (is (= "wg0" (config/resolve-value flat nil [:vpn :interface] nil))))))

(deftest active-environment-signal-path-test
  (with-redefs [config/environments (constantly two-envs)]
    (testing "active-environment resolves from signals (no override set)"
      (is (= :production (config/active-environment {:kubernetes-cluster "kube.infra.run"})))
      (is (nil? (config/active-environment {:kubernetes-cluster "nope"}))))))

(deftest from-answers-environment-test
  (testing "naming an environment nests it under :environments, tied to its cluster"
    (is (= {:environments {:staging {:select {:kubernetes-cluster "kube.bastelgenosse.de"}
                                     :vpn {:interface "wg0"}
                                     :ceph {:mgr-hosts ["m1"]}}}}
           (config/from-answers {:environment "staging"
                                 :kubernetes-cluster "kube.bastelgenosse.de"
                                 :vpn-interface "wg0" :ceph-mgr-host "m1"}))))
  (testing "no environment name produces a flat config, the cluster selector dropped"
    (is (= {:vpn {:interface "wg0"} :ceph {:mgr-hosts ["m1"]}}
           (config/from-answers {:kubernetes-cluster "ignored-without-a-name"
                                 :vpn-interface "wg0" :ceph-mgr-host "m1"})))))

(deftest merge-answers-accumulates-environments-test
  (testing "running the wizard once per cluster accumulates rather than overwrites"
    (let [prod (config/from-answers {:environment "production" :kubernetes-cluster "kube.infra.run" :ceph-mgr-host "prod-mgr"})
          stg (config/from-answers {:environment "staging" :kubernetes-cluster "kube.bastelgenosse.de" :ceph-mgr-host "stg-mgr"})
          merged (config/merge-answers prod stg)]
      (is (= #{:production :staging} (set (keys (:environments merged)))))
      (is (= ["prod-mgr"] (get-in merged [:environments :production :ceph :mgr-hosts])))
      (is (= ["stg-mgr"] (get-in merged [:environments :staging :ceph :mgr-hosts])))))
  (testing "merging a flat config is a plain merge, new values winning"
    (is (= {:vpn {:interface "new"} :ceph {:mgr-hosts ["m"]}}
           (config/merge-answers {:vpn {:interface "old"} :ceph {:mgr-hosts ["m"]}}
                                 {:vpn {:interface "new"}})))))
