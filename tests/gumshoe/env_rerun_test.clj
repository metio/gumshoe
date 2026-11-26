;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.env-rerun-test
  "Running setup/init again keeps every value of env.edn it did not ask
   about, keeps the previous file as a backup, and its confirmation names
   each value the write changes."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.config :as config]
            [gumshoe.inputs :as inputs]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.net :as net]
            [gumshoe.runbook :as runbook]))

(deftest a-rerun-on-a-flat-file-keeps-what-it-did-not-ask
  (let [existing {:matrix {:domain "example.org"} :vpn {:interface "wg0"} :dns {:zone "example.org"}}
        asked (set (map :env-path (inputs/missing existing nil)))
        new (config/from-answers {:inputs [[[:matrix :host] "synapse.example.org"]]})]
    (is (not (contains? asked [:matrix :domain])) "the wizard does not ask for what is set")
    (is (= {:matrix {:domain "example.org" :host "synapse.example.org"}
            :vpn {:interface "wg0"}
            :dns {:zone "example.org"}}
           (config/merge-answers existing new)))))

(deftest a-rerun-naming-the-same-environment-keeps-its-values
  (let [existing {:environments {:production {:select {:kubernetes-cluster "prod"}
                                              :vpn {:interface "wg0"}
                                              :matrix {:domain "example.org"}
                                              :dns {:zone "example.org"}}}}
        asked (set (map :env-path (inputs/missing existing :production)))
        new (config/from-answers {:environment "production" :kubernetes-cluster "prod"
                                  :inputs [[[:mail :host] "mail.example.org"]]})]
    (is (not (contains? asked [:matrix :domain])))
    (is (= {:environments {:production {:select {:kubernetes-cluster "prod"}
                                        :vpn {:interface "wg0"}
                                        :matrix {:domain "example.org"}
                                        :dns {:zone "example.org"}
                                        :mail {:host "mail.example.org"}}}}
           (config/merge-answers existing new)))))

(deftest a-new-value-wins-and-other-environments-stay
  (is (= {:environments {:production {:vpn {:interface "wg1" :peer "p"}}
                         :staging {:vpn {:interface "wg9"}}}
          :defaults {:dns {:zone "z"}}}
         (config/merge-answers {:environments {:production {:vpn {:interface "wg0" :peer "p"}}
                                               :staging {:vpn {:interface "wg9"}}}
                                :defaults {:dns {:zone "z"}}}
                               {:environments {:production {:vpn {:interface "wg1"}}}}))))

(deftest deep-merge-replaces-what-is-not-a-map
  (is (= {:a {:b [2]}} (config/deep-merge {:a {:b [1]}} {:a {:b [2]}})))
  (is (= {:a {:c 1}} (config/deep-merge {:a "x"} {:a {:c 1}})))
  (is (= {:a {:b 1}} (config/deep-merge {:a {:b 1}} {}))))

(deftest changes-names-every-value-that-moves
  (let [before {:environments {:production {:vpn {:interface "wg0"} :ceph {:mgr-hosts ["m1"]}}}}
        after {:environments {:production {:vpn {:interface "wg1"} :ceph {:mgr-hosts ["m1"]}
                                           :mail {:host "mail.example.org"}}}}]
    (is (= {:added [[:environments :production :mail :host]]
            :changed [[:environments :production :vpn :interface]]
            :removed []}
           (config/changes before after)))
    (is (= ["adds :environments :production :mail :host"
            "changes :environments :production :vpn :interface"]
           (config/change-items (config/changes before after))))
    (is (= ["REMOVES :dns :zone"] (config/change-items (config/changes {:dns {:zone "z"}} {}))))
    (is (= [] (config/change-items (config/changes before before))))))

(deftest a-write-keeps-the-previous-file
  (fs/with-temp-dir [dir {}]
    (let [path (str (fs/path dir "env.edn"))]
      (testing "a first write has nothing to keep"
        (is (= {:path path :backup nil} (config/write! {:vpn {:interface "wg0"}} path))))
      (let [before (slurp path)
            {:keys [backup]} (config/write! {:vpn {:interface "wg1"}} path)]
        (is (some? backup))
        (is (str/starts-with? backup (str path ".")))
        (is (str/ends-with? backup ".bak"))
        (is (= before (slurp backup)) "the backup holds the file as it was")
        (is (= {:vpn {:interface "wg1"}} (edn/read-string (slurp path))))))))

(deftest the-backup-is-named-after-the-time
  (is (= "env.edn.20261126-093005.bak"
         (config/backup-path "env.edn" (java.time.LocalDateTime/of 2026 11 26 9 30 5)))))

(defn- run-init
  "What setup/init writes to path and confirms, run whole with every
   question answered by answer (label -> value) and the cluster prod."
  [path answer]
  (let [confirmed (atom nil)
        config (binding [runbook/*loading* true] (load-file "runbooks/setup/init.clj"))
        out (java.io.StringWriter.)]
    (with-redefs [config/primary-path path
                  config/active-environment (constantly :production)
                  kubectl/current-cluster (constantly "prod")
                  capabilities/detect-answers (constantly {:present [] :unanswered []})
                  net/vpn-candidates (constantly [])
                  interact/ask-text (fn [label & _] (get answer label))
                  interact/confirm! (fn [request] (reset! confirmed request) true)]
      (let [result (binding [*out* out *err* out] ((:action config) {} {}))]
        {:result result :confirmed @confirmed :out (str out)}))))

(deftest init-run-again-keeps-the-environment-and-says-what-it-adds
  (fs/with-temp-dir [dir {}]
    (let [path (str (fs/path dir "env.edn"))
          existing {:environments {:production {:select {:kubernetes-cluster "prod"}
                                                :vpn {:interface "wg0"}
                                                :matrix {:domain "example.org"}
                                                :dns {:zone "example.org"}}}}]
      (spit path (config/render existing))
      (let [{:keys [result confirmed out]} (run-init path {"Environment name" "production" "Ceph mgr host" "mgr-1"})
            written (config/load-file-safely path)]
        (is (true? result) out)
        (is (= [path "adds :environments :production :ceph :mgr-hosts"] (:items confirmed)))
        (is (= (assoc-in existing [:environments :production :ceph :mgr-hosts] ["mgr-1"]) written))
        (is (= 1 (count (filter #(str/ends-with? (str %) ".bak") (fs/list-dir dir)))) "the previous file is kept")
        (is (str/includes? out "the previous env.edn is kept as"))))))
