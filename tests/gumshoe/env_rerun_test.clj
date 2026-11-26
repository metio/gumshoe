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

(defn- mode [path] (fs/posix->str (fs/posix-file-permissions path)))

(deftest a-write-keeps-the-previous-file-outside-the-working-tree
  (fs/with-temp-dir [dir {}]
    (fs/with-temp-dir [state {}]
      (let [path (str (fs/path dir "env.edn"))
            backups (str (fs/path state "gumshoe" "env-backups"))
            backup-at #(config/backup-path backups path (java.time.LocalDateTime/now))]
        (testing "a first write has nothing to keep"
          (is (= {:path path :backup nil} (config/write! {:vpn {:interface "wg0"}} path (backup-at)))))
        (let [before (slurp path)
              {:keys [backup]} (config/write! {:vpn {:interface "wg1"}} path (backup-at))]
          (is (some? backup))
          (is (not (str/starts-with? (str (fs/canonicalize backup)) (str (fs/canonicalize dir))))
              "the backup is not in the env.edn's directory")
          (is (= (str (fs/canonicalize backups)) (str (fs/canonicalize (fs/parent backup)))))
          (is (= before (slurp backup)) "the backup holds the file as it was")
          (is (= "rw-------" (mode backup)))
          (is (= "rwx------" (mode backups)))
          (is (= [] (filter #(str/includes? (str (fs/file-name %)) ".bak") (fs/list-dir dir)))
              "nothing is left beside env.edn")
          (is (= {:vpn {:interface "wg1"}} (edn/read-string (slurp path)))))))))

(deftest a-backup-never-overwrites-another
  (fs/with-temp-dir [dir {}]
    (let [path (str (fs/path dir "env.edn"))
          backup (str (fs/path dir "state" "b.edn.bak"))]
      (spit path "{}")
      (config/write! {:a 1} path backup)
      (is (thrown? Exception (config/write! {:a 2} path backup)))
      (is (= {:a 1} (edn/read-string (slurp path))) "a failed backup writes nothing")
      (is (= "{}" (slurp backup))))))

(deftest a-loose-backup-directory-is-tightened
  (fs/with-temp-dir [dir {}]
    (let [path (str (fs/path dir "env.edn"))
          backups (fs/create-dirs (fs/path dir "state"))]
      (fs/set-posix-file-permissions backups "rwxr-xr-x")
      (spit path "{}")
      (config/write! {:a 1} path (str (fs/path backups "x.edn.bak")))
      (is (= "rwx------" (mode backups))))))

(deftest the-backup-is-named-after-the-path-and-the-time
  (let [at (java.time.LocalDateTime/of 2026 11 26 9 30 5 123000000)
        a (config/backup-path "/state" "/cases/one/env.edn" at)
        b (config/backup-path "/state" "/cases/two/env.edn" at)]
    (is (re-matches #"/state/[0-9a-f]{16}-20261126-093005-123\.edn\.bak" a) a)
    (is (not= a b) "different casebooks never collide")
    (is (= a (config/backup-path "/state" "/cases/one/../one/env.edn" at)) "the path is compared normalised")
    (is (= (subs a 0 24) (subs (config/backup-path "/state" "/cases/one/env.edn" (.plusSeconds at 1)) 0 24))
        "every backup of one env.edn shares its prefix")))

(deftest backups-live-in-the-xdg-state-directory
  (is (= "/xdg/state/gumshoe/env-backups" (config/backup-dir "/xdg/state" "/home/u")))
  (is (= "/home/u/.local/state/gumshoe/env-backups" (config/backup-dir nil "/home/u")))
  (is (= "/home/u/.local/state/gumshoe/env-backups" (config/backup-dir "" "/home/u")))
  (is (= "/home/u/.local/state/gumshoe/env-backups" (config/backup-dir "relative/state" "/home/u"))
      "a relative XDG_STATE_HOME is ignored, as the XDG spec says"))

(defn- run-init
  "What setup/init writes to path and confirms, run whole with every
   question answered by answer (label -> value) and the cluster prod."
  [path answer]
  (let [confirmed (atom nil)
        config (binding [runbook/*loading* true] (load-file "runbooks/setup/init.clj"))
        out (java.io.StringWriter.)]
    (with-redefs [config/primary-path path
                  config/backup-dir (constantly (str (fs/path (fs/parent path) "state")))
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
        (let [[kept] (fs/list-dir (fs/path dir "state"))
              kept (str kept)]
          (is (= [path "adds :environments :production :ceph :mgr-hosts" (str "keeps the current file as " kept)]
                 (:items confirmed))
              "the confirmation names where the backup goes")
          (is (= (assoc-in existing [:environments :production :ceph :mgr-hosts] ["mgr-1"]) written))
          (is (= (config/render existing) (slurp kept)) "the previous file is kept")
          (is (str/includes? out (str "the previous env.edn is kept as " kept)) out)
          (is (= [] (filter #(str/ends-with? (str %) ".bak") (fs/list-dir dir))) "nothing is left beside env.edn"))))))

(deftest an-unwritable-state-directory-refuses-the-write-and-says-why
  (fs/with-temp-dir [dir {}]
    (let [path (str (fs/path dir "env.edn"))
          state (fs/create-dirs (fs/path dir "state"))
          backups (str (fs/path state "gumshoe" "env-backups"))]
      (spit path "{:a 0}")
      (fs/set-posix-file-permissions state "r-xr-xr-x")
      (try
        (let [e (try (config/write! {:a 1} path (config/backup-path backups path (java.time.LocalDateTime/now)))
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
          (is (some? e) "the write is refused")
          (is (true? (::config/backup-refused (ex-data e))))
          (is (= (str "could not keep a backup of env.edn in " backups ": permission denied on "
                      (fs/path state "gumshoe")
                      ". env.edn was not changed. Set XDG_STATE_HOME to a writable directory and run again.")
                 (ex-message e)))
          (is (= "{:a 0}" (slurp path)) "no backup, no write"))
        (finally (fs/set-posix-file-permissions state "rwx------"))))))

(deftest a-backup-that-cannot-be-filled-is-removed
  (fs/with-temp-dir [dir {}]
    (let [path (str (fs/path dir "env.edn"))
          backup (str (fs/path dir "state" "b.edn.bak"))]
      (spit path "{:a 0}")
      (fs/set-posix-file-permissions path "-w-------")
      (try
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"env.edn was not changed"
                              (config/write! {:a 1} path backup)))
        (is (not (fs/exists? backup)) "no empty backup is left behind")
        (finally (fs/set-posix-file-permissions path "rw-------")))
      (is (= "{:a 0}" (slurp path))))))

(deftest init-with-an-unusable-state-directory-refuses-clearly
  (fs/with-temp-dir [dir {}]
    (let [path (str (fs/path dir "env.edn"))]
      (spit path (config/render {:vpn {:interface "wg0"}}))
      ;; run-init keeps backups in <dir>/state; a file there cannot be a directory
      (spit (str (fs/path dir "state")) "")
      (let [{:keys [result out]} (run-init path {"Environment name" "production" "Ceph mgr host" "mgr-1"})]
        (is (false? result) out)
        (is (str/includes? out (str "could not keep a backup of env.edn in " (fs/path dir "state"))) out)
        (is (str/includes? out "env.edn was not changed") out)
        (is (not (str/includes? out "wrote ")) out)
        (is (not (str/includes? out "unexpected error")) out)
        (is (= {:vpn {:interface "wg0"}} (config/load-file-safely path)))))))
