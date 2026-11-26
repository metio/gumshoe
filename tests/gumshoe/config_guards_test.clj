;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.config-guards-test
  "env.edn parts of the wrong shape fail closed: a malformed :clusters or a
   dropped environment never widens the cluster gate, a malformed :announce
   refuses the change before anything runs, and the setup wizard never
   rewrites a file whose parts it could not read."
  (:require [babashka.fs :as fs]
            [clojure.edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.config :as config]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.spec :as spec]))

(defn- quietly
  "Runs f with stdout and stderr captured, as [result out err]."
  [f]
  (let [out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        result (binding [*out* out *err* err] (f))]
    [result (str out) (str err)]))

(defn- names [clusters] (vec (filter string? clusters)))
(defn- unread? [clusters] (boolean (some config/unread-allow-list? clusters)))

;; --- :clusters ----------------------------------------------------------------------

(deftest clusters-problem-accepts-only-names
  (doseq [ok [nil "kube.example.org" ["a" "b"] '("a") #{"a"} []]]
    (is (nil? (config/clusters-problem {:clusters ok})) (pr-str ok)))
  (doseq [bad ['kube.prod.example.org :kube 5 true {:a "b"} ["a" 'b] [""] "" ["a" nil] [["a"]]]]
    (is (string? (config/clusters-problem {:clusters bad})) (pr-str bad))))

(deftest a-malformed-clusters-matches-no-cluster
  (doseq [bad ['kube.prod.example.org :kube.prod.example.org 5 {:kube "x"} [:a] ""]]
    (testing (pr-str bad)
      (let [known (config/known-clusters {:clusters bad})]
        (is (seq known) "the allow-list is never empty, so the gate stays closed")
        (is (unread? known))
        (is (not-any? #{"kube.prod.example.org"} known))
        (is (str/includes? (str/join ", " (map str known)) ":clusters"))))))

(deftest a-malformed-entry-keeps-the-readable-names
  (let [known (config/known-clusters {:clusters ["kube.a" 'kube.b]})]
    (is (= ["kube.a"] (names known)))
    (is (unread? known))))

(deftest a-well-formed-allow-list-has-no-unread-entry
  (is (= ["c1" "a"] (config/known-clusters {:environments {:p {:select {:kubernetes-cluster "c1"}}}
                                            :clusters ["a"]})))
  (is (= [] (config/known-clusters {}))))

(deftest normalize-warns-about-a-malformed-clusters-and-keeps-it-unread
  (let [{c :config w :warnings} (config/normalize {:clusters 'kube.prod.example.org})]
    (is (= 1 (count w)))
    (is (str/includes? (first w) ":clusters"))
    (is (unread? (config/known-clusters c)))))

(deftest a-dropped-environment-keeps-the-gate-closed
  (doseq [raw [{:environments [{:select {:kubernetes-cluster "kube.prod"}}]}
               {:environments {:prod {:select "kube.prod"}}}
               {:environments {:prod "kube.prod"}}]]
    (testing (pr-str raw)
      (let [{c :config} (config/normalize raw)
            known (config/known-clusters c)]
        (is (seq known))
        (is (unread? known))
        (is (= [] (names known)))))))

(deftest the-marker-stays-out-of-what-a-detective-reads-and-what-is-written
  (let [{c :config} (config/normalize {:clusters 5 :environments [1] :x 1})]
    (is (not (contains? (config/active-config c nil) ::config/unread-clusters)))
    (is (not (str/includes? (config/render c) "unread-clusters")))))

(def ^:private cluster-item #'runbook/cluster-item)

(deftest the-cluster-prerequisite-refuses-every-cluster-on-a-malformed-clusters
  (fs/with-temp-dir [dir {}]
    (let [file (str (fs/path dir "env.edn"))]
      (spit file "{:clusters kube.prod.example.org}")
      (let [[loaded _ err] (quietly #(config/load-file-safely file))]
        (is (str/includes? err ":clusters"))
        (with-redefs [config/all (constantly loaded)
                      kubectl/current-cluster (constantly "kube.prod.example.org")]
          (let [{:keys [ok? label]} ((second (cluster-item [])))]
            (is (false? ok?))
            (is (str/includes? label ":clusters") label)))))))

;; --- the first existing env.edn -----------------------------------------------------

(deftest the-first-existing-env-edn-wins-even-when-it-is-broken
  (fs/with-temp-dir [dir {}]
    (let [broken (str (fs/path dir "env.edn"))
          home (str (fs/path dir "home.edn"))
          absent (str (fs/path dir "absent.edn"))]
      (spit home "{:clusters [\"kube.home.example\"] :ceph {:mgr-hosts [\"mgr-1.home.example\"]}}")
      (doseq [content ["{:environments {:prod " "[:not :a :map]" "#=(java.lang.System/exit 1)"]]
        (testing content
          (spit broken content)
          (let [[loaded _ err] (quietly #(config/load-first [absent broken home]))]
            (is (str/includes? err broken) "the warning names the file")
            (is (re-find #"not readable EDN|not a map" err) "and the reason")
            (is (str/includes? err "every cluster is refused"))
            (is (nil? (config/resolve-value loaded nil [:ceph :mgr-hosts] nil))
                "nothing of the next file applies")
            (is (= [] (names (config/known-clusters loaded))))
            (is (unread? (config/known-clusters loaded)) "the allow-list matches no cluster")
            (with-redefs [config/all (constantly loaded)
                          kubectl/current-cluster (constantly "kube.home.example")]
              (let [{:keys [ok? label]} ((second (cluster-item [])))]
                (is (false? ok?) "the cluster the next file names is refused")
                (is (str/includes? label broken) label)))))))))

(deftest a-missing-env-edn-falls-through-to-the-next
  (fs/with-temp-dir [dir {}]
    (let [home (str (fs/path dir "home.edn"))]
      (spit home "{:clusters [\"kube.home.example\"]}")
      (let [[loaded _ err] (quietly #(config/load-first [nil (str (fs/path dir "absent.edn")) home]))]
        (is (= "" err))
        (is (= ["kube.home.example"] (config/known-clusters loaded)))))))

(deftest an-empty-env-edn-is-an-empty-config-and-shadows-the-next
  (fs/with-temp-dir [dir {}]
    (let [empty-file (str (fs/path dir "env.edn"))
          home (str (fs/path dir "home.edn"))]
      (spit empty-file "")
      (spit home "{:clusters [\"kube.home.example\"]}")
      (let [[loaded _ err] (quietly #(config/load-first [empty-file home]))]
        (is (= {} loaded))
        (is (= "" err))))))

(deftest no-env-edn-at-all-is-an-empty-config
  (fs/with-temp-dir [dir {}]
    (is (= {} (config/load-first [(str (fs/path dir "a.edn")) (str (fs/path dir "b.edn"))])))))

;; --- :announce ----------------------------------------------------------------------

(deftest announcers-of-reads-only-announcer-lists
  (is (= {:announcers []} (announce/announcers-of nil)))
  (is (= {:announcers []} (announce/announcers-of [])))
  (is (= [{:type :webhook :url "u"}] (:announcers (announce/announcers-of [{:type :webhook :url "u"}]))))
  (let [one (announce/announcers-of {:type :webhook :url "u"})]
    (is (= [{:type :webhook :url "u"}] (:announcers one)) "a single announcer map is a list of one")
    (is (nil? (:warning one)) "a single announcer map is a supported shape"))
  (doseq [bad [:matrix true false 5 "matrix" [:matrix] [{:url "u"}] [{:type "webhook"}] {:url "u"}]]
    (is (string? (:problem (announce/announcers-of bad))) (pr-str bad))))

(deftest a-malformed-announce-refuses-before-any-announcer-runs
  (doseq [bad [:matrix true 5 [{:type :webhook :url "u"} :matrix]]]
    (testing (pr-str bad)
      (let [posted (atom 0)]
        (with-redefs [config/env-value (fn [_ path & _] (when (= [:announce] path) bad))
                      announce/announce-via (fn [& _] (swap! posted inc))]
          (let [e (try (announce/announce! "kube.example.org" {:cluster "k"} "change") nil
                       (catch clojure.lang.ExceptionInfo e e))]
            (is (some? e))
            (is (str/includes? (ex-message e) ":announce"))
            (is (str/includes? (ex-message e) "nothing was changed")))
          (is (= 0 @posted)))))))

(deftest a-single-announcer-map-is-announced-once
  (let [posted (atom [])]
    (with-redefs [config/env-value (fn [_ path & _] (when (= [:announce] path) {:type :webhook :url "u"}))
                  announce/announce-via (fn [announcer & _] (swap! posted conj announcer))]
      (let [[_ _ err] (quietly #(announce/announce! "kube.example.org" {:cluster "k"} "change"))]
        (is (= [{:type :webhook :url "u"}] @posted))
        (is (not (str/includes? err ":announce")) "a supported shape is not warned about")))))

(deftest spec-names-a-malformed-clusters-and-announce
  (is (some #(str/includes? % ":clusters") (spec/env-config-problems {:clusters 'kube.example.org})))
  (is (some #(str/includes? % ":announce") (spec/env-config-problems {:announce :matrix})))
  (is (some #(str/includes? % ":announce")
            (spec/env-config-problems {:environments {:p {:select {:kubernetes-cluster "k"} :announce [:matrix]}}})))
  (is (nil? (spec/env-config-problems {:clusters ["a"] :announce [{:type :webhook :url "u"}]
                                       :environments {:p {:select {:kubernetes-cluster "k"}
                                                          :announce {:type :matrix}}}}))))

;; --- the setup wizard ---------------------------------------------------------------

(deftest file-problems-names-what-loading-would-drop
  (fs/with-temp-dir [dir {}]
    (let [file (str (fs/path dir "env.edn"))]
      (is (nil? (config/file-problems file)) "a missing file has none")
      (doseq [[content expected] [["{:vpn {:interface \"wg0\"}}" nil]
                                  ["{:environments {:p {:select \"k\"}}}" ":select"]
                                  ["{:environments [{:select {}}]}" ":environments"]
                                  ["{:defaults [1]}" ":defaults"]
                                  ["{:clusters kube.example.org}" ":clusters"]
                                  ["[1 2]" "not a map"]
                                  ["{:a " "not readable EDN"]]]
        (spit file content)
        (let [problems (config/file-problems file)]
          (if expected
            (is (some #(str/includes? % expected) problems) (pr-str content problems))
            (is (nil? problems) content)))))))

(defn- wizard
  "The setup wizard's action, loaded from its book."
  []
  (binding [runbook/*loading* true]
    (:action (load-file "runbooks/setup/init.clj"))))

(deftest the-wizard-refuses-to-rewrite-a-file-it-can-not-read-whole
  (fs/with-temp-dir [dir {}]
    (let [file (str (fs/path dir "env.edn"))
          init (wizard)]
      (doseq [content ["{:environments {:prod {:select \"kube.prod\" :vpn {:interface \"wg0\"}}}}"
                       "{:environments [{:select {:kubernetes-cluster \"kube.prod\"}}]}"
                       "{:defaults [1] :vpn {:interface \"wg0\"}}"
                       "{:clusters kube.prod}"
                       "{:environments {:prod "]]
        (testing content
          (spit file content)
          (let [asked (atom 0)]
            (with-redefs [config/primary-path file
                          interact/ask-text (fn [& _] (swap! asked inc) "x")
                          interact/choose-one (fn [& _] (swap! asked inc) nil)
                          interact/confirm! (fn [& _] (swap! asked inc) true)
                          kubectl/current-cluster (constantly "kube.prod")]
              (let [[result _ err] (quietly #(init {} {}))]
                (is (false? result))
                (is (str/includes? err "nothing was written") err)
                (is (= 0 @asked) "the wizard asks nothing it could not write")
                (is (= content (slurp file)) "the file is left exactly as it was")))))))))

(deftest the-wizard-still-writes-over-a-file-it-reads-whole
  (fs/with-temp-dir [dir {}]
    (let [file (str (fs/path dir "env.edn"))
          init (wizard)]
      (spit file "{:announce [{:type :webhook :url \"u\"}] :clusters [\"kube.prod\"]}")
      (with-redefs [config/primary-path file
                    config/backup-dir (constantly (str (fs/path dir "state")))
                    interact/ask-text (fn [title & _] (when (= "Ceph mgr host" title) "mgr-1.example.org"))
                    interact/choose-one (fn [& _] nil)
                    interact/confirm! (fn [& _] true)
                    kubectl/current-cluster (constantly "")]
        (let [[result] (quietly #(init {} {}))
              written (clojure.edn/read-string (slurp file))]
          (is (true? result))
          (is (= ["mgr-1.example.org"] (get-in written [:ceph :mgr-hosts])))
          (is (= [{:type :webhook :url "u"}] (:announce written)) "the sections it did not ask about are kept")
          (is (= ["kube.prod"] (:clusters written)))
          (is (= 1 (count (fs/list-dir (fs/path dir "state")))) "the previous file is kept in the state directory"))))))
