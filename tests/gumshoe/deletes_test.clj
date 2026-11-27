;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.deletes-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.deletes :as deletes]
            [gumshoe.deletion :as deletion]
            [gumshoe.effect :as effect]
            [gumshoe.firebook :as firebook]
            [gumshoe.shell :as shell]
            [gumshoe.steps.objects :as objects]
            [gumshoe.storage :as storage]))

(def ^:private pins ["--ignore-not-found" "--interactive=false"])

(defn- refused? [f] (try (f) false (catch clojure.lang.ExceptionInfo _ true)))

(deftest every-delete-carries-the-pinned-flags
  (is (= pins deletes/pinned-flags))
  (doseq [target [{:type "configmaps" :namespace "shop" :name "settings"}
                  {:type "persistentvolumes" :name "pv-1"}
                  {:type "secrets" :namespace "shop" :selector "owner=helm,name=web"}
                  {:type "pods" :namespace "shop" :all? true}]
          options [nil {} {:wait false} {:wait true} {:cascade :orphan}
                   {:extra ["--request-timeout=30s"]}]]
    (let [args (deletes/args target options)]
      (is (every? (set args) pins) (pr-str target options))
      (is (= 1 (count (filter #{"delete"} args)))))))

(deftest a-target-becomes-its-selection
  (is (= ["--namespace=shop" "delete" "configmaps" "settings" "--ignore-not-found" "--interactive=false"]
         (deletes/args {:type "configmaps" :namespace "shop" :name "settings"})))
  (is (= ["delete" "persistentvolumes" "pv-1" "--ignore-not-found" "--interactive=false" "--wait=false"]
         (deletes/args {:type "persistentvolumes" :name "pv-1"} {:wait false})))
  (is (= ["--namespace=shop" "delete" "secrets" "--selector=owner=helm" "--ignore-not-found" "--interactive=false"]
         (deletes/args {:type "secrets" :namespace "shop" :selector "owner=helm"})))
  (is (= ["--namespace=shop" "delete" "pods" "--all" "--ignore-not-found" "--interactive=false"]
         (deletes/args {:type "pods" :namespace "shop" :all? true}))))

(deftest the-seam-passes-cascade-wait-and-allowed-extras
  (is (= ["--namespace=a" "delete" "statefulset" "web" "--ignore-not-found" "--interactive=false"
          "--cascade=orphan" "--wait=false" "--request-timeout=30s"]
         (deletes/args {:type "statefulset" :namespace "a" :name "web"}
                       {:cascade :orphan :wait false :extra ["--request-timeout=30s"]})))
  (doseq [c [:background :foreground :orphan]]
    (is (some #{(str "--cascade=" (name c))} (deletes/args {:type "t" :name "n"} {:cascade c}))))
  (testing "an absent :wait leaves kubectl's default, so a site states the wait it bounds"
    (is (not-any? #(str/starts-with? % "--wait") (deletes/args {:type "t" :name "n"})))
    (is (some #{"--wait=true"} (deletes/args {:type "t" :name "n"} {:wait true})))))

(deftest a-pinned-flag-cannot-be-turned-back-off
  (doseq [extra [["--ignore-not-found=false"] ["--ignore-not-found"] ["--interactive=true"]
                 ["--interactive"] ["-i"] ["-i=true"]]]
    (is (refused? #(deletes/args {:type "t" :name "n"} {:extra extra})) (pr-str extra))))

(deftest the-seam-refuses-what-it-does-not-name
  (testing "a flag with its own key, or one outside the allowlist, is refused in :extra"
    (doseq [extra [["--wait=true"] ["--cascade=orphan"] ["--selector=a=b"] ["-l" "a=b"] ["--all"]
                   ["--namespace=x"] ["-n" "x"] ["--context=prod"] ["--dry-run=server"]
                   ["--force"] ["--filename=x.yaml"] ["-f" "x.yaml"] ["--all-namespaces"]
                   ["--request-timeout="] [:keyword] "--request-timeout=30s"]]
      (is (refused? #(deletes/args {:type "t" :name "n"} {:extra extra})) (pr-str extra))))
  (testing "values outside the seam's domain"
    (is (refused? #(deletes/args {:type "t" :name "n"} {:cascade "orphan"})))
    (is (refused? #(deletes/args {:type "t" :name "n"} {:cascade :forced})))
    (is (refused? #(deletes/args {:type "t" :name "n"} {:wait "false"})))
    (is (refused? #(deletes/args {:type "t" :name "n"} {:ignore-not-found false})))
    (is (refused? #(deletes/args {:type "t" :name "n"} {:interactive true})))))

(deftest a-target-names-exactly-one-selection
  (doseq [target [{} nil "configmaps settings" {:name "n"} {:type "" :name "n"} {:type "t"}
                  {:type "t" :name "n" :selector "a=b"} {:type "t" :name "n" :all? true}
                  {:type "t" :name ""} {:type "t" :name "n" :namespace ""}
                  {:type "t" :selector " "} {:type "t" :all? "yes" :namespace "a"}
                  {:type "t" :all? true}]]
    (is (refused? #(deletes/args target)) (pr-str target))
    (is (refused? #(deletes/dry-run-args target)) (pr-str target))))

(deftest the-dry-run-refuses-a-missing-target
  (testing "the dry run pins --interactive=false and leaves --ignore-not-found out"
    (is (= ["--namespace=shop" "delete" "configmaps" "settings" "--interactive=false" "--dry-run=server"]
           (deletes/dry-run-args {:type "configmaps" :namespace "shop" :name "settings"})))
    (is (= (deletes/dry-run-args {:type "configmaps" :namespace "shop" :name "settings"})
           (deletion/dry-run-args {:type "configmaps" :namespace "shop" :name "settings" :extra :ignored})))))

(defn- fake-kubectl
  "kubectl's answer to a delete or a dry run over a cluster holding the
   names in the `present` atom: NotFound with exit 1 for a missing name
   unless --ignore-not-found is given, and a real delete removes the name."
  [present argv]
  (let [args (vec (drop-while #(not= "delete" %) argv))
        [_ _ name] args]
    (cond
      (contains? @present name)
      (do (when-not (some #{"--dry-run=server"} argv) (swap! present disj name))
          {:exit 0 :out (str name " deleted") :err ""})
      (some #{"--ignore-not-found"} argv) {:exit 0 :out "" :err ""}
      :else {:exit 1 :out "" :err (str "Error from server (NotFound): \"" name "\" not found")})))

(deftest absent-before-the-run-is-refused
  (let [present (atom #{"settings"})
        calls (atom [])]
    (with-redefs [shell/execute (fn [& argv] (swap! calls conj (vec argv)) (fake-kubectl present argv))]
      (testing "a pick naming no object - a typo - fails the dry run, which the book turns into a refusal"
        (let [result (deletion/dry-run "ctx" {:type "configmaps" :namespace "shop" :name "settings-old"})]
          (is (false? (:ok? result)))
          (is (str/includes? (:message result) "NotFound"))))
      (testing "an object that exists passes it, and nothing is deleted"
        (is (:ok? (deletion/dry-run "ctx" {:type "configmaps" :namespace "shop" :name "settings"})))
        (is (= #{"settings"} @present))))
    (is (every? #(some #{"--interactive=false"} %) @calls))
    (is (not-any? #(some #{"--ignore-not-found"} %) @calls))))

(deftest vanished-mid-run-is-tolerated
  (testing "a plan deleting two objects, the first gone between the judgement and the delete, runs to its end"
    (let [present (atom #{"db"})
          ran (atom [])
          plan (objects/delete-effect "ctx" [{:type "deployments" :namespace "apps" :name "web"}
                                             {:type "statefulsets" :namespace "apps" :name "db"}]
                                      "nothing")]
      (with-redefs [shell/run-attributed (fn [& argv]
                                           (swap! ran conj (vec argv))
                                           (:exit (fake-kubectl present argv)))]
        (is (true? (effect/run! plan))))
      (is (= 2 (count @ran)) "the second delete ran")
      (is (= #{} @present))))
  (testing "the same plan without --ignore-not-found stops at the first object, the second never deleted"
    (let [present (atom #{"db"})
          ran (atom [])
          plan (mapv #(vec (remove #{"--ignore-not-found"} %))
                     (objects/delete-effect "ctx" [{:type "deployments" :namespace "apps" :name "web"}
                                                   {:type "statefulsets" :namespace "apps" :name "db"}]
                                            "nothing"))]
      (with-redefs [shell/run-attributed (fn [& argv]
                                           (swap! ran conj (vec argv))
                                           (:exit (fake-kubectl present argv)))]
        (is (false? (effect/run! plan))))
      (is (= 1 (count @ran)))
      (is (= #{"db"} @present)))))

(deftest the-library-sites-build-through-the-builder
  (is (= ["--namespace=shop" "delete" "configmaps" "settings" "--ignore-not-found" "--interactive=false" "--wait=false"]
         (deletion/delete-args {:type "configmaps" :namespace "shop" :name "settings"})))
  (is (= [[:kubectl "ctx" "--namespace=apps" "delete" "deployments" "web" "--ignore-not-found" "--interactive=false" "--wait=false"]]
         (objects/delete-effect "ctx" [{:type "deployments" :namespace "apps" :name "web"}] "none")))
  (is (= [:kubectl "ctx" "--namespace=shop" "delete" "statefulset" "web" "--ignore-not-found" "--interactive=false"
          "--cascade=orphan" "--wait=false" "--request-timeout=30s"]
         (:effect (first (filter #(= :delete (:stage %))
                                 (storage/statefulset-resize-steps "ctx" {:metadata {:namespace "shop" :name "web"}
                                                                :spec {:volumeClaimTemplates [{:metadata {:name "data"}}]}}
                                                         "data" "2Gi" []))))))
  (testing "extinguish! runs its delete through the builder"
    (let [calls (atom [])]
      (with-redefs [shell/execute (fn [& argv] (swap! calls conj (vec argv)) {:exit 1 :out "" :err "stop here"})]
        (binding [*err* (java.io.StringWriter.)]
          (firebook/extinguish! "ctx")))
      (is (= [(into ["kubectl" "--context=ctx"] (deletes/args {:type "namespace" :name firebook/drill-namespace}))]
             (take 1 @calls))))))

;; --- every kubectl delete goes through the builder -----------------------------

(def ^:private shipped-roots
  ["libraries" "runbooks" "playbooks" "firebooks" "casebook-example" "examples"])

(def ^:private allowed
  "Files that may hold the literal \"delete\" without issuing a kubectl
   delete, each with the reason."
  {"libraries/gumshoe/deletes.clj" "the builder"
   "libraries/gumshoe/detectives/rbac.clj" "an RBAC verb in a rule set"
   "libraries/gumshoe/prerequisites.clj" "the verb of a kubectl auth can-i question"
   "libraries/gumshoe/testing/plan.clj" "a verb the plan reader classifies"})

(defn- shipped-files []
  (->> (concat shipped-roots
               (for [tool (fs/list-dir "tools") sub ["libraries" "runbooks" "playbooks" "firebooks"]]
                 (str (fs/path tool sub))))
       (filter fs/directory?)
       (mapcat #(fs/glob % "**.{clj,bb,edn}"))
       (map str)
       sort))

(defn- delete-literal-lines
  [text]
  (keep-indexed (fn [i line] (when (re-find #"\"delete\"" line) (inc i))) (str/split-lines text)))

(deftest the-scanner-finds-a-literal-delete
  (is (= [2] (delete-literal-lines "(ns x)\n(effect/kubectl ctx \"delete\" \"pod\" n)")))
  (is (empty? (delete-literal-lines "\"delete an object\" (deletes/effect ctx t)"))))

(deftest no-kubectl-delete-is-assembled-outside-the-builder
  (let [files (shipped-files)
        hits (for [f files
                   :when (not (contains? allowed f))
                   line (delete-literal-lines (slurp f))]
               (str f ":" line))]
    (is (< 100 (count files)) "the scan reached the shipped sources")
    (is (some #{"libraries/gumshoe/deletes.clj"} files))
    (is (empty? hits) (str "build these through gumshoe.deletes: " (str/join ", " hits)))
    (testing "every allowlisted file still exists and still needs its entry"
      (doseq [f (keys allowed)]
        (is (seq (delete-literal-lines (slurp f))) f)))))
