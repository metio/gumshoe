;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.interact-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.command :as command]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.interact :as interact]
            [gumshoe.outcome :as outcome]
            [gumshoe.shell :as shell]
            [gumshoe.ui :as ui]))

(deftest valid-choice-test
  (testing "accepts a candidate"
    (is (interact/valid-choice? ["a" "b"] "a")))
  (testing "rejects a non-candidate"
    (is (not (interact/valid-choice? ["a" "b"] "c"))))
  (testing "rejects nil"
    (is (not (interact/valid-choice? ["a" "b"] nil)))))

(deftest choose-one-test
  (testing "returns nil when there is nothing to choose from"
    (is (nil? (interact/choose-one "Node" [] "worker-1"))))
  (testing "uses a valid provided value without interaction"
    (is (= "worker-1" (interact/choose-one "Node" ["worker-1" "worker-2"] "worker-1")))))

(deftest choose-one-seeds-picker-test
  (let [calls (atom [])]
    (with-redefs [fuzzy/select-single (fn [& args] (swap! calls conj (vec args)) nil)]
      (testing "a typed near-miss seeds the picker query and forbids auto-accept"
        (reset! calls [])
        (interact/choose-one "Node" ["worker-1" "worker-2"] "worker-9")
        (is (= [["Node" ["worker-1" "worker-2"] {:query "worker-9" :auto-select? false :preview nil}]] @calls)
            "the near-miss name seeds the fuzzy query, and auto-select? is false so it never resolves silently"))
      (testing "no provided value opens the picker without a seed (auto-select left on)"
        (reset! calls [])
        (interact/choose-one "Node" ["worker-1" "worker-2"] nil)
        (is (= [["Node" ["worker-1" "worker-2"] {:preview nil}]] @calls)
            "with nothing typed there is no seed to guard, so the plain interactive pick is used"))
      (testing "a declared preview command is passed through to the picker"
        (reset! calls [])
        (interact/choose-one "Node" ["worker-1" "worker-2"] nil "kubectl describe node {}")
        (is (= "kubectl describe node {}" (:preview (last (first @calls))))
            "the book's :preview reaches fzf so the operator sees the node before picking"))
      (testing "candidates are sorted before they reach the picker"
        (reset! calls [])
        (interact/choose-one "Node" ["worker-2" "worker-1"] "worker")
        (is (= ["worker-1" "worker-2"] (second (first @calls))))))))

(deftest choose-one-echoes-selection-test
  (testing "a resolved pick is echoed to the terminal so the transcript shows it"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (interact/choose-one "Namespace" ["moodle-a" "moodle-b"] "moodle-a"))
      (is (re-find #"Namespace" (str err)))
      (is (re-find #"moodle-a" (str err))
          "the chosen value appears on stderr, not only in selections.edn"))))

(deftest choose-many-test
  (testing "returns nil when there is nothing to choose from"
    (is (nil? (interact/choose-many "Certificate" [] ["a"]))))
  (testing "uses valid provided values without interaction"
    (is (= ["a" "b"] (interact/choose-many "Certificate" ["a" "b" "c"] ["a" "b"])))))

(deftest choose-namespaced-test
  (testing "uses valid namespace and name flags without interaction"
    (is (= "monitoring/prometheus"
           (interact/choose-namespaced "Service"
                                       ["monitoring/prometheus" "loki/loki"]
                                       "monitoring"
                                       "prometheus")))))

(deftest confirmation-message-test
  (testing "states the action, the target, and every item"
    (is (= (str "This will delete a PersistentVolumeClaim on 'kube.example.org':\n"
                "  - moodle/data\n"
                "  - moodle/cache")
           (interact/confirmation-message {:action "delete a PersistentVolumeClaim"
                                           :target "kube.example.org"
                                           :items ["moodle/data" "moodle/cache"]})))))

(defn- confirm-with
  "Runs confirm! on `request` with `typed` on stdin (nil for an immediate EOF),
   gum installed and a UI backend whose :confirm approves. Returns the verdict
   and what reached the backend and gum, so a test can show a destructive change
   never takes the yes/no path."
  [request typed]
  (let [backend-calls (atom 0)
        gum-calls (atom [])
        record (outcome/fresh)]
    (with-redefs [ui/backend (fn [op] (when (= :confirm op) (fn [] (swap! backend-calls inc) true)))
                  command/installed? (constantly true)
                  shell/run-with-output (fn [& args] (swap! gum-calls conj (vec args)) 0)]
      (binding [*err* (java.io.StringWriter.)
                outcome/*record* record]
        (let [approved (with-in-str (or typed "") (interact/confirm! request))]
          {:approved approved
           :backend @backend-calls
           :gum @gum-calls
           :record @record})))))

(def ^:private destructive
  {:action "delete a PersistentVolumeClaim" :target "kube.example.org"
   :items ["moodle/data"] :destructive? true})

(deftest a-destructive-change-approves-only-on-the-typed-word-yes
  (doseq [typed ["yes\n" "  YES \n" "Yes\n"]]
    (testing (pr-str typed)
      (is (true? (:approved (confirm-with destructive typed))))))
  (doseq [typed ["y\n" "no\n" "n\n" "\n" "yess\n" "ye\n" nil]]
    (testing (pr-str typed)
      (let [{:keys [approved record]} (confirm-with destructive typed)]
        (is (false? approved))
        (is (some? (:aborted record)) "a declined confirmation records the run as aborted")))))

(deftest a-destructive-change-never-goes-to-the-backend-or-gum
  (doseq [typed ["yes\n" "n\n" nil]]
    (testing (pr-str typed)
      (let [{:keys [backend gum]} (confirm-with destructive typed)]
        (is (zero? backend) "an approving UI backend is never asked")
        (is (empty? gum) "gum's yes/no picker is never run")))))

(deftest a-non-destructive-change-goes-to-the-backend
  (let [{:keys [approved backend gum]} (confirm-with (dissoc destructive :destructive?) "n\n")]
    (is (true? approved) "the backend's answer decides, not stdin")
    (is (= 1 backend))
    (is (empty? gum))))
