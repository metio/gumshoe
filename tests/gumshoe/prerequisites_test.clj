;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prerequisites-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gumshoe.prerequisites :as prerequisites]))

;; The registry is a process-wide atom; reset it around each test.
(use-fixtures :each (fn [t]
                      (reset! @#'prerequisites/checks {})
                      (t)
                      (reset! @#'prerequisites/checks {})))

(defn- run-item
  "The {:ok? :label} a checklist item's thunk produces."
  [[_label thunk]]
  (thunk))

(deftest check-builds-a-passing-or-failing-item-test
  (testing "the check helper produces a checklist item that reports pass/fail"
    (let [ok (prerequisites/check "window" (constantly true) {:pass "open" :fail "closed"})
          no (prerequisites/check "window" (constantly false) {:pass "open" :fail "closed"})]
      (is (= {:ok? true :label "open"} (run-item ok)))
      (is (= {:ok? false :label "closed"} (run-item no))))))

(deftest items-only-for-declared-checks-test
  (testing "a registered check contributes items only when the book declares its key"
    (prerequisites/register-check!
     :change-window
     (fn [window _opts]
       [(prerequisites/check (str "change window: " window) (constantly true)
                             {:pass (str window " open") :fail (str window " closed")})]))
    (is (contains? (set (prerequisites/registered-checks)) :change-window))
    (testing "a book that declares it gets the item"
      (let [items (prerequisites/items {:change-window "prod"} {})]
        (is (= 1 (count items)))
        (is (= {:ok? true :label "prod open"} (run-item (first items))))))
    (testing "a book that does not declare it gets nothing"
      (is (empty? (prerequisites/items {:installed-tools ["kubectl"]} {}))))))

(deftest multiple-checks-run-in-stable-order-test
  (testing "declared checks contribute in sorted-key order regardless of registration order"
    (prerequisites/register-check! :zeta (fn [_ _] [(prerequisites/check "z" (constantly true) {})]))
    (prerequisites/register-check! :alpha (fn [_ _] [(prerequisites/check "a" (constantly true) {})]))
    (let [labels (map first (prerequisites/items {:zeta 1 :alpha 1} {}))]
      (is (= ["a" "z"] labels)))))

(deftest the-union-of-steps-joins-their-lists-without-duplicates
  (is (= {:installed-tools ["kubectl" "fzf" "git"]
          :cluster-capabilities []
          :kubectl-can-get ["nodes" "pods"]
          :kubectl-can-patch ["nodes"]}
         (prerequisites/union {:installed-tools ["kubectl" "fzf"]
                               :cluster-capabilities []
                               :kubectl-can-get ["nodes"]}
                              nil
                              {:installed-tools ["kubectl" "git"]
                               :kubectl-can-get ["nodes" "pods"]
                               :kubectl-can-patch ["nodes"]}))))

(deftest the-union-keeps-the-highest-version-floor-of-a-tool
  (is (= {:minimum-tool-versions {"kubectl" "1.30.0" "helm" "3.12"}}
         (prerequisites/union {:minimum-tool-versions {"kubectl" "1.28.2" "helm" "3.12"}}
                              {:minimum-tool-versions {"kubectl" "1.30.0"}}))))

(deftest the-union-refuses-values-it-can-not-join
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":change-window"
                        (prerequisites/union {:change-window :weekdays}
                                             {:change-window :weekends})))
  (is (= {:change-window :weekdays}
         (prerequisites/union {:change-window :weekdays} {:change-window :weekdays}))))

(deftest the-summary-names-what-a-book-needs
  (is (= ["tools: kubectl, fzf" "kubectl get: nodes" "kubectl at least 1.30" "cluster capabilities: flux"]
         (prerequisites/summary {:installed-tools ["kubectl" "fzf"]
                                 :kubectl-can-get ["nodes"]
                                 :minimum-tool-versions {"kubectl" "1.30"}
                                 :cluster-capabilities [:flux]}))))

(defn- path-results
  "Every {:ok? :label} the :existing-paths declaration yields for these options."
  [declaration opts]
  (mapv run-item (prerequisites/existing-path-items declaration opts)))

(deftest an-existing-directory-passes-and-names-the-option-test
  (fs/with-temp-dir [dir {}]
    (let [[result] (path-results [:checkout] {:checkout (str dir)})]
      (is (:ok? result))
      (is (str/includes? (:label result) "--checkout"))
      (is (str/includes? (:label result) (str dir))))))

(deftest a-missing-directory-fails-naming-the-option-test
  (fs/with-temp-dir [dir {}]
    (let [missing (str (fs/path dir "no-such-checkout"))
          [[label _] :as items] (prerequisites/existing-path-items [:k8s-deployments] {:k8s-deployments missing})
          [result] (mapv run-item items)]
      (is (str/includes? label "--k8s-deployments") "the pending line names the option")
      (is (false? (:ok? result)))
      (is (= (str "directory --k8s-deployments: " missing " does not exist") (:label result))))))

(deftest a-file-in-place-of-a-directory-fails-test
  (fs/with-temp-dir [dir {}]
    (let [file (str (fs/create-file (fs/path dir "hostname")))
          [result] (path-results [:fahrplan] {:fahrplan file})]
      (is (false? (:ok? result)))
      (is (= (str "directory --fahrplan: " file " is not a directory") (:label result))))))

(deftest an-unwritable-directory-fails-only-when-writable-is-required-test
  (fs/with-temp-dir [dir {}]
    (let [read-only (fs/create-dir (fs/path dir "read-only"))]
      (fs/set-posix-file-permissions read-only "r-xr-xr-x")
      (try
        (let [[readable writable] (path-results [:a {:option :b :writable true}]
                                                {:a (str read-only) :b (str read-only)})]
          (is (:ok? readable) "a plain entry only needs the directory to exist")
          (is (false? (:ok? writable)))
          (is (= (str "directory --b: " read-only " is not writable") (:label writable))))
        (finally (fs/set-posix-file-permissions read-only "rwxr-xr-x"))))))

(deftest a-writable-directory-passes-when-writable-is-required-test
  (fs/with-temp-dir [dir {}]
    (let [[result] (path-results [{:option :checkout :writable true}] {:checkout (str dir)})]
      (is (:ok? result))
      (is (str/includes? (:label result) "writable")))))

(deftest an-option-without-a-value-is-skipped-with-a-note-test
  (doseq [opts [{} {:checkout nil} {:checkout ""} {:checkout "  "}]]
    (let [[result :as results] (path-results [:checkout] opts)]
      (is (= 1 (count results)) "the checklist still shows the declared option")
      (is (:ok? result) "an optional option left unset does not stop the book")
      (is (= "directory --checkout: not set, nothing to check" (:label result))))))

(deftest a-relative-path-resolves-against-the-working-directory-test
  (let [[result] (path-results [:checkout] {:checkout "."})]
    (is (:ok? result))
    (is (str/includes? (:label result) (str (fs/cwd))))))

(deftest a-leading-tilde-names-the-home-directory-test
  (is (= (str (fs/home)) (prerequisites/directory-path "~")))
  (is (= (str (fs/path (fs/home) "checkouts" "fahrplan")) (prerequisites/directory-path "~/checkouts/fahrplan")))
  (is (= (str (fs/path (fs/cwd) "a")) (prerequisites/directory-path "x/../a"))))

(deftest a-malformed-entry-fails-closed-test
  (let [[result] (path-results ["checkout"] {"checkout" "/tmp"})]
    (is (false? (:ok? result)))
    (is (str/includes? (:label result) ":existing-paths"))))

(deftest the-summary-names-the-directories-a-book-needs-test
  (is (= ["existing directories: --k8s-deployments, --fahrplan (writable)"]
         (prerequisites/summary {:existing-paths [:k8s-deployments {:option :fahrplan :writable true}]}))))

(deftest the-union-joins-the-directories-of-several-steps-test
  (is (= {:existing-paths [:a {:option :b :writable true}]}
         (prerequisites/union {:existing-paths [:a]} {:existing-paths [:a {:option :b :writable true}]}))))
