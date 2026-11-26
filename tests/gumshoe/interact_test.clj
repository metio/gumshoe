;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.interact-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.command :as command]
            [gumshoe.config :as config]
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

;; --- the destructive picker, an estate opt-in -------------------------------

(defn- ask-with
  "Runs confirm! on `request` with everything around it faked: env.edn's
   :confirmations setting (`setting`), the active UI backend (`backend`, a
   function of whatever arguments it receives, plus `options?` for its
   :confirm-options? declaration), whether gum is installed (`gum?`), whether
   the operator is at a terminal (`attended?`) and what is typed on stdin.
   Returns the verdict, every call that reached the backend and gum, what was
   printed to stderr, the outcome record and the selections recorded."
  [{:keys [request setting backend options? gum? attended? typed gum-exit config-fn]
    :or {gum? true attended? true gum-exit 0}}]
  (let [backend-calls (atom [])
        gum-calls (atom [])
        record (outcome/fresh)
        err (java.io.StringWriter.)
        before (count (interact/recorded-selections))
        value (or config-fn
                  (fn ([path] (when (= path interact/destructive-setting) setting))
                    ([path default] (if (= path interact/destructive-setting)
                                      (if (nil? setting) default setting)
                                      default))))]
    (with-redefs [config/value value
                  ui/backend (fn [op]
                               (case op
                                 :confirm (when backend
                                            (fn [& args] (swap! backend-calls conj (vec args)) (apply backend args)))
                                 :confirm-options? (when backend options?)
                                 nil))
                  command/installed? (fn [tool] (and gum? (= "gum" tool)))
                  shell/run-with-output (fn [& args] (swap! gum-calls conj (vec args)) gum-exit)
                  interact/attended? (constantly attended?)]
      (binding [*err* err
                outcome/*record* record]
        (let [approved (with-in-str (or typed "") (interact/confirm! request))]
          {:approved approved
           :backend @backend-calls
           :gum @gum-calls
           :err (str err)
           :record @record
           :selections (vec (drop before (interact/recorded-selections)))})))))

(def ^:private fdb-delete
  {:action "delete 34 fdb rows" :target "leaf-1"
   :items ["aa:bb:cc:dd:ee:01" "aa:bb:cc:dd:ee:02"] :destructive? true})

(def ^:private approving-new-backend (fn [_opts] true))
(def ^:private approving-legacy-backend (fn [] true))

(deftest destructive-mode-is-picker-only-for-the-exact-keyword
  (is (= :picker (interact/destructive-mode :picker)))
  (doseq [setting [nil :typed "picker" 'picker true :Picker :pickers {:picker true} [:picker]]]
    (testing (pr-str setting)
      (is (= :typed (interact/destructive-mode setting))))))

(deftest the-opt-in-lives-under-top-level-confirmations
  (is (= [:confirmations :destructive] interact/destructive-setting)))

(deftest the-consequence-label-names-the-action
  (is (= "Yes, delete 34 fdb rows (2 items)" (interact/consequence-label fdb-delete)))
  (is (= "Yes, delete a PersistentVolumeClaim"
         (interact/consequence-label {:action "delete a PersistentVolumeClaim" :items ["moodle/data"]})))
  (is (= "Yes, delete a PersistentVolumeClaim"
         (interact/consequence-label {:action "delete a PersistentVolumeClaim"}))))

(deftest a-request-without-an-action-has-no-consequence-label
  (doseq [action [nil "" "   " "\u001b[2J" "\n\t"]]
    (testing (pr-str action)
      (is (nil? (interact/consequence-label {:action action :items ["x"]}))))))

(deftest the-consequence-label-strips-control-characters
  (testing "terminal escapes are removed"
    (is (= "Yes, delete everything" (interact/consequence-label {:action "delete \u001b[31meverything\u001b[0m"})))
    (is (= "Yes, delete rows" (interact/consequence-label {:action "delete \u001b]0;pwned\u0007rows"}))))
  (testing "line breaks, tabs, NUL, DEL and bidi overrides become single spaces"
    (is (= "Yes, delete a b c d e f"
           (interact/consequence-label {:action "delete a\nb\r\nc\td\u0000e\u007ff"})))
    (is (= "Yes, delete gnp. exe" (interact/consequence-label {:action "delete gnp.‮exe"}))
        "a right-to-left override is not kept to reorder the label")
    (is (= "Yes, delete x y" (interact/consequence-label {:action "delete x y​"})))))

(deftest a-long-action-is-cut-short-in-the-label
  (let [label (interact/consequence-label {:action (apply str "delete " (repeat 200 "x")) :items ["a" "b" "c"]})]
    (is (str/starts-with? label "Yes, delete xxx"))
    (is (str/ends-with? label "… (3 items)"))
    (is (<= (count label) (+ (count "Yes, ") 72 (count " (3 items)"))))))

(deftest printable-leaves-ordinary-text-alone
  (is (= "delete the fdb rows naming a VTEP - frames stop" (interact/printable "delete the fdb rows naming a VTEP - frames stop")))
  (is (= "Löschen: ümlaut ✓" (interact/printable "Löschen: ümlaut ✓"))))

(deftest by-default-a-destructive-action-asks-for-the-typed-word-whatever-the-backend
  (doseq [[setting what] [[nil "unset"] [:typed ":typed"] ["picker" "a string"] [true "true"]]
          [backend options? label] [[nil nil "gum only"]
                                    [approving-new-backend true "a backend honouring options"]
                                    [approving-legacy-backend nil "a legacy backend"]]]
    (testing (str what " with " label)
      (let [declined (ask-with {:request fdb-delete :setting setting :backend backend :options? options? :typed "n\n"})
            approved (ask-with {:request fdb-delete :setting setting :backend backend :options? options? :typed "yes\n"})]
        (is (false? (:approved declined)) "an approving backend or gum does not answer")
        (is (true? (:approved approved)) "the typed word does")
        (is (empty? (:backend declined)))
        (is (empty? (:gum declined)))
        (is (str/includes? (:err declined) "Type 'yes' to proceed"))))))

(deftest an-opted-in-destructive-action-asks-gum-with-no-selected-and-the-consequence
  (let [{:keys [approved gum backend err]} (ask-with {:request fdb-delete :setting :picker :typed "n\n"})]
    (is (true? approved) "gum's answer decides, not stdin")
    (is (empty? backend))
    (is (= [["gum" "confirm" "--default=false" "--timeout=0s"
             "--affirmative=Yes, delete 34 fdb rows (2 items)" "--negative=No" "Proceed?"]]
           gum))
    (is (not (str/includes? err "Type 'yes' to proceed")))))

(deftest an-opted-in-destructive-action-is-declined-when-gum-says-no
  (let [{:keys [approved record gum]} (ask-with {:request fdb-delete :setting :picker :gum-exit 1 :typed "yes\n"})]
    (is (false? approved) "a typed yes on stdin does not override gum's No")
    (is (= 1 (count gum)))
    (is (some? (:aborted record)))))

(deftest an-opted-in-destructive-action-goes-to-a-backend-honouring-the-options
  (let [{:keys [approved backend gum]} (ask-with {:request fdb-delete :setting :picker
                                                   :backend approving-new-backend :options? true
                                                   :typed "n\n"})]
    (is (true? approved))
    (is (empty? gum) "a backend overriding :confirm replaces gum")
    (is (= [[{:prompt "Proceed?" :affirmative "Yes, delete 34 fdb rows (2 items)" :negative "No"
              :default false :destructive? true}]]
           backend))))

(deftest a-backend-must-answer-true-to-approve
  (doseq [answer [nil "yes" 1 :yes]]
    (testing (pr-str answer)
      (is (false? (:approved (ask-with {:request fdb-delete :setting :picker
                                        :backend (fn [_] answer) :options? true})))))))

(deftest an-opted-in-destructive-action-with-a-legacy-backend-asks-for-the-typed-word
  (doseq [options? [nil false "true" 1]]
    (testing (pr-str options?)
      (let [declined (ask-with {:request fdb-delete :setting :picker :backend approving-legacy-backend
                                :options? options? :typed "n\n"})]
        (is (false? (:approved declined)))
        (is (empty? (:backend declined)) "the legacy backend may ignore a default of No, so it is never asked")
        (is (empty? (:gum declined)) "gum is not asked behind the operator's chosen backend")
        (is (str/includes? (:err declined) "Type 'yes' to proceed"))
        (is (true? (:approved (ask-with {:request fdb-delete :setting :picker :backend approving-legacy-backend
                                         :options? options? :typed "yes\n"}))))))))

(deftest an-opted-in-destructive-action-with-no-picker-asks-for-the-typed-word
  (let [declined (ask-with {:request fdb-delete :setting :picker :gum? false :typed "y\n"})]
    (is (false? (:approved declined)))
    (is (empty? (:gum declined)))
    (is (str/includes? (:err declined) "Type 'yes' to proceed")))
  (is (true? (:approved (ask-with {:request fdb-delete :setting :picker :gum? false :typed "yes\n"})))))

(deftest an-opted-in-destructive-action-away-from-a-terminal-asks-for-the-typed-word
  (doseq [[backend options?] [[nil nil] [approving-new-backend true]]]
    (testing (if backend "with a backend" "with gum")
      (let [declined (ask-with {:request fdb-delete :setting :picker :backend backend :options? options?
                                :attended? false :typed "n\n"})]
        (is (false? (:approved declined)))
        (is (empty? (:gum declined)))
        (is (empty? (:backend declined)))))))

(deftest an-opted-in-destructive-action-without-an-action-asks-for-the-typed-word
  (let [declined (ask-with {:request (assoc fdb-delete :action "\u001b[0m ") :setting :picker :typed "n\n"})]
    (is (false? (:approved declined)))
    (is (empty? (:gum declined)) "a picker with nothing to name on its affirmative option is not offered")))

(deftest non-destructive-confirmations-are-unchanged-by-the-opt-in
  (doseq [setting [nil :picker]]
    (testing (pr-str setting)
      (let [change (dissoc fdb-delete :destructive?)]
        (testing "a legacy backend is called with no arguments"
          (let [{:keys [approved backend gum]} (ask-with {:request change :setting setting
                                                          :backend approving-legacy-backend :typed "n\n"})]
            (is (true? approved))
            (is (= [[]] backend))
            (is (empty? gum))))
        (testing "gum is asked exactly as before"
          (let [{:keys [approved gum]} (ask-with {:request change :setting setting :typed "n\n"})]
            (is (true? approved))
            (is (= [["gum" "confirm" "--default=false" "Proceed?"]] gum))))
        (testing "without a picker the typed word decides"
          (is (false? (:approved (ask-with {:request change :setting setting :gum? false :typed "n\n"}))))
          (is (true? (:approved (ask-with {:request change :setting setting :gum? false :typed "yes\n"})))))))))

(deftest a-non-destructive-change-gives-an-options-backend-a-plain-yes
  (let [{:keys [backend]} (ask-with {:request (dissoc fdb-delete :destructive?) :setting :picker
                                     :backend approving-new-backend :options? true})]
    (is (= [[{:prompt "Proceed?" :affirmative "Yes" :negative "No" :default false :destructive? false}]]
           backend))))

(deftest every-destructive-path-prints-the-destructive-banner
  (doseq [[label opts] [["default, typed" {}]
                        ["opted in, gum" {:setting :picker}]
                        ["opted in, options backend" {:setting :picker :backend approving-new-backend :options? true}]
                        ["opted in, legacy backend" {:setting :picker :backend approving-legacy-backend}]
                        ["opted in, no picker" {:setting :picker :gum? false}]
                        ["opted in, no terminal" {:setting :picker :attended? false}]
                        ["opted in, no action" {:setting :picker :request (assoc fdb-delete :action "")}]]]
    (testing label
      (let [{:keys [err]} (ask-with (merge {:request fdb-delete :typed "n\n"} opts))]
        (is (str/includes? err "DESTRUCTIVE ACTION - this cannot be undone automatically"))))))

(deftest a-book-cannot-enable-the-picker-from-its-request
  (doseq [extra [{:confirmations {:destructive :picker}}
                 {:destructive :picker}
                 {:picker? true}
                 {:confirm-mode :picker}
                 {:confirm {:confirmations {:destructive :picker}}}]]
    (testing (pr-str extra)
      (let [{:keys [approved gum backend]} (ask-with {:request (merge fdb-delete extra)
                                                      :backend approving-new-backend :options? true
                                                      :typed "n\n"})]
        (is (false? approved))
        (is (empty? gum))
        (is (empty? backend))))))

(deftest the-opt-in-is-read-from-the-top-level-only
  (testing "an environment or :defaults setting it does not enable the picker"
    (let [config {:environments {:staging {:confirmations {:destructive :picker}}}
                  :defaults {:confirmations {:destructive :picker}}}
          {:keys [approved gum]} (ask-with {:request fdb-delete :typed "n\n"
                                            :config-fn (fn ([path] (get-in config path))
                                                         ([path default] (get-in config path default)))})]
      (is (false? approved))
      (is (empty? gum)))))

(deftest the-recorded-outcome-is-unchanged
  (doseq [[label opts verdict] [["typed yes" {:typed "yes\n"} "approved"]
                                ["typed no" {:typed "n\n"} "aborted"]
                                ["gum yes" {:setting :picker} "approved"]
                                ["gum no" {:setting :picker :gum-exit 1} "aborted"]
                                ["backend yes" {:setting :picker :backend approving-new-backend :options? true} "approved"]
                                ["backend no" {:setting :picker :backend (fn [_] false) :options? true} "aborted"]]]
    (testing label
      (is (= [{:label "confirm: delete 34 fdb rows" :value verdict}]
             (:selections (ask-with (merge {:request fdb-delete} opts))))))))

(deftest only-a-terminal-device-counts-as-attended
  (doseq [target ["/dev/pts/3" "/dev/tty1" "/dev/ttys004" "/dev/console"]]
    (is (true? (interact/terminal-device? target)) target))
  (doseq [target ["/dev/null" "/dev/zero" "pipe:[123456]" "socket:[42]" "/home/op/answers.txt" "" nil]]
    (is (false? (interact/terminal-device? target)) (pr-str target))))
