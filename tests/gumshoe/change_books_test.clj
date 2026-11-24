;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.change-books-test
  "Characterises the change books whose effect is not a kubectl call against
   the cluster the kube context names, or which do not announce: the ceph OSD
   book that acts over SSH and the netshoot debug books. Each book runs its
   whole :action against a recorded ceph cluster and a recorded kube cluster,
   with gumshoe.shell/execute answering for ssh and kubectl, the fuzzy finder
   answering as an operator who picks nothing, and flow/change! captured. A
   transcript keeps the result, what was printed, the confirmation, the
   plan, the announcement, what the precondition decides before the effect,
   and the post-checks judged after the effect and against a host that
   stopped answering. The transcripts are kept in
   tests/golden/change_books.edn.

   A book's :failing scenarios run for real instead, with flow/change! and
   the effect interpreter in place, every effect applying and the state not
   moving, so a post-check does not hold: they keep what the run printed,
   its banner and the recovery lines the harness prints under it.

   Regenerate the file after an intended change with
   (gumshoe.change-books-test/write-golden!) and review its diff."
  (:require [babashka.cli :as cli]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [gumshoe.announce :as announce]
            [gumshoe.flow :as flow]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

(def golden-path "tests/golden/change_books.edn")

;; --- the recorded ceph cluster and kube cluster -------------------------------------

(def ^:private unanswered
  {:exit 255 :out "" :err "ssh: connect to host mgr-1 port 22: Connection timed out"})

(defn- remote-command
  "The command an ssh argv runs on the far end."
  [argv]
  (vec (rest (drop-while #(not= "--" %) argv))))

(defn- answer-ssh
  [{:keys [osds answering?]} argv]
  (let [[_host & command] (remote-command argv)
        command (vec (remove #{"sudo" "cephadm" "shell" "--"} command))]
    (cond
      (not answering?) unanswered
      (#{["exit" "0"] ["-n" "true"]} command) {:exit 0 :out "" :err ""}
      (= ["ceph" "osd" "dump" "--format" "json"] command) {:exit 0 :out (json/generate-string {:osds osds}) :err ""}
      :else {:exit 0 :out "" :err ""})))

(defn- answer-kubectl
  [{:keys [objects answering?]} argv]
  (let [[_ type] (drop-while #(not= "get" %) argv)]
    (if answering?
      {:exit 0 :out (json/generate-string {:items (get objects type [])}) :err ""}
      {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"})))

(defn- answer
  [state argv]
  (case (first argv)
    "ssh" (answer-ssh state argv)
    "kubectl" (answer-kubectl state argv)
    {:exit 0 :out "" :err ""}))

;; --- running a book ------------------------------------------------------------------

(defn- load-config
  "The config a book file hands to runbook/execute!, directly or through
   mutation/book."
  [path]
  (binding [runbook/*loading* true]
    (load-file path)))

(defn- capture
  [thunk]
  (let [out (java.io.StringWriter.)
        result (binding [*out* out *err* out] (thunk))]
    {:result result :out (str/trim (str out))}))

(defn- attempt
  [f]
  (try (f) (catch Exception e (str "threw: " (ex-message e)))))

(defn- printable
  "x with every function replaced by :fn, so a transcript prints the same on
   every run."
  [x]
  (walk/postwalk #(if (fn? %) :fn %) x))

(defn transcript
  "What the book does in one scenario: state is {:osds .. :objects ..
   :answering?}, between is merged into it after the confirmation, after is
   the state the effect leaves, and the book runs with opts, or with what
   its options parse argv into."
  [config {:keys [state between after opts argv]}]
  (let [opts (if argv (cli/parse-opts argv {:spec (:options config)}) opts)
        state (atom (merge {:answering? true} state))
        seen (atom nil)]
    (binding [ssh/*user-config* nil]
      (with-redefs [shell/execute (fn [& argv] (answer @state (vec argv)))
                    shell/exit-code-of (fn [& argv] (:exit (answer @state (vec argv))))
                    kubectl/current-context (constantly "ctx")
                    kubectl/current-cluster (constantly "cluster")
                    fuzzy/select-single (constantly nil)
                    announce/announce! (fn [system _ message] [system message])
                    flow/change! (fn [config] (reset! seen config) true)]
        (let [{:keys [result out]} (capture #(attempt (fn [] ((:action config) opts {:announcement-data nil}))))
              {:keys [confirmation precondition announce! effect post-checks]} @seen
              _ (swap! state merge between)
              decided (when precondition (capture precondition))
              judged (fn [answering?]
                       (swap! state merge after {:answering? answering?})
                       (vec (for [{:keys [description check]} post-checks]
                              [description (attempt check)])))]
          (cond-> {:result result :out out}
            @seen (assoc :change (cond-> {:action (:action confirmation)
                                          :target (:target confirmation)
                                          :destructive? (boolean (:destructive? confirmation))
                                          :items (vec (:items confirmation))
                                          :plan (vec effect)}
                                   decided (assoc :precondition decided)
                                   announce! (assoc :announce (announce!))
                                   post-checks (assoc :checks {:after (judged true)
                                                               :unanswered (judged false)})))))))))

(defn failed-transcript
  "What the book prints when it runs for real against state with every
   effect applying and the state staying as it is: what the run printed, its
   banner and the recovery lines under it. Post-checks run once, with a
   timeout of 0."
  [config {:keys [state opts]}]
  (let [state (merge {:answering? true} state)
        record (outcome/fresh)
        eventually verify/eventually]
    (binding [ssh/*user-config* nil]
      (with-redefs [shell/execute (fn [& argv] (answer state (vec argv)))
                    shell/exit-code-of (fn [& argv] (:exit (answer state (vec argv))))
                    shell/run-with-output (constantly 0)
                    kubectl/current-context (constantly "ctx")
                    kubectl/current-cluster (constantly "cluster")
                    interact/confirm! (constantly true)
                    announce/announce! (fn [_ _ _] nil)
                    verify/eventually (fn [check] (eventually (assoc check :timeout 0)))]
        (let [{:keys [result out]} (capture #(binding [outcome/*record* record]
                                               (attempt (fn [] ((:action config) opts {:announcement-data nil})))))
              {:keys [tone text]} (outcome/banner {:changes? true :outcome (if (true? result) :ok :failed)} @record)]
          {:result result
           :out (str/split-lines (stdout/strip-colors out))
           :banner text
           :recovery (vec (outcome/recovery-lines tone @record))})))))

(defn surface
  "What the book declares outside a run, and the opts its options parse argv
   into."
  [config argv]
  (printable
   {:description (:description config)
    :options (:options config)
    :prerequisites (:prerequisites config)
    :announce? (get config :announce? true)
    :details (vec (:details config))
    :parsed (when argv (attempt #(cli/parse-opts argv {:spec (:options config)})))}))

;; --- the books and their scenarios ---------------------------------------------------

(defn- osd [id in] {:osd id :in (if in 1 0) :up 1})

(def ^:private out-osd [(osd 1 true) (osd 3 false)])

(defn- node [name] {:metadata {:name name}})
(defn- pod [namespace name] {:metadata {:namespace namespace :name name}})

(def books
  {"tools/ceph/runbooks/osd_in.clj"
   {:argv ["--host" "mgr-1" "--osd" "3"]
    :scenarios
    {:permitted {:state {:osds out-osd} :after {:osds [(osd 1 true) (osd 3 true)]}
                 :opts {:host "mgr-1" :osd "osd.3"}}
     :cephadm {:state {:osds out-osd} :after {:osds [(osd 1 true) (osd 3 true)]}
               :opts {:host "mgr-1" :user "admin" :cephadm-shell true :osd "osd.3"}}
     :numbered {:state {:osds out-osd} :after {:osds [(osd 1 true) (osd 3 true)]}
                :argv ["--host" "mgr-1" "--osd" "3"]}
     :still-out {:state {:osds out-osd} :after {:osds out-osd}
                 :opts {:host "mgr-1" :osd "osd.3"}}
     :brought-in-meanwhile {:state {:osds out-osd} :between {:osds [(osd 1 true) (osd 3 true)]}
                            :opts {:host "mgr-1" :osd "osd.3"}}
     :unanswered-meanwhile {:state {:osds out-osd} :between {:answering? false}
                            :opts {:host "mgr-1" :osd "osd.3"}}
     :all-in {:state {:osds [(osd 1 true) (osd 3 true)]} :opts {:host "mgr-1" :osd "osd.3"}}
     :unpicked {:state {:osds out-osd} :opts {:host "mgr-1" :osd "osd.9"}}
     :unreachable {:state {:osds out-osd :answering? false} :opts {:host "mgr-1" :osd "osd.3"}}}
    :failing
    {:failed-after-applying {:state {:osds out-osd} :opts {:host "mgr-1" :osd "osd.3"}}}}

   "runbooks/kubernetes/netshoot/node.clj"
   {:scenarios
    {:permitted {:state {:objects {"nodes" [(node "n1") (node "n2")]}} :opts {:node "n2"}}
     :unpicked {:state {:objects {"nodes" [(node "n1") (node "n2")]}} :opts {:node "n9"}}
     :unanswered {:state {:objects {"nodes" [(node "n1")]} :answering? false} :opts {:node "n1"}}}}

   "runbooks/kubernetes/netshoot/pod.clj"
   {:scenarios
    {:permitted {:state {:objects {"pods" [(pod "shop" "web-1") (pod "db" "pg-1")]}}
                 :opts {:namespace "shop" :pod "web-1"}}
     :unpicked {:state {:objects {"pods" [(pod "shop" "web-1")]}} :opts {:namespace "shop" :pod "web-9"}}
     :no-pods {:state {:objects {"pods" []}} :opts {:namespace "shop" :pod "web-1"}}
     :unanswered {:state {:objects {"pods" [(pod "shop" "web-1")]} :answering? false}
                  :opts {:namespace "shop" :pod "web-1"}}}}})

(defn transcripts
  []
  (into (sorted-map)
        (for [[path {:keys [argv scenarios failing]}] books]
          (let [config (load-config path)]
            [path (assoc (into (sorted-map) (concat (for [[k scenario] scenarios] [k (transcript config scenario)])
                                                    (for [[k scenario] failing] [k (failed-transcript config scenario)])))
                         :surface (surface config argv))]))))

(defn write-golden!
  []
  (spit golden-path
        ;; split, so a REUSE scan does not read these strings as this file's header
        (str ";; SPDX" "-FileCopyrightText: The gumshoe Authors\n"
             ";; SPDX" "-License-Identifier: 0BSD\n"
             ";; Generated by (gumshoe.change-books-test/write-golden!); review its diff.\n"
             (with-out-str (pprint/pprint (transcripts))))))

(deftest every-book-keeps-its-transcripts
  (let [golden (edn/read-string (slurp golden-path))
        actual (transcripts)]
    (is (= (set (keys golden)) (set (keys actual))) "the same books")
    (doseq [[path scenarios] actual
            [scenario transcript] scenarios]
      (testing (str path " " scenario)
        (is (= (get-in golden [path scenario]) transcript))))))
