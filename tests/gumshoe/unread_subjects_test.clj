;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.unread-subjects-test
  "A read that did not answer is reported as unread, never as an absent
   object or an empty selection: the drill-down's subject and etcd's keys."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.etcd :as etcd]
            [gumshoe.investigation :as investigation]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.subject :as subject]))

(def ^:private refused
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 192.0.2.1:6443: i/o timeout"})

(defn- captured
  "{:result .. :out <stdout and stderr, colours stripped>} of f."
  [f]
  (let [out (java.io.StringWriter.)
        result (binding [*out* out *err* out] (f))]
    {:result result :out (stdout/strip-colors (str out))}))

;; --- the drill-down -----------------------------------------------------------------

(defn- investigate
  "investigate! from Node n1 against kubectl answering through execute,
   keeping the trail off the disk."
  [execute]
  (with-redefs [shell/execute execute
                investigation/save-trail! (fn [& _] nil)]
    (captured #(investigation/investigate! "ctx" (subject/subject "Node" nil "n1")))))

(deftest a-subject-whose-read-did-not-answer-is-unread-and-fails
  (let [{:keys [result out]} (investigate (fn [& _] refused))]
    (is (false? result))
    (is (str/includes? out "could not read Node n1"))
    (is (not (str/includes? out "may have just been deleted")))))

(deftest a-subject-the-cluster-says-is-gone-is-reported-deleted
  (let [{:keys [result out]}
        (investigate (fn [& argv]
                       (if (some #{"--ignore-not-found"} argv)
                         {:exit 0 :out "" :err ""}
                         {:exit 1 :out "" :err "Error from server (NotFound): nodes \"n1\" not found"})))]
    (is (true? result))
    (is (str/includes? out "Node n1 not found - it may have just been deleted"))))

;; --- etcd get_value -----------------------------------------------------------------

(defn- get-value
  "Runs etcd get_value's action with opts against etcdctl answering through
   execute; {:result .. :out .. :commands [<argv> ..] :streamed [<argv> ..]}."
  [opts execute]
  (let [config (binding [runbook/*loading* true] (load-file "runbooks/etcd/get_value.clj"))
        commands (atom [])
        streamed (atom [])]
    (with-redefs [shell/execute (fn [& argv] (swap! commands conj (vec argv)) (apply execute argv))
                  shell/run-with-output (fn [& argv] (swap! streamed conj (vec argv)) 0)
                  kubectl/current-context (constantly "ctx")
                  select/pick (fn [& _] "kube-system/etcd-a")]
      (merge (captured #((:action config) opts {}))
             {:commands @commands :streamed @streamed}))))

(defn- listing? [argv] (some #{"--from-key"} argv))

(deftest a-named-key-is-read-on-its-own
  (let [{:keys [result commands streamed]}
        (get-value {:key "/registry/pods/a"}
                   (fn [& _] {:exit 0 :out "/registry/pods/a\n" :err ""}))]
    (is (true? result))
    (is (not-any? listing? commands) "the keyspace is never listed for a named key")
    (is (= "/registry/pods/a" (nth (last streamed) (- (count (last streamed)) 2))))))

(deftest a-named-key-etcd-does-not-hold-is-named
  (let [{:keys [result out streamed]} (get-value {:key "/nope"} (fn [& _] {:exit 0 :out "" :err ""}))]
    (is (false? result))
    (is (str/includes? out "etcd holds no key /nope"))
    (is (empty? streamed))))

(deftest a-listing-that-did-not-answer-is-reported-as-the-failed-read
  (let [{:keys [result out]} (get-value {} (fn [& _] {:exit 1 :out "" :err "error: unable to upgrade connection: Forbidden"}))]
    (is (false? result))
    (is (str/includes? out "could not list the etcd keys - error: unable to upgrade connection: Forbidden"))
    (is (not (str/includes? out "no key selected")))))

(deftest a-key-read-that-did-not-answer-names-the-key
  (let [{:keys [result out]} (get-value {:key "/registry/x"} (fn [& _] refused))]
    (is (false? result))
    (is (str/includes? out "could not read the etcd key /registry/x"))))

(deftest etcdctl-lines-tells-a-failed-read-from-an-empty-one
  (with-redefs [shell/execute (fn [& _] {:exit 0 :out "a\n\nb\n" :err ""})]
    (is (= {:lines ["a" "b"]} (etcd/etcdctl-lines "ctx" "ns" "pod" "get" "a"))))
  (with-redefs [shell/execute (fn [& _] {:exit 2 :out "" :err ""})]
    (is (= {:unread "etcdctl exited with 2"} (etcd/etcdctl-lines "ctx" "ns" "pod" "get" "a")))))
