;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.attributed-warnings-test
  "A warning a child command writes shows under the step that ran it. These
   tests run real children - a fake kubectl written as a shell script - through
   the real shell, effect, verify and detective code; only the program name
   \"kubectl\" is pointed at the script."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.render :as render]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

(def ^:private fake-kubectl-script
  "#!/bin/sh
case \"$*\" in
  *label*) echo \"ingress.networking.k8s.io/web labeled\"
           echo \"Warning: deprecated-ingress: label on a deprecated Ingress\" >&2 ;;
  *annotate*) echo \"ingress.networking.k8s.io/web annotated\"
              echo \"Warning: deprecated-ingress: annotate on a deprecated Ingress\" >&2 ;;
  *forbidden*) echo \"Warning: deprecated-ingress: still deprecated\" >&2
               echo \"Error from server (Forbidden): ingresses.networking.k8s.io \\\"web\\\" is forbidden:\" >&2
               echo \"  User \\\"ops\\\" cannot patch resource \\\"ingresses\\\"\" >&2
               exit 1 ;;
  *apply*) cat >/dev/null
           echo \"Warning: deprecated-ingress: apply on a deprecated Ingress\" >&2
           echo \"error: the server rejected the manifest\" >&2
           echo \"  second line of the rejection\" >&2
           exit 1 ;;
  *slow*) echo \"evicting pod default/web-0\" >&2
          sleep 2
          echo \"node/n drained\" ;;
  *kuberc*) echo \"Warning: KUBERC=$KUBERC\" >&2 ;;
  *endpoints*) echo \"Warning: v1 Endpoints is deprecated in v1.33+; use discovery.k8s.io/v1 EndpointSlice\" >&2
               echo '{\"items\":[]}' ;;
  *get*) echo '{\"items\":[]}' ;;
esac
")

(defn- fake-kubectl
  []
  (let [dir (fs/create-temp-dir {:prefix "gumshoe-attributed"})
        path (fs/path dir "kubectl")]
    (spit (str path) fake-kubectl-script)
    (fs/set-posix-file-permissions path "rwx------")
    (str path)))

(defn- to-fake
  "f, a shell function, with the program \"kubectl\" replaced by the fake."
  [f fake]
  (fn [& args]
    (let [[head & more] args]
      (cond
        (= "kubectl" head) (apply f fake more)
        ;; execute-with-stdin takes its input first
        (= "kubectl" (first more)) (apply f head fake (rest more))
        :else (apply f args)))))

(defmacro ^:private with-fake-kubectl
  [& body]
  `(let [fake# (fake-kubectl)]
     (with-redefs [shell/run-attributed (to-fake shell/run-attributed fake#)
                   shell/run-with-output (to-fake shell/run-with-output fake#)
                   shell/execute (to-fake shell/execute fake#)
                   shell/execute-with-stdin (to-fake shell/execute-with-stdin fake#)]
       ~@body)))

(defn- stderr-of
  "{:value <what f returned> :lines <what it wrote to stderr, without colour>}"
  [f]
  (let [err (java.io.StringWriter.)
        value (binding [*err* err] (f))]
    {:value value
     :lines (vec (str/split-lines (render/strip-colors (str err))))}))

(defn- index-of [lines pred]
  (first (keep-indexed (fn [i l] (when (pred l) i)) lines)))

(defn- under?
  "Whether the line matching child follows the line matching parent, with no
   other effect's line between them."
  [lines parent child]
  (let [p (index-of lines #(str/includes? % parent))
        c (index-of lines #(str/includes? % child))]
    (boolean (and p c (< p c)
                  (not-any? #(str/includes? % "▸ kubectl") (subvec lines (inc p) c))))))

;; --- effects ---------------------------------------------------------------------------

(deftest two-effects-show-their-own-warnings-under-their-own-lines
  (with-fake-kubectl
    (let [{:keys [value lines]}
          (stderr-of #(effect/run! [(effect/kubectl "c" "-n" "web" "label" "ingress" "web" "a=b")
                                    (effect/kubectl "c" "-n" "web" "annotate" "ingress" "web" "a=b")]))]
      (is (true? value))
      (is (under? lines "▸ kubectl -n web label ingress web a=b"
                  "🔶 Warning: deprecated-ingress: label on a deprecated Ingress")
          (str/join "\n" lines))
      (is (under? lines "▸ kubectl -n web annotate ingress web a=b"
                  "🔶 Warning: deprecated-ingress: annotate on a deprecated Ingress")
          (str/join "\n" lines))
      (testing "each warning is shown once, two levels in"
        (is (= ["    🔶 Warning: deprecated-ingress: label on a deprecated Ingress"
                "    🔶 Warning: deprecated-ingress: annotate on a deprecated Ingress"]
               (filterv #(str/includes? % "Warning:") lines)))))))

(deftest an-effect-without-warnings-shows-only-its-line
  ;; the positive control for the test above: the same harness, a child that
  ;; writes no warning, and no warning line appears
  (with-fake-kubectl
    (let [{:keys [value lines]} (stderr-of #(effect/run! [(effect/kubectl "c" "get" "pods")]))]
      (is (true? value))
      (is (= ["  ▸ kubectl get pods"] lines)))))

(deftest a-failed-effect-prints-its-full-stderr-and-its-warning
  (with-fake-kubectl
    (let [{:keys [value lines]} (stderr-of #(effect/run! [(effect/kubectl "c" "forbidden" "ingress" "web")
                                                         (effect/kubectl "c" "label" "never")]))
          text (str/join "\n" lines)]
      (is (false? value))
      (is (str/includes? text "Error from server (Forbidden): ingresses.networking.k8s.io \"web\" is forbidden:"))
      (is (str/includes? text "  User \"ops\" cannot patch resource \"ingresses\""))
      (is (under? lines "▸ kubectl forbidden ingress web" "🔶 Warning: deprecated-ingress: still deprecated") text)
      (is (not (str/includes? text "label never")) "the plan stops at the failed effect"))))

(deftest a-failed-stdin-effect-prints-its-full-stderr-once
  (with-fake-kubectl
    (let [{:keys [value lines]} (stderr-of #(effect/run! [(effect/kubectl-stdin "c" "{}" "apply" "--filename" "-")]))
          text (str/join "\n" lines)]
      (is (false? value))
      (is (str/includes? text "error: the server rejected the manifest"))
      (is (str/includes? text "  second line of the rejection"))
      (is (= 1 (count (filter #(str/includes? % "apply on a deprecated Ingress") lines))) text)
      (is (under? lines "▸ kubectl apply --filename - (with stdin)"
                  "🔶 Warning: deprecated-ingress: apply on a deprecated Ingress")
          text))))

(deftest a-long-effect-streams-its-output-while-it-runs
  (with-fake-kubectl
    (let [err (java.io.StringWriter.)
          run (future (binding [*err* err] (effect/run! [(effect/kubectl "c" "slow")])))
          deadline (+ (System/currentTimeMillis) 5000)]
      (while (and (not (str/includes? (str err) "evicting pod default/web-0"))
                  (< (System/currentTimeMillis) deadline))
        (Thread/sleep 20))
      (is (str/includes? (str err) "evicting pod default/web-0"))
      (is (not (realized? run)) "the line arrived while the child was still running")
      (is (true? (deref run 10000 ::timeout))))))

(deftest an-effect-child-runs-with-kuberc-off
  (with-fake-kubectl
    (let [{:keys [lines]} (stderr-of #(effect/run! [(effect/kubectl "c" "kuberc")]))]
      (is (some #(str/includes? % "🔶 Warning: KUBERC=off") lines) (str/join "\n" lines)))))

(deftest an-interactive-effect-keeps-the-terminal
  (let [streamed (atom [])]
    (with-redefs [shell/run-with-output (fn [& args] (swap! streamed conj (vec args)) 0)
                  shell/run-attributed (fn [& _] (throw (ex-info "an interactive effect must not be relayed" {})))]
      (is (true? (:value (stderr-of #(effect/run! [(effect/interactive (effect/kubectl-plugin "c" "netshoot" "debug" "node/n"))])))))
      (is (= [["kubectl" "netshoot" "debug" "node/n" "--context=c"]] @streamed)))))

(deftest a-plain-effect-is-relayed
  ;; the positive control for the test above
  (let [relayed (atom [])]
    (with-redefs [shell/run-with-output (fn [& _] (throw (ex-info "a plain effect must be relayed" {})))
                  shell/run-attributed (fn [& args] (swap! relayed conj (vec args)) 0)]
      (is (true? (:value (stderr-of #(effect/run! [(effect/kubectl-plugin "c" "cnpg" "promote" "db" "db-2")])))))
      (is (= [["kubectl" "cnpg" "promote" "db" "db-2" "--context=c"]] @relayed)))))

(deftest interactive-survives-plan-building
  (let [e (effect/interactive (effect/kubectl "c" "exec" "-it" "p" "--" "sh"))]
    (is (effect/interactive? (first (effect/plan nil e))))
    (is (effect/interactive? (first (effect/as-plan e))))
    (is (not (effect/interactive? (effect/kubectl "c" "exec" "p"))))
    (is (= [:kubectl "c" "exec" "-it" "p" "--" "sh"] e) "the effect's data is unchanged")))

(deftest the-netshoot-sessions-are-interactive
  (doseq [[path ctx] [["runbooks/kubernetes/netshoot/node.clj" {:context "c" :target "n"}]
                      ["runbooks/kubernetes/netshoot/pod.clj" {:context "c" :target "web/p"
                                                              :ref {:namespace "web" :name "p"}
                                                              :pick "web/p"}]]]
    (let [spec (with-redefs [mutation/book identity]
                 (binding [runbook/*loading* true] (load-file path)))
          plan (try ((:effect spec) ctx) (catch Exception _ nil))]
      (is (seq plan) path)
      (is (every? effect/interactive? plan) path))))

;; --- reads ------------------------------------------------------------------------------

(deftest a-post-check-read-shows-its-deprecation-notice-under-the-check
  (with-fake-kubectl
    (let [attempts (atom 0)
          {:keys [value lines]}
          (stderr-of #(verify/eventually {:description "the endpoints are listed"
                                          :timeout 5 :interval 1
                                          :check (fn [] (kubectl/get-json "c" "endpoints")
                                                   (> (swap! attempts inc) 1))}))]
      (is (true? value))
      (is (= 2 @attempts) "the check read twice")
      (is (= 1 (count (filter #(str/includes? % "v1 Endpoints is deprecated") lines)))
          "a warning read on every attempt is shown once")
      (is (under? lines "verified: the endpoints are listed"
                  "🔶 Warning: v1 Endpoints is deprecated in v1.33+; use discovery.k8s.io/v1 EndpointSlice")
          (str/join "\n" lines)))))

(deftest a-post-check-read-without-warnings-shows-only-its-verdict
  (with-fake-kubectl
    (let [{:keys [value lines]}
          (stderr-of #(verify/eventually {:description "the pods are listed" :timeout 1
                                          :check (fn [] (some? (kubectl/get-json "c" "pods")))}))]
      (is (true? value))
      (is (= ["✅ verified: the pods are listed"] lines)))))

(deftest an-evidence-read-shows-its-deprecation-notice-named-by-the-read
  (with-fake-kubectl
    (with-redefs [registry/collector-for (fn [_] (fn [context type] (kubectl/get-json context type)))
                  kubectl/current-cluster (constantly "c")]
      (let [{:keys [value lines]}
            (stderr-of #(detective/collect-evidence! "c" [{:name "a" :requires ["endpoints" "pods"]}]))]
        (is (= {:items []} (get value "endpoints")))
        (is (some #{"  🔶 endpoints: Warning: v1 Endpoints is deprecated in v1.33+; use discovery.k8s.io/v1 EndpointSlice"} lines)
            (str/join "\n" lines))
        (is (not-any? #(str/includes? % "pods: Warning") lines))))))

(deftest read-warning-lines-name-each-read
  (is (= ["  [WARN] endpoints: Warning: a" "  [WARN] endpoints: Warning: b" "  [WARN] events: Warning: c"]
         (detective/read-warning-lines (render/context :ascii false)
                                       {"events" ["Warning: c"] "endpoints" ["Warning: a" "Warning: b"]}))))

(deftest warning-lines-nest-two-levels-under-a-step
  (is (= ["    [WARN] Warning: a"] (render/warning-lines (render/context :ascii false) ["Warning: a"]))))

;; --- the collection -----------------------------------------------------------------------

(deftest a-captured-read-outside-a-collection-keeps-its-stderr-and-prints-nothing
  (with-fake-kubectl
    (let [{:keys [value lines]} (stderr-of #(shell/execute "kubectl" "get" "endpoints"))]
      (is (str/includes? (:err value) "v1 Endpoints is deprecated"))
      (is (= [""] lines)))))

(deftest a-nested-collection-keeps-its-warnings-to-itself
  (with-fake-kubectl
    (let [outer (shell/collecting-warnings
                 (fn []
                   (shell/collecting-warnings #(shell/execute "kubectl" "get" "endpoints"))))]
      (is (= [] (:warnings outer)))
      (is (= ["Warning: v1 Endpoints is deprecated in v1.33+; use discovery.k8s.io/v1 EndpointSlice"]
             (:warnings (:value outer)))))))

(deftest a-concurrent-read-is-collected-for-the-step-that-started-it
  (with-fake-kubectl
    (let [{:keys [warnings]} (shell/collecting-warnings
                              #(deref (future (shell/execute "kubectl" "get" "endpoints"))))]
      (is (= 1 (count warnings))))))

(deftest stdout-child-warnings-prints-nothing-for-none
  (is (= [""] (:lines (stderr-of #(stdout/child-warnings []))))))
