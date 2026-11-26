;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.child-output-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.child-output :as child-output]
            [gumshoe.shell :as shell]))

;; Lines in the shapes kubectl writes them: client-go's warning writer prints
;; "Warning: %s\n" (the label coloured yellow when stderr is a terminal), klog
;; "Lmmdd hh:mm:ss.uuuuuu <pid padded to 7> file:line] msg".
(def endpoints-deprecation
  "Warning: v1 Endpoints is deprecated in v1.33+; use discovery.k8s.io/v1 EndpointSlice")

(def admission-warning
  "Warning: Validation failed for ValidatingAdmissionPolicy 'deprecated-ingress' with binding 'deprecated-ingress': ingress-nginx annotations are deprecated")

(def drain-warning
  "Warning: ignoring DaemonSet-managed Pods: kube-system/calico-node-x2x7l, kube-system/kube-proxy-9fz8d")

(def klog-warning
  "W1126 10:15:30.123456   12345 warnings.go:70] metadata.finalizers: \"example.com/x\": prefer a domain-qualified finalizer name")

(def klog-error
  "E1126 10:15:30.123456   12345 memcache.go:265] couldn't get current server API group list: the server is currently unable to handle the request")

(deftest warning-lines-in-every-shape-kubectl-writes-them-are-warnings
  (doseq [line [endpoints-deprecation admission-warning drain-warning klog-warning
                "W0101 00:00:00.000000 1234567 reflector.go:561] k8s.io/client-go: failed to list"
                (str "\u001b[33;1mWarning:\u001b[0m " "v1 Endpoints is deprecated in v1.33+")
                (str endpoints-deprecation "\r")]]
    (is (child-output/warning-line? line) line)))

(deftest other-lines-are-not-warnings
  (doseq [line [klog-error
                "Error from server (Forbidden): ingresses.networking.k8s.io \"x\" is forbidden"
                "ingress.networking.k8s.io/x labeled"
                "Waiting for deployment \"web\" rollout to finish: 1 of 3 updated replicas are available..."
                "W1234 is not a klog line"
                "  Warning: indented, so not printed by kubectl's warning writer"
                "warning: lower case"
                ""]]
    (is (not (child-output/warning-line? line)) line)))

(deftest warning-text-drops-colour-and-line-ending-and-keeps-the-words
  (is (= "Warning: v1 Endpoints is deprecated in v1.33+"
         (child-output/warning-text "\u001b[33;1mWarning:\u001b[0m v1 Endpoints is deprecated in v1.33+\r\n"))))

(deftest split-stderr-separates-warnings-from-everything-else
  (let [text (str/join "\n" [endpoints-deprecation
                             "Error from server (NotFound): pods \"x\" not found"
                             ""
                             klog-warning
                             klog-error
                             endpoints-deprecation])]
    (is (= {:warnings [endpoints-deprecation klog-warning]
            :other ["Error from server (NotFound): pods \"x\" not found" klog-error]}
           (child-output/split-stderr text)))))

(deftest split-stderr-of-nothing-is-empty
  (is (= {:warnings [] :other []} (child-output/split-stderr nil)))
  (is (= {:warnings [] :other []} (child-output/split-stderr ""))))

(deftest line-start-decides-as-soon-as-the-beginning-allows
  (testing "a beginning that can still become a warning waits"
    (doseq [p ["" "W" "Warn" "Warning" "W1" "W1126" "\u001b" "\u001b[33;1" "\u001b[33;1mWarn"]]
      (is (= :undecided (child-output/line-start p)) (pr-str p))))
  (testing "a warning is known from its label"
    (doseq [p ["Warning:" "Warning: v1" "W1126 " "\u001b[33;1mWarning:"]]
      (is (= :warning (child-output/line-start p)) (pr-str p))))
  (testing "anything else is released at once"
    (doseq [p ["P" "Password: " "Wa1" "Wait" "W11a" "E1126 " " " "ingress"]]
      (is (= :other (child-output/line-start p)) (pr-str p)))))

;; --- relay-stderr ---------------------------------------------------------------------

(defn- relayed
  [text hold?]
  (let [out (java.io.StringWriter.)
        warnings (shell/relay-stderr (java.io.StringReader. text) out hold?)]
    {:written (str out) :warnings warnings}))

(deftest relay-holds-back-warnings-and-writes-everything-else
  (is (= {:written "pod/x labeled\nError from server (Forbidden): no\n"
          :warnings [admission-warning klog-warning]}
         (relayed (str "pod/x labeled\n" admission-warning "\n"
                       "Error from server (Forbidden): no\n" klog-warning "\n")
                  true))))

(deftest relay-without-a-collection-holds-nothing-back
  (let [text (str admission-warning "\nError: no\n")]
    (is (= {:written text :warnings []} (relayed text false)))))

(deftest relay-writes-a-held-line-that-turns-out-not-to-be-a-warning
  (is (= {:written "W1234 is not klog\nWarning-ish\n" :warnings []}
         (relayed "W1234 is not klog\nWarning-ish\n" true))))

(deftest relay-writes-a-prompt-without-a-line-ending
  (is (= {:written "Password: " :warnings []} (relayed "Password: " true))))

(deftest relay-collects-a-last-warning-without-a-line-ending
  (is (= {:written "" :warnings [endpoints-deprecation]} (relayed endpoints-deprecation true))))

(deftest relay-keeps-carriage-return-progress
  (is (= {:written "  10%\r  20%\r 100%\n" :warnings []}
         (relayed "  10%\r  20%\r 100%\n" true))))

(deftest relay-writes-a-line-before-the-child-ends-it
  ;; a long-running child whose progress line is not finished yet must show
  ;; it at once, not when the stream ends
  (let [sink (java.io.PipedInputStream.)
        pipe-in (java.io.PipedOutputStream. sink)
        reader (java.io.InputStreamReader. sink "UTF-8")
        out (java.io.StringWriter.)
        relay (future (shell/relay-stderr reader out true))
        send! (fn [^String s] (.write pipe-in (.getBytes s "UTF-8")) (.flush pipe-in))]
    (send! "Waiting for the drain")
    (let [deadline (+ (System/currentTimeMillis) 5000)]
      (while (and (not (str/includes? (str out) "Waiting for the drain"))
                  (< (System/currentTimeMillis) deadline))
        (Thread/sleep 20)))
    (is (= "Waiting for the drain" (str out)))
    (is (not (realized? relay)))
    (send! " - done\n")
    (.close pipe-in)
    (is (= [] (deref relay 5000 ::timeout)))
    (is (= "Waiting for the drain - done\n" (str out)))))
