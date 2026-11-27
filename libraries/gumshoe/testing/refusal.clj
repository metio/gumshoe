;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.testing.refusal
  "A test helper that proves a change's refusal fires for the right reason:
   it refuses while one read does not answer, and lets the change proceed
   once that read answers.

   A test that only asserts that a refusal fired cannot tell a working
   refusal from one that fires whatever the cluster answers, which blocks
   every target, nor from one that fired for an unrelated reason. Both
   polarities have to be watched, against the same stubbed cluster, through
   the spec's real wiring - its subjects, :reads, :values, :derive,
   :recheck, :precondition and :refusal as gumshoe.mutation runs them -
   because the polarity bug usually sits in the wiring and not in the
   predicate.

     (refusal/is-polar
       spec
       {:target  \"apps/web\"
        :read    (refusal/kubectl-get {:type \"secrets\" :namespace \"apps\" :name \"web-tls\"})
        :answer  {:kind \"Secret\" :metadata {:name \"web-tls\" :namespace \"apps\"}}
        :cluster [[(refusal/kubectl-get {:type \"deployments\" :namespace \"apps\" :name \"web\"})
                   deployment]]
        :refusal \"secret apps/web-tls could not be read\"})

   `polarity` runs the spec twice. In the unanswered run, every call
   matching :read gets `unanswered`, a failure no reader takes for an
   answer; in the answered run it gets :answer. Every other call is
   answered from :cluster, the same in both runs. Each run judges the
   change twice, as the engine does:
     :before  before the confirmation, through gumshoe.mutation/prejudge:
              parse-pick, the :derive phase (subjects, :reads, :values,
              :derive), :done, and the prejudgement (:recheck, :refusal
              with :precondition folded in, the pinned and the subject
              objects);
     :after   after the confirmation, through gumshoe.mutation/run-on! with
              the change marked :confirmed? and gumshoe.flow/change!
              replaced by a stand-in that calls the :precondition thunk the
              engine built - the subjects and :reads read again, :recheck,
              :refusal, :precondition and every pin's drift check - and
              never asks, announces, applies or checks anything.
   The polarity holds when both judgements of the unanswered run refuse
   with the reason :refusal names, and both judgements of the answered run
   proceed.

   No real process can start inside the helper. Every command gumshoe.shell
   would run is answered by the stub, a command neither :read nor :cluster
   matches is recorded and fails the polarity, naming the command, and the
   process launchers of babashka.process and clojure.java.shell are
   replaced for the duration as well, so a read that bypasses gumshoe.shell
   is caught too. The command is recorded before anything else happens, so
   a caller that catches the exception still fails the polarity. The kube
   context is pinned to :context (gumshoe.kube-context/*pinned*), so no
   kubeconfig is read either.

   :refusal names the reason the unanswered run must refuse with: a string
   is compared for equality, a regex must match (re-find), a function of
   the reason must return truthy. A reason is what the engine prints after
   \"refusing: \", or, for a read in the :derive phase that did not answer,
   \"could not read what the change rests on - <why>\". A :precondition
   that returned false refuses with `unmet` in both judgements. There is no
   way to accept any refusal: a broad expectation is how an unrelated
   refusal passes the test.

   The answered run also checks that :read was made, so a control whose
   read never happens cannot pass, and that :read matched exactly one
   distinct command, so a matcher that also answers another read cannot
   make the gate proceed for the wrong reason.

   A result names every failure as a sentence that says which polarity and
   which judgement did not hold; `is-polar` reports them through
   clojure.test. The helper rebinds vars with with-redefs, so tests using
   it must not run concurrently with other tests that stub the same vars."
  (:require [babashka.process :as process]
            [cheshire.core :as cheshire]
            [clojure.java.shell :as java-shell]
            [clojure.string :as str]
            [clojure.test :as test]
            [gumshoe.flow :as flow]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]))

;; ---------------------------------------------------------------------------
;; answers

(def unanswered
  "What a read that does not answer gets: kubectl could not reach the API
   server. Every reader takes it for a read that did not answer - a strict
   read throws, gumshoe.reads classifies it :unread - and none takes it for
   an absence."
  {:exit 1 :out ""
   :err "Unable to connect to the server: dial tcp 192.0.2.1:6443: i/o timeout"})

(def absent
  "The answer of a named `kubectl get --ignore-not-found` for an object that
   does not exist: a clean exit with no output. gumshoe.reads classifies it
   :absent."
  {:exit 0 :out "" :err ""})

(defn not-found
  "The answer of a named `kubectl get` without --ignore-not-found for an
   object of type that does not exist. The plain getters return nil for it,
   and a strict read does not throw."
  [type name]
  {:exit 1 :out ""
   :err (format "Error from server (NotFound): %s \"%s\" not found" type name)})

(def unmet
  "The reason a :precondition that returned false refuses with, in both
   judgements."
  mutation/unmet-before-confirmation)

(defn- result?
  [answer]
  (and (map? answer) (contains? answer :exit)))

(defn answer
  "The process result a stubbed call returns for answer: a map holding
   :exit is taken as it is ({:exit :out :err}); a string is the output of a
   clean exit; anything else is encoded as JSON, the output of a clean exit
   (an object, a List)."
  [answer]
  (cond
    (result? answer) (merge {:out "" :err ""} answer)
    (string? answer) {:exit 0 :out answer :err ""}
    :else {:exit 0 :out (cheshire/generate-string answer) :err ""}))

;; ---------------------------------------------------------------------------
;; matchers

(def ^:private value-flags
  "kubectl flags that take their value as the next argument."
  #{"-n" "--namespace" "-l" "--selector" "-o" "--output" "--field-selector"
    "-f" "--filename" "--context" "--request-timeout"})

(defn- positionals
  "The arguments after `get` in a kubectl argv that are neither flags nor
   the values of flags."
  [args]
  (loop [[a & more] args out []]
    (cond
      (nil? a) out
      (value-flags a) (recur (rest more) out)
      (str/starts-with? a "-") (recur more out)
      :else (recur more (conj out a)))))

(defn- namespace-of
  "The namespace an argv names (--namespace=x, --namespace x or -n x),
   :all for --all-namespaces or -A, nil when it names none."
  [argv]
  (loop [[a & more] argv]
    (cond
      (nil? a) nil
      (#{"--all-namespaces" "-A"} a) :all
      (str/starts-with? a "--namespace=") (subs a (count "--namespace="))
      (#{"-n" "--namespace"} a) (first more)
      :else (recur more))))

(defn describe
  "How a failure names a matcher: its :description metadata, or \"the read
   under test\"."
  [matcher]
  (or (:description (meta matcher)) "the read under test"))

(defn kubectl-get
  "A matcher for one `kubectl get`: a function of an argv that is true when
   the argv runs kubectl get with exactly the positional arguments type and
   name (type alone when name is nil, so a listing and a named read never
   match each other), in namespace - a name, :all for --all-namespaces, or
   nil for none, so a cluster-scoped read does not match a namespaced one.
   :args are further arguments that must all appear (\"--selector=app=web\").
   The kube context and the output flags are not compared."
  [{:keys [type name namespace args]}]
  (let [wanted (cond-> [type] name (conj name))
        description (str "kubectl get " (str/join " " wanted)
                         (cond (= :all namespace) " --all-namespaces"
                               namespace (str " --namespace=" namespace)
                               :else "")
                         (when (seq args) (str " " (str/join " " args))))]
    (with-meta
      (fn [argv]
        (let [argv (vec argv)
              get-at (.indexOf ^java.util.List argv "get")]
          (boolean
           (and (= "kubectl" (first argv))
                (<= 0 get-at)
                (= wanted (positionals (subvec argv (inc get-at))))
                (= namespace (namespace-of argv))
                (every? (set argv) args)))))
      {:description description})))

(defn argv-starting
  "A matcher for a command whose argv starts with prefix, such as
   [\"ssh\" \"node-1\"] - for reads that are not kubectl gets."
  [prefix]
  (with-meta
    (fn [argv] (= (seq prefix) (seq (take (count prefix) argv))))
    {:description (str/join " " prefix)}))

;; ---------------------------------------------------------------------------
;; the stub

(defn- argv-of
  [args]
  (mapv str (flatten args)))

(defn- answer-from
  "The answer the cluster gives argv, or nil when nothing in it matches.
   cluster is a function of argv, or a seq of [matcher answer] pairs, the
   first match winning."
  [cluster argv]
  (if (fn? cluster)
    (some-> (cluster argv) answer)
    (some (fn [[matches? a]] (when (matches? argv) (answer a))) cluster)))

(defn- unstubbed!
  "Records argv as a call nothing answers and throws, so nothing real runs."
  [calls argv]
  (swap! calls update :unstubbed conj argv)
  (throw (ex-info (str "the refusal polarity helper refuses to run a real command: "
                       (str/join " " argv))
                  {::unstubbed argv})))

(defn- stubbed
  "Calls f with every process launch replaced: a gumshoe.shell capture is
   answered by the read under test (read-answer when it matches) or by
   cluster, and anything else - an unmatched capture, a streamed or started
   command, a launch through babashka.process or clojure.java.shell - is
   recorded in calls and throws."
  [calls read read-answer cluster context f]
  (let [capture (fn [_opts args]
                  (let [argv (argv-of args)]
                    (swap! calls update :all conj argv)
                    (cond
                      (read argv) (do (swap! calls update :read conj argv) read-answer)
                      :else (or (answer-from cluster argv) (unstubbed! calls argv)))))
        refuse (fn [& args] (unstubbed! calls (argv-of (remove map? args))))]
    (with-redefs-fn {#'shell/capture capture
                     #'shell/safe-stream (fn [_opts args] (unstubbed! calls (argv-of args)))
                     #'shell/start refuse
                     #'process/process refuse
                     #'process/shell refuse
                     #'process/sh refuse
                     #'java-shell/sh refuse
                     #'kubectl/resource-kinds-cache (atom nil)}
      (fn []
        (binding [kube-context/*pinned* {:context context :cluster context :announce-as context}
                  kubectl/*read-cache* nil]
          (f))))))

;; ---------------------------------------------------------------------------
;; the two judgements

(defn- plain
  [text]
  (str/replace (str text) #"\u001b\[[0-9;]*m" ""))

(def ^:private unread-prefix "could not read what the change rests on - ")

(defn- printed-reason
  "The reason a run printed: the text after the first \"refusing: \", else
   the line saying a :derive read did not answer, else nil."
  [text]
  (let [text (plain text)]
    (or (some-> (re-find #"(?m)refusing: (.*)$" text) second str/trim)
        (some->> (re-find (re-pattern (str "(?m)" (java.util.regex.Pattern/quote unread-prefix) "(.*)$")) text)
                 second str/trim (str unread-prefix)))))

(defn- printed
  "[what f returned, what it printed to *out* and *err*], or [::threw e]."
  [f]
  (let [[result chunks] (try (mutation/held f)
                             (catch Throwable e [[::threw e] nil]))]
    [result (apply str (map second chunks))]))

(defn- judge-before
  "The judgement before the confirmation, as prejudge makes it."
  [spec target opts]
  (let [[result text] (printed #(mutation/prejudge spec target opts nil))
        {:keys [refusal unread done cache]} (when (map? result) result)]
    (cond
      (and (vector? result) (= ::threw (first result)))
      {:verdict :threw :reason (ex-message (second result))}
      refusal {:verdict :refused :reason refusal}
      unread {:verdict :refused :reason (str unread-prefix unread)}
      done {:verdict :done}
      cache {:verdict :proceeds}
      :else {:verdict :refused :reason (or (printed-reason text) (str/trim (plain text)))})))

(defn- judge-after
  "The judgement after the confirmation, as run-on! makes it for a change
   whose confirmation was given: the :precondition thunk the engine hands
   gumshoe.flow, called once, with nothing asked, announced or applied."
  [spec target opts]
  (let [reached (atom nil)
        stand-in (fn [{:keys [precondition]}]
                   (let [[ok? text] (printed #(if precondition (precondition) true))]
                     (reset! reached {:ok? ok? :text text})
                     true))
        [result text] (with-redefs [flow/change! stand-in]
                        (printed #(mutation/run-on! (assoc spec :confirmed? true) target opts nil)))
        {:keys [ok?] judged :text} @reached]
    (cond
      (and (vector? ok?) (= ::threw (first ok?)))
      {:verdict :threw :reason (ex-message (second ok?))}
      (and (vector? result) (= ::threw (first result)))
      {:verdict :threw :reason (ex-message (second result))}
      @reached (if ok?
                 {:verdict :proceeds}
                 {:verdict :refused :reason (or (printed-reason judged) unmet)})
      result {:verdict :done}
      :else {:verdict :refused :reason (or (printed-reason text) (str/trim (plain text)))})))

(def ^:private judges {:before judge-before :after judge-after})

(def ^:private phase-words
  {:before "before the confirmation" :after "after the confirmation"})

(defn- run
  "Both judgements of spec with read answered by read-answer, and the calls
   they made."
  [spec {:keys [target opts read cluster context phases]} read-answer]
  (let [calls (atom {:all [] :read [] :unstubbed []})
        verdicts (into {}
                       (map (fn [phase]
                              [phase (stubbed calls read read-answer cluster context
                                              #((judges phase) spec target opts))]))
                       phases)]
    (assoc verdicts :calls @calls)))

;; ---------------------------------------------------------------------------
;; the verdict

(defn- describe-expected
  [expected]
  (cond
    (string? expected) (pr-str expected)
    (instance? java.util.regex.Pattern expected) (str "a reason matching #\"" expected "\"")
    :else "a reason the given predicate accepts"))

(defn- expected?
  [expected reason]
  (boolean
   (when (string? reason)
     (cond
       (string? expected) (= expected reason)
       (instance? java.util.regex.Pattern expected) (re-find expected reason)
       :else (expected reason)))))

(defn- unanswered-failure
  [read-name expected phase {:keys [verdict reason]}]
  (let [must (format "while %s does not answer, the gate must refuse with %s" read-name (describe-expected expected))
        when-said (phase-words phase)]
    (case verdict
      :refused (when-not (expected? expected reason)
                 (format "%s, but %s it refused with another reason: %s" must when-said (pr-str reason)))
      :proceeds (format "%s, but %s it proceeded" must when-said)
      :done (format "%s, but %s it skipped the change as already done" must when-said)
      :threw (format "%s, but %s it threw: %s" must when-said reason))))

(defn- answered-failure
  [read-name phase {:keys [verdict reason]}]
  (let [must (format "once %s answers, the gate must proceed" read-name)
        when-said (phase-words phase)]
    (case verdict
      :proceeds nil
      :refused (format "%s, but %s it refused: %s" must when-said (pr-str reason))
      :done (format "%s, but %s it skipped the change as already done" must when-said)
      :threw (format "%s, but %s it threw: %s" must when-said reason))))

(defn- call-failures
  [read-name {:keys [unanswered answered]}]
  (let [unstubbed (distinct (concat (get-in unanswered [:calls :unstubbed])
                                    (get-in answered [:calls :unstubbed])))
        reads (distinct (get-in answered [:calls :read]))]
    (concat
     (for [argv unstubbed]
       (format "a real command was attempted and refused: %s - answer it in :cluster, or it would reach a live system"
               (str/join " " argv)))
     (cond
       (empty? reads)
       [(format "%s was never made in the answered run, so this control proves nothing about it" read-name)]
       (< 1 (count reads))
       [(format "%s matched %d distinct commands, so more than one read was answered: %s - narrow the matcher to one read"
                read-name (count reads) (str/join "; " (map #(str/join " " %) reads)))]))))

(defn polarity
  "Runs spec against a stubbed cluster twice, once with the read under test
   unanswered and once answered, and judges both polarities (see the
   namespace doc). opts:
     :read     a matcher (fn [argv] -> bool) naming the read under test,
               such as kubectl-get returns; required
     :answer   what the read under test answers in the answered run (see
               answer); required
     :refusal  the reason the unanswered run must refuse with: a string,
               a regex or a predicate; required
     :cluster  the answers to every other call, the same in both runs: a
               seq of [matcher answer] pairs, the first match winning, or
               a function of argv returning an answer or nil
     :target   the pick, as the change's select would hand it
     :opts     the parsed flags
     :context  the kube context the run is pinned to (default
               \"polarity-control\")
     :phases   which judgements to make (default [:before :after])
   Returns {:holds? bool :failures [sentence ..] :unanswered {..}
   :answered {..}}, where each run holds the verdict of each judgement,
   {:verdict :refused|:proceeds|:done|:threw :reason ..}, and the :calls
   it made."
  [spec {:keys [read refusal phases context] :as opts}]
  (when-not (fn? read)
    (throw (IllegalArgumentException. "polarity needs :read, a matcher naming the read under test")))
  (when-not (contains? opts :answer)
    (throw (IllegalArgumentException. "polarity needs :answer, what the read under test answers in the answered run")))
  (when-not (or (string? refusal) (instance? java.util.regex.Pattern refusal) (fn? refusal))
    (throw (IllegalArgumentException. "polarity needs :refusal, the reason the gate must refuse with - a string, a regex or a predicate")))
  (let [opts (assoc opts
                    :phases (or phases [:before :after])
                    :context (or context "polarity-control"))
        read-name (describe read)
        runs {:unanswered (run spec opts unanswered)
              :answered (run spec opts (answer (:answer opts)))}
        failures (vec (concat
                       (call-failures read-name runs)
                       (keep #(unanswered-failure read-name refusal % (get-in runs [:unanswered %])) (:phases opts))
                       (keep #(answered-failure read-name % (get-in runs [:answered %])) (:phases opts))))]
    (assoc runs :holds? (empty? failures) :failures failures)))

(defn is-polar
  "polarity as a clojure.test assertion: passes when both polarities hold,
   else fails with every failure, one per line. Returns the result of
   polarity."
  [spec opts]
  (let [{:keys [holds? failures] :as result} (polarity spec opts)]
    (test/do-report {:type (if holds? :pass :fail)
                     :message (if holds?
                                (str "refusal polarity holds for " (describe (:read opts)))
                                (str "refusal polarity does not hold:\n  " (str/join "\n  " failures)))
                     :expected (list 'polar? (describe (:read opts)))
                     :actual (if holds? :holds failures)})
    result))
