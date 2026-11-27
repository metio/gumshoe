;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.testing.plan
  "A test helper holding a mutation spec's dry-run plan to the evidence its
   effect runs on.

   A change shows the operator its :items and applies its :effect. check
   runs the spec's :derive phase once - the subject reads, :reads, :values
   and :derive, through gumshoe.mutation/derived-context, against whatever
   the test stubbed at the shell boundary - then computes :items and
   :effect over that one context through gumshoe.mutation/planned, and
   asserts that every object the effect plan touches is named by what the
   items list. It fails when the items leave out part of the plan, and
   when :items or :effect reads anything themselves: planned refuses a
   command or an HTTP request either starts, and check wraps the test's
   stubs of gumshoe.shell and gumshoe.http so that a stubbed read is
   refused the same way a real one is.

     (plan/check spec {:target \"shop/web\" :opts {..}})
     (plan/covers! spec {:target \"shop/web\"})   ; inside a deftest

   Objects are identified as {:type :namespace :name} refs.

   Touched, from each effect of the plan:
     [:kubectl ctx & args]   the objects its arguments name: TYPE NAME..,
                             TYPE/NAME.., the nodes of cordon, uncordon,
                             drain and taint, the NAME of create TYPE NAME,
                             the pod of exec, attach, port-forward and logs,
                             the object of a --raw API path; the namespace
                             from -n/--namespace. key=value and key- of
                             annotate, label, taint and set are arguments,
                             not objects.
     [:kubectl-stdin ctx stdin & args]
                             the objects of the manifest on stdin (JSON or
                             YAML, several documents, a List's items), by
                             kind, metadata.namespace (else -n) and
                             metadata.name; with --raw, the API path.
     [:kubectl-plugin ctx \"cnpg\" verb ..], [:cmd \"kubectl-cnpg\" verb ..]
                             the CloudNativePG cluster promote, restart,
                             reload, destroy, hibernate, fencing and
                             maintenance name, as clusters.postgresql.cnpg.io.
     [:cmd \"flux\" verb ..]   the object reconcile, suspend and resume name
                             (TYPE NAME, or source TYPE NAME).
     [:cmd \"helm\" verb ..]   the release rollback, uninstall, upgrade and
                             install name, as type helm-release.
     [:cmd \"cmctl\" \"renew\" ..]
                             the certificates it names.
     [:ssh connection ..]    the host, as {:type \"host\" :name <host>}.
     [:note ..]              nothing.
   Anything else is unidentified: another plugin or command, a custom
   effect, a call selecting by -l, --field-selector, --all or -A, reading a
   file with -f, naming several types at once, or creating an object by
   generateName. An unidentified effect fails the
   check, because what it touches cannot be compared with the items. The
   scenario's :touches, (fn [effect] -> refs or nil), names what an effect
   touches before these rules are tried: refs, [] for an effect that
   touches no cluster object (an ssh command on a host), nil to fall back
   to them.

   Listed: an item names a ref when the item, printed with str and split
   into words, holds the ref's name as a word, and, for a ref with a
   namespace, holds <namespace>/<name> or the namespace as a word as well,
   both in that one item. A word is split again at /, so \"deployment/web\"
   names web. The type is not compared: items name an object as the
   operator reads it (\"Deployment shop/web\", \"shop/web\", \"node n1\"),
   and kubectl spells one type many ways (deploy, deployment,
   deployments.apps, Deployment). A ref that no item names is :missing, and
   the failure lists each one as <type> <namespace>/<name>."
  (:require [cheshire.core :as json]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [clojure.test :as test]
            [gumshoe.change :as change]
            [gumshoe.effect :as effect]
            [gumshoe.http :as http]
            [gumshoe.mutation :as mutation]
            [gumshoe.offline :as offline]
            [gumshoe.shell :as shell]))

;; --- what an effect touches ----------------------------------------------------------

(def ^:private valued-flags
  "kubectl flags that take the next argument as their value when written
   without =."
  #{"-n" "--namespace" "-p" "--patch" "--type" "-l" "--selector" "-f" "--filename"
    "-o" "--output" "-c" "--container" "--field-manager" "--subresource" "--replicas"
    "--grace-period" "--timeout" "--from" "--raw" "--context" "--as" "-k" "--kustomize"
    "--field-selector" "--overrides" "--image" "--restart" "--cascade" "--patch-file"
    "--current-replicas" "--resource-version" "--pod-running-timeout" "--for"})

(def ^:private selecting-flags
  "Flags that make a kubectl call act on a set no argument names."
  #{"-l" "--selector" "--field-selector" "--all" "-A" "--all-namespaces" "-k" "--kustomize"})

(defn- flag-name [arg] (first (str/split arg #"=" 2)))

(defn parse-kubectl-args
  "Pure: kubectl arguments as {:positionals [..] :flags {name value}}; a
   flag without a value maps to true. Parsing stops at --, after which the
   arguments belong to the command exec runs."
  [args]
  (loop [[arg & more :as args] (map str args) positionals [] flags {}]
    (cond
      (empty? args) {:positionals positionals :flags flags}
      (= "--" arg) {:positionals positionals :flags flags}
      (and (str/starts-with? arg "-") (not= "-" arg))
      (let [[k v] (str/split arg #"=" 2)]
        (cond
          v (recur more positionals (assoc flags k v))
          (and (contains? valued-flags k) (seq more)) (recur (rest more) positionals (assoc flags k (first more)))
          :else (recur more positionals (assoc flags k true))))
      :else (recur more (conj positionals arg) flags))))

(defn- argument?
  "Whether a positional is an argument of annotate, label, taint or set -
   key=value, key:Effect, key- - and not an object."
  [s]
  (or (str/includes? s "=") (str/includes? s ":") (str/ends-with? s "-")))

(defn- slashed
  "TYPE/NAME as a ref, or nil."
  [s namespace]
  (let [[type name] (str/split s #"/" 2)]
    (when (and (seq type) (seq name) (not (str/includes? name "/")))
      {:type type :namespace namespace :name name})))

(defn raw-path-ref
  "Pure: the object a Kubernetes API path names, or nil:
   /api/v1/namespaces/web, /api/v1/namespaces/web/pods/p,
   /apis/apps/v1/namespaces/web/deployments/d/scale, /api/v1/nodes/n1."
  [path]
  (let [segments (vec (remove str/blank? (str/split (first (str/split (str path) #"\?" 2)) #"/")))
        rest-of (cond
                  (= "api" (first segments)) (subvec segments (min 2 (count segments)))
                  (= "apis" (first segments)) (subvec segments (min 3 (count segments)))
                  :else nil)]
    (when rest-of
      (let [[a b c d] rest-of]
        (cond
          (and (= "namespaces" a) b c d) {:type c :namespace b :name d}
          (and (= "namespaces" a) b (nil? c)) {:type "namespaces" :namespace nil :name b}
          (and (= "namespaces" a) b c) {:type "namespaces" :namespace nil :name b}
          (and a b (not= "namespaces" a)) {:type a :namespace nil :name b}
          :else nil)))))

(defn- objects-of-verb
  "The refs a kubectl verb's positionals name, or nil when they name none
   this helper can identify."
  [[verb & positionals] namespace]
  (let [names-in (fn [type names]
                   (let [names (remove argument? names)]
                     (when (and (seq names) (not (str/includes? type ",")))
                       (mapv #(or (slashed % namespace) {:type type :namespace namespace :name %}) names))))
        generic (fn [[first-arg & more :as ps]]
                  (cond
                    (empty? ps) nil
                    (str/includes? first-arg "/") (let [refs (map #(slashed % namespace) (remove argument? ps))]
                                                   (when (every? some? refs) (vec refs)))
                    :else (names-in first-arg more)))]
    (case verb
      ("cordon" "uncordon" "drain")
      (when (seq positionals)
        (mapv #(or (slashed % nil) {:type "nodes" :namespace nil :name %}) positionals))
      "taint" (let [[type & more] positionals]
                (if (and type (str/includes? type "/"))
                  (generic positionals)
                  (when type (names-in type more))))
      ("rollout" "set" "certificate") (generic (rest positionals))
      "create" (let [[type & more] positionals
                     more (if (#{"secret"} type) (rest more) more)]
                 (when (and type (first more))
                   [{:type type :namespace namespace :name (first more)}]))
      ("exec" "attach" "port-forward" "logs")
      (when-let [pod (first positionals)]
        [(or (slashed pod namespace) {:type "pods" :namespace namespace :name pod})])
      ("delete" "patch" "annotate" "label" "scale" "edit" "replace" "apply" "expose" "autoscale")
      (generic positionals)
      nil)))

(defn- manifest-documents
  [stdin]
  (let [text (str stdin)]
    (or (try (let [doc (json/parse-string text true)] (when (some? doc) [doc]))
             (catch Exception _ nil))
        (try (vec (remove nil? (yaml/parse-string text :load-all true :keywords true)))
             (catch Exception _ nil)))))

(defn manifest-refs
  "Pure: the refs of the objects a manifest holds, each document and each
   item of a List, or nil when one of them has no kind or no
   metadata.name. namespace stands in for an object naming none."
  [stdin namespace]
  (let [objects (mapcat #(if (and (map? %) (str/ends-with? (str (:kind %)) "List") (sequential? (:items %)))
                           (:items %)
                           [%])
                        (manifest-documents stdin))
        refs (for [o objects]
               (when (and (map? o) (seq (str (:kind o))) (seq (str (get-in o [:metadata :name]))))
                 {:type (str (:kind o))
                  :namespace (or (get-in o [:metadata :namespace]) namespace)
                  :name (str (get-in o [:metadata :name]))}))]
    (when (and (seq refs) (every? some? refs)) (vec refs))))

(defn- kubectl-refs
  [args stdin]
  (let [{:keys [positionals flags]} (parse-kubectl-args args)
        namespace (or (get flags "-n") (get flags "--namespace"))
        namespace (when (string? namespace) namespace)
        filename (or (get flags "-f") (get flags "--filename"))
        raw (get flags "--raw")]
    (cond
      (some selecting-flags (keys flags)) nil
      (string? raw) (some-> (raw-path-ref raw) vector)
      (= "-" filename) (when stdin (manifest-refs stdin namespace))
      (some? filename) nil
      :else (objects-of-verb positionals namespace))))

(defn- namespace-flag
  [flags]
  (let [v (or (get flags "-n") (get flags "--namespace"))]
    (when (string? v) v)))

(defn- cnpg-refs
  [[plugin verb & args]]
  (let [{:keys [positionals flags]} (parse-kubectl-args args)
        cluster (case verb
                  ("promote" "restart" "reload" "destroy") (first positionals)
                  ("hibernate" "fencing" "maintenance") (second positionals)
                  nil)]
    (when (and (= "cnpg" plugin) cluster (not (some selecting-flags (keys flags))))
      [{:type "clusters.postgresql.cnpg.io" :namespace (namespace-flag flags) :name cluster}])))

(defn- cmd-refs
  [[program & args]]
  (let [{:keys [positionals flags]} (parse-kubectl-args args)
        namespace (namespace-flag flags)
        [verb & more] positionals]
    (when-not (some selecting-flags (keys flags))
      (case program
        "flux" (when (#{"reconcile" "suspend" "resume"} verb)
                 (let [[type name] (if (= "source" (first more)) (rest more) more)]
                   (when (and type name) [{:type type :namespace namespace :name name}])))
        "helm" (when (and (#{"rollback" "uninstall" "upgrade" "install"} verb) (first more))
                 [{:type "helm-release" :namespace namespace :name (first more)}])
        "kubectl-cnpg" (cnpg-refs (cons "cnpg" args))
        "cmctl" (when (and (= "renew" verb) (seq more))
                  (mapv #(hash-map :type "certificates" :namespace namespace :name %) more))
        nil))))

(defn touched
  "Pure: {:refs [..]} the refs one effect touches, or {:unidentified
   effect} when they cannot be told apart from its arguments (see the
   namespace doc). touches, when given, is asked first."
  ([effect] (touched effect nil))
  ([[op & args :as effect] touches]
   (if-let [given (when touches (touches effect))]
     {:refs (vec given)}
     (let [refs (case op
                  :note []
                  :kubectl (kubectl-refs (rest args) nil)
                  :kubectl-stdin (let [[_ stdin & kargs] args] (kubectl-refs kargs stdin))
                  :kubectl-plugin (cnpg-refs (rest args))
                  :cmd (cmd-refs args)
                  :ssh (when-let [host (:host (first args))] [{:type "host" :namespace nil :name (str host)}])
                  nil)]
       (if (some? refs) {:refs refs} {:unidentified effect})))))

;; --- what the items list -------------------------------------------------------------

(defn- words
  [item]
  (let [ws (->> (str/split (str item) #"[\s,;:()\[\]{}\"'<>=@|]+")
                (map #(str/replace % #"[.!?]+$" ""))
                (remove str/blank?))]
    {:words (set ws)
     :parts (into (set ws) (mapcat #(str/split % #"/")) ws)}))

(defn names?
  "Pure: whether one confirmation item names ref (see the namespace doc)."
  [item {:keys [namespace name]}]
  (let [{w :words p :parts} (words item)
        qualified (str namespace "/" name)]
    (boolean
     (and (seq name)
          (contains? p name)
          (or (str/blank? namespace)
              (contains? p namespace)
              (some #(or (= qualified %) (str/ends-with? % (str "/" qualified))) w))))))

(defn listed?
  "Pure: whether one of items names ref."
  [items ref]
  (boolean (some #(names? % ref) items)))

(defn describe-ref
  "<type> <namespace>/<name>, or <type> <name> without a namespace."
  [{:keys [type namespace name]}]
  (if (str/blank? namespace) (str type " " name) (str type " " namespace "/" name)))

(defn coverage
  "Pure: how items cover plan. {:touched [refs, each once, in plan order]
   :missing [the touched refs no item names] :unidentified [effects whose
   objects cannot be told]}."
  [items plan touches]
  (let [results (map #(touched % touches) (or (effect/as-plan plan) []))
        refs (into [] (comp (mapcat :refs) (distinct)) results)]
    {:touched refs
     :missing (vec (remove #(listed? items %) refs))
     :unidentified (vec (keep :unidentified results))}))

;; --- running a spec ------------------------------------------------------------------

(def ^:private guarded-vars
  "The vars every command and every request of gumshoe goes through, which
   a test stubs; check wraps them so a stub refuses inside
   gumshoe.offline/forbidding as the real one does."
  [#'shell/execute #'shell/execute-with-stdin #'shell/execute-env #'shell/stdout-of
   #'shell/exit-code-of #'shell/run-with-output #'shell/run-with-output-env
   #'shell/run-attributed #'shell/capture-line #'shell/pipe-to! #'shell/start
   #'http/request #'http/fetch #'http/fetch-lines!])

(defn- guarding
  "thunk with each of guarded-vars, as the test bound it, checking
   gumshoe.offline before it runs."
  [thunk]
  (with-redefs-fn
    (into {} (for [v guarded-vars
                   :let [f @v]]
               [v (fn [& args] (offline/check! args) (apply f args))]))
    thunk))

(defn failure
  "Pure: why a check result fails, as lines, or nil when it passes."
  [{:keys [problem missing unidentified]}]
  (cond
    problem [problem]
    (or (seq missing) (seq unidentified))
    (concat
     (when (seq missing)
       (cons "the effect plan touches objects the dry-run plan (:items) does not list:"
             (map #(str "  " (describe-ref %)) missing)))
     (when (seq unidentified)
       (cons "the effect plan holds effects whose objects cannot be told - name them with :touches:"
             (map #(str "  " (effect/describe %)) unidentified))))
    :else nil))

(defn check
  "Runs spec's :derive phase once for the scenario, computes its :items and
   :effect over that context, and compares them (see the namespace doc).

   scenario: :target the pick, :opts the parsed flags, :context and
   :cluster the connection (\"ctx\" and \"cluster\" when absent), :touches
   (fn [effect] -> refs or nil).

   Returns {:ok? :items :plan :touched :missing :unidentified :failure},
   or {:ok? false :problem <why> :failure [<why>]} when the pick was
   refused, a read of the :derive phase did not answer, or :items or
   :effect reached outside the process."
  [spec {:keys [target opts context cluster touches] :or {context "ctx" cluster "cluster"}}]
  (let [spec (change/normalize spec)
        connection {:context context :cluster cluster}
        {:keys [ctx refusal unread]} (mutation/derived-context spec connection target (or opts {}))
        result (cond
                 refusal {:problem (str "the :derive phase was refused: " refusal)}
                 unread {:problem (str "a read of the :derive phase did not answer: " unread)}
                 :else (let [{:keys [items effect] why :refusal} (guarding #(mutation/planned spec ctx target))]
                         (if why
                           {:problem why}
                           (let [plan (effect/as-plan effect)]
                             (if (and (some? effect) (nil? plan))
                               {:problem (str ":effect returned something that is not a plan: " (pr-str effect))}
                               (merge {:items (vec items) :plan (vec plan)}
                                      (coverage items plan touches)))))))
        why (failure result)]
    (assoc result :ok? (nil? why) :failure (some-> why vec))))

(defn covers!
  "check, reported to clojure.test as one assertion: a failure's message
   lists the objects missing from the dry-run plan, the effects it cannot
   identify, or the problem. Returns the check result."
  [spec scenario]
  (let [{:keys [ok? failure items] :as result} (check spec scenario)]
    (test/do-report {:type (if ok? :pass :fail)
                     :message (if ok?
                                "the dry-run plan lists every object the effect touches"
                                (str/join "\n" failure))
                     :expected '(every touched object listed in :items)
                     :actual (if ok? items (select-keys result [:items :missing :unidentified :problem]))})
    result))
