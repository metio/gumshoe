;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.investigate
  "Drill down from where the pain is. Start from a hostname a user reported, a
   namespace, a node, or a pod, and follow the thread - object to related
   object, with the crash logs and events surfaced for you - until you find the
   cause. Nothing is changed; the whole path is kept as a reproducer."
  (:require [clojure.string :as str]
            [infra.fuzzy-finder :as fuzzy]
            [infra.interact :as interact]
            [infra.investigation :as investigation]
            [infra.kubectl :as kubectl]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]
            [infra.subject :as subject]))

(def options
  {:host {:desc "A hostname a user reported - resolves to the route, service, and pods behind it"
          :alias :H
          :coerce :string}
   :namespace {:desc "Start from a namespace - pick one of its pods (unhealthy first)"
               :alias :n
               :coerce :string}
   :node {:desc "Start from a node by name"
          :coerce :string}
   :pod {:desc "Start from a pod, given as namespace/name"
         :coerce :string}
   :resume {:desc "Resume the last investigation where you left off"
            :coerce :boolean}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]})

(defn- pick-subject
  [prompt subjects]
  (when (seq subjects)
    (let [by-label (into {} (map (juxt subject/display identity) subjects))]
      (get by-label (fuzzy/select-single prompt (map subject/display subjects))))))

(defn- host-subjects
  "The routes and ingresses serving a hostname and the services they point at -
   the entry points behind a customer-reported URL, whether it is fronted by
   Gateway API or a classic Ingress. The backend services reuse the pure edge
   logic, so the route/ingress chain is followed the same way everywhere."
  [context host]
  (let [routes (investigation/routes-for-host
                (kubectl/items-of (kubectl/get-all context (subject/kind->type "HTTPRoute"))) host)
        ingresses (investigation/ingresses-for-host
                   (kubectl/items-of (kubectl/get-all context "ingresses")) host)]
    (distinct
     (concat
      (for [route routes]
        (subject/subject "HTTPRoute" (kubectl/namespace-of route) (kubectl/name-of route)))
      (for [ingress ingresses]
        (subject/subject "Ingress" (kubectl/namespace-of ingress) (kubectl/name-of ingress)))
      (for [route routes edge (subject/object-edges "HTTPRoute" route)] (:subject edge))
      (for [ingress ingresses edge (subject/object-edges "Ingress" ingress)] (:subject edge))))))

(defn- namespace-subjects
  [context namespace]
  (->> (kubectl/items-of (kubectl/get-namespaced context namespace "pods"))
       (sort-by #(if (= :ok (subject/situation "Pod" %)) 1 0))
       (map #(subject/subject "Pod" namespace (kubectl/name-of %)))))

(defn- pod-from-flag
  [value]
  (let [[namespace name] (str/split (str value) #"/" 2)]
    (if (str/blank? (str name))
      (do (stdout/error "give the pod as namespace/name, e.g. moodle/web-1") nil)
      (subject/subject "Pod" namespace name))))

(defn- resolve-host
  [context host]
  (pick-subject "what serves this host"
                (stdout/with-spinner (format "resolving routes, ingresses and services for %s" host)
                                     #(host-subjects context host))))

(defn- resolve-namespace-pod
  [context namespace]
  (pick-subject "which pod (unhealthy first)"
                (stdout/with-spinner (format "listing pods in %s" namespace)
                                     #(namespace-subjects context namespace))))

(defn- guided-start
  [context]
  (case (interact/choose-one "where does it hurt?"
                             ["a hostname a user reported"
                              "a namespace"
                              "a node"
                              "a pod"]
                             nil)
    "a hostname a user reported"
    (when-let [host (interact/ask-text
                     "Hostname"
                     (str "Which URL is failing? I'll resolve it to the HTTPRoute or Ingress serving it,\n"
                          "its backend service, and the pods behind it - then you drill from there.\n"
                          "Hostname, e.g. moodle.example.org:"))]
      (resolve-host context host))

    "a namespace"
    (when-let [namespace (interact/ask-text
                          "Namespace"
                          (str "Which namespace? I'll list its pods, the unhealthy ones first, for you to pick.\n"
                               "Namespace, e.g. moodle-prod:"))]
      (resolve-namespace-pod context namespace))

    "a node"
    (some->> (interact/ask-text
              "Node"
              (str "Which node? I'll open it so you can see its health and everything running on it.\n"
                   "Node name, e.g. worker-3.kube.example.org:"))
             (subject/subject "Node" nil))

    "a pod"
    (some-> (interact/ask-text
             "Pod"
             (str "Which pod? I'll open it and surface its logs and events.\n"
                  "Pod as namespace/name, e.g. moodle-prod/web-6d9f-abcde:"))
            pod-from-flag)

    nil))

(defn- resolve-start
  [context opts]
  (cond
    (:pod opts) (pod-from-flag (:pod opts))
    (:node opts) (subject/subject "Node" nil (:node opts))
    (:host opts) (resolve-host context (:host opts))
    (:namespace opts) (resolve-namespace-pod context (:namespace opts))
    :else (guided-start context)))

(defn- resume!
  "Restarts the last investigation from where it ended, with the breadcrumb
   trail behind it - fresh state, so a subject the cluster no longer has degrades
   to 'may have been deleted, go back'."
  [context]
  (if-let [trail (seq (investigation/load-trail))]
    (do (stdout/ok (format "resuming from %s (%d step(s) behind it)"
                           (subject/display (last trail)) (dec (count trail))))
        (investigation/investigate! context (last trail) (vec (butlast trail))))
    (do (stdout/error "no previous investigation to resume - start one with --host, --namespace, --node, or --pod")
        false)))

(defn- investigate
  [opts _ctx]
  (let [context (kubectl/current-context)]
    (if (:resume opts)
      (resume! context)
      (let [start (resolve-start context opts)]
        (if (nil? start)
          (do (stdout/error "nothing to investigate - give --host, --namespace, --node, or --pod")
              false)
          (investigation/investigate! context start))))))

(runbook/execute!
 {:description "Drill down from a hostname, namespace, node, or pod to the root cause"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action investigate})
