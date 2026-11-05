;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.apiserver
  "Restarting kube-apiserver, which is a static pod the kubelet owns from
   /etc/kubernetes/manifests. Three things that look like a restart are not one:
   `kubectl delete pod` removes only the mirror pod, `systemctl restart kubelet`
   leaves static pod containers alone, and touching the manifest changes no pod
   hash. Stopping the container does work - the kubelet starts a replacement in
   the same sandbox - so the restart happens over SSH against the container
   runtime, not through the API.

   The reason to want one: a kube-apiserver can keep serving an aggregated
   discovery document that advertises a CRD version the CRD no longer serves,
   and then every client resolving that kind through discovery 404s objects that
   exist. Restarting the controller that complains does not help; restarting the
   API server does. gumshoe.detectives.apiversions finds that condition.

   These fns are pure over the runtime's output, so the book that drives them is
   testable without a cluster."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def control-plane-label
  "The label kubeadm puts on a control-plane node."
  "node-role.kubernetes.io/control-plane")

(defn control-plane-nodes
  "Names of the nodes carrying the control-plane role label. The label's value is
   conventionally empty, so presence of the key is what counts."
  [nodes-list]
  (kubectl/names-of
   {:items (kubectl/filter-list
            nodes-list
            #(contains? (-> % :metadata :labels) (keyword control-plane-label)))}))

(defn container-id
  "The running kube-apiserver container id from `crictl ps -q` output. The flag
   prints one id per line and nothing else; an empty or blank result means no
   such container is running, which is a reason to stop rather than a value."
  [text]
  (let [id (->> (str/split-lines (str text))
                (map str/trim)
                (remove str/blank?)
                first)]
    (when (seq id) id)))

(defn ready?
  "Whether /readyz answered ok. Anything else - an error page, a partial answer,
   nothing at all - is not ready."
  [text]
  (= "ok" (str/trim (str text))))

(defn restarted?
  "Whether the container was replaced: a new id exists and differs from the one
   that was stopped. A nil new id means the kubelet has not started the
   replacement yet, not that the restart failed."
  [before after]
  (boolean (and after (not= before after))))

(def ^:private socket
  "containerd's CRI socket. CRI-O hosts use /var/run/crio/crio.sock instead."
  "unix:///run/containerd/containerd.sock")

(defn configure-command
  "Writes /etc/crictl.yaml. Without it crictl probes several sockets and prints
   deprecation warnings on every call."
  []
  ["sudo" "crictl" "config"
   "--set" (str "runtime-endpoint=" socket)
   "--set" (str "image-endpoint=" socket)
   "--set" "timeout=10"])

(defn container-id-command
  []
  ["sudo" "crictl" "ps" "--name" "kube-apiserver" "-q"])

(defn started-at-command
  [id]
  ["sudo" "crictl" "inspect" "-o" "go-template"
   "--template" "{{.status.startedAt}}" id])

(defn stop-command
  "Stops the container. Without a timeout crictl sends SIGKILL, so the
   predecessor exits 137 - that is the stop, not a crash."
  [id]
  ["sudo" "crictl" "stop" id])

(defn readyz-command
  "Asks the local apiserver rather than the load balancer, so the answer is about
   this node. -k because the serving certificate is not valid for localhost."
  []
  ["curl" "-sk" "https://localhost:6443/readyz"])
