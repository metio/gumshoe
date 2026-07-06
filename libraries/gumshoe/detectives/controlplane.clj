;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.controlplane
  "Detective for the control plane: the kubeadm static pods (apiserver,
   scheduler, controller-manager, etcd) must be Running and Ready. Maps to the
   kube-prometheus KubeAPIDown / KubeSchedulerDown / KubeControllerManagerDown
   alerts, but read straight from the pods so it needs no metrics backend."
  (:require [gumshoe.kubectl :as kubectl]))

(def ^:private component-names
  {"kube-apiserver" "API server"
   "kube-scheduler" "scheduler"
   "kube-controller-manager" "controller-manager"
   "etcd" "etcd"})

(defn- control-plane-component
  [pod]
  (get component-names (-> pod :metadata :labels :component)))

(defn- ready?
  [pod]
  (and (= "Running" (-> pod :status :phase))
       (some #(and (= "Ready" (:type %)) (= "True" (:status %)))
             (-> pod :status :conditions))))

(defn detect-control-plane
  [evidence]
  (for [pod (kubectl/items-of (get evidence "pods"))
        ;; kubeadm runs the static control-plane pods in kube-system; scoping to
        ;; it keeps a user workload that merely carries a `component: etcd` label
        ;; from reading as a control-plane outage.
        :when (= "kube-system" (kubectl/namespace-of pod))
        :let [component (control-plane-component pod)]
        :when (and component (not (ready? pod)))]
    {:severity :critical
     :component (kubectl/namespace-name-of pod)
     :summary (format "control-plane %s is not ready (phase %s)"
                      component (or (-> pod :status :phase) "unknown"))
     :hint "a control-plane component is down - the cluster can not be managed until it recovers"}))

(def detectives
  [{:name "control-plane"
    :description "Control-plane static pods (apiserver, scheduler, controller-manager, etcd) are Ready"
    :requires ["pods"]
    :detect detect-control-plane}])
