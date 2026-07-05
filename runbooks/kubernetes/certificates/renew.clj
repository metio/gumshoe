;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.certificates.renew
  "Renews certificates managed by cert-manager."
  (:require [clojure.string :as str]
            [infra.effect :as effect]
            [infra.kubectl :as kubectl]
            [infra.mutation :as mutation]))

(def certificate-type "certificates.cert-manager.io")

(defn certificates
  [context]
  (kubectl/namespaces-names (kubectl/get-all context certificate-type)))

(defn renew-effect
  [context selected]
  (apply effect/plan
         (for [certificate selected
               :let [{:keys [namespace name]} (kubectl/split-namespace-name certificate)]]
           (effect/cmd "cmctl" "renew" name
                       (str "--context=" context)
                       (str "--namespace=" namespace)))))

(defn ready-checks
  [context selected]
  (for [certificate selected
        :let [{:keys [namespace name]} (kubectl/split-namespace-name certificate)]]
    {:description (format "certificate %s is Ready" certificate)
     :timeout 180 :interval 10
     :check (fn []
              (->> (-> (kubectl/get-namespaced-resource context namespace certificate-type name)
                       :status :conditions)
                   (some #(and (= "Ready" (:type %)) (= "True" (:status %))))
                   boolean))}))

(mutation/book
 {:description "Renews certificates managed by cert-manager"
  :options {:certificates {:desc "namespace/name of certificates to renew, repeatable - interactive selection when omitted"
                           :alias :c :coerce [:string]}}
  :prerequisites {:installed-tools ["cmctl" "kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [certificate-type]}
  :select {:mode :many :label "Certificate" :flag :certificates :candidates certificates}
  :confirm {:action "renew certificates - new certificates are requested from the issuer"}
  :announce (fn [{:keys [target]}] (format "Renew certificates [%s]" (str/join ", " target)))
  :effect (fn [{:keys [context target]}] (renew-effect context target))
  :verify (fn [{:keys [context target]}] (ready-checks context target))})
