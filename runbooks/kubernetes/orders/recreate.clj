;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.orders.recreate
  "Deletes invalid ACME orders so cert-manager recreates them."
  (:require [clojure.string :as str]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(def order-type "orders.acme.cert-manager.io")

(defn invalid-orders
  [context]
  (kubectl/namespaces-names
   (kubectl/filter-list (kubectl/get-all context order-type)
                        #(not= "valid" (-> % :status :state)))))

(defn recreate-effect
  [context selected]
  (apply effect/plan
         (for [order selected
               :let [{:keys [namespace name]} (kubectl/split-namespace-name order)]]
           (effect/kubectl context (str "--namespace=" namespace) "delete" order-type name))))

(defn cleared-checks
  "cert-manager recreates the order (often with the same name), so success is
   the invalid order being cleared - gone, or replaced by a non-invalid one."
  [context selected]
  (for [order selected
        :let [{:keys [namespace name]} (kubectl/split-namespace-name order)]]
    {:description (format "invalid order %s is cleared" order)
     :timeout 60 :interval 5
     :check (fn []
              (let [o (kubectl/get-namespaced-resource context namespace order-type name)]
                (or (nil? o)
                    (not (contains? #{"invalid" "errored"} (-> o :status :state))))))}))

(mutation/book
 {:description "Deletes invalid ACME orders so cert-manager recreates them"
  :options {:orders {:desc "namespace/name of invalid orders to recreate, repeatable - interactive selection when omitted"
                     :alias :o :coerce [:string]}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [order-type]
                  :kubectl-can-delete [order-type]}
  :select {:mode :many :label "Order" :flag :orders :candidates invalid-orders}
  :empty-message "every ACME order is valid"
  :confirm {:action "delete ACME orders so cert-manager recreates them"
            :destructive? true}
  :announce (fn [{:keys [target]}] (format "Recreate invalid orders [%s]" (str/join ", " target)))
  :effect (fn [{:keys [context target]}] (recreate-effect context target))
  :verify (fn [{:keys [context target]}] (cleared-checks context target))})
