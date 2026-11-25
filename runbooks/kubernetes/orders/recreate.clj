;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.orders.recreate
  "Deletes invalid ACME orders so cert-manager recreates them."
  (:require [clojure.string :as str]
            [gumshoe.cleanup :as cleanup]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]))

(def order-type "orders.acme.cert-manager.io")

(defn invalid-orders
  [context]
  (kubectl/namespaces-names
   (kubectl/filter-list (kubectl/get-all context order-type)
                        cleanup/invalid-order?)))

(defn order-refs
  "The ref of each picked order, which the engine reads again and pins."
  [{:keys [target]}]
  (for [order target]
    (assoc (kubectl/namespace-name-map order) :type order-type)))

(defn recreate-effect
  [context selected]
  (apply effect/plan
         (for [order selected
               :let [{:keys [namespace name]} (kubectl/namespace-name-map order)]]
           ;; --wait=false: the order's finalizer waits on cert-manager,
           ;; and cleared-checks is the bounded wait
           (effect/kubectl context (str "--namespace=" namespace) "delete" order-type name
                           "--wait=false"))))

(defn cleared-checks
  "cert-manager recreates the order (often with the same name), so success is
   the invalid order being cleared - gone, or replaced by a non-invalid one."
  [context selected]
  (for [order selected
        :let [{:keys [namespace name]} (kubectl/namespace-name-map order)]]
    {:description (format "invalid order %s is cleared" order)
     :timeout 60 :interval 5 :verifies-absence? true
     ;; gone needs a read that answered NotFound, whatever reads it
     :check (fn []
              (if-let [o (kubectl/get-namespaced-resource context namespace order-type name)]
                (not (contains? #{"invalid" "errored"} (-> o :status :state)))
                (kubectl/confirmed-gone? context namespace order-type name)))}))

(def selection
  {:mode :many :label "Order" :flag :orders :candidates invalid-orders})

(mutation/book
 {:description "Deletes invalid ACME orders so cert-manager recreates them"
  :options (select/options selection {:orders {:desc "namespace/name of invalid orders to recreate, repeatable - interactive selection when omitted"}})
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [order-type]
                  :kubectl-can-delete [order-type]}
  :select selection
  :empty-message "every ACME order is valid"
  :confirm {:action "delete ACME orders so cert-manager recreates them"
            :destructive? true}
  :subjects order-refs
  ;; cert-manager may have reported an order valid while the operator read the prompt
  :refusal (fn [{:keys [refs objects]}] (cleanup/first-refusal cleanup/order-refusal refs objects))
  :announce (fn [{:keys [target]}] (format "Recreate invalid orders [%s]" (str/join ", " target)))
  :effect (fn [{:keys [context target]}] (recreate-effect context target))
  :verify (fn [{:keys [context target]}] (cleared-checks context target))})
