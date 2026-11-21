;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prerequisites
  "Custom prerequisite checks - a plugin seam. The harness ships the built-in
   checks (installed tools, tool versions, connectivity, secrets, cluster
   capabilities, kubectl permissions). A plugin registers MORE with
   `register-check!`, so a book can gate on organisation policy the core knows
   nothing about: a change-freeze window, a maintenance ticket, an on-call
   acknowledgement, 'this cluster is not already mid-incident'.

   A book declares the check under its own key in :prerequisites; the registered
   builder turns that value into checklist items that animate in the Prerequisites
   section exactly like the built-ins, and any failure stops the book before it
   touches anything. Custom checks run after the built-in tooling/connectivity/
   permission checks."
  (:refer-clojure :exclude [check])
  (:require [gumshoe.command :as command]))

(defonce ^:private checks (atom {}))

(defn register-check!
  "Registers a prerequisite check. `key` is the :prerequisites map key a book
   declares (e.g. :change-window). `build` is (fn [value opts] -> seq of
   [label thunk] items); a thunk does the check and returns {:ok? bool
   :label \"...\"} (or a plain truthy/falsy value). See `check` for the common
   single-item case."
  [key build]
  (swap! checks assoc key build))

(defn registered-checks
  "Every registered check key, sorted for a stable checklist order."
  []
  (sort (keys @checks)))

(defn check
  "Builds one checklist item from a label, a predicate thunk, and the labels (or
   0-arg fns) to show on pass/fail - the common shape for a register-check!
   builder."
  [label predicate {:keys [pass fail]}]
  [label (fn []
           (if (predicate)
             {:ok? true :label (if (fn? pass) (pass) (or pass label))}
             {:ok? false :label (if (fn? fail) (fail) (or fail label))}))])

(defn items
  "The checklist items contributed by every registered check the book declares,
   in a stable order. The harness appends these after the built-in checks."
  [prerequisites opts]
  (mapcat (fn [key]
            (when (contains? prerequisites key)
              ((get @checks key) (get prerequisites key) opts)))
          (registered-checks)))

(defn- higher-version
  [a b]
  (if (command/version-at-least? a b) a b))

(defn- combine
  [key a b]
  (cond
    (= :minimum-tool-versions key) (merge-with higher-version a b)
    (= a b) a
    (and (sequential? a) (sequential? b)) (vec (distinct (concat a b)))
    (and (map? a) (map? b)) (merge a b)
    :else (throw (ex-info (format "the steps declare %s differently: %s and %s"
                                  key (pr-str a) (pr-str b))
                          {:key key :values [a b]}))))

(defn union
  "The prerequisites of several steps as one map, so a book that runs them all
   checks everything it will need before its first step. Lists are joined
   without duplicates, a tool's version floor is the highest one any step
   declares, and a key whose values can not be joined throws."
  [& prerequisites]
  (reduce (fn [acc m]
            (reduce-kv (fn [acc k v]
                         (if (contains? acc k) (update acc k #(combine k % v)) (assoc acc k v)))
                       acc m))
          {}
          (remove nil? prerequisites)))
