;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pick
  "Parsers for the strings a select hands a book as its pick. Each returns a
   map naming the object, or nil for a string of another shape, so a book can
   refuse a pick it cannot act on instead of acting on half of it:

     \"<namespace>/<name>\"          namespaced
     \"<name>\"                      cluster-scoped
     \"<Kind> <namespace>/<name>\"   (kinded kind->type)
     \"<type> <namespace>/<name>\"   (typed types)

   A mutation spec's :subject takes one of them as its :parse.")

(defn namespaced
  "\"<namespace>/<name>\" as {:namespace :name}, or nil."
  [pick]
  (when-let [[_ namespace name] (re-matches #"\s*([^/\s]+)/([^/\s]+)\s*" (str pick))]
    {:namespace namespace :name name}))

(defn cluster-scoped
  "\"<name>\" as {:name}, or nil."
  [pick]
  (when-let [[_ name] (re-matches #"\s*([^/\s]+)\s*" (str pick))]
    {:name name}))

(defn kinded
  "A parser of \"<Kind> <namespace>/<name>\" into {:kind :type :namespace
   :name}, for the kinds kind->type (a map or a function) knows. Any other
   kind parses to nil."
  [kind->type]
  (fn [pick]
    (when-let [[_ kind namespace name] (re-matches #"\s*(\w+)\s+([^/\s]+)/(\S+)\s*" (str pick))]
      (when-let [type (kind->type kind)]
        {:kind kind :type type :namespace namespace :name name}))))

(defn typed
  "A parser of \"<type> <namespace>/<name>\" into {:type :namespace :name},
   for the types in types. Any other type parses to nil."
  [types]
  (let [known (set types)]
    (fn [pick]
      (when-let [[_ type namespace name] (re-matches #"\s*(\S+)\s+([^/\s]+)/(\S+)\s*" (str pick))]
        (when (known type)
          {:type type :namespace namespace :name name})))))
