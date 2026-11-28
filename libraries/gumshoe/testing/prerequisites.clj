;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.testing.prerequisites
  "A test helper comparing the kubectl reads a run performed with the read
   permissions its book declares, so a role that cannot make one of those
   reads is named by the prerequisite checklist before anything is read.

   A test runs a book's change against a stubbed shell that records every
   command, then hands the recorded argvs and the book's :prerequisites to
   undeclared-reads:

     (is (empty? (prereqs/undeclared-reads (:prerequisites spec) commands)))

   A read is a `kubectl get TYPE ..` (TYPE, TYPE/NAME, or several types
   joined by commas); `get --raw` (discovery), `auth can-i` and every
   other verb are not reads of a type. A type counts as declared when
   :kubectl-can-get or :kubectl-can-get-if-served lists it in the same
   spelling. Types are compared as written: kubectl accepts deploy,
   deployment and deployments.apps for one type, and a book should spell a
   type in its prerequisites as its reads spell it. A type a run reads that
   no static declaration can name, such as the kind of an object a Helm
   manifest claims, goes in :except."
  (:require [clojure.string :as str]
            [gumshoe.testing.plan :as plan]))

(defn- types-of
  [type-arg]
  (for [t (str/split type-arg #",")
        :let [t (first (str/split t #"/" 2))]
        :when (not (str/blank? t))]
    t))

(defn kubectl-reads
  "Pure: the set of resource types the kubectl calls among argvs read with
   get. An argv may start with \"kubectl\" or with its first argument."
  [argvs]
  (set
   (for [argv argvs
         :let [args (map str (filter string? (flatten [argv])))
               args (if (= "kubectl" (first args)) (rest args) args)
               {:keys [positionals]} (plan/parse-kubectl-args args)]
         :when (= "get" (first positionals))
         :let [type-arg (second positionals)]
         :when type-arg
         t (types-of type-arg)]
     t)))

(defn declared-reads
  "Pure: the set of resource types the prerequisites declare a get
   permission for, unconditionally or where the cluster serves them."
  [prerequisites]
  (set (concat (:kubectl-can-get prerequisites) (:kubectl-can-get-if-served prerequisites))))

(defn undeclared-reads
  "Pure: the sorted resource types argvs read that prerequisites declare no
   get permission for, leaving out the types in except."
  ([prerequisites argvs] (undeclared-reads prerequisites argvs nil))
  ([prerequisites argvs except]
   (vec (sort (remove (into (declared-reads prerequisites) except) (kubectl-reads argvs))))))
