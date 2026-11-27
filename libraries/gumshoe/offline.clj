;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.offline
  "Code that must compute from what it was handed, and never from the world.

   forbidding runs a thunk with *forbidden* bound to the reason, and every
   place gumshoe starts a command or sends a request calls check! first:
   each launch in gumshoe.shell, which every kubectl, ssh and local command
   goes through, each request in gumshoe.http, and the posts of
   gumshoe.announce and gumshoe.pad. Reading a local file is not guarded. Inside the binding check! throws
   an ex-info carrying ::forbidden and naming what was about to run, before
   anything started, so nothing was read and nothing changed. Outside it
   check! does nothing.

   gumshoe.mutation computes a change's confirmation items and its effect
   plan this way: both describe the plan the operator approves, and both
   must come from the one context the :derive phase read, so the
   confirmation cannot list one reading while the effect applies another."
  (:require [clojure.string :as str]))

(def ^:dynamic *forbidden*
  "nil, or why nothing may reach outside the process while it is bound."
  nil)

(defn forbidden?
  "Whether x is the exception check! throws."
  [x]
  (boolean (and (instance? clojure.lang.ExceptionInfo x) (::forbidden (ex-data x)))))

(defn- described
  [what]
  (str/join " " (map str (flatten [what]))))

(defn check!
  "Throws the ::forbidden ex-info naming `what` (an argv, a URL) when
   *forbidden* is bound; nil otherwise."
  [what]
  (when-let [why *forbidden*]
    (throw (ex-info (format "not running '%s': %s" (described what) why)
                    {::forbidden true :what (described what) :why why}))))

(defn forbidding
  "The result of thunk, realized completely inside the binding, with
   *forbidden* bound to why, so a lazy sequence cannot reach out after the
   binding is gone. Throws what check! threw."
  [why thunk]
  (binding [*forbidden* why]
    (let [result (thunk)]
      (dorun (tree-seq coll? seq result))
      result)))
