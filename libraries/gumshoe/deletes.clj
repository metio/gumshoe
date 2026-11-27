;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.deletes
  "The one builder for a kubectl delete. Every delete a library, book or tool
   issues - as an effect, as a server-side dry run, or run directly - takes
   its arguments from here, so each carries the same pinned flags whatever
   the call site thought about.

   A target names what goes, as the objects of gumshoe.steps.objects do:

     {:type \"statefulsets\" :namespace \"shop\" :name \"web\"}   one object
     {:type \"secrets\" :namespace \"shop\" :selector \"owner=helm\"}
     {:type \"pods\" :namespace \"shop\" :all? true}             every one

   exactly one of :name, :selector or :all?, and :namespace left out for a
   cluster-scoped type. :all? needs a :namespace: a bulk delete over a
   cluster-scoped type, or one falling back to the context's default
   namespace, is not something a book should be able to say by omission.

   The options are the seam for what a call site does decide:

     :wait     true or false for --wait; absent leaves kubectl's default
               (wait for finalizers), so a site states it when it bounds
               the wait with its own post-check
     :cascade  :background, :foreground or :orphan for --cascade
     :extra    further flags from a short allowlist (allowed-extra-prefixes),
               each written as --flag=value

   A pinned flag cannot be turned back off through the seam, and a flag with
   its own key cannot be passed twice through :extra: either is refused with
   an ex-info naming the flag."
  (:require [clojure.string :as str]
            [gumshoe.effect :as effect]))

(def pinned-flags
  "The flags every delete carries. Both are about the environment the book
   runs in rather than about the object, which is what a call site does not
   think about.

   --ignore-not-found: a book judges its target before the confirmation and
   again after it, so an object missing at that point is refused there (and
   a server-side dry run, see dry-run-args, reports it). What this flag
   covers is the window after the judgement: a controller, a garbage
   collection cascade or another operator can remove the object before the
   delete reaches it. Without the flag that delete fails the step, and a
   plan deleting several objects stops halfway with the earlier deletes
   applied. Absence is the outcome the delete asked for, and the post-check
   (kubectl/confirmed-gone?) still demands a read that answered NotFound.

   --interactive=false: kubectl's -i asks before each object, and a kuberc
   defaults block can turn it on for the operator. A book has already taken
   its own confirmation, so a second prompt per object would mean the
   operator confirms twice, and a run with no terminal - an agent, a CI job -
   would block on the prompt. gumshoe.shell/child-env sets KUBERC=off for
   every child, which keeps a kuberc out already; the flag holds where that
   does not reach, such as a command copied from a printed plan, and
   documents the intent in the command itself. kubectl registers the flag
   from 1.29 on (1.27 and 1.28 only behind the alpha gate); an older kubectl
   refuses the delete with \"unknown flag: --interactive\" before deleting
   anything."
  ["--ignore-not-found" "--interactive=false"])

(def allowed-extra-prefixes
  "The flags :extra may carry, each as --flag=value. Anything else is
   refused, so a new kind of flag joins here, in one reviewed place."
  ["--request-timeout=" "--timeout=" "--grace-period="])

(def ^:private cascades #{:background :foreground :orphan})

(defn- refuse!
  [message data]
  (throw (ex-info (str "gumshoe.deletes: " message) data)))

(defn- check-target!
  [{:keys [type namespace name selector all?] :as target}]
  (when-not (map? target)
    (refuse! "a target is a map {:type .. :name|:selector|:all? ..}" {:target target}))
  (when-not (and (string? type) (not (str/blank? type)))
    (refuse! "a target needs a :type" {:target target}))
  (let [named (filter some? [name selector (when all? true)])]
    (when-not (= 1 (count named))
      (refuse! "a target names exactly one of :name, :selector or :all?" {:target target})))
  (doseq [[k v] {:name name :selector selector :namespace namespace}]
    (when (and (some? v) (or (not (string? v)) (str/blank? v)))
      (refuse! (str k " is a non-blank string") {:target target})))
  (when (and all? (not (true? all?)))
    (refuse! ":all? is true or absent" {:target target}))
  (when (and all? (nil? namespace))
    (refuse! ":all? needs a :namespace" {:target target})))

(defn- check-extra!
  [extra]
  (when-not (or (nil? extra) (sequential? extra))
    (refuse! ":extra is a vector of flags" {:extra extra}))
  (doseq [flag extra]
    (when-not (and (string? flag)
                   (some #(and (str/starts-with? flag %) (< (count %) (count flag)))
                         allowed-extra-prefixes))
      (refuse! (str "the flag " (pr-str flag) " is not one :extra may carry; allowed: "
                    (str/join ", " (map #(str % "<value>") allowed-extra-prefixes)))
               {:flag flag}))))

(defn- check-options!
  [{:keys [wait cascade extra] :as options}]
  (when-not (or (nil? options) (map? options))
    (refuse! "options are a map {:wait :cascade :extra}" {:options options}))
  (when-let [unknown (seq (remove #{:wait :cascade :extra} (keys options)))]
    (refuse! (str "unknown options " (str/join ", " unknown) "; the seam is :wait, :cascade and :extra")
             {:options options}))
  (when-not (or (nil? wait) (boolean? wait))
    (refuse! ":wait is true, false or absent" {:wait wait}))
  (when-not (or (nil? cascade) (contains? cascades cascade))
    (refuse! ":cascade is :background, :foreground or :orphan" {:cascade cascade}))
  (check-extra! extra))

(defn- selection
  [{:keys [type namespace name selector all?] :as target}]
  (check-target! target)
  (cond-> []
    namespace (conj (str "--namespace=" namespace))
    true (conj "delete" type)
    name (conj name)
    selector (conj (str "--selector=" selector))
    all? (conj "--all")))

(defn args
  "The kubectl arguments, after the context, that delete target: the
   selection, the pinned flags, then the seam's flags."
  ([target] (args target nil))
  ([target {:keys [wait cascade extra] :as options}]
   (check-options! options)
   (cond-> (into (selection target) pinned-flags)
     cascade (conj (str "--cascade=" (clojure.core/name cascade)))
     (some? wait) (conj (str "--wait=" wait))
     (seq extra) (into extra))))

(defn dry-run-args
  "The kubectl arguments, after the context, of a server-side dry run of
   deleting target. It carries --interactive=false and leaves
   --ignore-not-found out on purpose: the dry run is the judgement before
   the confirmation, and a target that does not exist then - a typo, an
   object already gone - must come back refused (NotFound, a non-zero exit)
   instead of passing as a successful delete."
  [target]
  (conj (selection target) "--interactive=false" "--dry-run=server"))

(defn effect
  "A :kubectl effect deleting target against context; see args."
  ([context target] (effect context target nil))
  ([context target options]
   (apply effect/kubectl context (args target options))))
