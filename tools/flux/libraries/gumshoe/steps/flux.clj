;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps.flux
  "Steps that suspend, resume and reconcile one Flux object.

   An object is {:kind :type :namespace :name}. Every kind in
   gumshoe.tools.flux/suspendable switches through spec.suspend; a
   flux-operator ResourceSet switches through its
   fluxcd.controlplane.io/reconcile annotation instead.

   A builder returns the parts of a step that act - :effect and :verify, and
   :derive where the step needs a token - and the caller merges them into a
   step map with its own title, confirmation, prerequisites, :precondition and
   :recovery (see gumshoe.playbook). The object comes from a function of the
   step's context, so a step acts on whatever its :derive observed; a step
   whose object is nil plans a note and checks nothing."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.tools.flux :as flux]))

(def resourceset-type "resourcesets.fluxcd.controlplane.io")

(def ^:private resourceset-reconcile :fluxcd.controlplane.io/reconcile)

(def ^:private requested-at "reconcile.fluxcd.io/requestedAt")

(defn suspended?
  "Whether the object is told not to reconcile: spec.suspend, or the reconcile
   annotation set to disabled on a ResourceSet."
  [kind object]
  (case kind
    "ResourceSet" (= "disabled" (get-in object [:metadata :annotations resourceset-reconcile]))
    (true? (-> object :spec :suspend))))

(defn suspend-effect
  "Suspends the object, or resumes it when `wanted` is false. A resume also
   asks for a reconcile at `timestamp`, so the object catches up at once
   instead of at its next interval."
  [context {:keys [kind type namespace name] :as object} wanted timestamp]
  (case kind
    "ResourceSet"
    (effect/plan
     (effect/kubectl context "-n" namespace "annotate" type name "--overwrite"
                     (str "fluxcd.controlplane.io/reconcile=" (if wanted "disabled" "enabled")))
     (when-not wanted
       (effect/kubectl context "-n" namespace "annotate" type name "--overwrite"
                       (str requested-at "=" timestamp))))
    (flux/suspend-effect context object wanted timestamp)))

(defn ready?
  "Whether the object's Ready condition is True."
  [object]
  (conditions/ready? object))

(defn fetch
  "The object as the cluster holds it now."
  [context {:keys [type namespace name]}]
  (kubectl/get-namespaced-resource context namespace type name))

(defn- label [{:keys [kind namespace name]}]
  (format "%s %s/%s" kind namespace name))

(defn- seconds [timeout ctx]
  (if (number? timeout) timeout (timeout ctx)))

(defn- switch
  [{:keys [object none timeout interval description also]} wanted done? default-description]
  {:effect (fn [{:keys [context timestamp] :as ctx}]
             (if-let [o (object ctx)]
               (suspend-effect context o wanted timestamp)
               (effect/plan (effect/note none))))
   :verify (fn [{:keys [context] :as ctx}]
             (when-let [o (object ctx)]
               [{:description (or description (default-description o))
                 :timeout (seconds timeout ctx) :interval interval
                 :check #(let [live (fetch context o)]
                           (and (done? (:kind o) live)
                                (if also (also ctx live) true)))}]))})

(defn suspend
  "The :effect and :verify of a step that suspends the object `(object ctx)`
   returns and waits until the cluster says it is suspended.

     :object      (fn [ctx] -> {:kind :type :namespace :name} or nil)
     :none        the note planned when there is no object
     :timeout     seconds to wait, or (fn [ctx] -> seconds)
     :interval    seconds between reads
     :description the post-check's description, \"<Kind> <ns>/<name> is
                  suspended\" when absent
     :also        (fn [ctx live] -> bool), a further condition the post-check
                  waits for, given the object as just read"
  [spec]
  (switch spec true suspended? #(str (label %) " is suspended")))

(defn resume
  "The :effect and :verify of a step that resumes the object `(object ctx)`
   returns, requesting a reconcile at the context's :timestamp, and waits
   until it is no longer suspended and Ready. Takes what `suspend` takes; the
   default description is \"<Kind> <ns>/<name> is resumed and Ready\". The
   step's :derive supplies :timestamp."
  [spec]
  (switch spec false (fn [kind live] (and (not (suspended? kind live)) (ready? live)))
          #(str (label %) " is resumed and Ready")))

(defn reconcile-effect
  "Asks the object to reconcile now, stamping `token` as requestedAt."
  [context {:keys [type namespace name]} token]
  (effect/plan
   (effect/kubectl context "-n" namespace "annotate" type name "--overwrite"
                   (str requested-at "=" token))))

(defn handled?
  "Whether the object reconciled since `token` was stamped: a Flux controller
   writes the requestedAt it acted on to status.lastHandledReconcileAt.
   Readiness is not the test, since an object that was Ready before the
   request stays Ready whether or not the request was handled."
  [object token]
  (= token (-> object :status :lastHandledReconcileAt)))

(defn reconcile
  "The :derive, :effect and :verify of a step that asks the object
   `(object ctx)` returns to reconcile now and waits until its controller
   handled that request. Takes :object, :none, :timeout, :interval and
   :description as `suspend` does; the default description is \"<Kind>
   <ns>/<name> handled the reconcile request\". The token is the context's
   :timestamp, which the returned :derive sets; a step with its own :derive
   sets :timestamp there instead."
  [{:keys [object none timeout interval description]}]
  {:derive (fn [_] {:timestamp (str (java.time.Instant/now))})
   :effect (fn [{:keys [context timestamp] :as ctx}]
             (if-let [o (object ctx)]
               (reconcile-effect context o timestamp)
               (effect/plan (effect/note none))))
   :verify (fn [{:keys [context timestamp] :as ctx}]
             (when-let [o (object ctx)]
               [{:description (or description (str (label o) " handled the reconcile request"))
                 :timeout (seconds timeout ctx) :interval interval
                 :check #(handled? (fetch context o) timestamp)}]))})
